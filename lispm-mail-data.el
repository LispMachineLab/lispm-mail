;;; lispm-mail-data.el --- LispM Mail's mail, fetched and kept  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-mail
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The mail of LispM Mail's accounts: fetched by an Emacs of each
;; account's own in the background (lispm-mail-worker.el), so that
;; nothing here waits on the network, and kept in
;; `lispm-mail-directory', so that a mailbox shows at once what it held
;; last, and a message read once opens again without the network.
;;
;; Each account has its mailboxes, as its server lists them, with
;; their counts, and the role each plays, Inbox, Sent, Trash...; each
;; mailbox the latest of its messages, `lispm-mail-window-size' of them
;; unless more were asked for, by UID, their headers, flags and first
;; words.  What is done to a message, read, flagged, moved, deleted, is
;; done here at once and on the server after, undone here again when
;; the server refuses.  `lispm-mail-changed-functions' says what
;; changed, for the windows showing it to draw it again.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-mail-accounts)
(require 'lispm-mail-worker)

(defcustom lispm-mail-directory (locate-user-emacs-file "lispm-mail/")
  "Where the mail fetched is kept, to be shown at once and read offline."
  :type 'directory
  :group 'lispm-mail)

(defcustom lispm-mail-window-size 200
  "How many of a mailbox's latest messages are listed, until more are asked for."
  :type 'natnum
  :group 'lispm-mail)

(defcustom lispm-mail-check-interval 5
  "Minutes between two checks for new mail, or nil to check only when asked."
  :type '(choice (const :tag "Only when asked" nil) natnum)
  :group 'lispm-mail)

(defvar lispm-mail-changed-functions nil
  "Functions called when what is kept of an account changes.
Each is called with the account's :id and the mailbox whose messages
changed, or nil when its mailboxes or its state did.")

(defvar lispm-mail-new-mail-functions nil
  "Functions called with an account's :id, a mailbox and the messages new in it.
The messages are those come in its Inbox since it was last checked,
unread.")

(defvar lispm-mail-sent-functions nil
  "Functions called with an account's :id, a message's subject, and nil
once it is sent, or what went wrong.")

;;;; The background Emacsen

(cl-defstruct (lispm-mail--worker (:constructor lispm-mail--worker-create)
                                  (:copier nil))
  "The Emacs speaking for an account, in the background."
  id process queue busy (serial 0) state error started)

(defvar lispm-mail--workers (make-hash-table :test #'equal)
  "The background Emacsen, by account :id.")

(defconst lispm-mail--priorities
  '((imap . -1) (fetch . 0) (send . 1) (store . 1) (move . 1) (expunge . 1) (append . 1) (forget . 1)
    (sync . 2) (mailboxes . 3) (snippets . 4))
  "Which requests go first, the lower before.")

(defun lispm-mail--worker-file ()
  "Return the file a background Emacs loads."
  (or (locate-library "lispm-mail-worker.el")
      (locate-library "lispm-mail-worker")))

(defun lispm-mail--worker-command (&rest arguments)
  "Return the command starting a background Emacs, with ARGUMENTS after."
  (append (list (expand-file-name invocation-name invocation-directory)
                "-Q" "--batch" "-l" (lispm-mail--worker-file))
          arguments))

(defun lispm-mail-worker-state (id)
  "Return the state of account ID's connection: (STATE . ERROR).
STATE is nil, not connected yet, `connecting', `online' or `offline'."
  (if-let* ((worker (gethash id lispm-mail--workers)))
      (cons (lispm-mail--worker-state worker) (lispm-mail--worker-error worker))
    (cons nil nil)))

(defun lispm-mail--set-state (worker state &optional error)
  "Note WORKER's STATE and ERROR, and say so."
  (unless (and (eq state (lispm-mail--worker-state worker))
               (equal error (lispm-mail--worker-error worker)))
    (setf (lispm-mail--worker-state worker) state
          (lispm-mail--worker-error worker) error)
    (run-hook-with-args 'lispm-mail-changed-functions (lispm-mail--worker-id worker) nil)))

(defun lispm-mail-parse-answers (buffer)
  "Return the answers BUFFER, a background Emacs's output, holds whole,
taking them out of it."
  (with-current-buffer buffer
    (let (answers)
      (goto-char (point-min))
      (while (and (search-forward lispm-mail-worker-tag nil t)
                  (save-excursion (search-forward "\n" nil t)))
        (let ((start (point)))
          (end-of-line)
          (push (condition-case nil (car (read-from-string (buffer-substring start (point))))
                  (error nil))
                answers)
          (delete-region (point-min) (min (point-max) (1+ (point))))
          (goto-char (point-min))))
      (nreverse (delq nil answers)))))

(defun lispm-mail--filter (process output)
  "Take OUTPUT, of PROCESS, a background Emacs: answer its requests."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (goto-char (point-max))
      (insert output))
    (let ((worker (process-get process 'lispm-mail-worker)))
      (dolist (answer (lispm-mail-parse-answers (process-buffer process)))
        (pcase-let ((`(,id ,result ,value) answer)
                    (busy (lispm-mail--worker-busy worker)))
          (when (and busy (eql id (car busy)))
            (setf (lispm-mail--worker-busy worker) nil)
            (condition-case err
                (if (eq result 'ok)
                    (funcall (cdr busy) value nil)
                  (funcall (cdr busy) nil (or value "It failed")))
              (error (message "LispM Mail: %s" (error-message-string err))))
            (lispm-mail--next worker)))))))

(defun lispm-mail--sentinel (process _event)
  "Note that PROCESS, a background Emacs, stopped."
  (unless (process-live-p process)
    (let ((worker (process-get process 'lispm-mail-worker)))
      (when (eq (lispm-mail--worker-process worker) process)
        (setf (lispm-mail--worker-process worker) nil)
        (when-let* ((busy (lispm-mail--worker-busy worker)))
          (setf (lispm-mail--worker-busy worker) nil)
          (ignore-errors (funcall (cdr busy) nil "The mail's Emacs in the background stopped")))
        (if (and (lispm-mail--worker-started worker)
                 (< (- (float-time) (lispm-mail--worker-started worker)) 10)
                 (lispm-mail--worker-queue worker))
            ;; Stopped as soon as started: not again and again.
            (lispm-mail--fail-queue worker "The mail's Emacs in the background could not start")
          (lispm-mail--next worker)))
      (when (buffer-live-p (process-buffer process))
        (kill-buffer (process-buffer process))))))

(defun lispm-mail--fail-queue (worker error)
  "Fail every request WORKER has waiting with ERROR."
  (let ((queue (lispm-mail--worker-queue worker)))
    (setf (lispm-mail--worker-queue worker) nil)
    (dolist (entry queue)
      (ignore-errors (funcall (nth 4 entry) nil error)))))

(defun lispm-mail--start (worker)
  "Start WORKER's Emacs, its first request to log in to the account's server."
  (let* ((id (lispm-mail--worker-id worker))
         (account (lispm-mail-account id))
         (server (and account (lispm-mail-server account 'imap))))
    (cond
     ((null account)
      (lispm-mail--fail-queue worker "No such account"))
     ((null (plist-get server :password))
      (lispm-mail--set-state worker 'offline "No password is kept for this account")
      (lispm-mail--fail-queue worker "No password is kept for this account: open Settings"))
     (t
      (let ((process (make-process
                      :name (format "lispm-mail %s" id)
                      :buffer (generate-new-buffer (format " *lispm-mail %s*" id))
                      :command (lispm-mail--worker-command "-f" "lispm-mail-worker-run")
                      :connection-type 'pipe
                      :coding 'binary
                      :noquery t
                      :stderr (get-buffer-create " *lispm-mail log*")
                      :filter #'lispm-mail--filter
                      :sentinel #'lispm-mail--sentinel)))
        (process-put process 'lispm-mail-worker worker)
        (setf (lispm-mail--worker-process worker) process
              (lispm-mail--worker-started worker) (float-time))
        (lispm-mail--set-state worker 'connecting)
        (lispm-mail--enqueue
         worker 'imap server
         (lambda (_capabilities error)
           (if error
               (progn
                 (lispm-mail--set-state worker 'offline error)
                 (lispm-mail--fail-queue worker error))
             (lispm-mail--set-state worker 'online)))))))))

(defun lispm-mail--enqueue (worker operation properties callback)
  "Put the request OPERATION with PROPERTIES in WORKER's queue, CALLBACK
to be called with its value and error."
  (let ((serial (cl-incf (lispm-mail--worker-serial worker))))
    (setf (lispm-mail--worker-queue worker)
          (sort (cons (list (alist-get operation lispm-mail--priorities 2) serial
                            operation properties callback)
                      (lispm-mail--worker-queue worker))
                (lambda (a b) (or (< (car a) (car b))
                                  (and (= (car a) (car b)) (< (nth 1 a) (nth 1 b)))))))))

(defun lispm-mail--next (worker)
  "Send WORKER's Emacs its next request, when it is not busy."
  (when (and (not (lispm-mail--worker-busy worker))
             (lispm-mail--worker-queue worker))
    (if (not (process-live-p (lispm-mail--worker-process worker)))
        (lispm-mail--start worker)
      (pcase-let* ((`(,_ ,serial ,operation ,properties ,callback) (pop (lispm-mail--worker-queue worker))))
        (setf (lispm-mail--worker-busy worker) (cons serial callback))
        (process-send-string (lispm-mail--worker-process worker)
                             (concat (lispm-mail-worker-print (cl-list* serial operation properties))
                                     "\n"))))
    (when (and (not (lispm-mail--worker-busy worker))
               (lispm-mail--worker-queue worker)
               (process-live-p (lispm-mail--worker-process worker)))
      (lispm-mail--next worker))))

(defun lispm-mail-request (id operation properties callback)
  "Ask account ID's Emacs in the background to do OPERATION with PROPERTIES.
CALLBACK is called with the value and nil, or nil and what went
wrong.  See lispm-mail-worker.el for the operations."
  (let ((worker (or (gethash id lispm-mail--workers)
                    (puthash id (lispm-mail--worker-create :id id) lispm-mail--workers))))
    (lispm-mail--enqueue worker operation properties callback)
    (lispm-mail--next worker)))

(defun lispm-mail-stop-workers (&optional id)
  "Stop the Emacsen in the background, or only account ID's."
  (maphash (lambda (key worker)
             (when (or (null id) (equal id key))
               (let ((process (lispm-mail--worker-process worker)))
                 (setf (lispm-mail--worker-process worker) nil
                       (lispm-mail--worker-busy worker) nil
                       (lispm-mail--worker-queue worker) nil
                       (lispm-mail--worker-state worker) nil
                       (lispm-mail--worker-error worker) nil)
                 (when (process-live-p process)
                   (delete-process process)))
               (remhash key lispm-mail--workers)))
           lispm-mail--workers))

(defun lispm-mail-check-servers (imap smtp callback)
  "Connect to IMAP and SMTP, servers with their passwords, and log in to
each, in an Emacs of its own.  Call CALLBACK with (:imap RESULT :smtp
RESULT), RESULT t or what went wrong."
  (let* ((buffer (generate-new-buffer " *lispm-mail check*"))
         (done nil)
         (process
          (make-process
           :name "lispm-mail check"
           :buffer buffer
           :command (lispm-mail--worker-command "-f" "lispm-mail-worker-run")
           :connection-type 'pipe
           :coding 'binary
           :noquery t
           :stderr (get-buffer-create " *lispm-mail log*")
           :filter (lambda (process output)
                     (when (buffer-live-p (process-buffer process))
                       (with-current-buffer (process-buffer process)
                         (goto-char (point-max))
                         (insert output))
                       (when-let* ((answer (car (lispm-mail-parse-answers (process-buffer process)))))
                         (setq done t)
                         (delete-process process)
                         (kill-buffer buffer)
                         (funcall callback (if (eq (nth 1 answer) 'ok)
                                               (nth 2 answer)
                                             (list :imap (nth 2 answer) :smtp (nth 2 answer)))))))
           :sentinel (lambda (process _event)
                       (unless (or done (process-live-p process))
                         (setq done t)
                         (when (buffer-live-p buffer) (kill-buffer buffer))
                         (funcall callback (list :imap "The check stopped" :smtp "The check stopped")))))))
    (process-send-string process
                         (concat (lispm-mail-worker-print (list 1 'check :imap imap :smtp smtp)) "\n"))
    process))

;;;; Where mail is kept

(defun lispm-mail--safe-name (name)
  "Return NAME, a mailbox's, as a file's name."
  (format "%s-%s" (replace-regexp-in-string "[^A-Za-z0-9._-]+" "_" name)
          (substring (md5 name) 0 6)))

(defun lispm-mail--account-directory (id)
  "Return the directory account ID's mail is kept in."
  (expand-file-name (lispm-mail--safe-name id) lispm-mail-directory))

(defun lispm-mail--mailbox-directory (id mailbox)
  "Return the directory MAILBOX of account ID is kept in."
  (expand-file-name (lispm-mail--safe-name mailbox)
                    (expand-file-name "mailboxes" (lispm-mail--account-directory id))))

(defun lispm-mail--read-file (file)
  "Return the form FILE holds, or nil."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (ignore-errors (read (current-buffer))))))

(defun lispm-mail--write-file (file form)
  "Write FORM to FILE, readable by its owner only."
  (make-directory (file-name-directory file) t)
  (let ((print-length nil) (print-level nil) (coding-system-for-write 'utf-8-unix))
    (with-temp-file file
      (insert ";; -*- lisp-data -*-\n")
      (prin1 form (current-buffer))
      (insert "\n"))
    (set-file-modes file #o600)))

;;;; Mailboxes

(defvar lispm-mail--mailboxes (make-hash-table :test #'equal)
  "The mailboxes of each account, by :id: a list of plists, as the
background Emacs's `mailboxes' gives them, with :role.")

(defconst lispm-mail-roles
  '((inbox "\\Inbox" "INBOX")
    (drafts "\\Drafts" "Drafts" "Draft" "Brouillons" "Entwürfe" "草稿箱" "草稿")
    (sent "\\Sent" "Sent" "Sent Messages" "Sent Items" "Sent Mail" "Gesendet" "已发送" "已发送邮件")
    (archive "\\Archive" "Archive" "Archives")
    (all "\\All" "All Mail")
    (flagged "\\Flagged" "Starred" "Flagged")
    (junk "\\Junk" "Junk" "Spam" "Junk E-mail" "Junk Email" "Bulk Mail" "垃圾邮件")
    (trash "\\Trash" "Trash" "Deleted Messages" "Deleted Items" "Deleted" "Bin" "Papierkorb" "已删除" "垃圾箱"))
  "The roles of mailboxes, in the order shown: (ROLE FLAG NAME ...).
A mailbox plays ROLE when its server says so with FLAG, else when its
name, or the last part of it, is one of the NAMEs.")

(defun lispm-mail-mailbox-role (mailbox)
  "Return the role MAILBOX, a plist, plays, or nil."
  (let* ((name (plist-get mailbox :name))
         (delimiter (plist-get mailbox :delimiter))
         (last (if (and delimiter (not (string-empty-p delimiter)))
                   (car (last (split-string name (regexp-quote delimiter))))
                 name))
         (flags (plist-get mailbox :flags)))
    (if (string-equal-ignore-case name "INBOX")
        'inbox
      (or (car (seq-find (lambda (role) (member-ignore-case (nth 1 role) flags)) lispm-mail-roles))
          (car (seq-find (lambda (role)
                           (and (not (eq (car role) 'inbox))
                                (or (member-ignore-case last (cddr role))
                                    (member-ignore-case name (cddr role)))))
                         lispm-mail-roles))))))

(defun lispm-mail-mailbox-selectable-p (mailbox)
  "Return non-nil when MAILBOX, a plist, holds messages to list."
  (let ((flags (plist-get mailbox :flags)))
    (not (or (member-ignore-case "\\Noselect" flags)
             (member-ignore-case "\\NonExistent" flags)))))

(defun lispm-mail--sort-mailboxes (mailboxes)
  "Return MAILBOXES in the order shown: by role, then by name."
  (let ((order (mapcar #'car lispm-mail-roles)))
    (sort (copy-sequence mailboxes)
          (lambda (a b)
            (let ((ra (seq-position order (plist-get a :role)))
                  (rb (seq-position order (plist-get b :role))))
              (cond ((and ra rb) (< ra rb))
                    (ra t)
                    (rb nil)
                    (t (string-collate-lessp (plist-get a :name) (plist-get b :name) nil t))))))))

(defun lispm-mail-mailboxes (id)
  "Return the mailboxes of account ID, in the order shown, as last known."
  (or (gethash id lispm-mail--mailboxes)
      (when-let* ((kept (lispm-mail--read-file
                         (expand-file-name "mailboxes.eld" (lispm-mail--account-directory id)))))
        (puthash id kept lispm-mail--mailboxes))))

(defun lispm-mail-mailbox (id name)
  "Return account ID's mailbox NAME, a plist, or nil."
  (seq-find (lambda (mailbox) (equal (plist-get mailbox :name) name)) (lispm-mail-mailboxes id)))

(defun lispm-mail-mailbox-of-role (id role)
  "Return the name of account ID's mailbox playing ROLE, or nil."
  (when-let* ((mailbox (seq-find (lambda (mailbox) (eq (plist-get mailbox :role) role))
                                 (lispm-mail-mailboxes id))))
    (plist-get mailbox :name)))

(defun lispm-mail--set-mailboxes (id mailboxes)
  "Keep MAILBOXES as account ID's, and say so."
  (let ((mailboxes (lispm-mail--sort-mailboxes
                    (mapcar (lambda (mailbox)
                              (append mailbox (list :role (lispm-mail-mailbox-role mailbox))))
                            mailboxes))))
    (puthash id mailboxes lispm-mail--mailboxes)
    (lispm-mail--write-file (expand-file-name "mailboxes.eld" (lispm-mail--account-directory id))
                            mailboxes)
    (run-hook-with-args 'lispm-mail-changed-functions id nil)))

(defun lispm-mail--adjust-unseen (id name delta)
  "Count DELTA more unread in account ID's mailbox NAME, here only."
  (when-let* ((mailbox (lispm-mail-mailbox id name))
              (unseen (plist-get mailbox :unseen)))
    (plist-put mailbox :unseen (max 0 (+ unseen delta)))
    (run-hook-with-args 'lispm-mail-changed-functions id nil)))

(defun lispm-mail-refresh-mailboxes (id &optional callback)
  "Ask account ID's server for its mailboxes and their counts.
Call CALLBACK with nil, or what went wrong."
  (lispm-mail-request id 'mailboxes nil
                      (lambda (mailboxes error)
                        (unless error (lispm-mail--set-mailboxes id mailboxes))
                        (when callback (funcall callback error)))))

;;;; Messages

(cl-defstruct (lispm-mail--box (:constructor lispm-mail--box-create)
                               (:copier nil))
  "What is kept of a mailbox's messages."
  uidvalidity exists (messages (make-hash-table)) limit synced state error)

(defvar lispm-mail--boxes (make-hash-table :test #'equal)
  "The mailboxes' messages, by (ID . MAILBOX).")

(defvar lispm-mail--save-timer nil
  "The timer saving the mailboxes changed, a moment after.")

(defvar lispm-mail--unsaved nil
  "The mailboxes changed since saved: a list of (ID . MAILBOX).")

(defun lispm-mail--box (id mailbox)
  "Return what is kept of MAILBOX of account ID, read from disk the first time."
  (let ((key (cons id mailbox)))
    (or (gethash key lispm-mail--boxes)
        (let ((box (lispm-mail--box-create))
              (kept (lispm-mail--read-file
                     (expand-file-name "messages.eld" (lispm-mail--mailbox-directory id mailbox)))))
          (when kept
            (setf (lispm-mail--box-uidvalidity box) (plist-get kept :uidvalidity)
                  (lispm-mail--box-exists box) (plist-get kept :exists)
                  (lispm-mail--box-limit box) (plist-get kept :limit))
            (dolist (message (plist-get kept :messages))
              (puthash (plist-get message :uid) message (lispm-mail--box-messages box))))
          (puthash key box lispm-mail--boxes)))))

(defun lispm-mail--save-boxes ()
  "Save the mailboxes changed."
  (setq lispm-mail--save-timer nil)
  (dolist (key (seq-uniq lispm-mail--unsaved))
    (let ((box (gethash key lispm-mail--boxes)))
      (when box
        (lispm-mail--write-file
         (expand-file-name "messages.eld" (lispm-mail--mailbox-directory (car key) (cdr key)))
         (list :uidvalidity (lispm-mail--box-uidvalidity box)
               :exists (lispm-mail--box-exists box)
               :limit (lispm-mail--box-limit box)
               :messages (hash-table-values (lispm-mail--box-messages box)))))))
  (setq lispm-mail--unsaved nil))

(defun lispm-mail--changed (id mailbox)
  "Say that MAILBOX of account ID changed, and save it a moment after."
  (push (cons id mailbox) lispm-mail--unsaved)
  (unless lispm-mail--save-timer
    (setq lispm-mail--save-timer (run-with-idle-timer 2 nil #'lispm-mail--save-boxes)))
  (run-hook-with-args 'lispm-mail-changed-functions id mailbox))

(defun lispm-mail-messages (id mailbox)
  "Return the messages kept of MAILBOX of account ID, the latest first."
  (sort (hash-table-values (lispm-mail--box-messages (lispm-mail--box id mailbox)))
        (lambda (a b)
          (let ((da (or (plist-get a :date) 0))
                (db (or (plist-get b :date) 0)))
            (if (= da db) (> (plist-get a :uid) (plist-get b :uid)) (> da db))))))

(defun lispm-mail-message (id mailbox uid)
  "Return the message UID of MAILBOX of account ID, or nil."
  (gethash uid (lispm-mail--box-messages (lispm-mail--box id mailbox))))

(defun lispm-mail-mailbox-state (id mailbox)
  "Return the state of MAILBOX of account ID: (STATE . ERROR).
STATE is nil, never fetched yet, `syncing', `synced' or `failed'."
  (let ((box (lispm-mail--box id mailbox)))
    (cons (lispm-mail--box-state box) (lispm-mail--box-error box))))

(defun lispm-mail-mailbox-more-p (id mailbox)
  "Return non-nil when MAILBOX of account ID holds more messages than kept."
  (let ((box (lispm-mail--box id mailbox)))
    (and (lispm-mail--box-exists box)
         (> (lispm-mail--box-exists box) (hash-table-count (lispm-mail--box-messages box))))))

(defun lispm-mail-seen-p (message)
  "Return non-nil when MESSAGE has been read."
  (member "\\Seen" (plist-get message :flags)))

(defun lispm-mail-flagged-p (message)
  "Return non-nil when MESSAGE is flagged."
  (member "\\Flagged" (plist-get message :flags)))

(defun lispm-mail-answered-p (message)
  "Return non-nil when MESSAGE was answered."
  (member "\\Answered" (plist-get message :flags)))

(defun lispm-mail--merge (id mailbox result)
  "Keep RESULT, of a `sync' of MAILBOX of account ID; return the messages new."
  (let* ((box (lispm-mail--box id mailbox))
         (messages (lispm-mail--box-messages box))
         (fresh (make-hash-table))
         (new nil))
    (unless (equal (plist-get result :uidvalidity) (lispm-mail--box-uidvalidity box))
      (clrhash messages))
    (dolist (header (plist-get result :headers))
      (puthash (plist-get header :uid) header messages)
      (push header new))
    (dolist (entry (plist-get result :flags))
      (when-let* ((message (gethash (car entry) messages)))
        (puthash (car entry) (plist-put message :flags (cdr entry)) fresh)))
    (setf (lispm-mail--box-messages box) fresh
          (lispm-mail--box-uidvalidity box) (plist-get result :uidvalidity)
          (lispm-mail--box-exists box) (plist-get result :exists)
          (lispm-mail--box-synced box) (float-time))
    (seq-filter (lambda (message) (gethash (plist-get message :uid) fresh)) new)))

(defun lispm-mail-sync (id mailbox &optional callback)
  "Ask account ID's server what MAILBOX holds now, and keep it.
Call CALLBACK with nil, or what went wrong."
  (let* ((box (lispm-mail--box id mailbox))
         (first (null (lispm-mail--box-synced box)))
         (known (hash-table-keys (lispm-mail--box-messages box))))
    (setf (lispm-mail--box-state box) 'syncing)
    (run-hook-with-args 'lispm-mail-changed-functions id mailbox)
    (lispm-mail-request
     id 'sync (list :mailbox mailbox
                    :limit (or (lispm-mail--box-limit box) lispm-mail-window-size)
                    :known known
                    :uidvalidity (lispm-mail--box-uidvalidity box))
     (lambda (result error)
       (if error
           (setf (lispm-mail--box-state box) 'failed
                 (lispm-mail--box-error box) error)
         (let ((new (lispm-mail--merge id mailbox result)))
           (setf (lispm-mail--box-state box) 'synced
                 (lispm-mail--box-error box) nil)
           (lispm-mail--note-new-mail id mailbox new first known)
           (lispm-mail--fetch-snippets id mailbox)))
       (lispm-mail--changed id mailbox)
       (when callback (funcall callback error))))))

(defun lispm-mail--note-new-mail (id mailbox new first known)
  "Say which of NEW, messages come in MAILBOX of account ID, are new mail.
Mail is new when unread in an Inbox, come since it was last checked:
not FIRST checked, after KNOWN, the messages known before."
  (when (and new (not first) known
             (eq (plist-get (lispm-mail-mailbox id mailbox) :role) 'inbox))
    (let* ((before (apply #'max known))
           (mail (seq-filter (lambda (message)
                               (and (> (plist-get message :uid) before)
                                    (not (lispm-mail-seen-p message))))
                             new)))
      (when mail
        (run-hook-with-args 'lispm-mail-new-mail-functions id mailbox mail)))))

(defun lispm-mail--fetch-snippets (id mailbox)
  "Ask for the first words of MAILBOX's latest messages without them."
  (let ((wanted (seq-take (seq-remove (lambda (message) (plist-member message :snippet))
                                      (lispm-mail-messages id mailbox))
                          60)))
    (when wanted
      (lispm-mail-request
       id 'snippets (list :mailbox mailbox :uids (mapcar (lambda (m) (plist-get m :uid)) wanted))
       (lambda (snippets error)
         (unless error
           (dolist (message wanted)
             (let ((uid (plist-get message :uid)))
               (when-let* ((kept (lispm-mail-message id mailbox uid)))
                 (plist-put kept :snippet (or (alist-get uid snippets) "")))))
           (lispm-mail--changed id mailbox)))))))

(defun lispm-mail-load-more (id mailbox &optional callback)
  "List more of MAILBOX's messages, older ones, as many again."
  (let ((box (lispm-mail--box id mailbox)))
    (setf (lispm-mail--box-limit box)
          (+ (or (lispm-mail--box-limit box) lispm-mail-window-size) lispm-mail-window-size))
    (lispm-mail-sync id mailbox callback)))

;;;; What is done to messages

(defun lispm-mail--report (what error)
  "Say that WHAT failed, for ERROR."
  (message "LispM Mail: %s failed: %s" what error))

(defun lispm-mail-set-flag (id mailbox uids flag on)
  "Set FLAG of messages UIDS of MAILBOX of account ID, ON or off."
  (let ((changed nil))
    (dolist (uid uids)
      (when-let* ((message (lispm-mail-message id mailbox uid)))
        (let ((flags (plist-get message :flags)))
          (unless (eq (and (member flag flags) t) (and on t))
            (push uid changed)
            (plist-put message :flags (if on (cons flag flags) (remove flag flags)))))))
    (when changed
      (when (equal flag "\\Seen")
        (lispm-mail--adjust-unseen id mailbox (if on (- (length changed)) (length changed))))
      (lispm-mail--changed id mailbox)
      (lispm-mail-request
       id 'store (list :mailbox mailbox :uids changed (if on :add :remove) flag)
       (lambda (_ error)
         (when error
           (dolist (uid changed)
             (when-let* ((message (lispm-mail-message id mailbox uid)))
               (let ((flags (plist-get message :flags)))
                 (plist-put message :flags (if on (remove flag flags) (cons flag flags))))))
           (when (equal flag "\\Seen")
             (lispm-mail--adjust-unseen id mailbox (if on (length changed) (- (length changed)))))
           (lispm-mail--changed id mailbox)
           (lispm-mail--report "Changing the message" error)))))))

(defun lispm-mail--take (id mailbox uids)
  "Take messages UIDS out of what is kept of MAILBOX of account ID, here only.
Return them."
  (let ((box (lispm-mail--box id mailbox))
        taken)
    (dolist (uid uids)
      (when-let* ((message (gethash uid (lispm-mail--box-messages box))))
        (push message taken)
        (remhash uid (lispm-mail--box-messages box))))
    (when-let* ((exists (lispm-mail--box-exists box)))
      (setf (lispm-mail--box-exists box) (max 0 (- exists (length taken)))))
    (let ((unseen (seq-count (lambda (message) (not (lispm-mail-seen-p message))) taken)))
      (when (> unseen 0)
        (lispm-mail--adjust-unseen id mailbox (- unseen))))
    (lispm-mail--changed id mailbox)
    taken))

(defun lispm-mail-move (id mailbox uids to &optional callback)
  "Move messages UIDS of MAILBOX of account ID to the mailbox TO.
Call CALLBACK with nil, or what went wrong."
  (lispm-mail--take id mailbox uids)
  (lispm-mail-request
   id 'move (list :mailbox mailbox :uids uids :to to)
   (lambda (_ error)
     (if error
         (progn (lispm-mail--report "Moving the message" error)
                (lispm-mail-sync id mailbox))
       (when (lispm-mail-mailbox id to)
         (lispm-mail-refresh-mailboxes id)))
     (when callback (funcall callback error)))))

(defun lispm-mail-delete (id mailbox uids &optional callback)
  "Delete messages UIDS of MAILBOX of account ID: to the Trash, or for good
from the Trash, or without one."
  (let ((trash (lispm-mail-mailbox-of-role id 'trash)))
    (if (and trash (not (equal trash mailbox)))
        (lispm-mail-move id mailbox uids trash callback)
      (lispm-mail--take id mailbox uids)
      (lispm-mail-request
       id 'expunge (list :mailbox mailbox :uids uids)
       (lambda (_ error)
         (when error
           (lispm-mail--report "Deleting the message" error)
           (lispm-mail-sync id mailbox))
         (when callback (funcall callback error)))))))

(defun lispm-mail-archive-mailbox (id)
  "Return the mailbox account ID archives in, or nil without one."
  (or (lispm-mail-mailbox-of-role id 'archive)
      (lispm-mail-mailbox-of-role id 'all)))

(defun lispm-mail-message-file (id mailbox uid)
  "Return the file message UID of MAILBOX of account ID is kept in."
  (expand-file-name (format "%s-%d.eml" (or (lispm-mail--box-uidvalidity (lispm-mail--box id mailbox)) "0")
                            uid)
                    (lispm-mail--mailbox-directory id mailbox)))

(defun lispm-mail-fetch (id mailbox uid callback)
  "Call CALLBACK with the file of message UID of MAILBOX of account ID, and nil,
once it is fetched, or nil and what went wrong."
  (let ((file (lispm-mail-message-file id mailbox uid)))
    (if (file-exists-p file)
        (funcall callback file nil)
      (lispm-mail-request id 'fetch (list :mailbox mailbox :uid uid :file file)
                          (lambda (_ error)
                            (if (or error (not (file-exists-p file)))
                                (funcall callback nil (or error "The message was not fetched"))
                              (set-file-modes file #o600)
                              (funcall callback file nil)))))))

(defun lispm-mail-append (id mailbox file flags &optional callback)
  "Add the message of FILE to MAILBOX of account ID, with FLAGS.
Call CALLBACK with nil, or what went wrong."
  (lispm-mail-request id 'append (list :mailbox mailbox :file file :flags flags)
                      (lambda (_ error)
                        (unless error (lispm-mail-sync id mailbox))
                        (when callback (funcall callback error)))))

(defun lispm-mail-sent-mailbox (id)
  "Return the mailbox account ID keeps mail sent in, or nil when its server does."
  (let ((sent (plist-get (lispm-mail-account id) :sent)))
    (cond ((eq sent 'server) nil)
          ((stringp sent) sent)
          (t (lispm-mail-mailbox-of-role id 'sent)))))

(defun lispm-mail-send-file (id file separator subject &optional callback)
  "Send the message of FILE from account ID, its headers ended by SEPARATOR.
A copy is kept in its Sent mailbox.  Call CALLBACK with nil, or what
went wrong; `lispm-mail-sent-functions' are told too."
  (let ((account (lispm-mail-account id))
        (sent (lispm-mail-sent-mailbox id)))
    (lispm-mail-request
     id 'send (list :smtp (lispm-mail-server account 'smtp) :file file :separator separator :sent sent)
     (lambda (_ error)
       (when (and (not error) sent)
         (lispm-mail-sync id sent))
       (run-hook-with-args 'lispm-mail-sent-functions id subject error)
       (when callback (funcall callback error))))))

;;;; Checking for mail

(defvar lispm-mail--timer nil
  "The timer checking for new mail.")

(defvar lispm-mail-watched-functions nil
  "Functions returning the mailboxes shown, checked with the Inboxes:
a list of (ID . MAILBOX).")

(defun lispm-mail-check (&optional id)
  "Check every account for new mail, or only account ID: its mailboxes, their
counts, its Inbox and the mailboxes shown."
  (dolist (account lispm-mail-accounts)
    (let ((account-id (lispm-mail-account-id account)))
      (when (or (null id) (equal id account-id))
        (lispm-mail-refresh-mailboxes
         account-id
         (lambda (error)
           (unless error
             (let ((boxes (delq nil (list (lispm-mail-mailbox-of-role account-id 'inbox)))))
               (dolist (watched (apply #'append (mapcar #'funcall lispm-mail-watched-functions)))
                 (when (equal (car watched) account-id)
                   (cl-pushnew (cdr watched) boxes :test #'equal)))
               (dolist (mailbox boxes)
                 (lispm-mail-sync account-id mailbox))))))))))

(defun lispm-mail-start-checking ()
  "Check for new mail every `lispm-mail-check-interval' minutes from now on."
  (when lispm-mail--timer
    (cancel-timer lispm-mail--timer))
  (setq lispm-mail--timer
        (and lispm-mail-check-interval (> lispm-mail-check-interval 0) lispm-mail-accounts
             (run-at-time (* 60 lispm-mail-check-interval) (* 60 lispm-mail-check-interval)
                          #'lispm-mail-check))))

(defun lispm-mail-stop-checking ()
  "Check for new mail no longer, until asked."
  (when lispm-mail--timer
    (cancel-timer lispm-mail--timer)
    (setq lispm-mail--timer nil)))

(defun lispm-mail-unread-count (&optional id)
  "Return how many messages are unread in the Inboxes, or account ID's."
  (let ((count 0))
    (dolist (account lispm-mail-accounts)
      (let ((account-id (lispm-mail-account-id account)))
        (when (or (null id) (equal id account-id))
          (dolist (mailbox (lispm-mail-mailboxes account-id))
            (when (eq (plist-get mailbox :role) 'inbox)
              (cl-incf count (or (plist-get mailbox :unseen) 0)))))))
    count))

;;;###autoload
(defun lispm-mail-new-mail-p ()
  "Return non-nil when mail is unread in an Inbox.
It is what `display-time-mail-function' asks, so that whatever shows
Emacs's mail flag shows LispM Mail's."
  (> (lispm-mail-unread-count) 0))

(provide 'lispm-mail-data)
;;; lispm-mail-data.el ends here
