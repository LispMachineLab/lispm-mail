;;; lispm-mail-compose.el --- LispM Mail's messages, written and sent  -*- lexical-binding: t; -*-

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

;; Messages are written in Emacs's own message mode, with what a newer
;; mail program adds: the account they go from, chosen in a pop-up
;; menu; addresses completed from the mail kept; the reply above the
;; message it quotes; drafts kept in the account's Drafts mailbox, on
;; its server, so that another mail program finds them too; and mail
;; sent in the background, the window closing at once, a message kept
;; in the Outbox when it could not go, to be sent again.
;;
;; `lispm-mail-user-agent' is Emacs's mail user agent of LispM Mail, so
;; that C-x m and mailto: links write with it, see `mail-user-agent'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'message)
(require 'mml)
(require 'nnheader)
(require 'lispm-kit-commands)
(require 'lispm-mail-style)
(require 'lispm-mail-accounts)
(require 'lispm-mail-data)
(require 'lispm-mail-view)

(defcustom lispm-mail-reply-above t
  "Non-nil to write a reply above the message it quotes, nil below it."
  :type 'boolean
  :group 'lispm-mail)

(defvar-local lispm-mail-compose--account nil
  "The :id of the account the message is written from.")

(defvar-local lispm-mail-compose--message-id nil
  "The Message-ID of the message written, kept as its drafts are saved.")

(defvar-local lispm-mail-compose--window nil
  "The window the message is written in, and what it showed before:
\(WINDOW . BUFFER).")

(defvar lispm-mail-compose-display-function nil
  "Function showing a new message's buffer, selected; nil to pop to it.")

;;;; The Outbox

(defvar lispm-mail-outbox nil
  "The messages not sent yet, or that could not be: a list of plists,
:account, :file, :separator, :subject, :state `sending' or `failed', and
:error.")

(defun lispm-mail-outbox-directory ()
  "Return the directory the Outbox's messages are kept in."
  (expand-file-name "outbox" lispm-mail-directory))

(defun lispm-mail--outbox-changed ()
  "Say that the Outbox changed."
  (run-hook-with-args 'lispm-mail-changed-functions nil nil))

(defun lispm-mail-outbox-send (entry)
  "Send ENTRY, of the Outbox, now."
  (plist-put entry :state 'sending)
  (plist-put entry :error nil)
  (lispm-mail--outbox-changed)
  (message "Sending \"%s\"…" (plist-get entry :subject))
  (lispm-mail-send-file
   (plist-get entry :account) (plist-get entry :file) (plist-get entry :separator)
   (plist-get entry :subject)
   (lambda (error)
     (if error
         (progn
           (plist-put entry :state 'failed)
           (plist-put entry :error error)
           (message "Could not send \"%s\": %s.  It is kept in the Outbox."
                    (plist-get entry :subject) error))
       (lispm-mail--outbox-forget entry)
       (when-let* ((id (plist-get entry :message-id))
                   (drafts (lispm-mail-mailbox-of-role (plist-get entry :account) 'drafts)))
         (lispm-mail-request (plist-get entry :account) 'forget
                             (list :mailbox drafts :message-id id)
                             (lambda (_ error)
                               (unless error (lispm-mail-sync (plist-get entry :account) drafts)))))
       (message "Sent \"%s\"" (plist-get entry :subject)))
     (lispm-mail--outbox-changed))))

(defun lispm-mail--outbox-forget (entry)
  "Take ENTRY out of the Outbox, and its files off the disk."
  (setq lispm-mail-outbox (delq entry lispm-mail-outbox))
  (ignore-errors (delete-file (plist-get entry :file)))
  (ignore-errors (delete-file (concat (plist-get entry :file) ".eld"))))

(defun lispm-mail-outbox-remove (entry)
  "Take ENTRY out of the Outbox, unsent."
  (lispm-mail--outbox-forget entry)
  (lispm-mail--outbox-changed))

(defun lispm-mail-outbox-load ()
  "Read the Outbox's messages left by an Emacs before, as not sent."
  (let ((directory (lispm-mail-outbox-directory)))
    (when (file-directory-p directory)
      (dolist (file (directory-files directory t "\\.eml\\.eld\\'"))
        (let ((entry (lispm-mail--read-file file)))
          (when (and (consp entry)
                     (file-exists-p (plist-get entry :file))
                     (not (seq-find (lambda (other) (equal (plist-get other :file) (plist-get entry :file)))
                                    lispm-mail-outbox)))
            (setq lispm-mail-outbox
                  (append lispm-mail-outbox
                          (list (append (list :state 'failed :error "Not sent before Emacs was closed")
                                        entry))))))))))

;;;; Sending

(defun lispm-mail-compose--from-account ()
  "Return the account the message in the current buffer is from."
  (let* ((from (save-restriction
                 (message-narrow-to-headers)
                 (message-fetch-field "from")))
         (address (and from (cadr (mail-extract-address-components from)))))
    (or (lispm-mail-account-of-address address)
        (and lispm-mail-compose--account (lispm-mail-account lispm-mail-compose--account))
        (lispm-mail-default-account)
        (error "No account to send it from: add one in Settings"))))

(defun lispm-mail-compose--send-it ()
  "Send the message in the current buffer, as message mode made it ready.
It goes in the background; it is kept in the Outbox meanwhile."
  (let* ((account (lispm-mail-compose--from-account))
         (subject (save-restriction
                    (message-narrow-to-headers)
                    (rfc2047-decode-string (or (message-fetch-field "subject") ""))))
         (message-id (save-restriction
                       (message-narrow-to-headers)
                       (message-fetch-field "message-id")))
         (directory (lispm-mail-outbox-directory))
         (file (progn (make-directory directory t)
                      (make-temp-file (expand-file-name "message" directory) nil ".eml")))
         (entry (list :account (lispm-mail-account-id account) :file file
                      :separator mail-header-separator
                      :subject (if (string-blank-p subject) "(no subject)" subject)
                      :message-id message-id)))
    (let ((coding-system-for-write 'binary))
      (write-region nil nil file nil 'silent))
    (set-file-modes file #o600)
    ;; What it is, for an Emacs after this one, should this one close first.
    (lispm-mail--write-file (concat file ".eld") entry)
    (setq lispm-mail-outbox (append lispm-mail-outbox (list entry)))
    (lispm-mail-outbox-send entry)))

(defun lispm-mail-compose-send ()
  "Send the message, in the background, and close its window."
  (interactive)
  (save-restriction
    (message-narrow-to-headers)
    (when (and (string-blank-p (or (message-fetch-field "subject") ""))
               (not (y-or-n-p "Send it without a subject? ")))
      (user-error "Not sent")))
  (message-send-and-exit))

;;;; Drafts

(defun lispm-mail-compose--mime ()
  "Return the message in the current buffer as MIME, as it would be sent."
  (let ((text (save-restriction
                ;; The headers out of sight too, its Message-ID first.
                (widen)
                (buffer-substring-no-properties (point-min) (point-max))))
        (separator mail-header-separator))
    (with-temp-buffer
      (insert text)
      (delay-mode-hooks (message-mode))
      (setq-local mail-header-separator separator)
      (save-restriction
        (message-narrow-to-headers)
        (unless (message-fetch-field "date")
          (goto-char (point-max))
          (insert "Date: " (message-make-date) "\n")))
      (mml-to-mime)
      (goto-char (point-min))
      (when (re-search-forward (concat "^" (regexp-quote separator) "\n") nil t)
        (replace-match "\n" t t))
      (prog1 (buffer-substring-no-properties (point-min) (point-max))
        ;; Else message mode asks whether to kill it.
        (set-buffer-modified-p nil)))))

(defun lispm-mail-compose-save-draft ()
  "Keep the message in its account's Drafts mailbox, on the server."
  (interactive)
  (message "Saving the draft…")
  (lispm-mail-compose--save-draft
   (lambda (drafts error)
     (if error
         (message "Could not save the draft: %s" error)
       (message "Draft saved in %s" drafts)))))

(defun lispm-mail-compose--save-draft (callback)
  "Keep the message in the current buffer in its account's Drafts mailbox,
on the server; call CALLBACK with the mailbox and nil, or with nil and
what went wrong."
  (let* ((account (lispm-mail-compose--from-account))
         (id (lispm-mail-account-id account))
         (drafts (or (lispm-mail-mailbox-of-role id 'drafts)
                     (user-error "%s has no Drafts mailbox" (lispm-mail-account-title account))))
         (mime (lispm-mail-compose--mime))
         (file (make-temp-file "lispm-mail-draft" nil ".eml"))
         (buffer (current-buffer)))
    (let ((coding-system-for-write 'binary))
      (with-temp-file file
        (set-buffer-multibyte nil)
        (insert (encode-coding-string mime 'utf-8))))
    (set-file-modes file #o600)
    (lispm-mail-request id 'append
                        (list :mailbox drafts :file file :flags '("\\Draft" "\\Seen")
                              :replace lispm-mail-compose--message-id)
                        (lambda (_ error)
                          (delete-file file)
                          (unless error
                            (when (buffer-live-p buffer)
                              (with-current-buffer buffer (set-buffer-modified-p nil)))
                            (lispm-mail-sync id drafts))
                          (funcall callback (and (not error) drafts) error)))))

(defun lispm-mail-save-draft (fields callback)
  "Keep a message written with FIELDS, keys and values of
`lispm-mail-compose', in its account's Drafts mailbox on the server,
unsent and unseen: whoever reads mail sends it from there.  Call CALLBACK
with the mailbox and nil, or with nil and what went wrong."
  (let ((buffer (apply #'lispm-mail-compose :switch-function #'ignore fields)))
    (cl-flet ((done (drafts error)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer (set-buffer-modified-p nil))
                  (let ((kill-buffer-query-functions nil))
                    (kill-buffer buffer)))
                (funcall callback drafts error)))
      (condition-case err
          (with-current-buffer buffer
            (lispm-mail-compose--save-draft #'done))
        (error (done nil (error-message-string err)))))))

;;;; Accounts

(defun lispm-mail-compose--from (account)
  "Return the From header of mail from ACCOUNT."
  (let ((name (plist-get account :name))
        (address (plist-get account :address)))
    (if (and name (not (string-blank-p name)))
        (message-make-from name address)
      address)))

(defun lispm-mail-compose-choose-account (&optional account)
  "Send the message from ACCOUNT, chosen from the accounts."
  (interactive)
  (let ((account
         (or account
             (let* ((choices (mapcar (lambda (account)
                                       (cons (lispm-mail-compose--from account) account))
                                     lispm-mail-accounts))
                    (choice (completing-read "From: " choices nil t)))
               (cdr (assoc choice choices))))))
    (when account
      (setq lispm-mail-compose--account (lispm-mail-account-id account))
      (save-excursion
        (save-restriction
          (message-narrow-to-headers)
          (message-remove-header "From")
          (goto-char (point-min))
          (insert "From: " (lispm-mail-compose--from account) "\n")))
      (lispm-mail-compose--decorate))))

;;;; Addresses

(defvar lispm-mail--addresses-cache nil
  "The addresses known, and when: (TIME . CANDIDATES).")

(defun lispm-mail-known-addresses ()
  "Return the addresses of the mail kept, those seen most first, as
\"Name <address>\"."
  (if (and lispm-mail--addresses-cache (< (- (float-time) (car lispm-mail--addresses-cache)) 60))
      (cdr lispm-mail--addresses-cache)
    (let ((counts (make-hash-table :test #'equal))
          (names (make-hash-table :test #'equal)))
      (maphash (lambda (_key box)
                 (maphash (lambda (_uid message)
                            (dolist (entry (append (list (plist-get message :from))
                                                   (plist-get message :to)
                                                   (plist-get message :cc)))
                              (when-let* ((address (and entry (cdr entry)))
                                          (key (downcase address)))
                                (unless (lispm-mail-own-address-p address)
                                  (puthash key (1+ (gethash key counts 0)) counts)
                                  (when (and (car entry) (not (string-empty-p (car entry))))
                                    (puthash key (car entry) names))))))
                          (lispm-mail--box-messages box)))
               lispm-mail--boxes)
      (let ((candidates (mapcar (lambda (key)
                                  (let ((name (gethash key names)))
                                    (if name (message-make-from name key) key)))
                                (sort (hash-table-keys counts)
                                      (lambda (a b) (> (gethash a counts) (gethash b counts)))))))
        (setq lispm-mail--addresses-cache (cons (float-time) candidates))
        candidates))))

(defun lispm-mail-compose-complete-address ()
  "Complete the address at point in To, Cc and Bcc, from the mail kept.
This is one of `completion-at-point-functions'."
  (when (and (message-point-in-header-p)
             (save-excursion
               (beginning-of-line)
               (while (and (looking-at "[ \t]") (not (bobp))) (forward-line -1))
               (looking-at "\\(To\\|Cc\\|Bcc\\|Reply-To\\):")))
    (let ((end (point))
          (start (save-excursion
                   (skip-chars-backward "^:,\n")
                   (skip-chars-forward " \t")
                   (point))))
      (list start end (completion-table-case-fold
                       (completion-table-dynamic (lambda (_) (lispm-mail-known-addresses))))
            :exclusive 'no))))

;;;; The window

(defface lispm-mail-compose-field '((t :inherit lispm-mail-dim))
  "Face of a header's name, in a message written."
  :group 'lispm-mail)

(defun lispm-mail-compose--toolbar-button (label command)
  "Return a button of the composer's header line reading LABEL, doing COMMAND."
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] command)
    (propertize (concat " " label " ")
                'face 'lispm-mail-button 'mouse-face 'lispm-mail-button-pressed
                'keymap map 'help-echo label 'pointer 'hand)))

(defun lispm-mail-compose--header-line ()
  "Return the composer's header line: its buttons, and whom it is from."
  (concat (lispm-mail-style-space 12)
          (lispm-mail-compose--toolbar-button "Send" #'lispm-mail-compose-send)
          (lispm-mail-style-space 8)
          (lispm-mail-compose--toolbar-button "Attach" #'mml-attach-file)
          (lispm-mail-style-space 8)
          (lispm-mail-compose--toolbar-button "Save Draft" #'lispm-mail-compose-save-draft)
          (lispm-mail-style-space 8)
          (lispm-mail-compose--toolbar-button "Discard" #'message-kill-buffer)))

(defun lispm-mail-compose--decorate ()
  "Draw the headers' names right-aligned in a column, as a classic mail
program's message window had them, and the line between the headers and
the text as a rule."
  (remove-overlays (point-min) (point-max) 'lispm-mail-compose t)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let* ((end (save-excursion
                    (and (re-search-forward (concat "^" (regexp-quote mail-header-separator) "$") nil t)
                         (match-beginning 0))))
             (names nil))
        (when end
          (while (re-search-forward "^\\([A-Za-z-]+\\):[ \t]?" end t)
            (push (list (match-beginning 0) (match-end 0) (match-string 1)) names))
          (let ((column (+ 14 (apply #'max 0 (mapcar (lambda (name)
                                                        (lispm-mail-style-width
                                                         (propertize (concat (nth 2 name) ":")
                                                                     'face 'lispm-mail-compose-field)))
                                                      names)))))
            (pcase-dolist (`(,start ,stop ,name) names)
              (let ((label (propertize (concat name ":") 'face 'lispm-mail-compose-field))
                    (overlay (make-overlay start stop)))
                (overlay-put overlay 'lispm-mail-compose t)
                (overlay-put overlay 'invisible t)
                (overlay-put overlay 'before-string
                             (concat (lispm-mail-style-to (- column 6 (lispm-mail-style-width label)))
                                     label (lispm-mail-style-to column))))))
          (goto-char end)
          (looking-at ".*$")
          (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
            (overlay-put overlay 'lispm-mail-compose t)
            (overlay-put overlay 'display '(space :align-to right :height (1)))
            (overlay-put overlay 'face 'lispm-mail-rule))
          ;; Its newline weighs nothing: the line is as high as the rule.
          (let ((overlay (make-overlay (match-end 0) (1+ (match-end 0)))))
            (overlay-put overlay 'lispm-mail-compose t)
            (overlay-put overlay 'line-height t)
            (overlay-put overlay 'line-spacing 14)))))))

(defvar-local lispm-mail-compose--decorate-timer nil
  "The timer drawing the headers again, a moment after they changed.")

(defun lispm-mail-compose--headers-changed (start _end _length)
  "Draw the headers again, a moment after, when the change at START is
among them.  This is in `after-change-functions'."
  (when (and (not lispm-mail-compose--decorate-timer)
             (save-excursion (goto-char start) (message-point-in-header-p)))
    (let ((buffer (current-buffer)))
      (setq lispm-mail-compose--decorate-timer
            (run-with-idle-timer 0.3 nil
                                 (lambda ()
                                   (when (buffer-live-p buffer)
                                     (with-current-buffer buffer
                                       (setq lispm-mail-compose--decorate-timer nil)
                                       (lispm-mail-compose--decorate)))))))))

(defun lispm-mail-compose-title ()
  "Return the title of the message written: its subject, or New Message."
  (let ((subject (save-restriction
                   (widen)
                   (ignore-errors (message-narrow-to-headers) (message-fetch-field "subject")))))
    (if (string-blank-p (or subject "")) "New Message" subject)))

;; Its own menu beside message mode's, which keeps its own.
(lispm-kit-define-commands lispm-mail-compose-mode
  :menu "Mail"
  (lispm-mail-compose-send :key "C-c C-c" :help "Send the message" :label "Send" :button "Send")
  (mml-attach-file :key "C-c C-a" :help "Attach a file" :label "Attach File…" :button "Attach")
  (lispm-mail-compose-save-draft :key ("C-x C-s" "C-c C-d") :help "Keep it in Drafts, on the server"
                                 :label "Save Draft" :button "Save Draft")
  (lispm-mail-compose-choose-account :key "C-c C-f f" :help "Send it from another account"
                                     :label "From…" :enabled (cdr lispm-mail-accounts))
  "---"
  (message-kill-buffer :key "C-c C-k" :help "Close it, unsent" :label "Discard" :button "Discard"))

(define-derived-mode lispm-mail-compose-mode message-mode "New Message"
  "A message written from one of LispM Mail's accounts.
\\{lispm-mail-compose-mode-map}"
  (setq-local message-send-mail-function #'lispm-mail-compose--send-it
              message-kill-buffer-on-exit t
              message-auto-save-directory nil
              message-hidden-headers (append '("^Message-ID:" "^X-Draft-From:")
                                             (if (listp message-hidden-headers)
                                                 message-hidden-headers
                                               (list message-hidden-headers))))
  (lispm-mail-style-dress)
  (face-remap-add-relative 'header-line '(:inherit lispm-mail-ui :height 0.95))
  (setq-local header-line-format '(:eval (lispm-mail-compose--header-line))
              left-margin-width 3
              right-margin-width 3
              line-spacing 6)
  (face-remap-add-relative 'message-header-name 'lispm-mail-compose-field)
  ;; The headers' values in the text's colour.
  (face-remap-set-base 'message-header-to '(:weight normal))
  (face-remap-set-base 'message-header-subject '(:weight bold))
  (dolist (face '(message-header-cc message-header-newsgroups))
    (face-remap-set-base face '(:weight normal)))
  (face-remap-set-base 'message-header-other '(:weight normal))
  (lispm-mail-style-hairline-mode-line)
  (dolist (face '(message-cited-text-1 message-cited-text-2 message-cited-text-3 message-cited-text-4))
    (face-remap-add-relative face 'lispm-mail-quote))
  (add-hook 'completion-at-point-functions #'lispm-mail-compose-complete-address nil t)
  (add-hook 'after-change-functions #'lispm-mail-compose--headers-changed nil t))

(defun lispm-mail-compose--show (buffer)
  "Show BUFFER, a message written, selected."
  (if lispm-mail-compose-display-function
      (funcall lispm-mail-compose-display-function buffer)
    (pop-to-buffer buffer)))

(defun lispm-mail-compose--restore ()
  "Show in the message's window what it showed before, as it closes."
  (pcase lispm-mail-compose--window
    (`(,window . ,buffer)
     (when (and (window-live-p window) (buffer-live-p buffer)
                (eq (window-buffer window) (current-buffer)))
       (set-window-buffer window buffer)))))

(cl-defun lispm-mail-compose (&key account to cc bcc subject headers body quote
                                   message-id replying switch-function)
  "Write a new message from ACCOUNT, by default the default one.
TO, CC, BCC and SUBJECT are its headers' values, HEADERS others, an
alist; BODY its text; QUOTE the text it replies to, with its line
of attribution first.  MESSAGE-ID is a draft's, to replace it.
REPLYING, the Message-ID and References of the message replied to,
\(ID . REFERENCES), makes its In-Reply-To and References.
Return its buffer."
  (let* ((account (or account (lispm-mail-default-account)
                      (user-error "Add an account first, in Settings")))
         (buffer (generate-new-buffer (if (string-blank-p (or subject "")) "*New Message*"
                                        (format "*%s*" subject))))
         (before (selected-window))
         (shown (window-buffer before)))
    ;; In its mode before it is shown, so that its window takes its look.
    (with-current-buffer buffer
      (lispm-mail-compose-mode))
    (funcall (or switch-function #'lispm-mail-compose--show) buffer)
    (with-current-buffer buffer
      (when (car replying)
        ;; As message mode knows the message a reply is to.
        (setq-local message-reply-headers
                    (make-full-mail-header 0 "" "" "" (car replying) (or (cdr replying) ""))))
      (setq lispm-mail-compose--account (lispm-mail-account-id account)
            lispm-mail-compose--message-id (or message-id (message-make-message-id))
            lispm-mail-compose--window (and (eq (selected-window) before)
                                            (not (eq shown buffer))
                                            (cons before shown)))
      (let ((message-this-is-mail t)
            (message-signature (let ((signature (plist-get account :signature)))
                                 (and signature (not (string-blank-p signature)) signature)))
            (user-full-name (or (plist-get account :name) user-full-name))
            (user-mail-address (plist-get account :address)))
        (message-setup (append `((From . ,(lispm-mail-compose--from account))
                                 (To . ,(or to "")))
                               (and cc `((Cc . ,cc)))
                               (and bcc `((Bcc . ,bcc)))
                               `((Subject . ,(or subject "")))
                               headers
                               `((Message-ID . ,lispm-mail-compose--message-id)))))
      (message-add-action #'lispm-mail-compose--restore 'exit 'kill 'postpone)
      (message-goto-body)
      (when body
        (save-excursion (insert body)))
      (when quote
        (save-excursion
          (if lispm-mail-reply-above
              (goto-char (point-max))
            (message-goto-body))
          (let ((start (point)))
            (insert (if lispm-mail-reply-above "\n\n" "\n") quote "\n")
            (add-face-text-property start (point) 'lispm-mail-quote))
          (unless lispm-mail-reply-above (insert "\n"))))
      (if (and quote (not lispm-mail-reply-above))
          (goto-char (point-max))
        (message-goto-body))
      (unless (and to (not (string-blank-p to)))
        (message-goto-to))
      ;; Out of sight: the Message-ID, In-Reply-To and References.
      (message-hide-headers)
      (lispm-mail-compose--decorate)
      (set-buffer-modified-p nil))
    buffer))

;;;; Replies and forwards

(defun lispm-mail-compose--subject (prefix subject)
  "Return SUBJECT with PREFIX, \"Re\" or \"Fwd\", once."
  (let ((bare (string-trim (or subject ""))))
    (while (string-match "\\`\\(re\\|fwd?\\|aw\\|sv\\|回复\\|转发\\)[ \t]*[:：][ \t]*" (downcase bare))
      (setq bare (substring bare (match-end 0))))
    (concat prefix ": " bare)))

(defun lispm-mail-compose--addresses (entries)
  "Return ENTRIES, (NAME . ADDRESS), as a header's value."
  (mapconcat (lambda (entry)
               (if (and (car entry) (not (string-empty-p (car entry))))
                   (message-make-from (car entry) (cdr entry))
                 (cdr entry)))
             entries ", "))

(defun lispm-mail-compose--quote (headers text)
  "Return TEXT, of the message of HEADERS, quoted under who wrote it when."
  (concat (format "On %s, %s wrote:\n"
                  (if (plist-get headers :date)
                      (format-time-string "%a %-d %b %Y at %H:%M" (plist-get headers :date))
                    "a day")
                  (lispm-mail-address-text (plist-get headers :from)))
          (replace-regexp-in-string "^" "> " (string-trim-right text))))

(defun lispm-mail-reply (id headers file &optional all)
  "Reply to the message of FILE, its HEADERS a plist, of account ID.
With ALL, to everyone it went to as well."
  (apply #'lispm-mail-compose
         (lispm-mail-reply-fields id headers (lispm-mail-view-plain-text file) all)))

(defun lispm-mail-reply-fields (id headers text &optional all)
  "Return what a reply to the message of HEADERS, a plist, and TEXT, of
account ID, is written with: keys and values of `lispm-mail-compose',
:account :to :cc :subject :replying and :quote.  With ALL, it goes to
everyone the message went to as well."
  (let* ((account (or (lispm-mail-account id) (lispm-mail-default-account)))
         (own (plist-get account :address))
         (to (or (plist-get headers :reply-to) (list (plist-get headers :from))))
         (to (if (and (lispm-mail-own-address-p (cdr (car to))) (plist-get headers :to))
                 ;; A reply to mail one sent goes to whom it went to.
                 (plist-get headers :to)
               to))
         (cc (and all
                  (seq-remove (lambda (entry)
                                (or (lispm-mail-own-address-p (cdr entry))
                                    (string-equal-ignore-case (cdr entry) (or own ""))
                                    (seq-find (lambda (other)
                                                (string-equal-ignore-case (cdr other) (cdr entry)))
                                              to)))
                              (append (plist-get headers :to) (plist-get headers :cc))))))
    (list :account account
          :to (lispm-mail-compose--addresses to)
          :cc (and cc (lispm-mail-compose--addresses cc))
          :subject (lispm-mail-compose--subject "Re" (plist-get headers :subject))
          :replying (cons (plist-get headers :message-id) (plist-get headers :references))
          :quote (lispm-mail-compose--quote headers text))))

(defun lispm-mail-forward (id headers file)
  "Forward the message of FILE, its HEADERS a plist, of account ID, attached."
  (let ((account (or (lispm-mail-account id) (lispm-mail-default-account)))
        (raw (with-temp-buffer
               (set-buffer-multibyte nil)
               (insert-file-contents-literally file)
               (buffer-string))))
    (with-current-buffer
        (lispm-mail-compose
         :account account
         :subject (lispm-mail-compose--subject "Fwd" (plist-get headers :subject)))
      (save-excursion
        (goto-char (point-max))
        (insert "\n")
        (mml-insert-tag 'part 'type "message/rfc822" 'disposition "inline"
                        'description (plist-get headers :subject))
        (insert (decode-coding-string raw 'raw-text) "\n")
        (mml-insert-tag '/part))
      (set-buffer-modified-p nil)
      (current-buffer))))

(defun lispm-mail-edit-draft (id file)
  "Write on the draft of FILE, of account ID."
  (let* ((account (or (lispm-mail-account id) (lispm-mail-default-account)))
         (text (with-temp-buffer
                 (insert-file-contents file)
                 (mime-to-mml)
                 (buffer-string)))
         (head-end (or (string-match "\n\n" text) (length text)))
         (head (substring text 0 head-end))
         (body (if (< head-end (length text)) (substring text (+ 2 head-end)) ""))
         fields)
    (with-temp-buffer
      (insert head "\n")
      (goto-char (point-min))
      (while (re-search-forward "^\\([^: \t\n]+\\):[ \t]*\\(\\(?:.\\|\n[ \t]\\)*\\)" nil t)
        (push (cons (intern (capitalize (match-string 1)))
                    (replace-regexp-in-string "\n[ \t]+" " " (match-string 2)))
              fields)))
    (setq fields (nreverse fields))
    (with-current-buffer
        (lispm-mail-compose
         :account (or (lispm-mail-account-of-address
                       (cadr (mail-extract-address-components (or (alist-get 'From fields) ""))))
                      account)
         :to (alist-get 'To fields) :cc (alist-get 'Cc fields) :bcc (alist-get 'Bcc fields)
         :subject (alist-get 'Subject fields)
         :replying (when-let* ((id (alist-get 'In-Reply-To fields)))
                     (cons id (string-trim (string-replace id "" (or (alist-get 'References fields) "")))))
         :message-id (alist-get 'Message-Id fields)
         :body body)
      ;; The draft's signature is in its text already.
      (save-excursion
        (goto-char (point-max))
        (when (and (re-search-backward "^-- $" nil t)
                   (re-search-backward "^-- $" nil t))
          (delete-region (point) (point-max))))
      (set-buffer-modified-p nil)
      (current-buffer))))

;;;; Emacs's mail user agent

;;;###autoload
(defun lispm-mail-compose-mail (&optional to subject other-headers _continue switch-function
                                          _yank-action send-actions return-action)
  "Write a message to TO about SUBJECT with OTHER-HEADERS, as `compose-mail' asks.
SWITCH-FUNCTION shows its buffer; SEND-ACTIONS and RETURN-ACTION are
done once it is sent, and as it closes."
  (let* ((headers (mapcar (lambda (header)
                            (cons (if (stringp (car header)) (intern (capitalize (car header))) (car header))
                                  (cdr header)))
                          other-headers))
         (body (alist-get 'Body headers))
         (buffer (lispm-mail-compose
                  :to to :subject subject
                  :cc (alist-get 'Cc headers) :bcc (alist-get 'Bcc headers)
                  :headers (seq-remove (lambda (header) (memq (car header) '(Cc Bcc Body))) headers)
                  :body body
                  :switch-function (and switch-function
                                        (lambda (buffer) (funcall switch-function buffer))))))
    (with-current-buffer buffer
      (dolist (action send-actions)
        (message-add-action action 'send))
      (when return-action
        (message-add-action return-action 'exit 'kill 'postpone)))
    buffer))

;;;###autoload
(define-mail-user-agent 'lispm-mail-user-agent
  #'lispm-mail-compose-mail #'message-send-and-exit #'message-kill-buffer 'message-send-hook)

(provide 'lispm-mail-compose)
;;; lispm-mail-compose.el ends here
