;;; lispm-mail.el --- Emacs's own mail, in a classic-style window  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-mail
;; Version: 0.2.1
;; Package-Requires: ((emacs "30.1") (lispm-kit "0.1.0"))
;; Keywords: mail
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

;; LispM Mail reads and sends mail with Emacs's own libraries, imap.el,
;; smtpmail, message mode, mm and shr, in a window laid out as the
;; classic Mac's mail programs laid theirs, Outlook Express and Eudora:
;; a folder list on the left, with how many messages are unread; a
;; table of the folder's messages, sorted by any column, the unread ones
;; bold; the message chosen under it, or beside it, its headers in a box
;; over its text.  Accounts are added in its settings window, with
;; their servers filled in from the address.  Nothing waits on the
;; network: an Emacs of each account's own fetches and sends in the
;; background, and what was fetched is kept, shown at once next time.
;;
;; M-x lispm-mail opens it.  Every command has a key and a menu item:
;; n and p go from message to message, TAB from pane to pane; c writes
;; a message, r replies, a replies to all, f forwards; e archives, d
;; deletes, m moves, u marks read or unread, ! flags; / searches the
;; messages listed; g gets mail; , opens the settings.  C-h m lists
;; them all.
;;
;; It is Emacs's mail too, unless `lispm-mail-use-for-emacs' says not:
;; C-x m and mailto: links write with it, Read Mail opens it, and the
;; mail flag of `display-time-mode' shows its unread mail.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'tabulated-list)
(require 'lispm-kit-commands)
(require 'lispm-mail-style)
(require 'lispm-mail-accounts)
(require 'lispm-mail-data)
(require 'lispm-mail-view)
(require 'lispm-mail-compose)
(require 'lispm-mail-settings)

(defcustom lispm-mail-layout 'below
  "Where the message chosen is shown: `below' the list, or `beside' it."
  :type '(choice (const :tag "Below the list" below)
                 (const :tag "Beside the list" beside))
  :group 'lispm-mail)

(defcustom lispm-mail-show-mailboxes t
  "Non-nil to show the mailboxes on the left."
  :type 'boolean
  :group 'lispm-mail)

(defcustom lispm-mail-sidebar-width 28
  "The width of the mailboxes on the left, in columns."
  :type 'natnum
  :group 'lispm-mail)

(defcustom lispm-mail-mark-read 0
  "When a message shown is marked read: after so many seconds, or never."
  :type '(choice (const :tag "As soon as it is shown" 0)
                 (number :tag "After seconds")
                 (const :tag "Never" nil))
  :group 'lispm-mail)

(defface lispm-mail-account-heading '((t :weight bold))
  "Face of an account's name over its folders."
  :group 'lispm-mail)

(defface lispm-mail-count '((t :weight bold))
  "Face of a folder's count of unread messages."
  :group 'lispm-mail)

(defface lispm-mail-date '((t nil))
  "Face of a message's date in the list."
  :group 'lispm-mail)

(defface lispm-mail-title '((t :weight bold))
  "Face of the name of the mailbox listed, in its status line."
  :group 'lispm-mail)

(defconst lispm-mail-list-buffer-name "*Mail*"
  "The name of the buffer listing messages.")

(defconst lispm-mail-mailboxes-buffer-name "*Mail Mailboxes*"
  "The name of the buffer of the mailboxes.")

(defconst lispm-mail-message-buffer-name "*Mail Message*"
  "The name of the buffer showing the message chosen.")

(defvar lispm-mail--place nil
  "The mailbox listed: (ID . MAILBOX), or (outbox).")

(defvar lispm-mail--query nil
  "The words the messages listed are searched for, or nil.")

(defvar lispm-mail--marked nil
  "The UIDs of the messages selected together, in the mailbox listed.")

(defvar lispm-mail--shown nil
  "The message shown: (ID MAILBOX UID).")

(defvar lispm-mail--list-top nil
  "Non-nil when the list is to be drawn from its top, a mailbox newly listed.")

(defvar lispm-mail--saved-windows nil
  "The windows before LispM Mail was opened, put back as it closes.")

;;;; Buffers and windows

(defun lispm-mail--buffer (name mode)
  "Return the buffer NAME, in MODE."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (unless (derived-mode-p mode) (funcall mode)))
    buffer))

(defun lispm-mail--list-buffer ()
  "Return the buffer listing messages."
  (lispm-mail--buffer lispm-mail-list-buffer-name 'lispm-mail-mode))

(defun lispm-mail--mailboxes-buffer ()
  "Return the buffer of the mailboxes."
  (lispm-mail--buffer lispm-mail-mailboxes-buffer-name 'lispm-mail-mailboxes-mode))

(defun lispm-mail--message-buffer ()
  "Return the buffer of the message shown."
  (lispm-mail--buffer lispm-mail-message-buffer-name 'lispm-mail-message-mode))

(defun lispm-mail--pane (pane &optional frame)
  "Return the window of PANE, `mailboxes', `list' or `message', or nil."
  (seq-find (lambda (window) (eq (window-parameter window 'lispm-mail-pane) pane))
            (window-list frame 'never)))

(defun lispm-mail--set-pane (window pane buffer)
  "Make WINDOW the pane PANE, showing BUFFER."
  (set-window-buffer window buffer)
  (set-window-parameter window 'lispm-mail-pane pane)
  (set-window-dedicated-p window (and (eq pane 'mailboxes) t))
  ;; The panes draw their own edges:
  ;; no fringes, a line too long ending in an ellipsis, and no scroll
  ;; bars.
  (set-window-fringes window 0 0)
  (set-window-scroll-bars window 0 nil 0 nil))

(defun lispm-mail--layout ()
  "Lay the mailboxes, the list and the message out in the selected frame."
  (let ((list (lispm-mail--list-buffer)))
    (delete-other-windows)
    (dolist (window (window-list nil 'never))
      (set-window-parameter window 'lispm-mail-pane nil))
    (let ((list-window (selected-window)))
      (set-window-dedicated-p list-window nil)
      (lispm-mail--set-pane list-window 'list list)
      (when lispm-mail-accounts
        (when (and lispm-mail-show-mailboxes
                   (> (window-total-width list-window) (+ lispm-mail-sidebar-width 40)))
          (let ((side (split-window list-window (- lispm-mail-sidebar-width) 'left)))
            (lispm-mail--set-pane side 'mailboxes (lispm-mail--mailboxes-buffer))
            (window-preserve-size side t t)))
        (let ((view (if (eq lispm-mail-layout 'beside)
                        (split-window list-window (max 36 (floor (* 0.42 (window-total-width list-window))))
                                      'right)
                      (split-window list-window (max 6 (floor (* 0.42 (window-total-height list-window))))
                                    'below))))
          (lispm-mail--set-pane view 'message (lispm-mail--message-buffer))))
      (select-window list-window))))

(defun lispm-mail--ensure-layout ()
  "Lay LispM Mail's panes out again if they are not all there."
  (unless (and (lispm-mail--pane 'list)
               (or (null lispm-mail-accounts) (lispm-mail--pane 'message)))
    (lispm-mail--layout)))

;;;; Places

(defun lispm-mail--listed-messages ()
  "Return the messages listed: of the mailbox listed, found by the search."
  (pcase lispm-mail--place
    (`(outbox) nil)
    (`(,id . ,mailbox)
     (let ((messages (lispm-mail-messages id mailbox)))
       (if (and lispm-mail--query (not (string-blank-p lispm-mail--query)))
           (let ((words (split-string (downcase lispm-mail--query) nil t)))
             (seq-filter (lambda (message)
                           (let ((text (downcase
                                        (concat (plist-get message :subject) " "
                                                (lispm-mail-address-text (plist-get message :from)) " "
                                                (mapconcat #'lispm-mail-address-text
                                                           (plist-get message :to) " ")
                                                " " (or (plist-get message :snippet) "")))))
                             (seq-every-p (lambda (word) (string-search word text)) words)))
                         messages))
         messages)))))

(defun lispm-mail--mailbox-title (id mailbox)
  "Return what MAILBOX of account ID is called."
  (let* ((box (lispm-mail-mailbox id mailbox))
         (delimiter (plist-get box :delimiter)))
    (cond ((string-equal-ignore-case mailbox "INBOX") "Inbox")
          ((and delimiter (not (string-empty-p delimiter)))
           (car (last (split-string mailbox (regexp-quote delimiter)))))
          (t mailbox))))

(defun lispm-mail--default-place ()
  "Return the place listed first: the default account's Inbox."
  (when-let* ((account (lispm-mail-default-account)))
    (cons (lispm-mail-account-id account) "INBOX")))

;;;; The mailboxes

(defun lispm-mail--state-dot (id)
  "Return what says how account ID's connection is: nothing while it is
well, else a word of it."
  (pcase (lispm-mail-worker-state id)
    (`(connecting . ,_) (propertize " …" 'face 'lispm-mail-dim 'help-echo "Connecting…"))
    (`(offline . ,error) (propertize " ⚠" 'face 'lispm-mail-error 'help-echo error))
    (_ "")))

(defconst lispm-mail-sidebar-padding 6
  "Pixels from the folder list's left edge to its disclosure triangles.")


(defun lispm-mail--mailbox-line (place role title count &optional depth bold)
  "Insert the line of a folder PLACE, of ROLE, called TITLE, COUNT after it.
DEPTH indents it under the folder it is in; BOLD sets it so, as one
holding mail unread."
  (let ((start (point))
        (selected (equal place lispm-mail--place)))
    (insert (lispm-mail-style-space (+ lispm-mail-sidebar-padding 18 (* 16 (or depth 0))))
            (lispm-mail-style-icon (or role 'folder))
            (lispm-mail-style-space 6)
            (propertize title 'face (and bold 'lispm-mail-strong))
            (if (and count (> count 0))
                (propertize (format " (%d)" count) 'face (if bold 'lispm-mail-count 'lispm-mail-dim))
              "")
            (propertize "\n" 'line-spacing 3))
    (put-text-property start (point) 'lispm-mail-place place)
    (when selected
      (add-face-text-property start (point) 'lispm-mail-selected-active))))

(defun lispm-mail--mailboxes-width ()
  "Return the width the mailboxes are drawn at, in pixels."
  (if-let* ((window (or (lispm-mail--pane 'mailboxes)
                        (get-buffer-window lispm-mail-mailboxes-buffer-name t))))
      (window-body-width window t)
    220))

(defun lispm-mail--draw-mailboxes ()
  "Draw the folder list again: each account, a disclosure triangle and its
name, over its folders."
  (when-let* ((buffer (get-buffer lispm-mail-mailboxes-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (place (get-text-property (point) 'lispm-mail-place)))
        (erase-buffer)
        (insert (lispm-mail-style-blank-line))
        (when lispm-mail-outbox
          (lispm-mail--mailbox-line '(outbox) 'outbox "Outbox" (length lispm-mail-outbox) -1
                                    (seq-find (lambda (entry) (eq (plist-get entry :state) 'failed))
                                              lispm-mail-outbox)))
        (dolist (account lispm-mail-accounts)
          (let ((id (lispm-mail-account-id account)))
            (insert (lispm-mail-style-space lispm-mail-sidebar-padding)
                    (propertize "▾" 'face 'lispm-mail-dim)
                    (lispm-mail-style-space 5)
                    (lispm-mail-style-icon 'account)
                    (lispm-mail-style-space 6)
                    (propertize (lispm-mail-account-title account) 'face 'lispm-mail-account-heading)
                    (lispm-mail--state-dot id)
                    (propertize "\n" 'line-spacing 3))
            (let ((mailboxes (lispm-mail-mailboxes id)))
              (if (null mailboxes)
                  (lispm-mail--mailbox-line (cons id "INBOX") 'inbox "Inbox" nil)
                (dolist (mailbox mailboxes)
                  (let* ((name (plist-get mailbox :name))
                         (role (plist-get mailbox :role))
                         (delimiter (plist-get mailbox :delimiter))
                         (depth (if (and delimiter (not (string-empty-p delimiter)) (not role))
                                    (1- (length (split-string name (regexp-quote delimiter))))
                                  0))
                         (unseen (plist-get mailbox :unseen)))
                    (if (lispm-mail-mailbox-selectable-p mailbox)
                        (lispm-mail--mailbox-line
                         (cons id name) (or role 'folder) (lispm-mail--mailbox-title id name)
                         (if (eq role 'drafts) (plist-get mailbox :messages)
                           (and (not (memq role '(sent trash all))) unseen))
                         depth (and (memq role '(inbox nil)) unseen (> unseen 0)))
                      (unless (string-prefix-p "[" name)
                        (insert (lispm-mail-style-space (+ lispm-mail-sidebar-padding 18 (* 16 depth)))
                                (propertize (lispm-mail--mailbox-title id name) 'face 'lispm-mail-dim)
                                (propertize "\n" 'line-spacing 3)))))))))
          (insert (lispm-mail-style-blank-line)))
        (goto-char (point-min))
        (when-let* ((found (or (and place (text-property-any (point-min) (point-max)
                                                             'lispm-mail-place place))
                               (text-property-any (point-min) (point-max)
                                                  'lispm-mail-place lispm-mail--place))))
          (goto-char found))
        (when-let* ((window (get-buffer-window buffer t)))
          (set-window-point window (point)))))))

(defun lispm-mail-mailboxes-open ()
  "List the mailbox at point."
  (interactive)
  (when-let* ((place (get-text-property (point) 'lispm-mail-place)))
    (lispm-mail-show-place place)))

(defun lispm-mail-mailboxes-next ()
  "List the next mailbox."
  (interactive)
  (let ((here (get-text-property (point) 'lispm-mail-place))
        (point (point)))
    (when here
      (setq point (or (next-single-property-change point 'lispm-mail-place) (point-max))))
    (when-let* ((next (text-property-not-all point (point-max) 'lispm-mail-place nil)))
      (goto-char next)
      (lispm-mail-mailboxes-open))))

(defun lispm-mail-mailboxes-previous ()
  "List the previous mailbox."
  (interactive)
  (let ((start (line-beginning-position))
        (found nil))
    (save-excursion
      (goto-char start)
      (while (and (not found) (zerop (forward-line -1)))
        (when (get-text-property (point) 'lispm-mail-place)
          (setq found (point)))))
    (when found
      (goto-char found)
      (lispm-mail-mailboxes-open))))

(defun lispm-mail-mailboxes-click (event)
  "List the mailbox clicked, EVENT the click."
  (interactive "e")
  (let ((position (event-start event)))
    (with-selected-window (posn-window position)
      (goto-char (posn-point position))
      (lispm-mail-mailboxes-open))))

;;;; The list

(defun lispm-mail--sender (message)
  "Return whom MESSAGE is shown as from: its sender, or for mail sent, whom
it went to."
  (let ((from (plist-get message :from))
        (role (and (consp lispm-mail--place) (stringp (cdr lispm-mail--place))
                   (plist-get (lispm-mail-mailbox (car lispm-mail--place) (cdr lispm-mail--place)) :role))))
    (if (and (memq role '(sent drafts)) (plist-get message :to))
        (concat "To " (mapconcat (lambda (entry) (lispm-mail-address-text entry t))
                                 (plist-get message :to) ", "))
      (lispm-mail-address-text from t))))


(defun lispm-mail--subject (message)
  "Return MESSAGE's subject, or words saying it has none."
  (let ((subject (plist-get message :subject)))
    (if (string-blank-p (or subject "")) "(no subject)" (string-trim subject))))

(defun lispm-mail--entry (message)
  "Return MESSAGE as a row of the table: (UID [STATUS FLAG CLIP WHO SUBJECT DATE]).
An unread message's row is bold, and marked by a closed envelope; one
answered by an arrow."
  (let* ((unread (not (lispm-mail-seen-p message)))
         (face (and unread 'lispm-mail-strong))
         (flags (plist-get message :flags)))
    (list (plist-get message :uid)
          (vector (cond (unread (propertize (lispm-mail-style-icon 'unread 14) 'help-echo "Unread"))
                        ((lispm-mail-answered-p message)
                         (propertize (lispm-mail-style-icon 'reply 14) 'help-echo "Answered"))
                        ((member "$Forwarded" flags)
                         (propertize (lispm-mail-style-icon 'forward 14) 'help-echo "Forwarded"))
                        (t ""))
                  (if (lispm-mail-flagged-p message)
                      (propertize (lispm-mail-style-icon 'flag 14) 'help-echo "Flagged")
                    "")
                  (if (plist-get message :attachment)
                      (propertize (lispm-mail-style-icon 'clip 14) 'help-echo "Attachments")
                    "")
                  (propertize (lispm-mail--sender message) 'face face)
                  (propertize (replace-regexp-in-string "[\t\n]+" " " (lispm-mail--subject message))
                              'face face
                              'help-echo (let ((snippet (plist-get message :snippet)))
                                           (and snippet (not (string-empty-p snippet)) snippet)))
                  (propertize (lispm-mail-date-text (plist-get message :date)) 'face face)))))

(defun lispm-mail--sort-by (key)
  "Return a predicate ordering the table's rows by KEY of their messages."
  (lambda (a b)
    (pcase-let ((`(,id . ,mailbox) lispm-mail--place))
      (let ((ma (lispm-mail-message id mailbox (car a)))
            (mb (lispm-mail-message id mailbox (car b))))
        (pcase key
          (:date (< (or (plist-get ma :date) 0) (or (plist-get mb :date) 0)))
          (:from (string-collate-lessp (downcase (lispm-mail--sender ma))
                                       (downcase (lispm-mail--sender mb)) nil t))
          (:subject (string-collate-lessp (downcase (lispm-mail--subject ma))
                                          (downcase (lispm-mail--subject mb)) nil t)))))))

(defun lispm-mail--columns (width)
  "Return the table's columns for a window WIDTH columns wide, as
`tabulated-list-format' takes them; the Subject takes what the others
leave."
  (let* ((sent (and (consp lispm-mail--place) (stringp (cdr lispm-mail--place))
                    (memq (plist-get (lispm-mail-mailbox (car lispm-mail--place) (cdr lispm-mail--place)) :role)
                          '(sent drafts))))
         (who (max 14 (min 26 (/ width 4))))
         (date 11)
         (subject (max 16 (- width 9 who date 5))))
    (vector (list (propertize "*" 'display (and (lispm-mail-style-svg-p)
                                               (get-text-property 0 'display (lispm-mail-style-icon 'unread 12))))
                  3 nil)
            (list (propertize "*" 'display (and (lispm-mail-style-svg-p)
                                               (get-text-property 0 'display (lispm-mail-style-icon 'flag 12))))
                  3 nil)
            (list (propertize "*" 'display (and (lispm-mail-style-svg-p)
                                               (get-text-property 0 'display (lispm-mail-style-icon 'clip 12))))
                  3 nil)
            (list (if sent "To" "From") who (lispm-mail--sort-by :from))
            (list "Subject" subject (lispm-mail--sort-by :subject))
            (list "Received" date (lispm-mail--sort-by :date)))))

(defun lispm-mail--print-entry (id cols)
  "Print the row ID of COLS, as `tabulated-list-printer', the message's UID on it."
  (let ((start (point)))
    (tabulated-list-print-entry id cols)
    (put-text-property start (point) 'lispm-mail-uid id)
    (when (memq id lispm-mail--marked)
      (add-face-text-property start (point) 'lispm-mail-marked t))))

(defun lispm-mail--list-width ()
  "Return the width the table is drawn at, in columns."
  (if-let* ((window (or (lispm-mail--pane 'list)
                        (get-buffer-window lispm-mail-list-buffer-name t))))
      (window-body-width window)
    100))

(defun lispm-mail--draw-welcome ()
  "Draw the page asking for a first account."
  (cl-flet ((centred (text)
              (insert (propertize " " 'display `(space :align-to (- center (,(/ (lispm-mail-style-width text) 2)))))
                      text "\n")))
    (insert (lispm-mail-style-blank-line))
    (centred (lispm-mail-style-icon 'inbox 64))
    (insert (lispm-mail-style-blank-line))
    (centred (propertize "Welcome to Mail" 'face '(:weight bold :height 1.4)))
    (insert (lispm-mail-style-blank-line))
    (centred "Read and send your mail from Emacs: Gmail, iCloud, Outlook,")
    (centred "QQ Mail, NetEase, Fastmail, or any server speaking IMAP.")
    (insert (lispm-mail-style-blank-line))
    (let ((label " Add Account… "))
      (insert (propertize " " 'display `(space :align-to (- center (,(/ (lispm-mail-style-width label) 2))))))
      (insert-text-button label 'face 'lispm-mail-button 'mouse-face 'lispm-mail-button-pressed
                          'follow-link t 'help-echo "Add a mail account"
                          'action (lambda (_) (lispm-mail-add-account)))
      (insert "\n"))))

(defun lispm-mail--draw-outbox ()
  "List the Outbox's messages, each not sent or being sent."
  (dolist (entry lispm-mail-outbox)
    (let ((start (point))
          (failed (eq (plist-get entry :state) 'failed)))
      (insert (lispm-mail-style-space 8)
              (lispm-mail-style-icon (if failed 'junk 'outbox) 14)
              (lispm-mail-style-space 8)
              (propertize (plist-get entry :subject) 'face 'lispm-mail-strong)
              "   "
              (if failed
                  (propertize (concat "Not sent: " (plist-get entry :error)) 'face 'lispm-mail-error)
                (propertize "Sending…" 'face 'lispm-mail-dim))
              (lispm-mail-style-to 120 t)
              (propertize (lispm-mail-account-title (lispm-mail-account (plist-get entry :account)))
                          'face 'lispm-mail-dim)
              "\n")
      (put-text-property start (point) 'lispm-mail-outbox entry))))

(defun lispm-mail--empty (text &optional face)
  "Insert TEXT, saying the list is empty, centred under its top."
  (let ((text (propertize text 'face (or face 'lispm-mail-dim))))
    (insert (lispm-mail-style-blank-line)
            (propertize " " 'display `(space :align-to (- center (,(/ (lispm-mail-style-width text) 2)))))
            text "\n")))

(defun lispm-mail--draw-list ()
  "Draw the table again, the message at point kept there."
  (when-let* ((buffer (get-buffer lispm-mail-list-buffer-name)))
    (with-current-buffer buffer
      (let* ((inhibit-read-only t)
             (window (or (lispm-mail--pane 'list) (get-buffer-window buffer t)))
             (top lispm-mail--list-top)
             (start-uid (and window (not top) (get-text-property (window-start window) 'lispm-mail-uid)))
             (uid (and (not top)
                       (or (get-text-property (if window (window-point window) (point)) 'lispm-mail-uid)
                           (nth 2 lispm-mail--shown)))))
        (setq lispm-mail--list-top nil)
        (cond
         ((null lispm-mail-accounts)
          (setq tabulated-list-format [] tabulated-list-entries nil
                header-line-format nil)
          (erase-buffer)
          (lispm-mail--draw-welcome))
         ((equal lispm-mail--place '(outbox))
          (setq tabulated-list-format [] tabulated-list-entries nil
                header-line-format nil)
          (erase-buffer)
          (insert (lispm-mail-style-blank-line))
          (if lispm-mail-outbox
              (lispm-mail--draw-outbox)
            (lispm-mail--empty "The Outbox is empty.")))
         (t
          (let ((messages (lispm-mail--listed-messages))
                (state (lispm-mail-mailbox-state (car lispm-mail--place) (cdr lispm-mail--place))))
            (setq tabulated-list-format (lispm-mail--columns (lispm-mail--list-width))
                  tabulated-list-entries (mapcar #'lispm-mail--entry messages))
            (unless (and (consp tabulated-list-sort-key)
                         (seq-find (lambda (column) (equal (car column) (car tabulated-list-sort-key)))
                                   tabulated-list-format))
              (setq tabulated-list-sort-key (cons "Received" t)))
            (tabulated-list-init-header)
            (tabulated-list-print)
            (goto-char (point-max))
            (cond
             (messages
              (when (and (not lispm-mail--query)
                         (lispm-mail-mailbox-more-p (car lispm-mail--place) (cdr lispm-mail--place)))
                (insert (lispm-mail-style-blank-line)
                        (propertize " " 'display '(space :align-to (- center (70)))))
                (insert-text-button " Load More Messages " 'face 'lispm-mail-button
                                    'mouse-face 'lispm-mail-button-pressed 'follow-link t
                                    'action (lambda (_) (lispm-mail-load-more-messages)))
                (insert "\n")))
             (lispm-mail--query (lispm-mail--empty "No message kept here holds those words."))
             ((eq (car state) 'failed)
              (lispm-mail--empty (concat "Could not open it: " (cdr state)) 'lispm-mail-error))
             ((memq (car state) '(nil syncing)) (lispm-mail--empty "Fetching the messages…"))
             (t (lispm-mail--empty "No Messages"))))))
        (goto-char (or (and uid (text-property-any (point-min) (point-max) 'lispm-mail-uid uid))
                       (text-property-not-all (point-min) (point-max) 'lispm-mail-uid nil)
                       (point-min)))
        (when window
          (set-window-point window (point))
          (set-window-start window (or (and start-uid (text-property-any (point-min) (point-max)
                                                                         'lispm-mail-uid start-uid))
                                       (point-min))
                            t))
        (lispm-mail--highlight)
        ;; The message chosen is shown, the first when none was, left
        ;; unread until chosen.
        (when-let* ((uid (get-text-property (point) 'lispm-mail-uid))
                    ((not lispm-mail--shown))
                    ((consp lispm-mail--place))
                    ((stringp (cdr lispm-mail--place)))
                    ((lispm-mail--pane 'message)))
          (lispm-mail--show (car lispm-mail--place) (cdr lispm-mail--place) uid t))))))

(defvar-local lispm-mail--highlight-overlay nil
  "The overlay over the message chosen.")

(defun lispm-mail--row-bounds (&optional position)
  "Return the start and end of the row at POSITION, or nil."
  (let ((position (or position (point))))
    (when (get-text-property position 'lispm-mail-uid)
      (cons (or (previous-single-property-change (1+ position) 'lispm-mail-uid) (point-min))
            (or (next-single-property-change position 'lispm-mail-uid) (point-max))))))

(defun lispm-mail--highlight (&rest _)
  "Highlight the row at point: in the accent colour while the list is the
pane in front, in grey otherwise."
  (when-let* ((buffer (get-buffer lispm-mail-list-buffer-name)))
    (with-current-buffer buffer
      (unless (overlayp lispm-mail--highlight-overlay)
        (setq lispm-mail--highlight-overlay (make-overlay 1 1))
        (overlay-put lispm-mail--highlight-overlay 'priority 10))
      (let* ((window (or (lispm-mail--pane 'list) (get-buffer-window buffer t)))
             (bounds (lispm-mail--row-bounds (if window (window-point window) (point)))))
        (overlay-put lispm-mail--highlight-overlay 'face
                     (if (and window (eq window (selected-window)))
                         'lispm-mail-selected-active
                       'lispm-mail-selected))
        (if bounds
            (move-overlay lispm-mail--highlight-overlay (car bounds) (cdr bounds) buffer)
          (delete-overlay lispm-mail--highlight-overlay))))))

(defun lispm-mail-list-title ()
  "Return the title of the list: the mailbox listed, and its account's name."
  (pcase lispm-mail--place
    (`(outbox) "Outbox")
    (`(,id . ,mailbox)
     (if-let* ((account (lispm-mail-account id)))
         (format "%s — %s" (lispm-mail--mailbox-title id mailbox) (lispm-mail-account-title account))
       "Mail"))
    (_ "Mail")))

(defun lispm-mail-message-title ()
  "Return the title of the message shown: its subject."
  (pcase lispm-mail--shown
    (`(,id ,mailbox ,uid)
     (let ((subject (plist-get (lispm-mail-message id mailbox uid) :subject)))
       (if (string-blank-p (or subject "")) "(no subject)" subject)))
    (_ "No Message")))

(defun lispm-mail-list-status ()
  "Return what the list says of itself, as strings: its unread messages,
whether it is being updated, what went wrong."
  (pcase lispm-mail--place
    (`(outbox) (list (format "%d not sent" (length lispm-mail-outbox))))
    (`(,id . ,mailbox)
     (let ((unseen (plist-get (lispm-mail-mailbox id mailbox) :unseen))
           (state (lispm-mail-mailbox-state id mailbox))
           (connection (lispm-mail-worker-state id)))
       (delq nil
             (list (let ((count (length (lispm-mail--listed-messages))))
                     (concat (format "%d %s" count (if (= count 1) "message" "messages"))
                             (if (and unseen (> unseen 0)) (format ", %d unread" unseen) "")))
                   (cond ((eq (car connection) 'offline) (cdr connection))
                         ((eq (car state) 'syncing) "Updating…")
                         ((eq (car state) 'failed) (cdr state)))))))))

(defun lispm-mail-search-label ()
  "Return what the list is searched for, as a label, or nil."
  (and lispm-mail--query (format "Search: %s" lispm-mail--query)))

(defun lispm-mail--status-line ()
  "Return the list's status line, as a classic mail program had one under
its table: the mailbox, its account, how many messages, how many unread,
what is searched for, what goes on."
  (pcase lispm-mail--place
    ('nil "")
    (`(outbox) (concat " " (propertize "Outbox" 'face 'lispm-mail-title)
                       (format "   %d not sent" (length lispm-mail-outbox))))
    (`(,id . ,mailbox)
     (let* ((account (lispm-mail-account id))
            (shown (length tabulated-list-entries))
            (state (lispm-mail-mailbox-state id mailbox))
            (connection (lispm-mail-worker-state id))
            (unseen (plist-get (lispm-mail-mailbox id mailbox) :unseen))
            (going (cond ((eq (car connection) 'offline)
                          (propertize (cdr connection) 'face 'lispm-mail-error))
                         ((eq (car state) 'failed) (propertize (cdr state) 'face 'lispm-mail-error))
                         ((eq (car state) 'syncing) "Updating…")
                         ((eq (car connection) 'connecting) "Connecting…")
                         (t ""))))
       (concat " " (propertize (lispm-mail--mailbox-title id mailbox) 'face 'lispm-mail-title)
               (if account (concat " — " (lispm-mail-account-title account)) "")
               (format "    %d %s" shown (if (= shown 1) "message" "messages"))
               (if (and unseen (> unseen 0)) (format ", %d unread" unseen) "")
               (if lispm-mail--query (format "    Searching for “%s”" lispm-mail--query) "")
               (propertize " " 'display `(space :align-to (- right ,(+ 2 (string-width going)))))
               going)))))

;;;; The message

(defun lispm-mail--draw-message (&optional keep-point)
  "Draw the message shown again; KEEP-POINT keeps where it was read."
  (when-let* ((buffer (get-buffer lispm-mail-message-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (point (point)))
        (lispm-mail-view-release)
        (erase-buffer)
        (pcase lispm-mail--shown
          ('nil
           (insert "\n\n" (propertize " " 'display '(space :align-to (- center 9)))
                   (propertize "No message chosen" 'face 'lispm-mail-dim)))
          (`(,id ,mailbox ,uid)
           (let ((message (lispm-mail-message id mailbox uid))
                 (file (lispm-mail-message-file id mailbox uid)))
             (if (file-exists-p file)
                 (condition-case err
                     (lispm-mail-view-insert file message)
                   (error (insert "\n" (propertize (format "Could not show the message: %s"
                                                           (error-message-string err))
                                                   'face 'lispm-mail-error))))
               (when message
                 (lispm-mail-view--header message))
               (insert (propertize "Fetching the message…" 'face 'lispm-mail-dim) "\n")
               (lispm-mail-fetch id mailbox uid
                                 (lambda (_file error)
                                   (when (equal lispm-mail--shown (list id mailbox uid))
                                     (if error
                                         (with-current-buffer buffer
                                           (let ((inhibit-read-only t))
                                             (goto-char (point-max))
                                             (insert (propertize (concat "Could not fetch it: " error)
                                                                 'face 'lispm-mail-error))))
                                       (lispm-mail--draw-message))))))))
          (_ nil))
        (goto-char (if keep-point (min point (point-max)) (point-min)))
        (when-let* ((window (get-buffer-window buffer t)))
          (set-window-point window (point))
          (unless keep-point (set-window-start window (point-min))))))))

(defvar lispm-mail--read-timer nil
  "The timer marking the message shown read.")

(defun lispm-mail--show (id mailbox uid &optional unread)
  "Show message UID of MAILBOX of account ID; UNREAD leaves it unread."
  (unless (equal lispm-mail--shown (list id mailbox uid))
    (setq lispm-mail--shown (and uid (list id mailbox uid)))
    (let ((message-window (lispm-mail--pane 'message)))
      (when (and message-window
                 (not (eq (window-buffer message-window) (get-buffer lispm-mail-message-buffer-name)))
                 (not (with-current-buffer (window-buffer message-window)
                        (derived-mode-p 'lispm-mail-compose-mode))))
        (set-window-buffer message-window (lispm-mail--message-buffer))))
    (lispm-mail--draw-message)
    (when lispm-mail--read-timer
      (cancel-timer lispm-mail--read-timer)
      (setq lispm-mail--read-timer nil))
    (when-let* ((message (and uid (not unread) (lispm-mail-message id mailbox uid)))
                ((not (lispm-mail-seen-p message)))
                ((numberp lispm-mail-mark-read)))
      (if (<= lispm-mail-mark-read 0)
          (lispm-mail-set-flag id mailbox (list uid) "\\Seen" t)
        (setq lispm-mail--read-timer
              (run-at-time lispm-mail-mark-read nil
                           (lambda ()
                             (when (equal lispm-mail--shown (list id mailbox uid))
                               (lispm-mail-set-flag id mailbox (list uid) "\\Seen" t)))))))))

(defvar lispm-mail--show-timer nil
  "The timer showing the message chosen, a moment after it is.")

(defun lispm-mail--follow ()
  "Show the message at point, a moment after, unless another is chosen.
This is in `post-command-hook' of the list."
  (lispm-mail--highlight)
  (let ((uid (get-text-property (point) 'lispm-mail-uid)))
    (when (and uid (consp lispm-mail--place) (stringp (cdr lispm-mail--place))
               (not (equal (nth 2 lispm-mail--shown) uid)))
      (when lispm-mail--show-timer (cancel-timer lispm-mail--show-timer))
      (let ((place lispm-mail--place))
        (setq lispm-mail--show-timer
              (run-with-idle-timer 0.05 nil
                                   (lambda ()
                                     (setq lispm-mail--show-timer nil)
                                     (when (equal place lispm-mail--place)
                                       (lispm-mail--show (car place) (cdr place) uid)))))))))

;;;; Drawing again

(defvar lispm-mail--redraw-timer nil
  "The timer drawing again what changed, a moment after.")

(defvar lispm-mail--redraw-list nil
  "Non-nil when the list is to be drawn again.")

(defun lispm-mail--redraw ()
  "Draw again what changed."
  (setq lispm-mail--redraw-timer nil)
  (lispm-mail--draw-mailboxes)
  (when lispm-mail--redraw-list
    (setq lispm-mail--redraw-list nil)
    (lispm-mail--draw-list))
  (force-mode-line-update t))

(defun lispm-mail--redraw-changed (id mailbox)
  "Draw again, a moment after, what of account ID's MAILBOX changed.
This is one of `lispm-mail-changed-functions'."
  (when (or (null id) (null mailbox)
            (equal (cons id mailbox) lispm-mail--place)
            (equal lispm-mail--place '(outbox)))
    (setq lispm-mail--redraw-list t))
  (unless lispm-mail--redraw-timer
    (setq lispm-mail--redraw-timer (run-at-time 0.05 nil #'lispm-mail--redraw))))

(defun lispm-mail--watched ()
  "Return the mailbox listed, checked with the Inboxes.
This is one of `lispm-mail-watched-functions'."
  (and (consp lispm-mail--place) (stringp (cdr lispm-mail--place))
       (list lispm-mail--place)))

;;;; Commands: places

(defun lispm-mail-show-place (place)
  "List PLACE, (ID . MAILBOX) or (outbox)."
  (unless (equal place lispm-mail--place)
    (setq lispm-mail--place place
          lispm-mail--query nil
          lispm-mail--marked nil)
    (setq lispm-mail--shown nil
          lispm-mail--list-top t)
    (lispm-mail--draw-message))
  (lispm-mail--draw-mailboxes)
  (lispm-mail--draw-list)
  (when (and (consp place) (stringp (cdr place)))
    (let ((box (lispm-mail--box (car place) (cdr place))))
      (when (or (null (lispm-mail--box-synced box))
                (> (- (float-time) (lispm-mail--box-synced box)) 30))
        (lispm-mail-sync (car place) (cdr place))))))

(defun lispm-mail-go-to-mailbox ()
  "List a mailbox, chosen by its name."
  (interactive)
  (let* ((choices
          (apply #'append
                 (mapcar (lambda (account)
                           (let ((id (lispm-mail-account-id account)))
                             (mapcar (lambda (mailbox)
                                       (cons (format "%s — %s" (lispm-mail--mailbox-title id (plist-get mailbox :name))
                                                     (lispm-mail-account-title account))
                                             (cons id (plist-get mailbox :name))))
                                     (seq-filter #'lispm-mail-mailbox-selectable-p
                                                 (lispm-mail-mailboxes id)))))
                         lispm-mail-accounts)))
         (choice (completing-read "Go to mailbox: " choices nil t)))
    (lispm-mail-show-place (cdr (assoc choice choices)))))

(defun lispm-mail-get-mail ()
  "Check every account for new mail now."
  (interactive)
  (if (null lispm-mail-accounts)
      (lispm-mail-add-account)
    (lispm-mail-check)
    (message "Getting mail…")))

(defun lispm-mail-load-more-messages ()
  "List more of the mailbox's messages, older ones."
  (interactive)
  (when (and (consp lispm-mail--place) (stringp (cdr lispm-mail--place)))
    (lispm-mail-load-more (car lispm-mail--place) (cdr lispm-mail--place))))

(defun lispm-mail-search (words)
  "List only the messages holding WORDS, searched as they are typed.
Empty words list them all again."
  (interactive
   (let ((before lispm-mail--query))
     (condition-case nil
         (list (minibuffer-with-setup-hook
                   (lambda ()
                     (add-hook 'after-change-functions
                               (lambda (&rest _)
                                 (let ((words (minibuffer-contents-no-properties)))
                                   (setq lispm-mail--query (and (not (string-blank-p words)) words))
                                   (lispm-mail--draw-list)
                                   (force-mode-line-update t)))
                               nil t))
                 (read-string "Search messages: " lispm-mail--query)))
       (quit (setq lispm-mail--query before)
             (lispm-mail--draw-list)
             (signal 'quit nil)))))
  (setq lispm-mail--query (and (not (string-blank-p words)) words))
  (lispm-mail--draw-list)
  (force-mode-line-update t))

;;;; Commands: messages

(defun lispm-mail--targets (&optional one)
  "Return the messages a command acts on: (ID MAILBOX UIDS).
They are those selected together, unless ONE, else the one chosen."
  (unless (and (consp lispm-mail--place) (stringp (cdr lispm-mail--place)))
    (user-error "No message here"))
  (let* ((id (car lispm-mail--place))
         (mailbox (cdr lispm-mail--place))
         (uid (with-current-buffer (lispm-mail--list-buffer)
                (or (get-text-property (point) 'lispm-mail-uid)
                    (and (equal (seq-take lispm-mail--shown 2) (list id mailbox))
                         (nth 2 lispm-mail--shown))))))
    (cond ((and lispm-mail--marked (not one)) (list id mailbox lispm-mail--marked))
          (uid (list id mailbox (list uid)))
          (t (user-error "No message chosen")))))

(defun lispm-mail--after-taking (uids)
  "Choose the message after the last of UIDS, about to go from the list."
  (with-current-buffer (lispm-mail--list-buffer)
    (let* ((rows (let (rows)
                   (save-excursion
                     (goto-char (point-min))
                     (let (match)
                       (while (setq match (text-property-search-forward 'lispm-mail-uid nil nil t))
                         (push (prop-match-value match) rows))))
                   (nreverse rows)))
           (positions (delq nil (mapcar (lambda (uid) (seq-position rows uid)) uids)))
           (after (and positions
                       (or (seq-find (lambda (uid) (not (memq uid uids)))
                                     (nthcdr (1+ (apply #'max positions)) rows))
                           (seq-find (lambda (uid) (not (memq uid uids)))
                                     (reverse (seq-take rows (apply #'min positions))))))))
      (setq lispm-mail--marked nil)
      (when-let* ((found (and after (text-property-any (point-min) (point-max) 'lispm-mail-uid after))))
        (goto-char found))
      (when (and lispm-mail--shown (memq (nth 2 lispm-mail--shown) uids))
        (setq lispm-mail--shown nil)
        (if after
            (lispm-mail--show (car lispm-mail--place) (cdr lispm-mail--place) after)
          (lispm-mail--draw-message))))))

(defun lispm-mail--with-file (id mailbox uid function)
  "Call FUNCTION with the headers and file of message UID, once fetched."
  (let ((headers (lispm-mail-message id mailbox uid)))
    (lispm-mail-fetch id mailbox uid
                      (lambda (file error)
                        (if error
                            (message "Could not fetch the message: %s" error)
                          (funcall function
                                   (append headers (lispm-mail-view--file-headers file))
                                   file))))))

(defun lispm-mail-view--file-headers (file)
  "Return what the headers of FILE say that a list's message does not."
  (let ((own (car (lispm-mail-view-parse file))))
    (list :message-id (plist-get own :message-id)
          :references (plist-get own :references))))

(defun lispm-mail-new-message ()
  "Write a new message."
  (interactive)
  (let ((account (and (consp lispm-mail--place) (stringp (cdr lispm-mail--place))
                      (lispm-mail-account (car lispm-mail--place)))))
    (lispm-mail-compose :account account)))

(defun lispm-mail-reply-to-message (&optional all)
  "Reply to the message chosen; with ALL, to everyone it went to too."
  (interactive)
  (pcase-let ((`(,id ,mailbox ,uids) (lispm-mail--targets t)))
    (lispm-mail--with-file id mailbox (car uids)
                           (lambda (headers file) (lispm-mail-reply id headers file all)))))

(defun lispm-mail-reply-all ()
  "Reply to the message chosen, to everyone it went to."
  (interactive)
  (lispm-mail-reply-to-message t))

(defun lispm-mail-forward-message ()
  "Forward the message chosen."
  (interactive)
  (pcase-let ((`(,id ,mailbox ,uids) (lispm-mail--targets t)))
    (lispm-mail--with-file id mailbox (car uids)
                           (lambda (headers file) (lispm-mail-forward id headers file)))))

(defun lispm-mail-open-message ()
  "Open the message chosen: a draft to write on, else in the message pane."
  (interactive)
  (cond
   ((equal lispm-mail--place '(outbox))
    (when-let* ((entry (get-text-property (point) 'lispm-mail-outbox)))
      (lispm-mail-outbox-action entry)))
   (t
    (pcase-let ((`(,id ,mailbox ,uids) (lispm-mail--targets t)))
      (if (eq (plist-get (lispm-mail-mailbox id mailbox) :role) 'drafts)
          (lispm-mail--with-file id mailbox (car uids)
                                 (lambda (_headers file) (lispm-mail-edit-draft id file)))
        (lispm-mail--show id mailbox (car uids))
        (when-let* ((window (lispm-mail--pane 'message)))
          (select-window window)))))))

(defun lispm-mail-outbox-action (entry)
  "Send ENTRY of the Outbox again, or take it out."
  (pcase (read-multiple-choice (format "\"%s\": " (plist-get entry :subject))
                               '((?s "send again" "Try to send it again")
                                 (?d "delete" "Take it out of the Outbox, unsent")))
    (`(?s . ,_) (lispm-mail-outbox-send entry))
    (`(?d . ,_) (lispm-mail-outbox-remove entry))))

(defun lispm-mail-delete-message ()
  "Delete the messages chosen: to the Trash, or for good from it."
  (interactive)
  (pcase-let ((`(,id ,mailbox ,uids) (lispm-mail--targets)))
    (lispm-mail--after-taking uids)
    (lispm-mail-delete id mailbox uids)))

(defun lispm-mail-archive-message ()
  "Archive the messages chosen."
  (interactive)
  (pcase-let ((`(,id ,mailbox ,uids) (lispm-mail--targets)))
    (let ((archive (or (lispm-mail-archive-mailbox id)
                       (user-error "%s has no Archive mailbox"
                                   (lispm-mail-account-title (lispm-mail-account id))))))
      (when (equal archive mailbox)
        (user-error "They are in the archive already"))
      (lispm-mail--after-taking uids)
      (lispm-mail-move id mailbox uids archive))))

(defun lispm-mail-junk-message ()
  "Move the messages chosen to Junk, or out of it to the Inbox."
  (interactive)
  (pcase-let ((`(,id ,mailbox ,uids) (lispm-mail--targets)))
    (let* ((junk (or (lispm-mail-mailbox-of-role id 'junk)
                     (user-error "This account has no Junk mailbox")))
           (to (if (equal junk mailbox) "INBOX" junk)))
      (lispm-mail--after-taking uids)
      (lispm-mail-move id mailbox uids to))))

(defun lispm-mail-move-message (to)
  "Move the messages chosen to the mailbox TO."
  (interactive
   (let* ((id (car (lispm-mail--targets)))
          (names (mapcar (lambda (mailbox) (plist-get mailbox :name))
                         (seq-filter #'lispm-mail-mailbox-selectable-p (lispm-mail-mailboxes id)))))
     (list (completing-read "Move to: " (remove (cdr lispm-mail--place) names) nil t))))
  (pcase-let ((`(,id ,mailbox ,uids) (lispm-mail--targets)))
    (lispm-mail--after-taking uids)
    (lispm-mail-move id mailbox uids to)))

(defun lispm-mail--chosen-message ()
  "Return the message chosen, a plist, or nil."
  (ignore-errors
    (pcase-let ((`(,id ,mailbox ,uids) (lispm-mail--targets t)))
      (lispm-mail-message id mailbox (car uids)))))

(defun lispm-mail-toggle-read ()
  "Mark the messages chosen read, or unread when the first is read."
  (interactive)
  (pcase-let ((`(,id ,mailbox ,uids) (lispm-mail--targets)))
    (let ((seen (lispm-mail-seen-p (lispm-mail-message id mailbox (car uids)))))
      (when lispm-mail--read-timer (cancel-timer lispm-mail--read-timer))
      (lispm-mail-set-flag id mailbox uids "\\Seen" (not seen)))))

(defun lispm-mail-toggle-flag ()
  "Flag the messages chosen, or no longer when the first is."
  (interactive)
  (pcase-let ((`(,id ,mailbox ,uids) (lispm-mail--targets)))
    (let ((flagged (lispm-mail-flagged-p (lispm-mail-message id mailbox (car uids)))))
      (lispm-mail-set-flag id mailbox uids "\\Flagged" (not flagged)))))

(defun lispm-mail-mark-all-read ()
  "Mark every message of the mailbox listed read."
  (interactive)
  (pcase-let ((`(,id . ,mailbox) lispm-mail--place))
    (unless (stringp mailbox) (user-error "No mailbox listed"))
    (let ((unread (mapcar (lambda (message) (plist-get message :uid))
                          (seq-remove #'lispm-mail-seen-p (lispm-mail-messages id mailbox)))))
      (if unread
          (lispm-mail-set-flag id mailbox unread "\\Seen" t)
        (message "Every message is read")))))

(defun lispm-mail-toggle-mark ()
  "Select the message chosen with the others, or no longer, and go to the next."
  (interactive)
  (with-current-buffer (lispm-mail--list-buffer)
    (when-let* ((uid (get-text-property (point) 'lispm-mail-uid)))
      (setq lispm-mail--marked (if (memq uid lispm-mail--marked)
                                   (delq uid lispm-mail--marked)
                                 (cons uid lispm-mail--marked)))
      (lispm-mail--draw-list)
      (lispm-mail-next-message))))

(defun lispm-mail-unmark-all ()
  "Select no message together."
  (interactive)
  (setq lispm-mail--marked nil)
  (lispm-mail--draw-list))

(defun lispm-mail-save-attachments ()
  "Save the attachments of the message shown."
  (interactive)
  (let ((handles (with-current-buffer (lispm-mail--message-buffer)
                   (lispm-mail-view-attachments-here))))
    (unless handles (user-error "The message has no attachments"))
    (let ((directory (read-directory-name "Save the attachments in: "
                                          (expand-file-name "~/Downloads/"))))
      (dolist (handle handles)
        (lispm-mail-view-save-attachment
         handle (expand-file-name (file-name-nondirectory (lispm-mail-view-attachment-name handle))
                                  directory))))))

(defun lispm-mail-show-pictures ()
  "Show the pictures from the web of the message shown, this once."
  (interactive)
  (with-current-buffer (lispm-mail--message-buffer)
    (lispm-mail-view-show-pictures)))

;;;; Commands: moving

(defun lispm-mail--move-row (direction &optional unread)
  "Choose the row DIRECTION rows away, 1 or -1; with UNREAD, the next unread."
  (with-current-buffer (lispm-mail--list-buffer)
    (let ((window (get-buffer-window (current-buffer) t))
          (found nil))
      (save-excursion
        (let ((bounds (lispm-mail--row-bounds)))
          (goto-char (if bounds (if (> direction 0) (cdr bounds) (car bounds)) (point)))
          (while (and (not found)
                      (if (> direction 0) (< (point) (point-max)) (> (point) (point-min))))
            (when (< direction 0) (backward-char))
            (let ((uid (get-text-property (point) 'lispm-mail-uid)))
              (if (and uid
                       (or (not unread)
                           (not (lispm-mail-seen-p (lispm-mail-message (car lispm-mail--place)
                                                                      (cdr lispm-mail--place) uid)))))
                  (setq found (text-property-any (point-min) (point-max) 'lispm-mail-uid uid))
                (if (> direction 0)
                    (goto-char (or (next-single-property-change (point) 'lispm-mail-uid) (point-max)))
                  (goto-char (or (previous-single-property-change (point) 'lispm-mail-uid)
                                 (point-min)))))))))
      (if (not found)
          (message (if (> direction 0) "No more messages" "No messages before"))
        (goto-char found)
        (when window (set-window-point window found))
        (lispm-mail--follow)))))

(defun lispm-mail-next-message ()
  "Choose the next message."
  (interactive)
  (lispm-mail--move-row 1))

(defun lispm-mail-previous-message ()
  "Choose the previous message."
  (interactive)
  (lispm-mail--move-row -1))

(defun lispm-mail-next-unread ()
  "Choose the next unread message."
  (interactive)
  (lispm-mail--move-row 1 t))

(defun lispm-mail--panes ()
  "Return the panes there are, in the order TAB goes through them."
  (delq nil (mapcar #'lispm-mail--pane '(mailboxes list message))))

(defun lispm-mail-next-pane (&optional backwards)
  "Go to the next pane, or with BACKWARDS the previous one."
  (interactive)
  (let* ((panes (lispm-mail--panes))
         (panes (if backwards (reverse panes) panes))
         (here (seq-position panes (selected-window))))
    (when panes
      (select-window (nth (mod (1+ (or here -1)) (length panes)) panes)))))

(defun lispm-mail-previous-pane ()
  "Go to the previous pane."
  (interactive)
  (lispm-mail-next-pane t))

(defun lispm-mail-toggle-mailboxes ()
  "Show the mailboxes on the left, or no longer."
  (interactive)
  (customize-save-variable 'lispm-mail-show-mailboxes (not lispm-mail-show-mailboxes))
  (lispm-mail--layout))

(defun lispm-mail-toggle-layout ()
  "Show the message beside the list, or below it."
  (interactive)
  (customize-save-variable 'lispm-mail-layout (if (eq lispm-mail-layout 'beside) 'below 'beside))
  (lispm-mail--layout))

(defun lispm-mail-quit ()
  "Close LispM Mail, the windows put back as they were."
  (interactive)
  (let ((saved lispm-mail--saved-windows))
    (setq lispm-mail--saved-windows nil)
    (dolist (name (list lispm-mail-mailboxes-buffer-name lispm-mail-message-buffer-name))
      (when-let* ((buffer (get-buffer name)))
        (dolist (window (get-buffer-window-list buffer nil t))
          (set-window-dedicated-p window nil))))
    (if (and saved (eq (window-configuration-frame saved) (selected-frame)))
        (set-window-configuration saved)
      (dolist (window (window-list nil 'never))
        (set-window-parameter window 'lispm-mail-pane nil))
      (bury-buffer (lispm-mail--list-buffer))
      (quit-window))))

;;;; Modes

(lispm-kit-define-commands lispm-mail-mode
  (lispm-mail-next-message :key ("n" "<down>" "C-n" "j") :help "Choose the next message")
  (lispm-mail-previous-message :key ("p" "<up>" "C-p" "k") :help "Choose the previous message")
  ;; The main commands, in the order of their buttons; their items are
  ;; in the menus below.
  (lispm-mail-get-mail :key "g" :help "Check every account for new mail" :label "Get Mail"
                       :button "Get Mail")
  (lispm-mail-new-message :key "c" :help "Write a new message" :label "New Message" :button "New"
                          :enabled lispm-mail-accounts)
  (lispm-mail-reply-to-message :key "r" :help "Reply to the sender" :label "Reply" :button "Reply"
                               :enabled (lispm-mail--chosen-message))
  (lispm-mail-reply-all :key "a" :help "Reply to everyone it went to" :label "Reply All"
                        :button "Reply All" :enabled (lispm-mail--chosen-message))
  (lispm-mail-forward-message :key "f" :help "Forward it" :label "Forward" :button "Forward"
                              :enabled (lispm-mail--chosen-message))
  (lispm-mail-archive-message :key "e" :help "Archive it" :label "Archive" :button "Archive"
                              :enabled (and (lispm-mail--chosen-message)
                                            (lispm-mail-archive-mailbox (car lispm-mail--place))))
  (lispm-mail-delete-message :key ("d" "<delete>" "<deletechar>") :help "Move it to the Trash"
                             :label "Delete" :button "Delete" :enabled (lispm-mail--chosen-message))
  (lispm-mail-search :key ("/" "s") :help "List the messages holding words" :label "Search…"
                     :button "Search" :enabled lispm-mail-accounts)
  (lispm-mail-settings :key "," :help "Accounts, checking, reading and writing"
                       :label "Settings…" :button "Settings")
  :menu "Mailbox"
  lispm-mail-get-mail
  (lispm-mail-go-to-mailbox :key "G" :help "List another mailbox, by its name"
                            :label "Go to Mailbox…" :enabled lispm-mail-accounts)
  lispm-mail-search
  (lispm-mail-load-more-messages :key "+" :help "List older messages too" :label "More Messages"
                                 :enabled (and (consp lispm-mail--place) (stringp (cdr lispm-mail--place))
                                               (lispm-mail-mailbox-more-p (car lispm-mail--place)
                                                                          (cdr lispm-mail--place))))
  (lispm-mail-mark-all-read :key "M" :help "Mark every message of the mailbox read"
                            :label "Mark All as Read" :enabled (consp lispm-mail--place))
  "---"
  lispm-mail-settings
  (lispm-mail-quit :key "q" :help "Close Mail" :label "Close Mail")
  :menu "Message"
  lispm-mail-new-message
  lispm-mail-reply-to-message
  lispm-mail-reply-all
  lispm-mail-forward-message
  (lispm-mail-open-message :key "RET" :help "Read it, or write on a draft" :label "Open")
  "---"
  lispm-mail-archive-message
  lispm-mail-delete-message
  (lispm-mail-junk-message :key "J" :help "Move it to Junk, or out of it" :label "Junk"
                           :enabled (lispm-mail--chosen-message))
  (lispm-mail-move-message :key "m" :help "Move it to another mailbox" :label "Move to…"
                           :enabled (lispm-mail--chosen-message))
  "---"
  (lispm-mail-toggle-read :key "u" :help "Mark it read, or unread"
                          :label (if (lispm-mail-seen-p (lispm-mail--chosen-message))
                                     "Mark as Unread" "Mark as Read")
                          :enabled (lispm-mail--chosen-message))
  (lispm-mail-toggle-flag :key ("!" "F") :help "Flag it, or no longer" :label "Flag"
                          :style toggle :selected (lispm-mail-flagged-p (lispm-mail--chosen-message))
                          :enabled (lispm-mail--chosen-message))
  (lispm-mail-toggle-mark :key "x" :help "Select it with others, or no longer" :label "Select"
                          :enabled (lispm-mail--chosen-message))
  (lispm-mail-unmark-all :key "U" :help "Select none together" :label "Select None"
                         :enabled lispm-mail--marked)
  (lispm-mail-save-attachments :key "S" :help "Save the attachments of the message shown"
                               :label "Save Attachments…" :enabled lispm-mail--shown)
  (lispm-mail-show-pictures :key "P" :help "Show the pictures from the web, this once"
                            :label "Show Pictures" :enabled lispm-mail--shown)
  :menu "View"
  (lispm-mail-next-unread :key ("N" "M-n") :help "Choose the next unread message"
                          :label "Next Unread")
  (lispm-mail-next-pane :key "TAB" :help "Go to the next pane" :label "Next Pane")
  (lispm-mail-previous-pane :key "<backtab>" :help "Go to the previous pane" :label "Previous Pane")
  "---"
  (lispm-mail-toggle-mailboxes :key "\\" :help "Show the mailboxes, or no longer"
                               :label "Show Mailboxes" :style toggle :selected lispm-mail-show-mailboxes)
  (lispm-mail-toggle-layout :key "L" :help "Show the message beside the list, or below"
                            :label "Message Beside the List" :style toggle
                            :selected (eq lispm-mail-layout 'beside)))

(define-derived-mode lispm-mail-mode tabulated-list-mode "Mail"
  "LispM Mail: the messages of the mailbox chosen, a table with a column
each, sorted by clicking its name.
\\{lispm-mail-mode-map}"
  (setq buffer-read-only t
        truncate-lines t
        cursor-type nil
        tabulated-list-padding 1
        tabulated-list-printer #'lispm-mail--print-entry
        tabulated-list-sort-key (cons "Received" t))
  (lispm-mail-style-dress)
  ;; The columns' names and the status line in the window's own face.
  (dolist (face '(header-line mode-line-active mode-line-inactive))
    (face-remap-add-relative face 'lispm-mail-ui))
  ;; A status line under the table, as classic mail programs had.
  (setq-local mode-line-format '(:eval (lispm-mail--status-line))
              line-spacing 3
              revert-buffer-function (lambda (&rest _) (lispm-mail-get-mail)))
  (add-hook 'post-command-hook #'lispm-mail--follow nil t)
  (add-hook 'window-selection-change-functions #'lispm-mail--highlight nil t)
  (add-hook 'window-size-change-functions
            (lambda (_) (setq lispm-mail--redraw-list t) (lispm-mail--redraw-changed nil nil))
            nil t))

(defvar-keymap lispm-mail-mailboxes-mode-map
  :parent lispm-mail-mode-map
  "RET" #'lispm-mail-mailboxes-open
  "n" #'lispm-mail-mailboxes-next
  "p" #'lispm-mail-mailboxes-previous
  "<down>" #'lispm-mail-mailboxes-next
  "<up>" #'lispm-mail-mailboxes-previous
  "C-n" #'lispm-mail-mailboxes-next
  "C-p" #'lispm-mail-mailboxes-previous
  "j" #'lispm-mail-mailboxes-next
  "k" #'lispm-mail-mailboxes-previous
  "<mouse-1>" #'lispm-mail-mailboxes-click)

(define-derived-mode lispm-mail-mailboxes-mode special-mode "Folders"
  "LispM Mail's folder list: RET, or a click, lists a folder.
\\{lispm-mail-mailboxes-mode-map}"
  (setq buffer-read-only t
        truncate-lines t
        cursor-type nil
        mode-line-format nil)
  (lispm-mail-style-dress)
  (setq-local line-spacing 0))

(defvar-keymap lispm-mail-message-mode-map
  :parent lispm-mail-mode-map
  "SPC" #'lispm-mail-scroll-message
  "S-SPC" #'scroll-down-command
  "DEL" #'scroll-down-command
  "RET" #'push-button
  "<mouse-2>" #'push-button
  "TAB" #'lispm-mail-next-pane
  "C-n" #'next-line
  "C-p" #'previous-line
  "<down>" #'next-line
  "<up>" #'previous-line)

(define-derived-mode lispm-mail-message-mode special-mode "Message"
  "LispM Mail's message shown.
\\{lispm-mail-message-mode-map}"
  (setq buffer-read-only t
        mode-line-format nil)
  (lispm-mail-style-dress)
  (setq-local lispm-mail-view-redraw-function (lambda () (lispm-mail--draw-message t))
              cursor-in-non-selected-windows nil
              left-margin-width 2
              right-margin-width 2
              word-wrap t
              truncate-lines nil
              line-spacing 3)
  (visual-line-mode 1)
  (add-hook 'kill-buffer-hook #'lispm-mail-view-release nil t))

(defun lispm-mail-scroll-message ()
  "Read on in the message shown, and at its end, go to the next one."
  (interactive)
  (let ((window (or (lispm-mail--pane 'message) (selected-window))))
    (if (with-selected-window window
          (pos-visible-in-window-p (point-max) window))
        (lispm-mail-next-message)
      (with-selected-window window (scroll-up-command)))))

;;;; Emacs's mail

(defcustom lispm-mail-use-for-emacs t
  "Non-nil to make LispM Mail Emacs's mail.
C-x m and mailto: links then write with it, Read Mail opens it, and
the mail flag of `display-time-mode' shows its unread mail; each only
while left as Emacs has it, never over a choice of yours."
  :type 'boolean
  :group 'lispm-mail
  :set (lambda (symbol value)
         (set-default-toplevel-value symbol value)
         (when (fboundp 'lispm-mail--use-for-emacs)
           (lispm-mail--use-for-emacs value))))

;; In the autoloads whole, to run as Emacs starts without loading the rest.
;;;###autoload
(progn
  (defun lispm-mail--use-for-emacs-at-start ()
    "Make LispM Mail Emacs's mail as Emacs starts, before it is loaded,
unless `lispm-mail-use-for-emacs' was set to nil, or saved so."
    (let ((saved (get 'lispm-mail-use-for-emacs 'saved-value)))
      (unless (or (and (boundp 'lispm-mail-use-for-emacs) (not lispm-mail-use-for-emacs))
                  (and saved (not (ignore-errors (eval (car saved) t)))))
        (when (memq read-mail-command '(rmail nil))
          (setq read-mail-command 'lispm-mail))
        (when (memq mail-user-agent '(message-user-agent sendmail-user-agent))
          (setq mail-user-agent 'lispm-mail-user-agent))
        (when (and (boundp 'display-time-mail-function) (null display-time-mail-function))
          (setq display-time-mail-function 'lispm-mail-new-mail-p)))))
  ;; Installed while Emacs runs, it is so at once.
  (if after-init-time
      (lispm-mail--use-for-emacs-at-start)
    (add-hook 'after-init-hook #'lispm-mail--use-for-emacs-at-start)))

(defun lispm-mail--use-for-emacs (on)
  "Make LispM Mail Emacs's mail when ON, or no longer."
  (if on
      (progn
        (when (memq read-mail-command '(rmail nil))
          (setq read-mail-command #'lispm-mail))
        (when (memq mail-user-agent '(message-user-agent sendmail-user-agent))
          (setq mail-user-agent 'lispm-mail-user-agent))
        (when (and (boundp 'display-time-mail-function) (null display-time-mail-function))
          (setq display-time-mail-function #'lispm-mail-new-mail-p)))
    (when (eq read-mail-command #'lispm-mail)
      (setq read-mail-command 'rmail))
    (when (eq mail-user-agent 'lispm-mail-user-agent)
      (setq mail-user-agent 'message-user-agent))
    (when (and (boundp 'display-time-mail-function)
               (eq display-time-mail-function #'lispm-mail-new-mail-p))
      (setq display-time-mail-function nil))))

(defun lispm-mail-notify (id _mailbox messages)
  "Say that MESSAGES came for account ID.
This is one of `lispm-mail-new-mail-functions'."
  (let ((first (car messages)))
    (message "New mail%s: %s, \"%s\"%s"
             (if (cdr lispm-mail-accounts)
                 (format " for %s" (lispm-mail-account-title (lispm-mail-account id)))
               "")
             (lispm-mail-address-text (plist-get first :from) t)
             (plist-get first :subject)
             (if (cdr messages) (format " and %d more" (1- (length messages))) ""))))

(defun lispm-mail--show-compose (buffer)
  "Show BUFFER, a message written, in the message pane, or else popped to."
  (if-let* ((window (lispm-mail--pane 'message)))
      (progn (set-window-buffer window buffer)
             (select-window window))
    (pop-to-buffer buffer)))

(setq lispm-mail-compose-display-function #'lispm-mail--show-compose)

(defun lispm-mail--started ()
  "Hook LispM Mail into what tells it of changes, once."
  (add-hook 'lispm-mail-changed-functions #'lispm-mail--redraw-changed)
  (add-hook 'lispm-mail-watched-functions #'lispm-mail--watched)
  (add-hook 'lispm-mail-new-mail-functions #'lispm-mail-notify)
  (add-hook 'lispm-mail-settings-changed-hook #'lispm-mail--settings-changed)
  (unless lispm-mail-outbox
    (lispm-mail-outbox-load))
  (unless lispm-mail--timer
    (lispm-mail-start-checking)))

(defun lispm-mail--settings-changed ()
  "Show anew what the settings changed.
This is in `lispm-mail-settings-changed-hook'."
  (unless (and (consp lispm-mail--place)
               (or (equal lispm-mail--place '(outbox))
                   (lispm-mail-account (car lispm-mail--place))))
    (setq lispm-mail--place (lispm-mail--default-place)
          lispm-mail--shown nil))
  (when (get-buffer lispm-mail-list-buffer-name)
    (setq lispm-mail--redraw-list t)
    (lispm-mail--redraw)
    (lispm-mail--draw-message t))
  (lispm-mail-start-checking))

;;;###autoload
(defun lispm-mail ()
  "Open LispM Mail: the mailboxes, their messages and the message chosen."
  (interactive)
  (lispm-mail--started)
  (unless (or lispm-mail--saved-windows (lispm-mail--pane 'list))
    (setq lispm-mail--saved-windows (current-window-configuration)))
  (unless (and lispm-mail--place
               (or (equal lispm-mail--place '(outbox))
                   (lispm-mail-account (car lispm-mail--place))))
    (setq lispm-mail--place (lispm-mail--default-place)))
  (lispm-mail--layout)
  (lispm-mail--draw-mailboxes)
  (lispm-mail--draw-list)
  (lispm-mail--draw-message)
  (when lispm-mail-accounts
    (lispm-mail-check)))

(lispm-mail--use-for-emacs lispm-mail-use-for-emacs)

(provide 'lispm-mail)
;;; lispm-mail.el ends here
