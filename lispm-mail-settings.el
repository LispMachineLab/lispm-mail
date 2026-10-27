;;; lispm-mail-settings.el --- LispM Mail's settings, in windows  -*- lexical-binding: t; -*-

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

;; LispM Mail's settings, in a window of their own, which , opens: the
;; accounts, each edited in a form; how often mail is checked for; how
;; messages are shown and written; whether LispM Mail is Emacs's mail.
;; Everything is set with Emacs's widgets, buttons, check boxes and
;; pop-up menus, from the mouse or the keyboard, TAB going from one to
;; the next; each change is saved at once, with Customize.
;;
;; An account's form fills its servers in from its address: those of
;; the providers most people have are known, others are asked of
;; Thunderbird's database of providers, and any can be typed.  Test
;; Connection logs in to both servers, in the background, and says how
;; it went; Save keeps the account, its password in the Keychain or
;; with auth-source.
;;
;; Whatever shows the window may dress it: its pop-up menus choose
;; through `lispm-mail-settings-choose-function', and
;; `lispm-mail-settings-drawn-hook' runs each time it is drawn.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'wid-edit)
(require 'lispm-kit-commands)
(require 'lispm-mail-style)
(require 'lispm-mail-accounts)
(require 'lispm-mail-data)

(defvar lispm-mail-layout)
(defvar lispm-mail-mark-read)
(defvar lispm-mail-use-for-emacs)
(defvar lispm-mail-prefer-html)
(defvar lispm-mail-load-remote-pictures)
(defvar lispm-mail-reply-above)
(declare-function lispm-mail "lispm-mail" ())

(defvar lispm-mail-settings-changed-hook nil
  "Normal hook run once a setting is changed and saved.")

(defvar lispm-mail-settings-drawn-hook nil
  "Normal hook run in the settings' and an account form's buffer, drawn.")

(defvar lispm-mail-settings-glyph-function nil
  "Function giving the picture of a control of the settings, or nil for text.
It is called with KIND, STATE and LABEL:
  KIND `check', STATE `on' or `off', LABEL nil;
  KIND `button', STATE nil, or `default' for the button C-c C-c
       presses, LABEL its label;
  KIND `popup', a pop-up menu, STATE nil, LABEL what it shows.
It returns an image, or nil for the control drawn as text.  Each
control's widget says what it is in its :lispm-mail-control: `check',
its state its value, or (KIND STATE LABEL).")

(defvar lispm-mail-settings-choose-function #'lispm-mail-settings-choose
  "Function showing a pop-up menu's choices; it returns the one chosen.
It is called with the menu's TITLE, its CHOICES, a list of (VALUE .
TEXT), and the VALUE chosen now, with point on the menu.  It returns
\(VALUE), or nil when none was chosen.")

(defconst lispm-mail-settings-buffer-name "*Mail Settings*"
  "The name of the settings' buffer.")

(defconst lispm-mail-account-buffer-name "*Mail Account*"
  "The name of the buffer of an account's form.")

(defface lispm-mail-settings-heading '((t :inherit bold :height 1.1))
  "Face of a heading of the settings."
  :group 'lispm-mail)

(defface lispm-mail-settings-title '((t :inherit bold :height 1.4))
  "Face of the settings' title."
  :group 'lispm-mail)

(defface lispm-mail-settings-label '((t :inherit default))
  "Face of a setting's name."
  :group 'lispm-mail)

(defface lispm-mail-settings-note '((t :inherit shadow))
  "Face of what is said of a setting."
  :group 'lispm-mail)

(defface lispm-mail-settings-good '((t :inherit success))
  "Face of what went well, as a server logged in to."
  :group 'lispm-mail)

(defface lispm-mail-settings-bad '((t :inherit error))
  "Face of what went wrong."
  :group 'lispm-mail)

(defface lispm-mail-settings-button '((t :inherit lispm-mail-button))
  "Face of a button, and of a pop-up menu, of the settings."
  :group 'lispm-mail)

(defface lispm-mail-settings-default-button
  '((((type graphic)) :inherit lispm-mail-button :weight bold
     :box (:line-width 3 :style released-button))
    (t :inherit lispm-mail-button))
  "Face of the button pressed by C-c C-c, outlined as the classic Mac's
default button was."
  :group 'lispm-mail)

(defun lispm-mail-settings--buttons ()
  "Draw the current buffer's buttons as buttons, not between brackets."
  (setq-local widget-push-button-prefix " "
              widget-push-button-suffix " "
              widget-button-face 'lispm-mail-settings-button
              widget-button-pressed-face 'lispm-mail-settings-button
              widget-mouse-face 'highlight))

(defun lispm-mail-settings--glyph (kind state &optional label)
  "Return the picture of a control, see `lispm-mail-settings-glyph-function'."
  (when lispm-mail-settings-glyph-function
    (funcall lispm-mail-settings-glyph-function kind state label)))

(defun lispm-mail-settings--button (tag action &optional default)
  "Insert a button TAG doing ACTION; DEFAULT, the one C-c C-c presses."
  (widget-create 'push-button :tag tag
                 :tag-glyph (lispm-mail-settings--glyph 'button (and default 'default) tag)
                 :button-face (if default 'lispm-mail-settings-default-button 'lispm-mail-settings-button)
                 :lispm-mail-control (list 'button (and default 'default) tag)
                 :action (lambda (&rest _) (funcall action))))

;;;; Choosing

(defun lispm-mail-settings-choose (title choices value)
  "Show CHOICES, (VALUE . TEXT), of the pop-up menu TITLE; return (VALUE).
VALUE is the one chosen now.  A menu where Emacs pops menus up, the
minibuffer elsewhere."
  (if (and (display-popup-menus-p) (posn-at-point))
      (let* ((posn (posn-at-point))
             (xy (posn-x-y posn))
             (chosen (x-popup-menu
                      (list (list (car xy) (+ (cdr xy) (frame-char-height))) (posn-window posn))
                      (list title (cons "" (mapcar (lambda (choice)
                                                     (cons (if (equal (car choice) value)
                                                               (concat "✓ " (cdr choice))
                                                             (concat "   " (cdr choice)))
                                                           (list (car choice))))
                                                   choices))))))
        chosen)
    (let* ((texts (mapcar #'cdr choices))
           (text (completing-read (format "%s: " title) texts nil t nil nil
                                  (cdr (assoc value choices)))))
      (when-let* ((choice (rassoc text choices)))
        (list (car choice))))))

(defun lispm-mail-settings--popup (title choices value on-change &optional width)
  "Insert a pop-up menu TITLE of CHOICES, (VALUE . TEXT), showing VALUE.
ON-CHANGE is called with the value chosen.  WIDTH pads its text."
  (let ((text (or (cdr (assoc value choices)) (format "%s" value))))
    (widget-create 'push-button
                   :tag (concat (string-pad text (or width 0)) " ▾")
                   :tag-glyph (lispm-mail-settings--glyph 'popup nil text)
                   :button-face 'lispm-mail-settings-button
                   :lispm-mail-control (list 'popup nil text)
                   :help-echo (format "Choose %s" (downcase title))
                   :action (lambda (&rest _)
                             (when-let* ((chosen (funcall lispm-mail-settings-choose-function
                                                          title choices value)))
                               (funcall on-change (car chosen)))))))

(defconst lispm-mail-settings-column 28
  "The column the settings' values start at.")

(defun lispm-mail-settings--label (text &optional column)
  "Insert TEXT, a setting's name, and align what follows to COLUMN."
  (widget-insert "    " (propertize text 'face 'lispm-mail-settings-label)
                 (propertize " " 'display `(space :align-to ,(or column lispm-mail-settings-column)))))

(defun lispm-mail-settings--heading (text)
  "Insert TEXT, a heading."
  (widget-insert "\n  " (propertize text 'face 'lispm-mail-settings-heading) "\n\n"))

(defun lispm-mail-settings--note (text)
  "Insert TEXT, said of the setting above, on a line of its own."
  (widget-insert (propertize " " 'display `(space :align-to ,lispm-mail-settings-column))
                 (propertize text 'face 'lispm-mail-settings-note
                             'wrap-prefix `(space :align-to ,lispm-mail-settings-column))
                 "\n"))

(defun lispm-mail-settings--show (buffer)
  "Show BUFFER, of the settings, in the selected window; over the whole
frame when it is one of the mail's panes."
  (when (window-parameter (selected-window) 'lispm-mail-pane)
    (dolist (window (window-list nil 'never))
      (set-window-parameter window 'lispm-mail-pane nil)
      (set-window-dedicated-p window nil))
    (delete-other-windows))
  (switch-to-buffer buffer))

(defun lispm-mail-settings--save (variable value)
  "Set VARIABLE to VALUE, save it for next time, and say so."
  (customize-save-variable variable value)
  (run-hooks 'lispm-mail-settings-changed-hook))

(defun lispm-mail-settings--clear-widgets ()
  "Empty the current buffer of its widgets, to draw it again."
  (let ((inhibit-read-only t))
    ;; Fields left in these lists would overlap the new ones.
    (setq widget-field-list nil
          widget-field-new nil
          widget-field-last nil)
    (remove-overlays)
    (erase-buffer)))

;;;; The settings

(defvar-local lispm-mail-settings--back nil
  "The buffer the settings were opened from, to go back to.")

(defun lispm-mail-settings--account-line (account)
  "Insert ACCOUNT's line in the settings: its name, address and buttons."
  (let* ((id (lispm-mail-account-id account))
         (state (car (lispm-mail-worker-state id)))
         (default (eq account (lispm-mail-default-account))))
    (widget-insert "    " (pcase state
                          ('online (propertize "●" 'face 'lispm-mail-settings-good))
                          ('offline (propertize "●" 'face 'lispm-mail-settings-bad))
                          (_ (propertize "○" 'face 'lispm-mail-settings-note)))
                   " " (propertize (lispm-mail-account-title account) 'face 'bold)
                   (propertize " " 'display `(space :align-to ,lispm-mail-settings-column))
                   (propertize (concat (plist-get account :address) (if default " · default" ""))
                               'face 'lispm-mail-settings-note)
                   (propertize " " 'display `(space :align-to ,(+ lispm-mail-settings-column 34))))
    (lispm-mail-settings--button "Edit…" (lambda () (lispm-mail-edit-account account)))
    (widget-insert " ")
    (lispm-mail-settings--button "Remove" (lambda () (lispm-mail-remove-account account)))
    (widget-insert "\n")
    (pcase (lispm-mail-worker-state id)
      (`(offline . ,error)
       (widget-insert "      " (propertize error 'face 'lispm-mail-settings-bad) "\n")))))

(defun lispm-mail-settings--draw ()
  "Draw the settings in the current buffer."
  (let ((point (point)))
    (lispm-mail-settings--clear-widgets)
    (widget-insert "\n  " (propertize "Mail Settings" 'face 'lispm-mail-settings-title) "\n")
    (lispm-mail-settings--heading "Accounts")
    (if lispm-mail-accounts
        (mapc #'lispm-mail-settings--account-line lispm-mail-accounts)
      (widget-insert "    " (propertize "No account yet: add one to read and send mail."
                                      'face 'lispm-mail-settings-note)
                     "\n"))
    (widget-insert "\n    ")
    (lispm-mail-settings--button "Add Account…" #'lispm-mail-add-account)
    (widget-insert "\n")
    (when (cdr lispm-mail-accounts)
      (widget-insert "\n")
      (lispm-mail-settings--label "Write new mail from")
      (lispm-mail-settings--popup
       "Write new mail from"
       (mapcar (lambda (account) (cons (lispm-mail-account-id account) (lispm-mail-account-title account)))
               lispm-mail-accounts)
       (lispm-mail-account-id (lispm-mail-default-account))
       (lambda (id) (lispm-mail-settings--save 'lispm-mail-default-account id)))
      (widget-insert "\n"))
    (lispm-mail-settings--heading "Checking for Mail")
    (lispm-mail-settings--label "Check for new mail")
    (lispm-mail-settings--popup "Check for new mail"
                                '((1 . "Every minute") (5 . "Every 5 minutes") (15 . "Every 15 minutes")
                                  (30 . "Every 30 minutes") (60 . "Every hour") (nil . "Only when asked"))
                                lispm-mail-check-interval
                                (lambda (value) (lispm-mail-settings--save 'lispm-mail-check-interval value)))
    (widget-insert "\n")
    (lispm-mail-settings--label "Messages listed at first")
    (lispm-mail-settings--popup "Messages listed at first"
                                '((50 . "The latest 50") (100 . "The latest 100") (200 . "The latest 200")
                                  (500 . "The latest 500"))
                                lispm-mail-window-size
                                (lambda (value) (lispm-mail-settings--save 'lispm-mail-window-size value)))
    (widget-insert "\n")
    (lispm-mail-settings--heading "Reading")
    (lispm-mail-settings--label "Show the message")
    (lispm-mail-settings--popup "Show the message" '((below . "Below the list") (beside . "Beside the list"))
                                lispm-mail-layout
                                (lambda (value)
                                  (lispm-mail-settings--save 'lispm-mail-layout value)))
    (widget-insert "\n")
    (lispm-mail-settings--label "Mark messages read")
    (lispm-mail-settings--popup "Mark messages read"
                                '((0 . "As soon as shown") (2 . "After 2 seconds") (5 . "After 5 seconds")
                                  (nil . "Never"))
                                lispm-mail-mark-read
                                (lambda (value) (lispm-mail-settings--save 'lispm-mail-mark-read value)))
    (widget-insert "\n")
    (lispm-mail-settings--check "Show HTML when a message has it, rather than its plain text"
                                'lispm-mail-prefer-html)
    (lispm-mail-settings--check "Load pictures from the web" 'lispm-mail-load-remote-pictures)
    (lispm-mail-settings--note "They tell senders that you read their message, and when.")
    (lispm-mail-settings--heading "Writing")
    (lispm-mail-settings--check "Write replies above the message they quote" 'lispm-mail-reply-above)
    (lispm-mail-settings--heading "Emacs")
    (lispm-mail-settings--check "Make Mail Emacs's mail: C-x m, mailto: links, Read Mail"
                                'lispm-mail-use-for-emacs)
    (lispm-mail-settings--label "Keep passwords in")
    (lispm-mail-settings--popup "Keep passwords in"
                                (append (and (eq system-type 'darwin)
                                             '((keychain . "The macOS Keychain")))
                                        '((auth-source . "auth-source (~/.authinfo…)")))
                                lispm-mail-password-store
                                (lambda (value) (lispm-mail-settings--save 'lispm-mail-password-store value)))
    (widget-insert "\n")
    (lispm-mail-settings--note "Passwords kept before stay where they are: save each account again to move them.")
    (widget-insert "\n\n    ")
    (lispm-mail-settings--button "Done" #'lispm-mail-settings-done t)
    (widget-insert "\n")
    (widget-setup)
    (goto-char (min point (point-max)))
    (run-hooks 'lispm-mail-settings-drawn-hook)))

(defun lispm-mail-settings--check (text variable)
  "Insert a check box TEXT, for VARIABLE, on or off."
  (widget-insert "    ")
  (widget-create 'checkbox :value (symbol-value variable)
                 :on-glyph (lispm-mail-settings--glyph 'check 'on)
                 :off-glyph (lispm-mail-settings--glyph 'check 'off)
                 :lispm-mail-control 'check
                 :notify (lambda (widget &rest _)
                           (lispm-mail-settings--save variable (widget-value widget))))
  (widget-insert " " text "\n"))

(defun lispm-mail-settings-refresh ()
  "Draw the settings again, when shown."
  (when-let* ((buffer (get-buffer lispm-mail-settings-buffer-name)))
    (with-current-buffer buffer
      (lispm-mail-settings--draw))))

(defun lispm-mail-settings-done ()
  "Go back to where the settings were opened from."
  (interactive)
  (let ((back lispm-mail-settings--back))
    (cond ((and (buffer-live-p back)
                (not (string-prefix-p "lispm-mail-" (symbol-name (buffer-local-value 'major-mode back)))))
           (switch-to-buffer back))
          ((or lispm-mail-accounts (buffer-live-p back)) (lispm-mail))
          (t (quit-window)))))

(defun lispm-mail-settings--changed-elsewhere (_id mailbox)
  "Draw the settings again when an account's state changed.
This is one of `lispm-mail-changed-functions'."
  (unless mailbox
    (when-let* ((buffer (get-buffer lispm-mail-settings-buffer-name))
                ((get-buffer-window buffer t)))
      (run-at-time 0 nil #'lispm-mail-settings-refresh))))

(lispm-kit-define-commands lispm-mail-settings-mode
  :menu "Settings"
  (lispm-mail-add-account :key "a" :help "Add a mail account" :label "Add Account…"
                          :button "Add Account")
  "---"
  (lispm-mail-settings-done :key ("q" "C-c C-c") :help "Go back to the mail" :label "Done"
                            :button "Done"))

(define-derived-mode lispm-mail-settings-mode nil "Mail Settings"
  "LispM Mail's settings: TAB goes from one to the next, RET presses it.
\\{lispm-mail-settings-mode-map}"
  (setq truncate-lines nil)
  (setq-local line-spacing 0.2
              word-wrap t)
  (lispm-mail-settings--buttons)
  (lispm-mail-style-dress)
  (add-hook 'lispm-mail-changed-functions #'lispm-mail-settings--changed-elsewhere))

(set-keymap-parent lispm-mail-settings-mode-map widget-keymap)

;;;###autoload
(defun lispm-mail-settings ()
  "Show LispM Mail's settings."
  (interactive)
  (let ((back (current-buffer))
        (buffer (get-buffer-create lispm-mail-settings-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'lispm-mail-settings-mode)
        (lispm-mail-settings-mode))
      (unless (or (eq back buffer)
                  (with-current-buffer back (derived-mode-p 'lispm-mail-account-mode)))
        (setq lispm-mail-settings--back back))
      (lispm-mail-settings--draw)
      (goto-char (point-min))
      (widget-forward 1))
    (lispm-mail-settings--show buffer)
    buffer))

;;;; An account's form

(defvar lispm-mail-account-field-map)
(defvar lispm-mail-account-text-map)

(defvar-local lispm-mail-account--editing nil
  "The account the form edits, or nil for a new one.")

(defvar-local lispm-mail-account--widgets nil
  "The form's fields: an alist (KEY . WIDGET).")

(defvar-local lispm-mail-account--values nil
  "The form's values not in fields: a plist, :imap-security, :smtp-security, :sent.")

(defvar-local lispm-mail-account--edited nil
  "Non-nil once the servers were typed, not to be filled in again.")

(defvar-local lispm-mail-account--note nil
  "What is said of the provider, its password above all, or nil.")

(defvar-local lispm-mail-account--status nil
  "What Test Connection found: nil, `checking', or (:imap R :smtp R).")

(defvar-local lispm-mail-account--status-start nil
  "Where the form says what Test Connection found.")

(defvar-local lispm-mail-account--status-end nil
  "Where what Test Connection found ends.")

(defconst lispm-mail-security-choices
  '((tls . "SSL/TLS") (starttls . "STARTTLS") (none . "None"))
  "The securities a server's connection may have.")

(defun lispm-mail-account--value (key)
  "Return the value of the form's field KEY, trimmed."
  (if-let* ((widget (alist-get key lispm-mail-account--widgets)))
      (let ((value (widget-value widget)))
        (if (stringp value) (string-trim value) value))
    (plist-get lispm-mail-account--values key)))

(defun lispm-mail-account--set (key value)
  "Set the form's field KEY to VALUE."
  (if-let* ((widget (alist-get key lispm-mail-account--widgets)))
      (save-excursion (widget-value-set widget (or value "")))
    (setq lispm-mail-account--values (plist-put lispm-mail-account--values key value))))

(defun lispm-mail-account--field (key label value &rest properties)
  "Insert the field KEY, LABEL, holding VALUE; PROPERTIES go to its widget."
  (lispm-mail-settings--label label)
  (push (cons key (apply #'widget-create 'editable-field
                         :value (or value "")
                         :size 30
                         :keymap lispm-mail-account-field-map
                         :lispm-mail-control 'field
                         properties))
        lispm-mail-account--widgets)
  (widget-insert "\n"))

(defun lispm-mail-account--fill (settings)
  "Fill the servers in from SETTINGS, as `lispm-mail-provider-settings' gives them."
  (let ((imap (plist-get settings :imap))
        (smtp (plist-get settings :smtp)))
    (lispm-mail-account--set :imap-host (plist-get imap :host))
    (lispm-mail-account--set :imap-port (number-to-string (plist-get imap :port)))
    (lispm-mail-account--set :imap-user (plist-get imap :user))
    (lispm-mail-account--set :imap-security (plist-get imap :security))
    (lispm-mail-account--set :smtp-host (plist-get smtp :host))
    (lispm-mail-account--set :smtp-port (number-to-string (plist-get smtp :port)))
    (lispm-mail-account--set :smtp-user (plist-get smtp :user))
    (lispm-mail-account--set :smtp-security (plist-get smtp :security))
    (lispm-mail-account--set :sent (plist-get settings :sent))
    (setq lispm-mail-account--note
          (concat (if (plist-get settings :provider)
                      (format "Servers of %s filled in." (plist-get settings :provider))
                    "Servers guessed from the address: check them with Test Connection.")
                  (if (plist-get settings :note) (concat "  " (plist-get settings :note)) "")))
    (lispm-mail-account--redraw)))

(defun lispm-mail-account--address-changed (widget &rest _)
  "Fill the servers in from the address WIDGET holds, if its provider is known."
  (let ((address (string-trim (widget-value widget))))
    (unless lispm-mail-account--edited
      (when-let* ((settings (lispm-mail-provider-settings address)))
        (let ((buffer (current-buffer)))
          ;; Not while the field is being changed.
          (run-at-time 0 nil (lambda ()
                               (when (buffer-live-p buffer)
                                 (with-current-buffer buffer
                                   (unless (equal (lispm-mail-account--value :imap-host)
                                                  (plist-get (plist-get settings :imap) :host))
                                     (lispm-mail-account--fill settings)))))))))))

(defun lispm-mail-account-look-up ()
  "Fill the servers in from the address: known, asked of Thunderbird's
database of providers, or guessed."
  (interactive)
  (let ((address (lispm-mail-account--value :address))
        (buffer (current-buffer)))
    (unless (lispm-mail-domain address)
      (user-error "Type the address first, as you@example.com"))
    (setq lispm-mail-account--edited nil)
    (if-let* ((settings (lispm-mail-provider-settings address)))
        (lispm-mail-account--fill settings)
      (setq lispm-mail-account--note "Looking the servers up…")
      (lispm-mail-account--redraw)
      (lispm-mail-ask-autoconfig
       address
       (lambda (settings)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (lispm-mail-account--fill (or settings (lispm-mail-guess-settings address))))))))))

(defun lispm-mail-account--server (which)
  "Return the form's server WHICH, `imap' or `smtp', as an account keeps it."
  (let ((prefix (symbol-name which)))
    (list :host (lispm-mail-account--value (intern (format ":%s-host" prefix)))
          :port (string-to-number (or (lispm-mail-account--value (intern (format ":%s-port" prefix))) "0"))
          :security (lispm-mail-account--value (intern (format ":%s-security" prefix)))
          :user (lispm-mail-account--value (intern (format ":%s-user" prefix))))))

(defun lispm-mail-account--password ()
  "Return the password of the form: typed, or kept for the account edited."
  (let ((typed (lispm-mail-account--value :password)))
    (if (and typed (not (string-empty-p typed)))
        typed
      (and lispm-mail-account--editing (lispm-mail-password lispm-mail-account--editing)))))

(defun lispm-mail-account--problem ()
  "Return what is missing from the form, or nil."
  (let ((address (lispm-mail-account--value :address)))
    (cond ((not (lispm-mail-domain address)) "Type the email address, as you@example.com")
          ((string-empty-p (or (lispm-mail-account--value :imap-host) "")) "Type the incoming mail server")
          ((string-empty-p (or (lispm-mail-account--value :smtp-host) "")) "Type the outgoing mail server")
          ((<= (plist-get (lispm-mail-account--server 'imap) :port) 0) "Type the incoming server's port")
          ((<= (plist-get (lispm-mail-account--server 'smtp) :port) 0) "Type the outgoing server's port")
          ((not (lispm-mail-account--password)) "Type the password"))))

(defun lispm-mail-account-test ()
  "Log in to the form's servers, in the background, and say how it went."
  (interactive)
  (when (string-empty-p (or (lispm-mail-account--value :imap-host) ""))
    (lispm-mail-account-look-up))
  (when-let* ((problem (lispm-mail-account--problem)))
    (user-error "%s" problem))
  (let ((buffer (current-buffer))
        (password (lispm-mail-account--password)))
    (setq lispm-mail-account--status 'checking)
    (lispm-mail-account--redraw-status)
    (lispm-mail-check-servers
     (append (list :password password) (lispm-mail-account--server 'imap))
     (append (list :password password) (lispm-mail-account--server 'smtp))
     (lambda (result)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq lispm-mail-account--status result)
           (lispm-mail-account--redraw-status)))))))

(defun lispm-mail-account--status-text ()
  "Return what Test Connection found, as the form says it."
  (pcase lispm-mail-account--status
    ('nil (propertize "Not tested yet." 'face 'lispm-mail-settings-note))
    ('checking (propertize "Logging in to the servers…" 'face 'lispm-mail-settings-note))
    (result
     (cl-flet ((line (label value)
                 (if (eq value t)
                     (concat (propertize "● " 'face 'lispm-mail-settings-good) label ": logged in")
                   (concat (propertize "● " 'face 'lispm-mail-settings-bad) label ": "
                           (propertize (format "%s" value) 'face 'lispm-mail-settings-bad)))))
       (propertize (concat (line "Incoming (IMAP)" (plist-get result :imap)) "\n"
                           (propertize " " 'display `(space :align-to ,lispm-mail-settings-column))
                           (line "Outgoing (SMTP)" (plist-get result :smtp)))
                   'wrap-prefix `(space :align-to ,lispm-mail-settings-column))))))

(defun lispm-mail-account--redraw-status ()
  "Say again what Test Connection found."
  (when (and (markerp lispm-mail-account--status-start)
             (marker-buffer lispm-mail-account--status-start))
    (save-excursion
      (let ((inhibit-read-only t)
            (inhibit-modification-hooks t))
        (goto-char lispm-mail-account--status-start)
        (delete-region (point) lispm-mail-account--status-end)
        (insert (lispm-mail-account--status-text))
        (set-marker lispm-mail-account--status-end (point))))))

(defun lispm-mail-account--redraw ()
  "Draw the form again, its fields' values kept."
  (let ((values (mapcar (lambda (entry) (cons (car entry) (widget-value (cdr entry))))
                        lispm-mail-account--widgets))
        (point (point)))
    (lispm-mail-account--draw (lambda (key) (alist-get key values)))
    (goto-char (min point (point-max)))))

(defun lispm-mail-account--draw (value)
  "Draw the form, VALUE giving each field's value from its key."
  (lispm-mail-settings--clear-widgets)
  (setq lispm-mail-account--widgets nil)
  (let* ((imap-security (or (plist-get lispm-mail-account--values :imap-security) 'tls))
         (smtp-security (or (plist-get lispm-mail-account--values :smtp-security) 'tls)))
    (widget-insert "\n  " (propertize (if lispm-mail-account--editing "Edit Account" "Add Account")
                                      'face 'lispm-mail-settings-title)
                   "\n\n")
    (lispm-mail-account--field :name "Your name" (funcall value :name))
    (lispm-mail-account--field :address "Email address" (funcall value :address)
                               :notify #'lispm-mail-account--address-changed)
    (lispm-mail-account--field :password "Password" (funcall value :password) :secret ?*)
    (when (and lispm-mail-account--editing (string-empty-p (or (funcall value :password) "")))
      (lispm-mail-settings--note "Leave it empty to keep the password kept."))
    (lispm-mail-account--field :label "Call it" (funcall value :label))
    (lispm-mail-settings--note "As the mailboxes show it; its address unless said.")
    (widget-insert "\n" (propertize " " 'display `(space :align-to ,lispm-mail-settings-column)))
    (lispm-mail-settings--button "Fill In Servers" #'lispm-mail-account-look-up)
    (widget-insert "\n")
    (when lispm-mail-account--note
      (lispm-mail-settings--note lispm-mail-account--note))
    (lispm-mail-settings--heading "Incoming Mail (IMAP)")
    (lispm-mail-account--field :imap-host "Server" (funcall value :imap-host)
                               :notify (lambda (&rest _) (setq lispm-mail-account--edited t)))
    (lispm-mail-account--field :imap-port "Port" (or (funcall value :imap-port) "993") :size 6)
    (lispm-mail-settings--label "Security")
    (lispm-mail-settings--popup "Security" lispm-mail-security-choices imap-security
                                (lambda (security)
                                  (lispm-mail-account--set :imap-security security)
                                  (lispm-mail-account--set :imap-port
                                                           (if (eq security 'tls) "993" "143"))
                                  (lispm-mail-account--redraw)))
    (widget-insert "\n")
    (lispm-mail-account--field :imap-user "User name" (funcall value :imap-user))
    (lispm-mail-settings--heading "Outgoing Mail (SMTP)")
    (lispm-mail-account--field :smtp-host "Server" (funcall value :smtp-host)
                               :notify (lambda (&rest _) (setq lispm-mail-account--edited t)))
    (lispm-mail-account--field :smtp-port "Port" (or (funcall value :smtp-port) "465") :size 6)
    (lispm-mail-settings--label "Security")
    (lispm-mail-settings--popup "Security" lispm-mail-security-choices smtp-security
                                (lambda (security)
                                  (lispm-mail-account--set :smtp-security security)
                                  (lispm-mail-account--set :smtp-port
                                                           (pcase security ('tls "465") ('starttls "587")
                                                                  (_ "25")))
                                  (lispm-mail-account--redraw)))
    (widget-insert "\n")
    (lispm-mail-account--field :smtp-user "User name" (funcall value :smtp-user))
    (lispm-mail-settings--label "Mail sent")
    (lispm-mail-settings--popup "Mail sent"
                                '((nil . "Kept in the Sent mailbox") (server . "Kept by the server itself"))
                                (plist-get lispm-mail-account--values :sent)
                                (lambda (sent)
                                  (lispm-mail-account--set :sent sent)
                                  (lispm-mail-account--redraw)))
    (widget-insert "\n")
    (lispm-mail-settings--heading "Signature")
    (widget-insert "    ")
    (push (cons :signature (widget-create 'text :value (or (funcall value :signature) "")
                                          :format "%v" :keymap lispm-mail-account-text-map
                                          :lispm-mail-control 'text-area))
          lispm-mail-account--widgets)
    (widget-insert "\n")
    (lispm-mail-settings--heading "Connection")
    (lispm-mail-settings--label "Status")
    (setq lispm-mail-account--status-start (point-marker))
    (widget-insert (lispm-mail-account--status-text))
    (setq lispm-mail-account--status-end (point-marker))
    (widget-insert "\n")
    (widget-insert "\n    ")
    (lispm-mail-settings--button "Test Connection" #'lispm-mail-account-test)
    (widget-insert "   ")
    (lispm-mail-settings--button "Cancel" #'lispm-mail-account-cancel)
    (widget-insert " ")
    (lispm-mail-settings--button "Save" #'lispm-mail-account-save t)
    (widget-insert "\n\n    "
                   (propertize "TAB goes to the next field, C-c C-c saves, C-c C-k cancels."
                               'face 'lispm-mail-settings-note)
                   "\n")
    (widget-setup)
    (run-hooks 'lispm-mail-settings-drawn-hook)))

(defun lispm-mail-account-save ()
  "Keep the account the form describes, and its password."
  (interactive)
  (when (string-empty-p (or (lispm-mail-account--value :imap-host) ""))
    (if-let* ((settings (lispm-mail-provider-settings (lispm-mail-account--value :address))))
        (lispm-mail-account--fill settings)
      (lispm-mail-account-look-up)
      (user-error "Looking the servers up: check them, then save")))
  (when-let* ((problem (lispm-mail-account--problem)))
    (user-error "%s" problem))
  (let* ((old lispm-mail-account--editing)
         (address (lispm-mail-account--value :address))
         (account (list :id (if old (lispm-mail-account-id old) (lispm-mail-new-account-id address))
                        :name (lispm-mail-account--value :name)
                        :address address
                        :label (let ((label (lispm-mail-account--value :label)))
                                 (and (not (string-empty-p label)) label))
                        :imap (lispm-mail-account--server 'imap)
                        :smtp (lispm-mail-account--server 'smtp)
                        :sent (plist-get lispm-mail-account--values :sent)
                        :signature (let ((signature (string-trim-right
                                                     (or (lispm-mail-account--value :signature) ""))))
                                     (and (not (string-empty-p signature)) signature))))
         (typed (let ((typed (lispm-mail-account--value :password)))
                  (and typed (not (string-empty-p typed)) typed)))
         (password (lispm-mail-account--password)))
    (when (and old (not (equal (plist-get old :imap) (plist-get account :imap))))
      (ignore-errors (lispm-mail-forget-password old)))
    (when (or typed (not old) (not (equal (plist-get old :imap) (plist-get account :imap))))
      (lispm-mail-set-password account password))
    (lispm-mail-save-accounts
     (if old
         (mapcar (lambda (each) (if (equal (lispm-mail-account-id each) (lispm-mail-account-id old))
                                    account each))
                 lispm-mail-accounts)
       (append lispm-mail-accounts (list account))))
    (lispm-mail-stop-workers (lispm-mail-account-id account))
    (run-hooks 'lispm-mail-settings-changed-hook)
    (lispm-mail-check (lispm-mail-account-id account))
    (message "%s saved" (lispm-mail-account-title account))
    (let ((first (and (not old) (null (cdr lispm-mail-accounts)))))
      (kill-buffer (current-buffer))
      (if first
          (lispm-mail)
        (lispm-mail-settings)))))

(defun lispm-mail-account-cancel ()
  "Close the form, the account left as it was."
  (interactive)
  (kill-buffer (current-buffer))
  (if (get-buffer lispm-mail-settings-buffer-name)
      (lispm-mail-settings)
    (when (fboundp 'lispm-mail) (lispm-mail))))

(lispm-kit-define-commands lispm-mail-account-mode
  :menu "Account"
  (lispm-mail-account-look-up :key "C-c C-l" :help "Fill the servers in from the address"
                              :label "Fill In Servers")
  (lispm-mail-account-test :key "C-c C-t" :help "Log in to the servers, to see that they work"
                           :label "Test Connection" :button "Test")
  "---"
  (lispm-mail-account-save :key "C-c C-c" :help "Keep the account" :label "Save" :button "Save")
  (lispm-mail-account-cancel :key "C-c C-k" :help "Close it, the account left as it was"
                             :label "Cancel" :button "Cancel"))

(define-derived-mode lispm-mail-account-mode nil "Mail Account"
  "A mail account's form: TAB goes to the next field, C-c C-c saves it.
\\{lispm-mail-account-mode-map}"
  (setq truncate-lines nil)
  (setq-local line-spacing 0.2
              word-wrap t)
  (lispm-mail-style-dress)
  (lispm-mail-settings--buttons))

(set-keymap-parent lispm-mail-account-mode-map widget-keymap)

(defun lispm-mail-settings--field-keymap (keymap)
  "Return a field's KEYMAP, widget.el's, with the form's own keys and menus."
  (let ((map (copy-keymap keymap)))
    (set-keymap-parent map lispm-mail-account-mode-map)
    map))

(defvar lispm-mail-account-field-map (lispm-mail-settings--field-keymap widget-field-keymap)
  "The keymap of a form's field: widget.el's, then the form's.")

(defvar lispm-mail-account-text-map (lispm-mail-settings--field-keymap widget-text-keymap)
  "The keymap of a form's text area: widget.el's, then the form's.")

(defun lispm-mail--account-form (account)
  "Show the form of ACCOUNT, or of a new one when nil."
  (let ((buffer (get-buffer-create lispm-mail-account-buffer-name)))
    (with-current-buffer buffer
      (lispm-mail-account-mode)
      (setq lispm-mail-account--editing account
            lispm-mail-account--edited (and account t)
            lispm-mail-account--status nil
            lispm-mail-account--note nil
            lispm-mail-account--values
            (list :imap-security (plist-get (plist-get account :imap) :security)
                  :smtp-security (plist-get (plist-get account :smtp) :security)
                  :sent (plist-get account :sent)))
      (let ((imap (plist-get account :imap))
            (smtp (plist-get account :smtp)))
        (lispm-mail-account--draw
         (lambda (key)
           (pcase key
             (:name (or (plist-get account :name) (and (not account) user-full-name)))
             (:address (plist-get account :address))
             (:password nil)
             (:label (plist-get account :label))
             (:imap-host (plist-get imap :host))
             (:imap-port (and imap (number-to-string (plist-get imap :port))))
             (:imap-user (plist-get imap :user))
             (:smtp-host (plist-get smtp :host))
             (:smtp-port (and smtp (number-to-string (plist-get smtp :port))))
             (:smtp-user (plist-get smtp :user))
             (:signature (plist-get account :signature))))))
      (goto-char (point-min))
      (widget-forward 1)
      (unless (or account (string-empty-p (or (lispm-mail-account--value :name) "")))
        ;; The name is filled in already: start at the address.
        (widget-forward 1)))
    (lispm-mail-settings--show buffer)
    buffer))

;;;###autoload
(defun lispm-mail-add-account ()
  "Add a mail account, in a form."
  (interactive)
  (lispm-mail--account-form nil))

(defun lispm-mail-edit-account (account)
  "Edit ACCOUNT, in a form."
  (interactive (list (let ((choices (mapcar (lambda (account)
                                              (cons (lispm-mail-account-title account) account))
                                            lispm-mail-accounts)))
                       (cdr (assoc (completing-read "Edit account: " choices nil t) choices)))))
  (lispm-mail--account-form account))

(defun lispm-mail-remove-account (account)
  "Remove ACCOUNT, its password and the mail kept of it, once asked."
  (when (yes-or-no-p (format "Remove %s, and the mail kept of it here? "
                             (lispm-mail-account-title account)))
    (let ((id (lispm-mail-account-id account)))
      (lispm-mail-stop-workers id)
      (ignore-errors (lispm-mail-forget-password account))
      (lispm-mail-save-accounts (seq-remove (lambda (each) (equal (lispm-mail-account-id each) id))
                                            lispm-mail-accounts))
      (when (equal lispm-mail-default-account id)
        (customize-save-variable 'lispm-mail-default-account nil))
      (remhash id lispm-mail--mailboxes)
      (maphash (lambda (key _) (when (equal (car key) id) (remhash key lispm-mail--boxes)))
               lispm-mail--boxes)
      (let ((directory (lispm-mail--account-directory id)))
        (when (file-directory-p directory)
          (delete-directory directory t)))
      (run-hooks 'lispm-mail-settings-changed-hook)
      (lispm-mail-settings-refresh)
      (message "%s removed" (lispm-mail-account-title account)))))

(provide 'lispm-mail-settings)
;;; lispm-mail-settings.el ends here
