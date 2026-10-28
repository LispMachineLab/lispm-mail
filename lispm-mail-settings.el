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
;; They are LispM UI's forms (lispm-ui-form.el), laid out as dialogs,
;; their buttons, check boxes and pop-up menus worked from the mouse or
;; the keyboard, TAB going from one to the next; each setting is saved
;; at once, with Customize, as it is changed.
;;
;; An account's form fills its servers in from its address: those of
;; the providers most people have are known, others are asked of
;; Thunderbird's database of providers, and any can be typed.  Test
;; Connection logs in to both servers, in the background, and says how
;; it went; Save keeps the account, its password in the Keychain or
;; with auth-source.
;;
;; Whatever shows LispM UI's forms dresses these too, as it dresses any.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'wid-edit)
(require 'lispm-kit-commands)
(require 'lispm-ui-form)
(require 'lispm-mail-style)
(require 'lispm-mail-accounts)
(require 'lispm-mail-data)
(require 'lispm-mail-sign-in)

(defvar lispm-mail-layout)
(defvar lispm-mail-mark-read)
(defvar lispm-mail-use-for-emacs)
(defvar lispm-mail-prefer-html)
(defvar lispm-mail-load-remote-pictures)
(defvar lispm-mail-reply-above)
(declare-function lispm-mail "lispm-mail" ())

(defvar lispm-mail-settings-changed-hook nil
  "Normal hook run once a setting is changed and saved.")

(defface lispm-mail-settings-note '((t :inherit shadow))
  "Face of what is said of a setting."
  :group 'lispm-mail)

(defface lispm-mail-settings-good '((t :inherit success))
  "Face of what went well, as a server logged in to."
  :group 'lispm-mail)

(defface lispm-mail-settings-bad '((t :inherit error))
  "Face of what went wrong."
  :group 'lispm-mail)

(defun lispm-mail-settings--buttons ()
  "Draw the current buffer's buttons, where they are text, as raised
buttons, not between brackets."
  (setq-local widget-push-button-prefix " "
              widget-push-button-suffix " "
              widget-button-face 'lispm-mail-button
              widget-button-pressed-face 'lispm-mail-button
              widget-mouse-face 'highlight))

(defun lispm-mail-settings--buffer (mode)
  "Return the buffer of the settings, or of an account's form, in MODE, or nil."
  (seq-find (lambda (buffer) (eq (buffer-local-value 'major-mode buffer) mode))
            (buffer-list)))

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

(defun lispm-mail-settings--saved (variable)
  "Return a function saving the value it is called with as VARIABLE's: a
setting's :on-change."
  (lambda (value) (lispm-mail-settings--save variable value)))

;;;; The settings

(defvar-local lispm-mail-settings--back nil
  "The buffer the settings were opened from, to go back to.")

(defun lispm-mail-settings--account-row (account)
  "Return ACCOUNT's row in the settings: whether it is online, its name,
its address, what went wrong, and its buttons."
  (let* ((id (lispm-mail-account-id account))
         (state (lispm-mail-worker-state id))
         (default (eq account (lispm-mail-default-account))))
    (list :text (list (concat (pcase (car state)
                                ('online (propertize "●" 'face 'lispm-mail-settings-good))
                                ('offline (propertize "●" 'face 'lispm-mail-settings-bad))
                                (_ (propertize "○" 'face 'lispm-mail-settings-note)))
                              " " (propertize (lispm-mail-account-title account) 'face 'bold))
                      (propertize (concat (plist-get account :address) (if default " · default" ""))
                                  'face 'lispm-mail-settings-note))
          :help (pcase state
                  (`(offline . ,error) (propertize error 'face 'lispm-mail-settings-bad)))
          :buttons `(("Edit…" . ,(lambda () (lispm-mail-edit-account account)))
                     ("Remove" . ,(lambda () (lispm-mail-remove-account account)))))))

(defun lispm-mail-settings--fields ()
  "Return the fields of the settings, as `lispm-ui-form' takes them: each
setting's is the variable it sets, saved as it is changed."
  `((accounts :type heading :label "Accounts")
    (account-list :type rows
                  :rows ,(lambda (_) (mapcar #'lispm-mail-settings--account-row lispm-mail-accounts))
                  :empty "No account yet: add one to read and send mail.")
    (add-account :type buttons :buttons (("Add Account…" . lispm-mail-add-account)))
    (lispm-mail-default-account
     :type choice :label "Write new mail from"
     :when ,(lambda (_) (cdr lispm-mail-accounts))
     :choices ,(lambda ()
                 (mapcar (lambda (account)
                           (cons (lispm-mail-account-id account) (lispm-mail-account-title account)))
                         lispm-mail-accounts))
     :on-change ,(lispm-mail-settings--saved 'lispm-mail-default-account))
    (checking :type heading :label "Checking for Mail")
    (lispm-mail-check-interval
     :type choice :label "Check for new mail"
     :choices ((1 . "Every minute") (5 . "Every 5 minutes") (15 . "Every 15 minutes")
               (30 . "Every 30 minutes") (60 . "Every hour") (nil . "Only when asked"))
     :on-change ,(lispm-mail-settings--saved 'lispm-mail-check-interval))
    (lispm-mail-window-size
     :type choice :label "Messages listed at first"
     :choices ((50 . "The latest 50") (100 . "The latest 100") (200 . "The latest 200")
               (500 . "The latest 500"))
     :on-change ,(lispm-mail-settings--saved 'lispm-mail-window-size))
    (reading :type heading :label "Reading")
    (lispm-mail-layout
     :type choice :label "Show the message"
     :choices ((below . "Below the list") (beside . "Beside the list"))
     :on-change ,(lispm-mail-settings--saved 'lispm-mail-layout))
    (lispm-mail-mark-read
     :type choice :label "Mark messages read"
     :choices ((0 . "As soon as shown") (2 . "After 2 seconds") (5 . "After 5 seconds") (nil . "Never"))
     :on-change ,(lispm-mail-settings--saved 'lispm-mail-mark-read))
    (lispm-mail-prefer-html
     :type check :text "Show HTML when a message has it, rather than its plain text"
     :on-change ,(lispm-mail-settings--saved 'lispm-mail-prefer-html))
    (lispm-mail-load-remote-pictures
     :type check :text "Load pictures from the web"
     :help "They tell senders that you read their message, and when."
     :on-change ,(lispm-mail-settings--saved 'lispm-mail-load-remote-pictures))
    (writing :type heading :label "Writing")
    (lispm-mail-reply-above
     :type check :text "Write replies above the message they quote"
     :on-change ,(lispm-mail-settings--saved 'lispm-mail-reply-above))
    (emacs :type heading :label "Emacs")
    (lispm-mail-use-for-emacs
     :type check :text "Make Mail Emacs's mail: C-x m, mailto: links, Read Mail"
     :on-change ,(lispm-mail-settings--saved 'lispm-mail-use-for-emacs))
    (lispm-mail-password-store
     :type choice :label "Keep passwords in"
     :choices ,(lambda ()
                 (append (and (eq system-type 'darwin) '((keychain . "The macOS Keychain")))
                         '((auth-source . "auth-source (~/.authinfo…)"))))
     :help "Passwords kept before stay where they are: save each account again to move them."
     :on-change ,(lispm-mail-settings--saved 'lispm-mail-password-store))))

(defun lispm-mail-settings--values ()
  "Return the settings as they are now, a list of VARIABLE VALUE ...: the
values of the settings' fields."
  (append (list 'lispm-mail-default-account
                (when-let* ((account (lispm-mail-default-account)))
                  (lispm-mail-account-id account)))
          (mapcan (lambda (variable) (list variable (symbol-value variable)))
                  '(lispm-mail-check-interval lispm-mail-window-size lispm-mail-layout
                    lispm-mail-mark-read lispm-mail-prefer-html lispm-mail-load-remote-pictures
                    lispm-mail-reply-above lispm-mail-use-for-emacs lispm-mail-password-store))))

(defun lispm-mail-settings-refresh ()
  "Draw the settings again, when shown, as they now are."
  (when-let* ((buffer (lispm-mail-settings--buffer 'lispm-mail-settings-mode)))
    (with-current-buffer buffer
      (apply #'lispm-ui-form-set (lispm-mail-settings--values)))))

(defun lispm-mail-settings-done ()
  "Close the settings, going back to where they were opened from."
  (interactive)
  (lispm-ui-form-submit))

(defun lispm-mail-settings--go-back ()
  "Go back to where the settings were opened from, as they close."
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
    (when-let* ((buffer (lispm-mail-settings--buffer 'lispm-mail-settings-mode))
                ((get-buffer-window buffer t)))
      (run-at-time 0 nil #'lispm-mail-settings-refresh))))

(lispm-kit-define-commands lispm-mail-settings-mode
  :menu "Settings"
  (lispm-mail-add-account :key "a" :help "Add a mail account" :label "Add Account…"
                          :button "Add Account")
  "---"
  (lispm-mail-settings-done :key ("q" "C-c C-c") :help "Go back to the mail" :label "Done"
                            :button "Done"))

(define-derived-mode lispm-mail-settings-mode lispm-ui-form-mode "Mail Settings"
  "LispM Mail's settings: TAB goes from one to the next, RET presses it.
\\{lispm-mail-settings-mode-map}"
  (lispm-mail-settings--buttons)
  (lispm-mail-style-dress)
  (add-hook 'lispm-mail-changed-functions #'lispm-mail-settings--changed-elsewhere))

;;;###autoload
(defun lispm-mail-settings ()
  "Show LispM Mail's settings."
  (interactive)
  (let* ((back (current-buffer))
         (old (lispm-mail-settings--buffer 'lispm-mail-settings-mode))
         (buffer (or old
                     (lispm-ui-form "Mail Settings" (lispm-mail-settings--fields)
                                    :values (cl-loop for (variable value) on (lispm-mail-settings--values)
                                                     by #'cddr collect (cons variable value))
                                    :instant t
                                    :submit "Done"
                                    :mode #'lispm-mail-settings-mode
                                    :on-submit (lambda (_) (lispm-mail-settings--go-back))
                                    :on-cancel #'lispm-mail-settings--go-back))))
    (with-current-buffer buffer
      (unless (or (eq back buffer)
                  (with-current-buffer back (derived-mode-p 'lispm-mail-account-mode)))
        (setq lispm-mail-settings--back back))
      (when old
        (lispm-mail-settings-refresh)
        (goto-char (point-min))
        (widget-forward 1)))
    (lispm-mail-settings--show buffer)
    buffer))

;;;; An account's form

(defvar-local lispm-mail-account--editing nil
  "The account the form edits, or nil for a new one.")

(defvar-local lispm-mail-account--edited nil
  "Non-nil once the servers were typed, not to be filled in again.")

(defvar-local lispm-mail-account--note nil
  "What is said of the provider, its password above all, or nil.")

(defvar-local lispm-mail-account--status nil
  "What Test Connection found: nil, `checking', or (:imap R :smtp R).")

(defvar-local lispm-mail-account--provider nil
  "What is known of the address's provider, as
`lispm-mail-provider-settings' says it.")

(defvar-local lispm-mail-account--sign-in nil
  "How the account signs in: nil, with its password, or a key of
`lispm-mail-sign-ins'.")

(defvar-local lispm-mail-account--token nil
  "What signing in gave just now, (:access-token :refresh-token ...), or nil.")

(defvar-local lispm-mail-account--waiting nil
  "While the browser is waited for, the function giving the sign-in up.")

(defvar-local lispm-mail-account--sign-in-error nil
  "What went wrong signing in last, or nil.")

(defconst lispm-mail-security-choices
  '((tls . "SSL/TLS") (starttls . "STARTTLS") (none . "None"))
  "The securities a server's connection may have.")

(defun lispm-mail-account--fill (settings)
  "Fill the servers in from SETTINGS, as `lispm-mail-provider-settings' gives them."
  (let ((imap (plist-get settings :imap))
        (smtp (plist-get settings :smtp)))
    (setq lispm-mail-account--provider settings
          lispm-mail-account--note
          (if (plist-get settings :provider)
              (format "Servers of %s filled in." (plist-get settings :provider))
            "Servers guessed from the address: check them with Test Connection."))
    (unless (eq (plist-get settings :sign-in) lispm-mail-account--sign-in)
      (lispm-mail-account--give-up)
      (setq lispm-mail-account--sign-in (plist-get settings :sign-in)
            lispm-mail-account--token nil
            lispm-mail-account--sign-in-error nil))
    (lispm-ui-form-set :imap-host (plist-get imap :host)
                       :imap-port (number-to-string (plist-get imap :port))
                       :imap-user (plist-get imap :user)
                       :imap-security (plist-get imap :security)
                       :smtp-host (plist-get smtp :host)
                       :smtp-port (number-to-string (plist-get smtp :port))
                       :smtp-user (plist-get smtp :user)
                       :smtp-security (plist-get smtp :security)
                       :sent (plist-get settings :sent))))

(defun lispm-mail-account--address-changed (address)
  "Fill the servers in from ADDRESS, as typed, if its provider is known.
This is the address field's :on-change."
  (unless lispm-mail-account--edited
    (when-let* ((settings (lispm-mail-provider-settings (string-trim address))))
      (unless (equal (lispm-ui-form-value :imap-host) (plist-get (plist-get settings :imap) :host))
        (lispm-mail-account--fill settings)))))

(defun lispm-mail-account--servers-edited (_value)
  "Note that the servers were typed, not to be filled in again.
This is the servers' fields' :on-change."
  (setq lispm-mail-account--edited t))

(defun lispm-mail-account-look-up ()
  "Fill the servers in from the address: known, asked of Thunderbird's
database of providers, or guessed."
  (interactive)
  (let ((address (lispm-ui-form-value :address))
        (buffer (current-buffer)))
    (unless (lispm-mail-domain address)
      (user-error "Type the address first, as you@example.com"))
    (setq lispm-mail-account--edited nil)
    (if-let* ((settings (lispm-mail-provider-settings address)))
        (lispm-mail-account--fill settings)
      (setq lispm-mail-account--note "Looking the servers up…")
      (lispm-ui-form-redraw)
      (lispm-mail-ask-autoconfig
       address
       (lambda (settings)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (lispm-mail-account--fill (or settings (lispm-mail-guess-settings address))))))))))

(defun lispm-mail-account--server (which)
  "Return the form's server WHICH, `imap' or `smtp', as an account keeps it."
  (let ((prefix (symbol-name which)))
    (list :host (lispm-ui-form-value (intern (format ":%s-host" prefix)))
          :port (string-to-number (or (lispm-ui-form-value (intern (format ":%s-port" prefix))) "0"))
          :security (lispm-ui-form-value (intern (format ":%s-security" prefix)))
          :user (lispm-ui-form-value (intern (format ":%s-user" prefix))))))

(defun lispm-mail-account--password ()
  "Return the password of the form: typed, or kept for the account edited."
  (or (lispm-ui-form-value :password)
      (and lispm-mail-account--editing (lispm-mail-password lispm-mail-account--editing))))

(defun lispm-mail-account--refresh-token ()
  "Return the token signing the form's account in: got just now, or kept
for the account edited; nil while it is not signed in."
  (or (plist-get lispm-mail-account--token :refresh-token)
      (and lispm-mail-account--editing
           (eq (plist-get lispm-mail-account--editing :sign-in) lispm-mail-account--sign-in)
           (lispm-mail-password lispm-mail-account--editing))))

(defun lispm-mail-account--credentials ()
  "Return what logs in to the form's servers, as `lispm-mail-server' has it."
  (if-let* ((kind lispm-mail-account--sign-in))
      (list :oauth (lispm-mail-sign-in-oauth kind (lispm-mail-account--refresh-token)
                                             (plist-get lispm-mail-account--token :access-token)))
    (list :password (lispm-mail-account--password))))

(defun lispm-mail-account--problem ()
  "Return what is missing from the form, or nil."
  (let ((address (lispm-ui-form-value :address)))
    (cond ((not (lispm-mail-domain address)) "Type the email address, as you@example.com")
          ((not (lispm-ui-form-value :imap-host)) "Type the incoming mail server")
          ((not (lispm-ui-form-value :smtp-host)) "Type the outgoing mail server")
          ((<= (plist-get (lispm-mail-account--server 'imap) :port) 0) "Type the incoming server's port")
          ((<= (plist-get (lispm-mail-account--server 'smtp) :port) 0) "Type the outgoing server's port")
          (lispm-mail-account--sign-in
           (unless (lispm-mail-account--refresh-token)
             (format "Sign in with %s first" (lispm-mail-sign-in-name lispm-mail-account--sign-in))))
          ((not (lispm-mail-account--password)) "Type the password"))))

(defun lispm-mail-account-test ()
  "Log in to the form's servers, in the background, and say how it went."
  (interactive)
  (unless (lispm-ui-form-value :imap-host)
    (lispm-mail-account-look-up))
  (when-let* ((problem (lispm-mail-account--problem)))
    (user-error "%s" problem))
  (let ((buffer (current-buffer))
        (credentials (lispm-mail-account--credentials)))
    (setq lispm-mail-account--status 'checking)
    (lispm-mail-account--say-status)
    (lispm-mail-check-servers
     (append credentials (lispm-mail-account--server 'imap))
     (append credentials (lispm-mail-account--server 'smtp))
     (lambda (result)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq lispm-mail-account--status result)
           (lispm-mail-account--say-status)))))))

(defun lispm-mail-account--give-up ()
  "Stop waiting for the browser, if the form is."
  (when lispm-mail-account--waiting
    (funcall lispm-mail-account--waiting)
    (setq lispm-mail-account--waiting nil)))

(defun lispm-mail-account--come-back (buffer)
  "Bring the window of BUFFER, the form, back in front of the browser."
  (when-let* (((not noninteractive))
              (window (get-buffer-window buffer t)))
    (select-frame-set-input-focus (window-frame window))
    (select-window window)))

(defun lispm-mail-account-sign-in ()
  "Sign the form's account in on its provider's page, in the browser.
Once signed in, the servers are logged in to, to see that they work."
  (interactive)
  (let ((kind lispm-mail-account--sign-in)
        (address (lispm-ui-form-value :address))
        (buffer (current-buffer)))
    (unless kind
      (user-error "This account logs in with its password"))
    (unless (lispm-mail-domain address)
      (user-error "Type the email address first, as you@example.com"))
    (lispm-mail-account--give-up)
    (setq lispm-mail-account--sign-in-error nil
          lispm-mail-account--waiting
          (lispm-mail-sign-in
           kind address
           (lambda (token error)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq lispm-mail-account--waiting nil)
                 (if token
                     (setq lispm-mail-account--token token)
                   (setq lispm-mail-account--sign-in-error error))
                 (lispm-ui-form-redraw)
                 (lispm-mail-account--come-back buffer)
                 (when token
                   (lispm-mail-account-test)))))))
    (lispm-ui-form-redraw)))

(defun lispm-mail-account-stop-sign-in ()
  "Stop waiting for the provider's page to sign the account in."
  (interactive)
  (lispm-mail-account--give-up)
  (lispm-ui-form-redraw))

(defun lispm-mail-account--sign-in-rows ()
  "Return the rows signing the account in on its provider's page: how it
stands, its button under that, and what is said under them."
  (let* ((name (lispm-mail-sign-in-name lispm-mail-account--sign-in))
         (help (cond
                (lispm-mail-account--sign-in-error
                 (propertize lispm-mail-account--sign-in-error 'face 'lispm-mail-settings-bad))
                ((not (lispm-mail-account--refresh-token))
                 (format "%s's own page opens in your browser: there, let LispM Mail read and send this account's mail.  No password is kept here."
                         name)))))
    (cond
     (lispm-mail-account--waiting
      (list (list :text (propertize (format "Waiting for %s, in your browser…" name)
                                    'face 'lispm-mail-settings-note))
            (list :buttons '(("Stop" . lispm-mail-account-stop-sign-in)) :help help)))
     ((lispm-mail-account--refresh-token)
      (list (list :text (concat (propertize "● " 'face 'lispm-mail-settings-good)
                                (format "Signed in with %s" name)))
            (list :buttons '(("Sign In Again…" . lispm-mail-account-sign-in)) :help help)))
     (t
      (list (list :buttons `((,(format "Sign In with %s…" name) . lispm-mail-account-sign-in))
                  :help help))))))

(defun lispm-mail-account--password-help (_values)
  "Return what is said under the password: that one left empty keeps the
one kept, and what its provider asks."
  (when-let* ((notes (delq nil (list (and lispm-mail-account--editing
                                          (not (lispm-ui-form-value :password))
                                          "Leave it empty to keep the password kept.")
                                     (plist-get lispm-mail-account--provider :note)))))
    (string-join notes "\n")))

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
       (concat (line "Incoming (IMAP)" (plist-get result :imap)) "\n"
               (line "Outgoing (SMTP)" (plist-get result :smtp)))))))

(defun lispm-mail-account--say-status ()
  "Say again what Test Connection found, where the form says it."
  (lispm-ui-form-set :status (lispm-mail-account--status-text)))

(defun lispm-mail-account--fields ()
  "Return the fields of an account's form, as `lispm-ui-form' takes them."
  `((:name :type text :label "Your name")
    (:address :type text :label "Email address" :on-change lispm-mail-account--address-changed)
    (:sign-in :type rows :label "Sign in"
              :when ,(lambda (_) lispm-mail-account--sign-in)
              :rows ,(lambda (_) (lispm-mail-account--sign-in-rows)))
    (:password :type password :label "Password"
               :when ,(lambda (_) (not lispm-mail-account--sign-in))
               :help lispm-mail-account--password-help)
    (:label :type text :label "Call it" :help "As the mailboxes show it; its address unless said.")
    (:look-up :type buttons :buttons (("Fill In Servers" . lispm-mail-account-look-up))
              :help ,(lambda (_) lispm-mail-account--note))
    (:incoming :type heading :label "Incoming Mail (IMAP)")
    (:imap-host :type text :label "Server" :on-change lispm-mail-account--servers-edited)
    (:imap-port :type text :label "Port" :width 6)
    (:imap-security :type choice :label "Security" :choices ,lispm-mail-security-choices
                    :on-change ,(lambda (security)
                                  (lispm-ui-form-set :imap-port (if (eq security 'tls) "993" "143"))))
    (:imap-user :type text :label "User name")
    (:outgoing :type heading :label "Outgoing Mail (SMTP)")
    (:smtp-host :type text :label "Server" :on-change lispm-mail-account--servers-edited)
    (:smtp-port :type text :label "Port" :width 6)
    (:smtp-security :type choice :label "Security" :choices ,lispm-mail-security-choices
                    :on-change ,(lambda (security)
                                  (lispm-ui-form-set :smtp-port (pcase security
                                                                  ('tls "465") ('starttls "587") (_ "25")))))
    (:smtp-user :type text :label "User name")
    (:sent :type choice :label "Mail sent"
           :choices ((nil . "Kept in the Sent mailbox") (server . "Kept by the server itself")))
    (:signature-group :type heading :label "Signature")
    (:signature :type text-area :lines 4)
    (:connection :type heading :label "Connection")
    (:status :type status :label "Status")
    (:test :type buttons :buttons (("Test Connection" . lispm-mail-account-test)))))

(defun lispm-mail-account--values (account)
  "Return the values of the form of ACCOUNT, or of a new one when nil."
  (let ((imap (plist-get account :imap))
        (smtp (plist-get account :smtp)))
    `((:name . ,(or (plist-get account :name) (and (not account) user-full-name)))
      (:address . ,(plist-get account :address))
      (:label . ,(plist-get account :label))
      (:imap-host . ,(plist-get imap :host))
      (:imap-port . ,(if imap (number-to-string (plist-get imap :port)) "993"))
      (:imap-security . ,(or (plist-get imap :security) 'tls))
      (:imap-user . ,(plist-get imap :user))
      (:smtp-host . ,(plist-get smtp :host))
      (:smtp-port . ,(if smtp (number-to-string (plist-get smtp :port)) "465"))
      (:smtp-security . ,(or (plist-get smtp :security) 'tls))
      (:smtp-user . ,(plist-get smtp :user))
      (:sent . ,(plist-get account :sent))
      (:signature . ,(plist-get account :signature))
      (:status . ,(propertize "Not tested yet." 'face 'lispm-mail-settings-note)))))

(defun lispm-mail-account-save ()
  "Keep the account the form describes, and its password."
  (interactive)
  (lispm-ui-form-submit))

(defun lispm-mail-account--save ()
  "Keep the account the form describes, and its password, then show the
settings, or the mail for the first account.  This is the form's
:on-submit."
  (unless (lispm-ui-form-value :imap-host)
    (if-let* ((settings (lispm-mail-provider-settings (lispm-ui-form-value :address))))
        (lispm-mail-account--fill settings)
      (lispm-mail-account-look-up)
      (user-error "Looking the servers up: check them, then save")))
  (when-let* ((problem (lispm-mail-account--problem)))
    (user-error "%s" problem))
  (let* ((old lispm-mail-account--editing)
         (address (lispm-ui-form-value :address))
         (account (list :id (if old (lispm-mail-account-id old) (lispm-mail-new-account-id address))
                        :name (or (lispm-ui-form-value :name) "")
                        :address address
                        :label (lispm-ui-form-value :label)
                        :imap (lispm-mail-account--server 'imap)
                        :smtp (lispm-mail-account--server 'smtp)
                        :sent (lispm-ui-form-value :sent)
                        :signature (lispm-ui-form-value :signature)))
         (account (if lispm-mail-account--sign-in
                      (append account (list :sign-in lispm-mail-account--sign-in))
                    account))
         (typed (if lispm-mail-account--sign-in
                    (let ((token (plist-get lispm-mail-account--token :refresh-token)))
                      (and token (not (string-empty-p token)) token))
                  (lispm-ui-form-value :password)))
         ;; Read before the one kept for the old servers is forgotten.
         (secret (if lispm-mail-account--sign-in
                     (lispm-mail-account--refresh-token)
                   (lispm-mail-account--password))))
    (when (and old (not (equal (plist-get old :imap) (plist-get account :imap))))
      (ignore-errors (lispm-mail-forget-password old)))
    (when (or typed (not old) (not (equal (plist-get old :imap) (plist-get account :imap))))
      (lispm-mail-set-password account secret))
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
    (if (and (not old) (null (cdr lispm-mail-accounts)))
        (lispm-mail)
      (lispm-mail-settings))))

(defun lispm-mail-account-cancel ()
  "Close the form, the account left as it was."
  (interactive)
  (lispm-ui-form-cancel))

(defun lispm-mail-account--cancelled ()
  "Show the settings, or the mail, the form cancelled.  This is the
form's :on-cancel."
  (if (lispm-mail-settings--buffer 'lispm-mail-settings-mode)
      (lispm-mail-settings)
    (when (fboundp 'lispm-mail) (lispm-mail))))

(lispm-kit-define-commands lispm-mail-account-mode
  :menu "Account"
  (lispm-mail-account-look-up :key "C-c C-l" :help "Fill the servers in from the address"
                              :label "Fill In Servers")
  (lispm-mail-account-sign-in :key "C-c C-s" :help "Sign in on the provider's own page, in the browser"
                              :label "Sign In…")
  (lispm-mail-account-test :key "C-c C-t" :help "Log in to the servers, to see that they work"
                           :label "Test Connection" :button "Test")
  "---"
  (lispm-mail-account-save :key "C-c C-c" :help "Keep the account" :label "Save" :button "Save")
  (lispm-mail-account-cancel :key "C-c C-k" :help "Close it, the account left as it was"
                             :label "Cancel" :button "Cancel"))

(define-derived-mode lispm-mail-account-mode lispm-ui-form-mode "Mail Account"
  "A mail account's form: TAB goes to the next field, C-c C-c saves it.
\\{lispm-mail-account-mode-map}"
  (lispm-mail-settings--buttons)
  (lispm-mail-style-dress)
  (add-hook 'kill-buffer-hook #'lispm-mail-account--give-up nil t))

(defun lispm-mail--account-form (account)
  "Show the form of ACCOUNT, or of a new one when nil."
  (when-let* ((old (lispm-mail-settings--buffer 'lispm-mail-account-mode)))
    ;; Its sign-in, if waited for, is given up.
    (kill-buffer old))
  (let ((buffer (lispm-ui-form (if account "Edit Account" "Add Account")
                               (lispm-mail-account--fields)
                               :values (lispm-mail-account--values account)
                               :submit "Save"
                               :mode #'lispm-mail-account-mode
                               :setup (lambda ()
                                        (setq lispm-mail-account--editing account
                                              lispm-mail-account--edited (and account t)
                                              lispm-mail-account--provider
                                              (lispm-mail-provider-settings (plist-get account :address))
                                              lispm-mail-account--sign-in (plist-get account :sign-in)))
                               :on-submit (lambda (_) (lispm-mail-account--save))
                               :on-cancel #'lispm-mail-account--cancelled)))
    (with-current-buffer buffer
      (unless (or account (not (lispm-ui-form-value :name)))
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
