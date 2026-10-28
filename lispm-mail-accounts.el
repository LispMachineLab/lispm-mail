;;; lispm-mail-accounts.el --- LispM Mail's accounts and their passwords  -*- lexical-binding: t; -*-

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

;; The mail accounts LispM Mail reads and sends from, kept in
;; `lispm-mail-accounts', which its settings window edits; their
;; servers, found from the address for the providers most people have
;; (`lispm-mail-providers') or asked of Thunderbird's database of
;; providers; and their passwords, kept in the macOS Keychain or with
;; auth-source, never in the accounts themselves.  An account of a
;; provider that lets mail programs in only through its own page, as
;; Outlook.com and Gmail do, signs in there instead (lispm-mail-sign-in.el),
;; and what it gets, a token, is kept as a password would be.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'url-util)
(require 'auth-source)
(require 'dom)

(defgroup lispm-mail nil
  "LispM Mail: Emacs's own mail, in a classic-style window."
  :group 'mail
  :prefix "lispm-mail-")

(defcustom lispm-mail-accounts nil
  "The mail accounts, each a plist:

:id         a name of its own, as \"work\", unique
:name       the name its mail is sent with, as \"Ada Lovelace\"
:address    its address, as \"ada@example.com\"
:label      what it is called in the window; by default its address
:imap       its IMAP server: (:host :port :security :user), :security
            `tls', `starttls' or `none'
:smtp       its SMTP server, the same way
:sent       the mailbox mail sent is kept in, nil to find it, or
            `server' for a server keeping it itself, as Gmail does
:signature  the text put under each message written, or nil
:sign-in    nil for an account logging in with its password, or a
            key of `lispm-mail-sign-ins', as `microsoft', for one
            signing in on its provider's own page

Its password is not here: see `lispm-mail-password-store'.  The
settings window, \\[lispm-mail-settings], edits them."
  :type '(repeat (plist :tag "Account"))
  :group 'lispm-mail)

(defcustom lispm-mail-default-account nil
  "The :id of the account mail is written from unless said, or nil for the first."
  :type '(choice (const :tag "The first" nil) string)
  :group 'lispm-mail)

(defcustom lispm-mail-password-store (if (and (eq system-type 'darwin) (executable-find "security"))
                                         'keychain
                                       'auth-source)
  "Where the accounts' passwords are kept.
`keychain', the macOS Keychain, through its security program;
`auth-source', the first of your `auth-sources' that can be written."
  :type '(choice (const :tag "The macOS Keychain" keychain)
                 (const :tag "auth-source" auth-source))
  :group 'lispm-mail)

(defvar lispm-mail-keychain nil
  "The keychain file passwords are kept in, or nil for the default one.")

(defconst lispm-mail-keychain-service "LispM Mail"
  "The service the Keychain keeps LispM Mail's passwords under.")

;;;; Accounts

(defun lispm-mail-account (id)
  "Return the account whose :id is ID, or nil."
  (seq-find (lambda (account) (equal (plist-get account :id) id)) lispm-mail-accounts))

(defun lispm-mail-account-id (account)
  "Return ACCOUNT's :id."
  (plist-get account :id))

(defun lispm-mail-account-title (account)
  "Return what ACCOUNT is called in the window."
  (let ((label (plist-get account :label)))
    (if (and label (not (string-blank-p label))) label (plist-get account :address))))

(defun lispm-mail-default-account ()
  "Return the account mail is written from unless said, or nil without one."
  (or (and lispm-mail-default-account (lispm-mail-account lispm-mail-default-account))
      (car lispm-mail-accounts)))

(defun lispm-mail-account-of-address (address)
  "Return the account whose address is ADDRESS, or nil."
  (when address
    (seq-find (lambda (account)
                (string-equal-ignore-case (or (plist-get account :address) "") address))
              lispm-mail-accounts)))

(defun lispm-mail-own-address-p (address)
  "Return non-nil when ADDRESS is one of the accounts'."
  (and (lispm-mail-account-of-address address) t))

(defun lispm-mail-new-account-id (address)
  "Return an :id for a new account of ADDRESS, unlike any other's."
  (let* ((base (downcase (replace-regexp-in-string
                          "[^a-z0-9]+" "-"
                          (downcase (or (car (split-string (or address "") "@")) "account")))))
         (base (if (string-empty-p (string-trim base "-" "-")) "account" (string-trim base "-" "-")))
         (id base)
         (n 1))
    (while (lispm-mail-account id)
      (setq n (1+ n) id (format "%s-%d" base n)))
    id))

(defun lispm-mail-save-accounts (accounts)
  "Keep ACCOUNTS as `lispm-mail-accounts', saved for next time."
  (customize-save-variable 'lispm-mail-accounts accounts))

;;;; Providers

(defcustom lispm-mail-microsoft-client-id nil
  "The client ID Microsoft knows LispM Mail by, to sign Outlook.com and
Microsoft 365 accounts in, or nil while LispM Mail has none.
An application registered with Microsoft for personal and work
accounts, its redirect http://localhost for mobile and desktop
applications, can be named here instead."
  :type '(choice (const :tag "None" nil) string)
  :group 'lispm-mail)

(defcustom lispm-mail-google-client-id
  "232170540593-m7o557hrusmbalsqorhqd4ckikmu4u4v.apps.googleusercontent.com"
  "The client ID Google knows LispM Mail by, to sign Gmail and Google
Workspace accounts in, or nil for none.
An OAuth client of Google Cloud for desktop applications, allowed
Gmail's scope, can be named here instead, with its secret in
`lispm-mail-google-client-secret'."
  :type '(choice (const :tag "None" nil) string)
  :group 'lispm-mail)

(defcustom lispm-mail-google-client-secret nil
  "The secret Google gives with `lispm-mail-google-client-id', or nil
for the one LispM Mail is packaged with, in `lispm-mail-secrets-file'.
A client ID of one's own takes its own secret here."
  :type '(choice (const :tag "Packaged" nil) string)
  :group 'lispm-mail)

(defvar lispm-mail-secrets-file
  (expand-file-name "lispm-mail-secrets.eld"
                    (file-name-directory (or load-file-name buffer-file-name default-directory)))
  "The file of the secrets LispM Mail is packaged with, beside it:
\((KIND :client-id ID :client-secret SECRET) ...), KIND a key of
`lispm-mail-sign-ins'.  The secrets are kept out of LispM Mail's
source: its releases write this file from the repository's secrets,
and the App Store puts it beside LispM Mail.")

(defconst lispm-mail-sign-ins
  '((microsoft
     :name "Microsoft"
     :authorize "https://login.microsoftonline.com/common/oauth2/v2.0/authorize"
     :token "https://login.microsoftonline.com/common/oauth2/v2.0/token"
     :scope "https://outlook.office.com/IMAP.AccessAsUser.All https://outlook.office.com/SMTP.Send offline_access"
     :client-id lispm-mail-microsoft-client-id
     :redirect "http://localhost:%d"
     :parameters (("response_mode" "query"))
     :hosts ("outlook.office365.com" "imap-mail.outlook.com"))
    (google
     :name "Google"
     :authorize "https://accounts.google.com/o/oauth2/v2/auth"
     :token "https://oauth2.googleapis.com/token"
     :scope "https://mail.google.com/"
     :client-id lispm-mail-google-client-id
     :client-secret lispm-mail-google-client-secret
     :redirect "http://127.0.0.1:%d"
     ;; A token to ask again with, given at each sign-in, not the first only.
     :parameters (("access_type" "offline") ("prompt" "consent"))
     :hosts ("imap.gmail.com")))
  "The providers whose own page signs accounts in: (KIND . PROPERTIES).
:name says whose page it is; :authorize and :token are its OAuth 2.0
endpoints; :scope what is asked for, the account's mail to read and
send, and a token to ask again with; :client-id the variable naming
LispM Mail there, and :client-secret the one giving the secret that
comes with it, if the provider asks for one; :redirect where the
browser comes back to, %d the port; :parameters more the page is
asked with, ((NAME VALUE) ...); :hosts the IMAP servers whose
accounts sign in so.")

(defun lispm-mail-sign-in-name (kind)
  "Return whose page signs KIND's accounts in, as \"Microsoft\"."
  (plist-get (alist-get kind lispm-mail-sign-ins) :name))

(defun lispm-mail-sign-in--setting (kind key)
  "Return the value of the variable KIND's KEY names, if a string, or nil."
  (when-let* ((variable (plist-get (alist-get kind lispm-mail-sign-ins) key)))
    (let ((value (symbol-value variable)))
      (and (stringp value) (not (string-blank-p value)) value))))

(defun lispm-mail-sign-in-client-id (kind)
  "Return LispM Mail's client ID at KIND's provider, or nil."
  (lispm-mail-sign-in--setting kind :client-id))

(defun lispm-mail-packaged-secret (kind client-id)
  "Return the secret LispM Mail is packaged with for CLIENT-ID at KIND's
provider, from `lispm-mail-secrets-file', or nil."
  (when (file-readable-p lispm-mail-secrets-file)
    (let ((entry (alist-get kind (with-temp-buffer
                                   (insert-file-contents lispm-mail-secrets-file)
                                   (ignore-errors (read (current-buffer)))))))
      (and (equal (plist-get entry :client-id) client-id)
           (stringp (plist-get entry :client-secret))
           (plist-get entry :client-secret)))))

(defun lispm-mail-sign-in-client-secret (kind)
  "Return the secret of LispM Mail's client ID at KIND's provider: the
one set, or the one LispM Mail is packaged with, or nil."
  (or (lispm-mail-sign-in--setting kind :client-secret)
      (when-let* ((client-id (lispm-mail-sign-in-client-id kind)))
        (lispm-mail-packaged-secret kind client-id))))

(defun lispm-mail-sign-in-oauth (kind refresh-token &optional access-token)
  "Return what logs in to a server of an account signing in with KIND:
the :oauth plist of `lispm-mail-server', its REFRESH-TOKEN kept, and
an ACCESS-TOKEN when one was got just now."
  (let ((provider (alist-get kind lispm-mail-sign-ins)))
    (list :token-url (plist-get provider :token)
          :client-id (lispm-mail-sign-in-client-id kind)
          :client-secret (lispm-mail-sign-in-client-secret kind)
          :scope (plist-get provider :scope)
          :refresh-token refresh-token
          :access-token access-token)))

(defconst lispm-mail-providers
  '(("Gmail" ("gmail.com" "googlemail.com")
     :imap (:host "imap.gmail.com" :port 993 :security tls)
     :smtp (:host "smtp.gmail.com" :port 465 :security tls)
     :sent server
     :sign-in google)
    ("iCloud" ("icloud.com" "me.com" "mac.com")
     :imap (:host "imap.mail.me.com" :port 993 :security tls)
     :smtp (:host "smtp.mail.me.com" :port 587 :security starttls)
     :note "iCloud asks for an app-specific password, made at account.apple.com.")
    ("Outlook.com" ("outlook.com" "hotmail.com" "live.com" "msn.com" "passport.com"
                    "outlook.co.uk" "hotmail.co.uk" "live.co.uk" "outlook.fr" "hotmail.fr" "live.fr"
                    "outlook.de" "hotmail.de" "live.de" "outlook.jp" "hotmail.co.jp" "live.jp"
                    "outlook.it" "hotmail.it" "outlook.es" "hotmail.es" "live.cn" "live.ca"
                    "live.com.au" "outlook.com.au")
     :imap (:host "outlook.office365.com" :port 993 :security tls)
     :smtp (:host "smtp-mail.outlook.com" :port 587 :security starttls)
     :sent server
     :sign-in microsoft)
    ("Yahoo" ("yahoo.com" "ymail.com" "rocketmail.com")
     :imap (:host "imap.mail.yahoo.com" :port 993 :security tls)
     :smtp (:host "smtp.mail.yahoo.com" :port 465 :security tls)
     :note "Yahoo asks for an app password, made in its account security settings.")
    ("AOL" ("aol.com")
     :imap (:host "imap.aol.com" :port 993 :security tls)
     :smtp (:host "smtp.aol.com" :port 465 :security tls)
     :note "AOL asks for an app password, made in its account security settings.")
    ("Fastmail" ("fastmail.com" "fastmail.fm")
     :imap (:host "imap.fastmail.com" :port 993 :security tls)
     :smtp (:host "smtp.fastmail.com" :port 465 :security tls)
     :note "Fastmail asks for an app password, made in Settings, Privacy & Security.")
    ("QQ Mail" ("qq.com" "foxmail.com" "vip.qq.com")
     :imap (:host "imap.qq.com" :port 993 :security tls)
     :smtp (:host "smtp.qq.com" :port 465 :security tls)
     :note "QQ Mail asks for its authorization code, turned on in its settings, for IMAP/SMTP.")
    ("NetEase 163" ("163.com")
     :imap (:host "imap.163.com" :port 993 :security tls)
     :smtp (:host "smtp.163.com" :port 465 :security tls)
     :note "NetEase asks for its authorization code, turned on in its settings, for IMAP/SMTP.")
    ("NetEase 126" ("126.com")
     :imap (:host "imap.126.com" :port 993 :security tls)
     :smtp (:host "smtp.126.com" :port 465 :security tls)
     :note "NetEase asks for its authorization code, turned on in its settings, for IMAP/SMTP.")
    ("NetEase Yeah" ("yeah.net")
     :imap (:host "imap.yeah.net" :port 993 :security tls)
     :smtp (:host "smtp.yeah.net" :port 465 :security tls)
     :note "NetEase asks for its authorization code, turned on in its settings, for IMAP/SMTP.")
    ("Zoho" ("zoho.com" "zohomail.com")
     :imap (:host "imap.zoho.com" :port 993 :security tls)
     :smtp (:host "smtp.zoho.com" :port 465 :security tls))
    ("Yandex" ("yandex.com" "yandex.ru" "ya.ru")
     :imap (:host "imap.yandex.com" :port 993 :security tls)
     :smtp (:host "smtp.yandex.com" :port 465 :security tls)
     :note "Yandex asks for an app password.")
    ("GMX" ("gmx.com" "gmx.us")
     :imap (:host "imap.gmx.com" :port 993 :security tls)
     :smtp (:host "mail.gmx.com" :port 587 :security starttls))
    ("GMX" ("gmx.net" "gmx.de" "gmx.at" "gmx.ch")
     :imap (:host "imap.gmx.net" :port 993 :security tls)
     :smtp (:host "mail.gmx.net" :port 587 :security starttls))
    ("Mail.ru" ("mail.ru" "inbox.ru" "list.ru" "bk.ru")
     :imap (:host "imap.mail.ru" :port 993 :security tls)
     :smtp (:host "smtp.mail.ru" :port 465 :security tls)
     :note "Mail.ru asks for a password made for applications.")
    ("Proton Mail" ("proton.me" "protonmail.com" "pm.me")
     :imap (:host "127.0.0.1" :port 1143 :security starttls)
     :smtp (:host "127.0.0.1" :port 1025 :security starttls)
     :note "Proton Mail is read through Proton Mail Bridge, running on this computer, with the password it gives."))
  "The providers whose servers are known: (NAME DOMAINS . SETTINGS).
SETTINGS give :imap and :smtp as `lispm-mail-accounts' does, without
:user, which is the address; :sent when the server keeps mail sent;
:sign-in, a key of `lispm-mail-sign-ins', for a provider whose
accounts sign in on its own page; and :note, what to know for the
password.")

(defun lispm-mail-domain (address)
  "Return the domain of ADDRESS, in lower case, or nil."
  (when (and address (string-match "@\\([^@ ]+\\)\\'" (string-trim address)))
    (downcase (match-string 1 (string-trim address)))))

(defun lispm-mail-provider (address)
  "Return the provider of ADDRESS, (NAME DOMAINS . SETTINGS), or nil."
  (when-let* ((domain (lispm-mail-domain address)))
    (seq-find (lambda (provider) (member domain (nth 1 provider))) lispm-mail-providers)))

(defun lispm-mail-provider-settings (address)
  "Return the servers of ADDRESS's provider, if known: a plist
\(:provider NAME :imap SERVER :smtp SERVER :sent SENT :sign-in KIND
:note NOTE), each server's :user the address."
  (when-let* ((provider (lispm-mail-provider address)))
    (let ((settings (cddr provider))
          (address (string-trim address)))
      (append (list :provider (car provider)
                    :imap (append (plist-get settings :imap) (list :user address))
                    :smtp (append (plist-get settings :smtp) (list :user address)))
              (cl-loop for key in '(:sent :sign-in :note)
                       append (list key (plist-get settings key)))))))

(defun lispm-mail-sign-in-of-host (host)
  "Return how an account whose IMAP server is HOST signs in: a key of
`lispm-mail-sign-ins', or nil for its password."
  (seq-some (lambda (entry)
              (and (member (downcase (or host "")) (plist-get (cdr entry) :hosts))
                   (car entry)))
            lispm-mail-sign-ins))

(defun lispm-mail-guess-settings (address)
  "Return servers guessed from ADDRESS's domain, as most providers name them."
  (when-let* ((domain (lispm-mail-domain address)))
    (let ((address (string-trim address)))
      (list :imap (list :host (concat "imap." domain) :port 993 :security 'tls :user address)
            :smtp (list :host (concat "smtp." domain) :port 465 :security 'tls :user address)))))

;;;; Thunderbird's database of providers

(defconst lispm-mail-autoconfig-urls
  '("https://autoconfig.thunderbird.net/v1.1/%s")
  "Where the servers of a domain are asked for, %s its domain.
Each answers Thunderbird's autoconfig, an XML clientConfig.")

(defun lispm-mail--autoconfig-security (socket)
  "Return the security of SOCKET, an autoconfig's socketType."
  (pcase (and socket (upcase socket))
    ("SSL" 'tls)
    ("STARTTLS" 'starttls)
    (_ 'none)))

(defun lispm-mail--autoconfig-user (template address)
  "Return the user name TEMPLATE, an autoconfig's username, says for ADDRESS."
  (let ((template (or template "%EMAILADDRESS%")))
    (string-replace "%EMAILLOCALPART%" (car (split-string address "@"))
                    (string-replace "%EMAILADDRESS%" address
                                    (string-replace "%EMAILDOMAIN%" (or (lispm-mail-domain address) "")
                                                    template)))))

(defun lispm-mail--dom-text (dom tag)
  "Return the text of DOM's first element TAG, trimmed, or \"\"."
  (let ((node (car (dom-by-tag dom tag))))
    (string-trim (mapconcat (lambda (child) (if (stringp child) child ""))
                            (and node (dom-children node)) ""))))

(defun lispm-mail-parse-autoconfig (dom address)
  "Return the servers DOM, an autoconfig's clientConfig, gives ADDRESS,
as `lispm-mail-provider-settings' does, or nil."
  (cl-flet ((server (node)
              (when node
                (list :host (lispm-mail--dom-text node 'hostname)
                      :port (string-to-number (lispm-mail--dom-text node 'port))
                      :security (lispm-mail--autoconfig-security (lispm-mail--dom-text node 'socketType))
                      :user (lispm-mail--autoconfig-user
                             (let ((user (lispm-mail--dom-text node 'username)))
                               (and (not (string-empty-p user)) user))
                             address)))))
    (let ((imap (seq-find (lambda (node) (equal (dom-attr node 'type) "imap"))
                          (dom-by-tag dom 'incomingServer)))
          (smtp (seq-find (lambda (node) (equal (dom-attr node 'type) "smtp"))
                          (dom-by-tag dom 'outgoingServer)))
          (name (lispm-mail--dom-text dom 'displayName)))
      (when (and imap smtp)
        (let* ((imap (server imap))
               (sign-in (lispm-mail-sign-in-of-host (plist-get imap :host))))
          (append (list :provider (and (not (string-empty-p name)) name)
                        :imap imap
                        :smtp (server smtp))
                  (and sign-in (list :sign-in sign-in))))))))

(defun lispm-mail-ask-autoconfig (address callback)
  "Ask Thunderbird's database for the servers of ADDRESS, in the background.
Call CALLBACK with what was found, as `lispm-mail-provider-settings'
returns it, or nil."
  (let ((domain (lispm-mail-domain address))
        (urls lispm-mail-autoconfig-urls))
    (if (not domain)
        (funcall callback nil)
      (cl-labels
          ((next ()
             (if (null urls)
                 (funcall callback nil)
               (let ((url (format (pop urls) (url-hexify-string domain))))
                 (condition-case nil
                     (url-retrieve
                      url
                      (lambda (status)
                        (let ((found
                               (and (not (plist-get status :error))
                                    (progn
                                      (goto-char (point-min))
                                      (re-search-forward "\r?\n\r?\n" nil t))
                                    (ignore-errors
                                      (lispm-mail-parse-autoconfig
                                       (if (fboundp 'libxml-parse-xml-region)
                                           (libxml-parse-xml-region (point) (point-max))
                                         (car (xml-parse-region (point) (point-max))))
                                       address)))))
                          (kill-buffer (current-buffer))
                          (if found (funcall callback found) (next))))
                      nil t t)
                   (error (next)))))))
        (next)))))

;;;; Passwords

(defvar lispm-mail--passwords (make-hash-table :test #'equal)
  "The passwords read so far, by account :id.")

(defun lispm-mail--password-key (account)
  "Return what ACCOUNT's password is kept under: its IMAP user at its server."
  (let ((imap (plist-get account :imap)))
    (format "%s@%s" (plist-get imap :user) (plist-get imap :host))))

(defun lispm-mail--keychain-quote (text)
  "Return TEXT quoted for a line the security program reads."
  (concat "\"" (replace-regexp-in-string "[\"\\]" "\\\\\\&" text) "\""))

(defun lispm-mail--keychain-read (key)
  "Return the password the Keychain keeps under KEY, or nil."
  (with-temp-buffer
    (when (zerop (apply #'call-process "security" nil '(t nil) nil
                        "find-generic-password" "-s" lispm-mail-keychain-service "-a" key "-w"
                        (and lispm-mail-keychain (list lispm-mail-keychain))))
      (let ((password (string-trim-right (buffer-string) "\n")))
        (and (not (string-empty-p password)) password)))))

(defun lispm-mail--keychain-write (key password)
  "Keep PASSWORD in the Keychain under KEY.
It is given to the security program on its input, never on its
command line, where other programs could read it."
  (when (string-match-p "[\n\r]" password)
    (user-error "A password cannot hold a new line"))
  (with-temp-buffer
    (insert (string-join (delq nil (list "add-generic-password" "-U"
                                         "-s" (lispm-mail--keychain-quote lispm-mail-keychain-service)
                                         "-a" (lispm-mail--keychain-quote key)
                                         "-w" (lispm-mail--keychain-quote password)
                                         (and lispm-mail-keychain
                                              (lispm-mail--keychain-quote lispm-mail-keychain))))
                         " ")
            "\n")
    ;; The line is replaced by what the program says, errors included.
    (let ((status (call-process-region (point-min) (point-max) "security" t t nil "-i")))
      (when (or (not (zerop status))
                (save-excursion (goto-char (point-min)) (search-forward "returned" nil t)))
        (error "The Keychain did not keep the password")))))

(defun lispm-mail--keychain-delete (key)
  "Forget the password the Keychain keeps under KEY."
  (apply #'call-process "security" nil nil nil
         "delete-generic-password" "-s" lispm-mail-keychain-service "-a" key
         (and lispm-mail-keychain (list lispm-mail-keychain))))

(defun lispm-mail--auth-source-spec (account)
  "Return the auth-source search spec of ACCOUNT's password."
  (let ((imap (plist-get account :imap)))
    (list :host (plist-get imap :host)
          :user (plist-get imap :user)
          :port (number-to-string (or (plist-get imap :port) 993)))))

(defun lispm-mail-password (account)
  "Return ACCOUNT's password, or nil when none is kept.
For an account signing in on its provider's page, it is the token
signing it in again, the refresh token."
  (let ((id (lispm-mail-account-id account)))
    (or (gethash id lispm-mail--passwords)
        (when-let* ((password
                     (pcase lispm-mail-password-store
                       ('keychain (lispm-mail--keychain-read (lispm-mail--password-key account)))
                       (_ (when-let* ((found (car (apply #'auth-source-search :max 1
                                                         (lispm-mail--auth-source-spec account))))
                                      (secret (plist-get found :secret)))
                            (if (functionp secret) (funcall secret) secret))))))
          (puthash id password lispm-mail--passwords)
          password))))

(defun lispm-mail-set-password (account password)
  "Keep PASSWORD as ACCOUNT's password."
  (pcase lispm-mail-password-store
    ('keychain (lispm-mail--keychain-write (lispm-mail--password-key account) password))
    (_ (let ((file (seq-find #'stringp auth-sources)))
         ;; auth-source writes only to a file there already.
         (when (and file (not (string-suffix-p ".gpg" file))
                    (not (file-exists-p (expand-file-name file))))
           (with-temp-file (expand-file-name file))
           (set-file-modes (expand-file-name file) #o600)))
       (auth-source-forget-all-cached)
       (let* ((auth-source-save-behavior t)
              (found (car (apply #'auth-source-search :max 1 :create t :secret password
                                 (lispm-mail--auth-source-spec account)))))
         (when-let* ((save (plist-get found :save-function)))
           (funcall save))
         (auth-source-forget-all-cached))))
  (puthash (lispm-mail-account-id account) password lispm-mail--passwords))

(defun lispm-mail-forget-password (account)
  "Forget ACCOUNT's password, here and where it is kept."
  (remhash (lispm-mail-account-id account) lispm-mail--passwords)
  (pcase lispm-mail-password-store
    ('keychain (lispm-mail--keychain-delete (lispm-mail--password-key account)))
    (_ (apply #'auth-source-forget (lispm-mail--auth-source-spec account)))))

(defun lispm-mail-server (account which)
  "Return ACCOUNT's server WHICH, `imap' or `smtp', with what logs in to
it: its :password, or, for an account signing in on its provider's
page, its :oauth, as `lispm-mail-sign-in-oauth' makes it, nil while it
is not signed in."
  (let ((server (plist-get account (if (eq which 'imap) :imap :smtp)))
        (secret (lispm-mail-password account)))
    (append (if-let* ((kind (plist-get account :sign-in)))
                (list :oauth (and secret (lispm-mail-sign-in-oauth kind secret)))
              (list :password secret))
            server)))

(provide 'lispm-mail-accounts)
;;; lispm-mail-accounts.el ends here
