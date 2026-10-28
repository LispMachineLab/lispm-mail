;;; lispm-mail-test.el --- Tests of LispM Mail  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

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

;; Run with `make test'.  Nothing here reaches a mail server: what the
;; background Emacs does is tested on what imap.el would have parsed,
;; and one test starts a background Emacs to check a server that is
;; not there.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'lispm-mail)

(defconst lispm-mail-test-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "The directory of the tests.")

(defmacro lispm-mail-test-with-home (&rest body)
  "Run BODY with LispM Mail keeping its mail and settings in a scratch directory."
  (declare (indent 0) (debug t))
  `(let* ((home (make-temp-file "lispm-mail-test" t))
          (lispm-mail-directory (expand-file-name "mail/" home))
          (custom-file (expand-file-name "custom.el" home))
          (lispm-mail-accounts nil)
          (lispm-mail-default-account nil)
          (lispm-mail-changed-functions nil)
          (lispm-mail-new-mail-functions nil)
          (lispm-mail--mailboxes (make-hash-table :test #'equal))
          (lispm-mail--boxes (make-hash-table :test #'equal))
          (lispm-mail--passwords (make-hash-table :test #'equal))
          (lispm-mail--save-timer nil)
          (lispm-mail--unsaved nil))
     (unwind-protect
         (progn ,@body)
       (when lispm-mail--save-timer (cancel-timer lispm-mail--save-timer))
       (delete-directory home t))))

(defconst lispm-mail-test-account
  '(:id "ada" :name "Ada Lovelace" :address "ada@example.com"
    :imap (:host "imap.example.com" :port 993 :security tls :user "ada@example.com")
    :smtp (:host "smtp.example.com" :port 465 :security tls :user "ada@example.com"))
  "An account the tests use.")

(defun lispm-mail-test-wait (predicate &optional seconds)
  "Run the event loop until PREDICATE holds or SECONDS (default 5) pass."
  (let ((deadline (+ (float-time) (or seconds 5))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (funcall predicate)))

;;;; The background Emacs

(ert-deftest lispm-mail-test-message-set ()
  "UIDs become an IMAP message set, runs as ranges."
  (should (equal (lispm-mail-worker-set '(10 1 2 3 5 7 8)) "1:3,5,7:8,10"))
  (should (equal (lispm-mail-worker-set '(4)) "4"))
  (should (equal (lispm-mail-worker-set '(2 2 3)) "2:3")))

(ert-deftest lispm-mail-test-dates ()
  "Dates are read as Date fields and IMAP's INTERNALDATE write them."
  (should (= (lispm-mail-worker-date "Tue, 27 Oct 2026 10:00:00 +0000") 1793095200))
  (should (= (lispm-mail-worker-date "27-Oct-2026 10:00:00 +0000") 1793095200))
  (should-not (lispm-mail-worker-date ""))
  (should-not (lispm-mail-worker-date nil)))

(ert-deftest lispm-mail-test-printed-answers ()
  "An answer is printed on one line of ASCII and read back the same."
  (let* ((answer '(3 ok (:subject "周末一起去爬山吗？\nNext line" :bytes "\351t\351")))
         (line (lispm-mail-worker-print answer)))
    (should-not (string-match-p "[^[:ascii:]]" line))
    (should-not (string-search "\n" line))
    (should (equal (car (read-from-string line)) answer))))

(ert-deftest lispm-mail-test-parse-answers ()
  "Answers whole are read from a background Emacs's output; a part waits."
  (with-temp-buffer
    (insert "imap: Connecting...\n" lispm-mail-worker-tag "(1 ok (\"INBOX\"))\n"
            lispm-mail-worker-tag "(2 error \"No")
    (should (equal (lispm-mail-parse-answers (current-buffer)) '((1 ok ("INBOX")))))
    (goto-char (point-max))
    (insert " such mailbox\")\n")
    (should (equal (lispm-mail-parse-answers (current-buffer)) '((2 error "No such mailbox"))))
    (should (equal (lispm-mail-parse-answers (current-buffer)) nil))))

(defconst lispm-mail-test-structure
  '((("text" "plain" ("charset" "utf-8") nil nil "QUOTED-PRINTABLE" 120 4 nil nil nil)
     ("text" "html" ("charset" "utf-8") nil nil "BASE64" 400 6 nil nil nil)
     "alternative" ("boundary" "b2") nil nil)
    ("application" "pdf" ("name" "report.pdf") nil nil "BASE64" 9000 nil ("attachment" ("filename" "report.pdf")) nil)
    "mixed" ("boundary" "b1") nil nil)
  "A body structure as imap.el parses one: text both ways, and a PDF attached.")

(ert-deftest lispm-mail-test-structure ()
  "The text part is found in a body structure, and the attachment."
  (should (equal (lispm-mail-worker--text-part lispm-mail-test-structure)
                 '("1.1" "QUOTED-PRINTABLE" "utf-8" "plain")))
  (should (equal (lispm-mail-worker--text-part lispm-mail-test-structure "html")
                 '("1.2" "BASE64" "utf-8" "html")))
  (should (equal (lispm-mail-worker--text-part '("text" "plain" nil nil nil "7BIT" 10 1 nil nil nil))
                 '("1" "7BIT" nil "plain")))
  (should (lispm-mail-worker--attachment-p lispm-mail-test-structure))
  (should-not (lispm-mail-worker--attachment-p (car lispm-mail-test-structure))))

(ert-deftest lispm-mail-test-snippets ()
  "The first words of a message are decoded, its quotations and markup left out."
  (should (equal (lispm-mail-worker-snippet "Caf=C3=A9 at noon?=\n\n> old words\nSee you" "QUOTED-PRINTABLE" "utf-8" "plain")
                 "Café at noon? See you"))
  (should (equal (lispm-mail-worker-snippet (base64-encode-string (encode-coding-string "你好，世界" 'utf-8))
                                            "BASE64" "utf-8" "plain")
                 "你好，世界"))
  (should (equal (lispm-mail-worker-snippet "<html><head><style>p{}</style></head><body><p>Hello&nbsp;<b>there</b></p></body></html>"
                                            "7BIT" nil "html")
                 "Hello there")))

(ert-deftest lispm-mail-test-unknown-operation ()
  "A request for no operation is answered with an error."
  (should (equal (lispm-mail-worker-answer '(7 fly)) '(7 error "No operation fly")))
  (let ((lispm-mail-worker--imap nil))
    (should (equal (lispm-mail-worker-answer '(8 mailboxes)) '(8 error "No IMAP server was given")))))

(ert-deftest lispm-mail-test-reasons ()
  "Connection failures are said in words."
  (should (equal (lispm-mail-worker-reason
                  '(error "make client process failed: Connection refused, :name, x, :buffer, b, :host, mail.example.com, :service, 993, :nowait, nil"))
                 "Cannot reach mail.example.com at port 993: connection refused")))

(ert-deftest lispm-mail-test-check-servers ()
  "A check runs in a background Emacs and says why a server cannot be reached."
  (let ((result nil))
    (lispm-mail-check-servers '(:host "127.0.0.1" :port 1 :security none :user "u" :password "p")
                              '(:host "127.0.0.1" :port 1 :security none :user "u" :password "p")
                              (lambda (answer) (setq result answer)))
    (with-timeout (60 (error "The check did not end"))
      (while (not result) (accept-process-output nil 0.1)))
    (should (stringp (plist-get result :imap)))
    (should (stringp (plist-get result :smtp)))
    (should (string-match-p "127.0.0.1" (plist-get result :smtp)))))

;;;; Accounts

(ert-deftest lispm-mail-test-providers ()
  "The servers of the providers most people have are known from the address."
  (let ((gmail (lispm-mail-provider-settings "ada@gmail.com")))
    (should (equal (plist-get gmail :provider) "Gmail"))
    (should (equal (plist-get gmail :imap)
                   '(:host "imap.gmail.com" :port 993 :security tls :user "ada@gmail.com")))
    (should (eq (plist-get gmail :sent) 'server)))
  (should (equal (plist-get (plist-get (lispm-mail-provider-settings "x@QQ.com ") :smtp) :host)
                 "smtp.qq.com"))
  (should-not (lispm-mail-provider-settings "ada@example.com"))
  (should (equal (plist-get (plist-get (lispm-mail-guess-settings "ada@example.com") :imap) :host)
                 "imap.example.com"))
  (should-not (lispm-mail-domain "not an address")))

(ert-deftest lispm-mail-test-autoconfig ()
  "Thunderbird's autoconfig gives the servers, the user name filled in."
  (let ((dom (with-temp-buffer
               (insert "<?xml version=\"1.0\"?><clientConfig version=\"1.1\"><emailProvider id=\"posteo.de\">"
                       "<displayName>Posteo</displayName>"
                       "<incomingServer type=\"pop3\"><hostname>pop.example.com</hostname><port>995</port>"
                       "<socketType>SSL</socketType><username>%EMAILADDRESS%</username></incomingServer>"
                       "<incomingServer type=\"imap\"><hostname> posteo.de </hostname><port>143</port>"
                       "<socketType>STARTTLS</socketType><username>%EMAILLOCALPART%</username></incomingServer>"
                       "<outgoingServer type=\"smtp\"><hostname>posteo.de</hostname><port>465</port>"
                       "<socketType>SSL</socketType><username>%EMAILADDRESS%</username></outgoingServer>"
                       "</emailProvider></clientConfig>")
               (if (fboundp 'libxml-parse-xml-region)
                   (libxml-parse-xml-region (point-min) (point-max))
                 (car (xml-parse-region (point-min) (point-max)))))))
    (should (equal (lispm-mail-parse-autoconfig dom "ada@posteo.de")
                   '(:provider "Posteo"
                     :imap (:host "posteo.de" :port 143 :security starttls :user "ada")
                     :smtp (:host "posteo.de" :port 465 :security tls :user "ada@posteo.de"))))))

(ert-deftest lispm-mail-test-account-ids ()
  "A new account's name is its address's, unlike the others'."
  (let ((lispm-mail-accounts (list lispm-mail-test-account)))
    (should (equal (lispm-mail-new-account-id "ada@other.org") "ada-2"))
    (should (equal (lispm-mail-new-account-id "Grace.Hopper@navy.mil") "grace-hopper"))
    (should (lispm-mail-own-address-p "ADA@example.com"))
    (should-not (lispm-mail-own-address-p "grace@navy.mil"))))

(ert-deftest lispm-mail-test-auth-source-password ()
  "A password is kept with auth-source, in a file made for it, and read back."
  (lispm-mail-test-with-home
    (let* ((auth-sources (list (expand-file-name "authinfo" home)))
           (lispm-mail-password-store 'auth-source)
           (auth-source-do-cache nil))
      (lispm-mail-set-password lispm-mail-test-account "s3cret word")
      (clrhash lispm-mail--passwords)
      (should (equal (lispm-mail-password lispm-mail-test-account) "s3cret word"))
      (should (= (file-modes (car auth-sources)) #o600)))))

(ert-deftest lispm-mail-test-keychain-quoting ()
  "What the Keychain is given is quoted as its program reads it."
  (should (equal (lispm-mail--keychain-quote "p a\"ss\\x") "\"p a\\\"ss\\\\x\"")))

;;;; Signing in on a provider's page

(defun lispm-mail-test-http-response (status json)
  "Return a buffer holding an HTTP response of STATUS with JSON, a string."
  (let ((buffer (generate-new-buffer " *lispm-mail-test http*")))
    (with-current-buffer buffer
      (set-buffer-multibyte nil)
      (insert (format "HTTP/1.1 %s\r\nContent-Type: application/json\r\n\r\n" status)
              (encode-coding-string json 'utf-8)))
    buffer))

(ert-deftest lispm-mail-test-sign-in-providers ()
  "Outlook.com signs in on Microsoft's page, Gmail on Google's."
  (let ((outlook (lispm-mail-provider-settings "ada@Hotmail.com")))
    (should (eq (plist-get outlook :sign-in) 'microsoft))
    (should (eq (plist-get outlook :sent) 'server))
    (should (equal (plist-get outlook :imap)
                   '(:host "outlook.office365.com" :port 993 :security tls :user "ada@Hotmail.com"))))
  (should (eq (plist-get (lispm-mail-provider-settings "ada@live.cn") :sign-in) 'microsoft))
  (let ((gmail (lispm-mail-provider-settings "ada@gmail.com")))
    (should (eq (plist-get gmail :sign-in) 'google))
    (should-not (plist-get gmail :note)))
  (should-not (plist-get (lispm-mail-provider-settings "ada@icloud.com") :sign-in))
  (should (eq (lispm-mail-sign-in-of-host "Outlook.Office365.com") 'microsoft))
  ;; A Google Workspace domain, its servers Gmail's, signs in on Google's page too.
  (should (eq (lispm-mail-sign-in-of-host "imap.gmail.com") 'google))
  (should-not (lispm-mail-sign-in-of-host "imap.example.com")))

(ert-deftest lispm-mail-test-sign-in-server ()
  "A signed-in account's servers log in with its token, the others with passwords."
  (lispm-mail-test-with-home
    (let* ((lispm-mail-microsoft-client-id "client")
           (account (append '(:id "out" :address "ada@outlook.com" :sign-in microsoft)
                            (cddr (lispm-mail-provider-settings "ada@outlook.com")))))
      (should (equal (lispm-mail-server account 'imap)
                     '(:oauth nil :host "outlook.office365.com" :port 993 :security tls
                              :user "ada@outlook.com")))
      (puthash "out" "refresh" lispm-mail--passwords)
      (let ((oauth (plist-get (lispm-mail-server account 'smtp) :oauth)))
        (should (equal (plist-get oauth :refresh-token) "refresh"))
        (should (equal (plist-get oauth :client-id) "client"))
        (should (string-match-p "/oauth2/v2.0/token\\'" (plist-get oauth :token-url)))
        (should (string-match-p "SMTP.Send" (plist-get oauth :scope))))
      (should-not (plist-get (plist-get (lispm-mail-server account 'smtp) :oauth) :client-secret))
      (let* ((lispm-mail-google-client-id "gclient")
             (lispm-mail-google-client-secret "gsecret")
             (gmail (append '(:id "gm" :address "ada@gmail.com" :sign-in google)
                            (cddr (lispm-mail-provider-settings "ada@gmail.com")))))
        (puthash "gm" "grefresh" lispm-mail--passwords)
        (let ((oauth (plist-get (lispm-mail-server gmail 'imap) :oauth)))
          (should (equal (plist-get oauth :client-id) "gclient"))
          (should (equal (plist-get oauth :client-secret) "gsecret"))
          (should (equal (plist-get oauth :token-url) "https://oauth2.googleapis.com/token"))
          (should (equal (plist-get oauth :scope) "https://mail.google.com/"))))
      (puthash "ada" "pw" lispm-mail--passwords)
      (should (equal (plist-get (lispm-mail-server lispm-mail-test-account 'imap) :password) "pw")))))

(ert-deftest lispm-mail-test-xoauth2 ()
  "SASL XOAUTH2 says the user and the token, base64-encoded."
  (should (equal (base64-decode-string (lispm-mail-worker-xoauth2 "ada@outlook.com" "T0K"))
                 "user=ada@outlook.com\1auth=Bearer T0K\1\1")))

(ert-deftest lispm-mail-test-token-answers ()
  "A token endpoint's answer gives the tokens, or says in words why not."
  (let ((buffer (lispm-mail-test-http-response
                 "200 OK" "{\"access_token\":\"A\",\"refresh_token\":\"R\",\"expires_in\":3599}")))
    (unwind-protect
        (should (equal (lispm-mail-worker-token-answer buffer)
                       '(:access-token "A" :refresh-token "R" :expires-in 3599)))
      (kill-buffer buffer)))
  (dolist (case '(("{\"error\":\"invalid_grant\",\"error_description\":\"AADSTS70000: gone\"}"
                   . "no longer signed in")
                  ("{\"error\":\"invalid_client\",\"error_description\":\"AADSTS7000218: The request body must contain client_assertion. Trace ID: 1 Correlation ID: 2\"}"
                   . "\\`AADSTS7000218: The request body must contain client_assertion\\.\\'")
                  ("not JSON" . "did not answer")))
    (let ((buffer (lispm-mail-test-http-response "400 Bad Request" (car case))))
      (unwind-protect
          (should (string-match-p (cdr case)
                                  (cadr (should-error (lispm-mail-worker-token-answer buffer)))))
        (kill-buffer buffer)))))

(ert-deftest lispm-mail-test-access-token-refreshed ()
  "The background Emacs gets an access token with the refresh token, keeps
it until it expires, and says the new refresh token given with it."
  (let ((lispm-mail-worker--token nil)
        (asked nil)
        (said nil)
        (oauth '(:token-url "https://login.example.com/token" :client-id "client"
                 :scope "mail offline_access" :refresh-token "R1")))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest _)
                 (push (cons url url-request-data) asked)
                 (lispm-mail-test-http-response
                  "200 OK" (format "{\"access_token\":\"A%d\",\"refresh_token\":\"R%d\",\"expires_in\":3600}"
                                   (length asked) (1+ (length asked))))))
              ((symbol-function 'lispm-mail-worker--say) (lambda (form) (push form said))))
      (should (equal (lispm-mail-worker-access-token oauth) "A1"))
      (should (equal (lispm-mail-worker-access-token oauth) "A1"))
      (should (= (length asked) 1))
      (should (equal (caar asked) "https://login.example.com/token"))
      (should (string-match-p "grant_type=refresh_token" (cdar asked)))
      (should (string-match-p "refresh_token=R1" (cdar asked)))
      (should-not (string-match-p "client_secret" (cdar asked)))
      (should (equal said '((nil refresh-token "R2"))))
      ;; Expired, it is got again, with the newer refresh token.
      (setf (nth 1 lispm-mail-worker--token) (float-time))
      (should (equal (lispm-mail-worker-access-token oauth) "A2"))
      (should (string-match-p "refresh_token=R2" (cdar asked)))
      ;; One given is used as it is.
      (should (equal (lispm-mail-worker-access-token (plist-put (copy-sequence oauth) :access-token "G"))
                     "G"))
      ;; A secret given goes with the request.
      (setq lispm-mail-worker--token nil)
      (lispm-mail-worker-access-token (plist-put (copy-sequence oauth) :client-secret "S"))
      (should (string-match-p "client_secret=S" (cdar asked))))))

(defun lispm-mail-test-ask (port request)
  "Send REQUEST to PORT on the loopback, as a browser, and return the answer."
  (let* ((buffer (generate-new-buffer " *lispm-mail-test browser*"))
         (process (make-network-process :name "lispm-mail-test browser" :buffer buffer
                                        :host "127.0.0.1" :service port :coding 'binary)))
    (unwind-protect
        (progn
          (process-send-string process request)
          (with-timeout (10 (error "No answer"))
            (while (process-live-p process)
              (accept-process-output process 0.1)))
          (with-current-buffer buffer (buffer-string)))
      (kill-buffer buffer))))

(ert-deftest lispm-mail-test-sign-in-in-browser ()
  "Signing in opens the provider's page and takes the code it sends back,
with the state asked for, exchanging it with the PKCE verifier."
  (let* ((lispm-mail-microsoft-client-id "client")
         (opened nil)
         (exchanged nil)
         (result nil)
         (lispm-mail-sign-in-browse-function (lambda (url) (setq opened url))))
    (cl-letf (((symbol-function 'lispm-mail-sign-in--exchange)
               (lambda (kind fields callback)
                 (setq exchanged (cons kind fields))
                 (funcall callback '(:access-token "A" :refresh-token "R" :expires-in 3600) nil))))
      (lispm-mail-sign-in 'microsoft "ada@outlook.com"
                          (lambda (value error) (setq result (list value error))))
      (let* ((query (url-parse-query-string (cadr (split-string opened "?"))))
             (said (lambda (name) (cadr (assoc name query))))
             (port (and (string-match ":\\([0-9]+\\)\\'" (funcall said "redirect_uri"))
                        (string-to-number (match-string 1 (funcall said "redirect_uri"))))))
        (should (string-prefix-p "https://login.microsoftonline.com/common/oauth2/v2.0/authorize?" opened))
        (should (equal (funcall said "client_id") "client"))
        (should (equal (funcall said "login_hint") "ada@outlook.com"))
        (should (equal (funcall said "code_challenge_method") "S256"))
        (should (equal (funcall said "response_mode") "query"))
        (should (string-match-p "offline_access" (funcall said "scope")))
        ;; A page the sign-in did not ask for is turned away.
        (should (string-match-p "\\`HTTP/1.1 400"
                                (lispm-mail-test-ask port "GET /?code=X&state=other HTTP/1.1\r\n\r\n")))
        (should (string-match-p "\\`HTTP/1.1 404"
                                (lispm-mail-test-ask port "GET /favicon.ico HTTP/1.1\r\n\r\n")))
        (should-not result)
        (let ((page (lispm-mail-test-ask
                     port (format "GET /?code=C0DE&state=%s HTTP/1.1\r\nHost: localhost\r\n\r\n"
                                  (funcall said "state")))))
          (should (string-match-p "\\`HTTP/1.1 200" page))
          (should (string-match-p "Signed in" page)))
        (should (equal result '((:access-token "A" :refresh-token "R" :expires-in 3600) nil)))
        (let ((fields (cdr exchanged)))
          (should (eq (car exchanged) 'microsoft))
          (should (equal (cadr (assoc "code" fields)) "C0DE"))
          (should (equal (cadr (assoc "redirect_uri" fields)) (funcall said "redirect_uri")))
          (should-not (cadr (assoc "client_secret" fields)))
          (should (equal (lispm-mail-sign-in-challenge (cadr (assoc "code_verifier" fields)))
                         (funcall said "code_challenge"))))
        ;; Done, nothing listens any more.
        (should-not (seq-find (lambda (process)
                                (and (equal (process-name process) "lispm-mail sign-in")
                                     (process-live-p process)))
                              (process-list)))))))

(ert-deftest lispm-mail-test-sign-in-google ()
  "Google's page is asked for a token to ask again with, at each sign-in,
and the code is exchanged with the client's secret."
  (let* ((lispm-mail-google-client-id "gclient")
         (lispm-mail-google-client-secret "gsecret")
         (opened nil)
         (exchanged nil)
         (lispm-mail-sign-in-browse-function (lambda (url) (setq opened url))))
    (cl-letf (((symbol-function 'lispm-mail-sign-in--exchange)
               (lambda (kind fields callback)
                 (setq exchanged (cons kind fields))
                 (funcall callback '(:access-token "A" :refresh-token "R" :expires-in 3600) nil))))
      (lispm-mail-sign-in 'google "ada@gmail.com" #'ignore)
      (let* ((query (url-parse-query-string (cadr (split-string opened "?"))))
             (said (lambda (name) (cadr (assoc name query))))
             (redirect (funcall said "redirect_uri")))
        (should (string-prefix-p "https://accounts.google.com/o/oauth2/v2/auth?" opened))
        (should (string-match-p "\\`http://127\\.0\\.0\\.1:[0-9]+\\'" redirect))
        (should (equal (funcall said "scope") "https://mail.google.com/"))
        (should (equal (funcall said "access_type") "offline"))
        (should (equal (funcall said "prompt") "consent"))
        (should-not (funcall said "response_mode"))
        (should-not (funcall said "client_secret"))
        (lispm-mail-test-ask (string-to-number (car (last (split-string redirect ":"))))
                             (format "GET /?code=C0DE&state=%s HTTP/1.1\r\n\r\n" (funcall said "state")))
        (should (eq (car exchanged) 'google))
        (should (equal (cadr (assoc "client_secret" (cdr exchanged))) "gsecret"))))))

(ert-deftest lispm-mail-test-packaged-secret ()
  "The secret LispM Mail is packaged with goes with its own client ID
only, and one set takes its place."
  (let* ((file (make-temp-file "lispm-mail-secrets" nil ".eld"
                               "((google :client-id \"gclient\" :client-secret \"packaged\"))\n"))
         (lispm-mail-secrets-file file)
         (lispm-mail-google-client-id "gclient")
         (lispm-mail-google-client-secret nil))
    (unwind-protect
        (progn
          (should (equal (lispm-mail-sign-in-client-secret 'google) "packaged"))
          (should (equal (plist-get (lispm-mail-sign-in-oauth 'google "R") :client-secret) "packaged"))
          (let ((lispm-mail-google-client-secret "own"))
            (should (equal (lispm-mail-sign-in-client-secret 'google) "own")))
          (let ((lispm-mail-google-client-id "other"))
            (should-not (lispm-mail-sign-in-client-secret 'google)))
          (should-not (lispm-mail-sign-in-client-secret 'microsoft))
          (with-temp-file file (insert "((google"))
          (should-not (lispm-mail-sign-in-client-secret 'google))
          (delete-file file)
          (should-not (lispm-mail-sign-in-client-secret 'google)))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest lispm-mail-test-sign-in-without-secret ()
  "Without the secret its provider asks for, the sign-in says so before
the browser opens."
  (let* ((lispm-mail-google-client-id "gclient")
         (lispm-mail-google-client-secret nil)
         (lispm-mail-secrets-file (expand-file-name "none.eld" temporary-file-directory))
         (opened nil)
         (lispm-mail-sign-in-browse-function (lambda (url) (setq opened url))))
    (should (string-match-p "App Store"
                            (cadr (should-error (lispm-mail-sign-in 'google "ada@gmail.com" #'ignore)
                                                :type 'user-error))))
    (should-not opened)))

(ert-deftest lispm-mail-test-sign-in-refused ()
  "Refused on the provider's page, the sign-in says so."
  (let* ((lispm-mail-microsoft-client-id "client")
         (opened nil)
         (result nil)
         (lispm-mail-sign-in-browse-function (lambda (url) (setq opened url))))
    (lispm-mail-sign-in 'microsoft "ada@outlook.com" (lambda (value error) (setq result (list value error))))
    (let* ((query (url-parse-query-string (cadr (split-string opened "?"))))
           (redirect (cadr (assoc "redirect_uri" query)))
           (port (string-to-number (car (last (split-string redirect ":"))))))
      (lispm-mail-test-ask port (format "GET /?error=access_denied&error_description=No+thanks&state=%s HTTP/1.1\r\n\r\n"
                                        (cadr (assoc "state" query))))
      (should (equal result '(nil "The sign-in was cancelled"))))
    (let ((lispm-mail-microsoft-client-id nil))
      (should-error (lispm-mail-sign-in 'microsoft "ada@outlook.com" #'ignore) :type 'user-error))))

(ert-deftest lispm-mail-test-sign-in-form ()
  "An Outlook.com or Gmail account's form signs in instead of asking for a
password, and keeps the token got."
  (lispm-mail-test-with-home
    (let ((lispm-mail-microsoft-client-id "client")
          (lispm-mail-google-client-id "gclient")
          (kept nil))
      (cl-letf (((symbol-function 'lispm-mail-set-password)
                 (lambda (account secret) (push (cons (plist-get account :id) secret) kept)))
                ((symbol-function 'lispm-mail-save-accounts)
                 (lambda (accounts) (setq lispm-mail-accounts accounts)))
                ((symbol-function 'lispm-mail-check) #'ignore)
                ((symbol-function 'lispm-mail) #'ignore))
        (with-current-buffer (lispm-mail-add-account)
          (unwind-protect
              (progn
                (lispm-ui-form-set :address "ada@gmail.com")
                (lispm-mail-account--fill (lispm-mail-provider-settings "ada@gmail.com"))
                (should (string-match-p "Sign In with Google…" (buffer-string)))
                (should-not (string-match-p "^ *Password" (buffer-string)))
                (should-not (string-match-p "[Aa]pp password" (buffer-string)))
                (lispm-ui-form-set :address "ada@icloud.com")
                (lispm-mail-account--fill (lispm-mail-provider-settings "ada@icloud.com"))
                (should (string-match-p "^ *Password" (buffer-string)))
                (lispm-ui-form-set :address "ada@hotmail.com")
                (lispm-mail-account--fill (lispm-mail-provider-settings "ada@hotmail.com"))
                (should (string-match-p "Sign In with Microsoft…" (buffer-string)))
                (should-not (string-match-p "^ *Password" (buffer-string)))
                (should (equal (lispm-mail-account--problem) "Sign in with Microsoft first"))
                (setq lispm-mail-account--token '(:access-token "A" :refresh-token "R"))
                (lispm-ui-form-redraw)
                (should (string-match-p "Signed in with Microsoft" (buffer-string)))
                (should (equal (plist-get (plist-get (lispm-mail-account--credentials) :oauth)
                                          :access-token)
                               "A"))
                (lispm-mail-account-save))
            (when (buffer-live-p (current-buffer)) (kill-buffer (current-buffer)))))
        (should (equal kept '(("ada" . "R"))))
        (should (eq (plist-get (car lispm-mail-accounts) :sign-in) 'microsoft))
        (should (equal (plist-get (plist-get (car lispm-mail-accounts) :imap) :host)
                       "outlook.office365.com"))))))

;;;; The settings

(defun lispm-mail-test--widget (key &optional nth)
  "Return the NTH (default first) widget of the field KEY of the form here."
  (nth (or nth 0)
       (let (found)
         (save-excursion
           (goto-char (point-min))
           (while (not (eobp))
             (when-let* ((widget (widget-at (point))))
               (when (and (eq (widget-get widget :lispm-form-key) key) (not (memq widget found)))
                 (push widget found)))
             (forward-char 1)))
         (nreverse found))))

(defun lispm-mail-test--type (key text)
  "Type TEXT at the end of the text field KEY of the form here."
  (let ((field (alist-get key (lispm-ui-form-widgets))))
    (goto-char (widget-field-text-end field))
    (insert text)))

(ert-deftest lispm-mail-test-settings-form ()
  "The settings are a form applying each change at once: the accounts a
row each, edited and removed from their buttons, each setting saved as
it is changed, Done going back."
  (lispm-mail-test-with-home
    (let ((saved nil)
          (removed nil)
          (lispm-mail-accounts (list lispm-mail-test-account
                                     '(:id "bob" :name "Bob" :address "bob@example.org" :label "Work"
                                       :imap (:host "imap.example.org" :port 993 :security tls :user "bob")
                                       :smtp (:host "smtp.example.org" :port 465 :security tls :user "bob"))))
          (lispm-mail-check-interval 15))
      (cl-letf (((symbol-function 'customize-save-variable)
                 (lambda (variable value) (set variable value) (push (cons variable value) saved)))
                ((symbol-function 'lispm-mail-save-accounts) (lambda (accounts) (setq lispm-mail-accounts accounts)))
                ((symbol-function 'lispm-mail-stop-workers) #'ignore)
                ((symbol-function 'lispm-mail-forget-password) (lambda (account) (push (plist-get account :id) removed)))
                ((symbol-function 'yes-or-no-p) (lambda (_) t))
                ((symbol-function 'lispm-mail) #'ignore))
        (save-window-excursion
          (let ((buffer (lispm-mail-settings))
                (form nil))
            (unwind-protect
                (progn
                  (with-current-buffer buffer
                    (should (eq major-mode 'lispm-mail-settings-mode))
                    (should (derived-mode-p 'lispm-ui-form-mode))
                    (let ((text (buffer-string)))
                      (should (string-match-p "○ ada@example\\.com +ada@example\\.com · default +Edit… +Remove" text))
                      (should (string-match-p "○ Work +bob@example\\.org +Edit… +Remove" text))
                      (should (string-match-p "Write new mail from: +ada@example\\.com ▾" text))
                      (should (string-match-p "Check for new mail: +Every 15 minutes ▾" text))
                      (should (string-match-p "They tell senders that you read their message" text))
                      (should-not (string-match-p "Cancel" text))
                      (should (string-match-p " Done $" text)))
                    ;; A check box is saved as it is pressed, a pop-up menu as
                    ;; it is chosen.
                    (widget-apply-action (lispm-mail-test--widget 'lispm-mail-load-remote-pictures))
                    (should (equal (car saved) '(lispm-mail-load-remote-pictures . t)))
                    (let ((lispm-ui-form-choose-function (lambda (&rest _) '(5))))
                      (widget-apply-action (lispm-mail-test--widget 'lispm-mail-check-interval)))
                    (should (equal (car saved) '(lispm-mail-check-interval . 5)))
                    (should (string-match-p "Every 5 minutes ▾" (buffer-string)))
                    ;; Remove, under the second account, removes it.
                    (widget-apply-action (lispm-mail-test--widget 'account-list 3))
                    (should (equal removed '("bob")))
                    (should (equal (mapcar #'lispm-mail-account-id lispm-mail-accounts) '("ada")))
                    (should-not (string-match-p "bob@example" (buffer-string)))
                    (should-not (string-match-p "Write new mail from" (buffer-string)))
                    ;; Edit… opens the account's form.
                    (widget-apply-action (lispm-mail-test--widget 'account-list 0)))
                  (setq form (window-buffer))
                  (with-current-buffer form
                    (should (eq major-mode 'lispm-mail-account-mode))
                    (should (equal (lispm-ui-form-value :address) "ada@example.com"))
                    (should (string-match-p "Edit Account" (buffer-string)))
                    ;; Cancelled, it shows the settings again.
                    (lispm-mail-account-cancel))
                  (should-not (buffer-live-p form))
                  (should (eq (window-buffer) buffer))
                  (with-current-buffer buffer
                    (lispm-mail-settings-done))
                  (should-not (buffer-live-p buffer)))
              (dolist (each (list form buffer))
                (when (buffer-live-p each) (kill-buffer each))))))))))

(ert-deftest lispm-mail-test-account-form ()
  "An account's form fills its servers in as its address is typed, takes
a password as stars, says how Test Connection went where it says it,
the rest of the form as it was, has its keys in its fields, and saves."
  (lispm-mail-test-with-home
   (let ((checked nil)
         (kept nil))
     (cl-letf (((symbol-function 'lispm-mail-check-servers)
                (lambda (imap smtp callback) (setq checked (list imap smtp callback))))
               ((symbol-function 'lispm-mail-set-password)
                (lambda (account secret) (push (cons (plist-get account :id) secret) kept)))
               ((symbol-function 'lispm-mail-save-accounts) (lambda (accounts) (setq lispm-mail-accounts accounts)))
               ((symbol-function 'lispm-mail-stop-workers) #'ignore)
               ((symbol-function 'lispm-mail-check) #'ignore)
               ((symbol-function 'lispm-mail) #'ignore))
       (save-window-excursion
         (let ((form (lispm-mail-add-account)))
           (unwind-protect
               (with-current-buffer form
                 (let ((icloud (lispm-mail-provider-settings "ada@icloud.com")))
                   (should (eq major-mode 'lispm-mail-account-mode))
                   (should (string-match-p "Add Account" (buffer-string)))
                   ;; Its keys work in its fields.
                   (goto-char (widget-field-start (alist-get :address (lispm-ui-form-widgets))))
                   (should (eq (key-binding (kbd "C-c C-c")) #'lispm-mail-account-save))
                   (should (eq (key-binding (kbd "C-c C-t")) #'lispm-mail-account-test))
                   ;; Typed, the address fills the servers in once it is done.
                   (lispm-mail-test--type :address "ada@icloud.com")
                   (should (equal (lispm-ui-form-value :imap-host) (plist-get (plist-get icloud :imap) :host)))
                   (should (lispm-mail-test-wait (lambda () (string-match-p "Servers of iCloud" (buffer-string)))))
                   (should (equal (widget-value (alist-get :smtp-host (lispm-ui-form-widgets)))
                                  (plist-get (plist-get icloud :smtp) :host)))
                   ;; The password is shown as stars.
                   (lispm-mail-test--type :password "s*cret")
                   (should (equal (lispm-ui-form-value :password) "s*cret"))
                   (should-not (string-match-p "cret" (buffer-string)))
                   ;; Test Connection says how it went where it says it.
                   (let ((field (alist-get :name (lispm-ui-form-widgets))))
                     (goto-char (widget-field-start field))
                     (let ((here (point)))
                       (lispm-mail-account-test)
                       (should (string-match-p "Status: Logging in to the servers…" (buffer-string)))
                       (should (equal (plist-get (car checked) :password) "s*cret"))
                       (funcall (nth 2 checked) '(:imap t :smtp "Refused"))
                       (should (string-match-p "Status: ● Incoming (IMAP): logged in\n *● Outgoing (SMTP): Refused"
                                               (buffer-string)))
                       (should (eq (alist-get :name (lispm-ui-form-widgets)) field))
                       (should (= (point) here))))
                   (lispm-mail-account-save)))
             (when (buffer-live-p form) (kill-buffer form)))))
       (should (equal kept '(("ada" . "s*cret"))))
       (should (equal (plist-get (plist-get (car lispm-mail-accounts) :imap) :host)
                      (plist-get (plist-get (lispm-mail-provider-settings "ada@icloud.com") :imap) :host)))))))

;;;; Mailboxes and messages

(ert-deftest lispm-mail-test-roles ()
  "A mailbox's role is what its server says, or what its name says."
  (should (eq (lispm-mail-mailbox-role '(:name "INBOX")) 'inbox))
  (should (eq (lispm-mail-mailbox-role '(:name "[Gmail]/Sent Mail" :delimiter "/" :flags ("\\HasNoChildren" "\\Sent")))
              'sent))
  (should (eq (lispm-mail-mailbox-role '(:name "Deleted Messages" :delimiter "/")) 'trash))
  (should (eq (lispm-mail-mailbox-role '(:name "INBOX.Spam" :delimiter ".")) 'junk))
  (should (eq (lispm-mail-mailbox-role '(:name "草稿箱" :delimiter "/")) 'drafts))
  (should-not (lispm-mail-mailbox-role '(:name "Projects/LispM" :delimiter "/")))
  (should (equal (mapcar (lambda (mailbox) (plist-get mailbox :name))
                         (lispm-mail--sort-mailboxes
                          '((:name "Zeta") (:name "Trash" :role trash) (:name "INBOX" :role inbox)
                            (:name "Alpha") (:name "Sent" :role sent))))
                 '("INBOX" "Sent" "Trash" "Alpha" "Zeta"))))

(defun lispm-mail-test-message (uid &rest properties)
  "Return a message UID as a sync gives it, with PROPERTIES."
  (append properties (list :uid uid :flags nil :date (+ 1793000000 uid) :subject (format "Message %d" uid)
                           :from (cons "Grace" "grace@navy.mil"))))

(ert-deftest lispm-mail-test-merge ()
  "A sync's result is kept: flags changed, messages gone dropped, new ones added."
  (lispm-mail-test-with-home
    (lispm-mail--merge "ada" "INBOX"
                       (list :uidvalidity "7" :exists 3
                             :flags '((1) (2 "\\Seen") (3))
                             :headers (list (lispm-mail-test-message 1) (lispm-mail-test-message 2)
                                            (lispm-mail-test-message 3))))
    (should (equal (mapcar (lambda (m) (plist-get m :uid)) (lispm-mail-messages "ada" "INBOX")) '(3 2 1)))
    (let ((new (lispm-mail--merge "ada" "INBOX"
                                  (list :uidvalidity "7" :exists 3
                                        :flags '((2) (3 "\\Flagged") (4))
                                        :headers (list (lispm-mail-test-message 4))))))
      (should (equal (mapcar (lambda (m) (plist-get m :uid)) new) '(4))))
    (should (equal (mapcar (lambda (m) (plist-get m :uid)) (lispm-mail-messages "ada" "INBOX")) '(4 3 2)))
    (should (lispm-mail-flagged-p (lispm-mail-message "ada" "INBOX" 3)))
    (should-not (lispm-mail-seen-p (lispm-mail-message "ada" "INBOX" 2)))
    ;; Another UIDVALIDITY: what was kept is no longer those messages.
    (lispm-mail--merge "ada" "INBOX" (list :uidvalidity "8" :exists 1 :flags '((9))
                                           :headers (list (lispm-mail-test-message 9))))
    (should (equal (mapcar (lambda (m) (plist-get m :uid)) (lispm-mail-messages "ada" "INBOX")) '(9)))))

(ert-deftest lispm-mail-test-kept-on-disk ()
  "What is kept of a mailbox is saved, and read again by another session."
  (lispm-mail-test-with-home
    (lispm-mail--merge "ada" "INBOX" (list :uidvalidity "7" :exists 1 :flags '((5 "\\Seen"))
                                           :headers (list (lispm-mail-test-message 5 :subject "周末"))))
    (lispm-mail--changed "ada" "INBOX")
    (lispm-mail--save-boxes)
    (clrhash lispm-mail--boxes)
    (let ((message (lispm-mail-message "ada" "INBOX" 5)))
      (should (equal (plist-get message :subject) "周末"))
      (should (lispm-mail-seen-p message)))))

(ert-deftest lispm-mail-test-flags-done-at-once ()
  "A flag is set here at once, and set back when the server refuses."
  (lispm-mail-test-with-home
    (let ((requests nil)
          (lispm-mail-accounts (list lispm-mail-test-account)))
      (lispm-mail--set-mailboxes "ada" '((:name "INBOX" :unseen 1)))
      (lispm-mail--merge "ada" "INBOX" (list :uidvalidity "7" :exists 1 :flags '((5))
                                             :headers (list (lispm-mail-test-message 5))))
      (cl-letf (((symbol-function 'lispm-mail-request)
                 (lambda (_id operation properties callback)
                   (push (cons operation properties) requests)
                   (funcall callback nil "No permission"))))
        (let ((inhibit-message t))
          (lispm-mail-set-flag "ada" "INBOX" '(5) "\\Seen" t)))
      (should (equal (car requests) '(store :mailbox "INBOX" :uids (5) :add "\\Seen")))
      ;; Refused: unread again, and counted so.
      (should-not (lispm-mail-seen-p (lispm-mail-message "ada" "INBOX" 5)))
      (should (= (plist-get (lispm-mail-mailbox "ada" "INBOX") :unseen) 1)))))

(ert-deftest lispm-mail-test-flagged-roles-win ()
  "A role the server flags a mailbox with leaves mailboxes only named so
ordinary folders, as Gmail's labels an old program made; without flags,
names still tell."
  (lispm-mail-test-with-home
    (let ((lispm-mail-accounts (list lispm-mail-test-account)))
      (lispm-mail--set-mailboxes
       "ada" '((:name "INBOX" :delimiter "/")
               (:name "Drafts" :delimiter "/")
               (:name "Sent Mail" :delimiter "/")
               (:name "[Gmail]" :delimiter "/" :flags ("\\Noselect"))
               (:name "[Gmail]/Drafts" :delimiter "/" :flags ("\\Drafts"))
               (:name "[Gmail]/Sent Mail" :delimiter "/" :flags ("\\Sent"))
               (:name "[Gmail]/Trash/Trash" :delimiter "/")
               (:name "[Gmail]/Trash" :delimiter "/" :flags ("\\HasChildren" "\\Trash"))
               (:name "Archive" :delimiter "/")))
      (should (equal (mapcar (lambda (role) (lispm-mail-mailbox-of-role "ada" role))
                             '(drafts sent trash archive))
                     '("[Gmail]/Drafts" "[Gmail]/Sent Mail" "[Gmail]/Trash" "Archive")))
      (dolist (name '("Drafts" "Sent Mail" "[Gmail]/Trash/Trash"))
        (should-not (plist-get (lispm-mail-mailbox "ada" name) :role)))
      (let ((requests nil))
        (cl-letf (((symbol-function 'lispm-mail-request)
                   (lambda (_id operation properties _callback)
                     (push (cons operation properties) requests))))
          (lispm-mail-delete "ada" "INBOX" '(5)))
        (should (equal requests '((move :mailbox "INBOX" :uids (5) :to "[Gmail]/Trash")))))
      (lispm-mail--set-mailboxes "ada" '((:name "INBOX") (:name "Sent") (:name "Trash")))
      (should (equal (mapcar (lambda (role) (lispm-mail-mailbox-of-role "ada" role)) '(sent trash))
                     '("Sent" "Trash"))))))

(ert-deftest lispm-mail-test-delete-to-trash ()
  "Deleting moves to the Trash, and from the Trash deletes for good."
  (lispm-mail-test-with-home
    (let ((requests nil)
          (lispm-mail-accounts (list lispm-mail-test-account)))
      (lispm-mail--set-mailboxes "ada" '((:name "INBOX") (:name "Trash" :flags ("\\Trash"))))
      (dolist (mailbox '("INBOX" "Trash"))
        (lispm-mail--merge "ada" mailbox (list :uidvalidity "7" :exists 1 :flags '((5 "\\Seen"))
                                               :headers (list (lispm-mail-test-message 5)))))
      (cl-letf (((symbol-function 'lispm-mail-request)
                 (lambda (_id operation properties _callback)
                   (push (cons operation properties) requests))))
        (lispm-mail-delete "ada" "INBOX" '(5))
        (lispm-mail-delete "ada" "Trash" '(5)))
      (should (equal (reverse requests)
                     '((move :mailbox "INBOX" :uids (5) :to "Trash")
                       (expunge :mailbox "Trash" :uids (5)))))
      (should-not (lispm-mail-messages "ada" "INBOX")))))

(ert-deftest lispm-mail-test-new-mail ()
  "Mail come in an Inbox since it was checked, unread, is said to be new."
  (lispm-mail-test-with-home
    (let ((said nil))
      (lispm-mail--set-mailboxes "ada" '((:name "INBOX")))
      (add-hook 'lispm-mail-new-mail-functions (lambda (_id _mailbox messages) (setq said messages)))
      (lispm-mail--note-new-mail "ada" "INBOX"
                                 (list (lispm-mail-test-message 8)
                                       (lispm-mail-test-message 9 :flags '("\\Seen")))
                                 nil '(1 2 7))
      (should (equal (mapcar (lambda (m) (plist-get m :uid)) said) '(8)))
      (setq said nil)
      (lispm-mail--note-new-mail "ada" "INBOX" (list (lispm-mail-test-message 8)) t nil)
      (should-not said))))

;;;; Searching the server

(defvar lispm-mail-test-imap-searches nil
  "The searches the fake IMAP server was asked, the latest first: (KEY TEXT
LITERAL), LITERAL non-nil when TEXT came as a literal.")

(defun lispm-mail-test-imap-uids (set)
  "Return the UIDs of SET, an IMAP message set of numbers and ranges."
  (mapcan (lambda (part)
            (if (string-match "\\`\\([0-9]+\\):\\([0-9]+\\)\\'" part)
                (number-sequence (string-to-number (match-string 1 part))
                                 (string-to-number (match-string 2 part)))
              (list (string-to-number part))))
          (split-string set ",")))

(defun lispm-mail-test-imap-answer (process tag command messages capabilities search)
  "Answer COMMAND, tagged TAG, on PROCESS, as a fake IMAP server of one
mailbox holding MESSAGES, ((UID . HEADER) ...), with CAPABILITIES.
SEARCH, a function of a search key and its text, gives the UIDs found."
  (let ((answer
         (cond
          ((string-match "\\`CAPABILITY" command)
           (format "* CAPABILITY IMAP4rev1 %s\r\n" capabilities))
          ((string-match "\\`\\(SELECT\\|EXAMINE\\) " command)
           (format "* %d EXISTS\r\n* 0 RECENT\r\n* OK [UIDVALIDITY 7] UIDs valid\r\n* FLAGS (\\Seen)\r\n"
                   (length messages)))
          ((string-match "\\`UID SEARCH \\(?:CHARSET UTF-8 \\)?\\([A-Z-]+\\) \\(?:\"\\(.*\\)\"\\|{[0-9]+}\r\n\\(.*\\)\\)\\'"
                         command)
           (let* ((key (match-string 1 command))
                  (quoted (match-string 2 command))
                  (text (or (and quoted (replace-regexp-in-string "\\\\\\(.\\)" "\\1" quoted))
                            (decode-coding-string (match-string 3 command) 'utf-8))))
             (push (list key text (null quoted)) lispm-mail-test-imap-searches)
             (format "* SEARCH%s\r\n" (mapconcat (lambda (uid) (format " %d" uid)) (funcall search key text) ""))))
          ((string-match "\\`UID FETCH \\([0-9,:]+\\) " command)
           (mapconcat (lambda (uid)
                        (when-let* ((header (alist-get uid messages)))
                          (format "* %d FETCH (UID %d FLAGS (\\Seen) INTERNALDATE \"27-Oct-2026 10:00:00 +0000\" RFC822.SIZE 400 BODYSTRUCTURE (\"text\" \"plain\" (\"charset\" \"utf-8\") NIL NIL \"7bit\" 5 1) BODY[HEADER.FIELDS (DATE FROM SUBJECT)] {%d}\r\n%s)\r\n"
                                  (1+ (seq-position (mapcar #'car messages) uid)) uid
                                  (string-bytes header) header)))
                      (lispm-mail-test-imap-uids (match-string 1 command)) ""))
          ((string-match "\\`LOGOUT" command) "* BYE\r\n")
          (t ""))))
    (process-send-string process (concat answer tag " OK done\r\n"))))

(defun lispm-mail-test-imap-server (messages capabilities search)
  "Start a fake IMAP server on the loopback; return its process.
See `lispm-mail-test-imap-answer' for MESSAGES, CAPABILITIES and SEARCH.
A command may end in one literal, as imap.el sends them."
  (let (server)
    (setq server
          (make-network-process
           :name "lispm-mail-test imap" :server t :host "127.0.0.1" :service t :coding 'binary
           :log (lambda (_server client _message)
                  (process-put server :clients (cons client (process-get server :clients)))
                  (process-send-string client "* OK fake IMAP ready\r\n"))
           :filter (lambda (process output)
                     (let ((input (concat (process-get process :input) output))
                           (more t))
                       (while (and more (string-match "\r\n" input))
                         (let* ((end (match-beginning 0))
                                (line (substring input 0 end))
                                (size (and (string-match "{\\([0-9]+\\)}\\'" line)
                                           (string-to-number (match-string 1 line)))))
                           (cond
                            ((null size)
                             (setq input (substring input (+ end 2)))
                             (when (string-match "\\`\\([^ ]+\\) \\(.*\\)\\'" line)
                               (lispm-mail-test-imap-answer process (match-string 1 line) (match-string 2 line)
                                                            messages capabilities search)))
                            ((>= (length input) (+ end 2 size 2))
                             ;; The literal is here, and the line ending it.
                             (let ((command (substring input 0 (+ end 2 size))))
                               (setq input (substring input (+ end 2 size 2)))
                               (process-put process :continued nil)
                               (when (string-match "\\`\\([^ ]+\\) \\(\\(?:.\\|\n\\)*\\)\\'" command)
                                 (lispm-mail-test-imap-answer process (match-string 1 command)
                                                              (match-string 2 command)
                                                              messages capabilities search))))
                            (t
                             (unless (process-get process :continued)
                               (process-put process :continued t)
                               (process-send-string process "+ go on\r\n"))
                             (setq more nil)))))
                       (process-put process :input input)))))
    server))

(defun lispm-mail-test-imap-stop (server)
  "Stop SERVER, a fake IMAP server, and its connections."
  (dolist (client (process-get server :clients))
    (when (process-live-p client) (delete-process client)))
  (when (process-live-p server) (delete-process server)))

(ert-deftest lispm-mail-test-search-server ()
  "A search on the server goes as Gmail's own search on Gmail, its text a
literal in UTF-8 unless plain ASCII; elsewhere a search a word, the UIDs
found by them all kept; the newest found come with their headers."
  (let* ((messages '((3 . "Subject: One\r\nFrom: ada@example.com\r\n\r\n")
                     (5 . "Subject: Two\r\nFrom: ada@example.com\r\n\r\n")
                     (9 . "Subject: Three\r\nFrom: Grace <grace@navy.mil>\r\n\r\n")))
         (lispm-mail-test-imap-searches nil)
         (lispm-mail-worker--buffer nil)
         (lispm-mail-worker--token nil)
         (inhibit-message t)
         (search (lambda (key text)
                   (pcase (list key text)
                     ((or '("X-GM-RAW" "from:ada réunion") '("X-GM-RAW" "from:ada \"lunch\"")) '(3 5 9))
                     ('("TEXT" "lunch") '(3 5 9))
                     ('("TEXT" "réunion") '(5 9 11)))))
         (server nil))
    (unwind-protect
        (dolist (gmail '(t nil))
          (setq server (lispm-mail-test-imap-server messages (if gmail "X-GM-EXT-1" "") search))
          (let ((lispm-mail-worker--imap (list :host "127.0.0.1" :port (process-contact server :service)
                                               :security 'none :user "ada" :password "pw")))
            (if gmail
                (let ((found (lispm-mail-worker--search
                              '(:mailbox "[Gmail]/All Mail" :query "from:ada réunion" :limit 2))))
                  (should (equal (car lispm-mail-test-imap-searches) '("X-GM-RAW" "from:ada réunion" t)))
                  (should (equal (plist-get found :uidvalidity) "7"))
                  (should (= (plist-get found :total) 3))
                  (should (equal (mapcar (lambda (m) (list (plist-get m :uid) (plist-get m :subject)))
                                         (plist-get found :headers))
                                 '((9 "Three") (5 "Two"))))
                  (should (equal (plist-get (car (plist-get found :headers)) :from)
                                 '("Grace" . "grace@navy.mil")))
                  ;; Plain ASCII goes quoted.
                  (lispm-mail-worker--search '(:mailbox "[Gmail]/All Mail" :query "from:ada \"lunch\""))
                  (should (equal (car lispm-mail-test-imap-searches)
                                 '("X-GM-RAW" "from:ada \"lunch\"" nil))))
              (setq lispm-mail-test-imap-searches nil)
              (let ((found (lispm-mail-worker--search '(:mailbox "INBOX" :query " lunch réunion "))))
                (should (equal (reverse lispm-mail-test-imap-searches)
                               '(("TEXT" "lunch" nil) ("TEXT" "réunion" t))))
                (should (= (plist-get found :total) 2))
                (should (equal (mapcar (lambda (m) (plist-get m :uid)) (plist-get found :headers)) '(9 5))))
              (should-error (lispm-mail-worker--search '(:mailbox "INBOX" :query "  ")))))
          (lispm-mail-worker--drop-connection)
          (lispm-mail-test-imap-stop server))
      (lispm-mail-worker--drop-connection)
      (when server (lispm-mail-test-imap-stop server)))))

(ert-deftest lispm-mail-test-search-on-server ()
  "A search asks the account's Emacs, in the mailbox of all its mail, or
its Inbox without one, and says where it looked."
  (lispm-mail-test-with-home
    (let ((lispm-mail-accounts (list lispm-mail-test-account))
          asked said)
      (cl-letf (((symbol-function 'lispm-mail-request)
                 (lambda (_id operation properties callback)
                   (push (cons operation properties) asked)
                   (funcall callback '(:uidvalidity 7 :total 0 :headers nil) nil))))
        (lispm-mail-search-on-server "ada" "lunch" (lambda (found error) (setq said (list found error))))
        (should (equal (car asked) '(search :mailbox "INBOX" :query "lunch" :limit 20)))
        (should (equal said '((:mailbox "INBOX" :uidvalidity 7 :total 0 :headers nil) nil)))
        (lispm-mail--set-mailboxes "ada" '((:name "INBOX") (:name "[Gmail]/All Mail" :flags ("\\All"))))
        (lispm-mail-search-on-server "ada" "lunch" #'ignore nil 5)
        (should (equal (car asked) '(search :mailbox "[Gmail]/All Mail" :query "lunch" :limit 5)))))))

;;;; Showing

(defun lispm-mail-test-file (name)
  "Return the file of the test message NAME."
  (expand-file-name name lispm-mail-test-directory))

(ert-deftest lispm-mail-test-words ()
  "Names, dates and sizes are said as a list says them."
  (should (equal (lispm-mail-style-initials '("Grace Hopper" . "grace@navy.mil")) "GH"))
  (should (equal (lispm-mail-style-initials '(nil . "ada@example.com")) "A"))
  (should (equal (lispm-mail-style-initials '("王小明" . "x@163.com")) "王"))
  (should (equal (lispm-mail-date-text (float-time)) (format-time-string "%-I:%M %p")))
  (should (equal (lispm-mail-date-text (- (float-time) 86400)) "Yesterday"))
  (should (equal (lispm-mail-size-text 9300) "9 KB"))
  (should (equal (lispm-mail-size-text 2500000) "2.5 MB")))

(ert-deftest lispm-mail-test-show-message ()
  "A message is drawn: its head, its HTML or text, its attachment."
  (with-temp-buffer
    (let* ((lispm-mail-prefer-html nil)
           (said (lispm-mail-view-insert (lispm-mail-test-file "report.eml")))
           (text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "Subject:\\s-*Quarterly report" text))
      (should (string-match-p "From:\\s-*Grace Hopper <grace@navy.mil>" text))
      (should (string-search "Grace Hopper" text))
      (should (string-search "The numbers for Q3 are on page 2." text))
      (should (string-search "report.pdf" text))
      (should (= (length (plist-get said :attachments)) 1))
      (should (equal (mapcar #'lispm-mail-view-attachment-name (lispm-mail-view-attachments-here))
                     '("report.pdf")))
      (lispm-mail-view-release))))

(ert-deftest lispm-mail-test-remote-pictures ()
  "Pictures on the web are not fetched unless asked, and the window says so."
  (with-temp-buffer
    (let* ((lispm-mail-prefer-html t)
           (lispm-mail-load-remote-pictures nil)
           (said (lispm-mail-view-insert (lispm-mail-test-file "report.eml"))))
      (when (fboundp 'libxml-parse-html-region)
        (should (plist-get said :pictures))
        (should (string-search "Pictures from the web are not shown" (buffer-string))))
      (lispm-mail-view-release))))

(ert-deftest lispm-mail-test-quoted-text ()
  "A reply quotes the message's plain text."
  (should (string-match-p "\\`Here is the report\\."
                          (lispm-mail-view-plain-text (lispm-mail-test-file "report.eml")))))

(ert-deftest lispm-mail-test-read-message ()
  "A message is read, fetched first: its headers, text and attachments' names."
  (lispm-mail-test-with-home
    (let ((lispm-mail-accounts (list lispm-mail-test-account))
          fetched said)
      (cl-letf (((symbol-function 'lispm-mail-request)
                 (lambda (_id operation properties callback)
                   (push (cons operation properties) fetched)
                   ;; As the background Emacs writes it.
                   (make-directory (file-name-directory (plist-get properties :file)) t)
                   (copy-file (lispm-mail-test-file "report.eml") (plist-get properties :file) t)
                   (funcall callback 100 nil))))
        (lispm-mail-read-message "ada" "INBOX" 4 (lambda (message error) (setq said (list message error))))
        (should (equal (caar fetched) 'fetch))
        (should-not (nth 1 said))
        (let ((message (car said)))
          (should (equal (plist-get message :subject) "Quarterly report"))
          (should (equal (plist-get message :from) '("Grace Hopper" . "grace@navy.mil")))
          (should (string-match-p "\\`Here is the report\\." (plist-get message :text)))
          (should (equal (plist-get message :attachments) '("report.pdf"))))
        ;; Kept, it is not fetched again.
        (lispm-mail-read-message "ada" "INBOX" 4 #'ignore)
        (should (= (length fetched) 1))
        (cl-letf (((symbol-function 'lispm-mail-request)
                   (lambda (_id _operation _properties callback) (funcall callback nil "The message is no longer there"))))
          (lispm-mail-read-message "ada" "INBOX" 5 (lambda (message error) (setq said (list message error))))
          (should (equal said '(nil "The message is no longer there"))))))))

;;;; Writing

(ert-deftest lispm-mail-test-subjects ()
  "A reply's subject is prefixed once, whatever language prefixed it before."
  (should (equal (lispm-mail-compose--subject "Re" "Lunch") "Re: Lunch"))
  (should (equal (lispm-mail-compose--subject "Re" "RE: Re: Lunch") "Re: Lunch"))
  (should (equal (lispm-mail-compose--subject "Re" "回复：周末") "Re: 周末"))
  (should (equal (lispm-mail-compose--subject "Fwd" "Fw: Notes") "Fwd: Notes")))

(ert-deftest lispm-mail-test-compose ()
  "A message is written from its account, its Message-ID out of sight."
  (lispm-mail-test-with-home
    (let ((lispm-mail-accounts (list lispm-mail-test-account)))
      (with-current-buffer (lispm-mail-compose :to "grace@navy.mil" :subject "Lunch"
                                               :replying '("<1@navy.mil>" . "<0@navy.mil>")
                                               :switch-function #'set-buffer)
        (unwind-protect
            (let ((shown (buffer-substring-no-properties (point-min) (point-max)))
                  (all (save-restriction (widen) (buffer-substring-no-properties (point-min) (point-max)))))
              (should (derived-mode-p 'lispm-mail-compose-mode))
              (should (string-search "From: Ada Lovelace <ada@example.com>" shown))
              (should (string-search "Subject: Lunch" shown))
              (should-not (string-search "Message-I" shown))
              (should (string-search "References: <0@navy.mil> <1@navy.mil>" all))
              (should (string-search "In-Reply-To: <1@navy.mil>" all))
              (should (eq message-send-mail-function #'lispm-mail-compose--send-it)))
          (set-buffer-modified-p nil)
          (kill-buffer))))))

(ert-deftest lispm-mail-test-reply-all ()
  "A reply to all goes to the sender, and to the others but oneself."
  (lispm-mail-test-with-home
    (let ((lispm-mail-accounts (list lispm-mail-test-account))
          (lispm-mail-compose-display-function #'set-buffer))
      (with-current-buffer
          (lispm-mail-reply "ada" (list :from '("Grace Hopper" . "grace@navy.mil")
                                        :to '((nil . "ada@example.com"))
                                        :cc '(("Alan Turing" . "alan@bletchley.uk") (nil . "ADA@example.com"))
                                        :subject "Quarterly report" :message-id "<1@navy.mil>"
                                        :date 1793000000)
                            (lispm-mail-test-file "report.eml") t)
        (unwind-protect
            (save-restriction
              (message-narrow-to-headers)
              (should (equal (message-fetch-field "to") "Grace Hopper <grace@navy.mil>"))
              (should (equal (message-fetch-field "cc") "Alan Turing <alan@bletchley.uk>"))
              (should (equal (message-fetch-field "subject") "Re: Quarterly report")))
          (should (string-search "> Here is the report." (buffer-string)))
          (set-buffer-modified-p nil)
          (kill-buffer))))))

(ert-deftest lispm-mail-test-draft-mime ()
  "A draft is kept as MIME, its headers ending at a blank line, with a Date."
  (lispm-mail-test-with-home
    (let ((lispm-mail-accounts (list lispm-mail-test-account)))
      (with-current-buffer (lispm-mail-compose :to "grace@navy.mil" :subject "Café" :body "Bonjour"
                                               :switch-function #'set-buffer)
        (unwind-protect
            (let ((mime (lispm-mail-compose--mime)))
              (should-not (string-search mail-header-separator mime))
              (should (string-match-p "^Date: " mime))
              (should (string-match-p "^Subject: =\\?utf-8\\?" mime))
              (should (string-search "\n\nBonjour" mime)))
          (set-buffer-modified-p nil)
          (kill-buffer))))))

(ert-deftest lispm-mail-test-save-draft ()
  "A message written by a program is kept in the Drafts mailbox, unsent and
shown nowhere: a reply with its headers and the text it quotes."
  (lispm-mail-test-with-home
    (let ((lispm-mail-accounts (list lispm-mail-test-account))
          (buffers (buffer-list))
          (windows (window-list))
          kept said)
      (lispm-mail--set-mailboxes "ada" '((:name "INBOX") (:name "Drafts" :flags ("\\Drafts"))))
      (cl-letf (((symbol-function 'lispm-mail-request)
                 (lambda (_id operation properties callback)
                   (when (eq operation 'append)
                     (push (list (plist-get properties :mailbox) (plist-get properties :flags)
                                 (with-temp-buffer
                                   (insert-file-contents (plist-get properties :file))
                                   (buffer-string)))
                           kept))
                   (funcall callback t nil))))
        (lispm-mail-save-draft
         (append (list :body "Thursday works.\n")
                 (lispm-mail-reply-fields "ada" (list :from '("Grace Hopper" . "grace@navy.mil")
                                                      :subject "Lunch" :message-id "<1@navy.mil>"
                                                      :date 1793000000)
                                          "Shall we?"))
         (lambda (mailbox error) (setq said (list mailbox error))))
        (should (equal said '("Drafts" nil)))
        (pcase-let ((`(,mailbox ,flags ,text) (car kept)))
          (should (equal mailbox "Drafts"))
          (should (equal flags '("\\Draft" "\\Seen")))
          (should (string-match-p "^From: Ada Lovelace <ada@example.com>$" text))
          (should (string-match-p "^To: Grace Hopper <grace@navy.mil>$" text))
          (should (string-match-p "^Subject: Re: Lunch$" text))
          ;; Emacs 30's message mode adds a comment after it.
          (should (string-match-p "^In-Reply-To: <1@navy.mil>" text))
          (should (string-search "Thursday works." text))
          (should (string-search "> Shall we?" text)))
        ;; No window showed it, and no buffer is left.
        (should (equal (window-list) windows))
        (should (seq-set-equal-p (seq-filter #'buffer-live-p buffers) (buffer-list)))
        ;; Without a Drafts mailbox, it says so.
        (lispm-mail--set-mailboxes "ada" '((:name "INBOX")))
        (lispm-mail-save-draft '(:to "grace@navy.mil" :subject "Hi" :body "Hello")
                               (lambda (mailbox error) (setq said (list mailbox error))))
        (should (equal said '(nil "ada@example.com has no Drafts mailbox")))
        (should (seq-set-equal-p (seq-filter #'buffer-live-p buffers) (buffer-list)))))))

(ert-deftest lispm-mail-test-outbox-kept ()
  "A message not sent is in the Outbox of the Emacs after, to send again."
  (lispm-mail-test-with-home
    (let ((lispm-mail-accounts (list lispm-mail-test-account))
          (lispm-mail-outbox nil)
          (inhibit-message t))
      (cl-letf (((symbol-function 'lispm-mail-request)
                 (lambda (_id _operation _properties callback) (funcall callback nil "Connection refused"))))
        (with-temp-buffer
          (insert "From: ada@example.com\nTo: grace@navy.mil\nSubject: Lunch\n"
                  mail-header-separator "\nHello\n")
          (lispm-mail-compose--send-it)))
      (should (eq (plist-get (car lispm-mail-outbox) :state) 'failed))
      (let ((file (plist-get (car lispm-mail-outbox) :file)))
        (setq lispm-mail-outbox nil)
        (lispm-mail-outbox-load)
        (should (equal (plist-get (car lispm-mail-outbox) :file) file))
        (should (equal (plist-get (car lispm-mail-outbox) :subject) "Lunch"))
        (lispm-mail-outbox-remove (car lispm-mail-outbox))
        (should-not (file-exists-p file))
        (should-not (file-exists-p (concat file ".eld")))))))

;;;; Emacs's mail

(ert-deftest lispm-mail-test-emacs-mail ()
  "LispM Mail becomes Emacs's mail only where Emacs's own was, and lets go."
  (let ((read-mail-command 'rmail)
        (mail-user-agent 'message-user-agent))
    (lispm-mail--use-for-emacs t)
    (should (eq read-mail-command #'lispm-mail))
    (should (eq mail-user-agent 'lispm-mail-user-agent))
    (lispm-mail--use-for-emacs nil)
    (should (eq read-mail-command 'rmail))
    (should (eq mail-user-agent 'message-user-agent)))
  (let ((read-mail-command 'gnus))
    (lispm-mail--use-for-emacs t)
    (should (eq read-mail-command 'gnus)))
  ;; As Emacs starts, before LispM Mail is loaded, unless said not.
  (let ((read-mail-command 'rmail)
        (mail-user-agent 'message-user-agent)
        (lispm-mail-use-for-emacs t))
    (lispm-mail--use-for-emacs-at-start)
    (should (eq read-mail-command 'lispm-mail))
    (should (eq mail-user-agent 'lispm-mail-user-agent)))
  (let ((read-mail-command 'rmail)
        (lispm-mail-use-for-emacs nil))
    (lispm-mail--use-for-emacs-at-start)
    (should (eq read-mail-command 'rmail))))

(ert-deftest lispm-mail-test-knows-no-desktop ()
  "LispM Mail knows nothing of LispM Desktop: the desktop hooks into it."
  (dolist (file (directory-files (expand-file-name ".." lispm-mail-test-directory) t "\\.el\\'"))
    (with-temp-buffer
      (insert-file-contents file)
      (should-not (re-search-forward "lispm-desktop\\|platinum" nil t)))))

(defun lispm-mail-test--weightless-blank-lines ()
  "Return where an empty line of this buffer has a newline of `line-height' t."
  (let ((found nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\n" nil t)
        (when (eq (get-text-property (1- (point)) 'line-height) t)
          (push (1- (point)) found))))
    found))

(ert-deftest lispm-mail-test-blank-lines-take-clicks ()
  "No empty line has a newline of `line-height' t: Emacs draws such a line a
line high but takes it as none where the mouse is, so that a click on the
welcome page's Add Account… button, under four of them, missed it."
  (with-temp-buffer
    (lispm-mail--draw-welcome)
    (lispm-mail--empty "Nothing here.")
    (lispm-mail-view--header (list :from '("Ada" . "ada@example.org") :subject "Hi" :date 1793095200))
    (should (string-search "Add Account" (buffer-string)))
    (should (string-search "Subject" (buffer-string)))
    (should-not (lispm-mail-test--weightless-blank-lines))))

(provide 'lispm-mail-test)
;;; lispm-mail-test.el ends here
