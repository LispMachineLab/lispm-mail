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

(provide 'lispm-mail-test)
;;; lispm-mail-test.el ends here
