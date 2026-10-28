;;; lispm-mail-worker.el --- LispM Mail's Emacs in the background  -*- lexical-binding: t; -*-

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

;; LispM Mail speaks to mail servers from Emacsen of its own, one an
;; account, in batch, so that the Emacs mail is read in never waits on
;; the network: imap.el reads the account's mailboxes and smtpmail sends
;; its mail, both Emacs's own.  Such an Emacs loads this file alone,
;; which needs nothing but Emacs:
;;
;;   emacs -Q --batch -l lispm-mail-worker.el -f lispm-mail-worker-run
;;
;; It reads requests on its standard input, a line each, and answers
;; each on its standard output, on a line beginning with
;; `lispm-mail-worker-tag', in the order asked:
;;
;;   request  (ID OPERATION . PROPERTIES)
;;   answer   (ID ok VALUE) or (ID error MESSAGE)
;;
;; Both are printed with every character beyond ASCII escaped, so that
;; a line is ASCII whatever it holds.  Messages go by files: the raw
;; message fetched is written to the file the request names, the
;; message sent or appended read from one.  Everything else said, on
;; standard error, is the log.
;;
;; The operations, each taking a plist:
;;
;;   imap       :host :port :security :user :password, the account's
;;              IMAP server, connected to and logged in at once; the
;;              connection is kept, and opened again when it drops.
;;              Answers the server's capabilities.  An account signing
;;              in on its provider's page has :oauth instead of
;;              :password, (:token-url :client-id :scope :refresh-token
;;              :access-token): it logs in with an access token, by
;;              SASL XOAUTH2, the one given or one got with the refresh
;;              token, again when it expires.
;;   mailboxes  The mailboxes, each (:name :delimiter :flags :messages
;;              :unseen); :status nil leaves out the counts.
;;   sync       :mailbox, :limit messages from its end, :known UIDs whose
;;              headers are had already, :uidvalidity they were had
;;              with.  Answers (:uidvalidity :exists :flags ((UID .
;;              FLAGS) ...) :headers (MESSAGE ...)), FLAGS a list of
;;              strings, a MESSAGE as `lispm-mail-worker--message' makes
;;              it.  A UID of the window not in :flags is gone.
;;   search     :mailbox :query, by Gmail's own search on Gmail, else for
;;              every word of it, and :limit.  Answers (:uidvalidity
;;              :total :headers (MESSAGE ...)), the newest :limit found.
;;   snippets   :mailbox :uids: the first words of each, ((UID . TEXT) ...)
;;   fetch      :mailbox :uid :file: the raw message written to the file.
;;   store      :mailbox :uids :add or :remove, a flag.
;;   move       :mailbox :uids :to, another mailbox.
;;   expunge    :mailbox :uids: deleted for good.
;;   append     :mailbox :file :flags, a list of flags: the file's message
;;              added to the mailbox; with :replace, a Message-ID, the
;;              mailbox's other messages of that Message-ID deleted, as
;;              a draft saved again replaces the one saved before.
;;   forget     :mailbox :message-id: its messages of that Message-ID
;;              deleted for good, as a draft once sent.
;;   send       :smtp (:host :port :security :user :password) :file
;;              :separator, the line ending the headers, and :sent, a
;;              mailbox to append a copy to, or nil.  An :smtp signing
;;              in whose :oauth has a :send-url, as Gmail's, sends
;;              through that HTTPS endpoint of its provider's API
;;              instead; :send-check-url checks it.
;;   check      :imap and :smtp, servers as `imap' and `send' take them:
;;              each connected to and logged in to, in an Emacs of its
;;              own, the settings' test.  Answers (:imap RESULT :smtp
;;              RESULT), RESULT t or what went wrong.
;;
;; :security is `tls', `starttls' or `none'.  Nothing asks a question:
;; the questions imap.el, smtpmail or the network's security manager
;; would ask on the terminal fail the request instead.
;;
;; When the provider gives a new refresh token with an access token,
;; the background Emacs says it, unasked, for it to be kept instead:
;;
;;   note     (nil refresh-token TOKEN)

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'imap)
(require 'mail-utils)
(require 'mail-parse)
(require 'rfc2047)
(require 'qp)
(require 'smtpmail)
(require 'nsm)
(require 'time-date)
(require 'url)
(require 'url-http)
(require 'url-util)

(defconst lispm-mail-worker-tag "\e[lispm-mail] "
  "What begins each answer of a background Emacs, on a line of its own.")

(defconst lispm-mail-worker-header-fields
  '("DATE" "FROM" "TO" "CC" "REPLY-TO" "SUBJECT" "MESSAGE-ID" "IN-REPLY-TO" "REFERENCES"
    "LIST-UNSUBSCRIBE" "CONTENT-TYPE")
  "The header fields a message's headers are fetched with.")

(defvar lispm-mail-worker--imap nil
  "The account's IMAP server, as `imap' was asked for it: a plist.")

(defvar lispm-mail-worker--buffer nil
  "The buffer of the connection to the IMAP server, or nil.")

(defvar lispm-mail-worker--token nil
  "The access token got last: (TOKEN EXPIRES REFRESH-TOKEN), EXPIRES a
`float-time', REFRESH-TOKEN the one to get the next with.")

;;;; Saying

(defun lispm-mail-worker-print (form)
  "Return FORM printed on one line of ASCII, to be read back."
  (let ((print-length nil)
        (print-level nil)
        (print-circle nil)
        (print-escape-newlines t)
        (print-escape-control-characters t)
        (print-escape-nonascii t)
        (print-escape-multibyte t))
    (prin1-to-string form)))

(defun lispm-mail-worker--say (form)
  "Say FORM, an answer, on standard output."
  ;; Unlike `princ', which batch Emacs keeps until it exits, this
  ;; reaches the Emacs asking at once.
  (send-string-to-terminal (concat "\n" lispm-mail-worker-tag (lispm-mail-worker-print form) "\n")))

(defun lispm-mail-worker--refuse (&rest _)
  "Fail a question asked: nobody is there to answer it."
  (error "The server asked a question nobody can answer here"))

(defmacro lispm-mail-worker--without-questions (&rest body)
  "Run BODY, any question asked on the terminal failing instead."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'read-from-minibuffer) #'lispm-mail-worker--refuse)
             ((symbol-function 'read-string) #'lispm-mail-worker--refuse)
             ((symbol-function 'read-passwd) #'lispm-mail-worker--refuse)
             ((symbol-function 'y-or-n-p) #'lispm-mail-worker--refuse)
             ((symbol-function 'yes-or-no-p) #'lispm-mail-worker--refuse)
             ((symbol-function 'read-multiple-choice) #'lispm-mail-worker--refuse))
     ,@body))

;;;; Message sets

(defun lispm-mail-worker-set (uids)
  "Return UIDS, numbers, as an IMAP message set, runs as ranges: \"1:4,9\"."
  (let ((uids (sort (seq-uniq uids) #'<))
        runs)
    (while uids
      (let ((start (pop uids))
            (end nil))
        (setq end start)
        (while (and uids (= (car uids) (1+ end)))
          (setq end (pop uids)))
        (push (if (= start end) (number-to-string start) (format "%d:%d" start end)) runs)))
    (string-join (nreverse runs) ",")))

;;;; Signing in with a token

(defun lispm-mail-worker--token-error (answer)
  "Return what ANSWER, a token endpoint's refusal, says, in words."
  (let ((code (alist-get 'error answer))
        (text (alist-get 'error_description answer)))
    (cond
     ((member code '("invalid_grant" "interaction_required" "consent_required"))
      "The account is no longer signed in: sign in again in its settings")
     ((stringp text)
      ;; Microsoft follows what it says with its trace and correlation IDs.
      (string-trim (car (split-string text "\\(\r?\n\\| Trace ID:\\)"))))
     (t (format "The sign-in failed: %s" code)))))

(defun lispm-mail-worker-token-answer (buffer)
  "Return what BUFFER, a token endpoint's HTTP response, gives:
\(:access-token :refresh-token :expires-in).  Signal an error saying
why when it refuses."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((answer (and (re-search-forward "\r?\n\r?\n" nil t)
                       (ignore-errors
                         (json-parse-string
                          (decode-coding-string (buffer-substring-no-properties (point) (point-max))
                                                'utf-8)
                          :object-type 'alist :null-object nil)))))
      (cond
       ((stringp (alist-get 'access_token answer))
        (list :access-token (alist-get 'access_token answer)
              :refresh-token (alist-get 'refresh_token answer)
              :expires-in (let ((expires (alist-get 'expires_in answer)))
                            (if (stringp expires) (string-to-number expires) (or expires 3600)))))
       ((alist-get 'error answer)
        (error "%s" (lispm-mail-worker--token-error answer)))
       (t (error "The sign-in server did not answer"))))))

(defun lispm-mail-worker-token-request (fields)
  "Return FIELDS, ((NAME VALUE) ...), as a token request's body,
without those whose VALUE is nil."
  (url-build-query-string (seq-filter #'cadr fields)))

(defun lispm-mail-worker-access-token (oauth)
  "Return an access token for OAUTH, a server's :oauth: the one it gives,
or one got with its refresh token, kept until it expires."
  (or (plist-get oauth :access-token)
      (pcase-let ((`(,token ,expires) lispm-mail-worker--token))
        (and token (< (float-time) (- expires 60)) token))
      (let* ((refresh (or (nth 2 lispm-mail-worker--token) (plist-get oauth :refresh-token)))
             (url-request-method "POST")
             (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
             (url-request-data (lispm-mail-worker-token-request
                                `(("client_id" ,(plist-get oauth :client-id))
                                  ("client_secret" ,(plist-get oauth :client-secret))
                                  ("grant_type" "refresh_token")
                                  ("refresh_token" ,refresh)
                                  ("scope" ,(plist-get oauth :scope)))))
             (buffer (progn
                       (unless (and refresh (plist-get oauth :client-id))
                         (error "The account is not signed in: sign in in its settings"))
                       (url-retrieve-synchronously (plist-get oauth :token-url) t t 60))))
        (unless buffer
          (error "The sign-in server did not answer"))
        (let ((answer (unwind-protect (lispm-mail-worker-token-answer buffer)
                        (kill-buffer buffer))))
          (let ((newer (or (plist-get answer :refresh-token) refresh)))
            (setq lispm-mail-worker--token (list (plist-get answer :access-token)
                                                 (+ (float-time) (plist-get answer :expires-in))
                                                 newer))
            (unless (equal newer refresh)
              (lispm-mail-worker--say (list nil 'refresh-token newer))))
          (plist-get answer :access-token)))))

;;;; The IMAP server

(defun lispm-mail-worker--stream (security)
  "Return imap.el's stream for SECURITY."
  (pcase security
    ('tls 'tls)
    ('starttls 'starttls)
    (_ 'network)))

(defun lispm-mail-worker--log-in (server buffer)
  "Open the connection to SERVER, a plist as `imap' takes it, in BUFFER,
and log in; return BUFFER.  Signal an error saying why when it fails."
  (let ((host (plist-get server :host))
        (port (plist-get server :port))
        (oauth (plist-get server :oauth))
        (imap-store-password t))
    (unless (imap-open host port (lispm-mail-worker--stream (plist-get server :security)) nil buffer)
      (error "Cannot reach %s at port %s" host port))
    (if oauth
        (let ((said (lispm-mail-worker--xoauth2 (plist-get server :user)
                                                (lispm-mail-worker-access-token oauth)
                                                buffer)))
          (unless (eq said t)
            (imap-close buffer)
            ;; Got again next time, in case it was taken back.
            (setq lispm-mail-worker--token nil)
            (error "The server refused the sign-in%s" (if said (concat ": " said) ""))))
      (with-current-buffer buffer
        (setq imap-auth (if (and (imap-capability 'LOGINDISABLED) (imap-capability 'AUTH=PLAIN))
                            'plain
                          'login)))
      (unless (condition-case nil
                  (imap-authenticate (plist-get server :user) (plist-get server :password) buffer)
                ;; imap.el asks for the password again when the server refuses it.
                (error nil))
        (let ((said (with-current-buffer buffer (and (boundp 'imap-error) imap-error))))
          (imap-close buffer)
          (error "The server refused the user name or password%s"
                 (if (stringp said) (concat ": " said) "")))))
    buffer))

(defun lispm-mail-worker-xoauth2 (user token)
  "Return what SASL XOAUTH2 says to log USER in with TOKEN, base64-encoded."
  (base64-encode-string
   (encode-coding-string (concat "user=" user "\1auth=Bearer " token "\1\1") 'utf-8)
   t))

(defun lispm-mail-worker--xoauth2 (user token buffer)
  "Log in as USER with TOKEN, an access token, on BUFFER's connection.
Return t, or what the server said refusing it, or nil."
  (let (said)
    (with-current-buffer buffer
      (if (imap-ok-p (imap-send-command-wait
                      (list (concat "AUTHENTICATE XOAUTH2 " (lispm-mail-worker-xoauth2 user token))
                            ;; Refusing, the server says why in a
                            ;; challenge, to be answered empty.
                            (lambda (challenge)
                              (setq said (ignore-errors (base64-decode-string challenge)))
                              ""))
                      buffer))
          (progn (setq imap-state 'auth) t)
        (or (imap-error-text buffer) said)))))

(defun lispm-mail-worker--connection ()
  "Return the buffer of the connection to the IMAP server, open and logged in."
  (unless lispm-mail-worker--imap
    (error "No IMAP server was given"))
  (let ((buffer lispm-mail-worker--buffer))
    (unless (and (buffer-live-p buffer) (imap-opened buffer))
      (setq buffer (get-buffer-create " *lispm-mail imap*"))
      (lispm-mail-worker--log-in lispm-mail-worker--imap buffer)
      (setq lispm-mail-worker--buffer buffer))
    buffer))

(defun lispm-mail-worker--select (mailbox)
  "Select MAILBOX afresh, for what it holds now; return the connection's buffer."
  (let ((buffer (lispm-mail-worker--connection)))
    (with-current-buffer buffer
      (setq imap-current-mailbox nil))
    (unless (imap-mailbox-select mailbox nil buffer)
      (error "Cannot open the mailbox %s" mailbox))
    buffer))

(defun lispm-mail-worker--forget-messages (buffer)
  "Forget what BUFFER's connection has fetched of messages so far."
  (with-current-buffer buffer
    (setq imap-message-data (obarray-make 2))))

(defun lispm-mail-worker--ok (buffer command &rest arguments)
  "Send COMMAND, formatted with ARGUMENTS, on BUFFER's connection and wait.
Signal an error when the server says it failed."
  (let ((response (imap-send-command-wait (apply #'format command arguments) buffer)))
    (unless (imap-ok-p response)
      (error "The server refused %s: %s" (car (split-string command))
             (or (imap-error-text buffer) "no reason given")))
    response))

;;;; Messages

(defun lispm-mail-worker--decode (bytes)
  "Return BYTES, a header's raw text, as text."
  (if (multibyte-string-p bytes)
      bytes
    (decode-coding-string bytes 'utf-8)))

(defun lispm-mail-worker--addresses (text)
  "Return the addresses of TEXT, a header's value: a list of (NAME . ADDRESS)."
  (when text
    (delq nil
          (mapcar (lambda (entry)
                    (let ((address (car entry))
                          (name (cdr entry)))
                      (when (or address name)
                        (cons (and name (string-trim (rfc2047-decode-string name)))
                              (and address (string-trim address))))))
                  (ignore-errors (mail-header-parse-addresses text))))))

(defun lispm-mail-worker-date (text)
  "Return the time TEXT, a date, says, in seconds since the epoch, or nil.
TEXT is as a Date field writes it, or as IMAP's INTERNALDATE does,
\"01-Jan-2010 01:01:00 +0000\"."
  (when (and text (not (string-blank-p text)))
    (let ((text (replace-regexp-in-string
                 "\\`[ \t]*\\([0-9]+\\)-\\([A-Za-z]+\\)-\\([0-9]+\\)" "\\1 \\2 \\3" text)))
      (ignore-errors (time-convert (date-to-time text) 'integer)))))

(defun lispm-mail-worker--attachment-p (body)
  "Return non-nil when BODY, a body structure as imap.el parses it, holds
an attachment."
  (let ((found nil))
    (cl-labels ((walk (part top)
                  (cond
                   (found)
                   ((stringp part)
                    (when (string-equal-ignore-case part "attachment")
                      (setq found t)))
                   ((consp part)
                    (if (stringp (car part))
                        ;; A part of its own: not text, it is attached.
                        (if (and (not top)
                                 (member (downcase (car part)) '("application" "image" "audio" "video"))
                                 (not (member-ignore-case "inline" (flatten-tree (nthcdr 9 part)))))
                            (setq found t)
                          (dolist (item (cdr part)) (walk item nil)))
                      (dolist (item part) (walk item nil)))))))
      (walk body t))
    found))

(defun lispm-mail-worker--message (uid buffer)
  "Return what BUFFER's connection fetched of the message UID: a plist.
:uid, :flags, :date (seconds), :size, :from (NAME . ADDRESS), :to, :cc
and :reply-to (lists of them), :subject, :message-id, :in-reply-to,
:references, :list-unsubscribe and :attachment."
  (let* ((detail (imap-message-get uid 'BODYDETAIL buffer))
         (header (nth 2 (car detail))))
    (with-temp-buffer
      (insert (string-replace "\r\n" "\n" (lispm-mail-worker--decode (or header ""))))
      (goto-char (point-min))
      (cl-flet ((field (name)
                  (when-let* ((value (mail-fetch-field name)))
                    (string-trim (rfc2047-decode-string value)))))
        (list :uid uid
              :flags (remove "\\Recent" (imap-message-get uid 'FLAGS buffer))
              :date (or (lispm-mail-worker-date (mail-fetch-field "date"))
                        (lispm-mail-worker-date (imap-message-get uid 'INTERNALDATE buffer)))
              :size (imap-message-get uid 'RFC822.SIZE buffer)
              :from (car (lispm-mail-worker--addresses (mail-fetch-field "from")))
              :to (lispm-mail-worker--addresses (mail-fetch-field "to" nil t))
              :cc (lispm-mail-worker--addresses (mail-fetch-field "cc" nil t))
              :reply-to (lispm-mail-worker--addresses (mail-fetch-field "reply-to"))
              :subject (or (field "subject") "")
              :message-id (field "message-id")
              :in-reply-to (field "in-reply-to")
              :references (field "references")
              :list-unsubscribe (field "list-unsubscribe")
              :attachment (or (lispm-mail-worker--attachment-p (imap-message-get uid 'BODYSTRUCTURE buffer))
                              ;; Where the structure could not be read.
                              (and (string-match-p "\\`multipart/mixed"
                                                   (downcase (or (mail-fetch-field "content-type") "")))
                                   (null (imap-message-get uid 'BODYSTRUCTURE buffer)))))))))

(defun lispm-mail-worker--uids (buffer)
  "Return the UIDs of the messages BUFFER's connection has fetched."
  (let (uids)
    (imap-message-map (lambda (uid _) (push uid uids)) 'UID buffer)
    (sort uids #'<)))

;;;; First words

(defun lispm-mail-worker--param (params name)
  "Return the value of NAME in PARAMS, a body structure's (NAME VALUE ...)."
  (cadr (member-ignore-case name params)))

(defun lispm-mail-worker--text-part (body &optional subtype)
  "Return the first part of BODY, a body structure, that is text of SUBTYPE,
plain unless said: (SECTION ENCODING CHARSET SUBTYPE), or nil."
  (let ((subtype (or subtype "plain")))
    (cl-labels ((find (part section)
                  (cond
                   ((not (consp part)) nil)
                   ((stringp (car part))
                    (when (and (string-equal-ignore-case (car part) "text")
                               (stringp (nth 1 part))
                               (string-equal-ignore-case (nth 1 part) subtype))
                      (list (or section "1") (nth 5 part)
                            (lispm-mail-worker--param (nth 2 part) "charset")
                            subtype)))
                   (t
                    (let ((number 0) found)
                      (dolist (child part)
                        (when (and (consp child) (not found))
                          (setq number (1+ number))
                          (setq found (find child (if section (format "%s.%d" section number)
                                                    (number-to-string number))))))
                      found)))))
      (find body nil))))

(defun lispm-mail-worker--charset (charset)
  "Return the coding system of CHARSET, a MIME charset's name, or utf-8."
  (let ((coding (and charset (intern (downcase charset)))))
    (if (and coding (coding-system-p coding)) coding 'utf-8)))

(defun lispm-mail-worker-snippet (bytes encoding charset subtype)
  "Return the first words of BYTES, the start of a text part of SUBTYPE in
ENCODING and CHARSET: one line, quotations and markup left out."
  (let ((text (pcase (and encoding (upcase encoding))
                ("BASE64"
                 (let ((clean (replace-regexp-in-string "[^A-Za-z0-9+/=]" "" bytes)))
                   (or (ignore-errors
                         (base64-decode-string (substring clean 0 (* 4 (/ (length clean) 4)))))
                       "")))
                ("QUOTED-PRINTABLE" (quoted-printable-decode-string bytes))
                (_ bytes))))
    (setq text (decode-coding-string text (lispm-mail-worker--charset charset)))
    (when (string-equal-ignore-case subtype "html")
      (setq text (replace-regexp-in-string "<\\(style\\|script\\|head\\)[^>]*>\\(.\\|\n\\)*?</\\1>" " " text))
      (setq text (replace-regexp-in-string "<[^>]*>?" " " text))
      (dolist (entity '(("&nbsp;" . " ") ("&amp;" . "&") ("&lt;" . "<") ("&gt;" . ">")
                        ("&quot;" . "\"") ("&#39;" . "'") ("&zwnj;" . "") ("&#8203;" . "")))
        (setq text (string-replace (car entity) (cdr entity) text))))
    (setq text (mapconcat #'identity
                          (seq-remove (lambda (line)
                                        (string-match-p "\\`[ \t]*\\(>\\|\\'\\)" line))
                                      (split-string text "\r?\n"))
                          " "))
    (setq text (string-trim (replace-regexp-in-string "[ \t ‌]+" " " text)))
    (truncate-string-to-width text 200)))

;;;; Operations

(defun lispm-mail-worker--connect (properties)
  "Connect to the IMAP server PROPERTIES say; return its capabilities."
  (setq lispm-mail-worker--imap properties)
  (when (buffer-live-p lispm-mail-worker--buffer)
    (ignore-errors (imap-close lispm-mail-worker--buffer)))
  (setq lispm-mail-worker--buffer nil)
  (let ((buffer (lispm-mail-worker--connection)))
    (mapcar #'symbol-name (imap-capability nil buffer))))

(defun lispm-mail-worker--mailboxes (properties)
  "Return the mailboxes, with their counts unless PROPERTIES say :status nil."
  (let* ((buffer (lispm-mail-worker--connection))
         (names (imap-mailbox-list "*" "" nil buffer))
         (status (plist-get (append properties '(:status t)) :status)))
    (mapcar (lambda (name)
              (let* ((flags (imap-mailbox-get 'list-flags name buffer))
                     (counts (and status
                                  (not (member-ignore-case "\\Noselect" flags))
                                  (not (member-ignore-case "\\NonExistent" flags))
                                  (ignore-errors
                                    (imap-mailbox-status name '(messages unseen) buffer)))))
                (list :name name
                      :delimiter (imap-mailbox-get 'delimiter name buffer)
                      :flags flags
                      :messages (nth 0 counts)
                      :unseen (nth 1 counts))))
            names)))

(defun lispm-mail-worker--sync (properties)
  "Return what a mailbox holds now, as PROPERTIES ask, see `sync'."
  (let* ((mailbox (plist-get properties :mailbox))
         (limit (or (plist-get properties :limit) 200))
         (buffer (lispm-mail-worker--select mailbox))
         (exists (or (imap-mailbox-get 'exists mailbox buffer) 0))
         (uidvalidity (imap-mailbox-get 'uidvalidity mailbox buffer))
         (known (and (equal uidvalidity (plist-get properties :uidvalidity))
                     (plist-get properties :known)))
         flags headers)
    (when (> exists 0)
      (lispm-mail-worker--forget-messages buffer)
      ;; By sequence number: the window's last LIMIT messages.
      (imap-fetch (format "%d:*" (max 1 (- exists limit -1))) "(UID FLAGS)" nil t buffer)
      (let* ((uids (lispm-mail-worker--uids buffer))
             (missing (seq-difference uids known)))
        (setq flags (mapcar (lambda (uid)
                              (cons uid (remove "\\Recent" (imap-message-get uid 'FLAGS buffer))))
                            uids))
        (when missing
          (setq headers (lispm-mail-worker--headers buffer missing)))))
    (list :uidvalidity uidvalidity :exists exists :flags flags :headers headers)))

(defun lispm-mail-worker--headers (buffer uids)
  "Return the headers of the selected mailbox's messages UIDS, plists as
`lispm-mail-worker--message' makes them, by UID, of those still there."
  (lispm-mail-worker--forget-messages buffer)
  (imap-fetch (lispm-mail-worker-set uids)
              (format "(UID FLAGS INTERNALDATE RFC822.SIZE BODYSTRUCTURE BODY.PEEK[HEADER.FIELDS (%s)])"
                      (string-join lispm-mail-worker-header-fields " "))
              nil nil buffer)
  (mapcar (lambda (uid) (lispm-mail-worker--message uid buffer))
          (lispm-mail-worker--uids buffer)))

(defun lispm-mail-worker--search-once (buffer key text)
  "Return the UIDs of the selected mailbox's messages that the search KEY,
one taking a string, finds for TEXT, on BUFFER's connection.
TEXT goes as a literal in UTF-8 when it is not plain ASCII: imap.el ends
a command after a literal, so it is the command's last argument."
  (let ((literal (unless (string-match-p "\\`[ -~]*\\'" text)
                   (generate-new-buffer " *lispm-mail search*"))))
    (unwind-protect
        (with-current-buffer buffer
          (when literal
            (with-current-buffer literal
              (set-buffer-multibyte nil)
              (insert (encode-coding-string text 'utf-8))))
          (imap-mailbox-put 'search 'dummy)
          (unless (imap-ok-p (imap-send-command-wait
                              (if literal
                                  (list (format "UID SEARCH CHARSET UTF-8 %s " key) literal)
                                (format "UID SEARCH %s %s" key (lispm-mail-worker--string text)))))
            (error "The server refused the search: %s" (or (imap-error-text buffer) "no reason given")))
          (let ((found (imap-mailbox-get-1 'search imap-current-mailbox)))
            (and (listp found) found)))
      (when literal (kill-buffer literal)))))

(defun lispm-mail-worker--search (properties)
  "Return what the :mailbox of PROPERTIES holds that their :query finds:
(:uidvalidity :total :headers), HEADERS those of the newest :limit
messages found, 20 by default, the newest first.  Gmail searches with
its own search, as its web page does; other servers find the messages
holding every word of the query, in their headers or text."
  (let* ((query (string-trim (or (plist-get properties :query) "")))
         (mailbox (plist-get properties :mailbox))
         (buffer (progn (when (string-empty-p query)
                          (error "Nothing to search for"))
                        (lispm-mail-worker--select mailbox)))
         (uids (if (imap-capability 'X-GM-EXT-1 buffer)
                   (lispm-mail-worker--search-once buffer "X-GM-RAW" query)
                 ;; One search a word, as one literal ends a command.
                 (let* ((words (split-string query nil t))
                        (found (lispm-mail-worker--search-once buffer "TEXT" (car words))))
                   (dolist (word (cdr words))
                     (when found
                       (setq found (seq-intersection
                                    found (lispm-mail-worker--search-once buffer "TEXT" word)))))
                   found)))
         (newest (seq-take (sort (copy-sequence uids) #'>) (or (plist-get properties :limit) 20))))
    (list :uidvalidity (imap-mailbox-get 'uidvalidity mailbox buffer)
          :total (length uids)
          :headers (and newest
                        (sort (lispm-mail-worker--headers buffer newest)
                              (lambda (a b) (> (plist-get a :uid) (plist-get b :uid))))))))

(defun lispm-mail-worker--snippets (properties)
  "Return the first words of the messages PROPERTIES name: ((UID . TEXT) ...)."
  (let* ((buffer (lispm-mail-worker--select (plist-get properties :mailbox)))
         (uids (plist-get properties :uids))
         parts snippets)
    (when uids
      (lispm-mail-worker--forget-messages buffer)
      (imap-fetch (lispm-mail-worker-set uids) "(UID BODYSTRUCTURE)" nil nil buffer)
      (dolist (uid (lispm-mail-worker--uids buffer))
        (let ((body (imap-message-get uid 'BODYSTRUCTURE buffer)))
          (when-let* ((part (or (lispm-mail-worker--text-part body)
                                (lispm-mail-worker--text-part body "html"))))
            (push (cons uid part) parts))))
      (dolist (entry (nreverse parts))
        (pcase-let ((`(,uid ,section ,encoding ,charset ,subtype) entry))
          (lispm-mail-worker--forget-messages buffer)
          (when (imap-fetch uid (format "(UID BODY.PEEK[%s]<0.%d>)" section
                                        (if (string-equal-ignore-case subtype "html") 6000 1500))
                            nil nil buffer)
            (when-let* ((bytes (nth 2 (car (imap-message-get uid 'BODYDETAIL buffer)))))
              (push (cons uid (lispm-mail-worker-snippet bytes encoding charset subtype)) snippets))))))
    (nreverse snippets)))

(defun lispm-mail-worker--fetch (properties)
  "Write the message PROPERTIES name to their :file; return its size."
  (let* ((buffer (lispm-mail-worker--select (plist-get properties :mailbox)))
         (uid (plist-get properties :uid)))
    (lispm-mail-worker--forget-messages buffer)
    (imap-fetch uid "(UID BODY.PEEK[])" nil nil buffer)
    (let ((bytes (nth 2 (car (imap-message-get uid 'BODYDETAIL buffer)))))
      (unless bytes
        (error "The message is no longer there"))
      (let ((coding-system-for-write 'binary)
            (file (plist-get properties :file)))
        (make-directory (file-name-directory file) t)
        (with-temp-file file
          (set-buffer-multibyte nil)
          (insert bytes)
          ;; Kept with Unix's line ends, as Emacs's mail libraries read them.
          (goto-char (point-min))
          (while (search-forward "\r\n" nil t)
            (replace-match "\n" t t))))
      (length bytes))))

(defun lispm-mail-worker--store (properties)
  "Add or remove a flag of the messages PROPERTIES name."
  (let ((buffer (lispm-mail-worker--select (plist-get properties :mailbox)))
        (uids (lispm-mail-worker-set (plist-get properties :uids))))
    (if-let* ((flag (plist-get properties :add)))
        (lispm-mail-worker--ok buffer "UID STORE %s +FLAGS.SILENT (%s)" uids flag)
      (lispm-mail-worker--ok buffer "UID STORE %s -FLAGS.SILENT (%s)" uids (plist-get properties :remove)))
    t))

(defun lispm-mail-worker--expunge-uids (buffer uids)
  "Delete the messages UIDS, a message set, marked deleted, on BUFFER's
connection: only them, when the server can, else all marked."
  (if (imap-capability 'UIDPLUS buffer)
      (lispm-mail-worker--ok buffer "UID EXPUNGE %s" uids)
    (lispm-mail-worker--ok buffer "EXPUNGE")))

(defun lispm-mail-worker--quote (mailbox)
  "Return MAILBOX quoted as IMAP takes a mailbox's name."
  (lispm-mail-worker--string (imap-utf7-encode mailbox)))

(defun lispm-mail-worker--string (text)
  "Return TEXT as an IMAP quoted string."
  (concat "\"" (replace-regexp-in-string "[\"\\]" "\\\\\\&" text) "\""))

(defun lispm-mail-worker--move (properties)
  "Move the messages PROPERTIES name to the mailbox :to."
  (let ((buffer (lispm-mail-worker--select (plist-get properties :mailbox)))
        (uids (lispm-mail-worker-set (plist-get properties :uids)))
        (to (lispm-mail-worker--quote (plist-get properties :to))))
    (if (imap-capability 'MOVE buffer)
        (lispm-mail-worker--ok buffer "UID MOVE %s %s" uids to)
      (lispm-mail-worker--ok buffer "UID COPY %s %s" uids to)
      (lispm-mail-worker--ok buffer "UID STORE %s +FLAGS.SILENT (\\Deleted)" uids)
      (lispm-mail-worker--expunge-uids buffer uids))
    t))

(defun lispm-mail-worker--expunge (properties)
  "Delete the messages PROPERTIES name, for good."
  (let ((buffer (lispm-mail-worker--select (plist-get properties :mailbox)))
        (uids (lispm-mail-worker-set (plist-get properties :uids))))
    (lispm-mail-worker--ok buffer "UID STORE %s +FLAGS.SILENT (\\Deleted)" uids)
    (lispm-mail-worker--expunge-uids buffer uids)
    t))

(defun lispm-mail-worker--append (properties)
  "Add the message of PROPERTIES' :file to their :mailbox, with :flags."
  (let ((buffer (lispm-mail-worker--connection))
        (message (generate-new-buffer " *lispm-mail append*")))
    (unwind-protect
        (progn
          (with-current-buffer message
            (set-buffer-multibyte nil)
            (let ((coding-system-for-read 'binary))
              (insert-file-contents (plist-get properties :file)))
            ;; IMAP wants lines ending in CRLF.
            (goto-char (point-min))
            (while (search-forward "\n" nil t)
              (unless (eq (char-before (1- (point))) ?\r)
                (replace-match "\r\n" t t))))
          (let ((response (imap-send-command-wait
                           (list "APPEND " (lispm-mail-worker--quote (plist-get properties :mailbox))
                                 (format " (%s) " (string-join (plist-get properties :flags) " "))
                                 message)
                           buffer)))
            (unless (imap-ok-p response)
              (error "The server refused to keep the message in %s: %s"
                     (plist-get properties :mailbox) (or (imap-error-text buffer) "no reason given")))))
      (kill-buffer message))
    (when-let* ((id (plist-get properties :replace)))
      ;; The messages of the same Message-ID but the one just added,
      ;; whose UID is the largest, were the earlier drafts.
      (let ((older (cdr (sort (lispm-mail-worker--with-message-id (plist-get properties :mailbox) id) #'>))))
        (when older
          (lispm-mail-worker--expunge (list :mailbox (plist-get properties :mailbox) :uids older)))))
    t))

(defun lispm-mail-worker--with-message-id (mailbox id)
  "Return the UIDs of MAILBOX's messages whose Message-ID is ID."
  (let ((buffer (lispm-mail-worker--select mailbox)))
    (imap-search (format "HEADER Message-ID %s" (lispm-mail-worker--string id)) buffer)))

(defun lispm-mail-worker--forget (properties)
  "Delete the messages of PROPERTIES' :mailbox whose Message-ID is their
:message-id, for good: a draft once sent."
  (let ((uids (lispm-mail-worker--with-message-id (plist-get properties :mailbox)
                                                  (plist-get properties :message-id))))
    (when uids
      (lispm-mail-worker--expunge (list :mailbox (plist-get properties :mailbox) :uids uids)))
    (length uids)))

(defun lispm-mail-worker--smtp-type (security)
  "Return smtpmail's stream type for SECURITY."
  (pcase security
    ('tls 'ssl)
    ('starttls 'starttls)
    (_ 'plain)))

(defun lispm-mail-worker--send (properties)
  "Send the message of PROPERTIES' :file through their :smtp server, or
its provider's API, when its :oauth names where."
  (let* ((smtp (plist-get properties :smtp))
         (oauth (plist-get smtp :oauth))
         (file (plist-get properties :file)))
    (if (plist-get oauth :send-url)
        (lispm-mail-worker--send-https oauth file (plist-get properties :separator))
      (lispm-mail-worker--send-smtp smtp file (plist-get properties :separator)))
    (when-let* ((sent (plist-get properties :sent)))
      (let ((copy (make-temp-file "lispm-mail-sent" nil ".eml")))
        (unwind-protect
            (progn
              (with-temp-file copy
                (set-buffer-multibyte nil)
                (let ((coding-system-for-read 'binary))
                  (insert-file-contents file))
                (goto-char (point-min))
                (when (re-search-forward (concat "^" (regexp-quote (plist-get properties :separator)) "\n")
                                         nil t)
                  (replace-match "\n" t t)))
              (lispm-mail-worker--append (list :mailbox sent :file copy :flags '("\\Seen"))))
          (delete-file copy))))
    t))

(defun lispm-mail-worker--send-smtp (smtp file separator)
  "Send the message of FILE, its headers ended by SEPARATOR, through SMTP."
  (let* ((user (plist-get smtp :user))
         (oauth (plist-get smtp :oauth))
         ;; Signing in, the access token is given as the password.
         (password (if oauth (lispm-mail-worker-access-token oauth) (plist-get smtp :password))))
    (with-temp-buffer
      (let ((coding-system-for-read 'raw-text))
        (insert-file-contents file))
      (let ((mail-header-separator separator)
            (smtpmail-smtp-server (plist-get smtp :host))
            (smtpmail-smtp-service (plist-get smtp :port))
            (smtpmail-stream-type (lispm-mail-worker--smtp-type (plist-get smtp :security)))
            (smtpmail-smtp-user user)
            (smtpmail-queue-mail nil)
            (smtpmail-debug-info nil)
            (smtpmail-warn-about-unknown-extensions nil)
            (smtpmail-auth-supported (if oauth '(xoauth2) smtpmail-auth-supported)))
        ;; smtpmail asks auth-source for the password, which is given
        ;; here; it logs in when the server offers to.
        (cl-letf (((symbol-function 'auth-source-search)
                   (lambda (&rest _)
                     (and user password
                          (list (list :host smtpmail-smtp-server :port smtpmail-smtp-service
                                      :user user :secret password))))))
          (smtpmail-send-it))))))

(defun lispm-mail-worker-message-bytes (file separator)
  "Return the message of FILE, its headers ended by SEPARATOR, as it goes
to a provider's API: the separator a blank line, each line ended in
CRLF.  Its Bcc stays: the provider sends to it, and takes it out."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'binary))
      (insert-file-contents file))
    (goto-char (point-min))
    (when (re-search-forward (concat "^" (regexp-quote separator) "\n") nil t)
      (replace-match "\n" t t))
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (unless (eq (char-before (1- (point))) ?\r)
        (replace-match "\r\n" t t)))
    (buffer-string)))

(defun lispm-mail-worker-http-answer (buffer)
  "Return what BUFFER, an HTTP response, says: (STATUS . ANSWER), STATUS
its code, ANSWER its body read as JSON, an alist, or nil."
  (with-current-buffer buffer
    (goto-char (point-min))
    (cons (and (looking-at "HTTP/[0-9.]+ \\([0-9]+\\)") (string-to-number (match-string 1)))
          (and (re-search-forward "\r?\n\r?\n" nil t)
               (ignore-errors
                 (json-parse-string
                  (decode-coding-string (buffer-substring-no-properties (point) (point-max)) 'utf-8)
                  :object-type 'alist :null-object nil))))))

(defun lispm-mail-worker--api (oauth url &optional message)
  "Ask the provider's API at URL, signed in with OAUTH: POST MESSAGE, a
message's bytes, or GET without.  Return its ANSWER, read as JSON, an
alist.  An access token refused is got anew, once: it was taken back
before it expired.  Signal an error saying why when the API refuses."
  (let ((again t)
        answer)
    (while again
      (let* ((url-request-method (if message "POST" "GET"))
             (url-request-extra-headers
              `(("Authorization" . ,(concat "Bearer " (lispm-mail-worker-access-token oauth)))
                ,@(and message '(("Content-Type" . "message/rfc822")))))
             (url-request-data message)
             (buffer (url-retrieve-synchronously url t t 300))
             (said (if buffer
                       (unwind-protect (lispm-mail-worker-http-answer buffer)
                         (kill-buffer buffer))
                     (error "%s did not answer" (url-host (url-generic-parse-url url))))))
        (cond ((and (car said) (<= 200 (car said) 299))
               (setq answer (cdr said) again nil))
              ((and (eql (car said) 401) (eq again t))
               ;; Got anew, once: the one given, or kept, no longer.
               (setq oauth (plist-put (copy-sequence oauth) :access-token nil)
                     lispm-mail-worker--token (and lispm-mail-worker--token
                                                   (list nil 0 (nth 2 lispm-mail-worker--token)))
                     again 'once))
              (t
               (error "%s" (lispm-mail-worker--api-error said))))))
    answer))

(defun lispm-mail-worker--api-error (said)
  "Return what SAID, (STATUS . ANSWER) of a provider's API refusing, says."
  (pcase-let* ((`(,status . ,answer) said)
               (text (alist-get 'message (alist-get 'error answer))))
    (cond ((eql status 401) "The account is no longer signed in: sign in again in its settings")
          ((stringp text) (format "The server refused: %s" text))
          (status (format "The server refused (HTTP %d)" status))
          (t "The server's answer could not be read"))))

(defun lispm-mail-worker--send-https (oauth file separator)
  "Send the message of FILE, its headers ended by SEPARATOR, through the
API of OAUTH's provider, at its :send-url; return the message's ID there."
  (or (alist-get 'id (lispm-mail-worker--api oauth (plist-get oauth :send-url)
                                             (lispm-mail-worker-message-bytes file separator)))
      t))

(defun lispm-mail-worker-check-https (server)
  "Ask the API SERVER's :oauth sends through whether it would, signed in.
Return t, or signal an error saying what went wrong."
  (let ((oauth (plist-get server :oauth)))
    (unless oauth
      (error "The account is not signed in: sign in in its settings"))
    (lispm-mail-worker--api oauth (plist-get oauth :send-check-url))
    t))

;;;; Checking a server

(defun lispm-mail-worker--smtp-reply (process)
  "Wait for the SMTP server's reply on PROCESS, its lines up to the last:
\(CODE . TEXT)."
  (with-current-buffer (process-buffer process)
    (let ((deadline (+ (float-time) 30)))
      (while (and (not (save-excursion
                         (goto-char (point-min))
                         (re-search-forward "^[0-9][0-9][0-9] .*\r?\n" nil t)))
                  (process-live-p process)
                  (< (float-time) deadline))
        (accept-process-output process 1))
      (goto-char (point-min))
      (if (re-search-forward "^\\([0-9][0-9][0-9]\\) .*?\r?\n" nil t)
          (prog1 (cons (string-to-number (match-string 1))
                       (string-trim (buffer-substring (point-min) (point))))
            (delete-region (point-min) (point)))
        (error "The server did not answer")))))

(defun lispm-mail-worker--smtp-say (process text)
  "Send TEXT, an SMTP command, on PROCESS and return the reply."
  (with-current-buffer (process-buffer process) (erase-buffer))
  (process-send-string process (concat text "\r\n"))
  (lispm-mail-worker--smtp-reply process))

(defconst lispm-mail-worker-smtp-greeting-timeout 30
  "Seconds an SMTP server checked has to greet and say what it can do.")

(defun lispm-mail-worker-check-smtp (server)
  "Connect to SERVER, an SMTP server as `send' takes it, and log in.
Return t, or signal an error saying what went wrong."
  (let ((buffer (generate-new-buffer " *lispm-mail smtp*"))
        (security (plist-get server :security))
        (process nil))
    (unwind-protect
        (let* ((result (with-timeout (lispm-mail-worker-smtp-greeting-timeout
                                      (error (concat "%s did not answer at port %s: a firewall"
                                                     " or proxy may be stopping mail from being sent")
                                             (plist-get server :host) (plist-get server :port)))
                         (open-network-stream
                          "lispm-mail-smtp" buffer (plist-get server :host) (plist-get server :port)
                          :type (pcase security ('tls 'tls) ('starttls 'starttls) (_ 'plain))
                          :return-list t
                          :end-of-command "^[0-9][0-9][0-9] .*\r?\n"
                          :success "^2"
                          :capability-command "EHLO lispm\r\n"
                          :starttls-function (lambda (capabilities)
                                               (and (string-match-p "STARTTLS" capabilities)
                                                    "STARTTLS\r\n")))))
               ;; Over TLS, what the server can do was asked as it was
               ;; connected to; in plain text, it is asked now.
               (capabilities (plist-get (cdr result) :capabilities)))
          (setq process (car result))
          (unless (and process (process-live-p process))
            (error "%s" (or (plist-get (cdr result) :error)
                            (format "Cannot reach %s at port %s"
                                    (plist-get server :host) (plist-get server :port)))))
          (unless capabilities
            (setq capabilities (cdr (lispm-mail-worker--smtp-say process "EHLO lispm"))))
          (when-let* ((user (plist-get server :user))
                      ((not (string-empty-p user)))
                      ((or (plist-get server :oauth) (plist-get server :password))))
            (let* ((password (plist-get server :password))
                   (reply
                    (cond
                     ((plist-get server :oauth)
                      (unless (string-match-p "AUTH.*XOAUTH2" capabilities)
                        (error "The server offers no sign-in with a token"))
                      (let ((reply (lispm-mail-worker--smtp-say
                                    process (concat "AUTH XOAUTH2 "
                                                    (lispm-mail-worker-xoauth2
                                                     user (lispm-mail-worker-access-token
                                                           (plist-get server :oauth)))))))
                        ;; Refusing, the server says why in a challenge,
                        ;; to be answered empty.
                        (if (= (car reply) 334)
                            (lispm-mail-worker--smtp-say process "")
                          reply)))
                     ((string-match-p "AUTH.*PLAIN" capabilities)
                      (lispm-mail-worker--smtp-say
                       process (concat "AUTH PLAIN "
                                       (base64-encode-string
                                        (encode-coding-string (concat "\0" user "\0" password)
                                                              'utf-8)
                                        t))))
                     ((string-match-p "AUTH.*LOGIN" capabilities)
                      (lispm-mail-worker--smtp-say process "AUTH LOGIN")
                      (lispm-mail-worker--smtp-say
                       process (base64-encode-string (encode-coding-string user 'utf-8) t))
                      (lispm-mail-worker--smtp-say
                       process (base64-encode-string (encode-coding-string password 'utf-8) t)))
                     (t (error "The server offers no way to log in that LispM Mail knows")))))
              (unless (= (car reply) 235)
                (error (if (plist-get server :oauth)
                           "The server refused the sign-in: %s"
                         "The server refused the user name or password: %s")
                       (cdr reply)))))
          (ignore-errors (lispm-mail-worker--smtp-say process "QUIT"))
          t)
      ;; The connection's, even while it was being opened.
      (when-let* ((live (get-buffer-process buffer)))
        (delete-process live))
      (kill-buffer buffer))))

(defun lispm-mail-worker--check (properties)
  "Connect to the servers PROPERTIES give and log in; say how each went."
  (list :imap (when-let* ((imap (plist-get properties :imap)))
                (condition-case err
                    (let ((buffer (generate-new-buffer " *lispm-mail check*")))
                      (unwind-protect
                          (progn (lispm-mail-worker--log-in imap buffer)
                                 (ignore-errors (imap-logout buffer))
                                 t)
                        (ignore-errors (imap-close buffer))
                        (kill-buffer buffer)))
                  (error (lispm-mail-worker-reason err))))
        :smtp (when-let* ((smtp (plist-get properties :smtp)))
                (condition-case err
                    (if (plist-get (plist-get smtp :oauth) :send-url)
                        (lispm-mail-worker-check-https smtp)
                      (lispm-mail-worker-check-smtp smtp))
                  (error (lispm-mail-worker-reason err))))))

(defun lispm-mail-worker--parse-body (parse)
  "Call PARSE, imap.el's parser of a body structure; when it fails on one
it cannot read, skip it, and return nil."
  (let ((start (point)))
    (condition-case err
        (funcall parse)
      (error
       (message "lispm-mail: skipped a body structure: %s" (error-message-string err))
       (goto-char start)
       (unless (ignore-errors (forward-sexp) t)
         (goto-char (point-max)))
       nil))))

(defun lispm-mail-worker--parse-response (parse)
  "Call PARSE, imap.el's parser of a response; when it fails, go on with the
next, which would otherwise wait in its buffer for more of the server."
  (condition-case err
      (funcall parse)
    (error (message "lispm-mail: skipped a response: %s" (error-message-string err)))))

(defconst lispm-mail-worker-timeouts
  '((send . 600) (fetch . 300) (append . 300) (check . 90))
  "How many seconds an operation may take, by name; 120 for the others.")

(defconst lispm-mail-worker-operations
  '((imap . lispm-mail-worker--connect)
    (mailboxes . lispm-mail-worker--mailboxes)
    (sync . lispm-mail-worker--sync)
    (search . lispm-mail-worker--search)
    (snippets . lispm-mail-worker--snippets)
    (fetch . lispm-mail-worker--fetch)
    (store . lispm-mail-worker--store)
    (move . lispm-mail-worker--move)
    (expunge . lispm-mail-worker--expunge)
    (append . lispm-mail-worker--append)
    (forget . lispm-mail-worker--forget)
    (send . lispm-mail-worker--send)
    (check . lispm-mail-worker--check))
  "The operations of a background Emacs: (NAME . FUNCTION of the request's plist).")

(defun lispm-mail-worker-reason (err)
  "Return what went wrong, ERR an error, in words for whoever reads mail."
  (let ((text (error-message-string err)))
    (if (string-match "\\`make client process failed: \\([^,]*\\).*:host, \\([^,]*\\), :service, \\([^,]*\\)"
                      text)
        (format "Cannot reach %s at port %s: %s" (match-string 2 text) (match-string 3 text)
                (downcase (match-string 1 text)))
      text)))

(defun lispm-mail-worker-answer (request)
  "Do REQUEST, (ID OPERATION . PROPERTIES), and return its answer."
  (pcase-let ((`(,id ,operation . ,properties) request))
    (condition-case err
        (let ((function (alist-get operation lispm-mail-worker-operations)))
          (unless function
            (error "No operation %s" operation))
          (list id 'ok (lispm-mail-worker--without-questions
                         (with-timeout ((alist-get operation lispm-mail-worker-timeouts 120)
                                        (lispm-mail-worker--drop-connection)
                                        (error "The server did not answer in time"))
                           (funcall function properties)))))
      (error (list id 'error (lispm-mail-worker-reason err))))))

(defun lispm-mail-worker--drop-connection ()
  "Close the connection to the IMAP server; the next request opens it again."
  (when (buffer-live-p lispm-mail-worker--buffer)
    (ignore-errors (imap-close lispm-mail-worker--buffer))
    (kill-buffer lispm-mail-worker--buffer))
  (setq lispm-mail-worker--buffer nil))

(defun lispm-mail-worker-run ()
  "Answer the requests on standard input until it ends."
  ;; The network's security manager closes a connection it doubts
  ;; rather than asking whether to go on.
  (setq nsm-noninteractive t
        imap-log nil
        imap-debug nil)
  (advice-add 'imap-parse-body :around #'lispm-mail-worker--parse-body)
  (advice-add 'imap-parse-response :around #'lispm-mail-worker--parse-response)
  (let ((running t))
    (while running
      (let ((line (condition-case nil (read-from-minibuffer "") (error nil))))
        (if (null line)
            (setq running nil)
          (unless (string-blank-p line)
            (let ((request (condition-case nil (car (read-from-string line)) (error nil))))
              (if (consp request)
                  (lispm-mail-worker--say (lispm-mail-worker-answer request))
                (message "lispm-mail: not a request: %s" line))))))))
  (when (buffer-live-p lispm-mail-worker--buffer)
    (ignore-errors (imap-logout lispm-mail-worker--buffer)))
  (kill-emacs 0))

(provide 'lispm-mail-worker)
;;; lispm-mail-worker.el ends here
