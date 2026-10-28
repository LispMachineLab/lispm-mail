;;; lispm-mail-sign-in.el --- Signing accounts in on their providers' pages  -*- lexical-binding: t; -*-

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

;; Some providers let mail programs in only through their own sign-in
;; page, Outlook.com and Gmail among them: the program is never given the
;; account's password, but a token, which the page gives once its
;; owner has allowed the program in.  LispM Mail opens the page in the
;; browser, where its owner is likely signed in already, as OAuth 2.0
;; asks of a program on a computer (RFC 8252): with an authorization
;; code and PKCE, the browser sent back to a port of this Emacs on the
;; computer's loopback, never reachable from elsewhere.  The code is
;; exchanged for an access token, which logs in to the account's
;; servers for an hour, and a refresh token, kept as a password would
;; be, with which the Emacs in the background gets the next.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'url-util)
(require 'browse-url)
(require 'xml)
(require 'lispm-mail-accounts)
(require 'lispm-mail-worker)

(defvar lispm-mail-sign-in-browse-function #'browse-url-default-browser
  "The function opening a provider's sign-in page, given its URL.
The computer's own browser, where its owner is signed in to the
provider likely, rather than one Emacs may have.")

(defconst lispm-mail-sign-in-timeout 600
  "Seconds the browser is waited for, after which the sign-in fails.")

(defun lispm-mail-sign-in--random ()
  "Return 32 random bytes, base64url-encoded: a PKCE verifier or a state."
  (base64url-encode-string
   (with-temp-buffer
     (set-buffer-multibyte nil)
     (unless (and (eql 0 (ignore-errors (call-process "head" "/dev/urandom" t nil "-c" "32")))
                  (= (buffer-size) 32))
       ;; A system without /dev/urandom: less random, but unguessable still.
       (erase-buffer)
       (insert (secure-hash 'sha256 (format "%S %S %S" (random t) (current-time) (emacs-pid)) nil nil t)))
     (buffer-string))
   t))

(defun lispm-mail-sign-in-challenge (verifier)
  "Return PKCE's challenge of VERIFIER, its SHA-256, base64url-encoded."
  (base64url-encode-string (secure-hash 'sha256 verifier nil nil t) t))

(defun lispm-mail-sign-in-query (request)
  "Return the query of REQUEST, an HTTP request's text, as an alist of
\(NAME VALUE), or nil when it asks for anything but the first page."
  (when (string-match "\\`GET /\\(?:\\?\\([^ #]*\\)\\)?[ #]" request)
    (url-parse-query-string (string-replace "+" " " (or (match-string 1 request) "")))))

(defun lispm-mail-sign-in-page (title text)
  "Return the page the browser comes back to, saying TITLE and TEXT."
  (encode-coding-string
   (format "<!DOCTYPE html>
<html><head><meta charset=\"utf-8\"><title>LispM Mail</title>
<style>
body { margin: 0; background: #cccccc; font: 13px Geneva, \"Lucida Grande\", Helvetica, sans-serif; }
.window { width: 400px; margin: 18vh auto; background: #eeeeee; border: 1px solid #000000;
          box-shadow: 2px 2px 0 #000000; }
.title { background: #dddddd; border-bottom: 1px solid #000000; padding: 3px; text-align: center;
         font-weight: bold; }
.body { padding: 4px 20px 12px; }
</style></head>
<body><div class=\"window\"><div class=\"title\">LispM Mail</div>
<div class=\"body\"><p><b>%s</b></p><p>%s</p></div></div></body></html>
"
           (xml-escape-string title) (xml-escape-string text))
   'utf-8))

(defun lispm-mail-sign-in--respond (client status page)
  "Answer CLIENT, a connection of the browser, with STATUS and PAGE, and close it."
  (when (process-live-p client)
    (process-send-string
     client
     (concat (format (concat "HTTP/1.1 %s\r\nContent-Type: text/html; charset=utf-8\r\n"
                             "Content-Length: %d\r\nCache-Control: no-store\r\nConnection: close\r\n\r\n")
                     status (length page))
             page))
    (delete-process client)))

(defun lispm-mail-sign-in--listen (handler)
  "Listen on a port of the loopback, for the browser coming back.
HANDLER is called with each connection and its request, once whole.
Return the servers, the IPv4 one first, the other on the same port."
  (let* ((filter (lambda (client text)
                   (set-process-query-on-exit-flag client nil)
                   (let ((request (concat (or (process-get client 'lispm-mail-request) "") text)))
                     (process-put client 'lispm-mail-request request)
                     (when (and (not (process-get client 'lispm-mail-answered))
                                (string-match-p "\r?\n\r?\n" request))
                       (process-put client 'lispm-mail-answered t)
                       (funcall handler client request)))))
         (ipv4 (make-network-process :name "lispm-mail sign-in" :server t
                                     :host "127.0.0.1" :service t :family 'ipv4
                                     :coding 'binary :noquery t :filter filter))
         ;; localhost may be ::1 to the browser.
         (ipv6 (ignore-errors
                 (make-network-process :name "lispm-mail sign-in" :server t
                                       :host "::1" :service (process-contact ipv4 :service)
                                       :family 'ipv6 :coding 'binary :noquery t :filter filter))))
    (delq nil (list ipv4 ipv6))))

(defun lispm-mail-sign-in--exchange (kind fields callback)
  "Ask KIND's token endpoint for tokens with FIELDS, in the background.
Call CALLBACK with them, as `lispm-mail-worker-token-answer' gives
them, and nil, or with nil and what went wrong."
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data (lispm-mail-worker-token-request fields)))
    (condition-case err
        (url-retrieve (plist-get (alist-get kind lispm-mail-sign-ins) :token)
                      (lambda (status)
                        (let ((buffer (current-buffer))
                              value error)
                          (condition-case err
                              (setq value (lispm-mail-worker-token-answer buffer))
                            (error (setq error (if (plist-get status :error)
                                                   "The sign-in server could not be reached"
                                                 (error-message-string err)))))
                          (kill-buffer buffer)
                          (funcall callback value error)))
                      nil t t)
      (error (funcall callback nil (error-message-string err))))))

(defun lispm-mail-sign-in (kind address callback)
  "Sign ADDRESS in on the page of KIND's provider, in the browser.
KIND is a key of `lispm-mail-sign-ins'.  Call CALLBACK once, with
what signs the account in, (:access-token :refresh-token :expires-in),
and nil, or with nil and what went wrong.  Return a function giving
the sign-in up, CALLBACK then never called."
  (let* ((provider (or (alist-get kind lispm-mail-sign-ins)
                       (error "No sign-in %s" kind)))
         (client-id (or (lispm-mail-sign-in-client-id kind)
                        (user-error "LispM Mail has no client ID at %s yet: see `%s'"
                                    (plist-get provider :name) (plist-get provider :client-id))))
         (client-secret (or (lispm-mail-sign-in-client-secret kind)
                            (when (plist-member provider :client-secret)
                              (user-error "This LispM Mail has no secret for its client ID at %s: \
install it from the App Store, or see `%s'"
                                          (plist-get provider :name)
                                          (plist-get provider :client-secret)))))
         (verifier (lispm-mail-sign-in--random))
         (state (lispm-mail-sign-in--random))
         (servers nil)
         (timer nil)
         (done nil)
         (exchanging nil)
         (redirect nil))
    (cl-labels
        ((stop ()
           (setq done t)
           (when timer (cancel-timer timer))
           (dolist (server servers)
             (when (process-live-p server) (delete-process server))))
         (finish (value error)
           (unless done
             (stop)
             (funcall callback value error)))
         (answer (client request)
           (let* ((query (lispm-mail-sign-in-query request))
                  (said (lambda (name) (cadr (assoc name query)))))
             (cond
              ((or done exchanging (not (funcall said "state")))
               (lispm-mail-sign-in--respond
                client "404 Not Found"
                (lispm-mail-sign-in-page "Nothing here" "This page is LispM Mail's, for signing in.")))
              ((not (equal (funcall said "state") state))
               (lispm-mail-sign-in--respond
                client "400 Bad Request"
                (lispm-mail-sign-in-page "Not signed in" "LispM Mail did not ask for this sign-in.")))
              ((funcall said "error")
               (let ((text (if (equal (funcall said "error") "access_denied")
                               "The sign-in was cancelled."
                             (or (funcall said "error_description") (funcall said "error")))))
                 (lispm-mail-sign-in--respond
                  client "200 OK" (lispm-mail-sign-in-page "Not signed in" text))
                 (finish nil (string-trim-right text "\\."))))
              (t
               (setq exchanging t)
               (lispm-mail-sign-in--exchange
                kind `(("client_id" ,client-id)
                       ("client_secret" ,client-secret)
                       ("grant_type" "authorization_code")
                       ("code" ,(funcall said "code"))
                       ("redirect_uri" ,redirect)
                       ("code_verifier" ,verifier)
                       ("scope" ,(plist-get provider :scope)))
                (lambda (value error)
                  (lispm-mail-sign-in--respond
                   client "200 OK"
                   (if value
                       (lispm-mail-sign-in-page
                        "Signed in"
                        (format "LispM Mail can read and send the mail of %s now.  This page may be closed."
                                address))
                     (lispm-mail-sign-in-page "Not signed in" error)))
                  (finish value error))))))))
      (setq servers (lispm-mail-sign-in--listen #'answer)
            redirect (format (plist-get provider :redirect) (process-contact (car servers) :service)))
      (condition-case err
          (funcall lispm-mail-sign-in-browse-function
                   (concat (plist-get provider :authorize) "?"
                           (url-build-query-string
                            `(("client_id" ,client-id)
                              ("response_type" "code")
                              ("redirect_uri" ,redirect)
                              ("scope" ,(plist-get provider :scope))
                              ("state" ,state)
                              ("code_challenge" ,(lispm-mail-sign-in-challenge verifier))
                              ("code_challenge_method" "S256")
                              ("login_hint" ,address)
                              ,@(plist-get provider :parameters)))))
        (error (stop) (signal (car err) (cdr err))))
      (setq timer (run-at-time lispm-mail-sign-in-timeout nil
                               #'finish nil "Signing in took too long: try again"))
      #'stop)))

(provide 'lispm-mail-sign-in)
;;; lispm-mail-sign-in.el ends here
