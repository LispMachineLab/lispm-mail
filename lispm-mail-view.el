;;; lispm-mail-view.el --- LispM Mail's messages, shown  -*- lexical-binding: t; -*-

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

;; A message drawn as the classic Mac's mail programs drew it: its
;; headers in a box over it, From, Date, To and Subject, its
;; attachments listed there with their documents' icons, each opening
;; or saving it; then its text, HTML drawn by shr and plain text with its
;; quotations under bars, as Netscape drew them, and its links
;; pressable.  Pictures on the web that HTML
;; links to are not fetched unless asked, as each tells its sender the
;; message was read.  Emacs's MIME library, mm, takes the message
;; apart.  `lispm-mail-view-insert' draws a message's file at point.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'mm-decode)
(require 'mm-view)
(require 'mail-parse)
(require 'rfc2047)
(require 'shr)
(require 'svg)
(require 'browse-url)
(require 'lispm-mail-style)
(require 'lispm-mail-accounts)

(declare-function lispm-mail-worker-date "lispm-mail-worker" (text))

(defcustom lispm-mail-prefer-html t
  "Non-nil to show a message's HTML when it is written both ways, nil its text."
  :type 'boolean
  :group 'lispm-mail)

(defcustom lispm-mail-load-remote-pictures nil
  "Non-nil to fetch the pictures on the web messages show.
Each tells its sender that the message was read, and when."
  :type 'boolean
  :group 'lispm-mail)

(defface lispm-mail-subject '((t :weight bold))
  "Face of a message's subject, in its headers."
  :group 'lispm-mail)

(defface lispm-mail-sender '((t :weight bold))
  "Face of the names of a message's headers."
  :group 'lispm-mail)

;; A line quoted is the message's colour, dimmed; its bar says it is quoted.
(defface lispm-mail-quote '((t :inherit lispm-mail-dim))
  "Face of the lines a message quotes."
  :group 'lispm-mail)

(defface lispm-mail-attachment
  '((((background dark)) :box (:line-width (8 . 5) :color "#2C2C2E") :background "#2C2C2E")
    (t :box (:line-width (8 . 5) :color "#F2F2F7") :background "#F2F2F7"))
  "Face of an attachment's button, where no picture is drawn."
  :group 'lispm-mail)

(defface lispm-mail-notice
  '((((background dark)) :background "#4A4426" :extend t)
    (t :background "#FFFFCC" :extend t))
  "Face of a notice over a message, as pictures not fetched."
  :group 'lispm-mail)

(defface lispm-mail-rule '((t :inherit lispm-mail-separator))
  "Face of the line between a message's headers and its text."
  :group 'lispm-mail)

(defvar-local lispm-mail-view--handles nil
  "The MIME parts of the message shown, to be let go when it is no longer.")

(defvar-local lispm-mail-view--pictures nil
  "Non-nil when the message shown links to pictures on the web not fetched.")

;;;; Words

(defun lispm-mail-address-text (entry &optional short)
  "Return ENTRY, (NAME . ADDRESS), as it is read: the name, or the address.
Unless SHORT, \"Name <address>\"."
  (let ((name (car entry))
        (address (cdr entry)))
    (cond ((and name (not (string-empty-p name)))
           (if (or short (null address)) name (format "%s <%s>" name address)))
          (address address)
          (t "(nobody)"))))

(defun lispm-mail-initials (entry)
  "Return the initials of ENTRY, (NAME . ADDRESS), at most two."
  (lispm-mail-style-initials entry))

(defun lispm-mail-date-text (time &optional long)
  "Return TIME, seconds since the epoch, as a list shows it: the hour today,
Yesterday, the day this week, the day and month this year, the date
before.  LONG says it in full."
  (if (not time)
      ""
    (let* ((now (decode-time))
           (then (decode-time time))
           (days (- (time-to-days (current-time)) (time-to-days time))))
      (cond
       (long (format-time-string "%A, %B %-d, %Y %-I:%M %p" time))
       ((= days 0) (format-time-string "%-I:%M %p" time))
       ((= days 1) "Yesterday")
       ((and (> days 1) (< days 7)) (format-time-string "%A" time))
       ((= (decoded-time-year now) (decoded-time-year then)) (format-time-string "%b %-d" time))
       (t (format-time-string "%-m/%-d/%y" time))))))

(defun lispm-mail-size-text (bytes)
  "Return BYTES as a size is read: 12 KB, 1.4 MB."
  (cond ((null bytes) "")
        ((< bytes 1000) (format "%d bytes" bytes))
        ((< bytes 1000000) (format "%d KB" (max 1 (round bytes 1000))))
        (t (format "%.1f MB" (/ bytes 1000000.0)))))

;;;; Pictures


;;;; Taking a message apart

(defun lispm-mail-view-parse (file)
  "Return the message of FILE taken apart: (HEADERS . HANDLE).
HEADERS is a plist as the list knows a message, HANDLE its MIME parts."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (goto-char (point-min))
    (while (search-forward "\r\n" nil t)
      (replace-match "\n" t t))
    (goto-char (point-min))
    (let ((headers (lispm-mail-view--headers))
          (handle (mm-dissect-buffer t t)))
      (cons headers handle))))

(defun lispm-mail-view--headers ()
  "Return the headers of the raw message in the current buffer, a plist."
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      (narrow-to-region (point-min) (if (re-search-forward "^\r?$" nil t) (point) (point-max)))
      (let ((text (decode-coding-string (buffer-string) 'utf-8)))
        (with-temp-buffer
          (insert text)
          (goto-char (point-min))
          (cl-flet ((field (name &optional all)
                      (when-let* ((value (mail-fetch-field name nil all)))
                        (string-trim value)))
                    (addresses (text)
                      (when text
                        (mapcar (lambda (entry)
                                  (cons (and (cdr entry) (rfc2047-decode-string (cdr entry)))
                                        (car entry)))
                                (ignore-errors (mail-header-parse-addresses text))))))
            (list :from (car (addresses (field "from")))
                  :to (addresses (field "to" t))
                  :cc (addresses (field "cc" t))
                  :bcc (addresses (field "bcc" t))
                  :reply-to (addresses (field "reply-to"))
                  :subject (rfc2047-decode-string (or (field "subject") ""))
                  :date (and (fboundp 'lispm-mail-worker-date) (lispm-mail-worker-date (field "date")))
                  :message-id (field "message-id")
                  :references (field "references")
                  :list-unsubscribe (field "list-unsubscribe"))))))))

(defun lispm-mail-view--attachment-p (handle)
  "Return non-nil when HANDLE, a single part, is shown as an attachment."
  (let ((type (mm-handle-media-type handle))
        (disposition (car (mm-handle-disposition handle))))
    (or (equal disposition "attachment")
        (not (or (member type '("text/plain" "text/html" "message/rfc822"))
                 (and (string-prefix-p "image/" type) (equal disposition "inline")))))))

(defun lispm-mail-view--choose (parts)
  "Return the part of PARTS, a multipart/alternative's, to show."
  (let ((wanted (if lispm-mail-prefer-html '("text/html" "text/plain") '("text/plain" "text/html"))))
    (or (seq-some (lambda (type)
                    (seq-find (lambda (part)
                                (if (stringp (car part))
                                    (and (equal type "text/html") (equal (car part) "multipart/related"))
                                  (equal (mm-handle-media-type part) type)))
                              parts))
                  wanted)
        (car (last parts)))))

(defun lispm-mail-view-parts (handle)
  "Return what HANDLE, a message's MIME parts, shows: (BODIES . ATTACHMENTS),
the parts drawn as its text, in order, and those listed as attachments."
  (let (bodies attachments)
    (cl-labels ((walk (part)
                  (cond
                   ((null part))
                   ((stringp (car part))
                    (pcase (car part)
                      ("multipart/alternative" (walk (lispm-mail-view--choose (cdr part))))
                      ("multipart/related"
                       ;; The first part is shown; the others are what it
                       ;; shows inside, by their Content-ID.
                       (walk (cadr part)))
                      (_ (mapc #'walk (cdr part)))))
                   ((lispm-mail-view--attachment-p part) (push part attachments))
                   (t (push part bodies)))))
      (walk handle))
    (cons (nreverse bodies) (nreverse attachments))))

(defun lispm-mail-view-attachment-name (handle)
  "Return the file name of HANDLE, an attachment."
  (or (mail-content-type-get (mm-handle-disposition handle) 'filename)
      (mail-content-type-get (mm-handle-type handle) 'name)
      (concat "attachment." (or (cadr (split-string (mm-handle-media-type handle) "/")) "bin"))))

(defun lispm-mail-view-attachment-size (handle)
  "Return the size of HANDLE's data, decoded, in bytes."
  (with-current-buffer (mm-handle-buffer handle)
    (let ((size (buffer-size)))
      (pcase (mm-handle-encoding handle)
        ('base64 (round (* size 0.74)))
        (_ size)))))

;;;; Drawing it

(defun lispm-mail-view--rule (&optional indent)
  "Insert a thin line across the message, from INDENT pixels in."
  (insert (lispm-mail-style-rule indent)))

(defun lispm-mail-view--addresses (entries)
  "Return ENTRIES, addresses, as the head of a message says them: \"me\"
for one's own."
  (mapconcat (lambda (entry)
               (propertize (if (lispm-mail-own-address-p (cdr entry))
                               "me"
                             (lispm-mail-address-text entry t))
                           'help-echo (cdr entry)))
             entries ", "))

(defvar lispm-mail-view--label-width 96
  "Pixels from the header box's edge to the end of its names, as drawn now.")

(defun lispm-mail-view--field (label value)
  "Insert a line of the header box: LABEL, right-aligned, then VALUE."
  (let ((label (propertize (concat label ":") 'face 'lispm-mail-sender)))
    (insert (lispm-mail-style-to (- lispm-mail-view--label-width (lispm-mail-style-width label)))
            label
            (lispm-mail-style-to (+ lispm-mail-view--label-width 10))
            value
            (propertize "\n" 'line-spacing 2))))

(defun lispm-mail-view--attachment-button (handle)
  "Return a button for HANDLE, an attachment: its document's icon, its
name and size."
  (let ((name (lispm-mail-view-attachment-name handle))
        (size (lispm-mail-size-text (lispm-mail-view-attachment-size handle))))
    (concat (lispm-mail-style-icon 'document 14)
            (lispm-mail-style-space 3)
            (propertize (format "%s (%s)" name size)
                        'face 'lispm-mail-accent 'mouse-face 'highlight
                        'button t 'category 'default-button 'follow-link t
                        'action (lambda (_) (lispm-mail-view--attachment-action handle))
                        'lispm-mail-attachment handle
                        'help-echo "Open or save it"))))

(defun lispm-mail-view--header (headers &optional attachments)
  "Insert HEADERS, a plist, as the box over the message shown: From, Date,
To, Cc, Subject, and ATTACHMENTS, handles, each a button."
  (let* ((from (plist-get headers :from))
         (start (point))
         (lispm-mail-view--label-width
          ;; As wide as the widest name the box shows, and a margin.
          (+ 16 (apply #'max (mapcar (lambda (label)
                                       (lispm-mail-style-width
                                        (propertize (concat label ":") 'face 'lispm-mail-sender)))
                                     (append '("From" "Date" "Subject")
                                             (and (plist-get headers :reply-to) '("Reply-To"))
                                             (and attachments '("Attachments"))))))))
    (insert (propertize "\n" 'line-height t 'line-spacing 8))
    (lispm-mail-view--field "From" (concat (propertize (lispm-mail-address-text from t) 'face 'lispm-mail-sender)
                                           (if (cdr from) (concat " <" (cdr from) ">") "")))
    (lispm-mail-view--field "Date" (lispm-mail-date-text (plist-get headers :date) t))
    (when-let* ((to (plist-get headers :to)))
      (lispm-mail-view--field "To" (lispm-mail-view--addresses to)))
    (when-let* ((cc (plist-get headers :cc)))
      (lispm-mail-view--field "Cc" (lispm-mail-view--addresses cc)))
    (when-let* ((bcc (plist-get headers :bcc)))
      (lispm-mail-view--field "Bcc" (lispm-mail-view--addresses bcc)))
    (when-let* ((reply-to (plist-get headers :reply-to))
                ((not (equal (cdr (car reply-to)) (cdr from)))))
      (lispm-mail-view--field "Reply-To" (lispm-mail-view--addresses reply-to)))
    (lispm-mail-view--field "Subject" (propertize (let ((subject (plist-get headers :subject)))
                                                    (if (string-blank-p (or subject "")) "(no subject)" subject))
                                                  'face 'lispm-mail-subject))
    (when attachments
      (lispm-mail-view--field (if (cdr attachments) "Attachments" "Attachment")
                              (mapconcat #'lispm-mail-view--attachment-button attachments
                                         (lispm-mail-style-space 14))))
    (insert (propertize "\n" 'line-height t 'line-spacing 6))
    (add-face-text-property start (point) 'lispm-mail-header-box t)
    (put-text-property start (point) 'lispm-mail-head t)
    (insert (propertize "\n" 'line-height t 'line-spacing 14))))

(defun lispm-mail-view--links (start end)
  "Make the web and mail addresses between START and END pressable."
  (save-excursion
    (goto-char start)
    (while (re-search-forward browse-url-button-regexp end t)
      (let ((url (match-string 0)))
        (unless (get-text-property (match-beginning 0) 'button)
          (make-text-button (match-beginning 0) (match-end 0)
                            'action (lambda (_) (browse-url url))
                            'follow-link t 'help-echo url 'face 'link))))))

(defun lispm-mail-view--quotes (start end)
  "Draw the lines quoted between START and END under bars, one a level, and
dim the signature."
  (save-excursion
    (goto-char start)
    (while (re-search-forward "^\\(\\(?:[ \t]*>\\)+\\)[ \t]?" end t)
      (let* ((depth (cl-count ?> (match-string 1)))
             (bars (apply #'concat
                          (make-list depth (concat (propertize " " 'display '(space :width (3))
                                                               'face (list :background
                                                                           (lispm-mail-style-colour
                                                                            'lispm-mail-quote-bar)))
                                                   (lispm-mail-style-space 10))))))
        (put-text-property (match-beginning 0) (match-end 0) 'display bars)
        (add-face-text-property (match-end 0) (line-end-position) 'lispm-mail-quote)))
    (goto-char start)
    (when (re-search-forward "^-- $" end t)
      (add-face-text-property (match-beginning 0) end 'lispm-mail-dim))))

(defun lispm-mail-view--remote-pictures-p (handle)
  "Return non-nil when HANDLE, HTML, shows pictures from the web."
  (mm-with-part handle
    (goto-char (point-min))
    (re-search-forward "<img[^>]+src=[\"']?https?:" nil t)))

(defun lispm-mail-view--decode (handle)
  "Return the text of HANDLE, a text part, decoded."
  (let ((charset (mail-content-type-get (mm-handle-type handle) 'charset)))
    (mm-decode-string (mm-get-part handle)
                      (intern (downcase (if (and charset (not (string-empty-p charset)))
                                            charset
                                          "utf-8"))))))

(defun lispm-mail-view--text (handle)
  "Insert HANDLE, a text part, drawn."
  (let ((start (point)))
    (pcase (mm-handle-media-type handle)
      ("text/html"
       (when (and (not lispm-mail-load-remote-pictures)
                  (lispm-mail-view--remote-pictures-p handle))
         (setq lispm-mail-view--pictures t))
       (let ((mm-html-blocked-images (if lispm-mail-load-remote-pictures nil "\\`https?:"))
             (shr-width nil)
             (shr-use-fonts t)
             (shr-max-width nil))
         (if (fboundp 'libxml-parse-html-region)
             (mm-shr handle)
           (mm-insert-part handle))))
      (_
       (insert (lispm-mail-view--decode handle))
         (when (equal (mail-content-type-get (mm-handle-type handle) 'format) "flowed")
           (require 'flow-fill)
           (save-restriction
             (narrow-to-region start (point))
             (fill-flowed nil (equal (mail-content-type-get (mm-handle-type handle) 'delsp) "yes"))
             (goto-char (point-max))))
         (lispm-mail-view--quotes start (point))
         (lispm-mail-view--links start (point))))
    (unless (bolp) (insert "\n"))))

(defun lispm-mail-view--picture (handle)
  "Insert HANDLE, a picture shown inline, fit to the window."
  (if (display-images-p)
      (let ((image (ignore-errors
                     (create-image (mm-with-unibyte-buffer
                                     (mm-insert-part handle)
                                     (buffer-string))
                                   nil t :max-width (max 200 (- (window-body-width nil t) 40))))))
        (if image
            (progn (insert-image image) (insert "\n"))
          (insert (propertize "(a picture Emacs cannot show)\n" 'face 'lispm-mail-dim))))
    (insert (propertize (format "(picture: %s)\n" (lispm-mail-view-attachment-name handle))
                        'face 'lispm-mail-dim))))

(defun lispm-mail-view--forwarded (handle)
  "Insert HANDLE, a message forwarded, as a message inside this one."
  (let* ((raw (mm-with-unibyte-buffer (mm-insert-part handle) (buffer-string)))
         (file (make-temp-file "lispm-mail-forwarded" nil ".eml")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'binary))
            (with-temp-file file (set-buffer-multibyte nil) (insert raw)))
          (insert (propertize "Forwarded message\n" 'face 'lispm-mail-dim))
          (let ((start (point)))
            (lispm-mail-view-insert file nil t)
            (put-text-property start (point) 'line-prefix
                               (propertize "┃ " 'face 'lispm-mail-dim))))
      (delete-file file))))

(defun lispm-mail-view-open-attachment (handle)
  "Open HANDLE, an attachment, as the system opens a file of its kind."
  (let* ((directory (make-temp-file "lispm-mail" t))
         (file (expand-file-name (file-name-nondirectory (lispm-mail-view-attachment-name handle))
                                 directory)))
    (mm-save-part-to-file handle file)
    (pcase system-type
      ('darwin (call-process "open" nil 0 nil file))
      ((or 'gnu/linux 'berkeley-unix)
       (if (executable-find "xdg-open")
           (call-process "xdg-open" nil 0 nil file)
         (find-file file)))
      (_ (find-file file)))
    (message "Opened %s" (file-name-nondirectory file))))

(defun lispm-mail-view-save-attachment (handle &optional file)
  "Save HANDLE, an attachment, to FILE, asked, in Downloads by default."
  (let* ((name (file-name-nondirectory (lispm-mail-view-attachment-name handle)))
         (downloads (seq-find #'file-directory-p
                              (list (expand-file-name "~/Downloads/") (expand-file-name "~/"))))
         (file (or file (read-file-name (format "Save %s to: " name) downloads nil nil name))))
    (when (file-directory-p file)
      (setq file (expand-file-name name file)))
    (mm-save-part-to-file handle file)
    (message "Saved %s" (abbreviate-file-name file))
    file))

(defun lispm-mail-view--attachment-action (handle)
  "Ask what to do with HANDLE, an attachment, and do it."
  (pcase (read-multiple-choice (format "%s: " (lispm-mail-view-attachment-name handle))
                               '((?o "open" "Open it as the system opens its kind")
                                 (?s "save" "Save it to a file")))
    (`(?o . ,_) (lispm-mail-view-open-attachment handle))
    (`(?s . ,_) (lispm-mail-view-save-attachment handle))))

(defun lispm-mail-view-insert (file &optional headers inner)
  "Insert the message of FILE drawn, at point.
HEADERS, a plist, says what the list knows of it, its own headers
otherwise; INNER, for a message inside another, keeps its parts with
the outer one's.  Return what of it the window needs: (:pictures
REMOTE-PICTURES-P :attachments HANDLES)."
  (unless inner
    ;; What shr finds by Content-ID, the parts of this message only.
    (setq mm-content-id-alist nil))
  (pcase-let* ((`(,own . ,handle) (lispm-mail-view-parse file))
               (headers (if headers
                            ;; What the list does not know of it, from its own.
                            (append (list :bcc (plist-get own :bcc)
                                          :reply-to (or (plist-get headers :reply-to)
                                                        (plist-get own :reply-to)))
                                    headers)
                          own))
               (`(,bodies . ,attachments) (lispm-mail-view-parts handle)))
    (push handle lispm-mail-view--handles)
    (unless inner (setq lispm-mail-view--pictures nil))
    (lispm-mail-view--header headers attachments)
    (let ((pictures (point)))
      (dolist (part bodies)
        (pcase (mm-handle-media-type part)
          ((or "text/plain" "text/html") (lispm-mail-view--text part))
          ("message/rfc822" (lispm-mail-view--forwarded part))
          ((pred (string-prefix-p "image/")) (lispm-mail-view--picture part))))
      (when (and lispm-mail-view--pictures (not inner))
        (save-excursion
          (goto-char pictures)
          (insert (propertize " " 'display '(space :width (8)) 'face 'lispm-mail-notice)
                  (propertize "Pictures from the web are not shown, as they tell the sender you read this.  "
                              'face 'lispm-mail-notice))
          (insert-text-button "Show Pictures" 'action (lambda (_) (lispm-mail-view-show-pictures))
                              'follow-link t 'face '(lispm-mail-accent lispm-mail-notice))
          (insert (propertize "\n" 'face 'lispm-mail-notice 'line-height 2.0)
                  (propertize "\n" 'line-spacing 6)))))
    (list :pictures lispm-mail-view--pictures :attachments attachments)))

(defvar lispm-mail-view-redraw-function nil
  "Function drawing the message shown again, as its window does.")

(defun lispm-mail-view-show-pictures ()
  "Show the pictures from the web of the message shown, this once."
  (interactive)
  (let ((lispm-mail-load-remote-pictures t))
    (when lispm-mail-view-redraw-function
      (funcall lispm-mail-view-redraw-function))))

(defun lispm-mail-view-release ()
  "Let go of the MIME parts of the message shown."
  (mapc #'mm-destroy-parts lispm-mail-view--handles)
  (setq lispm-mail-view--handles nil))

(defun lispm-mail-view-attachments-here ()
  "Return the attachments of the message shown in the current buffer."
  (let (handles)
    (save-excursion
      (goto-char (point-min))
      (let (match)
        (while (setq match (text-property-search-forward 'lispm-mail-attachment nil nil t))
          (push (prop-match-value match) handles))))
    (nreverse handles)))

(defun lispm-mail-view-plain-text (file)
  "Return the text of the message of FILE, as a reply quotes it."
  (pcase-let* ((`(,_ . ,handle) (lispm-mail-view-parse file))
               (`(,bodies . ,_) (let ((lispm-mail-prefer-html nil))
                                  (lispm-mail-view-parts handle))))
    (unwind-protect
        (with-temp-buffer
          (dolist (part bodies)
            (pcase (mm-handle-media-type part)
              ("text/plain" (insert (lispm-mail-view--decode part)))
              ("text/html"
               (let ((shr-width 72)
                     (shr-use-fonts nil)
                     (mm-html-inhibit-images t))
                 (when (fboundp 'libxml-parse-html-region)
                   (mm-shr part))))))
          (string-trim (buffer-substring-no-properties (point-min) (point-max))))
      (mm-destroy-parts handle))))

(provide 'lispm-mail-view)
;;; lispm-mail-view.el ends here
