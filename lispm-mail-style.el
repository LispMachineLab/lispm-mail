;;; lispm-mail-style.el --- LispM Mail's look  -*- lexical-binding: t; -*-

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

;; How LispM Mail looks: as the mail programs of the classic Mac did,
;; Outlook Express and Eudora, a folder list beside a table of messages
;; and the message under it, its headers in a box over its text.  Here
;; are the face its windows are set in, Geneva where Emacs finds it; its
;; colours, light and dark, which a desktop may give its own; and the
;; icons it draws with SVG, filled and outlined in the classic manner,
;; where Emacs shows SVG, text elsewhere: folders, trays, the Trash,
;; envelopes, flags, paper clips and documents.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'color)

(defcustom lispm-mail-font-families
  '("Geneva" "Lucida Grande" "Helvetica Neue" "Helvetica" "DejaVu Sans")
  "The families LispM Mail's windows are set in, the first Emacs finds used.
Where none is found, `variable-pitch' is."
  :type '(repeat string)
  :group 'lispm-mail)

(defface lispm-mail-ui '((t :inherit variable-pitch))
  "Face LispM Mail's windows are set in.  Its family, unless customized, is
the first of `lispm-mail-font-families' found."
  :group 'lispm-mail)

(defface lispm-mail-dim
  '((((background dark)) :foreground "#98989F")
    (t :foreground "#8A8A8E"))
  "Face of what is said beside: addresses, dates, first words."
  :group 'lispm-mail)

(defface lispm-mail-faint
  '((((background dark)) :foreground "#636366")
    (t :foreground "#AEAEB2"))
  "Face of what is said fainter still: counts of what is read."
  :group 'lispm-mail)

(defface lispm-mail-accent
  '((((background dark)) :foreground "#9999FF")
    (t :foreground "#3333CC"))
  "Face of what stands out: links and the like."
  :group 'lispm-mail)

(defface lispm-mail-strong '((t :weight bold))
  "Face of an unread message's sender, a title."
  :group 'lispm-mail)

(defface lispm-mail-separator
  '((((background dark)) :background "#38383A" :extend t)
    (t :background "#E5E5EA" :extend t))
  "Face of the thin line between two messages of the list."
  :group 'lispm-mail)

(defface lispm-mail-header-box
  '((((background dark)) :background "#3A3A3A" :extend t)
    (t :background "#E6E6E6" :extend t))
  "Face of the box of a message's headers, over its text."
  :group 'lispm-mail)

(defface lispm-mail-selected
  '((((background dark)) :background "#444444" :extend t)
    (t :background "#DDDDDD" :extend t))
  "Face of the message chosen, its pane not in front."
  :group 'lispm-mail)

(defface lispm-mail-selected-active '((t :inherit region :extend t))
  "Face of the message, or mailbox, chosen, its pane in front."
  :group 'lispm-mail)

(defface lispm-mail-marked
  '((((background dark)) :background "#3B3320" :extend t)
    (t :background "#FFF4CC" :extend t))
  "Face of the messages selected together."
  :group 'lispm-mail)

(defface lispm-mail-button
  '((((type graphic) (background dark))
     :box (:line-width 2 :style released-button) :background "#555555" :foreground "white")
    (((type graphic))
     :box (:line-width 2 :style released-button) :background "#DDDDDD" :foreground "black")
    (t :inverse-video t))
  "Face of a push button, raised as the classic Mac's were."
  :group 'lispm-mail)

(defface lispm-mail-button-pressed
  '((((type graphic)) :inherit lispm-mail-button :box (:line-width 2 :style pressed-button))
    (t :inherit lispm-mail-button))
  "Face of a push button under the mouse."
  :group 'lispm-mail)

(defface lispm-mail-error '((t :inherit error))
  "Face of what went wrong."
  :group 'lispm-mail)

(defface lispm-mail-quote-bar
  '((((background dark)) :foreground "#6666CC")
    (t :foreground "#9999FF"))
  "Face of the bar before a line quoted, as Netscape drew it."
  :group 'lispm-mail)

;;;; The font

(defvar lispm-mail--family nil
  "The family `lispm-mail-ui' was given, or `none'.")

(defun lispm-mail-style-font ()
  "Give `lispm-mail-ui' the first of `lispm-mail-font-families' found, once,
unless its family was customized."
  (when (and (null lispm-mail--family) (display-graphic-p))
    (setq lispm-mail--family
          (or (seq-find (lambda (family) (find-font (font-spec :family family)))
                        lispm-mail-font-families)
              'none))
    (when (and (stringp lispm-mail--family)
               (not (get 'lispm-mail-ui 'saved-face))
               (not (get 'lispm-mail-ui 'customized-face)))
      (set-face-attribute 'lispm-mail-ui nil :family lispm-mail--family))))

(defun lispm-mail-style-dress (&optional background)
  "Set the current buffer in `lispm-mail-ui', on BACKGROUND, a face, if given."
  (lispm-mail-style-font)
  (face-remap-add-relative 'default 'lispm-mail-ui)
  (when background
    (face-remap-add-relative 'default background)
    (face-remap-add-relative 'fringe background))
  ;; Its windows draw their own edges:
  ;; no fringes, no scroll bars.
  (setq-local left-fringe-width 0
              right-fringe-width 0
              vertical-scroll-bar nil
              horizontal-scroll-bar nil)
  ;; A line too long ends in an ellipsis, where a window has no fringe.
  (let ((table (make-display-table)))
    (set-display-table-slot table 'truncation (make-glyph-code ?… 'lispm-mail-dim))
    (setq buffer-display-table table)))

;;;; Lengths

(defun lispm-mail-style-width (text)
  "Return how many pixels TEXT takes, as its faces draw it."
  (if (and (fboundp 'string-pixel-width) (display-graphic-p))
      (string-pixel-width text)
    (* (string-width text) (max 1 (frame-char-width)))))

(defun lispm-mail-style-fit (text pixels)
  "Return TEXT cut to PIXELS wide as its faces draw it, ending in an ellipsis."
  (if (<= (lispm-mail-style-width text) pixels)
      text
    (let ((low 0) (high (length text)))
      ;; The longest start of TEXT that fits with its ellipsis.
      (while (< low high)
        (let ((middle (/ (+ low high 1) 2)))
          (if (<= (lispm-mail-style-width (concat (substring text 0 middle) "…")) pixels)
              (setq low middle)
            (setq high (1- middle)))))
      (concat (string-trim-right (substring text 0 low)) (propertize "…" 'face (get-text-property 0 'face text))))))

(defun lispm-mail-style-space (pixels)
  "Return a space PIXELS wide."
  (propertize " " 'display `(space :width (,(max 0 pixels)))))

(defun lispm-mail-style-to (pixels &optional from-right)
  "Return a space reaching PIXELS from the left, or FROM-RIGHT, from the right."
  (propertize " " 'display (if from-right
                               `(space :align-to (- right (,pixels)))
                             `(space :align-to (,pixels)))))

(defun lispm-mail-style-hairline-mode-line ()
  "Make the current buffer's mode line a hairline under its window."
  ;; Not `mode-line', which the header line inherits too.
  (dolist (face '(mode-line-active mode-line-inactive))
    (face-remap-add-relative face '(:inherit lispm-mail-separator :height 10 :box nil
                                    :underline nil :overline nil)))
  (setq-local mode-line-format " "))

(defun lispm-mail-style-rule (&optional indent)
  "Return a line a pixel high across the window, from INDENT pixels in.
Its newline weighs nothing, so that the line is as high as the rule."
  (concat (propertize " " 'display `(space :width (,(or indent 0)) :height (1)))
          (propertize " " 'display '(space :align-to right :height (1)) 'face 'lispm-mail-separator)
          (propertize "\n" 'line-height t 'line-spacing 0)))

;;;; Colours

(defun lispm-mail-style-hex (colour)
  "Return COLOUR, a name Emacs knows, as SVG knows it: #rrggbb."
  (if-let* ((values (and colour (color-values colour))))
      (apply #'format "#%02x%02x%02x" (mapcar (lambda (value) (/ value 257)) values))
    (or colour "#000000")))

(defun lispm-mail-style-colour (face &optional attribute)
  "Return FACE's foreground, or ATTRIBUTE, as #rrggbb."
  (lispm-mail-style-hex (face-attribute face (or attribute :foreground) nil 'default)))

;;;; Pictures

(defun lispm-mail-style-svg-p ()
  "Return non-nil where Emacs shows SVG pictures."
  (and (display-images-p) (image-type-available-p 'svg)))

(defvar lispm-mail--pictures (make-hash-table :test #'equal)
  "The pictures drawn, by what they show.")

(defmacro lispm-mail-style-picture (key &rest body)
  "Return the picture KEY names, drawn by BODY the first time."
  (declare (indent 1) (debug t))
  `(let ((key ,key))
     (or (gethash key lispm-mail--pictures)
         (puthash key (progn ,@body) lispm-mail--pictures))))

(defun lispm-mail-style-image-text (image text)
  "Return TEXT shown as IMAGE where it is one, as TEXT elsewhere."
  (if image (propertize text 'display image) text))

(defconst lispm-mail-icons
  '((folder ("M1.5 3.5 H6 L7.5 5 H14.5 V13.5 H1.5 Z" "#CCCCFF" "#000000")
            ("M2.5 6.5 H13.5" nil "#FFFFFF")
            ("M2.5 12.5 H13.5" nil "#9999CC"))
    (inbox ("M3.5 1.5 H12.5 V8.5 H3.5 Z" "#FFFFFF" "#000000")
           ("M3.5 1.5 L8 5.5 L12.5 1.5" nil "#000000")
           ("M1.5 8.5 H5.5 V10.5 H10.5 V8.5 H14.5 V14.5 H1.5 Z" "#CCCCCC" "#000000")
           ("M2.5 13.5 H13.5" nil "#999999"))
    (outbox ("M8 0.8 L11.8 4.6 H9.5 V9 H6.5 V4.6 H4.2 Z" "#33AA33" "#000000")
            ("M1.5 8.5 H5.5 V10.5 H10.5 V8.5 H14.5 V14.5 H1.5 Z" "#CCCCCC" "#000000")
            ("M2.5 13.5 H13.5" nil "#999999"))
    (sent ("M1.5 3.5 H14.5 V12.5 H1.5 Z" "#FFFFFF" "#000000")
          ("M1.5 3.5 L8 8.5 L14.5 3.5" nil "#000000")
          ("M9 10.5 H12 V9 L15 11.5 L12 14 V12.5 H9 Z" "#33AA33" "#000000"))
    (drafts ("M2.5 1.5 H9 L11.5 4 V14.5 H2.5 Z" "#FFFFFF" "#000000")
            ("M4.5 5.5 H8.5 M4.5 7.5 H9.5 M4.5 9.5 H7.5" nil "#999999")
            ("M8 14 L8.6 11.6 L13.6 6.6 L15.4 8.4 L10.4 13.4 Z" "#FFCC33" "#000000"))
    (trash ("M2.5 3.5 H13.5 V5.5 H2.5 Z" "#CCCCCC" "#000000")
           ("M6.5 3.5 V1.5 H9.5 V3.5" nil "#000000")
           ("M3.5 5.5 L4.5 14.5 H11.5 L12.5 5.5 Z" "#DDDDDD" "#000000")
           ("M6 7.5 V12.5 M8 7.5 V12.5 M10 7.5 V12.5" nil "#888888"))
    (junk ("M1.5 3.5 H14.5 V12.5 H1.5 Z" "#EEEEEE" "#000000")
          ("M1.5 3.5 L8 8.5 L14.5 3.5" nil "#000000")
          ("M8 4.5 A4 4 0 1 1 7.99 4.5 Z" "#FFFFFF" "#CC0000")
          ("M5.2 5.7 L10.8 11.3" nil "#CC0000"))
    (archive ("M1.5 3.5 H14.5 V6.5 H1.5 Z" "#CCCCFF" "#000000")
             ("M2.5 6.5 H13.5 V14.5 H2.5 Z" "#CCCCFF" "#000000")
             ("M6.5 8.5 H9.5" nil "#000000"))
    (all ("M3.5 1.5 H14.5 V9.5 H3.5 Z" "#FFFFFF" "#000000")
         ("M1.5 5.5 H12.5 V13.5 H1.5 Z" "#FFFFFF" "#000000")
         ("M1.5 5.5 L7 9.5 L12.5 5.5" nil "#000000"))
    (flagged ("M3.5 15 V1.5" nil "#000000")
             ("M3.5 2 H13 L11 5.5 L13 9 H3.5 Z" "#DD0000" "#000000"))
    (unread ("M1.5 3.5 H14.5 V12.5 H1.5 Z" "#FFFFFF" "#000000")
            ("M1.5 3.5 L8 8.5 L14.5 3.5" nil "#000000"))
    (reply ("M6.5 3 L2 7.5 L6.5 12 V9.2 H9 C11.5 9.2 13 10.5 14 13 C14 8.5 11.5 5.8 9 5.8 H6.5 Z"
            "#9999FF" "#000000"))
    (forward ("M9.5 3 L14 7.5 L9.5 12 V9.2 H7 C4.5 9.2 3 10.5 2 13 C2 8.5 4.5 5.8 7 5.8 H9.5 Z"
              "#9999FF" "#000000"))
    (flag ("M3.5 15 V1.5" nil "#000000")
          ("M3.5 2 H13 L11 5.5 L13 9 H3.5 Z" "#DD0000" "#000000"))
    (clip ("M10 4 V11 A2 2 0 0 1 6 11 V3.5 A1.5 1.5 0 0 1 9 3.5 V10.5 A0.6 0.6 0 0 1 7.8 10.5 V5"
           nil "#555555"))
    (document ("M3.5 1.5 H9.5 L12.5 4.5 V14.5 H3.5 Z" "#FFFFFF" "#000000")
              ("M9.5 1.5 V4.5 H12.5" nil "#000000")
              ("M5.5 7.5 H10.5 M5.5 9.5 H10.5 M5.5 11.5 H8.5" nil "#999999"))
    (account ("M1.5 2.5 H14.5 V11.5 H1.5 Z" "#DDDDDD" "#000000")
             ("M3.5 4.5 H12.5 V9.5 H3.5 Z" "#9999FF" "#000000")
             ("M5.5 13.5 H10.5" nil "#000000")
             ("M4.5 14.5 H11.5" nil "#000000")))
  "The icons, by name: each a list of SVG paths on 16 by 16, (PATH FILL
STROKE), outlined in black and filled, as the classic Mac drew them.")

(defconst lispm-mail-icon-glyphs
  '((inbox . "▤") (drafts . "✎") (sent . "➤") (archive . "▣") (all . "▦") (flagged . "⚑")
    (junk . "⊘") (trash . "✕") (outbox . "⇪") (folder . "▢") (clip . "+") (reply . "↩")
    (forward . "↪") (unread . "•") (flag . "⚑") (document . "▯") (account . "▣"))
  "The glyphs standing in for the icons where no picture is shown.")

(defun lispm-mail-style-icon (name &optional size)
  "Return a string showing the icon NAME, SIZE pixels high."
  (let ((size (or size 16)))
    (if (lispm-mail-style-svg-p)
        (lispm-mail-style-image-text
         (lispm-mail-style-picture (list 'icon name size)
           (let ((svg (svg-create size size :viewBox "0 0 16 16" :shape-rendering "crispEdges")))
             (dolist (path (cdr (or (assq name lispm-mail-icons) (assq 'folder lispm-mail-icons))))
               (pcase-let ((`(,d ,fill ,stroke) path))
                 (svg-node svg 'path :d d :fill (or fill "none") :stroke (or stroke "none")
                           :stroke-width 1 :stroke-linejoin "miter")))
             (svg-image svg :ascent 'center)))
         "*")
      (alist-get name lispm-mail-icon-glyphs "▢"))))

(defun lispm-mail-style-initials (entry)
  "Return the initials of ENTRY, (NAME . ADDRESS), at most two."
  (let* ((name (or (and (car entry) (not (string-empty-p (car entry))) (car entry))
                   (car (split-string (or (cdr entry) "?") "@"))))
         (words (seq-filter (lambda (word) (string-match-p "\\w" word))
                            (split-string name "[ ._-]+" t))))
    (upcase (if (and (cdr words) (string-match-p "\\`[[:ascii:]]" (car words)))
                (concat (substring (car words) 0 1) (substring (car (last words)) 0 1))
              (substring (or (car words) "?") 0 1)))))

(defun lispm-mail-style-forget-pictures (&rest _)
  "Forget the pictures drawn, to be drawn again in the colours now in use."
  (clrhash lispm-mail--pictures))

(add-hook 'enable-theme-functions #'lispm-mail-style-forget-pictures)

(provide 'lispm-mail-style)
;;; lispm-mail-style.el ends here
