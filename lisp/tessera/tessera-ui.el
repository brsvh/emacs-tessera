;;; tessera-ui.el --- UI primitives for Tessera  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bingshan Chang <chang@bingshan.org>

;; Author: Bingshan Chang <chang@bingshan.org>
;; Maintainer: Bingshan Chang <chang@bingshan.org>

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Shared presentation primitives for Tessera interface packages.

;;; Code:

(require 'tessera)

(defface tessera-header
  '((t :inherit header-line))
  "Face for a complete Tessera header line."
  :group 'tessera)

(defface tessera-header-status
  '((t :inherit tessera-header :weight bold))
  "Face for backend status in a Tessera header line."
  :group 'tessera)

(defface tessera-header-status-success
  '((t :inherit (success tessera-header-status)))
  "Face for a successful backend status in a Tessera header line."
  :group 'tessera)

(defface tessera-header-status-processing
  '((t :inherit (font-lock-variable-name-face tessera-header-status)))
  "Face for an ongoing backend status in a Tessera header line."
  :group 'tessera)

(defface tessera-header-status-fail
  '((t :inherit (error tessera-header-status)))
  "Face for a failed backend status in a Tessera header line."
  :group 'tessera)

(defface tessera-header-query
  '((t :inherit tessera-header))
  "Face for the current query in a Tessera header line."
  :group 'tessera)

(defface tessera-header-query-prefix
  '((t :inherit tessera-header-query :weight bold))
  "Face for the prefix of a Tessera header query."
  :group 'tessera)

(defface tessera-header-query-condition
  '((t :inherit (font-lock-keyword-face tessera-header-query)))
  "Face for the condition of a Tessera header query."
  :group 'tessera)

(defface tessera-header-statistics
  '((t :inherit (shadow tessera-header)))
  "Face for list statistics in a Tessera header line."
  :group 'tessera)

(defface tessera-header-statistics-unread
  '((t :inherit (error tessera-header-statistics)))
  "Face for a nonzero unread count in header statistics."
  :group 'tessera)

(defface tessera-header-statistics-visible
  '((t :inherit (success tessera-header-statistics)))
  "Face for a visible count in header statistics."
  :group 'tessera)

(defface tessera-entry-timestamp
  '((t :inherit shadow :weight normal))
  "Face for an entry timestamp."
  :group 'tessera)

(defface tessera-entry-timestamp-unread
  '((t :weight bold))
  "Face added to the timestamp of an unread entry."
  :group 'tessera)

(defface tessera-entry-current
  '((t :inherit highlight :extend t))
  "Face for the current complete entry."
  :group 'tessera)

(defface tessera-entry-subject
  '((t :inherit tessera-entry-timestamp :weight bold))
  "Face for the subject of a read entry."
  :group 'tessera)

(defface tessera-entry-unread
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the subject of an unread entry."
  :group 'tessera)

(defface tessera-entry-author
  '((t :inherit tessera-entry-timestamp :slant italic))
  "Face for the author of a read entry."
  :group 'tessera)

(defface tessera-entry-author-unread
  '((t :weight bold :slant italic))
  "Face added to the author of an unread entry."
  :group 'tessera)

(defface tessera-entry-feature
  '((t :inherit shadow))
  "Face for an entry content feature."
  :group 'tessera)

(defconst tessera-ui--header-top-padding 4
  "Header padding above the content row, in pixels.")

(defconst tessera-ui--header-bottom-padding 4
  "Header padding below the content row, in pixels.")

(defconst tessera-ui--entry-top-padding 8
  "Entry padding above the primary row, in pixels.")

(defconst tessera-ui--entry-bottom-padding 8
  "Entry padding below the secondary row, in pixels.")

(defconst tessera-ui--entry-horizontal-padding 2
  "Entry padding at each horizontal edge, in spaces.")

(defconst tessera-ui--entry-safety-gap 1
  "Gap between an entry and each window edge, in pixels.")

(defun tessera-ui--header-space (element)
  "Return one header space named ELEMENT."
  (propertize
   " "
   'face 'tessera-header
   'tessera-element element))

(defun tessera-ui--vertical-padding (face element pixels top-p window)
  "Return zero-width vertical padding using FACE and named ELEMENT.

PIXELS is the added height.  Add it above the baseline when TOP-P is
non-nil, and below otherwise.  Use WINDOW to resolve font metrics."
  (let* ((frame (window-frame window))
         (font (face-font face frame))
         (font-info (and font (font-info font frame)))
         (display
          (if (and (vectorp font-info) (>= (length font-info) 9))
              (let* ((font-height (aref font-info 3))
                     (font-ascent (aref font-info 8))
                     (height
                      (if (eq face 'default)
                          (window-default-font-height window)
                        font-height))
                     (ascent
                      (round
                       (* height
                          (/ (float font-ascent) font-height)))))
                `(space
                  :width (0)
                  :height (,(+ height pixels))
                  :ascent (,(+ ascent (if top-p pixels 0)))))
            '(space :width (0)))))
    (propertize
     " "
     'face face
     'display display
     'tessera-element element)))

(defun tessera-ui--header-vertical-padding
    (element pixels top-p window)
  "Return header padding named ELEMENT.

PIXELS, TOP-P, and WINDOW are passed to
`tessera-ui--vertical-padding'."
  (tessera-ui--vertical-padding
   'tessera-header element pixels top-p window))

(defun tessera-ui-entry-space (element)
  "Return one entry space named ELEMENT."
  (propertize " " 'tessera-element element))

(defun tessera-ui-entry-padding (element)
  "Return horizontal entry padding named ELEMENT."
  (propertize
   (make-string tessera-ui--entry-horizontal-padding ?\s)
   'tessera-element element))

(defun tessera-ui-entry-leading-safety-gap ()
  "Return the safety gap before an entry row."
  (propertize
   " "
   'display `(space :width (,tessera-ui--entry-safety-gap))
   'tessera-element 'entry.safety-gap))

(defun tessera-ui-entry-trailing-safety-gap ()
  "Return the marker for the safety gap after an entry row."
  (propertize
   " "
   'display '(space :width (0))
   'tessera-element 'entry.safety-gap))

(defun tessera-ui-entry-top-padding ()
  "Return zero-width padding above an entry."
  (tessera-ui--vertical-padding
   'default
   'entry.top-padding
   tessera-ui--entry-top-padding
   t
   (or (get-buffer-window (current-buffer) t)
       (selected-window))))

(defun tessera-ui-entry-bottom-padding ()
  "Return zero-width padding below an entry."
  (tessera-ui--vertical-padding
   'default
   'entry.bottom-padding
   tessera-ui--entry-bottom-padding
   nil
   (or (get-buffer-window (current-buffer) t)
       (selected-window))))

(defun tessera-ui-truncate-pixels (text width)
  "Truncate TEXT with an ellipsis to at most WIDTH pixels."
  (cond
   ((<= width 0) "")
   ((<= (string-pixel-width text) width) text)
   (t
    (let* ((end (1- (length text)))
           (ellipsis
            (let ((ellipsis (copy-sequence "…")))
              (add-text-properties
               0 1 (text-properties-at end text) ellipsis)
              ellipsis))
           (ellipsis-width
            (string-pixel-width ellipsis))
           (glyphs (string-glyph-split text))
           (result ""))
      (if (> ellipsis-width width)
          ""
        (while (and glyphs
                    (<= (string-pixel-width
                         (concat result (car glyphs) ellipsis))
                        width))
          (setq result (concat result (pop glyphs))))
        (concat result ellipsis))))))

(defun tessera-ui-query (prefix condition)
  "Return a header query made from PREFIX and CONDITION."
  (let ((prefix (copy-sequence prefix))
        (condition (copy-sequence condition)))
    (add-face-text-property
     0 (length prefix) 'tessera-header-query-prefix nil prefix)
    (add-text-properties
     0 (length prefix)
     '(tessera-element header.query.prefix)
     prefix)
    (add-face-text-property
     0 (length condition)
     'tessera-header-query-condition nil condition)
    (add-text-properties
     0 (length condition)
     '(tessera-element header.query.condition)
     condition)
    (concat
     prefix
     (tessera-ui--header-space 'headers.separator)
     condition)))

(defun tessera-ui--statistics-unread (count)
  "Return COUNT as a styled unread statistic."
  (let ((text (number-to-string count)))
    (add-text-properties
     0 (length text)
     '(tessera-element header.statistics.unread-count)
     text)
    (when (> count 0)
      (add-face-text-property
       0 (length text) 'tessera-header-statistics-unread nil text))
    text))

(defun tessera-ui--statistics-visible (count)
  "Return COUNT as a styled visible statistic."
  (let ((text (number-to-string count)))
    (add-text-properties
     0 (length text)
     '(face tessera-header-statistics-visible
            tessera-element header.statistics.visible-count)
     text)
    text))

(defun tessera-ui--statistics-text (text element)
  "Return statistics TEXT named ELEMENT."
  (propertize text 'tessera-element element))

(defun tessera-ui--statistics-separator (text)
  "Return statistics separator TEXT."
  (tessera-ui--statistics-text text 'header.statistics.separator))

(defun tessera-ui-statistics (unread visible total)
  "Return structured statistics for UNREAD, VISIBLE, and TOTAL."
  (concat
   (tessera-ui--statistics-unread unread)
   (tessera-ui--statistics-separator " ")
   (tessera-ui--statistics-text
    "unread" 'header.statistics.unread-text)
   (tessera-ui--statistics-separator " · ")
   (tessera-ui--statistics-visible visible)
   (tessera-ui--statistics-separator " ")
   (tessera-ui--statistics-text
    "visible" 'header.statistics.visible-text)
   (tessera-ui--statistics-separator " · ")
   (tessera-ui--statistics-text
    (number-to-string total)
    'header.statistics.total-count)
   (tessera-ui--statistics-separator " ")
   (tessera-ui--statistics-text
    "total" 'header.statistics.total-text)))

(defun tessera-ui-entry-flex-gap (right)
  "Return a pixel-aligned gap before RIGHT.

RIGHT is the complete text that follows the gap.  Reserve the trailing
entry safety gap at the window edge."
  (propertize
   " "
   'display
   `(space
     :align-to
     (- right
        (+ (,(string-pixel-width right))
           (,tessera-ui--entry-safety-gap))))
   'tessera-element 'entry.flex-gap))

(defun tessera-ui-header-line (status query statistics)
  "Return a header line containing STATUS, QUERY, and STATISTICS.

Inset the content from the window edges, truncate QUERY to the
available pixel width, and right-align STATISTICS."
  (let* ((window (selected-window))
         (top-padding
          (tessera-ui--header-vertical-padding
           'header.top-padding
           tessera-ui--header-top-padding
           t
           window))
         (left-padding
          (tessera-ui--header-space 'header.left-padding))
         (status (copy-sequence status))
         (separator
          (tessera-ui--header-space 'headers.separator))
         (query (copy-sequence query))
         (statistics (copy-sequence statistics))
         (right-padding
          (tessera-ui--header-space 'header.right-padding))
         (bottom-padding
          (tessera-ui--header-vertical-padding
           'header.bottom-padding
           tessera-ui--header-bottom-padding
           nil
           window)))
    (add-face-text-property
     0 (length status) 'tessera-header-status t status)
    (add-text-properties
     0 (length status)
     '(tessera-element header.status) status)
    (add-face-text-property
     0 (length query) 'tessera-header-query t query)
    (add-face-text-property
     0 (length statistics)
     'tessera-header-statistics t statistics)
    (add-text-properties
     0 (length statistics)
     '(tessera-parent-element header.statistics)
     statistics)
    (let* ((fixed-width
            (string-pixel-width
             (concat
              left-padding status separator
              statistics right-padding)))
           (query-width
            (max 0 (- (window-body-width window t) fixed-width)))
           (full-query (substring-no-properties query))
           (query (tessera-ui-truncate-pixels query query-width)))
      (add-text-properties
       0 (length query)
       (list
        'tessera-parent-element 'header.query
        'help-echo full-query)
       query)
      (let* ((parts
              (list
               top-padding
               left-padding
               status
               separator
               query
               'mode-line-format-right-align
               statistics
               right-padding
               bottom-padding))
             (mode-line-format parts)
             (mode-line-right-align-edge 'right-fringe)
             (line
              (format-mode-line
               parts nil window (current-buffer))))
        (dotimes (position (length line))
          (let ((display (get-text-property position 'display line)))
            (when (and (consp display) (memq :align-to display))
              (put-text-property
               position (1+ position)
               'tessera-element 'header.flex-gap
               line))))
        line))))

(provide 'tessera-ui)
;;; tessera-ui.el ends here
