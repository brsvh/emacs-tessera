;;; tessera-elfeed-search.el --- Tessera UI for Elfeed search  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bingshan Chang <chang@bingshan.org>

;; Author: Bingshan Chang <chang@bingshan.org>
;; Maintainer: Bingshan Chang <chang@bingshan.org>
;; Keywords: convenience, news

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

;; This module renders `elfeed-search-mode' entries with Tessera.
;; `tessera-elfeed-mode' manages its activation.

;;; Code:

(require 'subr-x)
(require 'tessera)

(declare-function elfeed-add-properties "elfeed-lib")
(declare-function elfeed-entry-date "elfeed-db")
(declare-function elfeed-entry-enclosures "elfeed-db")
(declare-function elfeed-entry-feed "elfeed-db")
(declare-function elfeed-entry-link "elfeed-db")
(declare-function elfeed-entry-tags "elfeed-db")
(declare-function elfeed-meta--title "elfeed-db")
(declare-function elfeed-search--faces "elfeed-search")
(declare-function elfeed-search-format-date "elfeed-search")
(declare-function elfeed-search-update "elfeed-search")

(defvar elfeed-search-print-entry-function)
(defvar elfeed-search-update-hook)
(defvar elfeed-search-separator-date-format)

(defun tessera-elfeed-search--set-glyph (symbol value)
  "Set glyph option SYMBOL to VALUE and refresh active buffers."
  (set-default symbol value)
  (when (and (gethash 'elfeed-search tessera--entry-backends)
             (fboundp 'tessera-elfeed-search--register)
             (fboundp 'tessera-elfeed-search--refresh-active-buffers))
    (tessera-elfeed-search--register)
    (tessera-elfeed-search--refresh-active-buffers)))

(defcustom tessera-elfeed-search-unread-glyph
  '("*" "●" nerd-icons-mdicon "nf-md-email" accent)
  "Glyph definition used for unread Elfeed entries.

The value contains the ASCII text, Unicode text, Nerd Icons function,
Nerd Icons name, and semantic role, in that order."
  :type '(list
          (string :tag "ASCII")
          (string :tag "Unicode")
          (symbol :tag "Nerd Icons function")
          (string :tag "Nerd Icons name")
          (symbol :tag "Semantic role"))
  :set #'tessera-elfeed-search--set-glyph
  :group 'tessera-elfeed)

(defcustom tessera-elfeed-search-read-glyph
  '("o" "○" nerd-icons-mdicon "nf-md-email_open_outline" muted)
  "Glyph definition used for read Elfeed entries.

The value has the same shape as
`tessera-elfeed-search-unread-glyph'."
  :type '(list
          (string :tag "ASCII")
          (string :tag "Unicode")
          (symbol :tag "Nerd Icons function")
          (string :tag "Nerd Icons name")
          (symbol :tag "Semantic role"))
  :set #'tessera-elfeed-search--set-glyph
  :group 'tessera-elfeed)

(defcustom tessera-elfeed-search-enclosure-glyph
  '("@" "📎" nerd-icons-mdicon "nf-md-paperclip" informational)
  "Glyph definition used for entries with enclosures.

The value has the same shape as
`tessera-elfeed-search-unread-glyph'."
  :type '(list
          (string :tag "ASCII")
          (string :tag "Unicode")
          (symbol :tag "Nerd Icons function")
          (string :tag "Nerd Icons name")
          (symbol :tag "Semantic role"))
  :set #'tessera-elfeed-search--set-glyph
  :group 'tessera-elfeed)

(defface tessera-elfeed-search-title-face
  '((t :inherit elfeed-search-title-face))
  "Face used for entry titles in Elfeed search buffers."
  :group 'tessera-elfeed)

(defface tessera-elfeed-search-unread-title-face
  '((t :inherit elfeed-search-unread-title-face))
  "Face used for unread entry titles in Elfeed search buffers."
  :group 'tessera-elfeed)

(defface tessera-elfeed-search-feed-face
  '((t :inherit elfeed-search-feed-face))
  "Face used for feed titles in Elfeed search buffers."
  :group 'tessera-elfeed)

(defface tessera-elfeed-search-url-face
  '((t :inherit (elfeed-search-date-face link)
       :slant italic))
  "Face used for entry URLs in Elfeed search buffers."
  :group 'tessera-elfeed)

(defface tessera-elfeed-search-tag-face
  '((t :inherit elfeed-search-tag-face))
  "Face used for entry tags in Elfeed search buffers."
  :group 'tessera-elfeed)

(defface tessera-elfeed-search-date-face
  '((t :inherit elfeed-search-date-face))
  "Face used for entry dates in Elfeed search buffers."
  :group 'tessera-elfeed)

(defvar-local tessera-elfeed-search--active nil
  "Non-nil when Tessera renders the current Elfeed search buffer.")

(defvar-local tessera-elfeed-search--saved-printer nil
  "Printer saved before enabling Tessera in this search buffer.")

(defvar-local tessera-elfeed-search--saved-printer-local-p nil
  "Whether the saved Elfeed printer was buffer-local.")

(defvar-local tessera-elfeed-search--saved-layout nil
  "Entry layout saved before enabling Tessera in this search buffer.")

(defvar-local tessera-elfeed-search--saved-layout-local-p nil
  "Whether the saved entry layout was buffer-local.")

(defvar-local tessera-elfeed-search--saved-separator-format nil
  "Date separator format saved before enabling Tessera.")

(defvar-local tessera-elfeed-search--saved-separator-format-local-p
    nil
  "Whether the saved date separator format was buffer-local.")

(defun tessera-elfeed-search--entry (context)
  "Return the Elfeed entry stored in CONTEXT."
  (tessera-entry-context-object context))

(defun tessera-elfeed-search--context (entry buffer window)
  "Build a Tessera context for Elfeed ENTRY in BUFFER and WINDOW."
  (make-tessera-entry-context
   :backend 'elfeed-search
   :object entry
   :buffer buffer
   :window window))

(defun tessera-elfeed-search--make-glyph (definition)
  "Build a Tessera glyph from Elfeed glyph DEFINITION."
  (pcase-let ((`(,ascii ,unicode ,function ,name ,semantic)
               definition))
    (make-tessera-glyph
     :ascii ascii
     :unicode unicode
     :nerd-icons `(:function ,function :name ,name)
     :semantic semantic)))

(defun tessera-elfeed-search--select-status (context)
  "Return the status glyph variant for CONTEXT."
  (if (memq 'unread
            (elfeed-entry-tags
             (tessera-elfeed-search--entry context)))
      'unread
    'read))

(defun tessera-elfeed-search--status-slot ()
  "Return the status glyph slot for Elfeed entries."
  (make-tessera-glyph-slot
   :name 'status
   :selector #'tessera-elfeed-search--select-status
   :width 1
   :align 'center
   :glyphs
   `((unread
      :glyph ,(tessera-elfeed-search--make-glyph
               tessera-elfeed-search-unread-glyph)
      :help-echo "Unread")
     (read
      :glyph ,(tessera-elfeed-search--make-glyph
               tessera-elfeed-search-read-glyph)
      :help-echo "Read"))))

(defun tessera-elfeed-search--title-faces (tags)
  "Return Tessera title faces corresponding to Elfeed TAGS."
  (mapcar
   (lambda (face)
     (pcase face
       ('elfeed-search-title-face
        'tessera-elfeed-search-title-face)
       ('elfeed-search-unread-title-face
        'tessera-elfeed-search-unread-title-face)
       (_ face)))
   (elfeed-search--faces tags)))

(defun tessera-elfeed-search--title (context)
  "Return the interactive title segment for CONTEXT."
  (let* ((entry (tessera-elfeed-search--entry context))
         (title (elfeed-meta--title entry)))
    (elfeed-add-properties
     (or (and title (not (string-empty-p title)) title)
         (elfeed-entry-link entry))
     'face (tessera-elfeed-search--title-faces
            (elfeed-entry-tags entry))
     'mouse-face 'highlight
     'follow-link [elfeed-entry])))

(defun tessera-elfeed-search--feed (context)
  "Return the interactive feed segment for CONTEXT."
  (when-let* ((entry (tessera-elfeed-search--entry context))
              (feed (elfeed-entry-feed entry))
              (title (elfeed-meta--title feed)))
    (propertize title
                'face 'tessera-elfeed-search-feed-face
                'mouse-face 'highlight
                'follow-link [elfeed-feed])))

(defun tessera-elfeed-search--url (context)
  "Return the entry URL segment for CONTEXT."
  (when-let* ((url
               (elfeed-entry-link
                (tessera-elfeed-search--entry context))))
    (propertize url
                'face 'tessera-elfeed-search-url-face
                'mouse-face 'highlight
                'follow-link [elfeed-entry])))

(defun tessera-elfeed-search--enclosure-help (enclosures)
  "Return help text describing ENCLOSURES."
  (let ((types
         (delete-dups
          (delq nil
                (mapcar (lambda (enclosure)
                          (nth 1 enclosure))
                        enclosures)))))
    (if types
        (format "Enclosure: %s" (string-join types ", "))
      "Has enclosure")))

(defun tessera-elfeed-search--enclosure (context)
  "Return an enclosure indicator for CONTEXT."
  (when-let* ((enclosures
               (elfeed-entry-enclosures
                (tessera-elfeed-search--entry context))))
    (tessera-glyph-render
     (tessera-elfeed-search--make-glyph
      tessera-elfeed-search-enclosure-glyph)
     context
     (list :help-echo
           (tessera-elfeed-search--enclosure-help enclosures)))))

(defun tessera-elfeed-search--tags (context)
  "Return the interactive textual tag segment for CONTEXT."
  (when-let* ((tags
               (delq 'unread
                     (copy-sequence
                      (elfeed-entry-tags
                       (tessera-elfeed-search--entry context))))))
    (concat
     "("
     (mapconcat
      (lambda (tag)
        (propertize (symbol-name tag)
                    'face 'tessera-elfeed-search-tag-face
                    'mouse-face 'highlight
                    'elfeed-tag tag
                    'follow-link [elfeed-tag]))
      tags
      ",")
     ")")))

(defun tessera-elfeed-search--date (context)
  "Return the interactive date segment for CONTEXT."
  (elfeed-add-properties
   (elfeed-search-format-date
    (elfeed-entry-date
     (tessera-elfeed-search--entry context)))
   'face 'tessera-elfeed-search-date-face
   'mouse-face 'highlight
   'follow-link [elfeed-date]))

(defun tessera-elfeed-search--single-line-layout ()
  "Return the single-line layout for Elfeed search entries."
  (make-tessera-entry-layout
   :main-glyph-slots '(status)
   :main-left-segments
   '((title :grow t :min-width 4 :truncate tail))
   :main-right-segments '(date)))

(defun tessera-elfeed-search--two-line-layout ()
  "Return the two-line layout for Elfeed search entries."
  (make-tessera-entry-layout
   :main-glyph-slots '(status)
   :main-left-segments
   '((title :grow t :min-width 4 :truncate tail))
   :main-right-segments '(feed)
   :extra-glyph-slots '((status :reserve t))
   :extra-left-segments
   '((url :grow t :min-width 8 :truncate tail :priority 0)
     (enclosure :optional t :priority 0))
   :extra-right-segments
   '((tags :grow t :min-width 4 :truncate tail :priority 10)
     date)))

(defun tessera-elfeed-search--register ()
  "Register the Elfeed search adapter with Tessera."
  (tessera-entry-register
   'elfeed-search
   :context #'tessera-elfeed-search--context
   :segments
   '((title . tessera-elfeed-search--title)
     (feed . tessera-elfeed-search--feed)
     (url . tessera-elfeed-search--url)
     (enclosure . tessera-elfeed-search--enclosure)
     (tags . tessera-elfeed-search--tags)
     (date . tessera-elfeed-search--date))
   :glyph-slots (list (tessera-elfeed-search--status-slot))
   :layouts
   `((single-line
      . ,(tessera-elfeed-search--single-line-layout))
     (two-line
      . ,(tessera-elfeed-search--two-line-layout)))))

(defun tessera-elfeed-search-print-entry (entry)
  "Insert a Tessera rendering of Elfeed ENTRY."
  (let ((start (point)))
    ;; A single-entry update retains the native terminating newline.
    (tessera-entry-clear-layout start (min (point-max) (1+ start)))
    (insert
     (tessera-entry-render
      'elfeed-search entry (get-buffer-window (current-buffer))))
    (when (eq (char-after) ?\n)
      (tessera-entry-apply-layout start (point)))))

(defun tessera-elfeed-search--refresh ()
  "Refresh the current Elfeed search buffer."
  (when (derived-mode-p 'elfeed-search-mode)
    (elfeed-search-update :force)))

(defun tessera-elfeed-search--style-separators ()
  "Keep native separator strings from inheriting an icon font."
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (when (and (eq (overlay-get overlay 'category)
                   'elfeed-search-separator)
               (not (overlay-get overlay
                                 'tessera-elfeed-search-separator)))
      (when-let* ((original (overlay-get overlay 'before-string)))
        (let ((string (copy-sequence original)))
          (add-face-text-property 0 (length string) 'default t string)
          (overlay-put overlay 'tessera-elfeed-search-separator
                       original)
          (overlay-put overlay 'before-string string))))))

(defun tessera-elfeed-search--restore-separators ()
  "Restore native separator strings when disabling the adapter."
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (when-let* ((original
                 (overlay-get overlay
                              'tessera-elfeed-search-separator)))
      (overlay-put overlay 'before-string original)
      (overlay-put overlay 'tessera-elfeed-search-separator nil))))

(defun tessera-elfeed-search--apply-layout ()
  "Attach layouts after Elfeed has inserted entry terminators."
  (when tessera-elfeed-search--active
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (while (< (point) (point-max))
          (tessera-entry-apply-layout (point) (line-end-position))
          (forward-line 1))))
    (tessera-elfeed-search--style-separators)
    (tessera-entry-highlight-current)))

(defun tessera-elfeed-search--refresh-active-buffers ()
  "Refresh live Elfeed search buffers using Tessera."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and tessera-elfeed-search--active
                 (derived-mode-p 'elfeed-search-mode))
        (tessera-elfeed-search--refresh)))))

(defun tessera-elfeed-search--enable ()
  "Enable Tessera rendering in the current Elfeed search buffer."
  (unless tessera-elfeed-search--active
    (setq tessera-elfeed-search--saved-printer-local-p
          (local-variable-p 'elfeed-search-print-entry-function)
          tessera-elfeed-search--saved-printer
          elfeed-search-print-entry-function
          tessera-elfeed-search--saved-layout-local-p
          (local-variable-p 'tessera-entry-layout)
          tessera-elfeed-search--saved-layout
          tessera-entry-layout
          tessera-elfeed-search--saved-separator-format-local-p
          (local-variable-p 'elfeed-search-separator-date-format)
          tessera-elfeed-search--saved-separator-format
          elfeed-search-separator-date-format)
    (setq-local elfeed-search-print-entry-function
                #'tessera-elfeed-search-print-entry)
    (setq-local tessera-entry-layout 'two-line)
    (setq-local elfeed-search-separator-date-format nil)
    (add-hook 'elfeed-search-update-hook
              #'tessera-elfeed-search--apply-layout t t)
    (add-hook 'post-command-hook
              #'tessera-entry-highlight-current nil t)
    (setq tessera-elfeed-search--active t)
    (tessera-elfeed-search--refresh)))

(defun tessera-elfeed-search--disable ()
  "Disable Tessera rendering in the current Elfeed search buffer."
  (when tessera-elfeed-search--active
    (if tessera-elfeed-search--saved-printer-local-p
        (setq-local elfeed-search-print-entry-function
                    tessera-elfeed-search--saved-printer)
      (kill-local-variable 'elfeed-search-print-entry-function))
    (if tessera-elfeed-search--saved-layout-local-p
        (setq-local tessera-entry-layout
                    tessera-elfeed-search--saved-layout)
      (kill-local-variable 'tessera-entry-layout))
    (if tessera-elfeed-search--saved-separator-format-local-p
        (setq-local elfeed-search-separator-date-format
                    tessera-elfeed-search--saved-separator-format)
      (kill-local-variable 'elfeed-search-separator-date-format))
    (remove-hook 'elfeed-search-update-hook
                 #'tessera-elfeed-search--apply-layout t)
    (remove-hook 'post-command-hook
                 #'tessera-entry-highlight-current t)
    (tessera-entry-clear-current)
    (tessera-entry-clear-layout)
    (tessera-elfeed-search--restore-separators)
    (setq tessera-elfeed-search--active nil
          tessera-elfeed-search--saved-printer nil
          tessera-elfeed-search--saved-printer-local-p nil
          tessera-elfeed-search--saved-layout nil
          tessera-elfeed-search--saved-layout-local-p nil
          tessera-elfeed-search--saved-separator-format nil
          tessera-elfeed-search--saved-separator-format-local-p nil)
    (tessera-elfeed-search--refresh)))

(provide 'tessera-elfeed-search)
;;; tessera-elfeed-search.el ends here
