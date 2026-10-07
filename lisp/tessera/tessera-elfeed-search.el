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
;; Options and faces live here; `tessera-elfeed' manages activation.

;;; Code:

(require 'subr-x)
(require 'tessera-elfeed)

(declare-function elfeed-add-properties "elfeed-lib")
(declare-function elfeed--with-position-f "elfeed-lib")
(declare-function elfeed--position-save "elfeed-lib")
(declare-function elfeed--position-restore "elfeed-lib")
(declare-function elfeed-entry-date "elfeed-db")
(declare-function elfeed-entry-enclosures "elfeed-db")
(declare-function elfeed-entry-feed "elfeed-db")
(declare-function elfeed-entry-link "elfeed-db")
(declare-function elfeed-entry-tags "elfeed-db")
(declare-function elfeed-meta--title "elfeed-db")
(declare-function elfeed-search--faces "elfeed-search")
(declare-function elfeed-search-format-date "elfeed-search")
(declare-function elfeed-search-update "elfeed-search")
(declare-function elfeed-search--update-immediately "elfeed-search")

(defvar elfeed-search-print-entry-function)
(defvar elfeed-search-update-hook)
(defvar elfeed-search-separator-date-format)

(declare-function elfeed-update "elfeed")
(declare-function elfeed-queue-count-total "elfeed")
(declare-function elfeed-entry-id "elfeed-db")
(declare-function elfeed-entry-feed-id "elfeed-db")
(defvar elfeed-search-entries)
(defvar elfeed-search-filter)
(defvar elfeed-fetch-functions)
(defvar elfeed-parse-error-hook)

(defgroup tessera-elfeed-search nil
  "Tessera entries in Elfeed search buffers."
  :group 'tessera-elfeed
  :prefix "tessera-elfeed-search-")

;;;; Month options

(defcustom tessera-elfeed-search-month-grouping 'inherit
  "Whether Elfeed search buffers use month grouping.
The value `inherit' follows `tessera-month-grouping'."
  :type '(choice
          (const :tag "Inherit global setting" inherit)
          (const :tag "Enabled" t)
          (const :tag "Disabled" nil))
  :initialize #'custom-initialize-default
  :set #'tessera--set-month-option
  :group 'tessera-elfeed-search)

;;;; Glyph options

(defvar tessera-elfeed-search--glyph-defaults
  '((status-unread
     :ascii "*"
     :unicode "●"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-email")
     :face tessera-glyph-accent-face)
    (status-read
     :ascii "o"
     :unicode "○"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-email_open_outline")
     :face tessera-glyph-muted-face)
    (enclosure
     :ascii "@"
     :unicode "📎"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-paperclip")
     :face tessera-glyph-informational-face))
  "Default glyph representations and shared semantic faces.")

(defun tessera-elfeed-search--set-glyphs
    (symbol value)
  "Set glyph option SYMBOL to validated VALUE and refresh views."
  (tessera--set-glyphs
   symbol value tessera-elfeed-search--glyph-defaults))

(defcustom tessera-elfeed-search-glyphs nil
  "Overrides for the named fields of this view's glyphs.
Each alist entry maps a glyph ID to a property list.  Missing fields
keep their defaults.  Use :ascii, :unicode, :nerd-icons, :face, and
:hidden; see `tessera-glyph-resolve'.  Customize and `setopt' redraw
active views.  After `setq', call `tessera-refresh-glyphs'."
  :type (tessera--glyph-custom-type
         tessera-elfeed-search--glyph-defaults)
  :initialize #'custom-initialize-default
  :set #'tessera-elfeed-search--set-glyphs
  :group 'tessera-elfeed-search)

;;;; Header line options

(defcustom tessera-elfeed-search-header-line-action-function
  #'tessera-elfeed-search-header-line-action
  "Function rendering the action header region, or nil to hide it.
The function receives a `tessera-header-line-context' and returns
single-line text with optional face, help and keymap properties."
  :type '(choice (const nil) function)
  :initialize #'custom-initialize-default
  :set #'tessera--set-header-line-option
  :group 'tessera-elfeed-search)

(defcustom tessera-elfeed-search-header-line-info-function
  #'tessera-elfeed-search-header-line-info
  "Function rendering the info header region, or nil to hide it.
The function receives a `tessera-header-line-context' and returns
single-line text with optional face, help and keymap properties."
  :type '(choice (const nil) function)
  :initialize #'custom-initialize-default
  :set #'tessera--set-header-line-option
  :group 'tessera-elfeed-search)

(defcustom tessera-elfeed-search-header-line-extra-function
  nil
  "Function rendering the extra header region, or nil to hide it.
The function receives a `tessera-header-line-context' and returns
single-line text with optional face, help and keymap properties."
  :type '(choice (const nil) function)
  :initialize #'custom-initialize-default
  :set #'tessera--set-header-line-option
  :group 'tessera-elfeed-search)

(defcustom tessera-elfeed-search-header-line-statistics-function
  #'tessera-header-line-statistics
  "Function rendering the statistics header region, or nil to hide it.
The function receives a `tessera-header-line-context' and returns
single-line text with optional face, help and keymap properties."
  :type '(choice (const nil) function)
  :initialize #'custom-initialize-default
  :set #'tessera--set-header-line-option
  :group 'tessera-elfeed-search)

(defcustom tessera-elfeed-search-header-line-next-update-function
  nil
  "Function returning the next update time, or nil when unknown.
The function receives a `tessera-header-line-context'.  It must
return an absolute Emacs time value or nil, without scheduling work."
  :type '(choice (const nil) function)
  :group 'tessera-elfeed-search)

;;;; Faces

(defface tessera-elfeed-search-title-face
  '((t :inherit elfeed-search-title-face))
  "Face used for entry titles in Elfeed search buffers."
  :group 'tessera-elfeed-search)

(defface tessera-elfeed-search-unread-title-face
  '((t :inherit elfeed-search-unread-title-face))
  "Face used for unread entry titles in Elfeed search buffers."
  :group 'tessera-elfeed-search)

(defface tessera-elfeed-search-feed-face
  '((t :inherit elfeed-search-feed-face
       :weight normal
       :slant italic
       :extend nil))
  "Face used for feed titles of read entries."
  :group 'tessera-elfeed-search)

(defface tessera-elfeed-search-unread-feed-face
  '((t :inherit (bold tessera-elfeed-search-feed-face)
       :extend nil))
  "Face used for feed titles of unread entries."
  :group 'tessera-elfeed-search)

(defface tessera-elfeed-search-tag-face
  '((t :inherit elfeed-search-tag-face))
  "Face used for entry tags in Elfeed search buffers."
  :group 'tessera-elfeed-search)

(defface tessera-elfeed-search-date-face
  '((t :inherit (elfeed-search-title-face elfeed-search-date-face)
       :weight normal
       :slant normal
       :extend nil))
  "Face used for read dates, with the native title color."
  :group 'tessera-elfeed-search)

(defface tessera-elfeed-search-unread-date-face
  '((t :inherit (bold elfeed-search-unread-title-face
                      tessera-elfeed-search-date-face)
       :slant normal
       :extend nil))
  "Face used for unread dates, with the native unread title color."
  :group 'tessera-elfeed-search)

(defface tessera-elfeed-search-url-face
  '((t :inherit (tessera-elfeed-search-date-face link)
       :slant italic
       :underline nil
       :extend nil))
  "Face used for read URLs, with the read date color."
  :group 'tessera-elfeed-search)

(defface tessera-elfeed-search-unread-url-face
  '((t :inherit (tessera-elfeed-search-unread-date-face
                 tessera-elfeed-search-url-face)
       :slant italic
       :underline nil
       :extend nil))
  "Face used for unread URLs, with unread date color and weight."
  :group 'tessera-elfeed-search)

;;;; Buffer state

(defvar-local tessera-elfeed-search--active nil
  "Non-nil when Tessera renders the current Elfeed search buffer.")

(defvar-local tessera-elfeed-search--months-dirty nil
  "Pending month work: nil, `display', or t for a full rebuild.")

(defvar-local tessera-elfeed-search--face-remapping nil
  "Font remapping used for the last complete entry rendering.")

(defvar-local tessera-elfeed-search--saved-settings nil
  "Original values and locality of settings replaced by Tessera.")

(defvar-local tessera-elfeed-search--emulation-map-alist nil
  "Buffer-local emulation map alist for Elfeed navigation.")

(defvar tessera-elfeed-search--navigation-users 0
  "Number of active Elfeed search buffers using navigation.")

(defvar tessera-elfeed-search--navigation-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'tessera-elfeed-search--next)
    (define-key map (kbd "p") #'tessera-elfeed-search--previous)
    map)
  "Internal keymap that adapts Elfeed navigation.")

(defvar-local tessera-elfeed-search--month-separator-hidden nil
  "Non-nil when month grouping hid the native date separator.")

;;;; Entry metadata

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

;;;; Month contexts

(defun tessera-elfeed-search--month-enabled-p ()
  "Return the effective Elfeed month grouping setting."
  (if (eq tessera-elfeed-search-month-grouping 'inherit)
      tessera-month-grouping
    tessera-elfeed-search-month-grouping))

(defun tessera-elfeed-search--native-separator-p ()
  "Return non-nil when the date separator has its native default."
  (when-let* ((standard
               (get 'elfeed-search-separator-date-format
                    'standard-value)))
    (equal elfeed-search-separator-date-format
           (eval (car standard) t))))

(defun tessera-elfeed-search--saved-separator ()
  "Return the date separator saved when Tessera was enabled."
  (nth 2
       (assq 'elfeed-search-separator-date-format
             tessera-elfeed-search--saved-settings)))

(defun tessera-elfeed-search--update-date-separator ()
  "Hide or restore the native default date separator."
  (cond
   ((and (tessera-elfeed-search--month-enabled-p)
         (not tessera-elfeed-search--month-separator-hidden)
         (tessera-elfeed-search--native-separator-p))
    (setq-local elfeed-search-separator-date-format nil)
    (setq tessera-elfeed-search--month-separator-hidden t))
   ((and (not (tessera-elfeed-search--month-enabled-p))
         tessera-elfeed-search--month-separator-hidden)
    (setq-local elfeed-search-separator-date-format
                (tessera-elfeed-search--saved-separator))
    (setq tessera-elfeed-search--month-separator-hidden nil))))

(defun tessera-elfeed-search--month-date (context)
  "Return CONTEXT's native entry date as an Emacs time."
  (let ((date
         (elfeed-entry-date
          (tessera-elfeed-search--entry context))))
    (when (numberp date) (seconds-to-time date))))

(defun tessera-elfeed-search--month-unread-p (context)
  "Return non-nil when CONTEXT contains an unread entry."
  (eq (tessera-elfeed-search--select-status context) 'unread))

(defun tessera-elfeed-search--month-glyph (state _context)
  "Return the configured status glyph for STATE."
  (tessera-glyph-resolve
   (if (eq state 'unread) 'status-unread 'status-read)
   tessera-elfeed-search--glyph-defaults
   tessera-elfeed-search-glyphs))

(defun tessera-elfeed-search--month-warning-segment (_context)
  "Return the segment that carries an Elfeed date warning."
  'title)

(defun tessera-elfeed-search--month-goto (context)
  "Move point to the rendered Elfeed entry in CONTEXT."
  (let ((object (tessera-entry-context-object context))
        found)
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (and (not found) (< (point) (point-max)))
        (let ((candidate
               (get-text-property
                (point) 'tessera-entry-context)))
          (if (and candidate
                   (eq object
                       (tessera-entry-context-object candidate)))
              (setq found t)
            (forward-line 1)))))
    (when-let* ((preferred (and found (tessera-entry-point))))
      (goto-char preferred))
    found))

;;;; Fields and glyphs

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
   ;; Normal-size Nerd Icons need more than one text column.
   :width 2
   :align 'center
   :glyphs
   `((unread
      :glyph ,(tessera-glyph-resolve
               'status-unread tessera-elfeed-search--glyph-defaults
               tessera-elfeed-search-glyphs)
      :help-echo "Unread")
     (read
      :glyph ,(tessera-glyph-resolve
               'status-read tessera-elfeed-search--glyph-defaults
               tessera-elfeed-search-glyphs)
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
    (propertize
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
                'face
                (if (eq (tessera-elfeed-search--select-status context)
                        'unread)
                    'tessera-elfeed-search-unread-feed-face
                  'tessera-elfeed-search-feed-face)
                'mouse-face 'highlight
                'follow-link [elfeed-feed])))

(defun tessera-elfeed-search--url (context)
  "Return the entry URL segment for CONTEXT."
  (when-let* ((url
               (elfeed-entry-link
                (tessera-elfeed-search--entry context))))
    (propertize url
                'face
                (if (eq (tessera-elfeed-search--select-status context)
                        'unread)
                    'tessera-elfeed-search-unread-url-face
                  'tessera-elfeed-search-url-face)
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
                (tessera-elfeed-search--entry context)))
              (glyph
               (tessera-glyph-resolve
                'enclosure tessera-elfeed-search--glyph-defaults
                tessera-elfeed-search-glyphs))
              ((not (tessera-glyph-hidden glyph))))
    (tessera-glyph-render
     glyph context
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
     (propertize "(" 'face 'default 'mouse-face 'default)
     (mapconcat
      (lambda (tag)
        (propertize (symbol-name tag)
                    'face 'tessera-elfeed-search-tag-face
                    'mouse-face 'highlight
                    'elfeed-tag tag
                    'follow-link [elfeed-tag]))
      tags
      (propertize "," 'face 'default 'mouse-face 'default))
     (propertize ")" 'face 'default 'mouse-face 'default))))

(defun tessera-elfeed-search--date (context)
  "Return the interactive date segment for CONTEXT."
  (let ((date (tessera--valid-time
               (elfeed-entry-date
                (tessera-elfeed-search--entry context)))))
    (elfeed-add-properties
     (if date (elfeed-search-format-date date) "Unknown")
     'face (if (eq (tessera-elfeed-search--select-status context)
                   'unread)
               'tessera-elfeed-search-unread-date-face
             'tessera-elfeed-search-date-face)
     'mouse-face (and date 'highlight)
     'follow-link (and date [elfeed-date]))))

;;;; Layout registration

(defun tessera-elfeed-search--single-line-layout ()
  "Return the single-line layout for Elfeed search entries."
  (make-tessera-entry-layout
   :main-glyph-slots '(status)
   :main-left-segments
   '((title :grow t :min-width 4 :truncate tail :point t))
   :main-right-segments '(date)))

(defun tessera-elfeed-search--two-line-layout ()
  "Return the two-line layout for Elfeed search entries."
  (make-tessera-entry-layout
   :main-glyph-slots '(status)
   :main-left-segments
   '((title :grow t :min-width 4 :truncate tail :point t))
   :main-right-segments
   '((feed :grow t
           :min-width 4
           :truncate tail
           :priority 10))
   :extra-glyph-slots '((status :reserve t))
   :extra-left-segments
   '((url :grow t :min-width 8 :truncate tail :priority 0)
     (enclosure :optional t :priority 0))
   :extra-right-segments
   '((tags :grow t :min-width 4 :truncate tail :priority 10)
     date)))

(defun tessera-elfeed-search--register ()
  "Register the Elfeed search adapter with Tessera."
  (tessera--validate-glyph-overrides
   tessera-elfeed-search--glyph-defaults
   tessera-elfeed-search-glyphs 2)
  (tessera-entry-register
   'elfeed-search
   :context #'tessera-elfeed-search--context
   :month-date #'tessera-elfeed-search--month-date
   :month-unread-p #'tessera-elfeed-search--month-unread-p
   :month-glyph #'tessera-elfeed-search--month-glyph
   :month-warning-segment
   #'tessera-elfeed-search--month-warning-segment
   :month-goto #'tessera-elfeed-search--month-goto
   :segments
   '((title . tessera-elfeed-search--title)
     (feed . tessera-elfeed-search--feed)
     (url . tessera-elfeed-search--url)
     (enclosure . tessera-elfeed-search--enclosure)
     (tags . tessera-elfeed-search--tags)
     (date . tessera-elfeed-search--date))
   :glyph-slots (list (tessera-elfeed-search--status-slot))
   :layouts
   `((single-line . ,(tessera-elfeed-search--single-line-layout))
     (two-line . ,(tessera-elfeed-search--two-line-layout)))))

;;;; Native row rendering

(defun tessera-elfeed-search-print-entry (entry)
  "Insert a Tessera rendering of Elfeed ENTRY."
  (let ((start (point))
        (single (eq (char-after) ?\n))
        (groups (unless (eq tessera-elfeed-search--months-dirty t)
                  tessera--month-groups)))
    ;; Restore suppressed decorations before replacing any overlays.
    ;; A failed render must also invalidate cached entry bounds.
    (unless (eq tessera-elfeed-search--months-dirty t)
      (tessera--month-clear-display))
    (setq tessera-elfeed-search--months-dirty t)
    (let ((rendered
           (tessera-entry-render
            'elfeed-search entry
            (get-buffer-window (current-buffer)))))
      ;; A single-entry update retains the native terminating newline.
      (tessera-entry-clear-layout start (min (point-max) (1+ start)))
      (insert rendered)
      (when (eq (char-after) ?\n)
        (tessera-entry-apply-layout start (point)))
      (when (and groups single
                 (tessera-elfeed-search--month-enabled-p)
                 (let ((tessera--month-groups groups))
                   (tessera--month-update-entry start rendered)))
        (setq tessera--month-groups groups
              tessera-elfeed-search--months-dirty 'display)))))

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

;;;; Content synchronization

(defun tessera-elfeed-search--apply-layout ()
  "Attach layouts after Elfeed has inserted entry terminators."
  (when tessera-elfeed-search--active
    (tessera--header-line-changed)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (while (< (point) (point-max))
          (tessera-entry-apply-layout (point) (line-end-position))
          (forward-line 1))))
    (tessera-elfeed-search--style-separators)
    (setq tessera-elfeed-search--months-dirty t
          tessera-elfeed-search--face-remapping
          (copy-tree face-remapping-alist))
    (tessera-elfeed-search--sync-months)
    (tessera-entry-highlight-current)))

(defun tessera-elfeed-search--sync-months (&optional _window)
  "Synchronize changed month metadata before display or navigation."
  (when (and tessera-elfeed-search--active
             tessera-elfeed-search--months-dirty)
    (if (eq tessera-elfeed-search--months-dirty 'display)
        (tessera--month-redisplay)
      (tessera-month-configure
       (tessera-elfeed-search--month-enabled-p) 'latest))
    (setq tessera-elfeed-search--months-dirty nil)))

(defun tessera-elfeed-search--call-widened (function &rest args)
  "Call FUNCTION with ARGS over all entries, preserving narrowing.
Save restriction boundaries by entry and column because native
redraws delete the text that anchors restriction markers."
  (let ((bounds
         (when (buffer-narrowed-p)
           (let ((positions (list (point-min) (point-max))))
             (save-excursion
               (save-restriction
                 (widen)
                 (mapcar
                  (lambda (position)
                    (goto-char position)
                    (elfeed--position-save 'elfeed-entry))
                  positions)))))))
    (unwind-protect
        (progn
          (widen)
          (apply function args))
      (when bounds
        (save-excursion
          (widen)
          (let ((positions
                 (mapcar
                  (lambda (position)
                    (elfeed--position-restore 'elfeed-entry position)
                    (point))
                  bounds)))
            (narrow-to-region (car positions) (cadr positions))))))))

(defun tessera-elfeed-search--prepare (&optional _window)
  "Redraw changed fonts and synchronize months before display."
  (when tessera-elfeed-search--active
    (unless (equal face-remapping-alist
                   tessera-elfeed-search--face-remapping)
      (tessera-elfeed-search--call-widened
       #'elfeed-search--update-immediately (current-buffer) :resize))
    (tessera-elfeed-search--sync-months)))

(defun tessera-elfeed-search--update-entries (function &rest entries)
  "Call native update FUNCTION for ENTRIES and restore display state.
Update full-buffer rows even under narrowing.  Preserve point and
mark by entry, and synchronize month metadata before navigation."
  (if (not tessera-elfeed-search--active)
      (apply function entries)
    (unwind-protect
        (elfeed--with-position-f
         'elfeed-entry
         (lambda ()
           (apply #'tessera-elfeed-search--call-widened
                  function entries)))
      (tessera--header-line-changed)
      (tessera-elfeed-search--sync-months)
      (tessera-entry-highlight-current))))

(defun tessera-elfeed-search--post-command ()
  "Reveal a hidden target and update the current entry face."
  (tessera-elfeed-search--prepare)
  (tessera-month-reveal-point)
  (tessera-entry-highlight-current))

;;;; Header line providers

(defvar tessera-elfeed-search--requests nil
  "Outstanding observed feed request tokens.")
(defvar tessera-elfeed-search--last-update nil
  "Completion time of the last observed feed update batch.")
(defvar tessera-elfeed-search--update-failed nil
  "Whether the observed batch contains a failed request.")

(defvar tessera-elfeed-search--batch-depth 0
  "Dynamic nesting depth of native feed update dispatch.")
(defvar tessera-elfeed-search--batch-started nil
  "Whether the current native dispatch started a request.")

(defun tessera-elfeed-search--header-line-state ()
  "Collect real displayed entries, query results, and feed counts."
  (let ((entries (make-hash-table :test #'equal))
        (feeds (make-hash-table :test #'equal))
        (unread 0))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (< (point) (point-max))
          (when-let* ((entry (get-text-property
                              (point) 'elfeed-entry))
                      (id (elfeed-entry-id entry)))
            (unless (gethash id entries)
              (puthash id t entries)
              (puthash (elfeed-entry-feed-id entry) t feeds)
              (when (memq 'unread (elfeed-entry-tags entry))
                (cl-incf unread))))
          (forward-line 1))))
    (list :shown (hash-table-count entries)
          :unread unread
          :matched (length elfeed-search-entries)
          :feeds (hash-table-count feeds)
          :query elfeed-search-filter
          :scope (concat "All inserted entries, including folded and "
                         "off-screen entries; excludes headings.")
          :glyph-defaults 'tessera-elfeed-search--glyph-defaults
          :glyph-overrides 'tessera-elfeed-search-glyphs)))

(defun tessera-elfeed-search-header-line-info (context)
  "Return the current search condition for CONTEXT."
  (let ((query (or (plist-get
                    (tessera-header-line-context-state context)
                    :query) "")))
    (tessera-header-line-field
     context 'info "Query" query (concat "Search filter:\n" query))))

(defun tessera-elfeed-search-header-line-action (context)
  "Return the feed update action for CONTEXT."
  (tessera-header-line-update
   context tessera-elfeed-search--last-update
   (when tessera-elfeed-search-header-line-next-update-function
     (funcall tessera-elfeed-search-header-line-next-update-function
              context))
   (or tessera-elfeed-search--requests
       (> tessera-elfeed-search--batch-depth 0))
   #'elfeed-update
   "Update all feeds.  Last time records the observed batch ending."
   tessera-elfeed-search--update-failed))

(defun tessera-elfeed-search--update-notify ()
  "Refresh registered headers, coalescing synchronous batch changes."
  (when (zerop tessera-elfeed-search--batch-depth)
    (dolist (buffer tessera--header-line-buffers)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (and (eq tessera--header-line-view 'elfeed-search)
                     tessera-elfeed-search--active
                     tessera-header-line-enabled)
            (force-mode-line-update)))))))

(defun tessera-elfeed-search--request-finished (token success)
  "Complete observed request TOKEN, recording SUCCESS.
Ignore callbacks from requests discarded by cancellation or disable."
  (when (memq token tessera-elfeed-search--requests)
    (setq tessera-elfeed-search--requests
          (delq token tessera-elfeed-search--requests))
    (unless success (setq tessera-elfeed-search--update-failed t))
    (when (and (null tessera-elfeed-search--requests)
               (zerop tessera-elfeed-search--batch-depth))
      (setq tessera-elfeed-search--last-update (current-time)))
    (tessera-elfeed-search--update-notify)))

(defun tessera-elfeed-search--cancel-update (&rest _ignored)
  "Discard observed requests after the native queue is reset."
  (when tessera-elfeed-search--requests
    (setq tessera-elfeed-search--requests nil
          tessera-elfeed-search--update-failed t)
    (tessera-elfeed-search--update-notify)))

(defun tessera-elfeed-search--observe-update (function url &rest args)
  "Observe FUNCTION updating URL with ARGS, including background work.
Wrap the native fetch completion callback without changing fetching,
parsing, native hooks, or request ordering."
  (let ((token (list url))
        (fetchers elfeed-fetch-functions)
        (queued (elfeed-queue-count-total))
        finished dispatched)
    (when (and (null tessera-elfeed-search--requests)
               (zerop tessera-elfeed-search--batch-depth))
      (setq tessera-elfeed-search--update-failed nil))
    (setq tessera-elfeed-search--batch-started t)
    (push token tessera-elfeed-search--requests)
    (tessera-elfeed-search--update-notify)
    (unwind-protect
        (let ((elfeed-fetch-functions
               (list
                (lambda (feed callback)
                  (let ((elfeed-fetch-functions fetchers))
                    (run-hook-with-args-until-success
                     'elfeed-fetch-functions feed
                     (lambda (result)
                       (let* ((success nil)
                              (parse-failed nil)
                              (elfeed-parse-error-hook
                               (cons (lambda (failed-url _error)
                                       (when (equal failed-url url)
                                         (setq parse-failed t)))
                                     elfeed-parse-error-hook)))
                         (unwind-protect
                             (prog1 (funcall callback result)
                               (setq success
                                     (not (or (eq result :error)
                                              parse-failed))))
                           (unless finished
                             (setq finished t)
                             (tessera-elfeed-search--request-finished
                              token success)))))))))))
          (setq dispatched (apply function url args)))
      ;; Curl can return nil after accepting another queued request.
      (unless (or dispatched finished
                  (> (elfeed-queue-count-total) queued))
        (setq finished t)
        (tessera-elfeed-search--request-finished token nil)))))

(defun tessera-elfeed-search--observe-batch (function &rest args)
  "Observe FUNCTION dispatching a native batch with ARGS."
  (when (and (null tessera-elfeed-search--requests)
             (zerop tessera-elfeed-search--batch-depth))
    (setq tessera-elfeed-search--update-failed nil))
  (unwind-protect
      (let ((tessera-elfeed-search--batch-depth
             (1+ tessera-elfeed-search--batch-depth))
            (tessera-elfeed-search--batch-started nil))
        (unwind-protect
            (apply function args)
          (when (and tessera-elfeed-search--batch-started
                     (null tessera-elfeed-search--requests))
            (setq tessera-elfeed-search--last-update
                  (current-time)))))
    (tessera-elfeed-search--update-notify)))

(defun tessera-elfeed-search--header-line-track (enable)
  "Observe native update batches when ENABLE is non-nil."
  (if enable
      (advice-add 'elfeed-unjam :after
                  #'tessera-elfeed-search--cancel-update)
    (advice-remove 'elfeed-unjam
                   #'tessera-elfeed-search--cancel-update)
    (setq tessera-elfeed-search--requests nil
          tessera-elfeed-search--update-failed nil))
  (if enable
      (advice-add 'elfeed--update-feed :around
                  #'tessera-elfeed-search--observe-update)
    (advice-remove 'elfeed--update-feed
                   #'tessera-elfeed-search--observe-update))
  (dolist (function '(elfeed-update elfeed-update-background))
    (if enable
        (advice-add function :around
                    #'tessera-elfeed-search--observe-batch)
      (advice-remove function
                     #'tessera-elfeed-search--observe-batch))))

(defun tessera-elfeed-search--header-line-enable ()
  "Attach the four-region header to this native view."
  (tessera--header-line-enable
   'elfeed-search #'tessera-elfeed-search--header-line-state
   '((action . tessera-elfeed-search-header-line-action-function)
     (info . tessera-elfeed-search-header-line-info-function)
     (extra . tessera-elfeed-search-header-line-extra-function)
     (statistics
      . tessera-elfeed-search-header-line-statistics-function))))

;;;; Native navigation

(defun tessera-elfeed-search--entry-at-point ()
  "Return the native Elfeed entry on the current line."
  (let ((position (line-beginning-position)))
    (when (< position (point-max))
      (get-text-property position 'elfeed-entry))))

(defun tessera-elfeed-search--position-point ()
  "Place point at the selected entry's title anchor."
  (when-let* ((position (tessera-entry-point)))
    (goto-char position)))

(defun tessera-elfeed-search--target-position (lines)
  "Return the target position LINES logical entries away.
Return nil when the requested logical Elfeed entry does not exist."
  (tessera-elfeed-search--sync-months)
  (unless (zerop lines)
    (if (and tessera--month-enabled tessera--month-groups)
        (tessera--month-visible-entry-position
         (if (> lines 0) 1 -1) (abs lines))
      (save-excursion
        (when (and (zerop (forward-line lines))
                   (tessera-elfeed-search--entry-at-point))
          (point))))))

(defun tessera-elfeed-search--move (lines)
  "Move point LINES logical Elfeed entries when the target exists."
  (when-let* ((target
               (tessera-elfeed-search--target-position lines)))
    (goto-char target)
    (tessera-elfeed-search--position-point)))

(defun tessera-elfeed-search--next (count)
  "Move forward COUNT logical Elfeed entries."
  (interactive "p")
  ;; Preserve native command identity for post-command consumers.
  (when (eq this-command 'tessera-elfeed-search--next)
    (setq this-command 'next-line))
  (tessera-elfeed-search--move count))

(defun tessera-elfeed-search--previous (count)
  "Move backward COUNT logical Elfeed entries."
  (interactive "p")
  (when (eq this-command 'tessera-elfeed-search--previous)
    (setq this-command 'previous-line))
  (tessera-elfeed-search--move (- count)))

(defun tessera-elfeed-search--navigation (enable)
  "Install navigation integration when ENABLE is non-nil."
  (if enable
      (advice-add 'elfeed-search-update-entry :around
                  #'tessera-elfeed-search--update-entries)
    (advice-remove 'elfeed-search-update-entry
                   #'tessera-elfeed-search--update-entries))
  (if enable
      (add-to-list 'emulation-mode-map-alists
                   'tessera-elfeed-search--emulation-map-alist)
    (setq emulation-mode-map-alists
          (delq 'tessera-elfeed-search--emulation-map-alist
                emulation-mode-map-alists))))

(defun tessera-elfeed-search--acquire-navigation ()
  "Register one buffer as a navigation integration user."
  (when (zerop tessera-elfeed-search--navigation-users)
    (tessera-elfeed-search--navigation t))
  (setq tessera-elfeed-search--navigation-users
        (1+ tessera-elfeed-search--navigation-users)))

(defun tessera-elfeed-search--release-navigation ()
  "Unregister one buffer from the navigation integration."
  (when (> tessera-elfeed-search--navigation-users 0)
    (setq tessera-elfeed-search--navigation-users
          (1- tessera-elfeed-search--navigation-users)))
  (when (zerop tessera-elfeed-search--navigation-users)
    (tessera-elfeed-search--navigation nil)))

;;;; Configuration changes

(defun tessera-elfeed-search--glyphs-changed (option)
  "Refresh active elfeed views after glyph OPTION changes.
Nil means explicitly refresh all glyphs and their hover faces."
  (when (or (null option)
            (memq option '(tessera-elfeed-search-glyphs
                           tessera-month-glyphs
                           tessera-entry-ellipsis
                           tessera-glyph-style tessera-glyph-color)))
    (tessera-elfeed-search--months-changed nil)))

(defun tessera-elfeed-search--months-changed (option)
  "Refresh active Elfeed views affected by month OPTION.
Nil requests a full refresh, including glyphs."
  (when (or (null option)
            (memq option '(tessera-month-grouping
                           tessera-elfeed-search-month-grouping)))
    (when (gethash 'elfeed-search tessera--entry-backends)
      (tessera-elfeed-search--register))
    (save-window-excursion
      (tessera--map-mode-buffers
       'elfeed-search-mode
       (lambda ()
         (when tessera-elfeed-search--active
           (setq-local tessera--month-enabled
                       (tessera-elfeed-search--month-enabled-p))
           (tessera-elfeed-search--update-date-separator)
           (tessera-elfeed-search--refresh)))))))

;;;; Buffer lifecycle

(defun tessera-elfeed-search--kill-buffer ()
  "Release navigation resources before killing this search buffer."
  (when tessera-elfeed-search--active
    (setq tessera-elfeed-search--active nil
          tessera-elfeed-search--emulation-map-alist nil)
    (tessera-elfeed-search--release-navigation)))

(defun tessera-elfeed-search--restore-native-state
    (release-navigation)
  "Restore native state in the current Elfeed search buffer.
When RELEASE-NAVIGATION is non-nil, release this buffer's shared
navigation registration."
  (tessera--header-line-disable)
  (setq tessera-elfeed-search--active nil
        tessera-elfeed-search--emulation-map-alist nil)
  (tessera--restore-settings
   tessera-elfeed-search--saved-settings)
  (remove-hook 'elfeed-search-update-hook
               #'tessera-elfeed-search--apply-layout t)
  (remove-hook 'post-command-hook
               #'tessera-elfeed-search--post-command t)
  (remove-hook 'pre-redisplay-functions
               #'tessera-elfeed-search--prepare t)
  (remove-hook 'change-major-mode-hook
               #'tessera-elfeed-search--disable t)
  (remove-hook 'kill-buffer-hook
               #'tessera-elfeed-search--kill-buffer t)
  (tessera-entry-clear-current)
  (tessera-month-clear)
  (tessera-entry-clear-layout)
  (tessera-elfeed-search--restore-separators)
  (setq tessera-elfeed-search--saved-settings nil
        tessera-elfeed-search--face-remapping nil
        tessera-elfeed-search--months-dirty nil
        tessera-elfeed-search--month-separator-hidden nil)
  (when release-navigation
    (tessera-elfeed-search--release-navigation)))

(defun tessera-elfeed-search--enable ()
  "Enable Tessera rendering in the current Elfeed search buffer."
  (require 'elfeed)
  (unless tessera-elfeed-search--active
    (setq tessera-elfeed-search--saved-settings
          (tessera--save-settings
           '(elfeed-search-print-entry-function
             tessera-entry-layout
             elfeed-search-separator-date-format)))
    (let (completed navigation-acquired)
      (unwind-protect
          (progn
            (setq-local elfeed-search-print-entry-function
                        #'tessera-elfeed-search-print-entry)
            (setq-local tessera-entry-layout 'two-line)
            (setq-local tessera--month-enabled
                        (tessera-elfeed-search--month-enabled-p))
            (tessera-elfeed-search--update-date-separator)
            (add-hook 'elfeed-search-update-hook
                      #'tessera-elfeed-search--apply-layout t t)
            (add-hook 'post-command-hook
                      #'tessera-elfeed-search--post-command nil t)
            (add-hook 'pre-redisplay-functions
                      #'tessera-elfeed-search--prepare nil t)
            (add-hook 'change-major-mode-hook
                      #'tessera-elfeed-search--disable nil t)
            (add-hook 'kill-buffer-hook
                      #'tessera-elfeed-search--kill-buffer nil t)
            (setq tessera-elfeed-search--active t
                  tessera-elfeed-search--emulation-map-alist
                  (list
                   (cons
                    'tessera-elfeed-search--active
                    tessera-elfeed-search--navigation-map)))
            (tessera-elfeed-search--acquire-navigation)
            (setq navigation-acquired t)
            (tessera-elfeed-search--refresh)
            (tessera-elfeed-search--header-line-enable)
            (setq completed t))
        (unless completed
          (condition-case nil
              (progn
                (tessera-elfeed-search--restore-native-state
                 navigation-acquired)
                (tessera-elfeed-search--refresh))
            (error nil)))))))

(defun tessera-elfeed-search--disable ()
  "Disable Tessera rendering in the current Elfeed search buffer."
  (when tessera-elfeed-search--active
    (tessera-elfeed-search--restore-native-state t)
    (condition-case error-data
        (tessera-elfeed-search--refresh)
      (error
       (ignore-errors (tessera-elfeed-search--refresh))
       (signal (car error-data) (cdr error-data))))))

(provide 'tessera-elfeed-search)
;;; tessera-elfeed-search.el ends here
