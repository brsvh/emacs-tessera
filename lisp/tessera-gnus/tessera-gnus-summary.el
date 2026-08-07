;;; tessera-gnus-summary.el --- Tessera interface for Gnus Summary  -*- lexical-binding: t; -*-

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

;; Header-line presentation and flat subject formatting for native
;; Gnus Summary buffers.

;;; Code:

(require 'gnus-sum)
(require 'subr-x)
(require 'tessera-ui)

(defvar gnus-tmp-name)
(defvar gnus-tmp-replied)
(defvar gnus-tmp-score-char)
(defvar gnus-tmp-unread)

(defvar-local tessera-gnus-summary--installed-p nil
  "Non-nil when Tessera owns the current Summary presentation.")

(defface tessera-gnus-summary-glyph
  '((t :inherit tessera-entry-feature))
  "Face used when Gnus Summary glyph colors are uniform."
  :group 'tessera-gnus)

(defface tessera-gnus-summary-glyph-success
  '((t :inherit success))
  "Face for successful Gnus Summary states."
  :group 'tessera-gnus)

(defface tessera-gnus-summary-glyph-attention
  '((t :inherit font-lock-keyword-face))
  "Face for Gnus Summary glyphs that need attention."
  :group 'tessera-gnus)

(defface tessera-gnus-summary-glyph-warning
  '((t :inherit warning))
  "Face for Gnus Summary glyphs that warrant caution."
  :group 'tessera-gnus)

(defface tessera-gnus-summary-glyph-error
  '((t :inherit error))
  "Face for erroneous Gnus Summary states."
  :group 'tessera-gnus)

(defface tessera-gnus-summary-glyph-workflow
  '((t :inherit font-lock-constant-face))
  "Face for Gnus Summary workflow glyphs."
  :group 'tessera-gnus)

(defface tessera-gnus-summary-glyph-availability
  '((t :inherit font-lock-variable-name-face))
  "Face for Gnus Summary availability glyphs."
  :group 'tessera-gnus)

(defface tessera-gnus-summary-glyph-security
  '((t :inherit font-lock-builtin-face))
  "Face for Gnus Summary security glyphs."
  :group 'tessera-gnus)

(defun tessera-gnus-summary--set-option (symbol value)
  "Set SYMBOL to VALUE and redraw active Summary buffers."
  (set-default symbol value)
  (when (and (bound-and-true-p tessera-gnus-summary--enabled-p)
             (fboundp 'tessera-gnus-summary--regenerate))
    (dolist (buffer
             (match-buffers '(derived-mode . gnus-summary-mode)))
      (with-current-buffer buffer
        (when (and tessera-gnus-summary--installed-p
                   (bound-and-true-p
                    tessera-gnus-summary--line-format-installed-p)
                   gnus-newsgroup-prepared)
          (tessera-gnus-summary--regenerate))))))

(defcustom tessera-gnus-summary-symbol-style 'unicode
  "Glyph style used for Gnus Summary marks and features.

Individual Summary buffers may override the global default."
  :type '(choice
          (const :tag "Native Gnus marks" native)
          (const :tag "Unicode symbols" unicode)
          (const :tag "Nerd Icons" nerd-icons))
  :set #'tessera-gnus-summary--set-option
  :group 'tessera-gnus)

(make-variable-buffer-local 'tessera-gnus-summary-symbol-style)

(defcustom tessera-gnus-summary-use-uniform-glyph-color nil
  "Whether Gnus Summary glyphs follow their adjacent text color.

When non-nil, marks use the subject color and features use the
author color.  When nil, each glyph uses a face chosen for its
semantic role."
  :type 'boolean
  :set #'tessera-gnus-summary--set-option
  :group 'tessera-gnus)

(defcustom tessera-gnus-summary-mark-symbol-alist nil
  "Overrides for individual Gnus Summary mark glyphs.

Each key is the name of a Gnus mark variable, such as
`gnus-ticked-mark'.  A value is either a literal string or a Nerd
Icons specification of the form (FAMILY . ICON-NAME).  Overrides
take precedence over `tessera-gnus-summary-symbol-style'."
  :type '(alist
          :key-type symbol
          :value-type
          (choice
           (string :tag "Literal glyph")
           (cons :tag "Nerd icon"
                 (symbol :tag "Family")
                 (string :tag "Icon name"))))
  :set #'tessera-gnus-summary--set-option
  :group 'tessera-gnus)

(defcustom tessera-gnus-summary-feature-categories
  '(security payload audience overflow)
  "Ordered feature categories displayed in Gnus Summary entries."
  :type '(repeat
          (choice
           (const security)
           (const payload)
           (const audience)
           (const overflow)))
  :set #'tessera-gnus-summary--set-option
  :group 'tessera-gnus)

(defcustom tessera-gnus-summary-feature-symbol-alist nil
  "Overrides for individual Gnus Summary feature glyphs.

Each key is a feature name such as `encrypted' or `overflow'.  A value
is either a literal string or a Nerd Icons specification of the form
\(FAMILY . ICON-NAME).  Overrides take precedence over
`tessera-gnus-summary-symbol-style'."
  :type '(alist
          :key-type symbol
          :value-type
          (choice
           (string :tag "Literal glyph")
           (cons :tag "Nerd icon"
                 (symbol :tag "Family")
                 (string :tag "Icon name"))))
  :set #'tessera-gnus-summary--set-option
  :group 'tessera-gnus)

(defcustom tessera-gnus-summary-use-semantic-dates t
  "Whether Gnus Summary entries use age-sensitive dates."
  :type 'boolean
  :set #'tessera-gnus-summary--set-option
  :group 'tessera-gnus)

(defcustom tessera-gnus-summary-date-format
  "%B %-d, %Y %I:%M %p"
  "Format used for non-semantic and old Gnus Summary dates."
  :type 'string
  :set #'tessera-gnus-summary--set-option
  :group 'tessera-gnus)

(defcustom tessera-gnus-summary-semantic-date-formats
  '(((gnus-seconds-today) . "Today %I:%M %p")
    ((+ 86400 (gnus-seconds-today)) . "Yesterday %I:%M %p")
    (604800 . "%A %I:%M %p")
    (t . "%B %-d, %Y %I:%M %p"))
  "Age-sensitive formats used for Gnus Summary dates."
  :type '(repeat (cons sexp string))
  :set #'tessera-gnus-summary--set-option
  :group 'tessera-gnus)

(defcustom tessera-gnus-summary-presentation-delay 0.08
  "Seconds to wait before remeasuring Gnus Summary entries."
  :type 'number
  :set #'tessera-gnus-summary--set-option
  :group 'tessera-gnus)

(defconst tessera-gnus-summary--header-line-format
  '(:eval (tessera-gnus-summary--header-line))
  "Header-line format installed in Gnus Summary buffers.")

(defconst tessera-gnus-summary--line-format
  (concat "%u&tessera-gnus-summary-primary-prefix;"
          "%z%O%R%U%u&tessera-gnus-summary-subject;\n"
          "%u&tessera-gnus-summary-metadata;\n")
  "Gnus Summary line format installed by Tessera in flat buffers.")

(defconst tessera-gnus-summary--sort-functions
  '((not gnus-article-sort-by-number)
    (not gnus-article-sort-by-date))
  "Article sort functions installed by Tessera in flat buffers.")

(defconst tessera-gnus-summary--mark-elements
  '((score . entry.native-score-mark)
    (download . entry.native-tertiary-mark)
    (replied . entry.native-secondary-mark)
    (unread . entry.native-main-mark))
  "Element names for native Gnus Summary marks.")

(defconst tessera-gnus-summary--mark-variables
  '((unread
     gnus-unsendable-mark gnus-downloadable-mark gnus-unread-mark
     gnus-ticked-mark gnus-spam-mark gnus-dormant-mark
     gnus-expirable-mark gnus-del-mark gnus-read-mark
     gnus-killed-mark gnus-kill-file-mark gnus-low-score-mark
     gnus-catchup-mark gnus-ancient-mark gnus-sparse-mark
     gnus-canceled-mark gnus-duplicate-mark)
    (replied
     gnus-process-mark gnus-cached-mark gnus-replied-mark
     gnus-forwarded-mark gnus-saved-mark gnus-unseen-mark
     gnus-no-mark)
    (download
     gnus-undownloaded-mark gnus-downloaded-mark gnus-no-mark)
    (score gnus-score-below-mark gnus-score-over-mark gnus-no-mark))
  "Native mark variables used by each Gnus Summary mark slot.")

(defconst tessera-gnus-summary--unicode-main-mark-symbols
  '((gnus-unsendable-mark . "↛")
    (gnus-downloadable-mark . "⇩")
    (gnus-unread-mark . "●")
    (gnus-ticked-mark . "★")
    (gnus-spam-mark . "⚠")
    (gnus-dormant-mark . "⏸")
    (gnus-expirable-mark . "♲")
    (gnus-del-mark . "×")
    (gnus-read-mark . "○")
    (gnus-killed-mark . "⊗")
    (gnus-kill-file-mark . "⌧")
    (gnus-low-score-mark . "▽")
    (gnus-catchup-mark . "⇥")
    (gnus-ancient-mark . "◌")
    (gnus-sparse-mark . "◇")
    (gnus-canceled-mark . "⊘")
    (gnus-duplicate-mark . "⧉"))
  "Unicode glyphs for main Gnus marks.")

(defconst tessera-gnus-summary--unicode-secondary-mark-symbols
  '((gnus-process-mark . "◆")
    (gnus-cached-mark . "▣")
    (gnus-replied-mark . "↩")
    (gnus-forwarded-mark . "↪")
    (gnus-saved-mark . "▤")
    (gnus-unseen-mark . "•")
    (gnus-no-mark . " "))
  "Unicode glyphs for secondary Gnus marks.")

(defconst tessera-gnus-summary--unicode-tertiary-mark-symbols
  '((gnus-undownloaded-mark . "☁")
    (gnus-downloaded-mark . "⬇")
    (gnus-no-mark . " "))
  "Unicode glyphs for tertiary Gnus marks.")

(defconst tessera-gnus-summary--unicode-score-mark-symbols
  '((gnus-score-below-mark . "↘")
    (gnus-score-over-mark . "↗")
    (gnus-no-mark . " "))
  "Unicode glyphs for Gnus score marks.")

(defconst tessera-gnus-summary--native-primary-feature-symbols
  '((encrypted . "E")
    (attachment . "A")
    (personal . "P"))
  "ASCII glyphs for primary features in native style.")

(defconst tessera-gnus-summary--native-secondary-feature-symbols
  '((signed . "S")
    (calendar . "C")
    (mailing-list . "L"))
  "ASCII glyphs for secondary features in native style.")

(defconst tessera-gnus-summary--native-overflow-symbols
  '((overflow . "+"))
  "ASCII glyph for feature overflow in native style.")

(defconst tessera-gnus-summary--unicode-primary-feature-symbols
  '((encrypted . "🔒")
    (attachment . "📎")
    (personal . "◎"))
  "Unicode glyphs for primary Gnus Summary features.")

(defconst tessera-gnus-summary--unicode-secondary-feature-symbols
  '((signed . "✎")
    (calendar . "▦")
    (mailing-list . "☷"))
  "Unicode glyphs for secondary Gnus Summary features.")

(defconst tessera-gnus-summary--unicode-overflow-symbols
  '((overflow . "⋯"))
  "Unicode glyph for feature overflow.")

(defconst tessera-gnus-summary--nerd-main-mark-icons
  '((gnus-unsendable-mark mdicon . "nf-md-email_off")
    (gnus-downloadable-mark mdicon . "nf-md-cloud_download")
    (gnus-unread-mark mdicon . "nf-md-email")
    (gnus-ticked-mark mdicon . "nf-md-star")
    (gnus-spam-mark mdicon . "nf-md-alert")
    (gnus-dormant-mark mdicon . "nf-md-message_reply_outline")
    (gnus-expirable-mark mdicon . "nf-md-delete_sweep_outline")
    (gnus-del-mark mdicon . "nf-md-delete")
    (gnus-read-mark mdicon . "nf-md-email_open")
    (gnus-killed-mark mdicon . "nf-md-eye_off_outline")
    (gnus-kill-file-mark mdicon . "nf-md-file_cancel_outline")
    (gnus-low-score-mark mdicon . "nf-md-trending_down")
    (gnus-catchup-mark mdicon . "nf-md-playlist_check")
    (gnus-ancient-mark mdicon . "nf-md-history")
    (gnus-sparse-mark mdicon . "nf-md-link_variant_off")
    (gnus-canceled-mark mdicon . "nf-md-cancel")
    (gnus-duplicate-mark mdicon . "nf-md-content_duplicate"))
  "Nerd Icons specifications for main Gnus marks.")

(defconst tessera-gnus-summary--nerd-secondary-mark-icons
  '((gnus-process-mark mdicon . "nf-md-checkbox_marked")
    (gnus-cached-mark mdicon . "nf-md-cached")
    (gnus-replied-mark mdicon . "nf-md-reply")
    (gnus-forwarded-mark mdicon . "nf-md-forward")
    (gnus-saved-mark mdicon . "nf-md-content_save")
    (gnus-unseen-mark mdicon . "nf-md-eye"))
  "Nerd Icons specifications for secondary Gnus marks.")

(defconst tessera-gnus-summary--nerd-tertiary-mark-icons
  '((gnus-undownloaded-mark mdicon . "nf-md-cloud_outline")
    (gnus-downloaded-mark mdicon . "nf-md-download_circle"))
  "Nerd Icons specifications for tertiary Gnus marks.")

(defconst tessera-gnus-summary--nerd-score-mark-icons
  '((gnus-score-below-mark mdicon . "nf-md-arrow_down_bold")
    (gnus-score-over-mark mdicon . "nf-md-arrow_up_bold"))
  "Nerd Icons specifications for Gnus score marks.")

(defconst tessera-gnus-summary--nerd-primary-feature-icons
  '((encrypted mdicon . "nf-md-lock")
    (attachment mdicon . "nf-md-paperclip")
    (personal mdicon . "nf-md-account"))
  "Nerd Icons specifications for primary Gnus Summary features.")

(defconst tessera-gnus-summary--nerd-secondary-feature-icons
  '((signed mdicon . "nf-md-certificate_outline")
    (calendar mdicon . "nf-md-calendar")
    (mailing-list mdicon . "nf-md-format_list_bulleted"))
  "Nerd Icons specifications for secondary Gnus Summary features.")

(defconst tessera-gnus-summary--nerd-overflow-icons
  '((overflow mdicon . "nf-md-dots_horizontal_circle_outline"))
  "Nerd Icons specification for feature overflow.")

(defconst tessera-gnus-summary--feature-specs
  '((encrypted security "Encrypted")
    (signed security "Signed")
    (attachment payload "Attachment")
    (calendar payload "Calendar")
    (personal audience "Personal")
    (mailing-list audience "Mailing list"))
  "Names, categories, and descriptions of Gnus Summary features.")

(defconst tessera-gnus-summary--mark-faces
  '((gnus-unsendable-mark . tessera-gnus-summary-glyph-error)
    (gnus-downloadable-mark . tessera-gnus-summary-glyph-availability)
    (gnus-unread-mark . tessera-gnus-summary-glyph-attention)
    (gnus-ticked-mark . tessera-gnus-summary-glyph-attention)
    (gnus-spam-mark . tessera-gnus-summary-glyph-error)
    (gnus-dormant-mark . tessera-gnus-summary-glyph-workflow)
    (gnus-expirable-mark . tessera-gnus-summary-glyph-warning)
    (gnus-del-mark . tessera-gnus-summary-glyph-error)
    (gnus-read-mark . tessera-gnus-summary-glyph)
    (gnus-killed-mark . tessera-gnus-summary-glyph-warning)
    (gnus-kill-file-mark . tessera-gnus-summary-glyph-warning)
    (gnus-low-score-mark . tessera-gnus-summary-glyph-error)
    (gnus-catchup-mark . tessera-gnus-summary-glyph-success)
    (gnus-ancient-mark . tessera-gnus-summary-glyph)
    (gnus-sparse-mark . tessera-gnus-summary-glyph-availability)
    (gnus-canceled-mark . tessera-gnus-summary-glyph-error)
    (gnus-duplicate-mark . tessera-gnus-summary-glyph)
    (gnus-process-mark . tessera-gnus-summary-glyph-workflow)
    (gnus-cached-mark . tessera-gnus-summary-glyph-availability)
    (gnus-replied-mark . tessera-gnus-summary-glyph-workflow)
    (gnus-forwarded-mark . tessera-gnus-summary-glyph-workflow)
    (gnus-saved-mark . tessera-gnus-summary-glyph-success)
    (gnus-unseen-mark . tessera-gnus-summary-glyph-attention)
    (gnus-undownloaded-mark . tessera-gnus-summary-glyph-availability)
    (gnus-downloaded-mark . tessera-gnus-summary-glyph-success)
    (gnus-score-below-mark . tessera-gnus-summary-glyph-error)
    (gnus-score-over-mark . tessera-gnus-summary-glyph-success))
  "Faces used for Gnus marks when semantic glyph colors are enabled.")

(defconst tessera-gnus-summary--feature-faces
  '((encrypted . tessera-gnus-summary-glyph-security)
    (signed . tessera-gnus-summary-glyph-security)
    (attachment . tessera-gnus-summary-glyph-availability)
    (calendar . tessera-gnus-summary-glyph-workflow)
    (personal . tessera-gnus-summary-glyph-attention)
    (mailing-list . tessera-gnus-summary-glyph)
    (overflow . tessera-gnus-summary-glyph-attention))
  "Faces used for features when semantic glyph colors are enabled.")

(defconst tessera-gnus-summary--face-remap-functions
  '(face-remap-add-relative
    face-remap-remove-relative
    face-remap-set-base
    face-remap-reset-base)
  "Face-remapping functions that invalidate entry measurements.")

(defvar-keymap tessera-gnus-summary--status-map
  :doc "Keymap for the status in a Tessera Gnus Summary header."
  "<header-line> <mouse-1>"
  #'tessera-gnus-summary--insert-new-articles)

(defvar-keymap tessera-gnus-summary--entry-map
  :doc "Keymap for Tessera Gnus Summary entries."
  "<mouse-1>" #'tessera-gnus-summary--mouse-select
  "<double-mouse-1>" #'tessera-gnus-summary--mouse-open)

(defvar tessera-gnus-summary--enabled-p nil
  "Non-nil when the Tessera Gnus Summary interface is enabled.")

(defvar-local tessera-gnus-summary--installed-header-line-format nil
  "Exact header-line format object installed by Tessera.")

(defvar-local tessera-gnus-summary--original-header-line-format nil
  "Header-line format saved before installing Tessera.")

(defvar-local tessera-gnus-summary--original-header-line-local-p nil
  "Non-nil when `header-line-format' was originally buffer-local.")

(defvar-local tessera-gnus-summary--line-format-installed-p nil
  "Non-nil when Tessera installed the current Summary line format.")

(defvar-local tessera-gnus-summary--original-line-format nil
  "Summary line format saved before installing Tessera.")

(defvar-local tessera-gnus-summary--original-line-format-local-p nil
  "Non-nil when `gnus-summary-line-format' was buffer-local.")

(defvar-local tessera-gnus-summary--sort-installed-p nil
  "Non-nil when Tessera installed the current article sort order.")

(defvar-local tessera-gnus-summary--original-sort-functions nil
  "Article sort functions saved before installing Tessera.")

(defvar-local
    tessera-gnus-summary--original-sort-functions-local-p nil
  "Non-nil when `gnus-article-sort-functions' was buffer-local.")

(defvar-local tessera-gnus-summary--window-overlays nil
  "Window-local display overlays in the current Summary buffer.")

(defvar-local tessera-gnus-summary--selected-entry-anchor nil
  "Gnus anchor of the entry last presented as selected.")

(defvar tessera-gnus-summary--entry-anchor nil
  "Gnus anchor of the entry whose presentation is being created.")

(defvar-local tessera-gnus-summary--presentation-timer nil
  "Timer waiting to update window-local Summary presentation.")

(defvar-local tessera-gnus-summary--glyph-width nil
  "Pixel width of a Unicode or Nerd Icons glyph cell.")

(defvar-local tessera-gnus-summary--mark-rail-width-cache nil
  "Pixel width of the complete native mark rail.")

(defvar-local tessera-gnus-summary--status-state 'success
  "Semantic status of the current Summary operation.")

(defvar-local tessera-gnus-summary--fetch-current nil
  "Number of headers parsed during the current fetch operation.")

(defvar-local tessera-gnus-summary--fetch-total nil
  "Number of headers requested by the current fetch operation.")

(defvar-local tessera-gnus-summary--fetch-failed nil
  "Number of reliably missing headers in the last fetch operation.")

(defvar-local tessera-gnus-summary--fetch-redraw-step 1
  "Number of parsed headers between fetch progress redraws.")

(defvar-local tessera-gnus-summary--fetch-next-redraw 1
  "Next parsed header count that requests a progress redraw.")

(defvar tessera-gnus-summary--fetch-buffer nil
  "Summary buffer whose synchronous Gnus fetch is active.")

(defun
    gnus-user-format-function-tessera-gnus-summary-primary-prefix
    (_header)
  "Return the padding before a Tessera Gnus Summary primary row."
  (concat
   (tessera-ui-entry-top-padding)
   (tessera-ui-entry-leading-safety-gap)
   (tessera-ui-entry-padding 'entry.left-padding)))

(defun gnus-user-format-function-tessera-gnus-summary-subject (header)
  "Return the primary-row suffix from Gnus HEADER."
  (let* ((separator (tessera-ui-entry-space 'entry.separator))
         (right-padding
          (tessera-ui-entry-padding 'entry.right-padding))
         (safety-gap
          (tessera-ui-entry-trailing-safety-gap)))
    (let ((trailing (concat right-padding safety-gap)))
      (if (not (mail-header-p header))
          (concat separator
                  (tessera-ui-entry-flex-gap trailing)
                  trailing)
        (concat
         separator
         (tessera-gnus-summary--field
          (mail-header-subject header)
          "(No subject)"
          'entry.subject
          'entry.subject.placeholder)
         (tessera-ui-entry-flex-gap trailing)
         trailing)))))

(defun
    gnus-user-format-function-tessera-gnus-summary-metadata
    (header)
  "Return the secondary row from Gnus HEADER."
  (if (not (mail-header-p header))
      ""
    (let* ((left
            (concat
             (tessera-ui-entry-leading-safety-gap)
             (tessera-ui-entry-padding 'entry.left-padding)))
           (indent
            (tessera-ui-entry-space 'entry.secondary-indent))
           (flex (tessera-ui-entry-space 'entry.flex-gap))
           (right-padding
            (tessera-ui-entry-padding 'entry.right-padding))
           (trailing-safety-gap
            (tessera-ui-entry-trailing-safety-gap))
           (trailing
            (concat right-padding trailing-safety-gap))
           (timestamp
            (tessera-gnus-summary--timestamp header))
           (right
            (concat timestamp trailing))
           (author
            (tessera-gnus-summary--field
             gnus-tmp-name
             "(Unknown sender)"
             'entry.author
             'entry.author.placeholder))
           (features
            (tessera-gnus-summary--features header)))
      (concat
       left
       indent
       author
       features
       flex
       right
       (tessera-ui-entry-bottom-padding)))))

(defun tessera-gnus-summary--field
    (value placeholder element placeholder-element)
  "Return VALUE as one line named ELEMENT.

Use PLACEHOLDER and PLACEHOLDER-ELEMENT when VALUE is empty."
  (let* ((value
          (string-trim
           (replace-regexp-in-string
            "[[:cntrl:]\n\r\t ]+" " " (or value ""))))
         (missing-p (string-empty-p value))
         (full-text (if missing-p placeholder value))
         (text (bidi-string-mark-left-to-right full-text)))
    (add-text-properties
     0 (length text)
     (list 'help-echo full-text
           'tessera-element
           (if missing-p placeholder-element element))
     text)
    text))

(defun tessera-gnus-summary--feature-category (feature)
  "Return the category of FEATURE."
  (nth 1 (assq feature tessera-gnus-summary--feature-specs)))

(defun tessera-gnus-summary--feature-description (feature)
  "Return a description of FEATURE."
  (nth 2 (assq feature tessera-gnus-summary--feature-specs)))

(defun tessera-gnus-summary--mark-glyph (variable style)
  "Return the default glyph specification for VARIABLE in STYLE."
  (pcase style
    ('unicode
     (or (alist-get
          variable tessera-gnus-summary--unicode-main-mark-symbols)
         (alist-get
          variable
          tessera-gnus-summary--unicode-secondary-mark-symbols)
         (alist-get
          variable
          tessera-gnus-summary--unicode-tertiary-mark-symbols)
         (alist-get
          variable tessera-gnus-summary--unicode-score-mark-symbols)))
    ('nerd-icons
     (or (alist-get
          variable tessera-gnus-summary--nerd-main-mark-icons)
         (alist-get
          variable tessera-gnus-summary--nerd-secondary-mark-icons)
         (alist-get
          variable tessera-gnus-summary--nerd-tertiary-mark-icons)
         (alist-get
          variable tessera-gnus-summary--nerd-score-mark-icons)))))

(defun tessera-gnus-summary--feature-glyph (feature style)
  "Return the default glyph specification for FEATURE in STYLE."
  (pcase style
    ('native
     (or (alist-get
          feature
          tessera-gnus-summary--native-primary-feature-symbols)
         (alist-get
          feature
          tessera-gnus-summary--native-secondary-feature-symbols)
         (alist-get
          feature tessera-gnus-summary--native-overflow-symbols)))
    ('unicode
     (or (alist-get
          feature
          tessera-gnus-summary--unicode-primary-feature-symbols)
         (alist-get
          feature
          tessera-gnus-summary--unicode-secondary-feature-symbols)
         (alist-get
          feature tessera-gnus-summary--unicode-overflow-symbols)))
    ('nerd-icons
     (or (alist-get
          feature tessera-gnus-summary--nerd-primary-feature-icons)
         (alist-get
          feature tessera-gnus-summary--nerd-secondary-feature-icons)
         (alist-get
          feature tessera-gnus-summary--nerd-overflow-icons)))))

(defun tessera-gnus-summary--nerd-icon (spec fallback)
  "Return the Nerd Icon described by SPEC, or FALLBACK."
  (let ((height 0.8)
        (function
         (and (consp spec)
              (symbolp (car spec))
              (intern (format "nerd-icons-%s" (car spec))))))
    (if (and function
             (stringp (cdr spec))
             (display-graphic-p)
             (require 'nerd-icons nil t)
             (fboundp function))
        (condition-case nil
            (funcall function (cdr spec)
                     :height height
                     :v-adjust (/ (- 1.0 height) 2))
          (error fallback))
      fallback)))

(defun tessera-gnus-summary--render-glyph (value fallback)
  "Render glyph VALUE, using FALLBACK when it is unavailable."
  (cond
   ((stringp value) (copy-sequence value))
   ((consp value)
    (tessera-gnus-summary--nerd-icon value fallback))
   (t fallback)))

(defun tessera-gnus-summary--glyph-face (glyph)
  "Return the first non-nil face found in GLYPH."
  (when-let* ((position
               (text-property-not-all
                0 (length glyph) 'face nil glyph)))
    (get-text-property position 'face glyph)))

(defun tessera-gnus-summary--glyph-color-face
    (key faces &optional uniform-face)
  "Return the color face for KEY from FACES.

Use UNIFORM-FACE when uniform glyph colors are enabled."
  (if tessera-gnus-summary-use-uniform-glyph-color
      (or uniform-face 'tessera-gnus-summary-glyph)
    (or (alist-get key faces)
        'tessera-gnus-summary-glyph)))

(defun tessera-gnus-summary--face-foreground (face)
  "Return the inherited foreground of FACE or the default face."
  (or (face-foreground face nil t)
      (face-foreground 'default nil t)))

(defun tessera-gnus-summary--glyph-color-spec (face)
  "Return the display color specification for FACE."
  (if tessera-gnus-summary-use-uniform-glyph-color
      (if-let* ((foreground
                 (tessera-gnus-summary--face-foreground face)))
          (list :foreground foreground)
        'default)
    face))

(defun tessera-gnus-summary--article-unread-p (article)
  "Return non-nil when native Gnus ARTICLE is unread."
  (and (numberp article)
       (when-let* ((data (gnus-data-find article)))
         (and (numberp (gnus-data-mark data))
              (gnus-data-unread-p data)))))

(defun tessera-gnus-summary--subject-color-face (article)
  "Return the subject color face for native Gnus ARTICLE."
  (if (tessera-gnus-summary--article-unread-p article)
      'tessera-entry-unread
    'tessera-entry-subject))

(defun tessera-gnus-summary--author-color-face (article)
  "Return the author color face for native Gnus ARTICLE."
  (if (tessera-gnus-summary--article-unread-p article)
      'tessera-entry-author-unread
    'tessera-entry-author))

(defun tessera-gnus-summary--feature-symbol-raw (feature)
  "Return the unpadded symbol for FEATURE."
  (let* ((unicode
          (tessera-gnus-summary--feature-glyph feature 'unicode))
         (fallback (or unicode (symbol-name feature)))
         (value
          (or (alist-get
               feature tessera-gnus-summary-feature-symbol-alist)
              (tessera-gnus-summary--feature-glyph
               feature tessera-gnus-summary-symbol-style))))
    (tessera-gnus-summary--render-glyph value fallback)))

(defun tessera-gnus-summary--feature-symbol (feature)
  "Return the displayed symbol cell for FEATURE."
  (tessera-gnus-summary--glyph-cell
   (tessera-gnus-summary--feature-symbol-raw feature)))

(defun tessera-gnus-summary--feature-facts (header)
  "Return content features known from Gnus HEADER."
  (let* ((case-fold-search t)
         (content-type (gnus-extra-header 'Content-Type header))
         (disposition (gnus-extra-header 'Content-Disposition header))
         (list-header
          (concat (gnus-extra-header 'List-Id header)
                  (gnus-extra-header 'List-Post header)
                  (gnus-extra-header 'Mailing-List header)))
         (recipients
          (concat (gnus-extra-header 'To header)
                  " "
                  (gnus-extra-header 'Cc header)))
         features)
    (when (string-match-p
           (concat "multipart/encrypted\\|"
                   "application/.*encrypted")
           content-type)
      (push 'encrypted features))
    (when (string-match-p
           "multipart/signed\\|application/.*signature" content-type)
      (push 'signed features))
    (when (or (string-match-p "attachment" disposition)
              (string-match-p "[; \\t]name=" content-type))
      (push 'attachment features))
    (when (string-match-p "text/calendar" content-type)
      (push 'calendar features))
    (when (and user-mail-address
               (not (string-empty-p user-mail-address))
               (string-match-p
                (regexp-quote user-mail-address) recipients))
      (push 'personal features))
    (unless (string-empty-p list-header)
      (push 'mailing-list features))
    (nreverse features)))

(defun tessera-gnus-summary--select-features (features)
  "Select visible FEATURES and return them with the hidden remainder."
  (let ((remaining features)
        selected hidden)
    (dolist (category tessera-gnus-summary-feature-categories)
      (unless (eq category 'overflow)
        (let (chosen rest)
          (dolist (feature remaining)
            (if (and
                 (not chosen)
                 (eq
                  (tessera-gnus-summary--feature-category feature)
                  category))
                (setq chosen feature)
              (push feature rest)))
          (setq remaining (nreverse rest))
          (when chosen
            (push chosen selected)))))
    (setq hidden remaining)
    (when (and
           hidden
           (memq
            'overflow tessera-gnus-summary-feature-categories))
      (push 'overflow selected))
    (cons (nreverse selected) hidden)))

(defun tessera-gnus-summary--feature-token
    (feature hidden &optional uniform-face)
  "Return the display token for FEATURE.

HIDDEN is the list represented by an overflow token.  UNIFORM-FACE is
the adjacent author face."
  (let* ((category
          (if (eq feature 'overflow)
              'overflow
            (tessera-gnus-summary--feature-category feature)))
         (color-face
          (tessera-gnus-summary--glyph-color-face
           feature tessera-gnus-summary--feature-faces uniform-face))
         (symbol (tessera-gnus-summary--feature-symbol feature))
         (help
          (if (eq feature 'overflow)
              (format "Hidden features: %s"
                      (mapconcat
                       #'tessera-gnus-summary--feature-description
                       hidden ", "))
            (tessera-gnus-summary--feature-description feature))))
    (let* ((text (copy-sequence symbol))
           (glyph-face (tessera-gnus-summary--glyph-face text))
           (display-face
            (if glyph-face
                (let ((foreground
                       (tessera-gnus-summary--face-foreground
                        color-face)))
                  (if foreground
                      (plist-put
                       (copy-sequence glyph-face)
                       :foreground foreground)
                    glyph-face))
              (tessera-gnus-summary--glyph-color-spec color-face))))
      (remove-text-properties
       0 (length text) '(font-lock-face nil) text)
      (put-text-property
       0 (length text) 'face
       (list display-face nil)
       text)
      (add-text-properties
       0 (length text)
       (list
        'gnus-face t
        'help-echo help
        'tessera-feature feature
        'tessera-hidden-features hidden
        'tessera-uniform-glyph-face uniform-face
        'tessera-element
        (intern (format "entry.feature.%s" category)))
       text)
      text)))

(defun tessera-gnus-summary--features (header)
  "Return the feature sequence known from Gnus HEADER."
  (pcase-let* ((`(,selected . ,hidden)
                (tessera-gnus-summary--select-features
                 (tessera-gnus-summary--feature-facts header)))
               (uniform-face
                (and tessera-gnus-summary-use-uniform-glyph-color
                     (tessera-gnus-summary--author-color-face
                      (mail-header-number header)))))
    (if (not selected)
        ""
      (let* ((gap (tessera-ui-entry-space 'entry.separator))
             (separator
              (tessera-gnus-summary--glyph-gap
               'entry.features.inline-gap))
             (tokens
              (mapcar
               (lambda (feature)
                 (tessera-gnus-summary--feature-token
                  feature hidden uniform-face))
               selected))
             (text
              (concat gap
                      (mapconcat #'identity tokens separator))))
        (add-text-properties
         0 (length text)
         '(tessera-parent-element entry.features)
         text)
        text))))

(defun tessera-gnus-summary--mark-variable (slot mark)
  "Return the native variable for MARK in SLOT."
  (catch 'variable
    (dolist (variable
             (cdr
              (assq slot tessera-gnus-summary--mark-variables)))
      (when (and (boundp variable)
                 (= mark (symbol-value variable)))
        (throw 'variable variable)))))

(defun tessera-gnus-summary--mark-symbol-raw (variable mark)
  "Return the unpadded symbol for native VARIABLE and MARK."
  (let* ((native (char-to-string mark))
         (unicode
          (or (tessera-gnus-summary--mark-glyph variable 'unicode)
              native))
         (value
          (or (alist-get
               variable tessera-gnus-summary-mark-symbol-alist)
              (and
               (not (eq tessera-gnus-summary-symbol-style 'native))
               (tessera-gnus-summary--mark-glyph
                variable tessera-gnus-summary-symbol-style))
              native)))
    (tessera-gnus-summary--render-glyph value unicode)))

(defun tessera-gnus-summary--glyph-cell-width ()
  "Return the common pixel width of configured non-native glyphs."
  (or tessera-gnus-summary--glyph-width
      (setq tessera-gnus-summary--glyph-width
            (let ((width 0))
              (dolist (slot tessera-gnus-summary--mark-variables)
                (dolist (variable (cdr slot))
                  (when (and (boundp variable)
                             (characterp (symbol-value variable)))
                    (setq width
                          (max
                           width
                           (string-pixel-width
                            (tessera-gnus-summary--mark-symbol-raw
                             variable (symbol-value variable))))))))
              (dolist (feature
                       (cons 'overflow
                             (mapcar
                              #'car
                              tessera-gnus-summary--feature-specs)))
                (setq width
                      (max
                       width
                       (string-pixel-width
                        (tessera-gnus-summary--feature-symbol-raw
                         feature)))))
              width))))

(defun tessera-gnus-summary--glyph-padding (width part)
  "Return a pixel WIDTH space identified as glyph PART."
  (if (<= width 0)
      ""
    (propertize
     " "
     'display `(space :width (,width))
     'tessera-glyph-part part)))

(defun tessera-gnus-summary--glyph-gap (element)
  "Return a compact space named ELEMENT between glyph cells."
  (let* ((space (tessera-ui-entry-space element))
         (width
          (max 1
               (round (* 0.67 (string-pixel-width space))))))
    (put-text-property
     0 (length space) 'display `(space :width (,width)) space)
    space))

(defun tessera-gnus-summary--glyph-cell (glyph)
  "Return GLYPH centered in the common visual glyph cell."
  (if (eq tessera-gnus-summary-symbol-style 'native)
      glyph
    (let* ((extra
            (max 0
                 (- (tessera-gnus-summary--glyph-cell-width)
                    (string-pixel-width glyph))))
           (leading (/ extra 2)))
      (concat
       (tessera-gnus-summary--glyph-padding
        leading 'glyph.leading-fill)
       glyph
       (tessera-gnus-summary--glyph-padding
        (- extra leading) 'glyph.trailing-fill)))))

(defun tessera-gnus-summary--mark-symbol (variable mark)
  "Return the displayed symbol cell for native VARIABLE and MARK."
  (tessera-gnus-summary--glyph-cell
   (tessera-gnus-summary--mark-symbol-raw variable mark)))

(defun tessera-gnus-summary--mark-description (variable)
  "Return the native Gnus description of mark VARIABLE."
  (when variable
    (car (split-string
          (or (documentation-property
               variable 'variable-documentation)
              (symbol-name variable))
          "\n" t))))

(defun tessera-gnus-summary--date-time (header)
  "Return the time represented by Gnus HEADER, or nil."
  (let ((date (mail-header-date header)))
    (and (not (string-empty-p (or date "")))
         (condition-case nil
             (gnus-date-get-time date)
           (error nil)))))

(defun tessera-gnus-summary--timestamp (header)
  "Return an age-sensitive date for Gnus HEADER."
  (let* ((date (mail-header-date header))
         (time (tessera-gnus-summary--date-time header))
         (text
          (and time
               (if (or (not tessera-gnus-summary-use-semantic-dates)
                       (time-less-p (current-time) time))
                   (format-time-string
                    tessera-gnus-summary-date-format time)
                 (let ((gnus-user-date-format-alist
                        tessera-gnus-summary-semantic-date-formats))
                   (gnus-user-date date)))))
         (missing-p (not text))
         (text (or text "Unknown")))
    (propertize
     text
     'face (list 'tessera-entry-timestamp nil)
     'gnus-face t
     'help-echo (or date text)
     'tessera-element
     (if missing-p
         'entry.timestamp.placeholder
       'entry.timestamp))))

(defun tessera-gnus-summary--total ()
  "Return Gnus's estimated total for the current group."
  (if gnus-newsgroup-active
      (range-length (list gnus-newsgroup-active))
    (length gnus-newsgroup-articles)))

(defun tessera-gnus-summary--statistics ()
  "Return statistics for the current native Gnus Summary snapshot."
  (let ((unread (length gnus-newsgroup-unreads))
        (visible (length gnus-newsgroup-data))
        (total (tessera-gnus-summary--total)))
    (tessera-ui-statistics unread visible total)))

(defun tessera-gnus-summary--format-status ()
  "Return the presentation of the current Summary operation status."
  (let (face help-echo text)
    (pcase tessera-gnus-summary--status-state
      ('processing
       (setq face 'tessera-header-status-processing
             help-echo "Gnus is fetching article headers"
             text
             (if tessera-gnus-summary--fetch-total
                 (format "FETCHING %d/%d"
                         tessera-gnus-summary--fetch-current
                         tessera-gnus-summary--fetch-total)
               "FETCHING")))
      ('fail
       (setq face 'tessera-header-status-fail
             help-echo
             (concat "The last fetch failed; "
                     "mouse-1: Get new articles")
             text
             (if tessera-gnus-summary--fetch-failed
                 (format "FETCH FAILED %d"
                         tessera-gnus-summary--fetch-failed)
               "FETCH FAILED")))
      (_
       (setq face 'tessera-header-status-success
             help-echo "mouse-1: Get new articles"
             text "IDLE")))
    (propertize
     text
     'face face
     'help-echo help-echo
     'keymap tessera-gnus-summary--status-map
     'mouse-face 'header-line-highlight)))

(defun tessera-gnus-summary--insert-new-articles (event)
  "Insert new articles in the Summary window from mouse EVENT."
  (interactive "e")
  (mouse-select-window event)
  (if (eq tessera-gnus-summary--status-state 'processing)
      (message "Gnus is already fetching article headers")
    (tessera-gnus-summary--begin-fetch)
    (condition-case err
        (prog1
            (gnus-summary-insert-new-articles)
          (when (eq tessera-gnus-summary--status-state 'processing)
            (tessera-gnus-summary--finish-fetch 0 nil)))
      ((error quit)
       (unless (eq tessera-gnus-summary--status-state 'fail)
         (tessera-gnus-summary--fail-fetch))
       (signal (car err) (cdr err))))))

(defun tessera-gnus-summary--mouse-select (event)
  "Select the Gnus article clicked by mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (when-let* ((article (get-text-property (point) 'gnus-number)))
    (gnus-summary-goto-subject article nil t)
    (gnus-summary-select-article)))

(defun tessera-gnus-summary--mouse-open (event)
  "Open the Gnus article double-clicked by mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (when-let* ((article (get-text-property (point) 'gnus-number)))
    (gnus-summary-goto-subject article nil t)
    (gnus-summary-select-article)
    (gnus-summary-select-article-buffer)))

(defun tessera-gnus-summary--header-line ()
  "Return the Tessera header for the current Summary buffer."
  (tessera-ui-header-line
   (tessera-gnus-summary--format-status)
   (tessera-ui-query "GROUP" gnus-newsgroup-name)
   (tessera-gnus-summary--statistics)))

(defun tessera-gnus-summary--redraw-status ()
  "Redisplay the current Summary status immediately."
  (force-mode-line-update)
  (redisplay))

(defun tessera-gnus-summary--begin-fetch (&optional total)
  "Begin presenting a fetch of TOTAL article headers.

When TOTAL is nil, display progress without a count."
  (setq tessera-gnus-summary--status-state 'processing
        tessera-gnus-summary--fetch-current (and total 0)
        tessera-gnus-summary--fetch-total total
        tessera-gnus-summary--fetch-failed nil
        tessera-gnus-summary--fetch-redraw-step
        (if total (max 1 (ceiling total 100)) 1)
        tessera-gnus-summary--fetch-next-redraw 1)
  (tessera-gnus-summary--redraw-status))

(defun tessera-gnus-summary--finish-fetch (completed reliable-p)
  "Finish a fetch with COMPLETED headers.

When RELIABLE-P is non-nil, present the number of missing headers."
  (let* ((failed
          (and reliable-p
               (- tessera-gnus-summary--fetch-total completed)))
         (failed-p (and failed (> failed 0))))
    (setq tessera-gnus-summary--status-state
          (if failed-p 'fail 'success)
          tessera-gnus-summary--fetch-current nil
          tessera-gnus-summary--fetch-total nil
          tessera-gnus-summary--fetch-failed
          (and failed-p failed)))
  (tessera-gnus-summary--redraw-status))

(defun tessera-gnus-summary--fail-fetch ()
  "Finish presenting the current fetch as failed."
  (setq tessera-gnus-summary--status-state 'fail
        tessera-gnus-summary--fetch-current nil
        tessera-gnus-summary--fetch-total nil
        tessera-gnus-summary--fetch-failed nil)
  (tessera-gnus-summary--redraw-status))

(defun tessera-gnus-summary--record-header (header)
  "Record one native parse during an active fetch and return HEADER."
  (when (and header
             (buffer-live-p tessera-gnus-summary--fetch-buffer))
    (with-current-buffer tessera-gnus-summary--fetch-buffer
      (when (and (eq tessera-gnus-summary--status-state 'processing)
                 (< tessera-gnus-summary--fetch-current
                    tessera-gnus-summary--fetch-total))
        (setq tessera-gnus-summary--fetch-current
              (1+ tessera-gnus-summary--fetch-current))
        (when (or (>= tessera-gnus-summary--fetch-current
                      tessera-gnus-summary--fetch-next-redraw)
                  (= tessera-gnus-summary--fetch-current
                     tessera-gnus-summary--fetch-total))
          (setq tessera-gnus-summary--fetch-next-redraw
                (+ tessera-gnus-summary--fetch-current
                   tessera-gnus-summary--fetch-redraw-step))
          (tessera-gnus-summary--redraw-status)))))
  header)

(defun tessera-gnus-summary--track-fetch
    (orig-fun articles &optional limit force-new dependencies)
  "Call ORIG-FUN for ARTICLES while displaying fetch progress.

LIMIT, FORCE-NEW, and DEPENDENCIES are passed to
`gnus-fetch-headers'."
  (let ((buffer (current-buffer)))
    (if tessera-gnus-summary--installed-p
        (let ((tessera-gnus-summary--fetch-buffer buffer))
          (tessera-gnus-summary--begin-fetch (length articles))
          (condition-case err
              (let ((headers
                     (funcall
                      orig-fun
                      articles limit force-new dependencies)))
                (with-current-buffer buffer
                  (tessera-gnus-summary--finish-fetch
                   (length headers)
                   force-new))
                headers)
            ((error quit)
             (with-current-buffer buffer
               (tessera-gnus-summary--fail-fetch))
             (signal (car err) (cdr err)))))
      (funcall orig-fun articles limit force-new dependencies))))

(defun tessera-gnus-summary--add-fetch-advice ()
  "Add advice used to present native Gnus fetch progress."
  (advice-add 'gnus-fetch-headers :around
              #'tessera-gnus-summary--track-fetch)
  (advice-add 'nnheader-parse-nov :filter-return
              #'tessera-gnus-summary--record-header)
  (advice-add 'nnheader-parse-head :filter-return
              #'tessera-gnus-summary--record-header))

(defun tessera-gnus-summary--remove-fetch-advice ()
  "Remove advice used to present native Gnus fetch progress."
  (advice-remove 'gnus-fetch-headers
                 #'tessera-gnus-summary--track-fetch)
  (advice-remove 'nnheader-parse-nov
                 #'tessera-gnus-summary--record-header)
  (advice-remove 'nnheader-parse-head
                 #'tessera-gnus-summary--record-header))

(defun tessera-gnus-summary--update-mark (orig-fun &rest args)
  "Call ORIG-FUN with ARGS at the article's primary row.

Only normalize point when Tessera's two-row format is active."
  (let* ((article
          (and tessera-gnus-summary--line-format-installed-p
               (get-text-property (point) 'gnus-number)))
         (data (and article (gnus-data-find article)))
         (position (and data (gnus-data-pos data))))
    (prog1
        (if (and position (< position (line-beginning-position)))
            (save-excursion
              (goto-char position)
              (apply orig-fun args))
          (apply orig-fun args))
      (when tessera-gnus-summary--line-format-installed-p
        (tessera-gnus-summary--refresh-entry-presentations
         (list position))))))

(defun tessera-gnus-summary--entry-bounds (anchor)
  "Return the two-row entry bounds containing ANCHOR."
  (save-excursion
    (goto-char anchor)
    (let ((start (line-beginning-position)))
      (forward-line 2)
      (cons start (point)))))

(defun tessera-gnus-summary--update-entry ()
  "Update presentation properties on the current entry."
  (when-let* ((article (get-text-property (point) 'gnus-number))
              (data (gnus-data-find article))
              (anchor (gnus-data-pos data))
              (bounds (tessera-gnus-summary--entry-bounds anchor))
              (start (car bounds))
              (end (cdr bounds)))
    (let ((face (get-text-property anchor 'face))
          (unread (gnus-data-unread-p data))
          (inhibit-read-only t)
          mark-start mark-end)
      (gnus-put-text-property-excluding-characters-with-faces
       start end 'face face)
      (add-text-properties
       start end
       (list 'keymap tessera-gnus-summary--entry-map))
      (let ((position start))
        (while (< position end)
          (let ((next
                 (next-single-property-change
                  position 'tessera-parent-element nil end)))
            (unless (get-text-property
                     position 'tessera-parent-element)
              (put-text-property
               position next 'tessera-parent-element 'entry))
            (setq position next))))
      (put-text-property start end 'mouse-face nil)
      (put-text-property start (1- end) 'mouse-face 'highlight)
      (dolist (mark tessera-gnus-summary--mark-elements)
        (when-let* ((offset
                     (cdr (assq (car mark)
                                gnus-summary-mark-positions))))
          (let* ((position (+ anchor offset -1))
                 (character (char-after position))
                 (variable
                  (and character
                       (tessera-gnus-summary--mark-variable
                        (car mark) character))))
            (when (< position end)
              (setq mark-start
                    (min (or mark-start position) position)
                    mark-end
                    (max (or mark-end position) (1+ position)))
              (add-text-properties
               position (1+ position)
               (list 'tessera-element (cdr mark)
                     'tessera-native-mark variable
                     'help-echo
                     (tessera-gnus-summary--mark-description
                      variable)))))))
      (when mark-start
        (put-text-property
         mark-start mark-end
         'tessera-parent-element 'entry.state-rail))
      (when-let* ((subject
                   (tessera-gnus-summary--element-bounds
                    start end
                    '(entry.subject entry.subject.placeholder))))
        (put-text-property
         (car subject) (cdr subject) 'face
         (if unread
             (if face
                 (list 'tessera-entry-unread face)
               'tessera-entry-unread)
           (if face
               (list 'tessera-entry-subject face)
             'tessera-entry-subject))))
      (when-let* ((author
                   (tessera-gnus-summary--element-bounds
                    start end
                    '(entry.author entry.author.placeholder))))
        (let ((author-face
               (if unread
                   'tessera-entry-author-unread
                 'tessera-entry-author)))
          (put-text-property
           (car author) (cdr author) 'face
           (if face
               (list author-face face)
             author-face))))
      (when-let* ((timestamp
                   (tessera-gnus-summary--element-bounds
                    start end
                    '(entry.timestamp entry.timestamp.placeholder))))
        (let ((timestamp-face
               (if unread
                   'tessera-entry-timestamp-unread
                 'tessera-entry-timestamp)))
          (put-text-property
           (car timestamp) (cdr timestamp) 'face
           (list timestamp-face face)))))))

(defun tessera-gnus-summary--update-entries ()
  "Apply Tessera properties to every native Summary entry."
  (save-excursion
    (dolist (data gnus-newsgroup-data)
      (goto-char (gnus-data-pos data))
      (tessera-gnus-summary--update-entry))))

(defun tessera-gnus-summary--hide-display-arrow ()
  "Hide the native Summary display arrow in the current buffer."
  (when (markerp overlay-arrow-position)
    (set-marker overlay-arrow-position nil)))

(defun tessera-gnus-summary--set-article-display-arrow
    (orig-fun position)
  "Call ORIG-FUN for POSITION unless the flat layout is active."
  (if tessera-gnus-summary--line-format-installed-p
      (tessera-gnus-summary--hide-display-arrow)
    (funcall orig-fun position)))

(defun tessera-gnus-summary--highlight-selected-entry ()
  "Extend Gnus's selected overlay across the current entry."
  (when (and tessera-gnus-summary--installed-p
             gnus-newsgroup-selected-overlay)
    (let ((previous tessera-gnus-summary--selected-entry-anchor))
      (if tessera-gnus-summary--line-format-installed-p
          (when-let* ((article (gnus-summary-article-number))
                      (data (gnus-data-find article))
                      (anchor (gnus-data-pos data))
                      (start
                       (save-excursion
                         (goto-char anchor)
                         (line-beginning-position))))
            (let ((end
                   (cdr
                    (tessera-gnus-summary--entry-bounds anchor))))
              (move-overlay gnus-newsgroup-selected-overlay
                            start end (current-buffer))
              (overlay-put gnus-newsgroup-selected-overlay
                           'face 'tessera-entry-current)
              (overlay-put gnus-newsgroup-selected-overlay
                           'tessera-element 'entry.current)
              (setq tessera-gnus-summary--selected-entry-anchor
                    anchor)
              (unless (equal previous anchor)
                (tessera-gnus-summary--refresh-entry-presentations
                 (list previous anchor)))))
        (overlay-put gnus-newsgroup-selected-overlay
                     'face gnus-summary-selected-face)
        (overlay-put gnus-newsgroup-selected-overlay
                     'tessera-element nil)
        (setq tessera-gnus-summary--selected-entry-anchor nil)))))

(defun tessera-gnus-summary--update-selected-entry ()
  "Update the presentation of Gnus's current article."
  (when (and (numberp gnus-current-article)
             (not (zerop gnus-current-article)))
    (save-excursion
      (when (gnus-summary-goto-subject gnus-current-article nil t)
        (gnus-highlight-selected-summary)))))

(defun tessera-gnus-summary--update-format-specification ()
  "Update Gnus's compiled Summary format and mark positions."
  (let ((gnus-summary-buffer (current-buffer)))
    (gnus-update-format-specifications nil 'summary)
    (gnus-update-summary-mark-positions)))

(defun tessera-gnus-summary--install-line-format ()
  "Install the Tessera line format in the current Summary buffer."
  (unless tessera-gnus-summary--line-format-installed-p
    (setq tessera-gnus-summary--original-line-format-local-p
          (local-variable-p 'gnus-summary-line-format)
          tessera-gnus-summary--original-line-format
          gnus-summary-line-format
          tessera-gnus-summary--line-format-installed-p t)
    (setq-local gnus-summary-line-format
                tessera-gnus-summary--line-format)
    (tessera-gnus-summary--update-format-specification)
    (tessera-gnus-summary--hide-display-arrow)))

(defun tessera-gnus-summary--restore-line-format ()
  "Restore the native line format in the current Summary buffer."
  (when tessera-gnus-summary--line-format-installed-p
    (when (eq gnus-summary-line-format
              tessera-gnus-summary--line-format)
      (if tessera-gnus-summary--original-line-format-local-p
          (setq-local gnus-summary-line-format
                      tessera-gnus-summary--original-line-format)
        (kill-local-variable 'gnus-summary-line-format)))
    (setq tessera-gnus-summary--line-format-installed-p nil
          tessera-gnus-summary--original-line-format nil
          tessera-gnus-summary--original-line-format-local-p nil)
    (tessera-gnus-summary--update-format-specification)
    t))

(defun tessera-gnus-summary--install-sort-functions ()
  "Sort flat Summary entries from newest to oldest."
  (unless tessera-gnus-summary--sort-installed-p
    (setq tessera-gnus-summary--original-sort-functions-local-p
          (local-variable-p 'gnus-article-sort-functions)
          tessera-gnus-summary--original-sort-functions
          gnus-article-sort-functions
          tessera-gnus-summary--sort-installed-p t)
    (setq-local gnus-article-sort-functions
                tessera-gnus-summary--sort-functions)))

(defun tessera-gnus-summary--restore-sort-functions ()
  "Restore native article sorting in the current Summary buffer."
  (when tessera-gnus-summary--sort-installed-p
    (when (equal gnus-article-sort-functions
                 tessera-gnus-summary--sort-functions)
      (if tessera-gnus-summary--original-sort-functions-local-p
          (setq-local gnus-article-sort-functions
                      tessera-gnus-summary--original-sort-functions)
        (kill-local-variable 'gnus-article-sort-functions)))
    (setq tessera-gnus-summary--sort-installed-p nil
          tessera-gnus-summary--original-sort-functions nil
          tessera-gnus-summary--original-sort-functions-local-p nil)
    t))

(defun tessera-gnus-summary--update-line-format ()
  "Use the Tessera format and sort order in a flat Summary buffer."
  (if gnus-show-threads
      (progn
        (tessera-gnus-summary--delete-window-overlays)
        (tessera-gnus-summary--restore-line-format)
        (tessera-gnus-summary--restore-sort-functions))
    (tessera-gnus-summary--install-line-format)
    (tessera-gnus-summary--install-sort-functions)))

(defun tessera-gnus-summary--regenerate ()
  "Regenerate the current Summary while preserving its article."
  (let ((article (and gnus-newsgroup-data
                      (gnus-summary-article-number))))
    (setq tessera-gnus-summary--glyph-width nil
          tessera-gnus-summary--mark-rail-width-cache nil)
    (gnus-summary-prepare)
    (when article
      (gnus-summary-goto-subject article nil t))))

(defun tessera-gnus-summary--element-bounds (start end elements)
  "Return bounds of the first named element between START and END.

ELEMENTS is a list of accepted `tessera-element' values."
  (let ((position start))
    (while (and (< position end)
                (not (memq (get-text-property
                            position 'tessera-element)
                           elements)))
      (setq position
            (next-single-property-change
             position 'tessera-element nil end)))
    (when (< position end)
      (cons position
            (next-single-property-change
             position 'tessera-element nil end)))))

(defun tessera-gnus-summary--parent-element-bounds
    (start end elements)
  "Return bounds of the first parent element between START and END.

ELEMENTS is a list of accepted `tessera-parent-element' values."
  (let ((position start))
    (while (and (< position end)
                (not (memq (get-text-property
                            position 'tessera-parent-element)
                           elements)))
      (setq position
            (next-single-property-change
             position 'tessera-parent-element nil end)))
    (when (< position end)
      (cons position
            (next-single-property-change
             position 'tessera-parent-element nil end)))))

(defun tessera-gnus-summary--make-window-overlay
    (start end display window)
  "Display text from START to END as DISPLAY in WINDOW."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'window window)
    (overlay-put overlay 'display display)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'tessera-window-presentation t)
    (overlay-put overlay 'tessera-entry-anchor
                 tessera-gnus-summary--entry-anchor)
    (push overlay tessera-gnus-summary--window-overlays)))

(defun tessera-gnus-summary--delete-presentations (predicate)
  "Delete presentation overlays satisfying PREDICATE."
  (let (remaining)
    (dolist (overlay tessera-gnus-summary--window-overlays)
      (if (funcall predicate overlay)
          (delete-overlay overlay)
        (push overlay remaining)))
    (setq tessera-gnus-summary--window-overlays
          (nreverse remaining))))

(defun tessera-gnus-summary--delete-window-overlays ()
  "Delete window-local display overlays in the current Summary."
  (tessera-gnus-summary--delete-presentations #'always))

(defun tessera-gnus-summary--delete-entry-presentations (anchors)
  "Delete window-local presentations for entries at ANCHORS."
  (tessera-gnus-summary--delete-presentations
   (lambda (overlay)
     (member (overlay-get overlay 'tessera-entry-anchor) anchors))))

(defun tessera-gnus-summary--delete-presentations-outside
    (window start end)
  "Delete WINDOW presentations with anchors outside START and END."
  (tessera-gnus-summary--delete-presentations
   (lambda (overlay)
     (let ((anchor (overlay-get overlay 'tessera-entry-anchor)))
       (and (eq (overlay-get overlay 'window) window)
            (or (not (number-or-marker-p anchor))
                (< anchor start)
                (>= anchor end)))))))

(defun tessera-gnus-summary--entry-presented-p (window anchor)
  "Return non-nil when the entry at ANCHOR is presented in WINDOW."
  (catch 'presented
    (dolist (overlay tessera-gnus-summary--window-overlays)
      (when (and (overlay-buffer overlay)
                 (eq (overlay-get overlay 'window) window)
                 (equal
                  (overlay-get overlay 'tessera-entry-anchor)
                  anchor))
        (throw 'presented t)))))

(defun tessera-gnus-summary--available-width (window left right)
  "Return the pixel width between LEFT and RIGHT in WINDOW."
  (max 0
       (- (window-body-width window t)
          (string-pixel-width (concat left right))
          (string-pixel-width
           (tessera-ui-entry-leading-safety-gap)))))

(defun tessera-gnus-summary--present-field
    (window bounds width)
  "Present the field at BOUNDS in WINDOW within pixel WIDTH.

Return the string displayed in WINDOW."
  (let* ((text (buffer-substring (car bounds) (cdr bounds)))
         (display (tessera-ui-truncate-pixels text width)))
    (unless (string= text display)
      (tessera-gnus-summary--make-window-overlay
       (car bounds) (cdr bounds) display window))
    display))

(defun tessera-gnus-summary--mark-slot (element)
  "Return the native mark slot named by ELEMENT."
  (car (rassq element tessera-gnus-summary--mark-elements)))

(defun tessera-gnus-summary--mark-slot-width (slot)
  "Return the widest configured symbol in native mark SLOT."
  (let ((width 0))
    (dolist (variable
             (cdr (assq slot tessera-gnus-summary--mark-variables)))
      (when (and (boundp variable)
                 (characterp (symbol-value variable)))
        (setq width
              (max width
                   (string-pixel-width
                    (tessera-gnus-summary--mark-symbol
                     variable (symbol-value variable)))))))
    width))

(defun tessera-gnus-summary--mark-rail-width ()
  "Return the fixed pixel width of the native mark rail."
  (or tessera-gnus-summary--mark-rail-width-cache
      (let ((width
             (* (1- (length tessera-gnus-summary--mark-elements))
                (string-pixel-width
                 (tessera-gnus-summary--glyph-gap
                  'entry.state.inline-gap)))))
        (dolist (mark tessera-gnus-summary--mark-elements)
          (setq width
                (+ width
                   (tessera-gnus-summary--mark-slot-width
                    (car mark)))))
        (setq tessera-gnus-summary--mark-rail-width-cache width))))

(defun tessera-gnus-summary--mark-display (bounds)
  "Return the display string for the mark at BOUNDS."
  (let* ((position (car bounds))
         (character (char-after position))
         (element (get-text-property position 'tessera-element))
         (slot (tessera-gnus-summary--mark-slot element))
         (variable
          (or (get-text-property position 'tessera-native-mark)
              (tessera-gnus-summary--mark-variable slot character)))
         (uniform-face
          (and tessera-gnus-summary-use-uniform-glyph-color
               (tessera-gnus-summary--subject-color-face
                (get-text-property position 'gnus-number))))
         (color-face
          (tessera-gnus-summary--glyph-color-face
           variable tessera-gnus-summary--mark-faces
           uniform-face))
         (symbol
          (tessera-gnus-summary--mark-symbol variable character))
         (text (copy-sequence symbol))
         (glyph-face (tessera-gnus-summary--glyph-face text))
         (native-face (get-text-property position 'face)))
    (add-text-properties
     0 (length text) (text-properties-at position) text)
    (unless native-face
      (remove-text-properties 0 (length text) '(face nil) text))
    (add-face-text-property
     0 (length text)
     (tessera-gnus-summary--glyph-color-spec color-face)
     nil text)
    (when glyph-face
      (add-face-text-property
       0 (length text) glyph-face t text))
    (when (and (overlayp gnus-newsgroup-selected-overlay)
               (overlay-buffer gnus-newsgroup-selected-overlay)
               (eq (overlay-get gnus-newsgroup-selected-overlay 'face)
                   'tessera-entry-current)
               (<= (overlay-start gnus-newsgroup-selected-overlay)
                   position)
               (< position
                  (overlay-end gnus-newsgroup-selected-overlay)))
      (add-face-text-property
       0 (length text) 'tessera-entry-current t text))
    text))

(defun tessera-gnus-summary--make-marks-overlay
    (start end display window)
  "Display the mark sequence from START to END as DISPLAY in WINDOW."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'window window)
    (overlay-put overlay 'before-string display)
    (overlay-put overlay 'display "")
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'tessera-window-presentation t)
    (overlay-put overlay 'tessera-entry-anchor
                 tessera-gnus-summary--entry-anchor)
    (push overlay tessera-gnus-summary--window-overlays)))

(defun tessera-gnus-summary--present-marks
    (window start subject-start)
  "Present native marks from START to SUBJECT-START in WINDOW.

Return the complete visual prefix before the subject."
  (let ((separator
         (tessera-gnus-summary--glyph-gap
          'entry.state.inline-gap))
        mark-start mark-end
        (marks "")
        (rail-width (tessera-gnus-summary--mark-rail-width)))
    (dolist (mark tessera-gnus-summary--mark-elements)
      (when-let* ((bounds
                   (tessera-gnus-summary--element-bounds
                    start subject-start (list (cdr mark)))))
        (let ((display (tessera-gnus-summary--mark-display bounds)))
          (unless (string-blank-p display)
            (setq marks
                  (concat marks
                          (unless (string-empty-p marks) separator)
                          display)))
          (setq mark-start (or mark-start (car bounds))
                mark-end (cdr bounds)))))
    (if (not mark-start)
        (buffer-substring start subject-start)
      (let* ((fill-width
              (max 0 (- rail-width (string-pixel-width marks))))
             (display
              (concat
               (propertize
                " " 'display
                (tessera-gnus-summary--space-display fill-width))
               marks)))
        (tessera-gnus-summary--make-marks-overlay
         mark-start mark-end display window)
        (concat
         (buffer-substring start mark-start)
         display
         (buffer-substring mark-end subject-start))))))

(defun tessera-gnus-summary--space-display (width)
  "Return a pixel WIDTH display space."
  `(space :width (,width)))

(defun tessera-gnus-summary--feature-token-bounds (start end)
  "Return the feature token bounds between START and END."
  (let ((position start)
        bounds)
    (while (< position end)
      (if (get-text-property position 'tessera-feature)
          (let ((next
                 (next-single-property-change
                  position 'tessera-feature nil end)))
            (push (cons position next) bounds)
            (setq position next))
        (setq position
              (next-single-property-change
               position 'tessera-feature nil end))))
    (nreverse bounds)))

(defun tessera-gnus-summary--fit-features (bounds width)
  "Fit the feature sequence at BOUNDS within pixel WIDTH."
  (let ((text (buffer-substring (car bounds) (cdr bounds))))
    (if (<= (string-pixel-width text) width)
        text
      (let* ((token-bounds
              (tessera-gnus-summary--feature-token-bounds
               (car bounds) (cdr bounds)))
             (gap
              (and token-bounds
                   (buffer-substring
                    (car bounds) (caar token-bounds))))
             (separator
              (tessera-gnus-summary--glyph-gap
               'entry.features.inline-gap))
             (uniform-face
              (and token-bounds
                   (get-text-property
                    (caar token-bounds) 'tessera-uniform-glyph-face)))
             (show-overflow-p
              (memq
               'overflow
               tessera-gnus-summary-feature-categories))
             actual hidden shown)
        (dolist (token-bounds token-bounds)
          (let ((feature
                 (get-text-property
                  (car token-bounds) 'tessera-feature)))
            (if (eq feature 'overflow)
                (setq hidden
                      (append hidden
                              (get-text-property
                               (car token-bounds)
                               'tessera-hidden-features)))
              (push (cons feature
                          (buffer-substring
                           (car token-bounds) (cdr token-bounds)))
                    actual))))
        (setq actual (nreverse actual))
        (while actual
          (let* ((token (pop actual))
                 (remaining actual)
                 (hidden-if-shown
                  (append (mapcar #'car remaining) hidden))
                 (overflow
                  (and show-overflow-p
                       hidden-if-shown
                       (tessera-gnus-summary--feature-token
                        'overflow hidden-if-shown uniform-face)))
                 (candidate
                  (concat gap
                          (mapconcat
                           #'cdr
                           (append (reverse shown) (list token))
                           separator)
                          (and overflow separator)
                          overflow)))
            (if (<= (string-pixel-width candidate) width)
                (push token shown)
              (setq hidden
                    (append
                     (mapcar #'car (cons token remaining))
                     hidden)
                    actual nil))))
        (setq shown (nreverse shown))
        (let* ((overflow
                (and show-overflow-p
                     hidden
                     (tessera-gnus-summary--feature-token
                      'overflow hidden uniform-face)))
               (display
                (concat (and (or shown overflow) gap)
                        (mapconcat #'cdr shown separator)
                        (and shown overflow separator)
                        overflow)))
          (if (<= (string-pixel-width display) width)
              display
            ""))))))

(defun tessera-gnus-summary--present-features
    (window bounds width)
  "Present the feature sequence at BOUNDS in WINDOW within WIDTH."
  (let* ((text (buffer-substring (car bounds) (cdr bounds)))
         (display (tessera-gnus-summary--fit-features bounds width)))
    (unless (string= text display)
      (tessera-gnus-summary--make-window-overlay
       (car bounds) (cdr bounds) display window))
    display))

(defun tessera-gnus-summary--present-entry (window start end)
  "Present the entry from START to END in WINDOW."
  (let* ((tessera-gnus-summary--entry-anchor start)
         (primary-start
          (save-excursion
            (goto-char start)
            (line-beginning-position)))
         (primary-end
          (save-excursion
            (goto-char start)
            (line-end-position)))
         (subject
          (tessera-gnus-summary--element-bounds
           primary-start primary-end
           '(entry.subject entry.subject.placeholder)))
         (primary-flex
          (tessera-gnus-summary--element-bounds
           primary-start primary-end '(entry.flex-gap))))
    (when (and subject primary-flex)
      (let* ((primary-prefix
              (tessera-gnus-summary--present-marks
               window primary-start (car subject)))
             (right
              (buffer-substring (cdr primary-flex) primary-end)))
        (tessera-gnus-summary--present-field
         window subject
         (tessera-gnus-summary--available-width
          window primary-prefix right))
        (let* ((secondary-start (1+ primary-end))
               (secondary-end
                (and (< secondary-start end)
                     (save-excursion
                       (goto-char secondary-start)
                       (line-end-position))))
               (author
                (and secondary-end
                     (tessera-gnus-summary--element-bounds
                      secondary-start secondary-end
                      '(entry.author entry.author.placeholder))))
               (indent
                (and secondary-end
                     (tessera-gnus-summary--element-bounds
                      secondary-start secondary-end
                      '(entry.secondary-indent))))
               (features
                (and secondary-end
                     (tessera-gnus-summary--parent-element-bounds
                      secondary-start secondary-end
                      '(entry.features))))
               (timestamp-flex
                (and secondary-end
                     (tessera-gnus-summary--element-bounds
                      secondary-start secondary-end
                      '(entry.flex-gap))))
               (timestamp
                (and secondary-end
                     (tessera-gnus-summary--element-bounds
                      secondary-start secondary-end
                      '(entry.timestamp
                        entry.timestamp.placeholder)))))
          (when (and author indent timestamp-flex timestamp)
            (let* ((base-left
                    (buffer-substring secondary-start (car indent)))
                   (indent-width
                    (max 0
                         (- (string-pixel-width primary-prefix)
                            (string-pixel-width base-left))))
                   (indent-display
                    (tessera-gnus-summary--space-display
                     indent-width))
                   (left primary-prefix)
                   (trailing
                    (buffer-substring (cdr timestamp) secondary-end))
                   (timestamp-display
                    (tessera-gnus-summary--present-field
                     window timestamp
                     (tessera-gnus-summary--available-width
                      window left trailing)))
                   (right (concat timestamp-display trailing))
                   (available
                    (tessera-gnus-summary--available-width
                     window left right))
                   (author-text
                    (buffer-substring (car author) (cdr author)))
                   (feature-display
                    (if (and features
                             (<= (string-pixel-width author-text)
                                 available))
                        (tessera-gnus-summary--present-features
                         window features
                         (- available
                            (string-pixel-width author-text)))
                      (when features
                        (tessera-gnus-summary--make-window-overlay
                         (car features) (cdr features) "" window))
                      ""))
                   (author-display
                    (tessera-gnus-summary--present-field
                     window author
                     (max 0
                          (- available
                             (string-pixel-width feature-display)))))
                   (remaining
                    (max 0
                         (- available
                            (string-pixel-width author-display)
                            (string-pixel-width feature-display)))))
              (tessera-gnus-summary--make-window-overlay
               (car indent) (cdr indent) indent-display window)
              (tessera-gnus-summary--make-window-overlay
               (car timestamp-flex) (cdr timestamp-flex)
               (tessera-gnus-summary--space-display remaining)
               window))))))))

(defun tessera-gnus-summary--present-window (window &optional limit)
  "Create presentation overlays for visible entries in WINDOW.

Stop at LIMIT when it is non-nil."
  (let ((position (window-start window))
        (limit (or limit (window-end window t) (point-max))))
    (while (< position limit)
      (let* ((article (get-text-property position 'gnus-number))
             (data (and article (gnus-data-find article)))
             (start (and data (gnus-data-pos data)))
             (end
              (and start
                   (next-single-property-change
                    start 'gnus-number nil (point-max)))))
        (if (and start end (> end position))
            (progn
              (unless (tessera-gnus-summary--entry-presented-p
                       window start)
                (tessera-gnus-summary--present-entry
                 window start end))
              (setq position end))
          (setq position
                (next-single-property-change
                 position 'gnus-number nil limit)))))))

(defun tessera-gnus-summary--present-visible-windows ()
  "Create missing presentations in visible Summary windows."
  (when (and tessera-gnus-summary--installed-p
             tessera-gnus-summary--line-format-installed-p)
    (dolist (window
             (get-buffer-window-list (current-buffer) nil t))
      (with-selected-window window
        (tessera-gnus-summary--present-window window)))))

(defun tessera-gnus-summary--refresh-entry-presentations (anchors)
  "Refresh visible presentations for entries at ANCHORS."
  (tessera-gnus-summary--delete-entry-presentations
   (delq nil anchors))
  (tessera-gnus-summary--present-visible-windows))

(defun tessera-gnus-summary--window-scrolled (window start)
  "Create presentations entering WINDOW at display position START."
  (when (and tessera-gnus-summary--installed-p
             tessera-gnus-summary--line-format-installed-p)
    (let (begin limit)
      (save-excursion
        (goto-char start)
        (forward-line -4)
        (setq begin (point))
        (goto-char start)
        (forward-line (+ (window-body-height window) 4))
        (setq limit (point)))
      (tessera-gnus-summary--delete-presentations-outside
       window begin limit)
      (with-selected-window window
        (tessera-gnus-summary--present-window window limit)))))

(defun tessera-gnus-summary--update-window-presentations ()
  "Update presentation overlays for every visible Summary window."
  (tessera-gnus-summary--delete-window-overlays)
  (when (and tessera-gnus-summary--installed-p
             tessera-gnus-summary--line-format-installed-p)
    (dolist (window
             (get-buffer-window-list (current-buffer) nil t))
      (with-selected-window window
        (tessera-gnus-summary--present-window window)))))

(defun tessera-gnus-summary--run-presentation-update (buffer)
  "Update window-local presentation in BUFFER after a delay."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq tessera-gnus-summary--presentation-timer nil)
      (tessera-gnus-summary--update-window-presentations))))

(defun tessera-gnus-summary--cancel-presentation-update ()
  "Cancel a pending window-local presentation update."
  (when (timerp tessera-gnus-summary--presentation-timer)
    (cancel-timer tessera-gnus-summary--presentation-timer))
  (setq tessera-gnus-summary--presentation-timer nil))

(defun tessera-gnus-summary--schedule-window-presentations
    (&rest _args)
  "Schedule an update of window-local Summary presentation."
  (when (and tessera-gnus-summary--installed-p
             tessera-gnus-summary--line-format-installed-p)
    (tessera-gnus-summary--cancel-presentation-update)
    (setq tessera-gnus-summary--presentation-timer
          (run-with-idle-timer
           tessera-gnus-summary-presentation-delay nil
           #'tessera-gnus-summary--run-presentation-update
           (current-buffer)))))

(defun tessera-gnus-summary--face-remap-changed (&rest _args)
  "Remeasure the current Summary after a face remapping change."
  (when (derived-mode-p 'gnus-summary-mode)
    (tessera-gnus-summary--schedule-window-presentations)))

(defun tessera-gnus-summary--refresh-presentation (&rest _args)
  "Regenerate the current flat Summary presentation."
  (when (and tessera-gnus-summary--installed-p
             tessera-gnus-summary--line-format-installed-p
             gnus-newsgroup-prepared)
    (tessera-gnus-summary--cancel-presentation-update)
    (tessera-gnus-summary--regenerate)))

(defun tessera-gnus-summary--refresh-presentations (&rest _args)
  "Regenerate all active Gnus Summary presentations."
  (dolist (buffer (match-buffers '(derived-mode . gnus-summary-mode)))
    (with-current-buffer buffer
      (tessera-gnus-summary--refresh-presentation))))

(defun tessera-gnus-summary--add-face-remap-advice ()
  "Add advice that invalidates Summary entry measurements."
  (require 'face-remap)
  (dolist (function tessera-gnus-summary--face-remap-functions)
    (advice-add function :after
                #'tessera-gnus-summary--face-remap-changed)))

(defun tessera-gnus-summary--remove-face-remap-advice ()
  "Remove advice that invalidates Summary entry measurements."
  (dolist (function tessera-gnus-summary--face-remap-functions)
    (advice-remove function
                   #'tessera-gnus-summary--face-remap-changed)))

(defun tessera-gnus-summary--install ()
  "Install Tessera in the current Gnus Summary buffer."
  (unless tessera-gnus-summary--installed-p
    (setq
     tessera-gnus-summary--original-header-line-local-p
     (local-variable-p 'header-line-format)
     tessera-gnus-summary--original-header-line-format
     header-line-format
     tessera-gnus-summary--installed-header-line-format
     tessera-gnus-summary--header-line-format
     tessera-gnus-summary--installed-p t
     tessera-gnus-summary--status-state 'success
     tessera-gnus-summary--fetch-current nil
     tessera-gnus-summary--fetch-total nil
     tessera-gnus-summary--fetch-failed nil)
    (setq-local
     header-line-format
     tessera-gnus-summary--installed-header-line-format)
    (add-hook 'gnus-summary-generate-hook
              #'tessera-gnus-summary--update-line-format nil t)
    (add-hook 'gnus-summary-update-hook
              #'tessera-gnus-summary--update-entry nil t)
    (add-hook 'gnus-summary-prepare-hook
              #'tessera-gnus-summary--update-selected-entry nil t)
    (add-hook
     'gnus-summary-prepare-hook
     #'tessera-gnus-summary--update-window-presentations nil t)
    (add-hook 'gnus-summary-prepare-hook
              #'tessera-gnus-summary--update-entries nil t)
    (add-hook
     'window-configuration-change-hook
     #'tessera-gnus-summary--schedule-window-presentations nil t)
    (add-hook 'window-scroll-functions
              #'tessera-gnus-summary--window-scrolled nil t)
    (add-hook 'text-scale-mode-hook
              #'tessera-gnus-summary--refresh-presentation nil t)
    (add-hook
     'kill-buffer-hook
     #'tessera-gnus-summary--cancel-presentation-update nil t)
    (when (and gnus-newsgroup-prepared
               (not gnus-show-threads))
      (tessera-gnus-summary--regenerate))
    (force-mode-line-update)))

(defun tessera-gnus-summary--restore ()
  "Restore native presentation in the current Gnus Summary buffer."
  (when tessera-gnus-summary--installed-p
    (remove-hook 'gnus-summary-generate-hook
                 #'tessera-gnus-summary--update-line-format t)
    (remove-hook 'gnus-summary-update-hook
                 #'tessera-gnus-summary--update-entry t)
    (remove-hook
     'gnus-summary-prepare-hook
     #'tessera-gnus-summary--update-window-presentations t)
    (remove-hook 'gnus-summary-prepare-hook
                 #'tessera-gnus-summary--update-entries t)
    (remove-hook
     'window-configuration-change-hook
     #'tessera-gnus-summary--schedule-window-presentations t)
    (remove-hook 'window-scroll-functions
                 #'tessera-gnus-summary--window-scrolled t)
    (remove-hook 'text-scale-mode-hook
                 #'tessera-gnus-summary--refresh-presentation t)
    (remove-hook 'kill-buffer-hook
                 #'tessera-gnus-summary--cancel-presentation-update t)
    (tessera-gnus-summary--cancel-presentation-update)
    (tessera-gnus-summary--delete-window-overlays)
    (when (eq
           header-line-format
           tessera-gnus-summary--installed-header-line-format)
      (if tessera-gnus-summary--original-header-line-local-p
          (setq-local
           header-line-format
           tessera-gnus-summary--original-header-line-format)
        (kill-local-variable 'header-line-format)))
    (setq tessera-gnus-summary--installed-header-line-format nil
          tessera-gnus-summary--original-header-line-format nil
          tessera-gnus-summary--original-header-line-local-p nil
          tessera-gnus-summary--selected-entry-anchor nil)
    (let ((line-format-restored-p
           (tessera-gnus-summary--restore-line-format))
          (sort-functions-restored-p
           (tessera-gnus-summary--restore-sort-functions)))
      (when (and (or line-format-restored-p
                     sort-functions-restored-p)
                 gnus-newsgroup-prepared)
        (tessera-gnus-summary--regenerate)))
    (remove-hook 'gnus-summary-prepare-hook
                 #'tessera-gnus-summary--update-selected-entry t)
    (setq tessera-gnus-summary--installed-p nil)
    (force-mode-line-update)))

(defun tessera-gnus-summary-enable ()
  "Enable Tessera in existing and future Summary buffers."
  (unless tessera-gnus-summary--enabled-p
    (setq tessera-gnus-summary--enabled-p t)
    (tessera-gnus-summary--add-fetch-advice)
    (tessera-gnus-summary--add-face-remap-advice)
    (advice-add 'gnus-summary-update-mark :around
                #'tessera-gnus-summary--update-mark)
    (advice-add 'gnus-summary-set-article-display-arrow :around
                #'tessera-gnus-summary--set-article-display-arrow)
    (advice-add 'gnus-highlight-selected-summary :after
                #'tessera-gnus-summary--highlight-selected-entry)
    (add-hook 'after-setting-font-hook
              #'tessera-gnus-summary--refresh-presentations)
    (add-hook 'enable-theme-functions
              #'tessera-gnus-summary--refresh-presentations)
    (add-hook 'disable-theme-functions
              #'tessera-gnus-summary--refresh-presentations)
    (add-hook 'gnus-summary-mode-hook #'tessera-gnus-summary--install)
    (dolist (buffer
             (match-buffers '(derived-mode . gnus-summary-mode)))
      (with-current-buffer buffer
        (tessera-gnus-summary--install)))))

(defun tessera-gnus-summary-disable ()
  "Disable Tessera presentation in existing Gnus Summary buffers."
  (when tessera-gnus-summary--enabled-p
    (setq tessera-gnus-summary--enabled-p nil)
    (tessera-gnus-summary--remove-fetch-advice)
    (tessera-gnus-summary--remove-face-remap-advice)
    (advice-remove 'gnus-summary-update-mark
                   #'tessera-gnus-summary--update-mark)
    (remove-hook 'after-setting-font-hook
                 #'tessera-gnus-summary--refresh-presentations)
    (remove-hook 'enable-theme-functions
                 #'tessera-gnus-summary--refresh-presentations)
    (remove-hook 'disable-theme-functions
                 #'tessera-gnus-summary--refresh-presentations)
    (remove-hook
     'gnus-summary-mode-hook #'tessera-gnus-summary--install)
    (dolist (buffer
             (match-buffers '(derived-mode . gnus-summary-mode)))
      (with-current-buffer buffer
        (tessera-gnus-summary--restore)))
    (advice-remove 'gnus-summary-set-article-display-arrow
                   #'tessera-gnus-summary--set-article-display-arrow)
    (advice-remove 'gnus-highlight-selected-summary
                   #'tessera-gnus-summary--highlight-selected-entry)))

(provide 'tessera-gnus-summary)
;;; tessera-gnus-summary.el ends here
