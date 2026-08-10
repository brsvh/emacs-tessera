;;; tessera-gnus-summary.el --- Tessera interface for Gnus Summary  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bingshan Chang <chang@bingshan.org>

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

;; Header-line and list presentation for native Gnus Summary buffers.

;;; Code:

(require 'gnus-sum)
(require 'mail-extr)
(require 'seq)
(require 'subr-x)
(require 'tessera-gnus)
(require 'tessera-gnus-status)
(require 'tessera-ui)

(defvar gnus-tmp-name)
(defvar gnus-tmp-level)
(defvar gnus-tmp-thread)
(defvar gnus-tmp-thread-tree-header-string)

(defvar-local tessera-gnus-summary--installed-p nil
  "Non-nil when Tessera owns the current Summary presentation.")

(defface tessera-gnus-summary-glyph
  '((t :inherit tessera-ui-glyph))
  "Face used when Gnus Summary glyph colors are uniform."
  :group 'tessera-gnus)

(defface tessera-gnus-summary-glyph-success
  '((t :inherit tessera-ui-glyph-success))
  "Face for successful Gnus Summary states."
  :group 'tessera-gnus)

(defface tessera-gnus-summary-glyph-attention
  '((t :inherit tessera-ui-glyph-attention))
  "Face for Gnus Summary glyphs that need attention."
  :group 'tessera-gnus)

(defface tessera-gnus-summary-glyph-warning
  '((t :inherit tessera-ui-glyph-warning))
  "Face for Gnus Summary glyphs that warrant caution."
  :group 'tessera-gnus)

(defface tessera-gnus-summary-glyph-error
  '((t :inherit tessera-ui-glyph-error))
  "Face for erroneous Gnus Summary states."
  :group 'tessera-gnus)

(defface tessera-gnus-summary-glyph-workflow
  '((t :inherit tessera-ui-glyph-workflow))
  "Face for Gnus Summary workflow glyphs."
  :group 'tessera-gnus)

(defface tessera-gnus-summary-glyph-availability
  '((t :inherit tessera-ui-glyph-availability))
  "Face for Gnus Summary availability glyphs."
  :group 'tessera-gnus)

(defface tessera-gnus-summary-glyph-security
  '((t :inherit tessera-ui-glyph-security))
  "Face for Gnus Summary security glyphs."
  :group 'tessera-gnus)

(defface tessera-gnus-summary-thread-subject-unread
  '((t :inherit gnus-summary-normal-unread :weight bold))
  "Face for the subject of a thread containing unread articles."
  :group 'tessera-gnus)

(defface tessera-gnus-summary-thread-subject-read
  '((t :inherit gnus-summary-normal-read :weight bold))
  "Face for the subject of a fully read thread."
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
                   (bound-and-true-p tessera-gnus-summary--line-format-installed-p)
                   gnus-newsgroup-prepared)
          (tessera-gnus-summary--regenerate))))))

(defcustom tessera-gnus-summary-mark-symbol-alist nil
  "Overrides for individual Gnus Summary mark glyphs.

Each key is the name of a Gnus mark variable, such as
`gnus-ticked-mark'.  A value is either a literal string or a Nerd
Icons specification of the form (FAMILY . ICON-NAME).  Overrides
take precedence over `tessera-gnus-symbol-style'."
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

(defcustom tessera-gnus-summary-feature-symbol-alist nil
  "Overrides for individual Gnus Summary feature glyphs.

Each key is a feature name such as `encrypted' or `overflow'.  A value
is either a literal string or a Nerd Icons specification of the form
\(FAMILY . ICON-NAME).  Overrides take precedence over
`tessera-gnus-symbol-style'."
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

(defcustom tessera-gnus-summary-group-by-month t
  "Whether Gnus Summary entries are grouped by calendar month."
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

(defconst tessera-gnus-summary--line-format
  (concat "%u&tessera-gnus-summary-primary-prefix;"
          "%z%O%R%U%u&tessera-gnus-summary-subject;\n"
          "%u&tessera-gnus-summary-metadata;\n")
  "Gnus Summary line format installed by Tessera in flat buffers.")

(defconst tessera-gnus-summary--thread-line-format
  (concat "%z%O%R%U"
          "%u&tessera-gnus-summary-thread-member;\n")
  "Gnus Summary line format installed in threaded buffers.")

(defconst tessera-gnus-summary--thread-tree-settings
  '((gnus-sum-thread-tree-root . "* ")
    (gnus-sum-thread-tree-false-root . "* ")
    (gnus-sum-thread-tree-single-indent . "* ")
    (gnus-sum-thread-tree-vertical . "│  ")
    (gnus-sum-thread-tree-indent . "   ")
    (gnus-sum-thread-tree-leaf-with-other . "├─ ")
    (gnus-sum-thread-tree-single-leaf . "└─ "))
  "Gnus thread tree settings installed by Tessera.")

(defconst tessera-gnus-summary--article-sort-functions
  '((not gnus-article-sort-by-number)
    (not gnus-article-sort-by-date))
  "Article sort functions installed by Tessera in flat buffers.")

(defconst tessera-gnus-summary--thread-sort-functions
  '(gnus-thread-sort-by-number
    (not gnus-thread-sort-by-date))
  "Thread sort functions installed by Tessera in threaded buffers.")

(defconst tessera-gnus-summary--mark-elements
  '((other score entry.native-score-mark)
    (tertiary download entry.native-tertiary-mark)
    (secondary replied entry.native-secondary-mark)
    (main unread entry.native-main-mark))
  "Semantic slots, native positions, and elements for Gnus marks.")

(defconst tessera-gnus-summary--mark-variables
  '((main
     gnus-unsendable-mark gnus-downloadable-mark gnus-unread-mark
     gnus-ticked-mark gnus-spam-mark gnus-dormant-mark
     gnus-expirable-mark gnus-del-mark gnus-read-mark
     gnus-killed-mark gnus-kill-file-mark gnus-low-score-mark
     gnus-catchup-mark gnus-ancient-mark gnus-sparse-mark
     gnus-canceled-mark gnus-duplicate-mark gnus-recent-mark)
    (secondary
     gnus-process-mark gnus-cached-mark gnus-replied-mark
     gnus-forwarded-mark gnus-saved-mark gnus-unseen-mark
     gnus-no-mark)
    (tertiary
     gnus-undownloaded-mark gnus-downloaded-mark gnus-no-mark)
    (other gnus-score-below-mark gnus-score-over-mark gnus-no-mark))
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
    (gnus-duplicate-mark . "⧉")
    (gnus-recent-mark . "◉"))
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

(defconst tessera-gnus-summary--unicode-other-mark-symbols
  '((gnus-score-below-mark . "↘")
    (gnus-score-over-mark . "↗")
    (gnus-no-mark . " "))
  "Unicode glyphs for other Gnus marks.")

(defconst tessera-gnus-summary--ascii-primary-feature-symbols
  '((encrypted . "E")
    (attachment . "A")
    (personal . "P"))
  "ASCII glyphs for primary Gnus Summary features.")

(defconst tessera-gnus-summary--ascii-secondary-feature-symbols
  '((signed . "S")
    (calendar . "C")
    (mailing-list . "L"))
  "ASCII glyphs for secondary Gnus Summary features.")

(defconst tessera-gnus-summary--ascii-overflow-symbols
  '((overflow . "+"))
  "ASCII glyph for Gnus Summary feature overflow.")

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
    (gnus-duplicate-mark mdicon . "nf-md-content_duplicate")
    (gnus-recent-mark mdicon . "nf-md-new_box"))
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

(defconst tessera-gnus-summary--nerd-other-mark-icons
  '((gnus-score-below-mark mdicon . "nf-md-arrow_down_bold")
    (gnus-score-over-mark mdicon . "nf-md-arrow_up_bold"))
  "Nerd Icons specifications for other Gnus marks.")

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
    (gnus-recent-mark . tessera-gnus-summary-glyph-attention)
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

(defvar-keymap tessera-gnus-summary--entry-map
  :doc "Keymap for Tessera Gnus Summary entries."
  "<mouse-1>" #'tessera-gnus-summary--mouse-select
  "<mouse-2>" #'tessera-gnus-summary--mouse-select
  "<double-mouse-1>" #'tessera-gnus-summary--mouse-open)

(defvar-keymap tessera-gnus-summary--month-map
  :doc "Keymap for month headings in a Tessera Gnus Summary."
  "C-n" #'tessera-gnus-summary--next-month-line
  "C-p" #'tessera-gnus-summary--previous-month-line
  "RET" #'tessera-gnus-summary--toggle-month
  "TAB" #'tessera-gnus-summary--toggle-month
  "<tab>" #'tessera-gnus-summary--toggle-month
  "<mouse-1>" #'tessera-gnus-summary--mouse-toggle-month
  "<mouse-2>" #'tessera-gnus-summary--mouse-toggle-month)

(defvar tessera-gnus-summary--enabled-p nil
  "Non-nil when the Tessera Gnus Summary interface is enabled.")

(defvar-local tessera-gnus-summary--line-format-installed-p nil
  "Non-nil when Tessera installed the current Summary line format.")

(defvar-local tessera-gnus-summary--original-line-format nil
  "Summary line format saved before installing Tessera.")

(defvar-local tessera-gnus-summary--original-line-format-local-p nil
  "Non-nil when `gnus-summary-line-format' was buffer-local.")

(defvar-local tessera-gnus-summary--original-thread-tree-settings nil
  "Thread tree settings saved before installing Tessera values.")

(defvar-local tessera-gnus-summary--sort-variable nil
  "Sort variable currently controlled by Tessera.")

(defvar-local tessera-gnus-summary--original-sort-functions nil
  "Sort functions saved before installing Tessera.")

(defvar-local tessera-gnus-summary--original-sort-functions-local-p nil
  "Non-nil when the saved sort variable was buffer-local.")

(defvar-local tessera-gnus-summary--subthread-sort-installed-p nil
  "Non-nil when Tessera preserved the native subthread order.")

(defvar-local tessera-gnus-summary--original-subthread-sort-local-p nil
  "Non-nil when the native subthread sort was buffer-local.")

(defvar-local tessera-gnus-summary--presentations nil
  "Window-local presentation state in the current Summary buffer.")

(defvar-local tessera-gnus-summary--month-overlays nil
  "Overlays presenting month headings and folds in this buffer.")

(defvar-local tessera-gnus-summary--collapsed-months nil
  "Calendar month keys collapsed in the current Summary buffer.")

(defvar-local tessera-gnus-summary--month-groups nil
  "Standard month groups in the current Summary buffer.")

(defvar-local tessera-gnus-summary--selected-entry-anchor nil
  "Gnus anchor of the entry last presented as selected.")

(defvar tessera-gnus-summary--presentation-anchor nil
  "Buffer anchor whose presentation is being created.")

(defvar-local tessera-gnus-summary--glyph-width nil
  "Pixel width of a Unicode or Nerd Icons glyph cell.")

(defvar-local tessera-gnus-summary--flat-rail nil
  "Shared rail specification for flat Summary entries.")

(defvar-local tessera-gnus-summary--thread-model-ready-p nil
  "Non-nil after every native thread member has a buffer position.")

(defvar-local tessera-gnus-summary--thread-groups nil
  "Map native Gnus article numbers to visible thread members.")

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

(defun tessera-gnus-summary--entry-leading ()
  "Return the padding before a Gnus Summary entry."
  (concat (tessera-ui-entry-top-padding)
          (tessera-ui-entry-leading-safety-gap)
          (tessera-ui-entry-padding 'entry.left-padding)))

(defun gnus-user-format-function-tessera-gnus-summary-primary-prefix (_header)
  "Return the padding before a Tessera Gnus Summary primary row."
  (tessera-gnus-summary--entry-leading))

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
        (concat separator
                (tessera-gnus-summary--field (mail-header-subject header)
                                             "(No subject)"
                                             'entry.subject
                                             'entry.subject.placeholder)
                (tessera-ui-entry-flex-gap trailing)
                trailing)))))

(defun gnus-user-format-function-tessera-gnus-summary-metadata (header)
  "Return the secondary row from Gnus HEADER."
  (if (not (mail-header-p header))
      ""
    (let* ((left
            (concat (tessera-ui-entry-leading-safety-gap)
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
            (tessera-gnus-summary--field gnus-tmp-name
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

(defun tessera-gnus-summary--thread-tree-prefix ()
  "Return Gnus's current thread tree prefix with Tessera properties."
  (let* ((text
          (copy-sequence (or gnus-tmp-thread-tree-header-string "")))
         (branch
          (alist-get 'gnus-sum-thread-tree-leaf-with-other
                     tessera-gnus-summary--thread-tree-settings))
         (last
          (alist-get 'gnus-sum-thread-tree-single-leaf
                     tessera-gnus-summary--thread-tree-settings))
         (suffix
          (cond
           ((zerop gnus-tmp-level)
            (alist-get 'gnus-sum-thread-tree-root
                       tessera-gnus-summary--thread-tree-settings))
           ((string-suffix-p branch text) branch)
           (t last)))
         (connector-end
          (max 0 (1- (length text))))
         (connector-start
          (max 0 (- (length text) (length suffix))))
         (vertical
          (alist-get 'gnus-sum-thread-tree-vertical
                     tessera-gnus-summary--thread-tree-settings))
         (position 0)
         ancestors)
    (while (< position connector-start)
      (let ((next
             (min connector-start
                  (+ position (length vertical)))))
        (push (string= (substring text position next)
                       vertical)
              ancestors)
        (setq position next)))
    (put-text-property 0 (length text) 'tessera-ui-thread-connector
                       (tessera-ui-thread-connector-create
                        :ancestors (nreverse ancestors)
                        :kind
                        (cond
                         ((zerop gnus-tmp-level) 'root)
                         ((string= suffix branch) 'branch)
                         (t 'last)))
                       text)
    (when (> connector-start 0)
      (add-text-properties 0 connector-start '(face tessera-ui-thread-connector tessera-ui--element thread.indentation) text))
    (when (> connector-end connector-start)
      (add-text-properties connector-start connector-end
                           '(face tessera-ui-thread-connector
                                  tessera-ui--element thread.connector)
                           text))
    (when (< connector-end (length text))
      (put-text-property connector-end (length text) 'tessera-ui--element 'thread.after-connector-gap text))
    (add-text-properties 0 (length text) '(tessera-ui--parent-element thread.tree-prefix) text)
    (when (zerop gnus-tmp-level)
      (put-text-property 0 (length text) 'tessera-ui-thread-known-count
                         (gnus-summary-number-of-articles-in-thread (and (boundp 'gnus-tmp-thread)
                                                                         (car gnus-tmp-thread))
                                                                    gnus-tmp-level)
                         text))
    text))

(defun gnus-user-format-function-tessera-gnus-summary-thread-member (header)
  "Return the threaded member suffix from Gnus HEADER."
  (if (not (mail-header-p header))
      ""
    (let* ((separator
            (tessera-ui-entry-space 'entry.separator))
           (tree (tessera-gnus-summary--thread-tree-prefix))
           (author
            (tessera-gnus-summary--field gnus-tmp-name
                                         "(Unknown sender)"
                                         'entry.author
                                         'entry.author.placeholder))
           (features (tessera-gnus-summary--features header))
           (right-padding
            (tessera-ui-entry-padding 'entry.right-padding))
           (safety-gap
            (tessera-ui-entry-trailing-safety-gap))
           (trailing (concat right-padding safety-gap))
           (timestamp (tessera-gnus-summary--timestamp header))
           (right (concat timestamp trailing)))
      (concat
       separator tree author features
       (tessera-ui-entry-flex-gap right)
       right))))

(defun tessera-gnus-summary--field (value placeholder element placeholder-element)
  "Return VALUE as one line named ELEMENT.

Use PLACEHOLDER and PLACEHOLDER-ELEMENT when VALUE is empty."
  (let* ((value
          (string-trim (replace-regexp-in-string "[[:cntrl:]\n\r\t ]+" " " (or value ""))))
         (missing-p (string-empty-p value))
         (full-text (if missing-p placeholder value))
         (text (bidi-string-mark-left-to-right full-text)))
    (add-text-properties 0 (length text)
                         (list 'help-echo (substring-no-properties full-text)
                               'tessera-ui--element
                               (if missing-p placeholder-element element))
                         text)
    text))

(defun tessera-gnus-summary--feature-category (feature)
  "Return the category of FEATURE."
  (when-let* ((spec
               (tessera-ui-glyph-spec 'gnus-summary 'feature feature)))
    (tessera-ui-glyph-spec-slot spec)))

(defun tessera-gnus-summary--feature-description (feature)
  "Return a description of FEATURE."
  (when-let* ((spec
               (tessera-ui-glyph-spec 'gnus-summary 'feature feature)))
    (tessera-ui-glyph-spec-description spec)))

(cl-labels ((mark-glyph (variable style)
              (pcase style
                ('unicode
                 (or
                  (alist-get variable
                             tessera-gnus-summary--unicode-main-mark-symbols)
                  (alist-get variable
                             tessera-gnus-summary--unicode-secondary-mark-symbols)
                  (alist-get variable
                             tessera-gnus-summary--unicode-tertiary-mark-symbols)
                  (alist-get variable
                             tessera-gnus-summary--unicode-other-mark-symbols)))
                ('nerd-icons
                 (or
                  (alist-get variable tessera-gnus-summary--nerd-main-mark-icons)
                  (alist-get variable
                             tessera-gnus-summary--nerd-secondary-mark-icons)
                  (alist-get variable
                             tessera-gnus-summary--nerd-tertiary-mark-icons)
                  (alist-get variable
                             tessera-gnus-summary--nerd-other-mark-icons)))))
            (feature-glyph (feature style)
              (pcase style
                ('ascii
                 (or
                  (alist-get feature
                             tessera-gnus-summary--ascii-primary-feature-symbols)
                  (alist-get feature
                             tessera-gnus-summary--ascii-secondary-feature-symbols)
                  (alist-get feature tessera-gnus-summary--ascii-overflow-symbols)))
                ('unicode
                 (or
                  (alist-get feature
                             tessera-gnus-summary--unicode-primary-feature-symbols)
                  (alist-get feature
                             tessera-gnus-summary--unicode-secondary-feature-symbols)
                  (alist-get feature tessera-gnus-summary--unicode-overflow-symbols)))
                ('nerd-icons
                 (or
                  (alist-get feature
                             tessera-gnus-summary--nerd-primary-feature-icons)
                  (alist-get feature
                             tessera-gnus-summary--nerd-secondary-feature-icons)
                  (alist-get feature tessera-gnus-summary--nerd-overflow-icons))))))
  (let (definitions)
    (dolist (slot-spec tessera-gnus-summary--mark-variables)
      (let ((slot (car slot-spec))
            (priority 100))
        (dolist (variable (cdr slot-spec))
          (unless (eq variable 'gnus-no-mark)
            (push (list :kind 'mark
                        :fact variable
                        :slot slot
                        :priority priority
                        :unicode
                        (mark-glyph variable 'unicode)
                        :nerd-icon
                        (mark-glyph variable 'nerd-icons)
                        :face
                        (alist-get variable tessera-gnus-summary--mark-faces)
                        :description
                        (car (split-string (or
                                            (documentation-property variable 'variable-documentation)
                                            (symbol-name variable))
                                           "\n" t)))
                  definitions)
            (setq priority (1- priority))))))
    (dolist
        (slot-spec
         `((primary
            ,tessera-gnus-summary--ascii-primary-feature-symbols)
           (secondary
            ,tessera-gnus-summary--ascii-secondary-feature-symbols)))
      (let ((slot (car slot-spec))
            (priority 100))
        (dolist (ascii (cadr slot-spec))
          (let* ((feature (car ascii))
                 (description
                  (nth 2
                       (assq feature
                             tessera-gnus-summary--feature-specs))))
            (push (list :kind 'feature
                        :fact feature
                        :slot slot
                        :priority priority
                        :ascii (cdr ascii)
                        :unicode
                        (feature-glyph feature 'unicode)
                        :nerd-icon
                        (feature-glyph feature 'nerd-icons)
                        :face
                        (alist-get feature tessera-gnus-summary--feature-faces)
                        :description description)
                  definitions)
            (setq priority (1- priority))))))
    (push (list :kind 'feature
                :fact 'overflow
                :slot 'overflow
                :priority 0
                :ascii
                (alist-get 'overflow tessera-gnus-summary--ascii-overflow-symbols)
                :unicode
                (alist-get 'overflow tessera-gnus-summary--unicode-overflow-symbols)
                :nerd-icon
                (alist-get 'overflow tessera-gnus-summary--nerd-overflow-icons)
                :face 'tessera-gnus-summary-glyph-attention
                :description "More features")
          definitions)
    (tessera-ui-glyph-register-source 'gnus-summary (nreverse definitions))))

(defun tessera-gnus-summary--glyph-face (glyph)
  "Return the first non-nil face found in GLYPH."
  (when-let* ((position
               (text-property-not-all 0 (length glyph) 'face nil glyph)))
    (get-text-property position 'face glyph)))

(defun tessera-gnus-summary--glyph-color (key faces &optional context-color)
  "Return the face or color for KEY from FACES.

Use CONTEXT-COLOR when glyphs follow adjacent text."
  (cond
   ((stringp tessera-gnus-glyph-color-style)
    tessera-gnus-glyph-color-style)
   ((null tessera-gnus-glyph-color-style)
    (or context-color
        (face-foreground 'tessera-gnus-summary-glyph nil t)
        (face-foreground 'default nil t)))
   (t
    (or (alist-get key faces)
        'tessera-gnus-summary-glyph))))

(defun tessera-gnus-summary--glyph-color-spec (color)
  "Return the display face for COLOR."
  (if (stringp color)
      (list :foreground color :weight 'normal :slant 'normal)
    (list :inherit color :weight 'normal :slant 'normal)))

(defun tessera-gnus-summary--glyph-foreground (color)
  "Return the foreground selected by COLOR."
  (if (stringp color)
      color
    (or (face-foreground color nil t)
        (face-foreground 'default nil t))))

(defun tessera-gnus-summary--glyph-face-color (face foreground)
  "Return a copy of FACE using FOREGROUND."
  (let ((face
         (if (and (consp face) (keywordp (car face)))
             (copy-sequence face)
           (list :inherit (or face 'default)))))
    (setq face (plist-put face :foreground foreground))
    (setq face (plist-put face :weight 'normal))
    (plist-put face :slant 'normal)))

(defun tessera-gnus-summary--foreground-at (position)
  "Return the displayed foreground at POSITION."
  (save-excursion
    (goto-char position)
    (or (foreground-color-at-point)
        (face-foreground 'default nil t))))

(defun tessera-gnus-summary--article-unread-p (article)
  "Return non-nil when native Gnus ARTICLE is unread."
  (and (numberp article)
       (when-let* ((data (gnus-data-find article)))
         (and (numberp (gnus-data-mark data))
              (gnus-data-unread-p data)))))

(defun tessera-gnus-summary--subject-face (article)
  "Return the native Gnus Summary face for ARTICLE."
  (or
   (when-let* ((data (gnus-data-find article))
               (anchor (gnus-data-pos data)))
     (tessera-gnus-summary--article-face anchor))
   'tessera-ui-entry-subject))

(defun tessera-gnus-summary--author-face (article)
  "Return a color-only italic author face for Gnus ARTICLE."
  (tessera-ui-entry-author-face (tessera-gnus-summary--subject-face article)
                                (tessera-gnus-summary--article-unread-p article)))

(defun tessera-gnus-summary--feature-symbol-raw (feature)
  "Return the unpadded symbol for FEATURE."
  (when-let* ((spec
               (tessera-ui-glyph-spec 'gnus-summary 'feature feature)))
    (tessera-ui-glyph-render spec
                             tessera-gnus-symbol-style
                             tessera-gnus-glyph-color-style
                             'tessera-ui-entry-author
                             (alist-get feature tessera-gnus-summary-feature-symbol-alist))))

(defun tessera-gnus-summary--feature-symbol (feature)
  "Return the displayed symbol cell for FEATURE."
  (tessera-gnus-summary--glyph-cell (tessera-gnus-summary--feature-symbol-raw feature)))

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
    (when (string-match-p (concat "multipart/encrypted\\|"
                                  "application/.*encrypted")
                          content-type)
      (push 'encrypted features))
    (when (string-match-p "multipart/signed\\|application/.*signature" content-type)
      (push 'signed features))
    (when (or (string-match-p "attachment" disposition)
              (string-match-p "[; \\t]name=" content-type))
      (push 'attachment features))
    (when (string-match-p "text/calendar" content-type)
      (push 'calendar features))
    (when (and user-mail-address
               (not (string-empty-p user-mail-address))
               (string-match-p (regexp-quote user-mail-address) recipients))
      (push 'personal features))
    (unless (string-empty-p list-header)
      (push 'mailing-list features))
    (nreverse features)))

(defun tessera-gnus-summary--select-features (features)
  "Select visible FEATURES and return them with the hidden remainder."
  (let ((selection
         (tessera-ui-glyph-select-features 'gnus-summary features)))
    (cons
     (mapcar #'tessera-ui-glyph-spec-fact
             (tessera-ui-glyph-selection-specs selection))
     (tessera-ui-glyph-selection-hidden selection))))

(defun tessera-gnus-summary--feature-token (feature hidden &optional context-color)
  "Return the display token for FEATURE.

HIDDEN is the list represented by an overflow token.  CONTEXT-COLOR is
the adjacent author foreground."
  (let* ((category
          (if (eq feature 'overflow)
              'overflow
            (tessera-gnus-summary--feature-category feature)))
         (color
          (tessera-gnus-summary--glyph-color feature tessera-gnus-summary--feature-faces context-color))
         (symbol (tessera-gnus-summary--feature-symbol feature))
         (help
          (if (eq feature 'overflow)
              (format "Hidden features: %s"
                      (mapconcat #'tessera-gnus-summary--feature-description
                                 hidden ", "))
            (tessera-gnus-summary--feature-description feature))))
    (let* ((text (copy-sequence symbol))
           (glyph-face (tessera-gnus-summary--glyph-face text))
           (display-face
            (if glyph-face
                (let ((foreground
                       (tessera-gnus-summary--glyph-foreground color)))
                  (if foreground
                      (tessera-gnus-summary--glyph-face-color glyph-face foreground)
                    glyph-face))
              (tessera-gnus-summary--glyph-color-spec color))))
      (remove-text-properties 0 (length text) '(font-lock-face nil) text)
      (put-text-property 0 (length text) 'face (list display-face nil) text)
      (add-text-properties 0 (length text)
                           (list 'gnus-face t
                                 'help-echo help
                                 'tessera-ui--feature feature
                                 'tessera-ui--hidden-features hidden
                                 'tessera-ui--glyph-context-color context-color
                                 'tessera-ui--element
                                 (intern (format "entry.feature.%s" category)))
                           text)
      text)))

(defun tessera-gnus-summary--set-feature-context-color (start end author-position)
  "Make features from START to END follow AUTHOR-POSITION."
  (when (null tessera-gnus-glyph-color-style)
    (let ((foreground
           (tessera-gnus-summary--foreground-at author-position))
          (position start))
      (while-let
          ((feature-position
            (text-property-not-all position end 'tessera-ui--feature nil)))
        (let* ((next
                (next-single-property-change feature-position 'tessera-ui--feature nil end))
               (faces
                (get-text-property feature-position 'face))
               (glyph-face (car faces))
               (article-face (cadr faces)))
          (put-text-property feature-position next 'face
                             (list (tessera-gnus-summary--glyph-face-color glyph-face foreground)
                                   article-face))
          (put-text-property feature-position next 'tessera-ui--glyph-context-color foreground)
          (setq position next))))))

(defun tessera-gnus-summary--features (header)
  "Return the feature sequence known from Gnus HEADER."
  (pcase-let* ((`(,selected . ,hidden)
                (tessera-gnus-summary--select-features (tessera-gnus-summary--feature-facts header))))
    (if (not selected)
        ""
      (let* ((gap (tessera-ui-entry-space 'entry.separator))
             (separator
              (tessera-gnus-summary--glyph-gap 'entry.features.inline-gap))
             (tokens
              (mapcar (lambda (feature)
                        (tessera-gnus-summary--feature-token feature hidden))
                      selected))
             (text
              (concat gap
                      (mapconcat #'identity tokens separator))))
        (add-text-properties 0 (length text) '(tessera-ui--parent-element entry.features) text)
        text))))

(defun tessera-gnus-summary--flat-author (header)
  "Return the display author from Gnus HEADER."
  (or (car (mail-extract-address-components (mail-header-from header)))
      "Unknown sender"))

(defun tessera-gnus-summary--flat-feature-text (header context-color)
  "Return selected feature glyphs for HEADER using CONTEXT-COLOR."
  (pcase-let* ((`(,selected . ,hidden)
                (tessera-gnus-summary--select-features (tessera-gnus-summary--feature-facts header))))
    (mapconcat (lambda (feature)
                 (tessera-gnus-summary--feature-token feature hidden context-color))
               selected
               (tessera-gnus-summary--glyph-gap 'entry.features.inline-gap))))

(defun tessera-gnus-summary--flat-mark-slots (header)
  "Return the mark slots available for Gnus HEADER."
  (let* ((article (mail-header-number header))
         (data (gnus-data-find article))
         (anchor (and data (gnus-data-pos data)))
         (bounds
          (and anchor
               (tessera-gnus-summary--entry-bounds anchor)))
         facts displays)
    (when bounds
      (dolist (mark tessera-gnus-summary--mark-elements)
        (when-let* ((mark-bounds
                     (tessera-gnus-summary--element-bounds (car bounds) (cdr bounds)
                                                           (list (nth 2 mark))))
                    (position (car mark-bounds))
                    (character (char-after position))
                    (variable
                     (or
                      (get-text-property position 'tessera-gnus-summary--native-mark)
                      (tessera-gnus-summary--mark-variable (car mark) character)))
                    (text
                     (tessera-gnus-summary--mark-display mark-bounds)))
          (unless (string-blank-p text)
            (push variable facts)
            (push (cons variable text) displays)))))
    (tessera-ui-glyph-make-mark-slots 'gnus-summary facts
                                      (lambda (spec)
                                        (alist-get (tessera-ui-glyph-spec-fact spec) displays)))))

(cl-defmethod tessera-ui-make-flat-entry ((_source (eql gnus-summary)) header)
  "Create a flat entry from Gnus Summary HEADER."
  (let* ((article (mail-header-number header))
         (subject-face
          (tessera-gnus-summary--subject-face article))
         (author-face
          (tessera-gnus-summary--author-face article))
         (context-color
          (plist-get author-face :foreground))
         (features
          (tessera-gnus-summary--flat-feature-text header context-color)))
    (tessera-ui-flat-entry-create
     :source 'gnus-summary
     :key (cons gnus-newsgroup-name article)
     :main-slots
     (tessera-gnus-summary--flat-mark-slots header)
     :main-left-segments
     (list (tessera-ui-make-segment
            'entry.subject
            (mail-header-subject header)
            'truncate
            (list :inherit subject-face :weight 'bold)))
     :extra-slots nil
     :extra-left-segments
     (list (tessera-ui-make-segment
            'entry.author
            (tessera-gnus-summary--flat-author header)
            'truncate author-face)
           (tessera-ui-make-segment 'entry.features features 'hide author-face
                                    'entry.separator))
     :extra-right-segments
     (list (tessera-ui-make-segment
            'entry.timestamp
            (tessera-gnus-summary--timestamp header)
            'preserve 'tessera-ui-entry-timestamp)))))

(defun tessera-gnus-summary--mark-variable (slot mark)
  "Return the native variable for MARK in SLOT."
  (catch 'variable
    (dolist (variable
             (cdr (assq slot tessera-gnus-summary--mark-variables)))
      (when (and (boundp variable)
                 (= mark (symbol-value variable)))
        (throw 'variable variable)))))

(defun tessera-gnus-summary--mark-symbol-raw (variable mark)
  "Return the unpadded symbol for native VARIABLE and MARK."
  (let ((native (char-to-string mark)))
    (if-let* ((spec
               (tessera-ui-glyph-spec 'gnus-summary 'mark variable)))
        (tessera-ui-glyph-render spec
                                 tessera-gnus-symbol-style
                                 tessera-gnus-glyph-color-style
                                 'tessera-ui-entry-subject
                                 (alist-get variable tessera-gnus-summary-mark-symbol-alist)
                                 native)
      native)))

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
                          (max width
                               (string-pixel-width (tessera-gnus-summary--mark-symbol-raw
                                                    variable (symbol-value variable))))))))
              (dolist (feature
                       (cons 'overflow
                             (mapcar #'car
                                     tessera-gnus-summary--feature-specs)))
                (setq width
                      (max width
                           (string-pixel-width (tessera-gnus-summary--feature-symbol-raw feature)))))
              width))))

(defun tessera-gnus-summary--glyph-padding (width part)
  "Return a pixel WIDTH space identified as glyph PART."
  (if (<= width 0)
      ""
    (propertize " "
                'display `(space :width (,width))
                'tessera-ui--glyph-part part)))

(defun tessera-gnus-summary--glyph-gap (element)
  "Return a compact space named ELEMENT between glyph cells."
  (let* ((space (tessera-ui-entry-space element))
         (width
          (max 1
               (round (* 0.67 (string-pixel-width space))))))
    (put-text-property 0 (length space) 'display `(space :width (,width)) space)
    space))

(defun tessera-gnus-summary--glyph-cell (glyph)
  "Return GLYPH centered in the common visual glyph cell."
  (if (eq tessera-gnus-symbol-style 'ascii)
      glyph
    (let* ((extra
            (max 0
                 (- (tessera-gnus-summary--glyph-cell-width)
                    (string-pixel-width glyph))))
           (leading (/ extra 2)))
      (concat
       (tessera-gnus-summary--glyph-padding leading 'glyph.leading-fill)
       glyph
       (tessera-gnus-summary--glyph-padding (- extra leading) 'glyph.trailing-fill)))))

(defun tessera-gnus-summary--mark-symbol (variable mark)
  "Return the displayed symbol cell for native VARIABLE and MARK."
  (tessera-gnus-summary--glyph-cell (tessera-gnus-summary--mark-symbol-raw variable mark)))

(defun tessera-gnus-summary--mark-description (variable)
  "Return the native Gnus description of mark VARIABLE."
  (when variable
    (car (split-string (or (documentation-property variable 'variable-documentation)
                           (symbol-name variable))
                       "\n" t))))

(defun tessera-gnus-summary--date-time (header)
  "Return the time represented by Gnus HEADER, or nil."
  (let ((date (mail-header-date header)))
    (and (not (string-empty-p (or date "")))
         (condition-case nil
             (gnus-date-get-time date)
           (error nil)))))

(defun tessera-gnus-summary--month-at-time (time)
  "Return the local calendar month containing TIME."
  (if time
      (let ((decoded (decode-time time)))
        (list (decoded-time-year decoded)
              (decoded-time-month decoded)
              (format-time-string "%B" time)))
    '(nil nil "Unknown date")))

(defun tessera-gnus-summary--month (header)
  "Return the local calendar month represented by Gnus HEADER."
  (tessera-gnus-summary--month-at-time (tessera-gnus-summary--date-time header)))

(defun tessera-gnus-summary--thread-rest (data-list)
  "Return the data following the thread at DATA-LIST."
  (let ((rest (cdr data-list)))
    (while (and rest
                (> (gnus-data-level (car rest)) 0))
      (setq rest (cdr rest)))
    rest))

(defun tessera-gnus-summary--thread-month (thread)
  "Return the latest member month of Gnus THREAD."
  (tessera-gnus-summary--month-at-time (seconds-to-time (gnus-thread-latest-date
                                                         (if (stringp (car-safe thread))
                                                             (cdr thread)
                                                           thread)))))

(cl-defstruct
    (tessera-gnus-summary--month-item (:constructor tessera-gnus-summary--month-item-create)
                                      (:copier nil))
  "One source item used while deriving Gnus month groups."
  month
  data
  data-rest
  thread-rest
  unread
  total)

(defun tessera-gnus-summary--month-item (data-list threads)
  "Return month data for the first item in DATA-LIST and THREADS.

The returned source item represents a complete thread when threading
is active, or one article in a flat summary."
  (let* ((threaded-p (and gnus-show-threads threads))
         (data-rest
          (if threaded-p
              (tessera-gnus-summary--thread-rest data-list)
            (cdr data-list)))
         (thread-rest
          (and threaded-p (cdr threads)))
         (month
          (if threaded-p
              (tessera-gnus-summary--thread-month (car threads))
            (tessera-gnus-summary--month (gnus-data-header (car data-list)))))
         (scan data-list)
         (unread 0)
         (total 0))
    (while (not (eq scan data-rest))
      (setq total (1+ total))
      (when (gnus-data-unread-p (car scan))
        (setq unread (1+ unread)))
      (setq scan (cdr scan)))
    (tessera-gnus-summary--month-item-create
     :month month
     :data (car data-list)
     :data-rest data-rest
     :thread-rest thread-rest
     :unread unread
     :total total)))

(defun tessera-gnus-summary--month-groups (buffer)
  "Derive standard month groups from Gnus Summary BUFFER."
  (with-current-buffer buffer
    (let ((data-list gnus-newsgroup-data)
          (threads (and gnus-show-threads
                        gnus-newsgroup-threads))
          groups)
      (while data-list
        (let* ((item
                (tessera-gnus-summary--month-item data-list threads))
               (month
                (tessera-gnus-summary--month-item-month item))
               (key (butlast month))
               (current (car groups)))
          (if (and current
                   (equal key
                          (tessera-ui-month-group-key current)))
              (let ((statistics
                     (tessera-ui-month-group-statistics current)))
                (setf (tessera-ui-month-group-items current)
                      (cons (tessera-gnus-summary--month-item-data item)
                            (tessera-ui-month-group-items current))
                      (tessera-ui-month-statistics-unread statistics)
                      (+ (tessera-ui-month-statistics-unread statistics)
                         (tessera-gnus-summary--month-item-unread item))
                      (tessera-ui-month-statistics-total statistics)
                      (+ (tessera-ui-month-statistics-total statistics)
                         (tessera-gnus-summary--month-item-total item))))
            (push (tessera-ui-month-group-create
                   :source 'gnus-summary
                   :key key
                   :year (car month)
                   :month-name (nth 2 month)
                   :statistics
                   (tessera-ui-month-statistics-create
                    :unread
                    (tessera-gnus-summary--month-item-unread item)
                    :total
                    (tessera-gnus-summary--month-item-total item))
                   :items
                   (list (tessera-gnus-summary--month-item-data item)))
                  groups))
          (setq data-list
                (tessera-gnus-summary--month-item-data-rest item)
                threads
                (tessera-gnus-summary--month-item-thread-rest item))))
      (dolist (group groups)
        (setf (tessera-ui-month-group-items group)
              (nreverse (tessera-ui-month-group-items group))))
      (nreverse groups))))

(tessera-ui-month-register 'gnus-summary #'tessera-gnus-summary--month-groups)

(defun tessera-gnus-summary--month-group-for-key (key)
  "Return the current standard month group identified by KEY."
  (seq-find (lambda (group)
              (equal key (tessera-ui-month-group-key group)))
            tessera-gnus-summary--month-groups))

(defun tessera-gnus-summary--month-overlay (key property)
  "Return the month overlay for KEY marked by PROPERTY."
  (catch 'overlay
    (dolist (overlay tessera-gnus-summary--month-overlays)
      (when (and (overlay-buffer overlay)
                 (overlay-get overlay property)
                 (equal key
                        (overlay-get overlay
                                     'tessera-ui--month-key)))
        (throw 'overlay overlay)))))

(defun tessera-gnus-summary--make-month-overlay (start end key property)
  "Make a month overlay from START to END for KEY and PROPERTY."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'tessera-ui--month-key key)
    (overlay-put overlay property t)
    (push overlay tessera-gnus-summary--month-overlays)
    overlay))

(defun tessera-gnus-summary--delete-month-overlays ()
  "Delete every month overlay owned by the current buffer."
  (mapc #'delete-overlay tessera-gnus-summary--month-overlays)
  (setq tessera-gnus-summary--month-overlays nil))

(defun tessera-gnus-summary--month-help (collapsed)
  "Return help text for a month heading in state COLLAPSED."
  (format "mouse-1, RET, TAB: %s month"
          (if collapsed "Expand" "Collapse")))

(defun tessera-gnus-summary--month-heading-bounds (key)
  "Return bounds of the month heading identified by KEY."
  (let ((position (point-min)))
    (while (and (< position (point-max))
                (not (equal key
                            (get-text-property position 'tessera-ui--month-key))))
      (setq position
            (next-single-property-change position 'tessera-ui--month-key nil (point-max))))
    (when (< position (point-max))
      (save-excursion
        (goto-char position)
        (cons (line-beginning-position)
              (min (point-max)
                   (1+ (line-end-position))))))))

(defun tessera-gnus-summary--month-body-bounds (key)
  "Return article body bounds of the month identified by KEY."
  (when-let* ((heading
               (tessera-gnus-summary--month-heading-bounds key)))
    (let* ((start
            (save-excursion
              (goto-char (car heading))
              (forward-line 1)
              (point)))
           (position start))
      (while (and (< position (point-max))
                  (not (get-text-property position 'tessera-ui--month-key)))
        (setq position
              (next-single-property-change position 'tessera-ui--month-key nil (point-max))))
      (cons start position))))

(defun tessera-gnus-summary--set-month-help (key collapsed)
  "Set help text for month KEY according to COLLAPSED."
  (when-let* ((bounds
               (tessera-gnus-summary--month-heading-bounds key)))
    (with-silent-modifications
      (put-text-property (car bounds) (cdr bounds) 'help-echo (tessera-gnus-summary--month-help collapsed)))))

(defun tessera-gnus-summary--timestamp (header)
  "Return an age-sensitive date for Gnus HEADER."
  (let* ((date (mail-header-date header))
         (time (tessera-gnus-summary--date-time header))
         (text
          (and time
               (if (or (not tessera-gnus-summary-use-semantic-dates)
                       (time-less-p (current-time) time))
                   (format-time-string tessera-gnus-summary-date-format time)
                 (let ((gnus-user-date-format-alist
                        tessera-gnus-summary-semantic-date-formats))
                   (gnus-user-date date)))))
         (missing-p (not text))
         (text (or text "Unknown")))
    (propertize text
                'face (list 'tessera-ui-entry-timestamp nil)
                'gnus-face t
                'help-echo (or date text)
                'tessera-ui--element
                (if missing-p
                    'entry.timestamp.placeholder
                  'entry.timestamp))))

(defun tessera-gnus-summary--total ()
  "Return Gnus's estimated total for the current group."
  (if gnus-newsgroup-active
      (range-length (list gnus-newsgroup-active))
    (length gnus-newsgroup-articles)))

(defun tessera-gnus-summary--statistics ()
  "Return metrics for the current native Gnus Summary snapshot."
  (let ((unread (length gnus-newsgroup-unreads))
        (visible (length gnus-newsgroup-data))
        (total (tessera-gnus-summary--total)))
    (tessera-ui-header-line-standard-metrics unread visible 'visible "visible" total)))

(defun tessera-gnus-summary--format-status ()
  "Return the presentation of the current Summary operation status."
  (tessera-gnus-status tessera-gnus-summary--status-state
                       (and tessera-gnus-summary--fetch-total
                            (cons tessera-gnus-summary--fetch-current
                                  tessera-gnus-summary--fetch-total))
                       tessera-gnus-summary--fetch-failed
                       (pcase tessera-gnus-summary--status-state
                         ('processing "Gnus is fetching article headers")
                         ('fail
                          (concat "The last fetch failed; "
                                  "mouse-1: Get new articles"))
                         (_ "mouse-1: Get new articles"))
                       #'tessera-gnus-summary--insert-new-articles))

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

(defun tessera-gnus-summary--header-status-segment (buffer)
  "Derive the registered status segment from Summary BUFFER."
  (with-current-buffer buffer
    (tessera-ui-header-line-status-segment (tessera-gnus-summary--format-status))))

(defun tessera-gnus-summary--header-query-segment (buffer)
  "Derive the registered query segment from Summary BUFFER."
  (with-current-buffer buffer
    (tessera-ui-header-line-query-segment (tessera-ui-header-scope-create
                                           :kind 'group
                                           :label "GROUP"
                                           :value gnus-newsgroup-name))))

(defun tessera-gnus-summary--header-statistics-segment (buffer)
  "Derive the registered statistics segment from Summary BUFFER."
  (with-current-buffer buffer
    (tessera-ui-header-line-statistics-segment (tessera-gnus-summary--statistics))))

(tessera-ui-header-line-register 'gnus-summary
                                 :left
                                 '((status . tessera-gnus-summary--header-status-segment)
                                   (query . tessera-gnus-summary--header-query-segment))
                                 :right
                                 '((statistics .
                                               tessera-gnus-summary--header-statistics-segment)))

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

(defun tessera-gnus-summary--track-fetch (orig-fun articles &optional limit force-new dependencies)
  "Call ORIG-FUN for ARTICLES while displaying fetch progress.

LIMIT, FORCE-NEW, and DEPENDENCIES are passed to
`gnus-fetch-headers'."
  (let ((buffer (current-buffer)))
    (if tessera-gnus-summary--installed-p
        (let ((tessera-gnus-summary--fetch-buffer buffer))
          (tessera-gnus-summary--begin-fetch (length articles))
          (condition-case err
              (let ((headers
                     (funcall orig-fun
                              articles limit force-new dependencies)))
                (with-current-buffer buffer
                  (tessera-gnus-summary--finish-fetch (length headers) force-new))
                headers)
            ((error quit)
             (with-current-buffer buffer
               (tessera-gnus-summary--fail-fetch))
             (signal (car err) (cdr err)))))
      (funcall orig-fun articles limit force-new dependencies))))

(defun tessera-gnus-summary--add-fetch-advice ()
  "Add advice used to present native Gnus fetch progress."
  (advice-add 'gnus-fetch-headers :around #'tessera-gnus-summary--track-fetch)
  (advice-add 'nnheader-parse-nov :filter-return #'tessera-gnus-summary--record-header)
  (advice-add 'nnheader-parse-head :filter-return #'tessera-gnus-summary--record-header))

(defun tessera-gnus-summary--remove-fetch-advice ()
  "Remove advice used to present native Gnus fetch progress."
  (advice-remove 'gnus-fetch-headers #'tessera-gnus-summary--track-fetch)
  (advice-remove 'nnheader-parse-nov #'tessera-gnus-summary--record-header)
  (advice-remove 'nnheader-parse-head #'tessera-gnus-summary--record-header))

(defun tessera-gnus-summary--update-mark (orig-fun &rest args)
  "Call ORIG-FUN with ARGS at the article's primary row.

Only normalize point when Tessera's two-row format is active."
  (let* ((article
          (and (eq gnus-summary-line-format
                   tessera-gnus-summary--line-format)
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
        (tessera-gnus-summary--refresh-presentations-at (list position))))))

(defun tessera-gnus-summary--entry-bounds (anchor)
  "Return the entry bounds containing ANCHOR."
  (save-excursion
    (goto-char anchor)
    (let ((start (line-beginning-position)))
      (forward-line (if (eq gnus-summary-line-format
                            tessera-gnus-summary--line-format)
                        2
                      1))
      (cons start (point)))))

(defun tessera-gnus-summary--set-thread-tree-face (start end)
  "Set the connector face between START and END."
  (save-restriction
    (narrow-to-region start end)
    (save-excursion
      (goto-char (point-min))
      (while-let
          ((match
            (text-property-search-forward 'tessera-ui--element)))
        (when (memq (prop-match-value match)
                    '(thread.indentation thread.connector))
          (put-text-property (prop-match-beginning match) (prop-match-end match) 'face 'tessera-ui-thread-connector))))))

(defun tessera-gnus-summary--article-face (anchor)
  "Return the Gnus Summary face for the article at ANCHOR."
  (save-excursion
    (goto-char anchor)
    (let* ((position (line-beginning-position))
           (face
            (gnus-get-text-property-excluding-characters-with-faces position 'face)))
      (when (memq face '(nil default))
        (gnus-summary-highlight-line)
        (setq face
              (gnus-get-text-property-excluding-characters-with-faces position 'face)))
      face)))

(defun tessera-gnus-summary--update-entry (&optional defer-presentation)
  "Update presentation properties on the current entry.

When DEFER-PRESENTATION is non-nil, leave window presentation updates
to the caller."
  (when-let* ((article (get-text-property (point) 'gnus-number))
              (data (gnus-data-find article))
              (anchor (gnus-data-pos data))
              (bounds (tessera-gnus-summary--entry-bounds anchor))
              (start (car bounds))
              (end (cdr bounds)))
    (let* ((face (get-text-property anchor 'face))
           (article-face
            (tessera-gnus-summary--article-face anchor))
           (unread (gnus-data-unread-p data))
           (threaded
            (eq gnus-summary-line-format
                tessera-gnus-summary--thread-line-format))
           (inhibit-read-only t)
           mark-start mark-end)
      (gnus-put-text-property-excluding-characters-with-faces start end 'face face)
      (add-text-properties start end (list 'keymap tessera-gnus-summary--entry-map))
      (let ((position start))
        (while (< position end)
          (let ((next
                 (next-single-property-change position 'tessera-ui--parent-element nil end)))
            (unless (get-text-property position 'tessera-ui--parent-element)
              (put-text-property position next 'tessera-ui--parent-element 'entry))
            (setq position next))))
      (put-text-property start end 'mouse-face nil)
      (put-text-property start (1- end) 'mouse-face 'highlight)
      (dolist (mark tessera-gnus-summary--mark-elements)
        (when-let* ((offset
                     (cdr (assq (nth 1 mark)
                                gnus-summary-mark-positions))))
          (let* ((position (+ anchor offset -1))
                 (character (char-after position))
                 (variable
                  (and character
                       (tessera-gnus-summary--mark-variable (car mark) character))))
            (when (< position end)
              (setq mark-start
                    (min (or mark-start position) position)
                    mark-end
                    (max (or mark-end position) (1+ position)))
              (add-text-properties position (1+ position)
                                   (list 'tessera-ui--element (nth 2 mark)
                                         'tessera-gnus-summary--native-mark variable
                                         'help-echo
                                         (tessera-gnus-summary--mark-description variable)))))))
      (when mark-start
        (put-text-property mark-start mark-end 'tessera-ui--parent-element 'entry.state-rail))
      (when-let* ((subject
                   (and
                    (not threaded)
                    (tessera-gnus-summary--element-bounds start end '(entry.subject entry.subject.placeholder)))))
        (put-text-property (car subject) (cdr subject) 'face (list :inherit article-face :weight 'bold)))
      (when-let* ((author
                   (tessera-gnus-summary--element-bounds start end '(entry.author entry.author.placeholder))))
        (let ((author-face
               (tessera-ui-entry-author-face article-face unread)))
          (put-text-property (car author) (cdr author) 'face author-face)
          (tessera-gnus-summary--set-feature-context-color start end (car author))))
      (when-let* ((timestamp
                   (tessera-gnus-summary--element-bounds start end '(entry.timestamp entry.timestamp.placeholder))))
        (let ((timestamp-face
               (if unread
                   'tessera-ui-entry-timestamp-unread
                 'tessera-ui-entry-timestamp)))
          (put-text-property (car timestamp) (cdr timestamp) 'face (list timestamp-face face))))
      (when threaded
        (tessera-gnus-summary--set-thread-tree-face start end))
      (unless defer-presentation
        (tessera-gnus-summary--refresh-presentations-at (list anchor))))))

(defun tessera-gnus-summary--update-entries ()
  "Apply Tessera properties to every native Summary entry."
  (save-excursion
    (dolist (data gnus-newsgroup-data)
      (goto-char (gnus-data-pos data))
      (tessera-gnus-summary--update-entry t)))
  (setq tessera-gnus-summary--thread-groups
        (and gnus-show-threads
             (tessera-gnus-summary--make-thread-groups)))
  (setq tessera-gnus-summary--thread-model-ready-p
        gnus-show-threads)
  (if tessera-gnus-summary-group-by-month
      (tessera-gnus-summary--insert-month-headings)
    (setq tessera-gnus-summary--month-groups nil)
    (remove-from-invisibility-spec 'tessera-gnus-summary-month)))

(defun tessera-gnus-summary--make-thread-groups ()
  "Return a map from articles to visible native Gnus threads."
  (let ((data-list
         (sort (seq-filter (lambda (data)
                             (number-or-marker-p (gnus-data-pos data)))
                           gnus-newsgroup-data)
               (lambda (left right)
                 (< (gnus-data-pos left) (gnus-data-pos right)))))
        (groups (make-hash-table :test #'eql))
        threads
        group)
    (dolist (data data-list)
      (when (and group (zerop (or (gnus-data-level data) 0)))
        (push (nreverse group) threads)
        (setq group nil))
      (push data group))
    (when group
      (push (nreverse group) threads))
    (dolist (thread threads)
      (dolist (member thread)
        (puthash (gnus-data-number member) thread groups)))
    groups))

(defun tessera-gnus-summary--thread-known-count (data)
  "Return the native known thread count stored on Gnus DATA."
  (save-excursion
    (goto-char (gnus-data-pos data))
    (let* ((start (line-beginning-position))
           (end (line-end-position))
           (position
            (text-property-not-all start end 'tessera-ui-thread-known-count nil)))
      (and position
           (get-text-property position 'tessera-ui-thread-known-count)))))

(defun tessera-gnus-summary--thread-subject (header)
  "Return the normalized thread subject from Gnus HEADER."
  (tessera-gnus-summary--field (gnus-simplify-subject-fully
                                (or (mail-header-subject header) ""))
                               "(No subject)"
                               'thread.subject
                               'entry.subject.placeholder))

(defun tessera-gnus-summary--thread-root-data-list (data)
  "Return Gnus data beginning with the thread containing DATA."
  (when tessera-gnus-summary--thread-groups
    (gethash (gnus-data-number data)
             tessera-gnus-summary--thread-groups)))

(defun tessera-gnus-summary--thread-statistic (data-list rest)
  "Return statistics for members before REST in DATA-LIST."
  (let ((members 0)
        (unread 0)
        (scan data-list))
    (while (not (eq scan rest))
      (setq members (1+ members))
      (when (gnus-data-unread-p (car scan))
        (setq unread (1+ unread)))
      (setq scan (cdr scan)))
    (let* ((known
            (max members
                 (or (tessera-gnus-summary--thread-known-count
                      (car data-list))
                     members)))
           (exactness
            (if (> known members) 'lower-bound 'exact)))
      (tessera-ui-thread-statistic-create
       :unread unread
       :visible members
       :known known
       :exactness exactness))))

(defun tessera-gnus-summary--thread-connector (data)
  "Return the semantic connector stored on Gnus DATA."
  (save-excursion
    (goto-char (gnus-data-pos data))
    (let* ((start (line-beginning-position))
           (end (line-end-position))
           (position
            (text-property-not-all start end 'tessera-ui-thread-connector nil)))
      (or (and position
               (get-text-property position 'tessera-ui-thread-connector))
          (let ((level (or (gnus-data-level data) 0)))
            (tessera-ui-thread-connector-create
             :ancestors
             (make-list (max 0 (1- level)) nil)
             :kind
             (if (zerop level) 'root 'last)))))))

(defun tessera-gnus-summary--thread-member (data)
  "Return a semantic thread member from Gnus DATA."
  (let* ((header (gnus-data-header data))
         (article (mail-header-number header))
         (author-face
          (tessera-gnus-summary--author-face article))
         (context-color (plist-get author-face :foreground))
         (features
          (tessera-gnus-summary--flat-feature-text header context-color)))
    (tessera-ui-thread-member-create
     :key (cons gnus-newsgroup-name article)
     :slots (tessera-gnus-summary--flat-mark-slots header)
     :connector
     (tessera-gnus-summary--thread-connector data)
     :left-segments
     (list (tessera-ui-make-segment
            'thread.member.author
            (tessera-gnus-summary--flat-author header)
            'truncate author-face)
           (tessera-ui-make-segment 'thread.member.features features 'hide author-face
                                    'entry.separator))
     :right-segments
     (list (tessera-ui-make-segment
            'thread.member.timestamp
            (tessera-gnus-summary--timestamp header)
            'preserve 'tessera-ui-entry-timestamp)))))

(cl-defmethod tessera-ui-make-thread ((_source (eql gnus-summary)) data-list)
  "Create a semantic thread from Gnus DATA-LIST."
  (let* ((rest
          (tessera-gnus-summary--thread-rest data-list))
         (statistic
          (tessera-gnus-summary--thread-statistic data-list rest))
         (metric (tessera-ui-format-thread-statistic statistic))
         (root (car data-list))
         (subject
          (tessera-gnus-summary--thread-subject (gnus-data-header root)))
         (scan data-list)
         members)
    (while (not (eq scan rest))
      (push (tessera-gnus-summary--thread-member (car scan))
            members)
      (setq scan (cdr scan)))
    (tessera-ui-thread-create
     :source 'gnus-summary
     :key
     (cons gnus-newsgroup-name (gnus-data-number root))
     :statistic statistic
     :main-left-segments
     (list (tessera-ui-make-segment
            'thread.subject subject 'truncate
            (tessera-gnus-summary--thread-subject-face metric)))
     :members (nreverse members))))

(defun tessera-gnus-summary--thread-subject-face (metric)
  "Return the subject face represented by thread METRIC."
  (if (text-property-any 0 (length metric) 'tessera-ui--element
                         'thread.metric.unread-count metric)
      'tessera-gnus-summary-thread-subject-unread
    'tessera-gnus-summary-thread-subject-read))

(defun tessera-gnus-summary--update-thread-metric (data)
  "Update the metric for the thread containing Gnus DATA."
  (when-let* ((data-list
               (tessera-gnus-summary--thread-root-data-list data)))
    (let ((rest
           (tessera-gnus-summary--thread-rest data-list))
          positions)
      (while (not (eq data-list rest))
        (when-let* ((position (gnus-data-pos (car data-list))))
          (push position positions))
        (setq data-list (cdr data-list)))
      (tessera-gnus-summary--refresh-presentations-at (nreverse positions)))))

(defun tessera-gnus-summary--update-current-metric ()
  "Update the thread metric for the current Gnus article."
  (when-let* ((article
               (get-text-property (point) 'gnus-number))
              (data (gnus-data-find article)))
    (when (and tessera-gnus-summary--thread-model-ready-p
               (eq gnus-summary-line-format
                   tessera-gnus-summary--thread-line-format)
               (number-or-marker-p (gnus-data-pos data)))
      (tessera-gnus-summary--update-thread-metric data)))
  (when (and tessera-gnus-summary-group-by-month
             tessera-gnus-summary--month-groups)
    (setq tessera-gnus-summary--month-groups
          (tessera-ui-make-month-groups 'gnus-summary (current-buffer)))
    (tessera-gnus-summary--update-window-presentations)))

(defun tessera-gnus-summary--insert-month-headings ()
  "Insert month headings in the current Summary buffer."
  (tessera-gnus-summary--delete-month-overlays)
  (when tessera-gnus-summary--line-format-installed-p
    (setq tessera-gnus-summary--month-groups
          (tessera-ui-make-month-groups 'gnus-summary (current-buffer)))
    (unless (or (eq buffer-invisibility-spec t)
                (memq 'tessera-gnus-summary-month
                      buffer-invisibility-spec))
      (add-to-invisibility-spec 'tessera-gnus-summary-month))
    (let ((groups tessera-gnus-summary--month-groups)
          (inhibit-read-only t))
      (while groups
        (let* ((group (car groups))
               (data (car (tessera-ui-month-group-items group)))
               (next (cadr groups))
               (key (tessera-ui-month-group-key group))
               (collapsed
                (and
                 (member key tessera-gnus-summary--collapsed-months)
                 t))
               (heading
                (tessera-ui-format-month-heading group collapsed)))
          (when (and data
                     (number-or-marker-p (gnus-data-pos data)))
            (goto-char (gnus-data-pos data))
            (beginning-of-line)
            (let ((start (point)))
              (insert heading "\n")
              (add-text-properties start (point)
                                   (list 'keymap tessera-gnus-summary--month-map
                                         'mouse-face 'tessera-ui-month-heading-highlight
                                         'help-echo
                                         (tessera-gnus-summary--month-help collapsed)
                                         'tessera-ui--month-key key))
              (let* ((body-start (point))
                     (data-list
                      (memq data gnus-newsgroup-data)))
                (when data-list
                  (gnus-data-update-list data-list (- body-start start)))
                (when collapsed
                  (let ((body-end
                         (if next
                             (save-excursion
                               (goto-char (gnus-data-pos
                                           (car (tessera-ui-month-group-items next))))
                               (line-beginning-position))
                           (point-max))))
                    (let ((overlay
                           (tessera-gnus-summary--make-month-overlay body-start body-end key 'tessera-ui--month-fold)))
                      (overlay-put overlay 'invisible
                                   'tessera-gnus-summary-month)
                      (overlay-put overlay 'isearch-open-invisible
                                   #'tessera-gnus-summary--open-month))))))
            (setq groups (cdr groups))))))))

(defun tessera-gnus-summary--open-month (overlay)
  "Expand the month hidden by OVERLAY."
  (when (overlay-buffer overlay)
    (let ((key (overlay-get overlay 'tessera-ui--month-key)))
      (setq tessera-gnus-summary--collapsed-months
            (delete key tessera-gnus-summary--collapsed-months)
            tessera-gnus-summary--month-overlays
            (delq overlay tessera-gnus-summary--month-overlays))
      (delete-overlay overlay)
      (tessera-gnus-summary--set-month-help key nil)
      (tessera-gnus-summary--update-selected-entry)
      (tessera-gnus-summary--update-window-presentations))))

(defun tessera-gnus-summary--collapse-month (key)
  "Collapse the month identified by KEY."
  (unless (tessera-gnus-summary--month-overlay
           key 'tessera-ui--month-fold)
    (when-let* ((bounds
                 (tessera-gnus-summary--month-body-bounds key)))
      (let ((overlay
             (tessera-gnus-summary--make-month-overlay (car bounds) (cdr bounds) key
                                                       'tessera-ui--month-fold))
            (selected-position
             (and gnus-newsgroup-selected-overlay
                  (overlay-start gnus-newsgroup-selected-overlay))))
        (overlay-put overlay 'invisible
                     'tessera-gnus-summary-month)
        (overlay-put overlay 'isearch-open-invisible
                     #'tessera-gnus-summary--open-month)
        (unless (member key
                        tessera-gnus-summary--collapsed-months)
          (push key tessera-gnus-summary--collapsed-months))
        (when (and selected-position
                   (<= (car bounds) selected-position)
                   (< selected-position (cdr bounds)))
          (when-let* ((heading
                       (tessera-gnus-summary--month-heading-bounds key)))
            (move-overlay gnus-newsgroup-selected-overlay
                          (car heading) (cdr heading) (current-buffer))))
        (tessera-gnus-summary--set-month-help key t)
        (tessera-gnus-summary--update-window-presentations)))))

(defun tessera-gnus-summary--toggle-month ()
  "Toggle the month heading at point."
  (interactive)
  (let ((key (get-text-property (point) 'tessera-ui--month-key)))
    (unless key
      (user-error "Point is not on a month heading"))
    (if-let* ((overlay
               (tessera-gnus-summary--month-overlay key 'tessera-ui--month-fold)))
        (tessera-gnus-summary--open-month overlay)
      (tessera-gnus-summary--collapse-month key))))

(defun tessera-gnus-summary--mouse-toggle-month (event)
  "Toggle the month heading clicked by mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (tessera-gnus-summary--toggle-month))

(defun tessera-gnus-summary--next-month-line ()
  "Move to the next visible line from a month heading."
  (interactive)
  (let* ((key (get-text-property (point) 'tessera-ui--month-key))
         (overlay
          (and key
               (tessera-gnus-summary--month-overlay key 'tessera-ui--month-fold))))
    (if overlay
        (goto-char (overlay-end overlay))
      (call-interactively #'next-line))))

(defun tessera-gnus-summary--previous-month-line ()
  "Move to the previous visible line from a month heading."
  (interactive)
  (let ((position (line-beginning-position))
        overlay)
    (when (> position (point-min))
      (catch 'found
        (dolist (candidate (overlays-at (1- position)))
          (when (overlay-get candidate 'tessera-ui--month-fold)
            (setq overlay candidate)
            (throw 'found t)))))
    (if-let* ((key (and overlay
                        (overlay-get overlay
                                     'tessera-ui--month-key)))
              (bounds
               (tessera-gnus-summary--month-heading-bounds key)))
        (goto-char (car bounds))
      (call-interactively #'previous-line))))

(defun tessera-gnus-summary--hide-display-arrow ()
  "Hide the native Summary display arrow in the current buffer."
  (when (markerp overlay-arrow-position)
    (set-marker overlay-arrow-position nil)))

(defun tessera-gnus-summary--set-article-display-arrow (orig-fun position)
  "Call ORIG-FUN for POSITION unless a Tessera layout is active."
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
                   (cdr (tessera-gnus-summary--entry-bounds anchor))))
              (move-overlay gnus-newsgroup-selected-overlay
                            start end (current-buffer))
              (overlay-put gnus-newsgroup-selected-overlay
                           'face 'tessera-ui-entry-current)
              (overlay-put gnus-newsgroup-selected-overlay
                           'tessera-ui--element 'entry.current)
              (setq tessera-gnus-summary--selected-entry-anchor
                    anchor)
              (unless (equal previous anchor)
                (tessera-gnus-summary--refresh-presentations-at (list previous anchor)))))
        (overlay-put gnus-newsgroup-selected-overlay
                     'face gnus-summary-selected-face)
        (overlay-put gnus-newsgroup-selected-overlay
                     'tessera-ui--element nil)
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

(defun tessera-gnus-summary--install-thread-tree-settings ()
  "Install Tessera thread tree settings in the current buffer."
  (unless tessera-gnus-summary--original-thread-tree-settings
    (setq tessera-gnus-summary--original-thread-tree-settings
          (mapcar (lambda (setting)
                    (let ((variable (car setting)))
                      (list variable
                            (local-variable-p variable)
                            (symbol-value variable))))
                  tessera-gnus-summary--thread-tree-settings)))
  (dolist (setting tessera-gnus-summary--thread-tree-settings)
    (set (make-local-variable (car setting)) (cdr setting))))

(defun tessera-gnus-summary--restore-thread-tree-settings ()
  "Restore the native thread tree settings in the current buffer."
  (when tessera-gnus-summary--original-thread-tree-settings
    (dolist
        (saved
         tessera-gnus-summary--original-thread-tree-settings)
      (let* ((variable (nth 0 saved))
             (local-p (nth 1 saved))
             (value (nth 2 saved))
             (installed
              (alist-get variable tessera-gnus-summary--thread-tree-settings)))
        (when (equal (symbol-value variable) installed)
          (if local-p
              (set variable value)
            (kill-local-variable variable)))))
    (setq tessera-gnus-summary--original-thread-tree-settings nil)))

(defun tessera-gnus-summary--install-line-format (format)
  "Install Tessera line FORMAT in the current Summary buffer."
  (unless tessera-gnus-summary--line-format-installed-p
    (setq tessera-gnus-summary--original-line-format-local-p
          (local-variable-p 'gnus-summary-line-format)
          tessera-gnus-summary--original-line-format
          gnus-summary-line-format
          tessera-gnus-summary--line-format-installed-p t))
  (unless (eq gnus-summary-line-format format)
    (setq-local gnus-summary-line-format format)
    (tessera-gnus-summary--update-format-specification)
    (tessera-gnus-summary--hide-display-arrow)))

(defun tessera-gnus-summary--restore-line-format ()
  "Restore the native line format in the current Summary buffer."
  (when tessera-gnus-summary--line-format-installed-p
    (when (memq gnus-summary-line-format
                (list tessera-gnus-summary--line-format
                      tessera-gnus-summary--thread-line-format))
      (if tessera-gnus-summary--original-line-format-local-p
          (setq-local gnus-summary-line-format
                      tessera-gnus-summary--original-line-format)
        (kill-local-variable 'gnus-summary-line-format)))
    (setq tessera-gnus-summary--line-format-installed-p nil
          tessera-gnus-summary--original-line-format nil
          tessera-gnus-summary--original-line-format-local-p nil)
    (tessera-gnus-summary--update-format-specification)
    t))

(defun tessera-gnus-summary--install-sort-functions (variable functions)
  "Install FUNCTIONS as the buffer-local sort VARIABLE."
  (unless (eq tessera-gnus-summary--sort-variable variable)
    (tessera-gnus-summary--restore-sort-functions))
  (unless tessera-gnus-summary--sort-variable
    (setq tessera-gnus-summary--original-sort-functions-local-p
          (local-variable-p variable)
          tessera-gnus-summary--original-sort-functions
          (symbol-value variable)
          tessera-gnus-summary--sort-variable variable)
    (set (make-local-variable variable) functions)))

(defun tessera-gnus-summary--preserve-subthread-sort-functions ()
  "Preserve native subthread sorting in the current buffer."
  (when (and
         (not tessera-gnus-summary--subthread-sort-installed-p)
         (eq gnus-subthread-sort-functions
             'gnus-thread-sort-functions))
    (setq tessera-gnus-summary--original-subthread-sort-local-p
          (local-variable-p 'gnus-subthread-sort-functions)
          tessera-gnus-summary--subthread-sort-installed-p t)
    (setq-local gnus-subthread-sort-functions
                tessera-gnus-summary--original-sort-functions)))

(defun tessera-gnus-summary--restore-subthread-sort-functions ()
  "Restore native subthread sorting in the current buffer."
  (when tessera-gnus-summary--subthread-sort-installed-p
    (when (equal gnus-subthread-sort-functions
                 tessera-gnus-summary--original-sort-functions)
      (if tessera-gnus-summary--original-subthread-sort-local-p
          (setq-local gnus-subthread-sort-functions
                      'gnus-thread-sort-functions)
        (kill-local-variable 'gnus-subthread-sort-functions)))
    (setq tessera-gnus-summary--subthread-sort-installed-p nil
          tessera-gnus-summary--original-subthread-sort-local-p nil)))

(defun tessera-gnus-summary--restore-sort-functions ()
  "Restore native sorting in the current Summary buffer."
  (when tessera-gnus-summary--sort-variable
    (tessera-gnus-summary--restore-subthread-sort-functions)
    (let* ((variable tessera-gnus-summary--sort-variable)
           (installed
            (if (eq variable 'gnus-thread-sort-functions)
                tessera-gnus-summary--thread-sort-functions
              tessera-gnus-summary--article-sort-functions)))
      (when (equal (symbol-value variable) installed)
        (if tessera-gnus-summary--original-sort-functions-local-p
            (set variable
                 tessera-gnus-summary--original-sort-functions)
          (kill-local-variable variable))))
    (setq tessera-gnus-summary--sort-variable nil
          tessera-gnus-summary--original-sort-functions nil
          tessera-gnus-summary--original-sort-functions-local-p nil)
    t))

(defun tessera-gnus-summary--update-line-format ()
  "Install the Tessera format for the current Summary layout."
  (setq tessera-gnus-summary--thread-model-ready-p nil
        tessera-gnus-summary--thread-groups nil)
  (if gnus-show-threads
      (progn
        (tessera-gnus-summary--delete-window-overlays)
        (tessera-gnus-summary--delete-month-overlays)
        (tessera-gnus-summary--install-thread-tree-settings)
        (tessera-gnus-summary--install-line-format tessera-gnus-summary--thread-line-format)
        (tessera-gnus-summary--install-sort-functions 'gnus-thread-sort-functions
                                                      tessera-gnus-summary--thread-sort-functions)
        (tessera-gnus-summary--preserve-subthread-sort-functions))
    (tessera-gnus-summary--restore-thread-tree-settings)
    (tessera-gnus-summary--install-line-format tessera-gnus-summary--line-format)
    (tessera-gnus-summary--install-sort-functions 'gnus-article-sort-functions
                                                  tessera-gnus-summary--article-sort-functions)))

(defun tessera-gnus-summary--regenerate ()
  "Regenerate the current Summary while preserving its article."
  (let ((article (and gnus-newsgroup-data
                      (gnus-summary-article-number))))
    (setq tessera-gnus-summary--glyph-width nil
          tessera-gnus-summary--flat-rail nil)
    (gnus-summary-prepare)
    (when article
      (gnus-summary-goto-subject article nil t))))

(defun tessera-gnus-summary-refresh ()
  "Refresh the Tessera presentation in the current Summary buffer."
  (when (and tessera-gnus-summary--installed-p
             tessera-gnus-summary--line-format-installed-p
             gnus-newsgroup-prepared)
    (tessera-gnus-summary--regenerate)))

(defun tessera-gnus-summary--element-bounds (start end elements)
  "Return bounds of the first named element between START and END.

ELEMENTS is a list of accepted `tessera-ui--element' values."
  (let ((position start))
    (while (and (< position end)
                (not (memq (get-text-property position 'tessera-ui--element)
                           elements)))
      (setq position
            (next-single-property-change position 'tessera-ui--element nil end)))
    (when (< position end)
      (cons position
            (next-single-property-change position 'tessera-ui--element nil end)))))

(defun tessera-gnus-summary--presentation-state ()
  "Return the window presentation state for the current Summary."
  (or tessera-gnus-summary--presentations
      (setq tessera-gnus-summary--presentations
            (tessera-ui-window-presentations-create))))

(defun tessera-gnus-summary--make-before-string-overlay (start end display window)
  "Replace text from START to END with DISPLAY in WINDOW."
  (tessera-ui-window-presentations-add (tessera-gnus-summary--presentation-state)
                                       (tessera-ui-make-window-overlay start end window
                                                                       :display ""
                                                                       :before-string display
                                                                       :properties
                                                                       (list 'tessera-ui--window-presentation t
                                                                             'tessera-ui--presentation-anchor
                                                                             tessera-gnus-summary--presentation-anchor))))

(defun tessera-gnus-summary--delete-presentations (predicate)
  "Delete presentation overlays satisfying PREDICATE."
  (tessera-ui-window-presentations-delete (tessera-gnus-summary--presentation-state)
                                          predicate))

(defun tessera-gnus-summary--delete-window-overlays ()
  "Delete window-local display overlays in the current Summary."
  (tessera-gnus-summary--delete-presentations #'always))

(defun tessera-gnus-summary--delete-presentations-at (anchors)
  "Delete window-local presentations at ANCHORS."
  (tessera-gnus-summary--delete-presentations (lambda (overlay)
                                                (member (overlay-get overlay 'tessera-ui--presentation-anchor)
                                                        anchors))))

(defun tessera-gnus-summary--delete-presentations-outside (window start end)
  "Delete WINDOW presentations with anchors outside START and END."
  (tessera-gnus-summary--delete-presentations (lambda (overlay)
                                                (let ((anchor
                                                       (overlay-get overlay 'tessera-ui--presentation-anchor)))
                                                  (and (eq (overlay-get overlay 'window) window)
                                                       (or (not (number-or-marker-p anchor))
                                                           (< anchor start)
                                                           (>= anchor end)))))))

(defun tessera-gnus-summary--presented-p (window anchor)
  "Return non-nil when ANCHOR is presented in WINDOW."
  (and
   (tessera-ui-window-presentations-find (tessera-gnus-summary--presentation-state)
                                         window 'tessera-ui--presentation-anchor anchor)
   t))

(defun tessera-gnus-summary--mark-slot (element)
  "Return the semantic mark slot named by ELEMENT."
  (car (seq-find (lambda (mark) (eq (nth 2 mark) element))
                 tessera-gnus-summary--mark-elements)))

(defun tessera-gnus-summary--entry-element-foreground (article elements)
  "Return the foreground of one of ELEMENTS in native ARTICLE."
  (when-let* ((data (gnus-data-find article))
              (anchor (gnus-data-pos data))
              (bounds (tessera-gnus-summary--entry-bounds anchor))
              (element
               (tessera-gnus-summary--element-bounds (car bounds) (cdr bounds) elements)))
    (tessera-gnus-summary--foreground-at (car element))))

(defun tessera-gnus-summary--mark-context-color (article)
  "Return the adjacent text foreground for marks in ARTICLE."
  (if gnus-show-threads
      (or
       (tessera-gnus-summary--entry-element-foreground article '(entry.author entry.author.placeholder))
       (plist-get (tessera-gnus-summary--author-face article)
                  :foreground))
    (or
     (tessera-gnus-summary--entry-element-foreground article '(entry.subject entry.subject.placeholder))
     (face-foreground (tessera-gnus-summary--subject-face article) nil t))))

(defun tessera-gnus-summary--flat-rail ()
  "Return the shared rail for flat Summary entries."
  (or tessera-gnus-summary--flat-rail
      (setq tessera-gnus-summary--flat-rail
            (tessera-ui-make-entry-rail tessera-gnus-symbol-style))))

(defun tessera-gnus-summary--mark-display (bounds)
  "Return the display string for the mark at BOUNDS."
  (let* ((position (car bounds))
         (character (char-after position))
         (element (get-text-property position 'tessera-ui--element))
         (slot (tessera-gnus-summary--mark-slot element))
         (variable
          (or (get-text-property position 'tessera-gnus-summary--native-mark)
              (tessera-gnus-summary--mark-variable slot character)))
         (context-color
          (and
           (null tessera-gnus-glyph-color-style)
           (tessera-gnus-summary--mark-context-color (get-text-property position 'gnus-number))))
         (color
          (tessera-gnus-summary--glyph-color variable tessera-gnus-summary--mark-faces context-color))
         (symbol
          (tessera-gnus-summary--mark-symbol variable character))
         (text (copy-sequence symbol))
         (glyph-face (tessera-gnus-summary--glyph-face text)))
    (add-text-properties 0 (length text) (text-properties-at position) text)
    (remove-text-properties 0 (length text) '(face nil font-lock-face nil) text)
    (add-face-text-property 0 (length text) (tessera-gnus-summary--glyph-color-spec color) nil text)
    (when glyph-face
      (add-face-text-property 0 (length text) glyph-face t text))
    text))

(defun tessera-gnus-summary--current-entry-p (position)
  "Return non-nil when POSITION belongs to the current entry."
  (and (overlayp gnus-newsgroup-selected-overlay)
       (overlay-buffer gnus-newsgroup-selected-overlay)
       (eq (overlay-get gnus-newsgroup-selected-overlay 'face)
           'tessera-ui-entry-current)
       (<= (overlay-start gnus-newsgroup-selected-overlay) position)
       (< position (overlay-end gnus-newsgroup-selected-overlay))))

(defun tessera-gnus-summary--present-entry (window start end)
  "Present the entry from START to END in WINDOW."
  (let* ((tessera-gnus-summary--presentation-anchor start)
         (article (get-text-property start 'gnus-number))
         (data (and article (gnus-data-find article)))
         (header (and data (gnus-data-header data)))
         (primary-start
          (save-excursion
            (goto-char start)
            (line-beginning-position)))
         (primary-end
          (save-excursion
            (goto-char start)
            (line-end-position)))
         (secondary-start (1+ primary-end))
         (secondary-end
          (and (< secondary-start end)
               (save-excursion
                 (goto-char secondary-start)
                 (line-end-position)))))
    (when (and header secondary-end)
      (let* ((entry
              (tessera-ui-make-flat-entry 'gnus-summary header))
             (lines
              (tessera-ui-flat-entry-window-lines entry window
                                                  (tessera-gnus-summary--flat-rail)))
             (current
              (tessera-gnus-summary--current-entry-p primary-start)))
        (dolist (line lines)
          (add-text-properties 0 (length line)
                               (list 'keymap tessera-gnus-summary--entry-map
                                     'mouse-face 'highlight
                                     'gnus-number article
                                     'tessera-ui--parent-element 'entry)
                               line)
          (when current
            (add-face-text-property 0 (length line) 'tessera-ui-entry-current t line)))
        (dolist
            (overlay
             (tessera-ui-present-flat-entry-lines lines primary-start window
                                                  (list 'tessera-ui--window-presentation t
                                                        'tessera-ui--presentation-anchor start)))
          (tessera-ui-window-presentations-add (tessera-gnus-summary--presentation-state)
                                               overlay))))))

(defun tessera-gnus-summary--present-thread-member (window start _end)
  "Present the threaded member at START in WINDOW."
  (when-let* ((article (get-text-property start 'gnus-number))
              (data (gnus-data-find article))
              (data-list
               (tessera-gnus-summary--thread-root-data-list data)))
    (let* ((thread
            (tessera-ui-make-thread 'gnus-summary data-list))
           (lines
            (tessera-ui-thread-window-lines thread window
                                            (tessera-gnus-summary--flat-rail)
                                            tessera-gnus-symbol-style))
           (scan data-list)
           (index 1)
           (line-start
            (save-excursion
              (goto-char start)
              (line-beginning-position)))
           (line-end
            (save-excursion
              (goto-char start)
              (line-end-position))))
      (while (and scan (not (eq (car scan) data)))
        (setq scan (cdr scan)
              index (1+ index)))
      (when-let* ((member-line (and scan (nth index lines))))
        (add-text-properties 0 (length member-line)
                             (list 'keymap tessera-gnus-summary--entry-map
                                   'mouse-face 'highlight
                                   'gnus-number article
                                   'tessera-ui--parent-element 'thread.member)
                             member-line)
        (when (tessera-gnus-summary--current-entry-p start)
          (add-face-text-property 0 (length member-line) 'tessera-ui-entry-current t member-line))
        (let (main-line)
          (when (= index 1)
            (setq main-line (car lines))
            (add-text-properties 0 (length main-line)
                                 (list 'mouse-face 'highlight
                                       'gnus-number article
                                       'tessera-ui--parent-element 'thread.main)
                                 main-line))
          (let ((tessera-gnus-summary--presentation-anchor start))
            (tessera-ui-window-presentations-add (tessera-gnus-summary--presentation-state)
                                                 (tessera-ui-make-virtual-row-overlay line-start line-end window member-line
                                                                                      :main-line main-line
                                                                                      :properties
                                                                                      (list 'tessera-ui--window-presentation t
                                                                                            'tessera-ui--presentation-anchor start)))))))))

(defun tessera-gnus-summary--present-month-heading (window start end)
  "Present the month heading from START to END in WINDOW."
  (let* ((tessera-gnus-summary--presentation-anchor start)
         (key (get-text-property start 'tessera-ui--month-key))
         (group
          (tessera-gnus-summary--month-group-for-key key))
         (collapsed
          (and
           (tessera-gnus-summary--month-overlay key 'tessera-ui--month-fold)
           t)))
    (when group
      (let ((display
             (tessera-ui-format-month-heading group collapsed window)))
        (add-text-properties 0 (length display)
                             (list 'keymap tessera-gnus-summary--month-map
                                   'mouse-face 'tessera-ui-month-heading-highlight
                                   'help-echo
                                   (tessera-gnus-summary--month-help collapsed)
                                   'tessera-ui--month-key key)
                             display)
        (tessera-gnus-summary--make-before-string-overlay start end display window)))))

(defun tessera-gnus-summary--present-window (window &optional limit)
  "Create presentation overlays for visible entries in WINDOW.

Stop at LIMIT when it is non-nil."
  (let ((position (window-start window))
        (limit (or limit (window-end window t) (point-max))))
    (while (< position limit)
      (cond
       ((get-text-property position 'tessera-ui--month-key)
        (let* ((start
                (save-excursion
                  (goto-char position)
                  (line-beginning-position)))
               (end
                (save-excursion
                  (goto-char position)
                  (line-end-position))))
          (unless (tessera-gnus-summary--presented-p window start)
            (tessera-gnus-summary--present-month-heading window start end))
          (setq position (min limit (1+ end)))))
       ((invisible-p position)
        (setq position
              (next-single-char-property-change position 'invisible nil limit)))
       (t
        (let* ((article (get-text-property position 'gnus-number))
               (data (and article (gnus-data-find article)))
               (start (and data (gnus-data-pos data)))
               (end
                (and start
                     (next-single-property-change start 'gnus-number nil (point-max)))))
          (if (and start end (> end position))
              (progn
                (unless (tessera-gnus-summary--presented-p
                         window start)
                  (if (eq gnus-summary-line-format
                          tessera-gnus-summary--thread-line-format)
                      (tessera-gnus-summary--present-thread-member window start end)
                    (tessera-gnus-summary--present-entry window start end)))
                (setq position end))
            (setq position
                  (next-single-property-change position 'gnus-number nil limit)))))))))

(defun tessera-gnus-summary--present-visible-windows ()
  "Create missing presentations in visible Summary windows."
  (when (and tessera-gnus-summary--installed-p
             tessera-gnus-summary--line-format-installed-p)
    (save-excursion
      (dolist (window
               (get-buffer-window-list (current-buffer) nil t))
        (with-selected-window window
          (tessera-gnus-summary--present-window window))))))

(defun tessera-gnus-summary--refresh-presentations-at (anchors)
  "Refresh visible presentations at ANCHORS."
  (tessera-gnus-summary--delete-presentations-at (delq nil anchors))
  (tessera-gnus-summary--present-visible-windows))

(defun tessera-gnus-summary--window-scrolled (window start)
  "Create presentations entering WINDOW at display position START."
  (when (and tessera-gnus-summary--installed-p
             tessera-gnus-summary--line-format-installed-p)
    (with-selected-window window
      (let (begin limit)
        (save-excursion
          (goto-char start)
          (vertical-motion -4 window)
          (setq begin (point))
          (goto-char start)
          (vertical-motion (+ (window-body-height window) 4)
                           window)
          (setq limit (point)))
        (tessera-gnus-summary--delete-presentations-outside window begin limit)
        (tessera-gnus-summary--present-window window limit)))))

(defun tessera-gnus-summary--update-window-presentations-now ()
  "Update presentation overlays for every visible Summary window."
  (tessera-gnus-summary--delete-window-overlays)
  (when (and tessera-gnus-summary--installed-p
             tessera-gnus-summary--line-format-installed-p)
    (force-window-update (current-buffer))
    (save-excursion
      (dolist (window
               (get-buffer-window-list (current-buffer) nil t))
        (with-selected-window window
          (tessera-gnus-summary--present-window window))))))

(defun tessera-gnus-summary--update-window-presentations ()
  "Update all visible Summary presentations without reentry."
  (tessera-ui-window-presentations-update (tessera-gnus-summary--presentation-state)
                                          #'tessera-gnus-summary--update-window-presentations-now))

(defun tessera-gnus-summary--cancel-presentation-update ()
  "Cancel a pending window-local presentation update."
  (tessera-ui-window-presentations-cancel (tessera-gnus-summary--presentation-state)))

(defun tessera-gnus-summary--schedule-window-presentations (&rest _args)
  "Schedule an update of window-local Summary presentation."
  (when (and tessera-gnus-summary--installed-p
             tessera-gnus-summary--line-format-installed-p)
    (tessera-ui-window-presentations-schedule (tessera-gnus-summary--presentation-state)
                                              tessera-gnus-summary-presentation-delay
                                              #'tessera-gnus-summary--update-window-presentations-now
                                              (current-buffer))))

(defun tessera-gnus-summary--face-remap-changed (&rest _args)
  "Remeasure the current Summary after a face remapping change."
  (when (derived-mode-p 'gnus-summary-mode)
    (tessera-gnus-summary--schedule-window-presentations)))

(defun tessera-gnus-summary--install ()
  "Install Tessera in the current Gnus Summary buffer."
  (unless tessera-gnus-summary--installed-p
    (setq tessera-gnus-summary--installed-p t
          tessera-gnus-summary--status-state 'success
          tessera-gnus-summary--fetch-current nil
          tessera-gnus-summary--fetch-total nil
          tessera-gnus-summary--fetch-failed nil)
    (tessera-ui-header-line-install 'gnus-summary)
    (setq-local tessera-gnus--face-remap-function
                #'tessera-gnus-summary--face-remap-changed)
    (add-hook 'gnus-summary-generate-hook #'tessera-gnus-summary--update-line-format nil t)
    (add-hook 'gnus-summary-update-hook #'tessera-gnus-summary--update-entry nil t)
    (add-hook 'gnus-summary-update-hook #'tessera-gnus-summary--update-current-metric t t)
    (add-hook 'gnus-summary-prepare-hook #'tessera-gnus-summary--update-selected-entry nil t)
    (add-hook 'gnus-summary-prepare-hook #'tessera-gnus-summary--update-window-presentations nil t)
    (add-hook 'gnus-summary-prepare-hook #'tessera-gnus-summary--update-entries nil t)
    (add-hook 'window-configuration-change-hook #'tessera-gnus-summary--schedule-window-presentations nil t)
    (add-hook 'window-scroll-functions #'tessera-gnus-summary--window-scrolled nil t)
    (add-hook 'text-scale-mode-hook #'tessera-gnus-summary-refresh nil t)
    (add-hook 'kill-buffer-hook #'tessera-gnus-summary--cancel-presentation-update nil t)
    (when gnus-newsgroup-prepared
      (tessera-gnus-summary--regenerate))
    (force-mode-line-update)))

(defun tessera-gnus-summary--restore ()
  "Restore native presentation in the current Gnus Summary buffer."
  (when tessera-gnus-summary--installed-p
    (remove-hook 'gnus-summary-generate-hook #'tessera-gnus-summary--update-line-format t)
    (remove-hook 'gnus-summary-update-hook #'tessera-gnus-summary--update-entry t)
    (remove-hook 'gnus-summary-update-hook #'tessera-gnus-summary--update-current-metric t)
    (remove-hook 'gnus-summary-prepare-hook #'tessera-gnus-summary--update-window-presentations t)
    (remove-hook 'gnus-summary-prepare-hook #'tessera-gnus-summary--update-entries t)
    (remove-hook 'window-configuration-change-hook #'tessera-gnus-summary--schedule-window-presentations t)
    (remove-hook 'window-scroll-functions #'tessera-gnus-summary--window-scrolled t)
    (remove-hook 'text-scale-mode-hook #'tessera-gnus-summary-refresh t)
    (remove-hook 'kill-buffer-hook #'tessera-gnus-summary--cancel-presentation-update t)
    (tessera-gnus-summary--cancel-presentation-update)
    (tessera-gnus-summary--delete-window-overlays)
    (tessera-gnus-summary--delete-month-overlays)
    (remove-from-invisibility-spec 'tessera-gnus-summary-month)
    (tessera-ui-header-line-restore)
    (setq tessera-gnus-summary--selected-entry-anchor nil
          tessera-gnus-summary--thread-groups nil)
    (when (eq tessera-gnus--face-remap-function
              #'tessera-gnus-summary--face-remap-changed)
      (kill-local-variable 'tessera-gnus--face-remap-function))
    (tessera-gnus-summary--restore-thread-tree-settings)
    (let ((line-format-restored-p
           (tessera-gnus-summary--restore-line-format))
          (sort-functions-restored-p
           (tessera-gnus-summary--restore-sort-functions)))
      (when (and (or line-format-restored-p
                     sort-functions-restored-p)
                 gnus-newsgroup-prepared)
        (tessera-gnus-summary--regenerate)))
    (remove-hook 'gnus-summary-prepare-hook #'tessera-gnus-summary--update-selected-entry t)
    (setq tessera-gnus-summary--installed-p nil)
    (force-mode-line-update)))

(defun tessera-gnus-summary-enable ()
  "Enable Tessera in existing and future Summary buffers."
  (unless tessera-gnus-summary--enabled-p
    (setq tessera-gnus-summary--enabled-p t)
    (tessera-gnus-summary--add-fetch-advice)
    (advice-add 'gnus-summary-update-mark :around #'tessera-gnus-summary--update-mark)
    (advice-add 'gnus-summary-set-article-display-arrow :around #'tessera-gnus-summary--set-article-display-arrow)
    (advice-add 'gnus-highlight-selected-summary :after #'tessera-gnus-summary--highlight-selected-entry)
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
    (advice-remove 'gnus-summary-update-mark #'tessera-gnus-summary--update-mark)
    (remove-hook 'gnus-summary-mode-hook #'tessera-gnus-summary--install)
    (dolist (buffer
             (match-buffers '(derived-mode . gnus-summary-mode)))
      (with-current-buffer buffer
        (tessera-gnus-summary--restore)))
    (advice-remove 'gnus-summary-set-article-display-arrow #'tessera-gnus-summary--set-article-display-arrow)
    (advice-remove 'gnus-highlight-selected-summary #'tessera-gnus-summary--highlight-selected-entry)))

(provide 'tessera-gnus-summary)
;;; tessera-gnus-summary.el ends here
