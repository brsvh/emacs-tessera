;;; tessera-gnus-summary.el --- Tessera entries for Gnus  -*- lexical-binding: t; -*-

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

;; Follow `gnus-sum' for labels, cached article properties, native
;; thread contexts, options, faces, and summary rendering.  Parsed
;; MIME observations come from the article adapter.  Four native marks
;; remain at fixed offsets in a zero-width prefix before the glyphs.
;; The visible entry is refreshed from those marks after changes.

;;; Code:

(require 'tessera-gnus)
(require 'tessera-gnus-article)
(require 'gnus-sum)
(require 'gnus-spec)
(require 'parse-time)
(require 'subr-x)
(require 'seq)

(defgroup tessera-gnus-summary nil
  "Tessera entries in Gnus summary buffers."
  :group 'tessera-gnus
  :prefix "tessera-gnus-summary-")

(defcustom tessera-gnus-summary-month-grouping 'inherit
  "Whether Gnus summary buffers use month grouping.
The value `inherit' follows `tessera-month-grouping'."
  :type '(choice
          (const :tag "Inherit global setting" inherit)
          (const :tag "Enabled" t)
          (const :tag "Disabled" nil))
  :initialize #'custom-initialize-default
  :set #'tessera--set-month-option
  :group 'tessera-gnus-summary)

(defcustom tessera-gnus-summary-month-thread-date 'inherit
  "Date policy used to place Gnus threads in a month.
The value `inherit' follows `tessera-month-thread-date'.  While month
grouping is enabled, Tessera temporarily applies matching native Gnus
thread and article sorting in each summary buffer."
  :type '(choice
          (const :tag "Inherit global setting" inherit)
          (const :tag "Latest thread article" latest)
          (const :tag "Thread root" root))
  :initialize #'custom-initialize-default
  :set #'tessera--set-month-option
  :group 'tessera-gnus-summary)

;;;; Glyph options

(defvar tessera-gnus-summary--glyph-defaults
  '((status-unread
     :ascii nil
     :unicode "●"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-email")
     :face tessera-glyph-accent-face)
    (status-ticked
     :ascii nil
     :unicode "★"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-star")
     :face tessera-glyph-attention-face)
    (status-dormant
     :ascii nil
     :unicode "◇"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-sleep")
     :face tessera-glyph-muted-face)
    (status-expirable
     :ascii nil
     :unicode "◷"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-calendar_clock_outline")
     :face tessera-glyph-warning-face)
    (status-spam
     :ascii nil
     :unicode "!"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-shield_alert_outline")
     :face tessera-glyph-negative-face)
    (status-downloadable
     :ascii nil
     :unicode "↓"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-download")
     :face tessera-glyph-informational-face)
    (status-unsendable
     :ascii nil
     :unicode "↛"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-email_off_outline")
     :face tessera-glyph-negative-face)
    (status-killed
     :ascii nil
     :unicode "×"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-close_circle_outline")
     :face tessera-glyph-muted-face)
    (status-kill-file
     :ascii nil
     :unicode "⊗"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-filter_remove")
     :face tessera-glyph-muted-face)
    (status-low-score
     :ascii nil
     :unicode "⇣"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-filter_check_outline")
     :face tessera-glyph-muted-face)
    (status-catchup
     :ascii nil
     :unicode "✓"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-playlist_check")
     :face tessera-glyph-muted-face)
    (status-ancient
     :ascii nil
     :unicode "◌"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-file_clock_outline")
     :face tessera-glyph-muted-face)
    (status-sparse
     :ascii nil
     :unicode "⋯"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-file_hidden")
     :face tessera-glyph-muted-face)
    (status-canceled
     :ascii nil
     :unicode "⊘"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-cancel")
     :face tessera-glyph-negative-face)
    (status-duplicate
     :ascii nil
     :unicode "⧉"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-content_copy")
     :face tessera-glyph-muted-face)
    (status-del
     :ascii nil
     :unicode "○"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-email_open_outline")
     :face tessera-glyph-muted-face)
    (status-read
     :ascii nil
     :unicode "○"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-email_open_outline")
     :face tessera-glyph-muted-face)
    (secondary-processable
     :ascii nil
     :unicode "◆"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-clipboard_clock_outline")
     :face tessera-glyph-attention-face)
    (secondary-cached
     :ascii nil
     :unicode "▣"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-database")
     :face tessera-glyph-positive-face)
    (secondary-replied
     :ascii nil
     :unicode "↶"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-reply")
     :face tessera-glyph-positive-face)
    (secondary-forwarded
     :ascii nil
     :unicode "↷"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-forward")
     :face tessera-glyph-informational-face)
    (secondary-saved
     :ascii nil
     :unicode "▣"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-content_save")
     :face tessera-glyph-positive-face)
    (secondary-unseen
     :ascii nil
     :unicode "✦"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-new_box")
     :face tessera-glyph-accent-face)
    (availability-undownloaded
     :ascii nil
     :unicode "↓"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-cloud_outline")
     :face tessera-glyph-muted-face)
    (availability-downloaded
     :ascii nil
     :unicode "✓"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-cloud_download")
     :face tessera-glyph-positive-face)
    (score-low
     :ascii nil
     :unicode "↓"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-arrow_down_bold")
     :face tessera-glyph-muted-face)
    (score-high
     :ascii nil
     :unicode "↑"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-arrow_up_bold")
     :face tessera-glyph-attention-face)
    (unknown
     :ascii "?"
     :unicode "?"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-help_circle")
     :face tessera-glyph-warning-face)
    (attachment-present
     :ascii "a"
     :unicode "📎"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-paperclip")
     :face tessera-glyph-informational-face)
    (signature-present
     :ascii "S"
     :unicode "✍︎"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-file_sign")
     :face tessera-glyph-informational-face)
    (signature-processed
     :ascii "S"
     :unicode "✍︎"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-file_sign")
     :face tessera-glyph-informational-face)
    (signature-error
     :ascii "!"
     :unicode "!"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-alert_circle_outline")
     :face tessera-glyph-negative-face)
    (encryption-present
     :ascii "E"
     :unicode "🔒"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-lock_outline")
     :face tessera-glyph-accent-face)
    (encryption-processed
     :ascii "E"
     :unicode "🔒"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-lock_outline")
     :face tessera-glyph-accent-face)
    (encryption-error
     :ascii "!"
     :unicode "!"
     :nerd-icons ( :function nerd-icons-mdicon
                   :name "nf-md-alert_circle_outline")
     :face tessera-glyph-negative-face))
  "Default glyph representations and shared semantic faces.")

(defun tessera-gnus-summary--set-glyphs
    (symbol value)
  "Set glyph option SYMBOL to validated VALUE and refresh views."
  (tessera--set-glyphs
   symbol value tessera-gnus-summary--glyph-defaults))

(defcustom tessera-gnus-summary-glyphs nil
  "Overrides for the named fields of this view's glyphs.
Each alist entry maps a glyph ID to a property list.  Missing fields
keep their defaults.  Use :ascii, :unicode, :nerd-icons, :face, and
:hidden; see `tessera-glyph-resolve'.  Customize and `setopt' redraw
active views.  After `setq', call `tessera-refresh-glyphs'."
  :type (tessera--glyph-custom-type
         tessera-gnus-summary--glyph-defaults)
  :initialize #'custom-initialize-default
  :set #'tessera-gnus-summary--set-glyphs
  :group 'tessera-gnus-summary)

;;;; Navigation options

(defcustom tessera-gnus-summary-boundary-navigation nil
  "Whether article navigation may continue beyond this summary.
When nil, Tessera blocks both `gnus-auto-extend-newsgroup' and
`gnus-auto-select-next' after an article command exhausts the
articles already in the summary.  When non-nil, those two native
options retain their normal meanings and values; this option does
not enable either native behavior by itself."
  :type 'boolean
  :group 'tessera-gnus-summary)

;;;; Faces

(defface tessera-gnus-summary-subject-face
  '((t :inherit gnus-header-subject :extend nil))
  "Base face for article subjects, below the native state face."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-unread-subject-face
  '((t :inherit (bold tessera-gnus-summary-subject-face)
       :extend nil))
  "Face for unread article subjects."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-thread-subject-face
  '((t :inherit (bold gnus-summary-normal-read) :extend nil))
  "Face for subjects of threads with no unread articles."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-thread-unread-subject-face
  '((t :inherit (bold gnus-summary-normal-unread) :extend nil))
  "Face for subjects of threads containing unread articles."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-author-face
  '((t :inherit (italic gnus-header-from)
       :weight normal
       :extend nil))
  "Face for article authors."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-read-author-face
  '((t :inherit gnus-summary-normal-read
       :weight normal
       :slant italic
       :extend nil))
  "Face for read authors outside thread layouts."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-unread-author-face
  '((t :inherit (bold gnus-summary-normal-unread)
       :slant italic
       :extend nil))
  "Face for unread authors outside thread layouts."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-date-face
  '((t :inherit gnus-summary-normal-read
       :weight normal
       :slant normal
       :extend nil))
  "Face for read article dates."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-unread-date-face
  '((t :inherit (bold gnus-summary-normal-unread)
       :slant normal
       :extend nil))
  "Face for unread article dates."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-label-face
  '((t :inherit gnus-header-content
       :weight normal
       :slant normal
       :extend nil))
  "Face for article labels from every supported source."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-spam-face
  '((t :inherit tessera-glyph-negative-face :extend nil))
  "Spam article state, supplementing the native summary face."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-expirable-face
  '((t :inherit tessera-glyph-warning-face :extend nil))
  "Expirable article state, supplementing the native summary face."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-thread-count-face
  '((t :inherit (gnus-summary-normal-read shadow) :extend nil))
  "Thread counts with no unread articles."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-thread-unread-count-face
  '((t :inherit (gnus-summary-normal-unread bold) :extend nil))
  "Thread counts containing unread articles."
  :group 'tessera-gnus-summary)

(defvar gnus-registry-db)

(declare-function gnus-registry-get-id-key "gnus-registry")

(defvar gnus-tmp-unread)
(defvar gnus-tmp-replied)
(defvar gnus-tmp-downloaded)
(defvar gnus-tmp-score-char)
(defvar gnus-tmp-from)

;;;; Native marks and glyphs

(defvar tessera-gnus-summary--states
  '((status 0
            (unread gnus-unread-mark "Unread")
            (ticked gnus-ticked-mark "Ticked")
            (dormant gnus-dormant-mark "Dormant")
            (expirable gnus-expirable-mark "Expirable")
            (spam gnus-spam-mark "Spam")
            (downloadable gnus-downloadable-mark "Downloadable")
            (unsendable gnus-unsendable-mark "Unsendable")
            (killed gnus-killed-mark "Killed")
            (kill-file gnus-kill-file-mark "Killed by rule")
            (low-score gnus-low-score-mark "Read by score")
            (catchup gnus-catchup-mark "Caught up")
            (ancient gnus-ancient-mark "Ancient")
            (sparse gnus-sparse-mark "Sparse")
            (canceled gnus-canceled-mark "Canceled")
            (duplicate gnus-duplicate-mark "Duplicate")
            (del gnus-del-mark "Marked as read")
            (read gnus-read-mark "Read"))
    (secondary 1
               (processable gnus-process-mark "Marked for processing")
               (cached gnus-cached-mark "Cached")
               (replied gnus-replied-mark "Replied")
               (forwarded gnus-forwarded-mark "Forwarded")
               (saved gnus-saved-mark "Saved")
               (unseen gnus-unseen-mark "Unseen"))
    (availability 2
                  (undownloaded gnus-undownloaded-mark
                                "Not downloaded")
                  (downloaded gnus-downloaded-mark "Downloaded"))
    (score 3
           (low gnus-score-below-mark "Below default score")
           (high gnus-score-over-mark "Above default score")))
  "Native mark variables and help labels, grouped by slot.")

;;;; Buffer state

(defvar tessera-gnus-summary--metadata nil
  "Metadata dynamically bound while rendering one article.")

(defvar-local tessera-gnus-summary--active nil
  "Whether Tessera is active in this summary buffer.")

(defvar-local tessera-gnus-summary--saved-settings nil
  "Original values and locality of settings replaced by Tessera.")

(defvar-local tessera-gnus-summary--saved-month-sorting nil
  "Native sorting replaced while month grouping is active.")

(defvar-local tessera-gnus-summary--dirty nil
  "Whether native buffer changes need synchronization.")

(defvar-local tessera-gnus-summary--updating nil
  "Non-nil while Tessera is updating its own presentation.")

(defvar tessera-gnus-summary--batching nil
  "Non-nil while article positions will be rebuilt as one batch.")

(defvar tessera-gnus-summary--navigating nil
  "Non-nil while an outer native summary move is in progress.")

(defvar tessera-gnus-summary--navigation-functions
  '(gnus-summary-next-subject
    gnus-summary-next-article
    gnus-summary-next-unseen-article
    gnus-summary-first-unread-article
    gnus-summary-first-unread-subject
    gnus-summary-first-unseen-subject
    gnus-summary-first-unseen-or-unread-subject
    gnus-summary-first-article
    gnus-summary-last-subject
    gnus-summary-next-thread
    gnus-summary-down-thread
    gnus-summary-top-thread)
  "Native Gnus functions that implement entry navigation.")

(defvar tessera-gnus-summary--navigation-wrappers
  '(gnus-summary-prev-subject
    gnus-summary-next-unread-subject
    gnus-summary-prev-unread-subject
    gnus-summary-prev-article
    gnus-summary-next-unread-article
    gnus-summary-prev-unread-article
    gnus-summary-next-same-subject
    gnus-summary-prev-same-subject
    gnus-summary-next-unread-same-subject
    gnus-summary-prev-unread-same-subject
    gnus-summary-prev-unseen-article
    gnus-summary-prev-thread
    gnus-summary-up-thread)
  "Gnus commands implemented through advised navigation functions.")

(defvar-local tessera-gnus-summary--appearance nil
  "Appearance used for the last synchronized entry rendering.")

;;;; Summary metadata

(defvar-local tessera-gnus-summary--content-cache nil
  "Snapshots of observed MIME properties, keyed by article identity.")

(defun tessera-gnus-summary-header-field (name header)
  "Return extra field NAME from native HEADER, ignoring case.
NAME is a string.  Return nil if absent, without changing HEADER.
No Tessera mode needs to be enabled."
  (cdr (seq-find
        (lambda (pair)
          (string-equal-ignore-case (format "%s" (car pair)) name))
        (mail-header-extra header))))

(defun tessera-gnus-summary--label-text (value)
  "Return VALUE as safe single-line label text."
  (string-trim
   (replace-regexp-in-string
    "[[:cntrl:]]+" " " (format "%s" value))))

(defun tessera-gnus-summary--gmail-labels (value)
  "Decode Gmail label VALUE without evaluating it."
  (when (stringp value)
    (setq value
          (condition-case nil
              (let ((read-circle nil)) (car (read-from-string value)))
            (error nil))))
  (when (and (proper-list-p value)
             (seq-every-p (lambda (item)
                            (or (stringp item) (symbolp item)))
                          value))
    value))

(defun tessera-gnus-summary-label-data (header)
  "Return labels from HEADER and the enabled registry.
Each item is (TEXT . SOURCES); equal names share one label.  TEXT is
single-line text; SOURCES lists Registry, Gmail, or Keywords strings.
Preserve first occurrence order, omitting empty names.  Do not change
HEADER or registry data.  No Tessera mode needs to be enabled."
  (let* ((id (mail-header-message-id header))
         (registry
          (when (and id (bound-and-true-p gnus-registry-db)
                     (fboundp 'gnus-registry-get-id-key))
            (gnus-registry-get-id-key id 'mark)))
         (gmail (tessera-gnus-summary--gmail-labels
                 (tessera-gnus-summary-header-field
                  "X-GM-LABELS" header)))
         (keywords
          (tessera-gnus-summary-header-field "Keywords" header))
         labels)
    (when (stringp keywords)
      (setq keywords (split-string keywords "," t "[[:space:]]+")))
    (dolist (source (list (cons "Registry" registry)
                          (cons "Gmail" gmail)
                          (cons "Keywords" keywords)))
      (dolist (value (cdr source))
        (let* ((text (tessera-gnus-summary--label-text value))
               (existing (assoc text labels)))
          (unless (string-empty-p text)
            (if existing
                (cl-pushnew (car source) (cdr existing) :test #'equal)
              (push (list text (car source)) labels))))))
    (nreverse labels)))

(defun tessera-gnus-summary--content-key (header)
  "Return an identity for HEADER within its summary buffer."
  (or (mail-header-message-id header) (mail-header-number header)))

(defun tessera-gnus-summary--prune-content-cache ()
  "Discard content observations absent from the native summary."
  (when tessera-gnus-summary--content-cache
    (let ((current (make-hash-table :test #'equal)))
      (dolist (header gnus-newsgroup-headers)
        (puthash (tessera-gnus-summary--content-key header)
                 t current))
      (maphash
       (lambda (key _value)
         (unless (gethash key current)
           (remhash key tessera-gnus-summary--content-cache)))
       tessera-gnus-summary--content-cache))))

(defun tessera-gnus-summary--content-data (header)
  "Return observed content properties for HEADER, or header hints."
  (or (and tessera-gnus-summary--content-cache
           (gethash (tessera-gnus-summary--content-key header)
                    tessera-gnus-summary--content-cache))
      (let* ((result (tessera-gnus-article--unknown-content))
             (value (tessera-gnus-summary-header-field
                     "Content-Type" header))
             (type (and (stringp value)
                        (car (mail-header-parse-content-type
                              value)))))
        (pcase type
          ("multipart/signed" (setq result
                                    (plist-put result :signature
                                               'present)))
          ("multipart/encrypted" (setq result
                                       (plist-put result :encryption
                                                  'present))))
        result)))

(defun tessera-gnus-summary--observe-content (header handles)
  "Save properties of already parsed HANDLES for HEADER.
Return non-nil only when the observed properties have changed."
  (let* ((key (tessera-gnus-summary--content-key header))
         (content (tessera-gnus-article--mime-content handles)))
    (unless tessera-gnus-summary--content-cache
      (setq tessera-gnus-summary--content-cache
            (make-hash-table :test #'equal)))
    (unless (equal content
                   (gethash key tessera-gnus-summary--content-cache))
      (puthash key content tessera-gnus-summary--content-cache)
      t)))

;;;; Thread contexts (gnus-sum.el)

(defvar-local tessera-gnus-summary--threads nil
  "Article numbers mapped to their displayed thread contexts.")

(defvar-local tessera-gnus-summary--thread-width 8
  "Shared leading width for counts and four native status slots.")

(defun tessera-gnus-summary--thread-leading-width (_context)
  "Return the leading width for the current summary's thread view."
  tessera-gnus-summary--thread-width)

(defun tessera-gnus-summary--thread-context (header)
  "Return the displayed thread context for native HEADER."
  (when (and gnus-show-threads tessera-gnus-summary--threads)
    (gethash (mail-header-number header)
             tessera-gnus-summary--threads)))

(defun tessera-gnus-summary--build-threads ()
  "Rebuild thread contexts from the completed native summary.
Use native display levels, including Gnus's treatment of missing
parents and adopted roots.  Threading follows `gnus-show-threads'."
  (let (entries stack)
    (when gnus-show-threads
      (let ((levels (make-hash-table :test #'eql)))
        (dolist (data gnus-newsgroup-data)
          (puthash (gnus-data-number data)
                   (gnus-data-level data) levels))
        (save-excursion
          (goto-char (point-min))
          (while (< (point) (point-max))
            (when (get-text-property
                   (point) 'tessera-gnus-summary-entry)
              (let* ((id (get-text-property (point) 'gnus-number))
                     (level (or (gethash id levels) 0)))
                (while (and stack (>= (caar stack) level))
                  (pop stack))
                (push (list id (cdar stack)
                            (not (gnus-read-mark-p (char-after)))
                            (not (invisible-p (point))))
                      entries)
                (push (cons level id) stack)))
            (forward-line 1)))))
    (setq tessera-gnus-summary--threads
          (tessera-thread-build-contexts (nreverse entries))
          tessera-gnus-summary--thread-width 8)
    (maphash
     (lambda (_id node)
       (when (tessera-thread-context-first node)
         (setq tessera-gnus-summary--thread-width
               (max tessera-gnus-summary--thread-width
                    (length (format
                             "%d/%d"
                             (tessera-thread-context-unread node)
                             (tessera-thread-context-total node)))))))
     tessera-gnus-summary--threads)))

;;;; Entry context and state

(defun tessera-gnus-summary--context (header buffer window)
  "Return the entry context for HEADER in BUFFER and WINDOW."
  (make-tessera-entry-context
   :backend 'gnus-summary
   :object header
   :buffer buffer
   :window window
   :metadata tessera-gnus-summary--metadata
   :thread (plist-get tessera-gnus-summary--metadata :thread)))

(defvar tessera-gnus-summary--root-month-sort-functions
  '(gnus-thread-sort-by-number
    (not gnus-thread-sort-by-date))
  "Gnus thread sorting used for `root' month assignment.")

(defvar tessera-gnus-summary--latest-month-sort-functions
  '(gnus-thread-sort-by-number
    gnus-thread-sort-by-most-recent-date)
  "Gnus thread sorting used for `latest' month assignment.")

(defvar tessera-gnus-summary--month-article-sort-functions
  '(gnus-article-sort-by-number
    (not gnus-article-sort-by-date))
  "Gnus article sorting used for unthreaded month grouping.")

(defun tessera-gnus-summary--month-enabled-p ()
  "Return the effective Gnus month grouping setting."
  (if (eq tessera-gnus-summary-month-grouping 'inherit)
      tessera-month-grouping
    tessera-gnus-summary-month-grouping))

(defun tessera-gnus-summary--month-thread-date ()
  "Return the effective Gnus thread date policy."
  (if (eq tessera-gnus-summary-month-thread-date 'inherit)
      tessera-month-thread-date
    tessera-gnus-summary-month-thread-date))

(defun tessera-gnus-summary--month-sort-functions ()
  "Return Gnus thread sorting for the effective month policy."
  (pcase tessera--month-thread-date
    ('latest
     tessera-gnus-summary--latest-month-sort-functions)
    ('root tessera-gnus-summary--root-month-sort-functions)
    (_ (error "Invalid Gnus month thread date: %S"
              tessera--month-thread-date))))

(defun tessera-gnus-summary--update-month-sorting ()
  "Apply or restore sorting for the current month configuration.
Return non-nil when the effective sort values change."
  (let ((old-thread gnus-thread-sort-functions)
        (old-subthread gnus-subthread-sort-functions)
        (old-article gnus-article-sort-functions))
    (if tessera--month-enabled
        (progn
          (unless tessera-gnus-summary--saved-month-sorting
            (setq tessera-gnus-summary--saved-month-sorting
                  (tessera--save-settings
                   '(gnus-thread-sort-functions
                     gnus-subthread-sort-functions
                     gnus-article-sort-functions))))
          (let ((subthread
                 (nth 2
                      (assq
                       'gnus-subthread-sort-functions
                       tessera-gnus-summary--saved-month-sorting))))
            (setq-local
             gnus-subthread-sort-functions
             (copy-tree
              (if (eq subthread 'gnus-thread-sort-functions)
                  (nth 2
                       (assq
                        'gnus-thread-sort-functions
                        tessera-gnus-summary--saved-month-sorting))
                subthread))))
          (setq-local
           gnus-thread-sort-functions
           (copy-tree
            (tessera-gnus-summary--month-sort-functions)))
          (setq-local
           gnus-article-sort-functions
           (copy-tree
            tessera-gnus-summary--month-article-sort-functions)))
      (when tessera-gnus-summary--saved-month-sorting
        (tessera--restore-settings
         tessera-gnus-summary--saved-month-sorting)
        (setq tessera-gnus-summary--saved-month-sorting nil)))
    (or (not (equal old-thread gnus-thread-sort-functions))
        (not (equal old-subthread
                    gnus-subthread-sort-functions))
        (not (equal old-article gnus-article-sort-functions)))))

(defun tessera-gnus-summary--month-date (context)
  "Return CONTEXT's native article date as an Emacs time."
  (date-to-time
   (mail-header-date (tessera-entry-context-object context))))

(defun tessera-gnus-summary--month-glyph (state _context)
  "Return the configured Gnus status glyph for STATE in CONTEXT."
  (tessera-glyph-resolve
   (if (eq state 'unread) 'status-unread 'status-read)
   tessera-gnus-summary--glyph-defaults
   tessera-gnus-summary-glyphs
   (if (eq state 'unread)
       (tessera-gnus-summary--ascii-mark 'gnus-unread-mark)
     (tessera-gnus-summary--ascii-mark 'gnus-read-mark))))

(defun tessera-gnus-summary--month-warning-segment (context)
  "Return the segment that carries CONTEXT's date warning."
  (if (tessera-entry-context-thread context) 'author 'subject))

(defun tessera-gnus-summary--month-goto (context)
  "Move to CONTEXT without changing Gnus's selected article."
  (let ((article
         (mail-header-number
          (tessera-entry-context-object context))))
    (when-let* ((data (gnus-data-find article))
                (position (gnus-data-pos data)))
      (goto-char position)
      (gnus-summary-position-point)
      t)))

(defun tessera-gnus-summary--state (slot context)
  "Return the native state of SLOT in CONTEXT."
  (let* ((spec (assq slot tessera-gnus-summary--states))
         (marks
          (plist-get (tessera-entry-context-metadata context) :marks))
         (mark (aref marks (cadr spec)))
         (variant
          (seq-find
           (lambda (entry)
             (eq mark (symbol-value (nth 1 entry))))
           (cddr spec))))
    (cond (variant (car variant))
          ((eq mark gnus-no-mark) nil)
          (t 'unknown))))

(defun tessera-gnus-summary--unread-p (context)
  "Return non-nil if CONTEXT's article has a native unread mark."
  (not (gnus-read-mark-p
        (aref (plist-get (tessera-entry-context-metadata context)
                         :marks) 0))))

(defun tessera-gnus-summary--face-index ()
  "Index native scores and uncached articles for one batch update.
Keep the first score for each article, as `assq' would."
  (let ((scores (make-hash-table :test #'eq))
        (uncached (make-hash-table :test #'eq)))
    (dolist (entry gnus-newsgroup-scored)
      (unless (gethash (car entry) scores)
        (puthash (car entry) entry scores)))
    (when gnus-summary-use-undownloaded-faces
      (dolist (article gnus-newsgroup-undownloaded)
        (puthash article t uncached))
      (dolist (article gnus-newsgroup-cached)
        (remhash article uncached)))
    (cons scores uncached)))

(defun tessera-gnus-summary--native-face
    (header marks &optional index)
  "Return Gnus's configured summary face for HEADER and MARKS.
Use the native rule evaluator with the same scoring and download
context as `gnus-summary-highlight-line'.  INDEX, when non-nil,
supplies scores and uncached articles for the current batch."
  (let* ((article (mail-header-number header))
         (score-entry (if index
                          (gethash article (car index))
                        (assq article gnus-newsgroup-scored)))
         (uncached-article
          (and gnus-summary-use-undownloaded-faces
               (if index
                   (gethash article (cdr index))
                 (and (memq article gnus-newsgroup-undownloaded)
                      (not (memq article gnus-newsgroup-cached))))))
         (face
          (cl-progv
              '(score default default-high default-low mark uncached)
              (list (or (cdr score-entry)
                        gnus-summary-default-score 0)
                    gnus-summary-default-score
                    gnus-summary-default-high-score
                    gnus-summary-default-low-score
                    (aref marks 0)
                    uncached-article)
            (funcall (gnus-summary-highlight-line-0)))))
    (if (and (symbolp face) (boundp face)) (symbol-value face) face)))

(defun tessera-gnus-summary--state-face (context base)
  "Combine CONTEXT's article state with the element BASE face.
Spam and expirable faces take precedence over native attributes."
  (let ((native (plist-get (tessera-entry-context-metadata context)
                           :native-face))
        (state (pcase (tessera-gnus-summary--state 'status context)
                 ('spam 'tessera-gnus-summary-spam-face)
                 ('expirable 'tessera-gnus-summary-expirable-face))))
    (append '((:extend nil))
            (and state (list state))
            (and native (list native)) (list base))))

(defun tessera-gnus-summary--thread-tree (context)
  "Return CONTEXT's configured thread branches."
  (let* ((window (tessera-entry-context-window context))
         ;; Bound hidden buffers by the selected window's width.
         (width (window-body-width window))
         (text (tessera-thread-prefix context width)))
    (when text
      (propertize text 'tessera--overflow-help
                  #'tessera-gnus-summary--overflow-help))))

(defun tessera-gnus-summary--overflow-help (window object position)
  "Describe the clipped article at POSITION in OBJECT or WINDOW."
  (when-let* ((buffer (if (bufferp object) object
                        (and (window-live-p window)
                             (window-buffer window)))))
    (with-current-buffer buffer
      (when-let* ((entry (get-text-property
                          position 'tessera-gnus-summary-entry))
                  (node (tessera-gnus-summary--thread-context
                         (car entry))))
        (let* ((parent (tessera-thread-context-parent node))
               (data (and parent (gnus-data-find parent))))
          (format
           "%s\n%s\nDepth: %d\nReply to: %s"
           (plist-get (cdr entry) :author)
           (mail-header-subject (car entry))
           (length (tessera--thread-path-tail node))
           (if data
               (format "%s (#%s)"
                       (mail-header-from (gnus-data-header data))
                       parent)
             (or parent "Root"))))))))

(defun tessera-gnus-summary--thread-count (context)
  "Return CONTEXT's thread count using the Gnus count faces."
  (when-let* ((text (tessera-thread-count context)))
    (propertize
     text 'face
     (if (> (tessera-thread-context-unread
             (tessera-entry-context-thread context)) 0)
         'tessera-gnus-summary-thread-unread-count-face
       'tessera-gnus-summary-thread-count-face))))

(defun tessera-gnus-summary--ascii-mark (mark)
  "Return an ASCII fallback for the native Gnus variable MARK.
Use its current value when ASCII, otherwise its declared standard
value.  Signal an error if neither value is an ASCII character."
  (let ((character (symbol-value mark)))
    (unless (and (characterp character) (<= 0 character 127))
      (setq character (eval (car (get mark 'standard-value)) t)))
    (unless (and (characterp character) (<= 0 character 127))
      (error "Gnus mark `%s' has no valid ASCII fallback" mark))
    (char-to-string character)))

(defun tessera-gnus-summary--slot (spec)
  "Build a native status slot from SPEC and configured glyphs."
  (make-tessera-glyph-slot
   :name (car spec)
   :width 2
   :align 'center
   :selector (apply-partially #'tessera-gnus-summary--state
                              (car spec))
   :glyphs
   (append
    (mapcar
     (lambda (entry)
       (pcase-let ((`(,id ,mark ,help) entry))
         (list id
               :glyph
               (tessera-glyph-resolve
                (intern (format "%s-%s" (car spec) id))
                tessera-gnus-summary--glyph-defaults
                tessera-gnus-summary-glyphs
                (tessera-gnus-summary--ascii-mark mark))
               :help-echo help)))
     (cddr spec))
    (list
     (list 'unknown :glyph
           (tessera-glyph-resolve
            'unknown tessera-gnus-summary--glyph-defaults
            tessera-gnus-summary-glyphs)
           :help-echo "Unrecognized Gnus mark")))))

;;;; Rendered fields

(defun tessera-gnus-summary--article-subject-face (context)
  "Return the ordinary article subject face for CONTEXT."
  (let ((unread (tessera-gnus-summary--unread-p context)))
    (append
     (when unread '(bold))
     (tessera-gnus-summary--state-face
      context
      (if unread 'tessera-gnus-summary-unread-subject-face
        'tessera-gnus-summary-subject-face)))))

(defun tessera-gnus-summary--subject (context)
  "Return the article subject in CONTEXT."
  (let ((subject
         (mail-header-subject (tessera-entry-context-object context)))
        (thread (tessera-entry-context-thread context)))
    (propertize
     (if (string-empty-p subject) "(no subject)" subject)
     'face
     (if thread
         (if (> (tessera-thread-context-unread thread) 0)
             'tessera-gnus-summary-thread-unread-subject-face
           'tessera-gnus-summary-thread-subject-face)
       (tessera-gnus-summary--article-subject-face context))
     'mouse-face 'highlight 'help-echo subject)))

(defun tessera-gnus-summary--author (context)
  "Return the contact name in CONTEXT, with full address help."
  (propertize
   (plist-get (tessera-entry-context-metadata context) :author)
   'face
   ;; Keep italics and unread emphasis above native attributes.
   (cons
    'italic
    (if (tessera-entry-context-thread context)
        (append
         (when (tessera-gnus-summary--unread-p context) '(bold))
         (tessera-gnus-summary--state-face
          context 'tessera-gnus-summary-author-face))
      (list (if (tessera-gnus-summary--unread-p context)
                'tessera-gnus-summary-unread-author-face
              'tessera-gnus-summary-read-author-face))))
   'help-echo
   (let* ((header (tessera-entry-context-object context))
          (to (tessera-gnus-summary-header-field "To" header)))
     (concat "From: " (mail-header-from header)
             (when to (concat "\nTo: " to))))))

(defun tessera-gnus-summary--date (context)
  "Return the native formatted date in CONTEXT."
  (let ((date
         (mail-header-date (tessera-entry-context-object context))))
    (propertize
     (condition-case nil (gnus-user-date date)
       (error date))
     'face (if (tessera-gnus-summary--unread-p context)
               'tessera-gnus-summary-unread-date-face
             'tessera-gnus-summary-date-face)
     'help-echo date)))

(defun tessera-gnus-summary--labels (context)
  "Return the labels in CONTEXT as one optional text segment."
  (when-let* ((labels (tessera-gnus-summary-label-data
                       (tessera-entry-context-object context))))
    (let ((all (mapconcat #'car labels ", ")))
      (mapconcat
       (lambda (label)
         (propertize
          (car label) 'face 'tessera-gnus-summary-label-face
          'help-echo (format "%s (%s)\nLabels: %s"
                             (car label)
                             (string-join (cdr label) ", ") all)))
       labels (propertize "," 'face 'default
                          'mouse-face 'default)))))

(defun tessera-gnus-summary--content-state (key context)
  "Return the visible content state for KEY in CONTEXT."
  (let* ((metadata (tessera-entry-context-metadata context))
         (content
          (if (plist-member metadata :content)
              (plist-get metadata :content)
            (tessera-gnus-summary--content-data
             (tessera-entry-context-object context))))
         (state (plist-get content key)))
    ;; Unknown and absent remain distinct data, but neither is shown.
    (unless (eq state 'unknown) state)))

(defun tessera-gnus-summary--content-help
    (key label window object position)
  "Describe KEY with LABEL at POSITION in OBJECT or WINDOW."
  (when-let* ((buffer (if (bufferp object) object
                        (and (window-live-p window)
                             (window-buffer window)))))
    (with-current-buffer buffer
      (when-let* ((entry (get-text-property
                          position 'tessera-gnus-summary-entry)))
        (let* ((content
                (tessera-gnus-summary--content-data (car entry)))
               (state (plist-get content key))
               (details
                (plist-get content
                           (pcase key
                             (:signature :signature-details)
                             (:encryption :encryption-details)))))
          (string-join
           (cons (concat label
                         (pcase state
                           ('processed ": Gnus result available")
                           ('error ": Gnus processing error")
                           (_ "")))
                 details)
           "\n"))))))

(defun tessera-gnus-summary--content-slots ()
  "Return configured attachment, signature, and encryption slots."
  (mapcar
   (lambda (spec)
     (pcase-let* ((`(,name ,key ,label) spec)
                  (help (apply-partially
                         #'tessera-gnus-summary--content-help
                         key label)))
       (make-tessera-glyph-slot
        :name name
        :width 2
        :align 'center
        :selector (apply-partially
                   #'tessera-gnus-summary--content-state key)
        :glyphs
        (mapcar
         (lambda (state)
           (list state :glyph
                 (tessera-glyph-resolve
                  (intern (format "%s-%s" name state))
                  tessera-gnus-summary--glyph-defaults
                  tessera-gnus-summary-glyphs)
                 :help-echo help))
         (if (eq key :attachment) '(present)
           '(present processed error))))))
   '((attachment :attachment "Attachment")
     (signature :signature "Signature")
     (encryption :encryption "Encrypted content"))))

;;;; Layout registration

(defun tessera-gnus-summary--thread-layout ()
  "Return the automatically selected native thread layout."
  (let* ((slots '(score availability secondary status))
         (left '(thread-tree
                 (author :grow t :min-width 4 :truncate tail :point t)
                 (:slots (attachment :optional t)
                         (signature :optional t)
                         (encryption :optional t))))
         (right '((labels :grow t
                          :max-width 24
                          :min-width 0
                          :truncate tail
                          :priority -1
                          :optional t)
                  date))
         (width #'tessera-gnus-summary--thread-leading-width))
    (make-tessera-thread-layout
     :head
     (make-tessera-entry-layout
      :glyph-slots-align 'right
      :leading-width width
      :main-leading-segments '(thread-count)
      :main-left-segments
      '((subject :grow t :min-width 4 :truncate tail))
      :extra-glyph-slots slots
      :extra-left-segments left
      :extra-right-segments right)
     :child
     (make-tessera-entry-layout
      :glyph-slots-align 'right
      :leading-width width
      :main-glyph-slots slots
      :main-left-segments left
      :main-right-segments right))))

(defun tessera-gnus-summary--register ()
  "Register the Gnus summary entry backend."
  (tessera--validate-glyph-overrides
   tessera-gnus-summary--glyph-defaults
   tessera-gnus-summary-glyphs 2)
  (tessera-entry-register
   'gnus-summary :context #'tessera-gnus-summary--context
   :month-date #'tessera-gnus-summary--month-date
   :month-unread-p #'tessera-gnus-summary--unread-p
   :month-glyph #'tessera-gnus-summary--month-glyph
   :month-warning-segment
   #'tessera-gnus-summary--month-warning-segment
   :month-goto #'tessera-gnus-summary--month-goto
   :segments
   '((subject . tessera-gnus-summary--subject)
     (author . tessera-gnus-summary--author)
     (date . tessera-gnus-summary--date)
     (labels . tessera-gnus-summary--labels)
     (thread-tree . tessera-gnus-summary--thread-tree)
     (thread-count . tessera-gnus-summary--thread-count))
   :glyph-slots
   (append (mapcar #'tessera-gnus-summary--slot
                   tessera-gnus-summary--states)
           (tessera-gnus-summary--content-slots))
   :thread-layout (tessera-gnus-summary--thread-layout)
   :layouts
   (list
    (cons 'single-line
          (make-tessera-entry-layout
           :glyph-slots-align 'right
           :main-glyph-slots
           '(score availability secondary status)
           :main-left-segments
           '((subject :grow t :min-width 4 :truncate tail :point t)
             (:slots (attachment :optional t)
                     (signature :optional t)
                     (encryption :optional t)))
           :main-right-segments
           '((labels :grow t
                     :max-width 24
                     :min-width 0
                     :truncate tail
                     :priority -1
                     :optional t)
             (author :max-width 20 :truncate tail :optional t)
             date)))
    (cons 'two-line
          (make-tessera-entry-layout
           :glyph-slots-align 'right
           :main-glyph-slots '(secondary status)
           :main-left-segments
           '((subject :grow t :min-width 4 :truncate tail :point t))
           :main-right-segments
           '((labels :grow t
                     :max-width 24
                     :min-width 0
                     :truncate tail
                     :priority -1
                     :optional t))
           :extra-glyph-slots '(score availability)
           :extra-left-segments
           '((author :grow t :min-width 4 :truncate tail)
             (:slots (attachment :optional t)
                     (signature :optional t)
                     (encryption :optional t)))
           :extra-right-segments '(date))))))

;;;; Native row rendering

(cl-defun tessera-gnus-summary--render
    (header metadata
            &optional (native-face
                       (tessera-gnus-summary--native-face
                        header (plist-get metadata :marks))))
  "Render HEADER with its native METADATA.
Use NATIVE-FACE when supplied, including an explicitly nil face."
  (let* ((metadata (plist-put (copy-sequence metadata) :thread
                              (tessera-gnus-summary--thread-context
                               header)))
         (metadata (plist-put metadata :native-face native-face))
         (metadata (plist-put metadata :content
                              (tessera-gnus-summary--content-data
                               header)))
         (tessera-gnus-summary--metadata metadata)
         (result
          (tessera-entry-render
           'gnus-summary header
           (get-buffer-window (current-buffer))
           (plist-get metadata :marks))))
    (when-let* ((position (tessera-entry-point result)))
      (put-text-property position (1+ position)
                         'gnus-position t result))
    (put-text-property 0 (length result) 'tessera-gnus-summary-entry
                       (cons header metadata) result)
    (let ((position 0))
      (while (< position (length result))
        (let ((next (next-single-property-change
                     position 'face result (length result))))
          (put-text-property
           position next 'tessera-gnus-summary-face
           (list (get-text-property position 'face result)) result)
          (setq position next))))
    result))

(defun tessera-gnus-summary--author-name (header from)
  "Return the sender name from HEADER and decoded FROM.
Use an email address when no name is available, including for mail
sent by the user."
  (let* ((gnus-ignored-from-addresses nil)
         (extract gnus-extract-address-components)
         (gnus-extract-address-components
          (lambda (address)
            (let* ((parts (funcall extract address))
                   (name (car parts)))
              (cons (or (and name (not (string-empty-p name)) name)
                        (cadr parts) "?")
                    (cdr parts))))))
    (gnus-summary-from-or-to-or-newsgroups header from)))

(defun tessera-gnus-summary-format-entry (header)
  "Return a Tessera Gnus summary representation of HEADER.
The first four characters retain Gnus's native status marks as a
zero-width prefix.  Visible glyphs follow that prefix, preserving
native mark discovery and in-place updates."
  (tessera-gnus-summary--render
   header
   (list :marks (string gnus-tmp-unread gnus-tmp-replied
                        gnus-tmp-downloaded gnus-tmp-score-char)
         :author
         (tessera-gnus-summary--author-name header gnus-tmp-from))))

;; Gnus user format names are part of its public format protocol.
(defalias 'gnus-user-format-function-tessera
  #'tessera-gnus-summary-format-entry)

;;;; Synchronization and lifecycle

(defun tessera-gnus-summary--appearance ()
  "Return the current width and shared appearance settings."
  (list
   (when-let* ((window (get-buffer-window (current-buffer))))
     (window-body-width window))
   gnus-show-threads custom-enabled-themes
   gnus-summary-highlight gnus-summary-default-score
   gnus-summary-default-high-score gnus-summary-default-low-score
   gnus-summary-use-undownloaded-faces
   tessera-thread-outer-top-padding
   tessera-thread-outer-bottom-padding
   tessera-thread-inner-top-padding
   tessera-thread-inner-bottom-padding
   tessera-entry-layout tessera-glyph-style tessera-glyph-color
   tessera-safe-gap tessera-entry-left-padding
   tessera-entry-right-padding tessera-entry-top-padding
   tessera-entry-bottom-padding tessera-entry-segment-gap
   tessera-flex-gap-min-width
   (tessera-gnus-summary--month-enabled-p)
   (tessera-gnus-summary--month-thread-date)
   tessera-month-glyphs))

(defun tessera-gnus-summary--restore-faces (start end)
  "Restore Tessera content faces between START and END.
Gnus applies its native row face before running the update hook."
  (tessera-entry-clear-current)
  (with-silent-modifications
    (let ((inhibit-read-only t))
      (while (< start end)
        (let ((next (next-single-property-change
                     start 'tessera-gnus-summary-face nil end))
              (saved
               (get-text-property start 'tessera-gnus-summary-face)))
          (when saved
            (put-text-property start next 'face (car saved)))
          (setq start next))))))

(defun tessera-gnus-summary--fold-changed (&rest _arguments)
  "Invalidate the presentation after a native folding operation."
  (when tessera-gnus-summary--active
    (setq tessera-gnus-summary--dirty t)))

(defun tessera-gnus-summary--track-folds (enable)
  "Track native folding when ENABLE is non-nil, or stop tracking."
  (dolist (function '(gnus-summary-hide-thread
                      gnus-summary-show-thread
                      gnus-summary-show-all-threads))
    (if enable
        (advice-add function :after
                    #'tessera-gnus-summary--fold-changed)
      (advice-remove function
                     #'tessera-gnus-summary--fold-changed))))

(defun tessera-gnus-summary--horizontal-recenter
    (function &rest arguments)
  "Call FUNCTION with ARGUMENTS outside Tessera summaries.
Native horizontal centering measures logical columns, which include
the text of both visual lines in a Tessera entry.  Leave vertical
centering and the user's chosen target row to Gnus."
  (unless tessera-gnus-summary--active
    (apply function arguments)))

(defun tessera-gnus-summary--summary-buffer ()
  "Return the native summary buffer for the current Gnus context."
  (if (derived-mode-p 'gnus-summary-mode)
      (current-buffer)
    (and (boundp 'gnus-summary-buffer)
         gnus-summary-buffer
         (get-buffer gnus-summary-buffer))))

(defun tessera-gnus-summary--navigation-buffer ()
  "Return the active summary buffer for native navigation."
  (let ((buffer (tessera-gnus-summary--summary-buffer)))
    (when (and (buffer-live-p buffer)
               (buffer-local-value
                'tessera-gnus-summary--active buffer))
      buffer)))

(defun tessera-gnus-summary--navigation-related-buffers ()
  "Return live native buffers used by summary navigation."
  (let (buffers)
    (dolist (variable '(gnus-group-buffer gnus-article-buffer))
      (when (boundp variable)
        (when-let* ((value (symbol-value variable))
                    (buffer (get-buffer value)))
          (push buffer buffers))))
    buffers))

(defun tessera-gnus-summary--navigation-identity (buffer)
  "Return the native group and article identity in BUFFER."
  (with-current-buffer buffer
    (when-let* ((article (gnus-summary-article-number)))
      (cons gnus-newsgroup-name article))))

(defun tessera-gnus-summary--navigation-changed-p
    (buffer identity)
  "Return non-nil when navigation left BUFFER's IDENTITY."
  (or (not (buffer-live-p buffer))
      (when-let* ((after-buffer
                   (tessera-gnus-summary--summary-buffer))
                  ((buffer-live-p after-buffer))
                  (after
                   (tessera-gnus-summary--navigation-identity
                    after-buffer)))
        (not (equal after identity)))))

(defun tessera-gnus-summary--navigation-key-command-p ()
  "Return non-nil when `n' or `p' invokes `this-command'."
  (and (commandp this-command)
       (or (eq (key-binding (kbd "n") t) this-command)
           (eq (key-binding (kbd "p") t) this-command))))

(defun tessera-gnus-summary--navigation-command-p ()
  "Return non-nil for a top-level entry navigation command."
  (or (called-interactively-p 'any)
      (memq this-command
            tessera-gnus-summary--navigation-functions)
      (memq this-command
            tessera-gnus-summary--navigation-wrappers)
      (tessera-gnus-summary--navigation-key-command-p)))

(defun tessera-gnus-summary--position-after-navigation (_result)
  "Place point at the active Gnus summary's layout anchor."
  (when-let* ((buffer
               (tessera-gnus-summary--navigation-buffer)))
    (with-current-buffer buffer
      (gnus-summary-position-point))))

(defun tessera-gnus-summary--group-point-marker ()
  "Return a marker at point in the live Gnus group buffer."
  (when-let* (((boundp 'gnus-group-buffer))
              (buffer (get-buffer gnus-group-buffer)))
    (with-current-buffer buffer
      (copy-marker (point)))))

(defun tessera-gnus-summary--first-article-number (unread)
  "Return the first native article.
Restrict the result to UNREAD articles when non-nil."
  (when-let* ((data
               (if unread
                   (let ((available (make-hash-table :test #'eq)))
                     ;; Index current native state for this scan.
                     (dolist (number gnus-newsgroup-unreads)
                       (puthash number t available))
                     (dolist (number gnus-newsgroup-unfetched)
                       (remhash number available))
                     (seq-find
                      (lambda (item)
                        (gethash (gnus-data-number item) available))
                      gnus-newsgroup-data))
                 (car gnus-newsgroup-data))))
    (gnus-data-number data)))

(defun tessera-gnus-summary--navigate-first
    (function arguments unread)
  "Run first-article FUNCTION with ARGUMENTS.
UNREAD non-nil restricts the target to unread articles.  Skip native
display and thread expansion when the target is already selected."
  (let ((buffer (tessera-gnus-summary--navigation-buffer)))
    (if (and buffer
             (tessera-gnus-summary--navigation-command-p)
             (with-current-buffer buffer
               (when-let*
                   ((target
                     (tessera-gnus-summary--first-article-number
                      unread)))
                 (eql (gnus-summary-article-number) target))))
        nil
      (apply #'tessera-gnus-summary--navigate
             function arguments))))

(defun tessera-gnus-summary--navigate-first-article
    (function &rest arguments)
  "Run first-article FUNCTION with ARGUMENTS atomically."
  (tessera-gnus-summary--navigate-first
   function arguments nil))

(defun tessera-gnus-summary--navigate-first-unread-article
    (function &rest arguments)
  "Run first-unread-article FUNCTION with ARGUMENTS atomically."
  (tessera-gnus-summary--navigate-first
   function arguments t))

(defun tessera-gnus-summary--navigate-next-article
    (function &rest arguments)
  "Run native next-article FUNCTION with ARGUMENTS atomically.
Honor `tessera-gnus-summary-boundary-navigation'."
  (let ((buffer (tessera-gnus-summary--navigation-buffer)))
    (if (not buffer)
        (apply function arguments)
      (let* ((atomic
              (and (not tessera-gnus-summary--navigating)
                   (tessera-gnus-summary--navigation-command-p)))
             (group-point
              (unless
                  (or tessera-gnus-summary-boundary-navigation
                      tessera-gnus-summary--navigating atomic)
                (tessera-gnus-summary--group-point-marker))))
        (unwind-protect
            (let ((gnus-auto-extend-newsgroup
                   (and
                    tessera-gnus-summary-boundary-navigation
                    gnus-auto-extend-newsgroup))
                  (gnus-auto-select-next
                   (and
                    tessera-gnus-summary-boundary-navigation
                    gnus-auto-select-next)))
              (if atomic
                  (apply #'tessera-gnus-summary--navigate
                         function arguments)
                (apply function arguments)))
          (when group-point
            (when (marker-buffer group-point)
              (with-current-buffer (marker-buffer group-point)
                (goto-char group-point)))
            (set-marker group-point nil)))))))

(defun tessera-gnus-summary--navigate
    (function &rest arguments)
  "Run native FUNCTION with ARGUMENTS as one navigation action."
  (let ((buffer (tessera-gnus-summary--navigation-buffer)))
    (if (or tessera-gnus-summary--navigating
            (not (tessera-gnus-summary--navigation-command-p))
            (not buffer))
        (apply function arguments)
      (let ((identity
             (tessera-gnus-summary--navigation-identity buffer)))
        (if (not identity)
            (apply function arguments)
          (let ((tessera-gnus-summary--navigating t)
                (tessera--month-navigation
                 tessera--month-enabled))
            (tessera--navigation-call
             buffer function arguments
             (lambda (_result)
               (tessera-gnus-summary--navigation-changed-p
                buffer identity))
             #'tessera-gnus-summary--position-after-navigation
             (tessera-gnus-summary--navigation-related-buffers))))))))

(defun tessera-gnus-summary--navigation-advice (function)
  "Return the navigation advice for native FUNCTION."
  (pcase function
    ('gnus-summary-next-article
     #'tessera-gnus-summary--navigate-next-article)
    ('gnus-summary-first-article
     #'tessera-gnus-summary--navigate-first-article)
    ('gnus-summary-first-unread-article
     #'tessera-gnus-summary--navigate-first-unread-article)
    (_ #'tessera-gnus-summary--navigate)))

(defun tessera-gnus-summary--search-forward
    (function &rest arguments)
  "Call native search FUNCTION, skipping folded month entries.
ARGUMENTS retain Gnus's unread, subject, and direction filters."
  (if (not (and tessera-gnus-summary--active
                tessera--month-enabled))
      (apply function arguments)
    (let ((tessera--month-navigation t)
          (origin (point-marker))
          result previous hidden)
      (unwind-protect
          (progn
            (while
                (progn
                  (setq previous (point)
                        result (apply function arguments)
                        hidden
                        (and result
                             (not
                              (tessera-month-entry-visible-p
                               (point)))))
                  (when (and hidden (= previous (point)))
                    (setq result nil
                          hidden nil))
                  hidden))
            (when (null result)
              (goto-char origin))
            result)
        (set-marker origin nil)))))

(defun tessera-gnus-summary--positioned (&rest _arguments)
  "Reveal point after native Gnus positioning."
  (when tessera-gnus-summary--active
    (tessera-month-reveal-point)))

(defun tessera-gnus-summary--navigation (enable)
  "Adapt native centering when ENABLE is non-nil, or restore it."
  (if enable
      (progn
        (advice-add 'gnus-horizontal-recenter :around
                    #'tessera-gnus-summary--horizontal-recenter)
        (dolist (function
                 tessera-gnus-summary--navigation-functions)
          (advice-add function :around
                      (tessera-gnus-summary--navigation-advice
                       function)))
        (advice-add 'gnus-summary-search-forward :around
                    #'tessera-gnus-summary--search-forward)
        (advice-add 'gnus-summary-position-point :after
                    #'tessera-gnus-summary--positioned))
    (advice-remove 'gnus-horizontal-recenter
                   #'tessera-gnus-summary--horizontal-recenter)
    (dolist (function tessera-gnus-summary--navigation-functions)
      (advice-remove function
                     (tessera-gnus-summary--navigation-advice
                      function)))
    (advice-remove 'gnus-summary-search-forward
                   #'tessera-gnus-summary--search-forward)
    (advice-remove 'gnus-summary-position-point
                   #'tessera-gnus-summary--positioned)))

(defun tessera-gnus-summary--sync-buffer (&optional force)
  "Synchronize all entries, preserving point within its article.
FORCE also redraws entries with unchanged marks."
  (save-restriction
    (widen)
    (tessera--month-clear-display)
    (let ((width tessera-gnus-summary--thread-width))
      (tessera-gnus-summary--build-threads)
      (when (/= width tessera-gnus-summary--thread-width)
        (setq force t)))
    (let ((saved-point (tessera-entry-save-point))
          (face-index (tessera-gnus-summary--face-index))
          (thread-paths (make-hash-table :test #'eq))
          (tessera-gnus-summary--batching t)
          (tessera-gnus-summary--updating t))
      (unwind-protect
          (progn
            (goto-char (point-min))
            (while (< (point) (point-max))
              (tessera-gnus-summary--sync-line
               force face-index thread-paths)
              (forward-line 1)))
        (tessera-gnus-summary--reindex)
        (tessera-entry-restore-point saved-point)))
    (when tessera-gnus-summary--active
      (tessera-month-sync))))

(defun tessera-gnus-summary--reindex ()
  "Restore native integer positions after a batch of row changes."
  (let ((entries (make-hash-table :test #'eql)))
    (dolist (data gnus-newsgroup-data)
      (puthash (gnus-data-number data) data entries))
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (while (< (point) (point-max))
          (when-let* ((data
                       (gethash
                        (get-text-property (point) 'gnus-number)
                        entries)))
            (setf (gnus-data-pos data) (1+ (point))))
          (forward-line 1))))
    (setq gnus-newsgroup-data-reverse nil)))

(defun tessera-gnus-summary--sync-line
    (&optional force face-index thread-paths)
  "Synchronize the current logical article line.
FORCE also redraws entries whose native marks have not changed.
FACE-INDEX supplies native face data during a batch update.
THREAD-PATHS caches shared ancestor comparisons for that update."
  (let* ((start (line-beginning-position))
         (end (line-end-position))
         (entry
          (get-text-property start 'tessera-gnus-summary-entry)))
    (when (and entry (>= (- end start) 4))
      (let* ((marks
              (buffer-substring-no-properties start (+ start 4)))
             (metadata (cdr entry))
             (thread
              (tessera-gnus-summary--thread-context (car entry)))
             (native-face (tessera-gnus-summary--native-face
                           (car entry) marks face-index))
             (inhibit-read-only t)
             (inhibit-modification-hooks t))
        (when (or force
                  (not (equal marks (plist-get metadata :marks)))
                  (not (equal (plist-get metadata :native-face)
                              native-face))
                  (not (tessera-thread-context-key-equal-p
                        (tessera-thread-context-key
                         (plist-get metadata :thread))
                        (tessera-thread-context-key thread)
                        thread-paths)))
          (tessera-entry-clear-current)
          (tessera-entry-clear-layout start (1+ end))
          (let* ((updated
                  (plist-put (copy-sequence metadata) :marks marks))
                 (rendered (tessera-gnus-summary--render
                            (car entry) updated native-face))
                 (number (get-text-property start 'gnus-number))
                 (intangible
                  (get-text-property start 'gnus-intangible)))
            ;; Keep native marks and their fixed positions intact.
            (delete-region (+ start 4) end)
            (goto-char (+ start 4))
            (insert (substring rendered 4))
            (dotimes (offset 4)
              (set-text-properties
               (+ start offset) (+ start offset 1)
               (text-properties-at offset rendered)))
            ;; Gnus stores integer positions, not markers.  Keep later
            ;; articles addressable when a visual layout changes size.
            (when (and (not tessera-gnus-summary--batching)
                       (/= end (point)))
              (gnus-data-update-list
               (cdr (gnus-data-find-list number)) (- (point) end)))
            (setq end (point))
            (add-text-properties
             start (1+ end)
             (list 'gnus-number number 'gnus-intangible intangible))))
        ;; Keep path tails shared even when the row needs no redraw.
        (setf (plist-get
               (cdr (get-text-property
                     start 'tessera-gnus-summary-entry)) :thread)
              thread)
        (when-let* ((context
                     (get-text-property start
                                        'tessera-entry-context)))
          (setf (tessera-entry-context-thread context) thread))
        (tessera-gnus-summary--restore-faces start end)
        (if (invisible-p start)
            (tessera-entry-clear-layout start (1+ end))
          (unless (tessera-entry-layout-applied-p start)
            (tessera-entry-apply-layout start end)))))))

(defun tessera-gnus-summary--refresh-content (article)
  "Refresh ARTICLE after observing new MIME state.
Preserve point, narrowing, and month folds while row widths change."
  (save-restriction
    (widen)
    (when-let* ((position
                 (text-property-any (point-min) (point-max)
                                    'gnus-number article)))
      (let ((saved-point (tessera-entry-save-point))
            (tessera-gnus-summary--updating t))
        (tessera-entry-clear-current)
        (tessera--month-clear-display)
        (unwind-protect
            (progn
              (goto-char position)
              (tessera-gnus-summary--sync-line t))
          (tessera-entry-restore-point saved-point)
          (tessera-month-sync)))))
  (tessera-entry-highlight-current))

(defun tessera-gnus-summary--update-line ()
  "Synchronize the article just updated by Gnus."
  (when (and tessera-gnus-summary--active
             (not tessera-gnus-summary--updating))
    (setq tessera-gnus-summary--dirty t)
    ;; A command may mark many articles.  Rebuild month and thread
    ;; counts once after the command, when all marks have settled.
    (unless tessera--month-enabled
      (let ((tessera-gnus-summary--updating t)
            (saved-point (tessera-entry-save-point)))
        (unwind-protect
            (tessera-gnus-summary--sync-line)
          (tessera-entry-restore-point saved-point))))))

(defun tessera-gnus-summary--prepare ()
  "Attach entry layouts after Gnus has generated a summary."
  (when tessera-gnus-summary--active
    (tessera-gnus-summary--prune-content-cache)
    (tessera-entry-clear-current)
    (tessera-entry-clear-layout)
    (tessera-gnus-summary--sync-buffer)
    (setq tessera-gnus-summary--dirty nil
          tessera-gnus-summary--appearance
          (tessera-gnus-summary--appearance))
    (tessera-entry-highlight-current)))

(defun tessera-gnus-summary--changed (_start _end _old-length)
  "Record a native summary buffer change."
  (unless tessera-gnus-summary--updating
    (setq tessera-gnus-summary--dirty t)))

(defun tessera-gnus-summary--post-command ()
  "Synchronize native changes and highlight the current entry."
  (when tessera-gnus-summary--active
    (setq-local tessera--month-enabled
                (tessera-gnus-summary--month-enabled-p))
    (setq-local tessera--month-thread-date
                (tessera-gnus-summary--month-thread-date))
    (let* ((appearance (tessera-gnus-summary--appearance))
           (force (or (not (equal appearance
                                  tessera-gnus-summary--appearance))
                      (and (symbolp this-command)
                           (string-prefix-p
                            "gnus-registry-"
                            (symbol-name this-command))))))
      (when (or force tessera-gnus-summary--dirty)
        (tessera-gnus-summary--sync-buffer force)
        (setq tessera-gnus-summary--appearance appearance
              tessera-gnus-summary--dirty nil)))
    ;; Already at the root, Gnus's top-thread command does not move.
    (when (and tessera--month-enabled tessera--month-groups
               gnus-show-threads
               (eq this-command 'gnus-summary-top-thread))
      (gnus-summary-position-point))
    (tessera-month-reveal-point)
    (tessera-entry-highlight-current)))

(defun tessera-gnus-summary--resize (_frame)
  "Update entries after a window size change."
  (tessera-gnus-summary--post-command))

(defun tessera-gnus-summary--refresh ()
  "Recompile the native summary format and regenerate articles."
  (when gnus-newsgroup-headers
    (let ((article (get-text-property (point) 'gnus-number))
          (gnus-summary-buffer (current-buffer)))
      (gnus-update-format-specifications nil 'summary)
      (gnus-update-summary-mark-positions)
      (gnus-summary-prepare)
      (when article (gnus-summary-goto-subject article)))))

(defun tessera-gnus-summary--restore-native-state ()
  "Restore native state in the current Gnus summary buffer."
  (setq tessera-gnus-summary--active nil)
  (remove-hook 'gnus-summary-update-hook
               #'tessera-gnus-summary--update-line t)
  (remove-hook 'gnus-summary-prepare-hook
               #'tessera-gnus-summary--prepare t)
  (remove-hook 'after-change-functions
               #'tessera-gnus-summary--changed t)
  (remove-hook 'post-command-hook
               #'tessera-gnus-summary--post-command t)
  (remove-hook 'window-size-change-functions
               #'tessera-gnus-summary--resize t)
  (remove-hook 'change-major-mode-hook
               #'tessera-gnus-summary--disable t)
  (tessera-entry-clear-current)
  (tessera-month-clear)
  (tessera-gnus-summary--update-month-sorting)
  (tessera-entry-clear-layout)
  (tessera--restore-settings tessera-gnus-summary--saved-settings)
  (setq tessera-gnus-summary--saved-settings nil
        tessera-gnus-summary--saved-month-sorting nil
        tessera-gnus-summary--appearance nil
        tessera-gnus-summary--dirty nil
        tessera-gnus-summary--content-cache nil
        tessera-gnus-summary--threads nil
        tessera-gnus-summary--thread-width 8))

(defun tessera-gnus-summary--enable ()
  "Enable Tessera in the current Gnus summary buffer."
  (unless tessera-gnus-summary--active
    (setq tessera-gnus-summary--saved-settings
          (tessera--save-settings
           '(gnus-summary-line-format tessera-entry-layout)))
    (let (completed)
      (unwind-protect
          (progn
            (setq-local gnus-summary-line-format "%u&tessera;\n")
            (setq-local tessera-entry-layout 'two-line)
            (setq-local tessera--month-enabled
                        (tessera-gnus-summary--month-enabled-p))
            (setq-local tessera--month-thread-date
                        (tessera-gnus-summary--month-thread-date))
            (tessera-gnus-summary--update-month-sorting)
            (setq tessera-gnus-summary--active t)
            (add-hook 'gnus-summary-update-hook
                      #'tessera-gnus-summary--update-line t t)
            (add-hook 'gnus-summary-prepare-hook
                      #'tessera-gnus-summary--prepare t t)
            (add-hook 'after-change-functions
                      #'tessera-gnus-summary--changed nil t)
            (add-hook 'post-command-hook
                      #'tessera-gnus-summary--post-command t t)
            (add-hook 'window-size-change-functions
                      #'tessera-gnus-summary--resize nil t)
            (add-hook 'change-major-mode-hook
                      #'tessera-gnus-summary--disable nil t)
            (tessera-gnus-summary--refresh)
            (setq completed t))
        (unless completed
          (condition-case nil
              (progn
                (tessera-gnus-summary--restore-native-state)
                (tessera-gnus-summary--refresh))
            (error nil)))))))

(defun tessera-gnus-summary--disable ()
  "Restore the native summary presentation."
  (when tessera-gnus-summary--active
    (tessera-gnus-summary--restore-native-state)
    (condition-case error-data
        (tessera-gnus-summary--refresh)
      (error
       (ignore-errors (tessera-gnus-summary--refresh))
       (signal (car error-data) (cdr error-data))))))

(defun tessera-gnus-summary--glyphs-changed (option)
  "Refresh active gnus views after glyph OPTION changes.
Nil means explicitly refresh all glyphs and their hover faces."
  (when (or (null option)
            (memq option '(tessera-gnus-summary-glyphs
                           tessera-month-glyphs
                           tessera-thread-glyphs
                           tessera-entry-ellipsis
                           tessera-glyph-style tessera-glyph-color)))
    (tessera-gnus-summary--months-changed nil)))

(defun tessera-gnus-summary--months-changed (option)
  "Refresh active Gnus views affected by month OPTION.
Nil requests a full refresh, including glyphs and sorting."
  (when (or (null option)
            (memq option '(tessera-month-grouping
                           tessera-month-thread-date
                           tessera-gnus-summary-month-grouping
                           tessera-gnus-summary-month-thread-date)))
    (when (gethash 'gnus-summary tessera--entry-backends)
      (tessera-gnus-summary--register))
    (save-window-excursion
      (tessera--map-mode-buffers
       'gnus-summary-mode
       (lambda ()
         (when tessera-gnus-summary--active
           (setq-local tessera--month-enabled
                       (tessera-gnus-summary--month-enabled-p))
           (setq-local tessera--month-thread-date
                       (tessera-gnus-summary--month-thread-date))
           (let ((sort-changed
                  (tessera-gnus-summary--update-month-sorting)))
             (tessera-entry-clear-current)
             (if (and sort-changed gnus-newsgroup-headers)
                 (tessera-gnus-summary--refresh)
               (tessera-gnus-summary--sync-buffer t)))
           (setq tessera-gnus-summary--dirty nil
                 tessera-gnus-summary--appearance
                 (tessera-gnus-summary--appearance))
           (tessera-entry-highlight-current)))))))

(provide 'tessera-gnus-summary)
;;; tessera-gnus-summary.el ends here
