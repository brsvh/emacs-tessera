;;; tessera-ui.el --- UI primitives for Tessera  -*- lexical-binding: t; -*-

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

;; Shared presentation primitives for Tessera interface packages.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tessera)

(cl-defstruct
    (tessera-ui-segment (:constructor tessera-ui-segment-create)
                        (:copier nil))
  "One semantic content segment in a flat entry.

NAME identifies the segment for styling and presentation.  TEXT is
its display text.  OVERFLOW is one of `preserve', `truncate', or
`hide'.  SEPARATOR is nil or a semantic spacing name.  ALTERNATIVES is
a widest-to-narrowest list of complete display strings used instead
of character truncation.  Source adapters must not store rendered
spacing strings or numeric widths in SEPARATOR; the presenter resolves
it at display time."
  name
  text
  overflow
  separator
  alternatives)

(cl-defstruct
    (tessera-ui-header-line (:constructor tessera-ui-header-line-create)
                            (:copier nil))
  "Source-neutral semantic content for one Tessera header line.

LEFT-SEGMENTS and RIGHT-SEGMENTS contain ordered `tessera-ui-segment'
values.  The presenter places one flexible gap between the two sides."
  source
  left-segments
  right-segments)

(cl-defstruct
    (tessera-ui-slot (:constructor tessera-ui-slot-create)
                     (:copier nil))
  "One compact status glyph in an entry slot rail.

NAME identifies the slot for styling and presentation.  TEXT is its
display text.  Slots contain compact status symbols rather than labels
or other descriptive text."
  name
  text)

(cl-defstruct
    (tessera-ui-entry-row (:constructor tessera-ui-entry-row-create)
                          (:copier nil))
  "Source-neutral semantic content for one list row.

SLOTS contains compact status glyphs.  LEFT-SEGMENTS and
RIGHT-SEGMENTS contain ordered `tessera-ui-segment' values.  RAIL-TEXT
is semantic content used instead of SLOTS in the shared rail."
  slots
  rail-text
  left-segments
  right-segments)

(cl-defstruct
    (tessera-ui-thread-statistic (:constructor tessera-ui-thread-statistic-create)
                                 (:copier nil))
  "Semantic counts displayed in a thread heading.

UNREAD is the number of unread visible members.  VISIBLE is the
number of represented members.  KNOWN is the backend's known member
count.  EXACTNESS describes KNOWN and is one of `exact',
`lower-bound', or `unknown'."
  unread
  visible
  known
  exactness)

(cl-defstruct
    (tessera-ui-month-statistics (:constructor tessera-ui-month-statistics-create)
                                 (:copier nil))
  "Semantic counts displayed for one calendar month."
  unread
  total)

(cl-defstruct
    (tessera-ui-month-group (:constructor tessera-ui-month-group-create)
                            (:copier nil))
  "Source-neutral semantic content for one calendar month group.

SOURCE names the adapter that produced the group.  KEY is its stable
source key.  YEAR and MONTH-NAME form the visible heading.
STATISTICS describes every represented message, while ITEMS contains
ordered opaque source entries belonging to the group."
  source
  key
  year
  month-name
  statistics
  items)

(defvar tessera-ui--month-registrations
  (make-hash-table :test #'eq)
  "Month group providers keyed by source.")

(defun tessera-ui-month-register (source provider)
  "Register the month group PROVIDER for SOURCE.

PROVIDER receives a derivation context and returns ordered
`tessera-ui-month-group' values."
  (unless (symbolp source)
    (error "Month source must be a symbol"))
  (unless (functionp provider)
    (error "Invalid month provider for %S: %S" source provider))
  (puthash source provider tessera-ui--month-registrations)
  source)

(defun tessera-ui--month-validate-statistics (source statistics)
  "Validate SOURCE month STATISTICS."
  (unless (and (tessera-ui-month-statistics-p statistics)
               (natnump (tessera-ui-month-statistics-unread statistics))
               (natnump (tessera-ui-month-statistics-total statistics))
               (<= (tessera-ui-month-statistics-unread statistics)
                   (tessera-ui-month-statistics-total statistics)))
    (error "Invalid month statistics for %S: %S"
           source statistics)))

(defun tessera-ui--month-validate-group (source group)
  "Validate and return SOURCE month GROUP."
  (unless (and (tessera-ui-month-group-p group)
               (eq source (tessera-ui-month-group-source group))
               (consp (tessera-ui-month-group-key group))
               (or (null (tessera-ui-month-group-year group))
                   (natnump (tessera-ui-month-group-year group)))
               (stringp (tessera-ui-month-group-month-name group))
               (listp (tessera-ui-month-group-items group)))
    (error "Invalid month group for %S: %S" source group))
  (tessera-ui--month-validate-statistics source (tessera-ui-month-group-statistics group))
  group)

(defun tessera-ui-make-month-groups (source context)
  "Return registered month groups for SOURCE and CONTEXT."
  (let ((provider (gethash source tessera-ui--month-registrations)))
    (unless provider
      (error "No Tessera month registration for %S" source))
    (let ((groups (funcall provider context))
          (keys (make-hash-table :test #'equal)))
      (unless (listp groups)
        (error "Month provider for %S returned invalid groups: %S"
               source groups))
      (dolist (group groups)
        (tessera-ui--month-validate-group source group)
        (let ((key (tessera-ui-month-group-key group)))
          (when (gethash key keys)
            (error "Duplicate month key for %S: %S" source key))
          (puthash key t keys)))
      groups)))

(cl-defstruct
    (tessera-ui-thread-connector (:constructor tessera-ui-thread-connector-create)
                                 (:copier nil))
  "Source-neutral topology before a thread member.

ANCESTORS is a list of booleans from outermost to innermost depth.  A
non-nil value means that the ancestor continues below this member.
KIND is one of `root', `branch', `last', or `orphan'."
  ancestors
  kind)

(cl-defstruct
    (tessera-ui-thread-member (:constructor tessera-ui-thread-member-create)
                              (:copier nil))
  "Source-neutral semantic content for one thread member."
  key
  slots
  connector
  left-segments
  right-segments)

(cl-defstruct
    (tessera-ui-thread (:constructor tessera-ui-thread-create)
                       (:copier nil))
  "Source-neutral semantic content for one complete thread."
  source
  key
  statistic
  main-left-segments
  main-right-segments
  members)

(defconst tessera-ui-glyph-mark-slots
  '(main secondary tertiary other)
  "Mark slots in descending semantic priority.")

(defconst tessera-ui-glyph-feature-slots
  '(primary secondary overflow)
  "Feature slots in descending semantic priority.")

(defconst tessera-ui-glyph-mark-display-slots
  '(other tertiary secondary main)
  "Mark slots in their left-to-right rail order.")

(cl-defstruct
    (tessera-ui-glyph-spec (:constructor tessera-ui-glyph-spec-create)
                           (:copier nil))
  "One source fact registered for a semantic glyph slot."
  source
  kind
  fact
  slot
  priority
  ascii
  unicode
  nerd-icon
  face
  description)

(cl-defstruct
    (tessera-ui-glyph-selection (:constructor tessera-ui-glyph-selection-create)
                                (:copier nil))
  "Selected glyph SPECS and the HIDDEN source facts."
  specs
  hidden)

(cl-defstruct
    (tessera-ui-entry-rail (:constructor tessera-ui-entry-rail-create)
                           (:copier nil))
  "Shared slot rail specification for an entry.

COLUMNS is the number of fixed cells.  CELL-WIDTH and GAP are pixel
measurements shared by every source using the same symbol style."
  columns
  cell-width
  gap)

(cl-defstruct
    (tessera-ui-window-presentations (:constructor tessera-ui-window-presentations-create)
                                     (:copier nil))
  "Window-local overlay and refresh state for one buffer."
  overlays
  timer
  updating-p
  window-starts)

(cl-defstruct
    (tessera-ui-flat-entry (:constructor tessera-ui-flat-entry-create)
                           (:copier nil))
  "Source-neutral semantic content for a two-row entry.

SOURCE names the adapter that produced the entry, and KEY is its
stable source key.  MAIN-SLOTS and EXTRA-SLOTS contain compact status
glyphs.  The remaining fields contain `tessera-ui-segment' values for
the left and right sides of each row.

Both slot rails are structural even when their lists are nil.  A
presenter must reserve one shared rail width for both rows and start
MAIN-LEFT-SEGMENTS and EXTRA-LEFT-SEGMENTS at the same horizontal
position.  Source adapters put subject-like content in
MAIN-LEFT-SEGMENTS, author and feature content in
EXTRA-LEFT-SEGMENTS, and labels in EXTRA-RIGHT-SEGMENTS."
  source
  key
  main-slots
  main-left-segments
  main-right-segments
  extra-slots
  extra-left-segments
  extra-right-segments)

(cl-defgeneric tessera-ui-make-flat-entry (source item)
  "Return a flat entry for ITEM supplied by SOURCE.")

(cl-defgeneric tessera-ui-make-thread (source item)
  "Return a semantic thread for ITEM supplied by SOURCE.")

(defun tessera-ui-make-segment (name text overflow &optional face separator alternatives)
  "Return a semantic segment named NAME containing TEXT.

OVERFLOW, FACE, and SEPARATOR control presentation.  ALTERNATIVES is
a widest-to-narrowest list of complete fallback strings."
  (let ((text (copy-sequence (or text "")))
        (alternatives
         (mapcar #'copy-sequence alternatives)))
    (when face
      (add-face-text-property 0 (length text) face t text)
      (dolist (alternative alternatives)
        (add-face-text-property 0 (length alternative) face t alternative)))
    (tessera-ui-segment-create
     :name name
     :text text
     :overflow overflow
     :separator separator
     :alternatives alternatives)))

(defface tessera-ui-header
  '((t :inherit header-line))
  "Face for a complete Tessera header line."
  :group 'tessera)

(defface tessera-ui-header-status
  '((t :inherit tessera-ui-header :weight bold))
  "Face for backend status in a Tessera header line."
  :group 'tessera)

(defface tessera-ui-header-status-success
  '((t :inherit (success tessera-ui-header-status)))
  "Face for a successful backend status in a Tessera header line."
  :group 'tessera)

(defface tessera-ui-header-status-processing
  '((t :inherit
       (font-lock-variable-name-face tessera-ui-header-status)))
  "Face for an ongoing backend status in a Tessera header line."
  :group 'tessera)

(defface tessera-ui-header-status-fail
  '((t :inherit (error tessera-ui-header-status)))
  "Face for a failed backend status in a Tessera header line."
  :group 'tessera)

(defface tessera-ui-header-status-warning
  '((t :inherit (warning tessera-ui-header-status)))
  "Face for a warning backend status in a Tessera header line."
  :group 'tessera)

(defface tessera-ui-header-query
  '((t :inherit tessera-ui-header))
  "Face for the current query in a Tessera header line."
  :group 'tessera)

(defface tessera-ui-header-query-prefix
  '((t :inherit tessera-ui-header-query :weight bold))
  "Face for the prefix of a Tessera header query."
  :group 'tessera)

(defface tessera-ui-header-query-condition
  '((t :inherit (font-lock-keyword-face tessera-ui-header-query)))
  "Face for the condition of a Tessera header query."
  :group 'tessera)

(defface tessera-ui-header-context
  '((t :inherit (font-lock-constant-face tessera-ui-header)))
  "Face for the current backend context in a Tessera header line."
  :group 'tessera)

(defface tessera-ui-header-statistics
  '((t :inherit (shadow tessera-ui-header)))
  "Face for list statistics in a Tessera header line."
  :group 'tessera)

(defface tessera-ui-header-statistics-unread
  '((t :inherit (error tessera-ui-header-statistics)))
  "Face for a nonzero unread count in header statistics."
  :group 'tessera)

(defface tessera-ui-header-statistics-success
  '((t :inherit (success tessera-ui-header-statistics)))
  "Face for a successful count in header statistics."
  :group 'tessera)

(defface tessera-ui-month-heading
  '((t :inherit bold))
  "Face for a month heading in a Tessera list."
  :group 'tessera)

(defface tessera-ui-month-heading-highlight
  '((t :inherit highlight :extend t))
  "Face for a month heading under the mouse."
  :group 'tessera)

(defface tessera-ui-month-metric
  '((t :inherit shadow :weight normal))
  "Face for statistics in a collapsed month heading."
  :group 'tessera)

(defface tessera-ui-month-metric-unread
  '((t :inherit (error tessera-ui-month-metric)))
  "Face for a nonzero unread count in a month heading."
  :group 'tessera)

(defface tessera-ui-thread-heading
  '((t :weight bold))
  "Face for a thread heading in a Tessera list."
  :group 'tessera)

(defface tessera-ui-thread-metric
  '((t :inherit shadow :weight normal))
  "Face for the member metric in a thread heading."
  :group 'tessera)

(defface tessera-ui-thread-metric-unread
  '((t :inherit (error tessera-ui-thread-metric)))
  "Face for a nonzero unread count in a thread heading."
  :group 'tessera)

(defface tessera-ui-thread-connector
  '((t :inherit (shadow fixed-pitch)))
  "Face for every stroke in a thread tree."
  :group 'tessera)

(defface tessera-ui-entry-timestamp
  '((t :inherit shadow :weight normal))
  "Face for an entry timestamp."
  :group 'tessera)

(defface tessera-ui-entry-timestamp-unread
  '((t :weight bold))
  "Face added to the timestamp of an unread entry."
  :group 'tessera)

(defface tessera-ui-entry-current
  '((t :inherit highlight :extend t))
  "Face for the current complete entry."
  :group 'tessera)

(defface tessera-ui-entry-subject
  '((t :inherit tessera-ui-entry-timestamp :weight bold))
  "Face for the subject of a read entry."
  :group 'tessera)

(defface tessera-ui-entry-author
  '((t :inherit tessera-ui-entry-timestamp :slant italic))
  "Face for the author of a read entry."
  :group 'tessera)

(defun tessera-ui--face-foreground (face)
  "Return the resolved foreground represented by FACE."
  (cond
   ((stringp face) face)
   ((and (symbolp face) (facep face))
    (face-foreground face nil t))
   ((and (consp face) (keywordp (car face)))
    (let ((foreground (plist-get face :foreground)))
      (or (and (stringp foreground) foreground)
          (tessera-ui--face-foreground (plist-get face :inherit)))))
   ((listp face)
    (seq-some #'tessera-ui--face-foreground face))))

(defun tessera-ui-entry-author-face (native-face &optional unread)
  "Return an italic color-only author face from NATIVE-FACE.

Make the result bold when UNREAD is non-nil."
  (list :foreground
        (or (tessera-ui--face-foreground native-face)
            (face-foreground 'default nil t))
        :weight (if unread 'bold 'normal)
        :slant 'italic))

(defface tessera-ui-entry-feature
  '((t :inherit shadow))
  "Face for an entry content feature."
  :group 'tessera)

(defconst tessera-ui--header-top-padding 4
  "Header padding above the content row, in pixels.")

(defconst tessera-ui--header-bottom-padding 4
  "Header padding below the content row, in pixels.")

(defconst tessera-ui--header-horizontal-padding 1
  "Header padding at each horizontal edge, in spaces.")

(defconst tessera-ui--header-safety-gap 1
  "Gap between a header and each window edge, in pixels.")

(defconst tessera-ui--entry-top-padding 8
  "Entry padding above the primary row, in pixels.")

(defconst tessera-ui--entry-bottom-padding 8
  "Entry padding below the secondary row, in pixels.")

(defconst tessera-ui--entry-horizontal-padding 2
  "Entry padding at each horizontal edge, in spaces.")

(defconst tessera-ui--entry-safety-gap 1
  "Gap between an entry and each window edge, in pixels.")

(defconst tessera-ui--entry-slot-gap 3
  "Gap between adjacent entry slots, in pixels.")

(defconst tessera-ui-entry-slot-count
  (length tessera-ui-glyph-mark-slots)
  "Number of cells in a shared entry status rail.")

(defconst tessera-ui--month-heading-top-padding 8
  "Month heading padding above the content row, in pixels.")

(defconst tessera-ui--month-heading-bottom-padding 8
  "Month heading padding below the content row, in pixels.")

(defconst tessera-ui--month-heading-horizontal-padding 2
  "Month heading padding at each horizontal edge, in spaces.")

(defconst tessera-ui--month-heading-safety-gap 1
  "Gap between a month heading and the window edge, in pixels.")

(defconst tessera-ui--thread-heading-top-padding 12
  "Thread heading padding above the content row, in pixels.")

(defconst tessera-ui--thread-heading-bottom-padding 4
  "Thread heading padding below the content row, in pixels.")

(defun tessera-ui--header-space (element)
  "Return one header space named ELEMENT."
  (propertize " "
              'face 'tessera-ui-header
              'tessera-ui--element element))

(defun tessera-ui--header-padding (element)
  "Return horizontal header padding named ELEMENT."
  (propertize (make-string tessera-ui--header-horizontal-padding ?\s)
              'face 'tessera-ui-header
              'tessera-ui--element element))

(defun tessera-ui--header-edge-safety-gap ()
  "Return one exact safety gap for a header edge."
  (propertize " "
              'face 'tessera-ui-header
              'display `(space :width (,tessera-ui--header-safety-gap))
              'tessera-ui--element 'header.safety-gap))

(defun tessera-ui--vertical-padding (face element top bottom window)
  "Return zero-width vertical padding using FACE and named ELEMENT.

TOP and BOTTOM are the added pixels above and below the baseline.
Use WINDOW to resolve font metrics."
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
                      (round (* height
                                (/ (float font-ascent) font-height)))))
                `(space
                  :width (0)
                  :height (,(+ height top bottom))
                  :ascent (,(+ ascent top))))
            '(space :width (0)))))
    (propertize " "
                'face face
                'display display
                'tessera-ui--element element)))

(defun tessera-ui-vertical-padding (face element top bottom)
  "Return vertical padding using FACE and named ELEMENT.

TOP and BOTTOM are the added pixels above and below the baseline."
  (tessera-ui--vertical-padding face element top bottom
                                (or (get-buffer-window (current-buffer) t)
                                    (selected-window))))

(defun tessera-ui-vertical-spacer (element pixels)
  "Return a display-only vertical spacer named ELEMENT.

PIXELS is the exact height of the spacer."
  (concat (propertize " "
                      'display
                      `(space :width (0) :height (,pixels) :ascent (,pixels))
                      'tessera-ui--element element)
          (propertize "\n"
                      'line-height t
                      'tessera-ui--element element)))

(defun tessera-ui--header-vertical-padding (element window)
  "Return header padding named ELEMENT.

Use WINDOW to resolve font metrics."
  (tessera-ui--vertical-padding 'tessera-ui-header
                                element
                                tessera-ui--header-top-padding
                                tessera-ui--header-bottom-padding
                                window))

(defun tessera-ui--month-heading-vertical-padding (element window)
  "Return month heading padding named ELEMENT.

Use WINDOW to resolve font metrics."
  (tessera-ui--vertical-padding 'tessera-ui-month-heading
                                element
                                tessera-ui--month-heading-top-padding
                                tessera-ui--month-heading-bottom-padding
                                window))

(defun tessera-ui--thread-heading-vertical-padding (element top bottom window)
  "Return thread heading padding named ELEMENT.

TOP and BOTTOM are the added pixels.  Use WINDOW to resolve font
metrics."
  (tessera-ui--vertical-padding 'tessera-ui-thread-heading element top bottom window))

(defun tessera-ui-entry-space (element)
  "Return one entry space named ELEMENT."
  (propertize " " 'tessera-ui--element element))

(defun tessera-ui-entry-padding (element)
  "Return horizontal entry padding named ELEMENT."
  (propertize (make-string tessera-ui--entry-horizontal-padding ?\s)
              'tessera-ui--element element))

(defun tessera-ui-entry-leading-safety-gap ()
  "Return the safety gap before an entry row."
  (propertize " "
              'display `(space :width (,tessera-ui--entry-safety-gap))
              'tessera-ui--element 'entry.safety-gap))

(defun tessera-ui-entry-trailing-safety-gap ()
  "Return the marker for the safety gap after an entry row."
  (propertize " "
              'display '(space :width (0))
              'tessera-ui--element 'entry.safety-gap))

(defun tessera-ui-entry-top-padding ()
  "Return zero-width padding above an entry."
  (tessera-ui--vertical-padding 'default
                                'entry.top-padding
                                tessera-ui--entry-top-padding
                                0
                                (or (get-buffer-window (current-buffer) t)
                                    (selected-window))))

(defun tessera-ui-entry-bottom-padding ()
  "Return zero-width padding below an entry."
  (tessera-ui--vertical-padding 'default
                                'entry.bottom-padding
                                0
                                tessera-ui--entry-bottom-padding
                                (or (get-buffer-window (current-buffer) t)
                                    (selected-window))))

(defun tessera-ui--month-metric-text (text element)
  "Return month metric TEXT named ELEMENT."
  (propertize text 'tessera-ui--element element))

(defun tessera-ui-format-month-statistics (statistics)
  "Return the display text for month STATISTICS."
  (let* ((unread (tessera-ui-month-statistics-unread statistics))
         (total (tessera-ui-month-statistics-total statistics))
         (unread-count
          (tessera-ui--month-metric-text (number-to-string unread)
                                         'month.statistics.unread-count))
         (total-count
          (tessera-ui--month-metric-text (number-to-string total)
                                         'month.statistics.total-count))
         (metric
          (concat unread-count
                  (tessera-ui--month-metric-text " " 'month.statistics.separator)
                  (tessera-ui--month-metric-text "unread" 'month.statistics.unread-text)
                  (tessera-ui--month-metric-text " · " 'month.statistics.special-separator)
                  total-count
                  (tessera-ui--month-metric-text " " 'month.statistics.separator)
                  (tessera-ui--month-metric-text "total" 'month.statistics.total-text))))
    (when (> unread 0)
      (add-face-text-property 0 (length unread-count) 'tessera-ui-month-metric-unread nil metric))
    (add-face-text-property 0 (length metric) 'tessera-ui-month-metric t metric)
    (add-text-properties 0 (length metric) '(tessera-ui--parent-element month.statistics) metric)
    metric))

(defun tessera-ui--month-heading-padding (element)
  "Return horizontal month heading padding named ELEMENT."
  (propertize (make-string tessera-ui--month-heading-horizontal-padding ?\s)
              'tessera-ui--element element))

(defun tessera-ui--month-heading-safety-gap ()
  "Return an exact month heading edge safety gap."
  (propertize " "
              'display `(space :width (,tessera-ui--month-heading-safety-gap))
              'tessera-ui--element 'month.heading.safety-gap))

(defun tessera-ui-format-month-heading (group collapsed-p &optional window)
  "Render month GROUP for WINDOW.

Display the group's statistics only when COLLAPSED-P is non-nil."
  (let* ((window (or window (selected-window)))
         (top-padding
          (tessera-ui--month-heading-vertical-padding 'month.heading.top-padding window))
         (leading-safety-gap
          (tessera-ui--month-heading-safety-gap))
         (left-padding
          (tessera-ui--month-heading-padding 'month.heading.left-padding))
         (year
          (and (tessera-ui-month-group-year group)
               (propertize (format "%04d"
                                   (tessera-ui-month-group-year group))
                           'tessera-ui--element 'month.heading.year)))
         (separator
          (and year
               (propertize " " 'tessera-ui--element 'month.heading.separator)))
         (month
          (propertize (copy-sequence (tessera-ui-month-group-month-name group))
                      'tessera-ui--element 'month.heading.month))
         (statistics
          (and collapsed-p
               (tessera-ui-format-month-statistics (tessera-ui-month-group-statistics group))))
         (right-padding
          (tessera-ui--month-heading-padding 'month.heading.right-padding))
         (trailing-safety-gap
          (tessera-ui--month-heading-safety-gap))
         (bottom-padding
          (tessera-ui--month-heading-vertical-padding 'month.heading.bottom-padding window))
         (left-fixed
          (concat leading-safety-gap left-padding year separator))
         (right
          (concat statistics right-padding trailing-safety-gap))
         (month
          (tessera-ui-truncate-pixels month
                                      (max 0
                                           (- (window-body-width window t)
                                              (string-pixel-width left-fixed)
                                              (string-pixel-width right)))))
         (flex-gap
          (tessera-ui--flex-gap right tessera-ui--month-heading-safety-gap 'month.heading.flex-gap))
         (heading
          (concat top-padding left-fixed month flex-gap right
                  bottom-padding)))
    (add-face-text-property 0 (length heading) 'tessera-ui-month-heading t heading)
    (let ((position 0))
      (while (< position (length heading))
        (let ((next
               (next-single-property-change position 'tessera-ui--parent-element
                                            heading (length heading))))
          (unless (get-text-property position 'tessera-ui--parent-element heading)
            (put-text-property position next 'tessera-ui--parent-element 'month.heading heading))
          (setq position next))))
    heading))

(defun tessera-ui-format-thread-metric (unread members incomplete)
  "Return a thread metric for UNREAD and MEMBERS.

Append an incomplete indicator when INCOMPLETE is non-nil."
  (let* ((unread-count
          (and (> unread 0)
               (propertize (number-to-string unread)
                           'tessera-ui--element 'thread.metric.unread-count)))
         (separator
          (and unread-count
               (propertize "/" 'tessera-ui--element 'thread.metric.separator)))
         (member-count
          (propertize (number-to-string members)
                      'tessera-ui--element 'thread.metric.member-count))
         (indicator
          (and incomplete
               (propertize "+" 'tessera-ui--element 'thread.metric.incomplete)))
         (metric
          (concat unread-count separator member-count indicator)))
    (add-face-text-property 0 (length metric) 'tessera-ui-thread-metric t metric)
    (when unread-count
      (add-face-text-property 0 (length unread-count) 'tessera-ui-thread-metric-unread nil metric))
    (add-text-properties 0 (length metric) '(tessera-ui--parent-element thread.metric) metric)
    metric))

(defun tessera-ui-truncate-pixels (text width)
  "Truncate TEXT with an ellipsis to at most WIDTH pixels."
  (cond
   ((<= width 0) "")
   ((<= (string-pixel-width text) width) text)
   (t
    (let* ((end (1- (length text)))
           (ellipsis
            (let ((ellipsis (copy-sequence "…")))
              (add-text-properties 0 1 (text-properties-at end text) ellipsis)
              ellipsis))
           (ellipsis-width
            (string-pixel-width ellipsis))
           (glyphs (string-glyph-split text))
           (result ""))
      (if (> ellipsis-width width)
          ""
        (while (and glyphs
                    (<= (string-pixel-width (concat result (car glyphs) ellipsis))
                        width))
          (setq result (concat result (pop glyphs))))
        (concat result ellipsis))))))

(defun tessera-ui--entry-pixel-space (width)
  "Return a display space that is WIDTH pixels wide."
  (if (<= width 0)
      ""
    (propertize " " 'display `(space :width (,width)))))

(defun tessera-ui--entry-cell-probe (style)
  "Return a cell-width probe string for symbol STYLE."
  (pcase style
    ('ascii "M")
    ('unicode "⏸")
    ('nerd-icons
     (propertize "" 'face
                 '(:family "Symbols Nerd Font Mono" :height 0.8)))
    (_ "M")))

(defun tessera-ui-make-entry-rail (style &optional columns)
  "Return a shared entry rail for symbol STYLE.

COLUMNS defaults to `tessera-ui-entry-slot-count'."
  (tessera-ui-entry-rail-create
   :columns (or columns tessera-ui-entry-slot-count)
   :cell-width
   (string-pixel-width (tessera-ui--entry-cell-probe style))
   :gap tessera-ui--entry-slot-gap))

(defun tessera-ui-entry-rail-pixel-width (rail)
  "Return the complete pixel width of RAIL."
  (+ (* (tessera-ui-entry-rail-columns rail)
        (tessera-ui-entry-rail-cell-width rail))
     (* (max 0
             (1- (tessera-ui-entry-rail-columns rail)))
        (tessera-ui-entry-rail-gap rail))))

(defun tessera-ui--entry-slot-cell (slot width)
  "Return SLOT centered in a cell of pixel WIDTH."
  (if (null slot)
      (tessera-ui--entry-pixel-space width)
    (let* ((text (tessera-ui-slot-text slot))
           (extra
            (max 0 (- width (string-pixel-width text))))
           (leading (/ extra 2)))
      (concat
       (tessera-ui--entry-pixel-space leading)
       text
       (tessera-ui--entry-pixel-space (- extra leading))))))

(defun tessera-ui--entry-slot-rail (slots rail element)
  "Return SLOTS right-aligned in a rail named ELEMENT.

RAIL defines the number and width of fixed cells."
  (let* ((columns (tessera-ui-entry-rail-columns rail))
         (slots (last slots columns))
         (cells
          (append (make-list (- columns (length slots)) nil)
                  slots))
         (text
          (mapconcat (lambda (slot)
                       (tessera-ui--entry-slot-cell slot
                                                    (tessera-ui-entry-rail-cell-width rail)))
                     cells
                     (tessera-ui--entry-pixel-space (tessera-ui-entry-rail-gap rail)))))
    (add-text-properties 0 (length text) (list 'tessera-ui--element element) text)
    text))

(defun tessera-ui--entry-segments (segments)
  "Return the display text for SEGMENTS."
  (let ((first t)
        (text ""))
    (dolist (segment segments)
      (let ((value
             (copy-sequence (or (tessera-ui-segment-text segment) ""))))
        (unless (string-empty-p value)
          (add-text-properties 0 (length value) (list 'tessera-ui--element (tessera-ui-segment-name segment)) value)
          (setq text
                (concat text
                        (if first
                            ""
                          (if-let* ((separator
                                     (tessera-ui-segment-separator segment)))
                              (tessera-ui-entry-space separator)
                            ""))
                        value)
                first nil))))
    text))

(defun tessera-ui--entry-fit-left (segments width)
  "Fit left-side SEGMENTS within WIDTH pixels."
  (let* ((full (tessera-ui--entry-segments segments))
         (reduced
          (tessera-ui--entry-segments (seq-remove (lambda (segment)
                                                    (eq (tessera-ui-segment-overflow segment) 'hide))
                                                  segments)))
         (text
          (if (<= (string-pixel-width full) width)
              full
            reduced)))
    (tessera-ui-truncate-pixels text width)))

(defun tessera-ui--entry-fit-right (segments width)
  "Fit right-side SEGMENTS within WIDTH pixels."
  (let* ((required
          (seq-remove (lambda (segment)
                        (eq (tessera-ui-segment-overflow segment) 'hide))
                      segments))
         (full (tessera-ui--entry-segments segments))
         (fitted (copy-sequence required)))
    (if (<= (string-pixel-width full) width)
        full
      (let ((tail fitted))
        (while tail
          (let* ((segment (car tail))
                 (current
                  (tessera-ui--entry-segments fitted))
                 (excess
                  (- (string-pixel-width current) width)))
            (when (and (> excess 0)
                       (eq (tessera-ui-segment-overflow segment)
                           'truncate))
              (let* ((text (or (tessera-ui-segment-text segment) ""))
                     (target
                      (max 0
                           (- (string-pixel-width text) excess)))
                     (alternatives
                      (tessera-ui-segment-alternatives segment))
                     (replacement
                      (if alternatives
                          (or
                           (seq-find (lambda (alternative)
                                       (<= (string-pixel-width alternative)
                                           target))
                                     alternatives)
                           (car (last alternatives)))
                        (tessera-ui-truncate-pixels text target))))
                (setcar tail
                        (tessera-ui-segment-create
                         :name (tessera-ui-segment-name segment)
                         :text replacement
                         :overflow 'truncate
                         :separator
                         (tessera-ui-segment-separator segment)
                         :alternatives alternatives))))
            (setq tail (cdr tail))))
        (tessera-ui-truncate-pixels (tessera-ui--entry-segments fitted)
                                    width)))))

(defun tessera-ui--entry-minimum-right (segments)
  "Return the minimum preferred display text for SEGMENTS.

Truncatable segments retain an ellipsis, preserved segments retain
their complete text, and hidden segments are omitted."
  (tessera-ui--entry-segments (delq nil
                                    (mapcar (lambda (segment)
                                              (pcase (tessera-ui-segment-overflow segment)
                                                ('hide nil)
                                                ('truncate
                                                 (let* ((text (or (tessera-ui-segment-text segment) ""))
                                                        (alternatives
                                                         (tessera-ui-segment-alternatives segment))
                                                        (minimum
                                                         (if alternatives
                                                             (car (last alternatives))
                                                           (and
                                                            (not (string-empty-p text))
                                                            (tessera-ui-truncate-pixels text
                                                                                        (string-pixel-width (propertize "…" 'face
                                                                                                                        (get-text-property 0 'face text))))))))
                                                   (tessera-ui-segment-create
                                                    :name (tessera-ui-segment-name segment)
                                                    :text minimum
                                                    :overflow 'truncate
                                                    :separator
                                                    (tessera-ui-segment-separator segment)
                                                    :alternatives alternatives)))
                                                (_ segment)))
                                            segments))))

(defun tessera-ui-entry-row-line (row width rail rail-element &optional rail-width)
  "Return semantic ROW constrained to WIDTH pixels.

RAIL describes the shared slot rail.  Name that region
RAIL-ELEMENT.  RAIL-WIDTH can reserve additional leading space."
  (let* ((slots (tessera-ui-entry-row-slots row))
         (left (tessera-ui-entry-row-left-segments row))
         (right (tessera-ui-entry-row-right-segments row))
         (rail-text
          (copy-sequence (or (tessera-ui-entry-row-rail-text row)
                             (tessera-ui--entry-slot-rail slots rail rail-element))))
         (rail-width
          (max (or rail-width 0)
               (string-pixel-width rail-text)))
         (rail-leading
          (tessera-ui--entry-pixel-space (- rail-width (string-pixel-width rail-text))))
         (rail-content
          (let ((text (concat rail-leading rail-text)))
            (add-text-properties 0 (length text) (list 'tessera-ui--element rail-element) text)
            text))
         (leading
          (concat (tessera-ui-entry-leading-safety-gap)
                  (tessera-ui-entry-padding 'entry.left-padding)
                  rail-content
                  (tessera-ui-entry-space 'entry.separator)))
         (trailing
          (concat (tessera-ui-entry-padding 'entry.right-padding)
                  (tessera-ui-entry-trailing-safety-gap)))
         (available
          (max 0
               (- width
                  (string-pixel-width leading)
                  (string-pixel-width trailing))))
         (left-full
          (tessera-ui--entry-segments left))
         (right-full
          (tessera-ui--entry-segments right))
         (right-minimum
          (tessera-ui--entry-minimum-right right))
         (side-gap
          (if (and (not (string-empty-p left-full))
                   (not (string-empty-p right-full)))
              (tessera-ui-entry-space 'entry.separator)
            ""))
         (content-width
          (max 0
               (- available
                  (string-pixel-width side-gap))))
         (right-width
          (if (<= (+ (string-pixel-width left-full)
                     (string-pixel-width right-full))
                  content-width)
              (string-pixel-width right-full)
            (min content-width
                 (max (string-pixel-width right-minimum)
                      (- content-width
                         (string-pixel-width left-full))))))
         (right-text
          (tessera-ui--entry-fit-right right right-width))
         (left-width
          (max 0
               (- content-width
                  (string-pixel-width right-text))))
         (left-text
          (tessera-ui--entry-fit-left left left-width))
         (flex-width
          (max 0
               (- left-width
                  (string-pixel-width left-text)))))
    (concat
     leading left-text
     (tessera-ui--entry-pixel-space flex-width)
     side-gap
     right-text trailing)))

(defun tessera-ui-flat-entry-lines (entry width rail)
  "Return the two display rows for ENTRY.

WIDTH is measured in pixels.  Both rows reserve RAIL, so their left
segments begin at the same horizontal position."
  (list (concat (tessera-ui-entry-top-padding)
                (tessera-ui-entry-row-line (tessera-ui-entry-row-create
                                            :slots (tessera-ui-flat-entry-main-slots entry)
                                            :left-segments
                                            (tessera-ui-flat-entry-main-left-segments entry)
                                            :right-segments
                                            (tessera-ui-flat-entry-main-right-segments entry))
                                           width rail 'entry.main-slot-rail))
        (concat (tessera-ui-entry-row-line
                 (tessera-ui-entry-row-create
                  :slots (tessera-ui-flat-entry-extra-slots entry)
                  :left-segments
                  (tessera-ui-flat-entry-extra-left-segments entry)
                  :right-segments
                  (tessera-ui-flat-entry-extra-right-segments entry))
                 width rail 'entry.extra-slot-rail)
                (tessera-ui-entry-bottom-padding))))

(defun tessera-ui-entry-window-width (window)
  "Return the usable entry pixel width in WINDOW."
  (max 0 (1- (window-body-width window t))))

(defun tessera-ui-flat-entry-window-lines (entry window rail)
  "Return display rows for ENTRY in WINDOW using RAIL."
  (tessera-ui-flat-entry-lines entry
                               (tessera-ui-entry-window-width window)
                               rail))

(defun tessera-ui-format-thread-statistic (statistic)
  "Return display text for thread STATISTIC."
  (let* ((unread
          (max 0
               (or (tessera-ui-thread-statistic-unread statistic)
                   0)))
         (visible
          (max 0
               (or (tessera-ui-thread-statistic-visible statistic)
                   0)))
         (known
          (tessera-ui-thread-statistic-known statistic)))
    (tessera-ui-format-thread-metric unread visible
                                     (and (numberp known) (> known visible)))))

(defun tessera-ui--thread-connector-symbols (style)
  "Return thread connector symbols for STYLE."
  (if (eq style 'ascii)
      '((vertical . "|  ")
        (indent . "   ")
        (root . "* ")
        (branch . "+- ")
        (last . "`- ")
        (orphan . "?- "))
    '((vertical . "│  ")
      (indent . "   ")
      (root . "* ")
      (branch . "├─ ")
      (last . "└─ ")
      (orphan . "◇─ "))))

(defun tessera-ui-format-thread-connector (connector style)
  "Return display text for CONNECTOR using symbol STYLE."
  (let* ((symbols (tessera-ui--thread-connector-symbols style))
         (ancestors
          (mapconcat (lambda (continued-p)
                       (alist-get (if continued-p 'vertical 'indent) symbols))
                     (tessera-ui-thread-connector-ancestors connector)
                     ""))
         (tail
          (or (alist-get (tessera-ui-thread-connector-kind connector)
                         symbols)
              (alist-get 'branch symbols)))
         (text (concat ancestors tail)))
    (add-face-text-property 0 (length text) 'tessera-ui-thread-connector t text)
    text))

(defun tessera-ui--thread-member-row (member style)
  "Return the semantic row for MEMBER using symbol STYLE."
  (let ((connector
         (tessera-ui-make-segment 'thread.member.connector
                                  (tessera-ui-format-thread-connector (tessera-ui-thread-member-connector member)
                                                                      style)
                                  'preserve)))
    (tessera-ui-entry-row-create
     :slots (tessera-ui-thread-member-slots member)
     :left-segments
     (cons connector
           (tessera-ui-thread-member-left-segments member))
     :right-segments
     (tessera-ui-thread-member-right-segments member))))

(defun tessera-ui--thread-padding (element top bottom)
  "Return thread padding ELEMENT with TOP and BOTTOM pixels."
  (tessera-ui--thread-heading-vertical-padding element top bottom
                                               (or (get-buffer-window (current-buffer) t)
                                                   (selected-window))))

(defun tessera-ui-thread-lines (thread width rail style)
  "Return display lines for THREAD constrained to WIDTH pixels.

RAIL is shared with flat entries.  STYLE controls connector symbols."
  (let* ((statistic
          (tessera-ui-format-thread-statistic (tessera-ui-thread-statistic thread)))
         (rail-width
          (max (tessera-ui-entry-rail-pixel-width rail)
               (string-pixel-width statistic)))
         (main-row
          (tessera-ui-entry-row-create
           :rail-text statistic
           :left-segments
           (tessera-ui-thread-main-left-segments thread)
           :right-segments
           (tessera-ui-thread-main-right-segments thread)))
         (main-line
          (concat (tessera-ui--thread-padding 'thread.top-padding tessera-ui--thread-heading-top-padding 0)
                  (tessera-ui-entry-row-line main-row width rail 'thread.statistic-rail
                                             rail-width)))
         (members (tessera-ui-thread-members thread))
         (member-lines
          (mapcar (lambda (member)
                    (tessera-ui-entry-row-line (tessera-ui--thread-member-row member style)
                                               width rail 'thread.member-slot-rail
                                               rail-width))
                  members))
         (lines (cons main-line member-lines))
         (last (car (last lines))))
    (setcar (last lines)
            (concat last
                    (tessera-ui--thread-padding 'thread.bottom-padding 0 tessera-ui--thread-heading-bottom-padding)))
    lines))

(defun tessera-ui-thread-window-lines (thread window rail style)
  "Return display lines for THREAD in WINDOW using RAIL and STYLE."
  (tessera-ui-thread-lines thread
                           (tessera-ui-entry-window-width window)
                           rail style))

(cl-defun tessera-ui-make-window-overlay (start end window &key display before-string after-string properties)
  "Return a presentation overlay from START to END in WINDOW.

DISPLAY, BEFORE-STRING, AFTER-STRING, and PROPERTIES configure its
presentation."
  (let ((overlay (make-overlay start end nil t nil)))
    (overlay-put overlay 'window window)
    (overlay-put overlay 'evaporate t)
    (when display
      (overlay-put overlay 'display display))
    (when before-string
      (overlay-put overlay 'before-string before-string))
    (when after-string
      (overlay-put overlay 'after-string after-string))
    (while properties
      (overlay-put overlay (pop properties) (pop properties)))
    overlay))

(cl-defun tessera-ui-make-virtual-row-overlay (start end window row &key main-line properties)
  "Present virtual ROW over START through END in WINDOW.

When MAIN-LINE is non-nil, display it on a separate line before ROW.
PROPERTIES are installed on the returned overlay.  The virtual rows
are an after string so their nested display properties remain
effective."
  (tessera-ui-make-window-overlay start end window
                                  :display ""
                                  :after-string
                                  (if main-line
                                      (concat main-line "\n" row)
                                    row)
                                  :properties properties))

(defun tessera-ui--present-flat-entry-line (start text window properties)
  "Present TEXT over the line at START in WINDOW."
  (save-excursion
    (goto-char start)
    (let ((overlay
           (tessera-ui-make-window-overlay start (line-end-position) window
                                           :display ""
                                           :before-string text
                                           :properties
                                           (append '(tessera-ui--flat-entry-presentation t)
                                                   properties))))
      overlay)))

(defun tessera-ui-present-flat-entry-lines (lines start window &optional properties)
  "Present two flat-entry LINES at START in WINDOW.

PROPERTIES is an overlay property list.  Return both overlays."
  (let (overlays)
    (save-excursion
      (goto-char start)
      (push (tessera-ui--present-flat-entry-line
             (point) (car lines) window properties)
            overlays)
      (forward-line 1)
      (push (tessera-ui--present-flat-entry-line
             (point) (cadr lines) window properties)
            overlays))
    (nreverse overlays)))

(defun tessera-ui-window-presentations-add (presentations overlay)
  "Add OVERLAY to PRESENTATIONS and return it."
  (push overlay
        (tessera-ui-window-presentations-overlays presentations))
  overlay)

(defun tessera-ui-window-presentations-delete (presentations &optional predicate)
  "Delete overlays in PRESENTATIONS satisfying PREDICATE.

When PREDICATE is nil, delete every overlay."
  (let (remaining)
    (dolist
        (overlay
         (tessera-ui-window-presentations-overlays presentations))
      (if (or (null predicate) (funcall predicate overlay))
          (delete-overlay overlay)
        (push overlay remaining)))
    (setf (tessera-ui-window-presentations-overlays presentations)
          (nreverse remaining))))

(defun tessera-ui-window-presentations-find (presentations window property value)
  "Find an overlay in PRESENTATIONS for WINDOW with PROPERTY VALUE."
  (seq-find (lambda (overlay)
              (and (overlay-buffer overlay)
                   (eq (overlay-get overlay 'window) window)
                   (equal (overlay-get overlay property) value)))
            (tessera-ui-window-presentations-overlays presentations)))

(defun tessera-ui-window-presentations-record-start (presentations window start)
  "Record START for WINDOW in PRESENTATIONS."
  (setf (alist-get window
                   (tessera-ui-window-presentations-window-starts presentations)
                   nil nil #'eq)
        start))

(defun tessera-ui-window-presentations-start-changed-p (presentations window start)
  "Record START in PRESENTATIONS and report changes for WINDOW."
  (let* ((starts
          (tessera-ui-window-presentations-window-starts presentations))
         (entry (assq window starts)))
    (unless (and entry (equal (cdr entry) start))
      (tessera-ui-window-presentations-record-start presentations window start)
      t)))

(defun tessera-ui-window-presentations-cancel (presentations)
  "Cancel the pending refresh in PRESENTATIONS."
  (when-let* ((timer
               (tessera-ui-window-presentations-timer presentations)))
    (cancel-timer timer))
  (setf (tessera-ui-window-presentations-timer presentations) nil))

(defun tessera-ui-window-presentations-update (presentations function)
  "Run FUNCTION once while PRESENTATIONS is marked as updating."
  (unless (tessera-ui-window-presentations-updating-p presentations)
    (setf (tessera-ui-window-presentations-updating-p presentations) t)
    (unwind-protect
        (funcall function)
      (setf (tessera-ui-window-presentations-updating-p presentations)
            nil))))

(defun tessera-ui--run-window-presentation-update (presentations buffer function)
  "Run FUNCTION in BUFFER for PRESENTATIONS."
  (setf (tessera-ui-window-presentations-timer presentations) nil)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (tessera-ui-window-presentations-update presentations function))))

(defun tessera-ui-window-presentations-schedule (presentations delay function buffer)
  "Schedule FUNCTION in BUFFER after idle DELAY.

PRESENTATIONS retains and replaces the pending timer."
  (unless (tessera-ui-window-presentations-updating-p presentations)
    (tessera-ui-window-presentations-cancel presentations)
    (setf (tessera-ui-window-presentations-timer presentations)
          (run-with-idle-timer delay nil
                               #'tessera-ui--run-window-presentation-update
                               presentations buffer function))))

(defun tessera-ui-format-query (prefix condition)
  "Return a header query made from PREFIX and CONDITION."
  (let ((prefix (copy-sequence prefix))
        (condition (copy-sequence condition)))
    (add-face-text-property 0 (length prefix) 'tessera-ui-header-query-prefix nil prefix)
    (add-text-properties 0 (length prefix) '(tessera-ui--element header.query.prefix) prefix)
    (add-face-text-property 0 (length condition) 'tessera-ui-header-query-condition nil condition)
    (add-text-properties 0 (length condition) '(tessera-ui--element header.query.condition) condition)
    (concat
     prefix
     (tessera-ui--header-space 'header.separator)
     condition)))

(defun tessera-ui--flex-gap (right inset element)
  "Return a pixel-aligned gap before RIGHT named ELEMENT.

INSET is the additional distance reserved at the window edge."
  (propertize " "
              'display
              `(space
                :align-to
                (- right
                   (+ (,(string-pixel-width right))
                      (,inset))))
              'tessera-ui--element element))

(defun tessera-ui-entry-flex-gap (right)
  "Return a pixel-aligned gap before RIGHT.

RIGHT is the complete text that follows the gap.  Reserve the trailing
entry safety gap at the window edge."
  (tessera-ui--flex-gap right tessera-ui--entry-safety-gap 'entry.flex-gap))

(defun tessera-ui--copy-segment (segment)
  "Return a presentation copy of SEGMENT."
  (tessera-ui-segment-create
   :name (tessera-ui-segment-name segment)
   :text (copy-sequence (or (tessera-ui-segment-text segment) ""))
   :overflow (tessera-ui-segment-overflow segment)
   :separator (tessera-ui-segment-separator segment)
   :alternatives
   (mapcar #'copy-sequence
           (tessera-ui-segment-alternatives segment))))

(defun tessera-ui--header-segments-text (segments &optional flexible-width parent)
  "Return display text for header SEGMENTS.

FLEXIBLE-WIDTH is the shared pixel budget for segments whose overflow
policy is `truncate'.  A nil value preserves their complete text.
PARENT names the containing side."
  (let ((remaining flexible-width)
        (first t)
        (text ""))
    (dolist (segment segments)
      (let* ((full
              (copy-sequence (or (tessera-ui-segment-text segment) "")))
             (value
              (if (and (numberp remaining)
                       (eq (tessera-ui-segment-overflow segment)
                           'truncate))
                  (let ((truncated
                         (tessera-ui-truncate-pixels full remaining)))
                    (setq remaining
                          (max 0
                               (- remaining
                                  (string-pixel-width truncated))))
                    truncated)
                full)))
        (when (and (eq (tessera-ui-segment-overflow segment)
                       'truncate)
                   (< (length value) (length full)))
          (add-text-properties 0 (length value) (list 'help-echo (substring-no-properties full)) value))
        (add-text-properties 0 (length value) (list 'tessera-ui--element (tessera-ui-segment-name segment)) value)
        (setq text
              (concat text
                      (if first
                          ""
                        (tessera-ui--header-space 'header.separator))
                      value)
              first nil)))
    (when parent
      (add-text-properties 0 (length text) (list 'tessera-ui--parent-element parent) text))
    text))

(defun tessera-ui--header-narrow-one (segments)
  "Use one narrower alternative among SEGMENTS."
  (catch 'narrowed
    (dolist (segment (reverse segments))
      (when-let* ((alternatives
                   (tessera-ui-segment-alternatives segment)))
        (setf (tessera-ui-segment-text segment) (car alternatives)
              (tessera-ui-segment-alternatives segment)
              (cdr alternatives))
        (throw 'narrowed t)))
    nil))

(defun tessera-ui-format-header-line (header &optional window)
  "Render standard semantic HEADER for WINDOW.

The layout is top padding, a safety gap, left padding, left segments,
a flexible gap, right segments, right padding, a safety gap, and
bottom padding.  Right alternatives narrow before left alternatives;
left `truncate' segments receive the remaining width."
  (let* ((window (or window (selected-window)))
         (left
          (mapcar #'tessera-ui--copy-segment
                  (tessera-ui-header-line-left-segments header)))
         (right
          (mapcar #'tessera-ui--copy-segment
                  (tessera-ui-header-line-right-segments header)))
         (top-padding
          (tessera-ui--header-vertical-padding 'header.top-padding window))
         (leading-safety-gap
          (tessera-ui--header-edge-safety-gap))
         (left-padding
          (tessera-ui--header-padding 'header.left-padding))
         (right-padding
          (tessera-ui--header-padding 'header.right-padding))
         (trailing-safety-gap
          (tessera-ui--header-edge-safety-gap))
         (bottom-padding
          (tessera-ui--header-vertical-padding 'header.bottom-padding window))
         (body-width (window-body-width window t))
         fixed-width)
    (cl-labels
        ((measure-fixed
           ()
           (string-pixel-width (concat leading-safety-gap
                                       left-padding
                                       (tessera-ui--header-segments-text left 0)
                                       (tessera-ui--header-segments-text right)
                                       right-padding
                                       trailing-safety-gap))))
      (setq fixed-width (measure-fixed))
      (while (and (> fixed-width body-width)
                  (tessera-ui--header-narrow-one right))
        (setq fixed-width (measure-fixed)))
      (while (and (> fixed-width body-width)
                  (tessera-ui--header-narrow-one left))
        (setq fixed-width (measure-fixed))))
    (let* ((flexible-width
            (max 0 (- body-width fixed-width)))
           (left-text
            (tessera-ui--header-segments-text left flexible-width 'header.left-segments))
           (right-text
            (tessera-ui--header-segments-text right nil 'header.right-segments))
           (parts
            (list top-padding
                  leading-safety-gap
                  left-padding
                  left-text
                  (and right 'mode-line-format-right-align)
                  right-text
                  right-padding
                  trailing-safety-gap
                  bottom-padding))
           (mode-line-format parts)
           (mode-line-right-align-edge 'right-fringe)
           (line
            (format-mode-line parts nil window (current-buffer))))
      (dotimes (position (length line))
        (let ((display (get-text-property position 'display line)))
          (when (and (consp display) (memq :align-to display))
            (put-text-property position (1+ position) 'tessera-ui--element 'header.flex-gap line))))
      line)))


(defface tessera-ui-glyph
  '((t :inherit tessera-ui-entry-feature))
  "Face used for neutral Tessera glyphs."
  :group 'tessera)

(defface tessera-ui-glyph-success
  '((t :inherit success))
  "Face used for successful Tessera glyph states."
  :group 'tessera)

(defface tessera-ui-glyph-attention
  '((t :inherit font-lock-keyword-face))
  "Face used for Tessera glyphs that need attention."
  :group 'tessera)

(defface tessera-ui-glyph-warning
  '((t :inherit warning))
  "Face used for Tessera glyphs that warrant caution."
  :group 'tessera)

(defface tessera-ui-glyph-error
  '((t :inherit error))
  "Face used for erroneous Tessera glyph states."
  :group 'tessera)

(defface tessera-ui-glyph-workflow
  '((t :inherit font-lock-constant-face))
  "Face used for Tessera workflow glyphs."
  :group 'tessera)

(defface tessera-ui-glyph-availability
  '((t :inherit font-lock-variable-name-face))
  "Face used for Tessera availability glyphs."
  :group 'tessera)

(defface tessera-ui-glyph-security
  '((t :inherit font-lock-builtin-face))
  "Face used for Tessera security glyphs."
  :group 'tessera)

(defvar tessera-ui--glyph-registry (make-hash-table :test #'equal)
  "Registered glyph specifications keyed by source, kind, and fact.")

(defun tessera-ui--glyph-validate-spec (spec)
  "Validate and return glyph SPEC."
  (let ((kind (tessera-ui-glyph-spec-kind spec))
        (slot (tessera-ui-glyph-spec-slot spec)))
    (unless
        (memq slot
              (pcase kind
                ('mark tessera-ui-glyph-mark-slots)
                ('feature tessera-ui-glyph-feature-slots)
                (_ (error "Unknown Tessera glyph kind: %S" kind))))
      (error "Invalid %S glyph slot: %S" kind slot))
    (unless (symbolp (tessera-ui-glyph-spec-source spec))
      (error "Glyph source must be a symbol"))
    (unless (symbolp (tessera-ui-glyph-spec-fact spec))
      (error "Glyph fact must be a symbol")))
  spec)

(defun tessera-ui-glyph-register-source (source definitions)
  "Replace SOURCE registrations with glyph DEFINITIONS.

Each definition is a property list accepted by
`tessera-ui-glyph-spec-create', except that SOURCE is supplied here."
  (let (keys)
    (maphash (lambda (key _spec)
               (when (eq (car key) source)
                 (push key keys)))
             tessera-ui--glyph-registry)
    (dolist (key keys)
      (remhash key tessera-ui--glyph-registry)))
  (dolist (definition definitions)
    (let* ((spec
            (apply #'tessera-ui-glyph-spec-create
                   :source source definition))
           (key
            (list source
                  (tessera-ui-glyph-spec-kind spec)
                  (tessera-ui-glyph-spec-fact spec))))
      (tessera-ui--glyph-validate-spec spec)
      (puthash key spec tessera-ui--glyph-registry)))
  source)

(defun tessera-ui-glyph-spec (source kind fact)
  "Return the glyph registered for SOURCE, KIND, and FACT."
  (gethash (list source kind fact) tessera-ui--glyph-registry))

(defun tessera-ui--glyph-select-slot (source kind slot facts)
  "Select a SOURCE glyph of KIND in SLOT from FACTS.

Choose the glyph with the highest priority."
  (let (selected)
    (dolist (fact facts)
      (when-let* ((spec (tessera-ui-glyph-spec source kind fact)))
        (when (and (eq slot (tessera-ui-glyph-spec-slot spec))
                   (or
                    (null selected)
                    (> (or (tessera-ui-glyph-spec-priority spec) 0)
                       (or
                        (tessera-ui-glyph-spec-priority selected)
                        0))))
          (setq selected spec))))
    selected))

(defun tessera-ui-glyph-select-marks (source facts)
  "Select semantic mark slots for SOURCE from FACTS.

The returned list follows the physical left-to-right rail order and
omits empty slots, so the selected glyphs remain adjacent."
  (delq nil
        (mapcar (lambda (slot)
                  (tessera-ui--glyph-select-slot source 'mark slot facts))
                tessera-ui-glyph-mark-display-slots)))

(defun tessera-ui-glyph-select-features (source facts)
  "Select feature slots for SOURCE from FACTS.

Return a `tessera-ui-glyph-selection'.  At most one primary and one
secondary feature are selected.  When registered facts remain, add
the registered overflow glyph and record the remainder as hidden."
  (let* ((known
          (seq-filter (lambda (fact)
                        (tessera-ui-glyph-spec source 'feature fact))
                      facts))
         (primary
          (tessera-ui--glyph-select-slot source 'feature 'primary known))
         (secondary
          (tessera-ui--glyph-select-slot source 'feature 'secondary known))
         (chosen
          (delq nil
                (list (and primary (tessera-ui-glyph-spec-fact primary))
                      (and secondary
                           (tessera-ui-glyph-spec-fact secondary)))))
         (hidden
          (seq-remove (lambda (fact) (memq fact chosen)) known))
         (overflow
          (and hidden
               (tessera-ui-glyph-spec source 'feature 'overflow))))
    (tessera-ui-glyph-selection-create
     :specs (delq nil (list primary secondary overflow))
     :hidden hidden)))

(defun tessera-ui--glyph-nerd-icon (spec fallback)
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

(defun tessera-ui--glyph-render-value (value fallback)
  "Render glyph VALUE, using FALLBACK when it is unavailable."
  (cond
   ((stringp value) (copy-sequence value))
   ((consp value) (tessera-ui--glyph-nerd-icon value fallback))
   (t (copy-sequence fallback))))

(defun tessera-ui--glyph-value (spec style)
  "Return the configured value from SPEC for STYLE."
  (pcase style
    ('ascii (tessera-ui-glyph-spec-ascii spec))
    ('unicode (tessera-ui-glyph-spec-unicode spec))
    ('nerd-icons (tessera-ui-glyph-spec-nerd-icon spec))
    (_ (error "Unknown Tessera glyph style: %S" style))))

(defun tessera-ui--glyph-color-face (spec color-style context-face)
  "Return the color face for SPEC under COLOR-STYLE.

CONTEXT-FACE is used when COLOR-STYLE is nil."
  (let ((face
         (cond
          ((stringp color-style)
           (list :foreground color-style))
          ((null color-style)
           (list :inherit (or context-face 'default)))
          (t
           (list :inherit
                 (or (tessera-ui-glyph-spec-face spec)
                     'tessera-ui-glyph))))))
    (append face '(:weight normal :slant normal))))

(defun tessera-ui-glyph-render (spec style color-style context-face &optional override fallback)
  "Render glyph SPEC in STYLE and COLOR-STYLE.

CONTEXT-FACE supplies the adjacent face when COLOR-STYLE is nil.
OVERRIDE, when non-nil, replaces the registered style value.
FALLBACK defaults to the registered Unicode symbol or fact name."
  (let* ((fallback
          (or fallback
              (tessera-ui-glyph-spec-unicode spec)
              (symbol-name (tessera-ui-glyph-spec-fact spec))))
         (value
          (or override (tessera-ui--glyph-value spec style)))
         (text (tessera-ui--glyph-render-value value fallback))
         (glyph-face
          (or (get-text-property 0 'face text)
              (get-text-property 0 'font-lock-face text)))
         (color-face
          (tessera-ui--glyph-color-face spec color-style context-face)))
    (remove-text-properties 0 (length text) '(face nil font-lock-face nil) text)
    (put-text-property 0 (length text) 'face (delq nil (list color-face glyph-face)) text)
    (add-text-properties 0 (length text)
                         (list 'help-echo (tessera-ui-glyph-spec-description spec)
                               'tessera-ui--element
                               (intern (format "entry.%s.%s"
                                               (tessera-ui-glyph-spec-kind spec)
                                               (tessera-ui-glyph-spec-slot spec)))
                               'tessera-ui--glyph-fact (tessera-ui-glyph-spec-fact spec)
                               'tessera-ui--glyph-kind (tessera-ui-glyph-spec-kind spec)
                               'tessera-ui--glyph-slot (tessera-ui-glyph-spec-slot spec))
                         text)
    text))

(defun tessera-ui-glyph-make-mark-slots (source facts renderer)
  "Return compact mark slots for SOURCE and FACTS.

RENDERER is called with each selected `tessera-ui-glyph-spec'."
  (delq nil
        (mapcar (lambda (spec)
                  (let ((text (funcall renderer spec)))
                    (unless (string-blank-p (or text ""))
                      (tessera-ui-slot-create
                       :name
                       (intern (format "entry.mark.%s"
                                       (tessera-ui-glyph-spec-slot spec)))
                       :text text))))
                (tessera-ui-glyph-select-marks source facts))))


(cl-defstruct
    (tessera-ui-header-activity (:constructor tessera-ui-header-activity-create)
                                (:copier nil))
  "An operation state displayed in a Tessera header.

STATE is one of `idle', `working', `warning', or `error'.  OPERATION
names the backend operation.  CURRENT and TOTAL describe progress,
and FAILED is an optional failure count.  ACTION is the primary mouse
command.  HELP-ECHO describes that action."
  state
  operation
  current
  total
  failed
  action
  help-echo)

(cl-defstruct
    (tessera-ui-header-scope (:constructor tessera-ui-header-scope-create)
                             (:copier nil))
  "The collection described by a Tessera header.

KIND is a semantic scope such as `all', `group', or `query'.  LABEL is
its display label and VALUE identifies the selected collection.  An
`all' scope remains in the model but is not displayed."
  kind
  label
  value)

(cl-defstruct
    (tessera-ui-header-metric (:constructor tessera-ui-header-metric-create)
                              (:copier nil))
  "One quantitative fact displayed in a Tessera header.

KIND identifies the fact.  VALUE is its numeric value and LABEL is its
display label.  EXACTNESS is one of `exact', `lower-bound',
`estimated', or `unknown'.  PRIORITY controls which metrics survive in
narrow windows; larger values are retained first."
  kind
  value
  label
  exactness
  priority)

(cl-defstruct
    (tessera-ui-header-line-installation (:constructor tessera-ui-header-line-installation-create)
                                         (:copier nil))
  "Ownership state for a Tessera header-line installation."
  source
  installed-format
  original-format
  original-local-p)

(cl-defstruct
    (tessera-ui-header-line-registration (:constructor tessera-ui-header-line-registration-create)
                                         (:copier nil))
  "Providers registered for the two sides of a header line."
  source
  left
  right)

(defconst tessera-ui-header-line-segment-kinds
  '(status query current-context-name statistics)
  "Standard semantic segment kinds for Tessera header lines.")

(defvar tessera-ui--header-line-registrations
  (make-hash-table :test #'eq)
  "Header-line registrations keyed by source.")

(defun tessera-ui--header-line-validate-providers (providers)
  "Validate and return segment PROVIDERS."
  (dolist (provider providers)
    (unless (and (consp provider)
                 (memq (car provider)
                       tessera-ui-header-line-segment-kinds)
                 (functionp (cdr provider)))
      (error "Invalid Tessera header provider: %S" provider)))
  providers)

(cl-defun tessera-ui-header-line-register (source &key left right)
  "Register LEFT and RIGHT segment providers for SOURCE.

Each provider is a cons of a standard segment kind and a function.
The function receives the derivation context and returns a
`tessera-ui-segment' or nil."
  (unless (symbolp source)
    (error "Header source must be a symbol"))
  (puthash source
           (tessera-ui-header-line-registration-create
            :source source
            :left (tessera-ui--header-line-validate-providers left)
            :right (tessera-ui--header-line-validate-providers right))
           tessera-ui--header-line-registrations)
  source)

(defun tessera-ui--header-line-derive-segments (source providers context)
  "Derive SOURCE segments from PROVIDERS and CONTEXT."
  (let (segments)
    (dolist (provider providers)
      (when-let* ((segment (funcall (cdr provider) context)))
        (unless (tessera-ui-segment-p segment)
          (error "Header provider for %S returned invalid segment: %S"
                 source segment))
        (let ((expected
               (intern (format "header.%s" (car provider)))))
          (unless (eq (tessera-ui-segment-name segment) expected)
            (error "Header provider for %S/%S returned %S"
                   source (car provider)
                   (tessera-ui-segment-name segment))))
        (push segment segments)))
    (nreverse segments)))

(defun tessera-ui-make-header-line (source context)
  "Return a registered header derived from SOURCE and CONTEXT."
  (let ((registration
         (gethash source tessera-ui--header-line-registrations)))
    (unless registration
      (error "No Tessera header registration for %S" source))
    (tessera-ui-header-line-create
     :source source
     :left-segments
     (tessera-ui--header-line-derive-segments source
                                              (tessera-ui-header-line-registration-left registration)
                                              context)
     :right-segments
     (tessera-ui--header-line-derive-segments source
                                              (tessera-ui-header-line-registration-right registration)
                                              context))))

(defconst tessera-ui--header-line-format
  '(:eval (tessera-ui--header-line-evaluate))
  "Header-line format installed by the shared presenter.")

(defvar-local tessera-ui--header-line-installation nil
  "Current Tessera header-line installation in this buffer.")

(defun tessera-ui-header-line-standard-metrics (unread middle middle-kind middle-label total &optional unread-exactness middle-exactness total-exactness)
  "Return standard metrics for UNREAD, MIDDLE, and TOTAL.

MIDDLE-KIND and MIDDLE-LABEL describe MIDDLE.  The optional exactness
arguments default to `exact'."
  (list (tessera-ui-header-metric-create
         :kind 'unread
         :value unread
         :label "unread"
         :exactness (or unread-exactness 'exact)
         :priority 100)
        (tessera-ui-header-metric-create
         :kind middle-kind
         :value middle
         :label middle-label
         :exactness (or middle-exactness 'exact)
         :priority 50)
        (tessera-ui-header-metric-create
         :kind 'total
         :value total
         :label "total"
         :exactness (or total-exactness 'exact)
         :priority 10)))

(defun tessera-ui--header-line-activity-operation-name (operation state compact-p)
  "Return the name of OPERATION in STATE.

Use its compact form when COMPACT-P is non-nil."
  (if (eq state 'idle)
      "IDLE"
    (pcase (cons operation state)
      (`(fetch . working) (if compact-p "FETCH" "FETCHING"))
      (`(fetch . warning) "FETCH WARNING")
      (`(fetch . error) (if compact-p "FAILED" "FETCH FAILED"))
      (`(update . working) (if compact-p "UPDATE" "UPDATING"))
      (`(update . warning) "UPDATE WARNING")
      (`(update . error) (if compact-p "FAILED" "UPDATE FAILED"))
      (`(,_ . working) (if compact-p "WORK" "WORKING"))
      (`(,_ . warning) "WARNING")
      (`(,_ . error) "FAILED")
      (_ "IDLE"))))

(defun tessera-ui--header-line-activity-face (state)
  "Return the header status face for STATE."
  (pcase state
    ('working 'tessera-ui-header-status-processing)
    ('warning 'tessera-ui-header-status-warning)
    ('error 'tessera-ui-header-status-fail)
    (_ 'tessera-ui-header-status-success)))

(defun tessera-ui--header-line-activity-text (activity compact-p)
  "Return the presentation of ACTIVITY.

Use a compact operation label when COMPACT-P is non-nil."
  (let* ((state (tessera-ui-header-activity-state activity))
         (current (tessera-ui-header-activity-current activity))
         (total (tessera-ui-header-activity-total activity))
         (failed (tessera-ui-header-activity-failed activity))
         (name
          (tessera-ui--header-line-activity-operation-name (tessera-ui-header-activity-operation activity)
                                                           state compact-p))
         (progress
          (cond
           ((and (numberp current) (numberp total))
            (format " %d/%d" current total))
           ((numberp current) (format " %d" current))
           (t "")))
         (failure
          (if (and (eq state 'error) (numberp failed))
              (format " %d" failed)
            ""))
         (text (concat name progress failure))
         (action (tessera-ui-header-activity-action activity))
         (map (and action (make-sparse-keymap))))
    (when map
      (define-key map [header-line mouse-1] action))
    (add-text-properties 0 (length text)
                         (list 'face (tessera-ui--header-line-activity-face state)
                               'tessera-ui--element 'header.activity
                               'help-echo (tessera-ui-header-activity-help-echo activity)
                               'keymap map
                               'mouse-face (and action 'header-line-highlight))
                         text)
    text))

(defun tessera-ui--header-line-activity-variants (activity)
  "Return widest-to-narrowest strings for ACTIVITY."
  (delete-dups (list (tessera-ui--header-line-activity-text activity nil)
                     (tessera-ui--header-line-activity-text activity t))))

(defun tessera-ui--header-line-scope-text (scope)
  "Return the presentation of SCOPE, or nil when it is implicit."
  (unless (or (null scope)
              (eq (tessera-ui-header-scope-kind scope) 'all))
    (let ((label (or (tessera-ui-header-scope-label scope)
                     (upcase (symbol-name (tessera-ui-header-scope-kind scope)))))
          (value (or (tessera-ui-header-scope-value scope) "")))
      (tessera-ui-format-query label value))))

(defun tessera-ui--header-line-metric-value (metric)
  "Return the formatted numeric value of METRIC."
  (let ((value (tessera-ui-header-metric-value metric)))
    (pcase (tessera-ui-header-metric-exactness metric)
      ('lower-bound (format "%s+" value))
      ('estimated (format "~%s" value))
      ('unknown "?")
      (_ (format "%s" value)))))

(defun tessera-ui--header-line-metric-element (metric suffix)
  "Return an element name for METRIC ending in SUFFIX."
  (intern (format "header.metrics.%s-%s"
                  (tessera-ui-header-metric-kind metric) suffix)))

(defun tessera-ui--header-line-metric-face (metric)
  "Return the value face for METRIC."
  (pcase (tessera-ui-header-metric-kind metric)
    ('unread
     (and (> (or (tessera-ui-header-metric-value metric) 0) 0)
          'tessera-ui-header-statistics-unread))
    ((or 'visible 'groups)
     'tessera-ui-header-statistics-success)
    (_ nil)))

(defun tessera-ui--header-line-metric-text (metric compact-p)
  "Return METRIC text, using a compact label when COMPACT-P."
  (let* ((value
          (propertize (tessera-ui--header-line-metric-value metric)
                      'tessera-ui--element
                      (tessera-ui--header-line-metric-element metric 'count)))
         (face (tessera-ui--header-line-metric-face metric))
         (label
          (if compact-p
              (substring (tessera-ui-header-metric-label metric) 0 1)
            (tessera-ui-header-metric-label metric)))
         (label
          (propertize label 'tessera-ui--element
                      (tessera-ui--header-line-metric-element metric 'text))))
    (when face
      (add-face-text-property 0 (length value) face nil value))
    (concat value (if compact-p "" " ") label)))

(defun tessera-ui--header-line-metrics-text (metrics compact-p)
  "Return METRICS text, using compact labels when COMPACT-P."
  (let ((text
         (mapconcat (lambda (metric)
                      (tessera-ui--header-line-metric-text metric compact-p))
                    metrics
                    (if compact-p " · " " · "))))
    (add-text-properties 0 (length text) '(tessera-ui--parent-element header.metrics) text)
    text))

(defun tessera-ui--header-line-drop-lowest-priority (metrics)
  "Return METRICS without one lowest-priority metric."
  (let* ((minimum
          (apply #'min
                 (mapcar (lambda (metric)
                           (or (tessera-ui-header-metric-priority metric) 0))
                         metrics)))
         (dropped-p nil))
    (seq-filter (lambda (metric)
                  (if (and (not dropped-p)
                           (= (or (tessera-ui-header-metric-priority metric) 0)
                              minimum))
                      (progn (setq dropped-p t) nil)
                    t))
                metrics)))

(defun tessera-ui--header-line-metric-variants (metrics)
  "Return widest-to-narrowest strings for METRICS."
  (let ((remaining metrics)
        (variants nil))
    (when remaining
      (push (tessera-ui--header-line-metrics-text remaining nil)
            variants)
      (push (tessera-ui--header-line-metrics-text remaining t)
            variants)
      (while (> (length remaining) 1)
        (setq remaining
              (tessera-ui--header-line-drop-lowest-priority remaining))
        (push (tessera-ui--header-line-metrics-text remaining t)
              variants)))
    (delete-dups (nreverse variants))))

(defun tessera-ui-header-line-status-segment (activity)
  "Return a standard status segment for ACTIVITY."
  (when activity
    (let ((variants
           (tessera-ui--header-line-activity-variants activity)))
      (tessera-ui-make-segment 'header.status
                               (car variants)
                               'preserve
                               'tessera-ui-header-status
                               nil
                               (cdr variants)))))

(defun tessera-ui-header-line-query-segment (scope)
  "Return a standard query segment for SCOPE."
  (when-let* ((text (tessera-ui--header-line-scope-text scope)))
    (tessera-ui-make-segment 'header.query text 'truncate 'tessera-ui-header-query)))

(defun tessera-ui-header-line-context-segment (name)
  "Return a current-context-name segment for NAME."
  (when (and (stringp name) (not (string-empty-p name)))
    (tessera-ui-make-segment 'header.current-context-name name 'preserve 'tessera-ui-header-context)))

(defun tessera-ui-header-line-statistics-segment (metrics)
  "Return a standard statistics segment for METRICS."
  (when-let* ((variants
               (tessera-ui--header-line-metric-variants metrics)))
    (tessera-ui-make-segment 'header.statistics
                             (car variants)
                             'preserve
                             'tessera-ui-header-statistics
                             nil
                             (cdr variants))))

(defun tessera-ui--header-line-evaluate ()
  "Derive and render the header for the current buffer."
  (when tessera-ui--header-line-installation
    (tessera-ui-format-header-line (tessera-ui-make-header-line
                                    (tessera-ui-header-line-installation-source tessera-ui--header-line-installation)
                                    (current-buffer))
                                   (selected-window))))

(defun tessera-ui-header-line-install (source)
  "Install or reassert a derived header for SOURCE.

Preserve the original value and locality of `header-line-format' on
the first installation in the current buffer."
  (unless tessera-ui--header-line-installation
    (setq tessera-ui--header-line-installation
          (tessera-ui-header-line-installation-create
           :source source
           :installed-format tessera-ui--header-line-format
           :original-format header-line-format
           :original-local-p (local-variable-p 'header-line-format))))
  (setf (tessera-ui-header-line-installation-source tessera-ui--header-line-installation)
        source)
  (setq-local header-line-format tessera-ui--header-line-format))

(defun tessera-ui-header-line-restore ()
  "Restore the header replaced by Tessera in the current buffer."
  (when tessera-ui--header-line-installation
    (let ((installation tessera-ui--header-line-installation))
      (when (eq header-line-format
                (tessera-ui-header-line-installation-installed-format installation))
        (if (tessera-ui-header-line-installation-original-local-p
             installation)
            (setq-local header-line-format
                        (tessera-ui-header-line-installation-original-format installation))
          (kill-local-variable 'header-line-format)))
      (setq tessera-ui--header-line-installation nil))))

(provide 'tessera-ui)
;;; tessera-ui.el ends here
