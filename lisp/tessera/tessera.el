;;; tessera.el --- Common foundation for Tessera  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bingshan Chang <chang@bingshan.org>

;; Author: Bingshan Chang <chang@bingshan.org>
;; Maintainer: Bingshan Chang <chang@bingshan.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (alert "1.2"))
;; Keywords: convenience, mail, news
;; URL: https://github.com/brsvh/emacs-tessera

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

;; Tessera is a modern interface suite for `elfeed', `gnus', and
;; `mu4e'.

;;; Code:

(require 'cl-lib)
(require 'gv)
(require 'seq)
(require 'subr-x)
(require 'wid-edit)

;;;; Customization

(defgroup tessera nil
  "Modern interfaces for Emacs communication tools."
  :group 'applications
  :prefix "tessera-")

(defun tessera--nonnegative-number-p (value)
  "Return non-nil when VALUE is a nonnegative number."
  (and (numberp value) (>= value 0)))

(defcustom tessera-entry-layout 'single-line
  "Layout used to render Tessera entries."
  :type 'symbol
  :safe #'symbolp
  :group 'tessera)

(defcustom tessera-entry-safe-gap 1
  "Width in columns outside each side of an entry surface."
  :type 'natnum
  :safe #'natnump
  :group 'tessera)

(defcustom tessera-entry-left-padding 1
  "Width in columns inside the left edge of an entry surface."
  :type 'natnum
  :safe #'natnump
  :group 'tessera)

(defcustom tessera-entry-right-padding 1
  "Width in columns inside the right edge of an entry surface."
  :type 'natnum
  :safe #'natnump
  :group 'tessera)

(defcustom tessera-entry-top-padding 0.2
  "Height above entry content in normal line heights."
  :type '(restricted-sexp
          :tag "Normal line heights"
          :match-alternatives (tessera--nonnegative-number-p))
  :safe #'tessera--nonnegative-number-p
  :group 'tessera)

(defcustom tessera-entry-bottom-padding 0.2
  "Height below entry content in normal line heights."
  :type '(restricted-sexp
          :tag "Normal line heights"
          :match-alternatives (tessera--nonnegative-number-p))
  :safe #'tessera--nonnegative-number-p
  :group 'tessera)

(defcustom tessera-thread-outer-top-padding 0.2
  "Height above the first thread member in normal line heights."
  :type '(restricted-sexp
          :tag "Normal line heights"
          :match-alternatives (tessera--nonnegative-number-p))
  :safe #'tessera--nonnegative-number-p
  :group 'tessera)

(defcustom tessera-thread-outer-bottom-padding 0.2
  "Height below the last visible member in normal line heights."
  :type '(restricted-sexp
          :tag "Normal line heights"
          :match-alternatives (tessera--nonnegative-number-p))
  :safe #'tessera--nonnegative-number-p
  :group 'tessera)

(defcustom tessera-thread-inner-top-padding 0.05
  "Height above non-first thread members in normal line heights."
  :type '(restricted-sexp
          :tag "Normal line heights"
          :match-alternatives (tessera--nonnegative-number-p))
  :safe #'tessera--nonnegative-number-p
  :group 'tessera)

(defcustom tessera-thread-inner-bottom-padding 0.05
  "Height below non-last visible members in normal line heights."
  :type '(restricted-sexp
          :tag "Normal line heights"
          :match-alternatives (tessera--nonnegative-number-p))
  :safe #'tessera--nonnegative-number-p
  :group 'tessera)

(defcustom tessera-entry-segment-gap 1
  "Width in columns between adjacent entry segments."
  :type 'natnum
  :safe #'natnump
  :group 'tessera)

(defcustom tessera-entry-flex-gap-min-width 1
  "Minimum width in columns between left and right content."
  :type 'natnum
  :safe #'natnump
  :group 'tessera)

(defcustom tessera-glyph-style 'unicode
  "Preferred visual style for Tessera glyphs."
  :type '(choice
          (const :tag "ASCII" ascii)
          (const :tag "Unicode" unicode)
          (const :tag "Nerd Icons" nerd-icons))
  :safe #'symbolp
  :initialize #'custom-initialize-default
  :set #'tessera--set-glyph-appearance
  :group 'tessera)

(defcustom tessera-glyph-color t
  "Color treatment applied to Tessera glyphs.

A nil value inherits the surrounding foreground.  A t value uses
the glyph's semantic face.  A color string applies that foreground
to every glyph."
  :type '(choice
          (const :tag "Monochrome" nil)
          (const :tag "Semantic colors" t)
          (color :tag "Uniform color"))
  :initialize #'custom-initialize-default
  :set #'tessera--set-glyph-appearance
  :group 'tessera)

(defface tessera-entry-current-face
  '((t :inherit hl-line :extend nil))
  "Face used for the entry containing point."
  :group 'tessera)

(defface tessera-entry-hover-face
  '((t :inherit highlight))
  "Face used when the pointer is over an entry surface."
  :group 'tessera)

(defface tessera-glyph-accent-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Theme emphasis for new, unread, and distinctive content."
  :group 'tessera)

(defface tessera-glyph-attention-face
  '((t :inherit warning :weight bold))
  "Theme warning color for important or pending actions."
  :group 'tessera)

(defface tessera-glyph-informational-face
  '((t :inherit font-lock-type-face))
  "Theme information color for attributes and correspondence."
  :group 'tessera)

(defface tessera-glyph-muted-face
  '((t :inherit shadow))
  "Face used for muted Tessera content."
  :group 'tessera)

(defface tessera-glyph-negative-face
  '((t :inherit error))
  "Theme error color for failures and destructive actions."
  :group 'tessera)

(defface tessera-glyph-neutral-face
  '((t :inherit default))
  "Face used for neutral Tessera content."
  :group 'tessera)

(defface tessera-glyph-positive-face
  '((t :inherit success))
  "Theme success color for completed actions and availability."
  :group 'tessera)

(defface tessera-glyph-warning-face
  '((t :inherit warning))
  "Theme warning color for urgent or uncertain states."
  :group 'tessera)

(defun tessera--save-settings (variables)
  "Snapshot current values and buffer locality of VARIABLES."
  (mapcar (lambda (variable)
            (list variable (local-variable-p variable)
                  (symbol-value variable)))
          variables))

(defun tessera--restore-settings (settings)
  "Restore a SETTINGS snapshot in the current buffer."
  (dolist (setting settings)
    (pcase-let ((`(,variable ,local ,value) setting))
      (if local
          (set (make-local-variable variable) value)
        (kill-local-variable variable)))))

;;;; Data model

(cl-defstruct tessera-entry-context
  "Describe one backend entry and its display environment.

BACKEND identifies the registered adapter.  OBJECT is the backend's
native object.  BUFFER and WINDOW identify where the entry is being
rendered.  METADATA belongs to the adapter and remains opaque to the
Tessera core.  THREAD is an optional `tessera-thread-context'
provided only when the native backend enables threading."
  backend
  object
  buffer
  window
  metadata
  thread)

(cl-defstruct (tessera-thread-context
               (:constructor make-tessera-thread-context
                             (&key id parent root path
                                   first last total unread
                                   &aux (forward-path path)
                                   (reverse-path (reverse path)))))
  "Describe one displayed member of a native thread.
ID, PARENT, and ROOT are opaque backend identifiers.  PATH lists
ancestor branches from the root, excluding the root itself; each
boolean says whether that branch has a following sibling.
FIRST identifies the displayed representative, LAST the last visible
member.  TOTAL and UNREAD include folded members of the result set."
  id parent root forward-path reverse-path first last total unread)

(defun tessera-thread-context-path (context)
  "Return CONTEXT's branch path in root-to-child order.
Built contexts share reversed paths.  Materialize the public list
only when requested, rather than copying every ancestor per row."
  (or (tessera-thread-context-forward-path context)
      (setf (tessera-thread-context-forward-path context)
            (reverse (tessera-thread-context-reverse-path context)))))

(gv-define-setter tessera-thread-context-path (value context)
  `(let ((node ,context)
         (path ,value))
     (setf (tessera-thread-context-forward-path node) path
           (tessera-thread-context-reverse-path node) (reverse path))
     path))

(defun tessera--thread-path-tail (context)
  "Return CONTEXT's branch path in child-to-root order."
  (if-let* ((path (tessera-thread-context-forward-path context)))
      (reverse path)
    (tessera-thread-context-reverse-path context)))

(defun tessera-thread-context-key (context)
  "Return the fields affecting CONTEXT's displayed row, or nil.
Counts affect the head only.  All ancestor branches affect the
displayed tree, including those outside the current window."
  (when context
    (let ((first (tessera-thread-context-first context)))
      (list first (tessera-thread-context-last context)
            (and first (tessera-thread-context-total context))
            (and first (tessera-thread-context-unread context))
            (tessera--thread-path-tail context)))))

(cl-defstruct tessera-thread-layout
  "Compose ordinary entry layouts for thread HEAD and CHILD members."
  head child)

(cl-defstruct tessera-glyph
  "Describe a glyph's forms, FACE, and optional HIDDEN state."
  ascii unicode nerd-icons face hidden)

(cl-defstruct tessera-glyph-slot
  "Describe a fixed-width, single-choice glyph channel.

NAME identifies the slot within its backend.  SELECTOR is called with
an entry context and returns a variant ID or nil.  WIDTH is measured
in columns.  ALIGN is one of `left', `center', or `right'.  GLYPHS is
an alist of variant specifications.  On graphical frames, oversized
glyphs are scaled down to fit WIDTH without changing the slot."
  name
  selector
  width
  align
  glyphs)

(cl-defstruct tessera-entry-layout
  "Describe the placement of slots and segments in an entry.
Non-nil extra fields add a second visual line to the logical entry.
Segment lists also accept `(:slots SLOT...)' for an inline group
of fixed-width glyph slots.
The segment option `:point t' marks its first displayed character as
the preferred navigation position, found by `tessera-entry-point'.
Glyph slot lists accept slot names and `(NAME :reserve t)'
references.  A reserved reference occupies the selected glyph's
display width without showing it.  `(NAME :optional t)' omits the
slot when its selector returns nil, including its width.
GLYPH-SLOTS-ALIGN, when `left' or `right', packs visible prefix icons
within the full area of all referenced slots.  Nil preserves each
slot position.  Inline slot groups are unaffected.
MAIN-LEADING-SEGMENTS and EXTRA-LEADING-SEGMENTS replace glyph slots
on their respective lines with a right-aligned segment group.
LEADING-WIDTH is a minimum column width, or a context function
returning one, shared by both lines."
  main-glyph-slots
  main-left-segments
  main-right-segments
  extra-glyph-slots
  extra-left-segments
  extra-right-segments
  glyph-slots-align
  main-leading-segments extra-leading-segments leading-width)

;;;; Backend registration

(cl-defstruct (tessera--entry-backend
               (:constructor tessera--make-entry-backend))
  "Store a validated backend entry definition."
  name
  context
  segments
  glyph-slots
  layouts
  thread-layout)

(defvar tessera--entry-backends (make-hash-table :test #'eq)
  "Registered Tessera entry backends.")

(defvar tessera--segment-properties
  '(:grow :min-width :max-width :truncate :priority :optional :point)
  "Properties accepted in a layout segment reference.")

(defvar tessera--glyph-slot-properties
  '(:reserve :optional)
  "Properties accepted in a layout glyph slot reference.")

(defvar tessera--glyph-variant-properties
  '(:glyph :mouse-face :help-echo :keymap :pointer :follow-link)
  "Properties accepted in a glyph variant specification.")

(defun tessera--ensure-list (value description)
  "Ensure VALUE is a proper list described by DESCRIPTION."
  (unless (proper-list-p value)
    (error "%s must be a proper list" description)))

(defun tessera--ensure-unique (values description)
  "Ensure VALUES contains unique symbols described by DESCRIPTION."
  (unless (= (length values)
             (length
              (cl-delete-duplicates (copy-sequence values)
                                    :test #'eq)))
    (error "%s must not contain duplicate IDs" description)))

(defun tessera--ensure-plist-keys (plist allowed description)
  "Ensure PLIST uses ALLOWED keys for DESCRIPTION."
  (unless (and (proper-list-p plist) (plistp plist)
               (cl-loop for (key _)
                        on plist
                        by #'cddr
                        always (memq key allowed)))
    (error "%s contains invalid properties" description))
  (tessera--ensure-unique
   (cl-loop for (key _) on plist by #'cddr collect key) description))

(defun tessera--validate-glyph (glyph description)
  "Validate GLYPH described by DESCRIPTION."
  (unless (tessera-glyph-p glyph)
    (error "%s must contain a Tessera glyph" description))
  (let ((ascii (tessera-glyph-ascii glyph))
        (unicode (tessera-glyph-unicode glyph))
        (icons (tessera-glyph-nerd-icons glyph))
        (face (tessera-glyph-face glyph)))
    (unless (and (tessera--glyph-string-p ascii)
                 (cl-every (lambda (char) (< char 128)) ascii))
      (error "%s has an invalid :ascii representation" description))
    (unless (or (null unicode) (tessera--glyph-string-p unicode))
      (error "%s has an invalid :unicode representation" description))
    (when icons
      (tessera--ensure-plist-keys
       icons '(:function :name) description)
      (unless (and (plist-get icons :function)
                   (symbolp (plist-get icons :function))
                   (stringp (plist-get icons :name))
                   (not (string-empty-p (plist-get icons :name))))
        (error "%s has an invalid :nerd-icons descriptor"
               description)))
    (unless (or (null face) (facep face))
      (error "%s has an invalid :face: %S" description face))
    (unless (memq (tessera-glyph-hidden glyph) '(nil t))
      (error "%s has an invalid :hidden value" description))))

;;;; Glyph configuration

(defvar tessera--glyph-change-functions nil
  "Functions called with a changed glyph option, or nil for all.
Modes install these callbacks only while enabled.")

(defun tessera--glyph-string-p (value)
  "Return non-nil for nonempty, single-line display text VALUE."
  (and (stringp value) (> (string-width value) 0)
       (not (string-match-p "[[:cntrl:]]" value))))

(define-widget 'tessera-glyph-spec 'plist
  "Overrides for the named fields of a Tessera glyph."
  :options
  '((:ascii (choice :tag "ASCII"
                    (const :tag "Native mark" nil) string))
    (:unicode (choice :tag "Unicode"
                      (const :tag "Use ASCII" nil) string))
    (:nerd-icons
     (choice :tag "Nerd Icons"
             (const :tag "Use Unicode or ASCII" nil)
             (plist :options
                    ((:function (symbol :tag "Function"))
                     (:name (string :tag "Icon name"))))))
    (:face (choice :tag "Face"
                   (const :tag "Inherit surrounding text" nil) face))
    (:hidden boolean)))

(defun tessera--glyph-custom-type (defaults)
  "Return a Customize type for overrides of DEFAULTS."
  `(alist :key-type
          (choice ,@(mapcar (lambda (entry) `(const ,(car entry)))
                            defaults))
          :value-type tessera-glyph-spec))

(defun tessera--glyph-definition (id defaults overrides)
  "Merge the named fields of glyph ID in DEFAULTS and OVERRIDES."
  (let ((base (assq id defaults)))
    (unless base (error "Unknown glyph: %S" id))
    (tessera--ensure-plist-keys
     (cdr base) '(:ascii :unicode :nerd-icons :face :hidden)
     (format "Default glyph `%s'" id))
    (let ((definition (copy-tree (cdr base)))
          (override (cdr (assq id overrides))))
      (tessera--ensure-plist-keys
       override '(:ascii :unicode :nerd-icons :face :hidden)
       (format "Glyph `%s'" id))
      (cl-loop for (key value) on override by #'cddr
               do
               (when (and (eq key :nerd-icons) value)
                 (tessera--ensure-plist-keys
                  value '(:function :name) (format "Glyph `%s'" id))
                 (let ((icons (copy-tree
                               (plist-get definition :nerd-icons))))
                   (cl-loop for (field setting) on value by #'cddr
                            do (setq icons
                                     (plist-put icons field setting)))
                   (setq value icons)))
               (setq definition (plist-put definition key value)))
      definition)))

(defun tessera-glyph-resolve (id defaults overrides &optional ascii)
  "Return glyph ID from DEFAULTS merged with OVERRIDES.
Both tables are alists mapping symbols to glyph property lists.
Missing fields inherit defaults; explicit nil values are retained.
Nerd Icons descriptors merge their named fields as well.  ASCII
supplies the native fallback when the default :ascii value is nil.
Signal an error for invalid representations, faces, or properties."
  (let* ((definition
          (tessera--glyph-definition id defaults overrides))
         (text (plist-get definition :ascii))
         (glyph
          (make-tessera-glyph
           :ascii (if (and (null text)
                           (null (plist-get (cdr (assq id defaults))
                                            :ascii)))
                      ascii text)
           :unicode (plist-get definition :unicode)
           :nerd-icons (plist-get definition :nerd-icons)
           :face (plist-get definition :face)
           :hidden (plist-get definition :hidden))))
    (tessera--validate-glyph glyph (format "Glyph `%s'" id))
    glyph))

(defun tessera--validate-glyph-overrides (defaults value width)
  "Validate glyph overrides VALUE against DEFAULTS and WIDTH.
WIDTH is a maximum column count or an alist of counts by glyph ID."
  (tessera--ensure-list value "Glyph overrides")
  (dolist (entry value)
    (unless (and (consp entry) (assq (car entry) defaults))
      (error "Unknown glyph override: %S" entry)))
  (tessera--ensure-unique (mapcar #'car value) "Glyph overrides")
  (dolist (entry defaults)
    (let* ((id (car entry))
           (glyph (tessera-glyph-resolve id defaults value "?"))
           (limit (if (numberp width) width (alist-get id width))))
      (dolist (text (list (tessera-glyph-ascii glyph)
                          (tessera-glyph-unicode glyph)))
        (when (and text limit (> (string-width text) limit))
          (error "Glyph `%s' exceeds its %d-column width"
                 id limit))))))

(defun tessera--set-glyphs (symbol value defaults &optional width)
  "Set glyph option SYMBOL to VALUE using DEFAULTS and WIDTH.
Validate before changing the option or notifying active adapters."
  (tessera--validate-glyph-overrides defaults value (or width 2))
  (set-default symbol value)
  (run-hook-with-args 'tessera--glyph-change-functions symbol))

(defun tessera--validate-glyph-appearance (symbol value)
  "Validate appearance option SYMBOL with proposed VALUE."
  (unless
      (pcase symbol
        ('tessera-glyph-style
         (memq value '(ascii unicode nerd-icons)))
        ('tessera-glyph-color
         (or (memq value '(nil t))
             (and (stringp value) (color-defined-p value))))
        ('tessera-entry-ellipsis (tessera--glyph-string-p value)))
    (error "Invalid %s value: %S" symbol value)))

(defun tessera--set-glyph-appearance (symbol value)
  "Validate appearance option SYMBOL, set VALUE, and redraw glyphs."
  (tessera--validate-glyph-appearance symbol value)
  (set-default symbol value)
  (run-hook-with-args 'tessera--glyph-change-functions symbol))

(defvar tessera--thread-glyph-defaults
  '((branch :ascii "+-" :unicode "├─" :face tessera-glyph-muted-face)
    (last :ascii "`-" :unicode "└─" :face tessera-glyph-muted-face)
    (vertical :ascii "|" :unicode "│" :face tessera-glyph-muted-face))
  "Default thread connectors, with fixed two- and one-column widths.")

(defun tessera--validate-thread-glyphs (value)
  "Validate thread connector overrides VALUE and their geometry."
  (tessera--validate-glyph-overrides
   tessera--thread-glyph-defaults value
   '((branch . 2) (last . 2) (vertical . 1)))
  (dolist (id '(branch last vertical))
    (let* ((glyph (tessera-glyph-resolve
                   id tessera--thread-glyph-defaults value))
           (width (if (eq id 'vertical) 1 2)))
      (when (or (tessera-glyph-hidden glyph)
                (tessera-glyph-nerd-icons glyph))
        (error "Thread glyph `%s' must be visible text" id))
      (dolist (text (list (tessera-glyph-ascii glyph)
                          (tessera-glyph-unicode glyph)))
        (when (and text (/= (string-width text) width))
          (error "Thread glyph `%s' must occupy %d columns"
                 id width))))))

(defun tessera--set-thread-glyphs (symbol value)
  "Set thread glyph option SYMBOL to validated VALUE."
  (tessera--validate-thread-glyphs value)
  (tessera--set-glyphs symbol value tessera--thread-glyph-defaults))

(defcustom tessera-thread-glyphs nil
  "Overrides for thread connectors, as an alist of glyph plists.
IDs are `branch', `last', and `vertical'.  Omitted fields keep their
defaults.  Branch and last occupy two columns; vertical occupies
one.  Connectors cannot be hidden or use Nerd Icons.  See
`tessera-glyph-resolve' for the common fields."
  :type (tessera--glyph-custom-type tessera--thread-glyph-defaults)
  :initialize #'custom-initialize-default
  :set #'tessera--set-thread-glyphs
  :group 'tessera)

(defcustom tessera-entry-ellipsis "…"
  "Text indicating truncated entry content.
Use nonempty single-line text.  Narrow areas may clip this marker."
  :type 'string
  :initialize #'custom-initialize-default
  :set #'tessera--set-glyph-appearance
  :group 'tessera)

;;;###autoload
(defun tessera-refresh-glyphs ()
  "Validate glyph settings and redraw all active Tessera views.
Use after `setq' or face customization.  Customize and `setopt'
refresh affected views automatically.  Disabled adapters stay off."
  (interactive)
  (tessera--validate-thread-glyphs tessera-thread-glyphs)
  (dolist (option '(tessera-glyph-style tessera-glyph-color
                                        tessera-entry-ellipsis))
    (tessera--validate-glyph-appearance option (symbol-value option)))
  (run-hook-with-args 'tessera--glyph-change-functions nil))

(defun tessera--validate-glyph-variant (variant slot-name)
  "Validate VARIANT belonging to SLOT-NAME and return its glyph."
  (unless (and (consp variant)
               (car variant)
               (symbolp (car variant)))
    (error "Glyph slot `%s' has an invalid variant" slot-name))
  (let ((description
         (format "Glyph variant `%s' in slot `%s'"
                 (car variant) slot-name))
        (properties (cdr variant)))
    (tessera--ensure-plist-keys properties
                                tessera--glyph-variant-properties
                                description)
    (unless (plist-member properties :glyph)
      (error "%s has no :glyph property" description))
    (let ((glyph (plist-get properties :glyph)))
      (tessera--validate-glyph glyph description)
      glyph)))

(defun tessera--validate-glyph-slot (slot)
  "Validate glyph SLOT."
  (unless (tessera-glyph-slot-p slot)
    (error "Glyph slots must be Tessera glyph slots"))
  (let ((name (tessera-glyph-slot-name slot))
        (selector (tessera-glyph-slot-selector slot))
        (width (tessera-glyph-slot-width slot))
        (align (tessera-glyph-slot-align slot))
        (glyphs (tessera-glyph-slot-glyphs slot)))
    (unless (and name (symbolp name))
      (error "Glyph slot names must be non-nil symbols"))
    (unless (functionp selector)
      (error "Glyph slot `%s' has an invalid selector" name))
    (unless (and (integerp width) (> width 0))
      (error "Glyph slot `%s' has an invalid width" name))
    (unless (memq align '(left center right))
      (error "Glyph slot `%s' has an invalid alignment" name))
    (tessera--ensure-list glyphs (format "Glyphs in slot `%s'" name))
    (tessera--ensure-unique (mapcar #'car glyphs)
                            (format "Glyphs in slot `%s'" name))
    (dolist (variant glyphs)
      (let* ((glyph (tessera--validate-glyph-variant variant name))
             (ascii (tessera-glyph-ascii glyph)))
        (when (> (string-width ascii) width)
          (error "Glyph variant `%s' exceeds slot `%s' width"
                 (car variant) name))))))

(defun tessera--reference-name (reference kind)
  "Return the name in REFERENCE, or report an invalid KIND reference."
  (cond
   ((and reference (symbolp reference))
    reference)
   ((and (consp reference)
         (symbolp (car reference))
         (car reference))
    (car reference))
   (t
    (error "Invalid %s reference `%S'" kind reference))))

(defun tessera--validate-segment-reference (reference names)
  "Validate segment REFERENCE against registered NAMES."
  (let ((name (tessera--reference-name reference "segment")))
    (unless (memq name names)
      (error "Layout references unknown segment `%s'" name))
    (when (consp reference)
      (tessera--ensure-plist-keys
       (cdr reference)
       tessera--segment-properties
       (format "Segment reference `%s'" name))
      (let* ((properties (cdr reference))
             (grow (plist-get properties :grow))
             (minimum (plist-get properties :min-width))
             (maximum (plist-get properties :max-width))
             (truncate (plist-get properties :truncate))
             (priority (plist-get properties :priority))
             (optional (plist-get properties :optional))
             (point (plist-get properties :point)))
        (unless (memq grow '(nil t))
          (error "Segment reference `%s' has invalid :grow" name))
        (unless (or (null minimum) (natnump minimum))
          (error "Segment reference `%s' has invalid :min-width"
                 name))
        (unless (or (null maximum) (natnump maximum))
          (error "Segment reference `%s' has invalid :max-width"
                 name))
        (when (and minimum maximum (> minimum maximum))
          (error
           "Segment reference `%s' has :min-width above :max-width"
           name))
        (unless (memq truncate '(nil head middle tail))
          (error "Segment reference `%s' has invalid :truncate" name))
        (unless (or (null priority) (integerp priority))
          (error "Segment reference `%s' has invalid :priority" name))
        (unless (memq optional '(nil t))
          (error "Segment reference `%s' has invalid :optional"
                 name))
        (unless (memq point '(nil t))
          (error "Segment reference `%s' has invalid :point"
                 name))))))

(defun tessera--glyph-slot-reference-reserved-p (reference)
  "Return non-nil when glyph slot REFERENCE reserves its space."
  (and (consp reference)
       (plist-get (cdr reference) :reserve)))

(defun tessera--validate-glyph-slot-reference
    (reference names description)
  "Validate glyph slot REFERENCE against NAMES for DESCRIPTION."
  (let ((name (tessera--reference-name reference "glyph slot")))
    (unless (memq name names)
      (error "%s references unknown glyph slot `%s'"
             description name))
    (when (consp reference)
      (tessera--ensure-plist-keys
       (cdr reference)
       tessera--glyph-slot-properties
       (format "Glyph slot reference `%s'" name))
      (dolist (property tessera--glyph-slot-properties)
        (unless (memq (plist-get (cdr reference) property) '(nil t))
          (error "Glyph slot reference `%s' has invalid %s"
                 name property))))))

(defun tessera--validate-layout
    (layout segment-names slot-names description)
  "Validate LAYOUT for SEGMENT-NAMES and SLOT-NAMES.
DESCRIPTION identifies the layout in errors."
  (unless (tessera-entry-layout-p layout)
    (error "%s must contain a Tessera entry layout" description))
  (unless (memq (tessera-entry-layout-glyph-slots-align layout)
                '(nil left right))
    (error "%s has invalid glyph slot alignment" description))
  (let ((width (tessera-entry-layout-leading-width layout)))
    (unless (or (null width) (natnump width) (functionp width))
      (error "%s has invalid leading width" description)))
  (when (or (and (tessera-entry-layout-main-glyph-slots layout)
                 (tessera-entry-layout-main-leading-segments layout))
            (and (tessera-entry-layout-extra-glyph-slots layout)
                 (tessera-entry-layout-extra-leading-segments
                  layout)))
    (error "%s mixes leading segments and glyph slots" description))
  (let ((slot-lists
         (list (tessera-entry-layout-main-glyph-slots layout)
               (tessera-entry-layout-extra-glyph-slots layout)))
        (segment-lists
         (list (tessera-entry-layout-main-left-segments layout)
               (tessera-entry-layout-main-right-segments layout)
               (tessera-entry-layout-extra-left-segments layout)
               (tessera-entry-layout-extra-right-segments layout)
               (tessera-entry-layout-main-leading-segments layout)
               (tessera-entry-layout-extra-leading-segments layout))))
    (dolist (slots slot-lists)
      (tessera--ensure-list slots description)
      (dolist (slot slots)
        (tessera--validate-glyph-slot-reference
         slot slot-names description)))
    (dolist (segments segment-lists)
      (tessera--ensure-list segments description)
      (dolist (segment segments)
        (if (eq (car-safe segment) :slots)
            (progn
              (tessera--ensure-list segment description)
              (unless (cdr segment)
                (error "%s contains an empty slot group" description))
              (dolist (slot (cdr segment))
                (tessera--validate-glyph-slot-reference
                 slot slot-names description)))
          (tessera--validate-segment-reference
           segment segment-names))))))

(defun tessera--validate-segments (segments)
  "Validate the SEGMENTS provider alist."
  (tessera--ensure-list segments "Segments")
  (dolist (segment segments)
    (unless (and (consp segment)
                 (car segment)
                 (symbolp (car segment))
                 (functionp (cdr segment)))
      (error "Invalid segment provider `%S'" segment)))
  (tessera--ensure-unique (mapcar #'car segments) "Segments"))

(defun tessera--validate-layouts
    (layouts segment-names slot-names)
  "Validate LAYOUTS against SEGMENT-NAMES and SLOT-NAMES."
  (tessera--ensure-list layouts "Layouts")
  (unless layouts
    (error "A backend must register at least one layout"))
  (dolist (entry layouts)
    (unless (and (consp entry)
                 (car entry)
                 (symbolp (car entry)))
      (error "Invalid layout entry `%S'" entry))
    (tessera--validate-layout
     (cdr entry)
     segment-names
     slot-names
     (format "Layout `%s'" (car entry))))
  (tessera--ensure-unique (mapcar #'car layouts) "Layouts"))

(cl-defun tessera-entry-register
    (backend &key context segments glyph-slots layouts thread-layout)
  "Register or replace entry BACKEND.

CONTEXT is a function of an object, buffer, and window which returns
a `tessera-entry-context'.  SEGMENTS is an alist mapping segment IDs
to provider functions.  GLYPH-SLOTS is a list of
`tessera-glyph-slot' objects.  LAYOUTS is an alist mapping layout IDs
to `tessera-entry-layout' objects.  Optional THREAD-LAYOUT is a
`tessera-thread-layout' used when CONTEXT supplies thread membership.

The new definition is installed only after it has been validated.
Return BACKEND."
  (unless (and backend (symbolp backend))
    (error "Backend must be a non-nil symbol"))
  (unless (functionp context)
    (error "Backend `%s' has an invalid context function" backend))
  (tessera--validate-segments segments)
  (tessera--ensure-list glyph-slots "Glyph slots")
  (dolist (slot glyph-slots)
    (tessera--validate-glyph-slot slot))
  (let ((segment-names (mapcar #'car segments))
        (slot-names (mapcar #'tessera-glyph-slot-name glyph-slots)))
    (tessera--ensure-unique slot-names "Glyph slots")
    (tessera--validate-layouts layouts segment-names slot-names)
    (when thread-layout
      (unless (tessera-thread-layout-p thread-layout)
        (error "Invalid thread layout for `%s'" backend))
      (dolist (layout (list (tessera-thread-layout-head thread-layout)
                            (tessera-thread-layout-child
                             thread-layout)))
        (tessera--validate-layout
         layout segment-names slot-names "Thread layout")))
    (let ((definition
           (tessera--make-entry-backend
            :name backend
            :context context
            :segments segments
            :glyph-slots glyph-slots
            :layouts layouts
            :thread-layout thread-layout)))
      (puthash backend definition tessera--entry-backends)))
  backend)

(defun tessera-thread-build-contexts (entries)
  "Build shared thread contexts from ordered native ENTRIES.
Each entry is (ID PARENT UNREAD VISIBLE).  IDs are compared with
`equal'; PARENT is nil for a displayed root, or names an earlier
entry.  Backends determine parentage and visibility, including any
visible representative of folded messages.  Return a hash table
from IDs to contexts with counts, branch paths, and boundaries."
  (let ((contexts (make-hash-table :test #'equal))
        (last-child (make-hash-table :test #'equal))
        (totals (make-hash-table :test #'equal))
        (unreads (make-hash-table :test #'equal))
        (last-visible (make-hash-table :test #'equal))
        nodes)
    (dolist (entry entries)
      (pcase-let* ((`(,id ,parent-id ,unread ,visible) entry)
                   (parent (gethash parent-id contexts))
                   (root (if parent
                             (tessera-thread-context-root parent)
                           id)))
        (when (and parent-id (null parent))
          (error "Thread parent %S must precede child %S"
                 parent-id id))
        (let ((node (make-tessera-thread-context
                     :id id
                     :parent parent-id
                     :root root
                     :first (null parent))))
          (puthash id node contexts)
          (push node nodes))
        (puthash root (1+ (gethash root totals 0)) totals)
        (when unread
          (puthash root (1+ (gethash root unreads 0)) unreads))
        (when parent (puthash parent-id id last-child))
        (when visible (puthash root id last-visible))))
    (dolist (node (nreverse nodes))
      (let* ((id (tessera-thread-context-id node))
             (root (tessera-thread-context-root node))
             (parent-id (tessera-thread-context-parent node))
             (parent (gethash parent-id contexts)))
        (setf (tessera-thread-context-total node)
              (gethash root totals)
              (tessera-thread-context-unread node)
              (gethash root unreads 0)
              (tessera-thread-context-last node)
              (equal id (gethash root last-visible)))
        (when parent
          (setf (tessera-thread-context-reverse-path node)
                (cons (not (equal id (gethash parent-id last-child)))
                      (tessera-thread-context-reverse-path
                       parent))))))
    contexts))

;;;; Rendering support

(defun tessera--find-entry-backend (backend)
  "Return the registered definition for BACKEND."
  (or (gethash backend tessera--entry-backends)
      (error "Unknown Tessera entry backend `%s'" backend)))

(defun tessera--find-entry-layout (definition &optional context)
  "Return the layout for DEFINITION and optional CONTEXT."
  (let ((thread (and context (tessera-entry-context-thread context)))
        (layout (tessera--entry-backend-thread-layout definition)))
    (if (and thread layout)
        (if (tessera-thread-context-first thread)
            (tessera-thread-layout-head layout)
          (tessera-thread-layout-child layout))
      (or (cdr (assq tessera-entry-layout
                     (tessera--entry-backend-layouts definition)))
          (error "Backend `%s' has no layout `%s'"
                 (tessera--entry-backend-name definition)
                 tessera-entry-layout)))))

(defun tessera-thread-count (context)
  "Return the unread/total count of the thread in CONTEXT."
  (when-let* ((thread (tessera-entry-context-thread context)))
    (propertize
     (format "%d/%d" (tessera-thread-context-unread thread)
             (tessera-thread-context-total thread))
     'face (if (> (tessera-thread-context-unread thread) 0)
               'tessera-glyph-accent-face 'tessera-glyph-muted-face)
     'help-echo "Unread / total, including folded messages")))

(defvar tessera--thread-prefix-paths
  (make-hash-table :test #'eq :weakness 'key)
  "Bounded forward paths cached by shared reverse path identity.
Values contain a path limit and its immutable forward prefix.")

(defun tessera--thread-prefix-path (tail limit)
  "Return at most LIMIT root-to-child branches from reverse TAIL.
Reuse cached ancestor prefixes without retaining dead contexts."
  (let ((cursor tail) cached pending)
    (while (and cursor
                (not (and (setq cached
                                (gethash
                                 cursor tessera--thread-prefix-paths))
                          (= limit (car cached)))))
      (push cursor pending)
      (setq cursor (cdr cursor)))
    (let ((path (and cursor (cdr cached))))
      (dolist (node pending)
        (when (< (length path) limit)
          (setq path (append path (list (car node)))))
        (puthash node (cons limit path) tessera--thread-prefix-paths))
      path)))

(defun tessera-thread-prefix (context &optional width)
  "Return the configured tree prefix for native member CONTEXT.
Align each branch with its parent text using the segment gap.
WIDTH, when non-nil, bounds generation to cover that many window
columns, with extra branches for subsequent layout clipping.
Without WIDTH, preserve every ancestor column.  Spaces become
layout overlays; the context always retains its full ancestry."
  (unless (or (null width) (natnump width))
    (error "Thread prefix width must be a nonnegative integer"))
  (when-let* ((thread (tessera-entry-context-thread context))
              (tail (tessera--thread-path-tail thread)))
    (let* ((indent (+ 2 tessera-entry-segment-gap))
           (path (if width
                     (tessera--thread-prefix-path
                      tail (+ 2 (ceiling width indent)))
                   (reverse tail)))
           (branch
            (tessera-glyph-render
             (tessera-glyph-resolve
              (if (car (last path)) 'branch 'last)
              tessera--thread-glyph-defaults tessera-thread-glyphs)
             context))
           (vertical
            (tessera-glyph-render
             (tessera-glyph-resolve
              'vertical tessera--thread-glyph-defaults
              tessera-thread-glyphs) context)))
      (concat
       (mapconcat
        (lambda (continues)
          (concat
           (when continues vertical)
           (propertize
            (make-string (if continues (1- indent) indent) ?\s)
            'tessera--layout-space t)))
        (butlast path) "")
       branch))))

(defun tessera--make-entry-context
    (definition object window)
  "Build a context for OBJECT, DEFINITION, and WINDOW."
  (let* ((backend (tessera--entry-backend-name definition))
         (context
          (funcall (tessera--entry-backend-context definition)
                   object
                   (current-buffer)
                   window)))
    (unless (tessera-entry-context-p context)
      (error "Backend `%s' returned an invalid entry context"
             backend))
    (unless (eq (tessera-entry-context-backend context) backend)
      (error "Entry context names backend `%s', expected `%s'"
             (tessera-entry-context-backend context) backend))
    context))

(defun tessera--space (width)
  "Return a display space occupying WIDTH columns."
  (if (> width 0)
      (propertize " " 'display `(space :width ,width)
                  'tessera--layout-space t)
    ""))

(defun tessera--align-space (right-offset)
  "Return a display space aligned RIGHT-OFFSET from the right edge.
RIGHT-OFFSET is a column count or a one-element pixel count list."
  (propertize
   " " 'display `(space :align-to (- right ,right-offset))
   'tessera--layout-space t))

(defun tessera--add-default-property (string property value)
  "Add PROPERTY with VALUE where STRING does not already have it."
  (let ((position 0)
        (end (length string)))
    (while (< position end)
      (let ((next
             (next-single-property-change
              position property string end)))
        (unless (get-text-property position property string)
          (put-text-property position next property value string))
        (setq position next))))
  string)

;;;; Segment rendering

(cl-defstruct (tessera--rendered-segment
               (:constructor tessera--make-rendered-segment))
  "Store one rendered segment and its width policy."
  string
  width
  target-width
  grow
  min-width
  max-width
  truncate
  priority
  optional
  visible
  point)

(defun tessera--render-segment (reference definition context)
  "Render segment REFERENCE using DEFINITION and CONTEXT."
  (if (eq (car-safe reference) :slots)
      (tessera--render-slot-group (cdr reference) definition context)
    (let* ((name (tessera--reference-name reference "segment"))
           (provider
            (cdr (assq name
                       (tessera--entry-backend-segments definition))))
           (value (funcall provider context)))
      (unless (or (null value) (stringp value))
        (error "Segment provider `%s' returned `%S'" name value))
      (when (and value (string-match-p "[\n\r]" value))
        (error "Segment provider `%s' returned multiline text" name))
      (when value
        (let* ((properties (and (consp reference) (cdr reference)))
               (width (string-width value))
               (maximum (plist-get properties :max-width))
               (truncate (plist-get properties :truncate)))
          (tessera--make-rendered-segment
           :string value
           :width width
           :target-width (if (and maximum truncate)
                             (min width maximum)
                           width)
           :grow (plist-get properties :grow)
           :min-width (or (plist-get properties :min-width) 0)
           :max-width maximum
           :truncate truncate
           :priority (or (plist-get properties :priority) 0)
           :optional (plist-get properties :optional)
           :point (plist-get properties :point)
           :visible t))))))

(defun tessera--render-segments (references definition context)
  "Render REFERENCES using DEFINITION and CONTEXT."
  (delq nil
        (mapcar (lambda (reference)
                  (tessera--render-segment
                   reference definition context))
                references)))

(defun tessera--visible-segments (segments)
  "Return the visible members of SEGMENTS."
  (cl-remove-if-not #'tessera--rendered-segment-visible segments))

(defun tessera--segments-width (segments)
  "Return the allocated width of visible SEGMENTS."
  (let ((visible (tessera--visible-segments segments)))
    (+ (cl-loop for segment in visible
                sum (tessera--rendered-segment-target-width segment))
       (* tessera-entry-segment-gap
          (max 0 (1- (length visible)))))))

(defun tessera--single-line-width (left right slot-width)
  "Return the allocated line width for LEFT, RIGHT, and SLOT-WIDTH."
  (+ (* 2 tessera-entry-safe-gap)
     tessera-entry-left-padding
     slot-width
     (if (and (> slot-width 0)
              (tessera--visible-segments left))
         tessera-entry-segment-gap
       0)
     (tessera--segments-width left)
     tessera-entry-flex-gap-min-width
     (tessera--segments-width right)
     tessera-entry-right-padding))

(defun tessera--segments-by-priority (segments predicate)
  "Return SEGMENTS matching PREDICATE from low to high priority."
  (cl-stable-sort
   (cl-remove-if-not predicate (copy-sequence segments))
   #'<
   :key #'tessera--rendered-segment-priority))

(defun tessera--shrink-segments (segments overflow predicate)
  "Shrink SEGMENTS by up to OVERFLOW columns when PREDICATE allows it.
Return the number of columns still overflowing."
  (dolist (segment (tessera--segments-by-priority segments predicate))
    (when (> overflow 0)
      (let* ((target (tessera--rendered-segment-target-width segment))
             (minimum (min target
                           (tessera--rendered-segment-min-width
                            segment)))
             (reduction (min overflow (- target minimum))))
        (setf (tessera--rendered-segment-target-width segment)
              (- target reduction))
        (setq overflow (- overflow reduction)))))
  overflow)

(defun tessera--grow-segments (segments spare-width)
  "Give visible growing SEGMENTS up to SPARE-WIDTH columns."
  (dolist (segment
           (cl-stable-sort
            (cl-remove-if-not
             (lambda (candidate)
               (and (tessera--rendered-segment-visible candidate)
                    (tessera--rendered-segment-grow candidate)))
             (copy-sequence segments))
            #'>
            :key #'tessera--rendered-segment-priority))
    (when (> spare-width 0)
      (let* ((target (tessera--rendered-segment-target-width segment))
             (natural (tessera--rendered-segment-width segment))
             (maximum (tessera--rendered-segment-max-width segment))
             (desired (if (and maximum
                               (tessera--rendered-segment-truncate
                                segment))
                          (min natural maximum)
                        natural))
             (increase (min spare-width (- desired target))))
        (setf (tessera--rendered-segment-target-width segment)
              (+ target increase))
        (setq spare-width (- spare-width increase)))))
  spare-width)

(defun tessera--allocate-segment-widths
    (left right slot-width available-width)
  "Fit LEFT and RIGHT segments beside SLOT-WIDTH in AVAILABLE-WIDTH."
  (let* ((segments (append left right))
         (overflow (max 0 (- (tessera--single-line-width
                              left right slot-width)
                             available-width))))
    (setq overflow
          (tessera--shrink-segments
           segments overflow
           (lambda (segment)
             (and (tessera--rendered-segment-visible segment)
                  (tessera--rendered-segment-grow segment)
                  (tessera--rendered-segment-truncate segment)))))
    (dolist (segment
             (tessera--segments-by-priority
              segments
              (lambda (candidate)
                (and (tessera--rendered-segment-visible candidate)
                     (tessera--rendered-segment-optional
                      candidate)))))
      (when (> overflow 0)
        (setf (tessera--rendered-segment-visible segment) nil)
        (setq overflow
              (max 0 (- (tessera--single-line-width
                         left right slot-width)
                        available-width)))))
    (when (= overflow 0)
      (tessera--grow-segments
       segments
       (- available-width
          (tessera--single-line-width left right slot-width))))
    (tessera--shrink-segments
     segments overflow
     (lambda (segment)
       (and (tessera--rendered-segment-visible segment)
            (tessera--rendered-segment-truncate segment))))))

(defun tessera--ellipsis (width)
  "Return the configured truncation marker within WIDTH columns.
Use a period if the marker's first character is too wide to fit."
  (let ((text (truncate-string-to-width
               tessera-entry-ellipsis width)))
    (if (and (> width 0) (string-empty-p text)) "." text)))

(defun tessera--truncate-string (string width method)
  "Truncate STRING to WIDTH columns according to METHOD."
  (let ((natural-width (string-width string)))
    (cond
     ((or (>= width natural-width) (null method))
      string)
     ((<= width 0)
      "")
     ((eq method 'tail)
      (truncate-string-to-width
       string width nil nil (tessera--ellipsis width)))
     (t
      (let* ((ellipsis (tessera--ellipsis width))
             (ellipsis-width (string-width ellipsis))
             (content-width (max 0 (- width ellipsis-width))))
        (if (<= width ellipsis-width)
            (truncate-string-to-width ellipsis width)
          (pcase method
            ('head
             (concat
              ellipsis
              (truncate-string-to-width
               string natural-width (- natural-width content-width))))
            ('middle
             (let* ((left-width (/ (1+ content-width) 2))
                    (right-width (- content-width left-width)))
               (concat
                (truncate-string-to-width string left-width)
                ellipsis
                (truncate-string-to-width
                 string natural-width
                 (- natural-width right-width)))))
            (_ string))))))))

(defun tessera--render-segment-group (segments)
  "Return visible SEGMENTS as one rendered string."
  (mapconcat
   (lambda (segment)
     (let ((text
            (copy-sequence
             (tessera--truncate-string
              (tessera--rendered-segment-string segment)
              (tessera--rendered-segment-target-width segment)
              (tessera--rendered-segment-truncate segment)))))
       (when (and (tessera--rendered-segment-point segment)
                  (not (string-empty-p text)))
         (put-text-property 0 1 'tessera-entry-point t text))
       (tessera--prepare-hover text)))
   (tessera--visible-segments segments)
   (tessera--space tessera-entry-segment-gap)))

(defun tessera--prepare-hover (text)
  "Give each mouse-face span in TEXT a private face value.
Emacs joins adjacent mouse-face regions by identity.  Fresh values
keep elements separate after layout spaces move into overlays.
Unstyled spans, including ellipses, inherit neighboring hover faces
or use `tessera-entry-hover-face'.  Preserve neutral separators."
  (let ((position 0)
        (end (length text))
        previous faces)
    (while (< position end)
      (let* ((next (next-single-property-change
                    position 'mouse-face text end))
             (neighbor (if (and previous (not (eq previous 'default)))
                           previous
                         (and (< next end)
                              (get-text-property
                               next 'mouse-face text))))
             (face (or (get-text-property position 'mouse-face text)
                       (and (not (eq neighbor 'default)) neighbor)
                       'tessera-entry-hover-face))
             (private
              (if (eq face 'default)
                  face
                (or (cdr (assq face faces))
                    (let ((copy (if (consp face) (copy-tree face)
                                  (list face))))
                      (push (cons face copy) faces)
                      copy)))))
        (put-text-property position next 'mouse-face private text)
        (setq previous face position next))))
  text)

;;;; Glyph rendering

(defvar tessera--glyph-interaction-properties
  '((:mouse-face . mouse-face)
    (:help-echo . help-echo)
    (:keymap . keymap)
    (:pointer . pointer)
    (:follow-link . follow-link))
  "Map glyph variant keys to text properties.")

(defvar tessera--nerd-icons-availability nil
  "Cached availability of the optional Nerd Icons library.")

(defun tessera--glyph-frame (context)
  "Return the frame used to render glyphs for CONTEXT."
  (let ((window (tessera-entry-context-window context)))
    (if (window-live-p window)
        (window-frame window)
      (selected-frame))))

(defun tessera--glyph-string-displayable-p (string frame)
  "Return non-nil when every character in STRING displays on FRAME."
  (with-selected-frame frame
    (cl-every #'char-displayable-p string)))

(defun tessera--unicode-glyph-text (glyph frame)
  "Return GLYPH's Unicode text when it can display on FRAME."
  (let ((text (tessera-glyph-unicode glyph)))
    (when (and text (display-graphic-p frame)
               (tessera--glyph-string-displayable-p
                text frame))
      text)))

(defun tessera--nerd-icons-available-p ()
  "Return non-nil when the optional Nerd Icons library is available."
  (cond
   ((featurep 'nerd-icons)
    (setq tessera--nerd-icons-availability 'available))
   ((eq tessera--nerd-icons-availability 'unavailable)
    nil)
   ((require 'nerd-icons nil t)
    (setq tessera--nerd-icons-availability 'available))
   (t
    (setq tessera--nerd-icons-availability 'unavailable)
    nil)))

(defun tessera--nerd-icons-glyph-text (glyph frame)
  "Return GLYPH's Nerd Icons text when it can display on FRAME."
  (when (and (tessera-glyph-nerd-icons glyph)
             (display-graphic-p frame)
             (tessera--nerd-icons-available-p))
    (let* ((descriptor (tessera-glyph-nerd-icons glyph))
           (function (plist-get descriptor :function))
           (name (plist-get descriptor :name)))
      (when (fboundp function)
        (condition-case nil
            (let ((text (funcall function name)))
              (when (and (tessera--glyph-string-p text)
                         (tessera--glyph-string-displayable-p
                          text frame))
                text))
          (error nil))))))

(defun tessera--glyph-text (glyph context)
  "Return the best available text for GLYPH in CONTEXT."
  (let ((frame (tessera--glyph-frame context))
        (ascii (tessera-glyph-ascii glyph)))
    (pcase tessera-glyph-style
      ('ascii ascii)
      ('unicode
       (or (tessera--unicode-glyph-text glyph frame)
           ascii))
      ('nerd-icons
       (or (tessera--nerd-icons-glyph-text glyph frame)
           (tessera--unicode-glyph-text glyph frame)
           ascii))
      (_
       (error "Unknown Tessera glyph style `%s'"
              tessera-glyph-style)))))

(defun tessera--glyph-color-face (glyph)
  "Return GLYPH's face under the current color preference."
  (cond
   ((eq tessera-glyph-color t) (tessera-glyph-face glyph))
   ((stringp tessera-glyph-color)
    `(:foreground ,tessera-glyph-color))))

(defun tessera--glyph-hover-color-face (face context)
  "Return a foreground-only hover FACE for CONTEXT."
  (if (symbolp face)
      `(:foreground
        ,(face-attribute face :foreground
                         (tessera--glyph-frame context) 'default))
    face))

(defun tessera--apply-glyph-color (text glyph context)
  "Apply GLYPH color and metadata to TEXT in CONTEXT."
  (let* ((face (tessera--glyph-color-face glyph))
         (hover-face (and face
                          (tessera--glyph-hover-color-face
                           face context))))
    (put-text-property 0 (length text) 'tessera-glyph t text)
    (when face
      (add-face-text-property 0 (length text) face t text)
      (put-text-property 0 (length text) 'mouse-face
                         (list hover-face 'tessera-entry-hover-face)
                         text)))
  text)

(defun tessera--apply-glyph-interaction (text properties glyph)
  "Apply variant PROPERTIES for GLYPH to TEXT."
  (let ((color-face (tessera--glyph-color-face glyph)))
    (dolist (entry tessera--glyph-interaction-properties)
      (let ((keyword (car entry))
            (property (cdr entry)))
        (when (plist-member properties keyword)
          (let ((value (plist-get properties keyword)))
            (when (and value
                       (eq property 'mouse-face)
                       color-face)
              (setq value (list value color-face)))
            (put-text-property
             0 (length text) property value text))))))
  text)

(defun tessera-glyph-render (glyph context &optional properties)
  "Render GLYPH for CONTEXT with optional interaction PROPERTIES.

PROPERTIES accepts the keys supported by registered glyph variants.
Color comes from GLYPH and `tessera-glyph-color'.  Hidden glyphs
render as an empty string."
  (tessera--validate-glyph glyph "Glyph")
  (unless (tessera-entry-context-p context)
    (error "Glyph context must be a Tessera entry context"))
  (tessera--ensure-plist-keys properties
                              tessera--glyph-variant-properties
                              "Glyph interaction properties")
  (when (plist-member properties :glyph)
    (error "Glyph interaction properties must not contain :glyph"))
  (tessera--render-glyph glyph context properties))

(defun tessera--render-glyph (glyph context properties)
  "Render validated GLYPH and interaction PROPERTIES in CONTEXT."
  (if (tessera-glyph-hidden glyph)
      ""
    (let ((text (copy-sequence (tessera--glyph-text glyph context))))
      (tessera--apply-glyph-color text glyph context)
      (tessera--apply-glyph-interaction text properties glyph)
      (tessera--prepare-hover text))))

(defun tessera--glyph-slot-padding
    (slot content-width &optional target-width)
  "Return SLOT padding for CONTENT-WIDTH and optional TARGET-WIDTH."
  (let* ((width (or target-width (tessera-glyph-slot-width slot)))
         (remaining (max 0 (- width content-width)))
         (left
          (pcase (tessera-glyph-slot-align slot)
            ('left 0)
            ('center (/ remaining 2))
            ('right remaining))))
    (cons left (- remaining left))))

(defun tessera--fit-glyph-width (text width)
  "Fit TEXT within WIDTH pixels, returning its final pixel width.
Shrink only oversized glyphs, preserving their other properties.
TEXT must be a private rendered copy.  Use the selected frame."
  (let ((pixels (string-pixel-width text)))
    ;; Font sizes are discrete, so a proportional step can round up.
    ;; Bound retries for fonts that cannot be scaled.
    (cl-loop repeat 16
             while (> pixels width)
             do (add-face-text-property
                 0 (length text)
                 (list :height (* 0.99 (/ (float width) pixels)))
                 nil text)
             (setq pixels (string-pixel-width text)))
    (when (> pixels width)
      (error "Glyph cannot fit within %d pixels" width))
    pixels))

(defun tessera--render-glyph-slot (slot context &optional omit-empty)
  "Render glyph SLOT for CONTEXT at its fixed width.
Return nil when the selector returns nil and OMIT-EMPTY is non-nil."
  (let* ((variant-id
          (funcall (tessera-glyph-slot-selector slot) context))
         (variant
          (and variant-id
               (assq variant-id
                     (tessera-glyph-slot-glyphs slot)))))
    (cond
     ((or (null variant-id)
          (and variant
               (tessera-glyph-hidden
                (plist-get (cdr variant) :glyph))))
      (unless omit-empty
        (tessera--space (tessera-glyph-slot-width slot))))
     ((null variant)
      (display-warning
       'tessera
       (format "Glyph slot `%s' selected unknown variant `%s'"
               (tessera-glyph-slot-name slot) variant-id)
       :warning)
      (tessera--space (tessera-glyph-slot-width slot)))
     (t
      (let* ((properties (cdr variant))
             (glyph (plist-get properties :glyph))
             (text
              (tessera--render-glyph
               glyph context
               (cl-loop for (key value) on properties by #'cddr
                        unless (eq key :glyph)
                        append (list key value))))
             (content-width (string-width text)))
        (when (> content-width
                 (tessera-glyph-slot-width slot))
          (error "Glyph variant `%s' exceeds slot `%s' width"
                 variant-id (tessera-glyph-slot-name slot)))
        (let* ((frame (tessera--glyph-frame context))
               (pixels (display-graphic-p frame))
               (padding
                (if pixels
                    (with-selected-frame frame
                      (let ((width
                             (* (tessera-glyph-slot-width slot)
                                (frame-char-width frame))))
                        (tessera--glyph-slot-padding
                         slot (tessera--fit-glyph-width text width)
                         width)))
                  (tessera--glyph-slot-padding slot content-width)))
               (space (if pixels #'tessera--pixel-space
                        #'tessera--space)))
          (concat (funcall space (car padding))
                  text
                  (funcall space (cdr padding)))))))))

(defun tessera--pixel-space (width)
  "Return a decorative space occupying WIDTH pixels."
  (if (> width 0)
      (propertize " " 'display `(space :width (,width))
                  'tessera--layout-space t)
    ""))

(defun tessera--reserve-glyph-slot (slot rendered context)
  "Return a blank occupying RENDERED SLOT's display width in CONTEXT."
  (let ((frame (tessera--glyph-frame context)))
    (if (display-graphic-p frame)
        (with-selected-frame frame
          (tessera--pixel-space (string-pixel-width rendered)))
      (tessera--space (tessera-glyph-slot-width slot)))))

(defun tessera--render-glyph-slots
    (references definition context &optional align)
  "Render slot REFERENCES using DEFINITION and CONTEXT.
Return (TEXT . WIDTH), or nil if all references are omitted.
ALIGN packs visible icons at the `left' or `right' of the full area.
With nil ALIGN, empty optional slots are omitted and other slots
keep their individual positions.  Each selector runs once."
  (let (visible hidden (width 0))
    (dolist (reference references)
      (let* ((name (tessera--reference-name reference "glyph slot"))
             (slot
              (cl-find name
                       (tessera--entry-backend-glyph-slots definition)
                       :key #'tessera-glyph-slot-name))
             (text (tessera--render-glyph-slot
                    slot context
                    (and (null align) (consp reference)
                         (plist-get (cdr reference) :optional)))))
        (when text
          (cl-incf width (tessera-glyph-slot-width slot))
          (when (tessera--glyph-slot-reference-reserved-p reference)
            (setq text (tessera--reserve-glyph-slot
                        slot text context)))
          (if (and align
                   (not (text-property-not-all
                         0 (length text)
                         'tessera-glyph nil text)))
              (push text hidden)
            (push text visible)))))
    (when (or visible hidden)
      (let* ((text (apply #'concat (nreverse visible)))
             (padding (apply #'concat (nreverse hidden)))
             ;; Preserve decorative strings and their pixel widths.
             (rendered (if (eq align 'right)
                           (concat padding text)
                         (concat text padding)))
             (window (tessera-entry-context-window context))
             (frame (and (window-live-p window)
                         (window-frame window))))
        (cons rendered
              (if (and frame (display-graphic-p frame))
                  (with-selected-frame frame
                    (ceiling (string-pixel-width rendered)
                             (frame-char-width)))
                width))))))

(defun tessera--render-slot-group (references definition context)
  "Render inline slot REFERENCES from DEFINITION in CONTEXT."
  (when-let* ((area (tessera--render-glyph-slots
                     references definition context)))
    (let ((width (cdr area)))
      (tessera--make-rendered-segment
       :string (car area)
       :width width
       :target-width width
       :min-width width
       :max-width width
       :priority 0
       :visible t))))

;;;; Entry rendering

(defun tessera--layout-has-extra-line-p (layout)
  "Return non-nil when LAYOUT defines an extra visual line."
  (or (tessera-entry-layout-extra-leading-segments layout)
      (tessera-entry-layout-extra-glyph-slots layout)
      (tessera-entry-layout-extra-left-segments layout)
      (tessera-entry-layout-extra-right-segments layout)))

(defun tessera--visual-line-break ()
  "Return a logical space displayed as a visual line break."
  (propertize " " 'display "\n"))

(defun tessera--clip-thread-content (text width)
  "Clip thread TEXT on the right to WIDTH columns when necessary.
The tree provider supplies `tessera--overflow-help' for the ellipsis.
Retain original columns and move a hidden navigation anchor onto the
ellipsis.  Keep layout whitespace outside its mouse hover range."
  (let ((help (and (> (length text) 0)
                   (get-text-property
                    0 'tessera--overflow-help text)))
        (width (max 1 width)))
    (if (or (not help) (<= (string-width text) width))
        text
      (let* ((marker (tessera--ellipsis width))
             (remaining (- width (string-width marker)))
             (prefix (truncate-string-to-width text remaining))
             (ellipsis
              (propertize marker 'tessera--overflow t
                          'face 'tessera-glyph-muted-face
                          'mouse-face (list 'tessera-entry-hover-face)
                          'help-echo help)))
        (when (and (tessera-entry-point text)
                   (not (tessera-entry-point prefix)))
          (put-text-property 0 (length ellipsis)
                             'tessera-entry-point t ellipsis))
        (concat prefix
                (tessera--space (- remaining (string-width prefix)))
                ellipsis)))))

(defun tessera--render-line
    (slot-references left-references right-references
                     definition context &optional glyph-align
                     leading-references leading-width)
  "Render one visual line from SLOT-REFERENCES and segment references.
LEFT-REFERENCES and RIGHT-REFERENCES name segments in DEFINITION.
CONTEXT supplies their entry data and target window.
GLYPH-ALIGN optionally packs prefix icons within their fixed area.
LEADING-REFERENCES replace those icons with segments.
LEADING-WIDTH supplies the shared minimum width of that area."
  (let* ((window (tessera-entry-context-window context))
         (slot-area (tessera--render-glyph-slots
                     slot-references definition context glyph-align))
         (leading (tessera--render-segments
                   leading-references definition context))
         (slot-area (if leading
                        (cons (tessera--render-segment-group leading)
                              (tessera--segments-width leading))
                      slot-area))
         (minimum (if (functionp leading-width)
                      (funcall leading-width context)
                    (or leading-width 0)))
         (slot-width (max minimum (or (cdr slot-area) 0)))
         (padding (tessera--space
                   (- slot-width (or (cdr slot-area) 0))))
         (slots (if (eq glyph-align 'left)
                    (concat (car slot-area) padding)
                  (concat padding (car slot-area))))
         (left
          (tessera--render-segments
           left-references definition context))
         (right
          (tessera--render-segments
           right-references definition context)))
    (when (window-live-p window)
      (tessera--allocate-segment-widths
       left right slot-width (window-body-width window)))
    (let* ((left-string (tessera--render-segment-group left))
           (right-string (tessera--render-segment-group right))
           (slot-gap
            (if (and (> slot-width 0) (> (length left-string) 0))
                (tessera--space tessera-entry-segment-gap)
              ""))
           (margin (+ tessera-entry-safe-gap
                      tessera-entry-right-padding))
           (right-offset
            (if (and (window-live-p window)
                     (display-graphic-p (window-frame window)))
                (save-excursion
                  (with-selected-window window
                    ;; Fallback fonts need not occupy whole columns.
                    (list (+ (* margin (frame-char-width))
                             (string-pixel-width right-string)))))
              (+ margin (tessera--segments-width right))))
           (left-string
            (if (window-live-p window)
                (tessera--clip-thread-content
                 left-string
                 (- (window-body-width window)
                    tessera-entry-safe-gap tessera-entry-left-padding
                    slot-width (if (string-empty-p slot-gap) 0
                                 tessera-entry-segment-gap)
                    tessera-entry-flex-gap-min-width
                    (if (consp right-offset)
                        (ceiling (car right-offset)
                                 (frame-char-width
                                  (window-frame window)))
                      right-offset)))
              left-string))
           (surface
            (concat
             (tessera--space tessera-entry-left-padding)
             slots
             slot-gap
             left-string
             (tessera--space tessera-entry-flex-gap-min-width)
             (tessera--align-space right-offset)
             right-string)))
      ;; Right alignment already reserves right padding and safe gap.
      ;; A trailing after-string can wrap the native newline.
      (concat (tessera--space tessera-entry-safe-gap) surface))))

(defun tessera--render-single-line (layout definition context)
  "Render single-line LAYOUT using DEFINITION and CONTEXT."
  (tessera--render-line
   (tessera-entry-layout-main-glyph-slots layout)
   (tessera-entry-layout-main-left-segments layout)
   (tessera-entry-layout-main-right-segments layout)
   definition context
   (tessera-entry-layout-glyph-slots-align layout)
   (tessera-entry-layout-main-leading-segments layout)
   (tessera-entry-layout-leading-width layout)))

(defun tessera--render-two-line (layout definition context)
  "Render two-line LAYOUT using DEFINITION and CONTEXT."
  (let ((main (concat
               (tessera--render-single-line layout definition context)
               (tessera--visual-line-break)))
        (thread (and (tessera--entry-backend-thread-layout definition)
                     (tessera-entry-context-thread context))))
    ;; A thread heading belongs to the group, not its first message.
    (when (and thread (tessera-thread-context-first thread))
      (put-text-property 0 (length main)
                         'tessera--thread-heading t main))
    (concat
     main
     (tessera--render-line
      (tessera-entry-layout-extra-glyph-slots layout)
      (tessera-entry-layout-extra-left-segments layout)
      (tessera-entry-layout-extra-right-segments layout)
      definition context
      (tessera-entry-layout-glyph-slots-align layout)
      (tessera-entry-layout-extra-leading-segments layout)
      (tessera-entry-layout-leading-width layout)))))

(defun tessera--render-entry-lines (layout definition context)
  "Render the visual lines of LAYOUT using DEFINITION and CONTEXT."
  (if (tessera--layout-has-extra-line-p layout)
      (tessera--render-two-line layout definition context)
    (tessera--render-single-line layout definition context)))

(defun tessera--inert-layout-string (string)
  "Copy decorative STRING without changing its appearance on hover.
An empty face blocks hover inherited from the underlying buffer.
Keep each face span separate: Emacs paints an entire mouse-face
span using the face of the character under the pointer."
  (let* ((text (tessera--add-default-property
                (copy-sequence string) 'face 'default))
         (position 0)
         (end (length text)))
    (while (< position end)
      (let ((next (next-single-property-change
                   position 'face text end)))
        (put-text-property position next 'mouse-face
                           (list :inherit nil) text)
        (setq position next)))
    text))

(defun tessera--padding-string (height)
  "Return display-only vertical padding of HEIGHT normal lines."
  (when (> height 0)
    (tessera--inert-layout-string
     (propertize " \n" 'face `((:height ,height) default)
                 'line-height t))))

(defun tessera--entry-content (rendered &optional prefix)
  "Extract RENDERED content and layout after native PREFIX."
  ;; Keep the padding's buffer position outside every content hover
  ;; span.  A before-string containing a newline can otherwise enter
  ;; the first element's mouse-face rectangle, despite its own face.
  ;; A zero-width display keeps this anchor on the content's visual
  ;; line; making it invisible breaks backward visual-line motion.
  (let* ((prefix (tessera--inert-layout-string
                  (if (string-empty-p (or prefix "")) " " prefix)))
         (position 0)
         (length (length rendered))
         (content prefix)
         (pending (tessera--padding-string
                   tessera-entry-top-padding))
         placements)
    (put-text-property 0 (length prefix) 'display
                       '(space :width 0) prefix)
    (while (< position length)
      (let* ((space (get-text-property
                     position 'tessera--layout-space rendered))
             (end (next-single-property-change
                   position 'tessera--layout-space rendered length))
             (part (substring rendered position end)))
        (if space
            (setq pending (concat pending part))
          (when pending
            ;; Leading decoration precedes native control text too.
            ;; Otherwise its newline leaves a hidden visual row.
            (push (list (if (= (length content) (length prefix))
                            0
                          (length content))
                        'before-string pending)
                  placements)
            (setq pending nil))
          (setq content (concat content part)))
        (setq position end)))
    (when pending
      (push (list (1- (length content)) 'after-string pending)
            placements))
    (put-text-property
     0 1 'tessera--entry-layout
     (list (nreverse placements) tessera-entry-bottom-padding)
     content)
    content))

;;;; Entry navigation

(defun tessera-entry-point (&optional string)
  "Return the preferred navigation position in STRING or this line.
Return nil when no displayed segment specifies `:point t'."
  (text-property-any
   (if string 0 (line-beginning-position))
   (if string (length string) (line-end-position))
   'tessera-entry-point t string))

(defun tessera-entry-save-point ()
  "Save point within its logical entry for a subsequent redraw.
Pass the returned snapshot to `tessera-entry-restore-point' to
restore point and release its marker, even if redrawing fails."
  (list (copy-marker (line-beginning-position))
        (- (point) (line-beginning-position))
        (get-text-property (point) 'tessera-entry-point)))

(defun tessera-entry-restore-point (snapshot)
  "Restore point from SNAPSHOT after redrawing its logical entry.
A preferred navigation position follows its segment; other
positions retain their character offset.  Release the saved marker."
  (pcase-let ((`(,origin ,offset ,anchored) snapshot))
    (unwind-protect
        (when (marker-buffer origin)
          (with-current-buffer (marker-buffer origin)
            (goto-char origin)
            (goto-char
             (or (and anchored (tessera-entry-point))
                 (min (+ (point) offset) (line-end-position))))))
      (set-marker origin nil))))

;;;; Current entry highlighting

(defvar-local tessera--current-entry nil
  "Current entry's boundary markers, layout, and saved decorations.")

(defun tessera-entry-clear-current ()
  "Restore the current entry's original faces and decorations."
  (when tessera--current-entry
    (pcase-let ((`(,start ,end ,_layout ,decorations)
                 tessera--current-entry))
      (save-restriction
        (widen)
        (with-silent-modifications
          (let ((inhibit-read-only t)
                (position (marker-position start))
                (limit (marker-position end)))
            (while (< position limit)
              (let ((next (next-single-property-change
                           position 'tessera--current-face nil limit))
                    (saved (get-text-property
                            position 'tessera--current-face)))
                (when saved
                  (if (car saved)
                      (put-text-property
                       position next 'face (car saved))
                    (remove-text-properties
                     position next '(face nil)))
                  (remove-text-properties
                   position next '(tessera--current-face nil)))
                (setq position next))))))
      (dolist (decoration decorations)
        (pcase-let ((`(,overlay ,property ,original ,styled)
                     decoration))
          (when (and (overlay-buffer overlay)
                     (eq (overlay-get overlay property) styled))
            (overlay-put overlay property original))))
      (set-marker start nil)
      (set-marker end nil))
    (setq tessera--current-entry nil)))

(defun tessera--current-entry-decorations (start end)
  "Style horizontal layout spaces between START and END.
Return the original and styled overlay strings for restoration."
  (let (decorations)
    (dolist (overlay (overlays-in start end))
      (when (overlay-get overlay 'tessera-entry-overlay)
        (dolist (property '(before-string after-string))
          (when-let* ((original (overlay-get overlay property)))
            (let ((styled (copy-sequence original))
                  (position 0)
                  (limit (length original)))
              (while (< position limit)
                (let ((next
                       (min (next-single-property-change
                             position 'tessera--layout-space
                             original limit)
                            (next-single-property-change
                             position 'tessera--thread-heading
                             original limit))))
                  (when (and (get-text-property
                              position 'tessera--layout-space
                              original)
                             (not (get-text-property
                                   position 'tessera--thread-heading
                                   original)))
                    (add-face-text-property
                     position next 'tessera-entry-current-face
                     nil styled))
                  (setq position next)))
              (overlay-put overlay property styled)
              (push (list overlay property original styled)
                    decorations))))))
    decorations))

(defun tessera-entry-highlight-current ()
  "Visually distinguish the entry containing point.
Keep native content faces and mouse interactions, and exclude
vertical padding and virtual thread headings.  Restore the previous
entry before moving the highlight.  This function is suitable for
`post-command-hook'."
  (let* ((start (line-beginning-position))
         (end (min (point-max) (1+ (line-end-position))))
         (layout (get-text-property start 'tessera--entry-layout)))
    (unless (and tessera--current-entry
                 (= start (nth 0 tessera--current-entry))
                 (= end (nth 1 tessera--current-entry))
                 (eq layout (nth 2 tessera--current-entry)))
      (tessera-entry-clear-current)
      (when layout
        (with-silent-modifications
          (let ((inhibit-read-only t)
                (position start))
            (while (< position end)
              (let ((next
                     (min (next-single-property-change
                           position 'face nil end)
                          (next-single-property-change
                           position 'tessera--thread-heading
                           nil end))))
                (unless (get-text-property
                         position 'tessera--thread-heading)
                  (put-text-property
                   position next 'tessera--current-face
                   (list (get-text-property position 'face)))
                  (add-face-text-property
                   position next 'tessera-entry-current-face))
                (setq position next)))
            (let ((position start))
              (while (< position end)
                (let ((next (next-single-property-change
                             position 'display nil end)))
                  (when (and (equal (get-text-property
                                     position 'display) "\n")
                             (not (get-text-property
                                   position
                                   'tessera--thread-heading)))
                    (add-face-text-property
                     position next '(:extend t)))
                  (setq position next))))
            (when (eq (char-before end) ?\n)
              (add-face-text-property (1- end) end '(:extend t)))))
        (setq tessera--current-entry
              (list (copy-marker start) (copy-marker end) layout
                    (tessera--current-entry-decorations
                     start end)))))))

(defun tessera-entry-clear-layout (&optional start end)
  "Remove Tessera layout overlays between START and END.
Omitted bounds select the whole accessible buffer."
  (when (and tessera--current-entry
             (< (or start (point-min))
                (nth 1 tessera--current-entry))
             (> (or end (point-max))
                (nth 0 tessera--current-entry)))
    (tessera-entry-clear-current))
  (remove-overlays start end 'tessera-entry-overlay t))

(defun tessera-entry-layout-applied-p (start)
  "Return non-nil if START's current layout is still attached."
  (let ((layout (get-text-property start 'tessera--entry-layout))
        (overlay (get-text-property start 'tessera--layout-overlay)))
    (and (overlayp overlay) (overlay-buffer overlay)
         (eq layout (overlay-get overlay 'tessera--entry-layout)))))

(defun tessera-entry-apply-layout (start end)
  "Attach rendered entry layout to buffer content from START to END.
END excludes the client's terminating newline, which must already
exist when bottom padding is requested.  Reapplying replaces only
Tessera overlays within this entry.  Native content properties stay
on buffer text, and all decorative spaces live in overlay strings."
  (let* ((layout (get-text-property start 'tessera--entry-layout))
         (bottom (cadr layout))
         (terminator (eq (char-after end) ?\n))
         (limit (if terminator (1+ end) end))
         anchor)
    (when (and bottom (> bottom 0) (not terminator))
      (error "Entry bottom padding needs a terminating newline"))
    (tessera-entry-clear-layout start limit)
    (dolist (placement (car layout))
      (pcase-let* ((`(,offset ,property ,string) placement)
                   (position (+ start offset))
                   (overlay (make-overlay position (1+ position))))
        (unless anchor (setq anchor overlay))
        (overlay-put overlay 'tessera-entry-overlay t)
        (overlay-put overlay 'evaporate t)
        ;; Place entry decoration after native boundary headings.
        (overlay-put overlay 'priority 1)
        (overlay-put
         overlay property
         (tessera--inert-layout-string string))))
    (when (and bottom (> bottom 0))
      (let ((overlay (make-overlay start limit)))
        (unless anchor (setq anchor overlay))
        (overlay-put overlay 'tessera-entry-overlay t)
        (overlay-put overlay 'evaporate t)
        (overlay-put overlay 'after-string
                     (tessera--padding-string bottom))))
    (when anchor
      (overlay-put anchor 'tessera--entry-layout layout)
      (with-silent-modifications
        (put-text-property start (1+ start)
                           'tessera--layout-overlay anchor)))))

(defun tessera-entry-render
    (backend object &optional window prefix)
  "Render OBJECT registered for BACKEND in WINDOW.

The result contains one logical line of content and layout metadata.
After inserting it and the native terminating newline, call
`tessera-entry-apply-layout' to display padding and alignment.
WINDOW defaults to a window displaying the current buffer.
PREFIX is optional non-displaying native text placed before content;
it does not participate in width allocation.  It also anchors layout
outside content hover ranges.  Without PREFIX, use one zero-width
space for that anchor."
  (when (and window (not (window-live-p window)))
    (error "Cannot render an entry for a dead window"))
  (let* ((definition (tessera--find-entry-backend backend))
         (target-window (or window
                            (get-buffer-window (current-buffer))))
         (context
          (tessera--make-entry-context
           definition object target-window))
         (layout (tessera--find-entry-layout definition context))
         (thread (and (tessera--entry-backend-thread-layout
                       definition)
                      (tessera-entry-context-thread context)))
         (tessera-entry-top-padding
          (if thread
              (if (tessera-thread-context-first thread)
                  tessera-thread-outer-top-padding
                tessera-thread-inner-top-padding)
            tessera-entry-top-padding))
         (tessera-entry-bottom-padding
          (if thread
              (if (tessera-thread-context-last thread)
                  tessera-thread-outer-bottom-padding
                tessera-thread-inner-bottom-padding)
            tessera-entry-bottom-padding)))
    (tessera--entry-content
     (tessera--render-entry-lines layout definition context)
     prefix)))

(provide 'tessera)
;;; tessera.el ends here
