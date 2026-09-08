;;; tessera.el --- Common foundation for Tessera  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bingshan Chang <chang@bingshan.org>

;; Author: Bingshan Chang <chang@bingshan.org>
;; Maintainer: Bingshan Chang <chang@bingshan.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (alert "1.2"))
;; Keywords: convenience, mail, news

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
(require 'subr-x)

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

(defcustom tessera-entry-top-padding 0
  "Height above entry content in normal line heights."
  :type '(restricted-sexp
          :tag "Normal line heights"
          :match-alternatives (tessera--nonnegative-number-p))
  :safe #'tessera--nonnegative-number-p
  :group 'tessera)

(defcustom tessera-entry-bottom-padding 0
  "Height below entry content in normal line heights."
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
  '((t :inherit font-lock-keyword-face))
  "Face used for accent Tessera glyphs."
  :group 'tessera)

(defface tessera-glyph-attention-face
  '((t :inherit font-lock-warning-face))
  "Face used for attention Tessera glyphs."
  :group 'tessera)

(defface tessera-glyph-informational-face
  '((t :inherit font-lock-type-face))
  "Face used for informational Tessera glyphs."
  :group 'tessera)

(defface tessera-glyph-muted-face
  '((t :inherit shadow))
  "Face used for muted Tessera glyphs."
  :group 'tessera)

(defface tessera-glyph-negative-face
  '((t :inherit error))
  "Face used for negative Tessera glyphs."
  :group 'tessera)

(defface tessera-glyph-neutral-face
  '((t :inherit default))
  "Face used for neutral Tessera glyphs."
  :group 'tessera)

(defface tessera-glyph-positive-face
  '((t :inherit success))
  "Face used for positive Tessera glyphs."
  :group 'tessera)

(defface tessera-glyph-warning-face
  '((t :inherit warning))
  "Face used for warning Tessera glyphs."
  :group 'tessera)

;;;; Data model

(defvar tessera-glyph-semantics
  '( accent attention informational muted negative neutral positive
     warning)
  "Semantic roles available to Tessera glyphs.")

(cl-defstruct tessera-entry-context
  "Describe one backend entry and its display environment.

BACKEND identifies the registered adapter.  OBJECT is the backend's
native object.  BUFFER and WINDOW identify where the entry is being
rendered.  METADATA belongs to the adapter and remains opaque to the
Tessera core."
  backend
  object
  buffer
  window
  metadata)

(cl-defstruct tessera-glyph
  "Describe the visual forms and semantic role of a glyph."
  ascii
  unicode
  nerd-icons
  semantic)

(cl-defstruct tessera-glyph-slot
  "Describe a fixed-width, single-choice glyph channel.

NAME identifies the slot within its backend.  SELECTOR is called with
an entry context and returns a variant ID or nil.  WIDTH is measured
in columns.  ALIGN is one of `left', `center', or `right'.  GLYPHS is
an alist of variant specifications."
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
Glyph slot lists accept slot names and `(NAME :reserve t)'
references.  A reserved reference occupies the selected glyph's
display width without showing it.  `(NAME :optional t)' omits the
slot when its selector returns nil, including its width."
  main-glyph-slots
  main-left-segments
  main-right-segments
  extra-glyph-slots
  extra-left-segments
  extra-right-segments)

;;;; Backend registration

(cl-defstruct (tessera--entry-backend
               (:constructor tessera--make-entry-backend))
  "Store a validated backend entry definition."
  name
  context
  segments
  glyph-slots
  layouts)

(defvar tessera--entry-backends (make-hash-table :test #'eq)
  "Registered Tessera entry backends.")

(defvar tessera--segment-properties
  '(:grow :min-width :max-width :truncate :priority :optional)
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
  (unless (and (plistp plist)
               (cl-loop for (key _)
                        on plist
                        by #'cddr
                        always (memq key allowed)))
    (error "%s contains invalid properties" description)))

(defun tessera--validate-glyph (glyph description)
  "Validate GLYPH described by DESCRIPTION."
  (unless (tessera-glyph-p glyph)
    (error "%s must contain a Tessera glyph" description))
  (let ((ascii (tessera-glyph-ascii glyph))
        (unicode (tessera-glyph-unicode glyph))
        (nerd-icons (tessera-glyph-nerd-icons glyph))
        (semantic (tessera-glyph-semantic glyph)))
    (unless (and (stringp ascii)
                 (> (length ascii) 0)
                 (cl-every (lambda (character)
                             (< character 128))
                           ascii))
      (error "%s has an invalid ASCII representation" description))
    (unless (and (stringp unicode) (> (length unicode) 0))
      (error "%s has an invalid Unicode representation" description))
    (unless (and (plistp nerd-icons)
                 (plist-get nerd-icons :function)
                 (symbolp (plist-get nerd-icons :function))
                 (stringp (plist-get nerd-icons :name))
                 (> (length (plist-get nerd-icons :name)) 0))
      (error "%s has an invalid Nerd Icons descriptor" description))
    (unless (memq semantic tessera-glyph-semantics)
      (error "%s has unknown semantic `%s'" description semantic))))

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

(defun tessera--segment-reference-name (reference)
  "Return the segment name in REFERENCE, or signal an error."
  (cond
   ((and reference (symbolp reference))
    reference)
   ((and (consp reference)
         (symbolp (car reference))
         (car reference))
    (car reference))
   (t
    (error "Invalid segment reference `%S'" reference))))

(defun tessera--validate-segment-reference (reference names)
  "Validate segment REFERENCE against registered NAMES."
  (let ((name (tessera--segment-reference-name reference)))
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
             (optional (plist-get properties :optional)))
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
                 name))))))

(defun tessera--glyph-slot-reference-name (reference)
  "Return the glyph slot name in REFERENCE, or signal an error."
  (cond
   ((and reference (symbolp reference))
    reference)
   ((and (consp reference)
         (symbolp (car reference))
         (car reference))
    (car reference))
   (t
    (error "Invalid glyph slot reference `%S'" reference))))

(defun tessera--glyph-slot-reference-reserved-p (reference)
  "Return non-nil when glyph slot REFERENCE reserves its space."
  (and (consp reference)
       (plist-get (cdr reference) :reserve)))

(defun tessera--validate-glyph-slot-reference
    (reference names description)
  "Validate glyph slot REFERENCE against NAMES for DESCRIPTION."
  (let ((name (tessera--glyph-slot-reference-name reference)))
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
  (let ((slot-lists
         (list (tessera-entry-layout-main-glyph-slots layout)
               (tessera-entry-layout-extra-glyph-slots layout)))
        (segment-lists
         (list (tessera-entry-layout-main-left-segments layout)
               (tessera-entry-layout-main-right-segments layout)
               (tessera-entry-layout-extra-left-segments layout)
               (tessera-entry-layout-extra-right-segments layout))))
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
    (backend &key context segments glyph-slots layouts)
  "Register or replace entry BACKEND.

CONTEXT is a function of an object, buffer, and window which returns
a `tessera-entry-context'.  SEGMENTS is an alist mapping segment IDs
to provider functions.  GLYPH-SLOTS is a list of
`tessera-glyph-slot' objects.  LAYOUTS is an alist mapping layout IDs
to `tessera-entry-layout' objects.

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
    (let ((definition
           (tessera--make-entry-backend
            :name backend
            :context context
            :segments segments
            :glyph-slots glyph-slots
            :layouts layouts)))
      (puthash backend definition tessera--entry-backends)))
  backend)

;;;; Rendering support

(defun tessera--find-entry-backend (backend)
  "Return the registered definition for BACKEND."
  (or (gethash backend tessera--entry-backends)
      (error "Unknown Tessera entry backend `%s'" backend)))

(defun tessera--find-entry-layout (definition)
  "Return the selected layout from backend DEFINITION."
  (let ((entry
         (assq tessera-entry-layout
               (tessera--entry-backend-layouts definition))))
    (or (cdr entry)
        (error "Backend `%s' has no layout `%s'"
               (tessera--entry-backend-name definition)
               tessera-entry-layout))))

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
  "Return a display space aligned RIGHT-OFFSET from the right edge."
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
  visible)

(defun tessera--render-segment (reference definition context)
  "Render segment REFERENCE using DEFINITION and CONTEXT."
  (if (eq (car-safe reference) :slots)
      (tessera--render-slot-group (cdr reference) definition context)
    (let* ((name (tessera--segment-reference-name reference))
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

(defun tessera--truncate-string (string width method)
  "Truncate STRING to WIDTH columns according to METHOD."
  (let ((natural-width (string-width string)))
    (cond
     ((or (>= width natural-width) (null method))
      string)
     ((<= width 0)
      "")
     ((eq method 'tail)
      (truncate-string-to-width string width nil nil t))
     (t
      (let* ((ellipsis "…")
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
       (tessera--add-default-property
        text 'mouse-face 'tessera-entry-hover-face)))
   (tessera--visible-segments segments)
   (tessera--space tessera-entry-segment-gap)))

;;;; Glyph rendering

(defvar tessera--glyph-semantic-faces
  '((accent . tessera-glyph-accent-face)
    (attention . tessera-glyph-attention-face)
    (informational . tessera-glyph-informational-face)
    (muted . tessera-glyph-muted-face)
    (negative . tessera-glyph-negative-face)
    (neutral . tessera-glyph-neutral-face)
    (positive . tessera-glyph-positive-face)
    (warning . tessera-glyph-warning-face))
  "Map glyph semantics to their faces.")

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
    (when (and (display-graphic-p frame)
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
  (when (and (display-graphic-p frame)
             (tessera--nerd-icons-available-p))
    (let* ((descriptor (tessera-glyph-nerd-icons glyph))
           (function (plist-get descriptor :function))
           (name (plist-get descriptor :name)))
      (when (fboundp function)
        (condition-case nil
            (let ((text (funcall function name)))
              (when (and (stringp text)
                         (> (length text) 0)
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
  "Return the configured color face for GLYPH, or nil."
  (cond
   ((eq tessera-glyph-color t)
    (alist-get (tessera-glyph-semantic glyph)
               tessera--glyph-semantic-faces))
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
  "Apply GLYPH's configured color and metadata to TEXT in CONTEXT."
  (let* ((face (tessera--glyph-color-face glyph))
         (hover-face (and face
                          (tessera--glyph-hover-color-face
                           face context))))
    (put-text-property 0 (length text) 'tessera-glyph-semantic
                       (tessera-glyph-semantic glyph) text)
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

PROPERTIES accepts the interaction keys supported by registered
glyph variants."
  (tessera--validate-glyph glyph "Glyph")
  (unless (tessera-entry-context-p context)
    (error "Glyph context must be a Tessera entry context"))
  (tessera--ensure-plist-keys properties
                              tessera--glyph-variant-properties
                              "Glyph interaction properties")
  (when (plist-member properties :glyph)
    (error "Glyph interaction properties must not contain :glyph"))
  (let ((text (copy-sequence (tessera--glyph-text glyph context))))
    (tessera--apply-glyph-color text glyph context)
    (tessera--apply-glyph-interaction text properties glyph)
    (tessera--add-default-property
     text 'mouse-face 'tessera-entry-hover-face)
    text))

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

(defun tessera--glyph-slots-width (references definition)
  "Return the fixed width of slot REFERENCES in DEFINITION."
  (cl-loop for reference in references
           for name = (tessera--glyph-slot-reference-name reference)
           for slot = (cl-find
                       name
                       (tessera--entry-backend-glyph-slots definition)
                       :key #'tessera-glyph-slot-name)
           sum (tessera-glyph-slot-width slot)))

(defun tessera--render-glyph-slot (slot context)
  "Render glyph SLOT for CONTEXT at its fixed width."
  (let* ((variant-id
          (funcall (tessera-glyph-slot-selector slot) context))
         (variant
          (and variant-id
               (assq variant-id
                     (tessera-glyph-slot-glyphs slot)))))
    (cond
     ((null variant-id)
      (tessera--space (tessera-glyph-slot-width slot)))
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
              (tessera-glyph-render
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
                      (tessera--glyph-slot-padding
                       slot (string-pixel-width text)
                       (* (tessera-glyph-slot-width slot)
                          (frame-char-width frame))))
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
  (let* ((window (tessera-entry-context-window context))
         (frame (and (window-live-p window) (window-frame window))))
    (if (display-graphic-p frame)
        (let ((width
               (if frame
                   (with-selected-frame frame
                     (string-pixel-width rendered))
                 (string-pixel-width rendered))))
          (propertize " " 'display `(space :width (,width))
                      'tessera--layout-space t))
      (tessera--space (tessera-glyph-slot-width slot)))))

(defun tessera--render-glyph-slots
    (references definition context)
  "Render glyph slot REFERENCES using DEFINITION and CONTEXT."
  (mapconcat
   (lambda (reference)
     (let* ((name (tessera--glyph-slot-reference-name reference))
            (slot
             (cl-find name
                      (tessera--entry-backend-glyph-slots definition)
                      :key #'tessera-glyph-slot-name))
            (rendered (tessera--render-glyph-slot slot context)))
       (if (tessera--glyph-slot-reference-reserved-p reference)
           (tessera--reserve-glyph-slot slot rendered context)
         rendered)))
   references
   ""))

(defun tessera--rendered-slots-width
    (references rendered definition context)
  "Measure RENDERED slot REFERENCES from DEFINITION in CONTEXT."
  (let* ((window (tessera-entry-context-window context))
         (frame (and (window-live-p window) (window-frame window))))
    (if (and frame (display-graphic-p frame))
        (with-selected-frame frame
          (ceiling (string-pixel-width rendered) (frame-char-width)))
      (tessera--glyph-slots-width references definition))))

(defun tessera--active-slot-references (references definition context)
  "Omit empty optional REFERENCES in DEFINITION for CONTEXT."
  (cl-remove-if
   (lambda (reference)
     (and (consp reference)
          (plist-get (cdr reference) :optional)
          (let ((slot
                 (cl-find (car reference)
                          (tessera--entry-backend-glyph-slots
                           definition)
                          :key #'tessera-glyph-slot-name)))
            (null (funcall (tessera-glyph-slot-selector slot)
                           context)))))
   references))

(defun tessera--render-slot-group (references definition context)
  "Render inline slot REFERENCES from DEFINITION in CONTEXT."
  (when-let* ((active (tessera--active-slot-references
                       references definition context)))
    (let* ((text (tessera--render-glyph-slots
                  active definition context))
           (width (tessera--rendered-slots-width
                   active text definition context)))
      (tessera--make-rendered-segment
       :string text :width width :target-width width
       :min-width width :max-width width :priority 0 :visible t))))

;;;; Entry rendering

(defun tessera--layout-has-extra-line-p (layout)
  "Return non-nil when LAYOUT defines an extra visual line."
  (or (tessera-entry-layout-extra-glyph-slots layout)
      (tessera-entry-layout-extra-left-segments layout)
      (tessera-entry-layout-extra-right-segments layout)))

(defun tessera--visual-line-break ()
  "Return a logical space displayed as a visual line break."
  (propertize " " 'display "\n"))

(defun tessera--render-line
    (slot-references left-references right-references
                     definition context)
  "Render one visual line from SLOT-REFERENCES and segment references.
LEFT-REFERENCES and RIGHT-REFERENCES name segments in DEFINITION.
CONTEXT supplies their entry data and target window."
  (let* ((window (tessera-entry-context-window context))
         (slot-references (tessera--active-slot-references
                           slot-references definition context))
         (slots (tessera--render-glyph-slots
                 slot-references definition context))
         (slot-width
          (tessera--rendered-slots-width
           slot-references slots definition context))
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
            (if (and slot-references (> (length left-string) 0))
                (tessera--space tessera-entry-segment-gap)
              ""))
           (right-offset
            (+ tessera-entry-safe-gap
               tessera-entry-right-padding
               (tessera--segments-width right)))
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
   definition
   context))

(defun tessera--render-two-line (layout definition context)
  "Render two-line LAYOUT using DEFINITION and CONTEXT."
  (mapconcat
   #'identity
   (list
    (tessera--render-line
     (tessera-entry-layout-main-glyph-slots layout)
     (tessera-entry-layout-main-left-segments layout)
     (tessera-entry-layout-main-right-segments layout)
     definition
     context)
    (tessera--render-line
     (tessera-entry-layout-extra-glyph-slots layout)
     (tessera-entry-layout-extra-left-segments layout)
     (tessera-entry-layout-extra-right-segments layout)
     definition
     context))
   (tessera--visual-line-break)))

(defun tessera--render-entry-lines (layout definition context)
  "Render the visual lines of LAYOUT using DEFINITION and CONTEXT."
  (if (tessera--layout-has-extra-line-p layout)
      (tessera--render-two-line layout definition context)
    (tessera--render-single-line layout definition context)))

(defun tessera--padding-string (height)
  "Return display-only vertical padding of HEIGHT normal lines."
  (when (> height 0)
    (propertize " \n" 'face `((:height ,height) default)
                'line-height t 'mouse-face 'default)))

(defun tessera--entry-content (rendered &optional prefix)
  "Extract RENDERED content and layout after native PREFIX."
  (let ((position 0)
        (length (length rendered))
        (content (copy-sequence (or prefix "")))
        (pending (tessera--padding-string
                  tessera-entry-top-padding))
        placements)
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
    (when (string-empty-p content)
      (setq content (propertize " " 'display '(space :width 0))))
    (when pending
      (push (list (1- (length content)) 'after-string pending)
            placements))
    (put-text-property
     0 1 'tessera--entry-layout
     (list (nreverse placements) tessera-entry-bottom-padding)
     content)
    content))

(defvar-local tessera--current-entry nil
  "Current entry's boundary markers, layout, and saved decorations.")

(defun tessera-entry-clear-current ()
  "Restore the current entry's original faces and decorations."
  (when tessera--current-entry
    (pcase-let ((`(,start ,end ,_layout ,decorations)
                 tessera--current-entry))
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
                  (remove-text-properties position next '(face nil)))
                (remove-text-properties
                 position next '(tessera--current-face nil)))
              (setq position next)))))
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
                (let ((next (next-single-property-change
                             position 'tessera--layout-space
                             original limit)))
                  (when (get-text-property
                         position 'tessera--layout-space original)
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
vertical padding.  Restore the previous entry before moving the
highlight.  This function is suitable for `post-command-hook'."
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
              (let ((next (next-single-property-change
                           position 'face nil end)))
                (put-text-property
                 position next 'tessera--current-face
                 (list (get-text-property position 'face)))
                (add-face-text-property
                 position next 'tessera-entry-current-face)
                (setq position next)))
            (let ((position start))
              (while (< position end)
                (let ((next (next-single-property-change
                             position 'display nil end)))
                  (when (equal (get-text-property position 'display)
                               "\n")
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

(defun tessera-entry-apply-layout (start end)
  "Attach rendered entry layout to buffer content from START to END.
END excludes the client's terminating newline, which must already
exist when bottom padding is requested.  Reapplying replaces only
Tessera overlays within this entry.  Native content properties stay
on buffer text, and all decorative spaces live in overlay strings."
  (let* ((layout (get-text-property start 'tessera--entry-layout))
         (bottom (cadr layout))
         (terminator (eq (char-after end) ?\n))
         (limit (if terminator (1+ end) end)))
    (when (and bottom (> bottom 0) (not terminator))
      (error "Entry bottom padding needs a terminating newline"))
    (tessera-entry-clear-layout start limit)
    (dolist (placement (car layout))
      (pcase-let* ((`(,offset ,property ,string) placement)
                   (position (+ start offset))
                   (overlay (make-overlay position (1+ position))))
        (overlay-put overlay 'tessera-entry-overlay t)
        (overlay-put overlay 'evaporate t)
        ;; Place entry decoration after native boundary headings.
        (overlay-put overlay 'priority 1)
        (overlay-put
         overlay property
         (propertize
          (tessera--add-default-property
           (copy-sequence string) 'face 'default)
          'mouse-face 'default))))
    (when (and bottom (> bottom 0))
      (let ((overlay (make-overlay start limit)))
        (overlay-put overlay 'tessera-entry-overlay t)
        (overlay-put overlay 'evaporate t)
        (overlay-put overlay 'after-string
                     (tessera--padding-string bottom))))))

(defun tessera-entry-render
    (backend object &optional window prefix)
  "Render OBJECT registered for BACKEND in WINDOW.

The result contains one logical line of content and layout metadata.
After inserting it and the native terminating newline, call
`tessera-entry-apply-layout' to display padding and alignment.
WINDOW defaults to a window displaying the current buffer.
PREFIX is optional non-displaying native text placed before content;
it does not participate in width allocation."
  (when (and window (not (window-live-p window)))
    (error "Cannot render an entry for a dead window"))
  (let* ((definition (tessera--find-entry-backend backend))
         (layout (tessera--find-entry-layout definition))
         (target-window (or window
                            (get-buffer-window (current-buffer))))
         (context
          (tessera--make-entry-context
           definition object target-window)))
    (tessera--entry-content
     (tessera--render-entry-lines layout definition context)
     prefix)))

(provide 'tessera)
;;; tessera.el ends here
