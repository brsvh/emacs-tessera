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

;;;; Customization

(defgroup tessera nil
  "Modern interfaces for Emacs communication tools."
  :group 'applications
  :prefix "tessera-")

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

(defface tessera-entry-hover-face
  '((t :inherit highlight))
  "Face used when the pointer is over an entry surface."
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
  "Describe the placement of slots and segments in an entry."
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
       (format "Segment reference `%s'" name)))))

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
        (unless (and (symbolp slot) (memq slot slot-names))
          (error "%s references unknown glyph slot `%s'"
                 description slot))))
    (dolist (segments segment-lists)
      (tessera--ensure-list segments description)
      (dolist (segment segments)
        (tessera--validate-segment-reference segment segment-names)))))

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

;;;; Rendering

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

(defun tessera--ensure-single-line-layout (layout)
  "Ensure LAYOUT contains no extra visual line."
  (when (or (tessera-entry-layout-extra-glyph-slots layout)
            (tessera-entry-layout-extra-left-segments layout)
            (tessera-entry-layout-extra-right-segments layout))
    (error "Layout `%s' requires the two-line renderer"
           tessera-entry-layout)))

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
      (error "Backend `%s' returned an invalid entry context" backend))
    (unless (eq (tessera-entry-context-backend context) backend)
      (error "Entry context names backend `%s', expected `%s'"
             (tessera-entry-context-backend context) backend))
    context))

(defun tessera--space (width)
  "Return a display space occupying WIDTH columns."
  (if (> width 0)
      (propertize " " 'display `(space :width ,width))
    ""))

(defun tessera--align-space (right-offset)
  "Return a display space aligned RIGHT-OFFSET from the right edge."
  (propertize " " 'display `(space :align-to (- right ,right-offset))))

(defun tessera--add-default-property (string property value)
  "Add PROPERTY with VALUE where STRING does not already have it."
  (let ((position 0)
        (end (length string)))
    (while (< position end)
      (let ((next
             (next-single-property-change position property string end)))
        (unless (get-text-property position property string)
          (put-text-property position next property value string))
        (setq position next))))
  string)

(defun tessera--render-segment (reference definition context)
  "Render segment REFERENCE using DEFINITION and CONTEXT."
  (let* ((name (tessera--segment-reference-name reference))
         (provider
          (cdr (assq name
                     (tessera--entry-backend-segments definition))))
         (value (funcall provider context)))
    (unless (or (null value) (stringp value))
      (error "Segment provider `%s' returned `%S'" name value))
    (when (and value (string-match-p "[\n\r]" value))
      (error "Segment provider `%s' returned multiline text" name))
    value))

(defun tessera--render-segments (references definition context)
  "Render REFERENCES using DEFINITION and CONTEXT.
Return a cons of the rendered string and its width in columns."
  (let (strings
        (width 0))
    (dolist (reference references)
      (let ((string
             (tessera--render-segment reference definition context)))
        (when string
          (when strings
            (setq width (+ width tessera-entry-segment-gap)))
          (setq width (+ width (string-width string)))
          (push string strings))))
    (setq strings (nreverse strings))
    (cons (mapconcat #'identity strings
                     (tessera--space tessera-entry-segment-gap))
          width)))

(defun tessera--glyph-slot-padding (slot content-width)
  "Return left and right padding for SLOT and CONTENT-WIDTH."
  (let* ((width (tessera-glyph-slot-width slot))
         (remaining (- width content-width))
         (left
          (pcase (tessera-glyph-slot-align slot)
            ('left 0)
            ('center (/ remaining 2))
            ('right remaining))))
    (cons left (- remaining left))))

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
      (let* ((glyph (plist-get (cdr variant) :glyph))
             (text (tessera-glyph-unicode glyph))
             (content-width (string-width text)))
        (when (> content-width
                 (tessera-glyph-slot-width slot))
          (error "Glyph variant `%s' exceeds slot `%s' width"
                 variant-id (tessera-glyph-slot-name slot)))
        (let ((padding
               (tessera--glyph-slot-padding slot content-width)))
          (concat (tessera--space (car padding))
                  text
                  (tessera--space (cdr padding)))))))))

(defun tessera--render-glyph-slots
    (names definition context)
  "Render glyph slots NAMES using DEFINITION and CONTEXT."
  (mapconcat
   (lambda (name)
     (let ((slot
            (cl-find name (tessera--entry-backend-glyph-slots definition)
                     :key #'tessera-glyph-slot-name)))
       (tessera--render-glyph-slot slot context)))
   names
   ""))

(defun tessera--render-single-line
    (layout definition context)
  "Render single-line LAYOUT using DEFINITION and CONTEXT."
  (let* ((slot-names
          (tessera-entry-layout-main-glyph-slots layout))
         (slots
          (tessera--render-glyph-slots slot-names definition context))
         (left
          (tessera--render-segments
           (tessera-entry-layout-main-left-segments layout)
           definition
           context))
         (right
          (tessera--render-segments
           (tessera-entry-layout-main-right-segments layout)
           definition
           context))
         (slot-gap
          (if (and slot-names (> (length (car left)) 0))
              (tessera--space tessera-entry-segment-gap)
            ""))
         (right-offset
          (+ tessera-entry-safe-gap
             tessera-entry-right-padding
             (cdr right)))
         (surface
          (concat
           (tessera--space tessera-entry-left-padding)
           slots
           slot-gap
           (car left)
           (tessera--space tessera-entry-flex-gap-min-width)
           (tessera--align-space right-offset)
           (car right)
           (tessera--space tessera-entry-right-padding))))
    (tessera--add-default-property surface
                                   'mouse-face
                                   'tessera-entry-hover-face)
    (concat (tessera--space tessera-entry-safe-gap)
            surface
            (tessera--space tessera-entry-safe-gap))))

(defun tessera-entry-render (backend object &optional window)
  "Render OBJECT registered for BACKEND in WINDOW.

Return a propertized string without newline characters.  WINDOW
defaults to a window displaying the current buffer, when one exists."
  (when (and window (not (window-live-p window)))
    (error "Cannot render an entry for a dead window"))
  (let* ((definition (tessera--find-entry-backend backend))
         (layout (tessera--find-entry-layout definition))
         (target-window (or window
                            (get-buffer-window (current-buffer))))
         (context
          (tessera--make-entry-context definition object target-window)))
    (tessera--ensure-single-line-layout layout)
    (tessera--render-single-line layout definition context)))

(provide 'tessera)
;;; tessera.el ends here
