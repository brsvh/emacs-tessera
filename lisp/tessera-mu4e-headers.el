;;; tessera-mu4e-headers.el --- Mu4e header layouts  -*- lexical-binding: t; -*-

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

;; Reversible thread and entry layouts over native mu4e rows.
;; The native docid cookie, mark fringe, and message identity remain.

;;; Code:

(require 'cl-lib)
(require 'hl-line)
(require 'subr-x)
(require 'tessera-mu4e)
(require 'tessera-mu4e-thread)

(defvar mu4e-search-threads)
(defvar mu4e-headers-visible-flags)
(defvar mu4e-headers-show-target)
(defvar mu4e-headers-date-format)
(defvar mu4e-headers-time-format)
(defvar mu4e--mark-map)
(defvar mu4e--mark-fringe-len)
(defvar mu4e~headers-docid-pre)
(defvar mu4e~headers-docid-post)
(defvar mu4e~end-of-results)

(declare-function mu4e-personal-address-p "mu4e-contacts")
(declare-function mu4e~headers-human-date "mu4e-headers")
(declare-function mu4e-mark-at-point "mu4e-mark")
(declare-function mu4e~headers-apply-flags "mu4e-headers")
(declare-function mu4e~headers-thread-root-p "mu4e-headers")
(declare-function mu4e-get-headers-buffer "mu4e-window")
(declare-function mu4e~headers-docid-at-point "mu4e-headers")
(declare-function mu4e~headers-field-for-docid "mu4e-headers")

;;;; Message state and face composition

(defun tessera-mu4e-headers--unread-p (message)
  "Return non-nil when native MESSAGE flags indicate unread or new."
  (let ((flags (plist-get message :flags)))
    (or (memq 'unread flags) (memq 'new flags))))

(defun tessera-mu4e-headers--styled-text (role message text &optional
                                               thread)
  "Return TEXT styled for ROLE and native MESSAGE without mutation.
ROLE is subject, contact, date, or label.  Subject styling preserves
mu4e's native state evaluator, including theme-supplied attributes.
Contacts and dates depend only on unread state outside THREAD.
THREAD is an optional shared thread context; its subject uses the
aggregate unread state, while contacts retain native message state."
  (let* ((result (copy-sequence text))
         (unread (tessera-mu4e-headers--unread-p message))
         (face
          (pcase role
            ('subject
             (if thread
                 (if (> (tessera-thread-context-unread thread) 0)
                     'tessera-mu4e-headers-thread-unread-subject-face
                   'tessera-mu4e-headers-thread-subject-face)
               (if unread 'tessera-mu4e-headers-unread-subject-face
                 'tessera-mu4e-headers-subject-face)))
            ('contact
             (if thread
                 (if unread
                     'tessera-mu4e-headers-thread-unread-contact-face
                   'tessera-mu4e-headers-thread-contact-face)
               (if unread 'tessera-mu4e-headers-unread-contact-face
                 'tessera-mu4e-headers-read-contact-face)))
            ('date (if unread
                       'tessera-mu4e-headers-unread-date-face
                     'tessera-mu4e-headers-date-face))
            ('label 'tessera-mu4e-headers-label-face)
            (_ (error "Unknown mu4e face role: %S" role)))))
    ;; A previously styled string must not carry an old state forward.
    (remove-text-properties 0 (length result) '(face nil) result)
    (when (or (and (eq role 'subject) (null thread))
              (and (eq role 'contact) thread))
      (require 'mu4e-headers)
      (mu4e~headers-apply-flags message result))
    (add-face-text-property 0 (length result) face nil result)
    result))

(defun tessera-mu4e-headers--glyph-face (slot variant)
  "Return the face for glyph VARIANT in SLOT.
Pending operation marks take precedence over matching flag names."
  (let ((role
         (if (eq slot 'operation)
             'operation
           (pcase variant
             ('seen 'read)
             ('passed 'forwarded)
             ('high 'high-priority)
             ('low 'low-priority)
             ('attach 'attachment)
             ('signed 'signature)
             ('encrypted 'encryption)
             ((or 'new 'unread 'draft 'trashed 'flagged 'replied
                  'list 'personal 'calendar)
              variant)
             (_ (error "Unknown mu4e glyph variant: %S" variant))))))
    (intern (format "tessera-mu4e-headers-%s-face" role))))

;;;; Glyphs and header fields

(defconst tessera-mu4e-headers--icons
  '((status
     (trashed "T" "⌫" "trash-can-outline" "Trashed")
     (draft "D" "✎" "email-edit-outline" "Draft")
     (new "N" "✦" "new-box" "New")
     (unread "u" "●" "email" "Unread")
     (seen "S" "○" "email-open-outline" "Read"))
    (priority
     (high "H" "↑" "arrow-up-bold" "High priority")
     (flagged "F" "★" "star" "Flagged")
     (low "L" "↓" "arrow-down-bold" "Low priority"))
    (operation
     (move "m" "→" "folder-move-outline" "Move")
     (refile "r" "↧" "archive-arrow-down-outline" "Refile")
     (trash "d" "⌫" "trash-can-outline" "Trash action")
     (untrash "=" "↶" "restore" "Untrash action")
     (delete "D" "×" "delete-forever-outline" "Delete action")
     (flag "+" "☆" "star-plus-outline" "Flag action")
     (unflag "-" "⊖" "star-minus-outline" "Unflag action")
     (read "!" "○" "email-open-outline" "Read action")
     (mark-unread "?" "●" "email" "Unread action")
     (label "l" "+" "tag-plus-outline" "Add/remove labels")
     (unlabel "L" "−" "tag-remove-outline" "Clear labels")
     (action "a" "▶" "play-circle-outline" "Custom action")
     (something "*" "◆" "clipboard-clock-outline" "Deferred"))
    (secondary
     (replied "R" "↩" "reply" "Replied")
     (passed "P" "↪" "forward" "Forwarded")
     (personal "p" "♙" "account-outline" "Personal")
     (list "l" "≡" "format-list-bulleted" "Mailing list"))
    (attach
     (attach "a" "📎" "paperclip" "Attachment present"))
    (signed
     (signed "S" "✍" "file-sign" "Signed; not verified"))
    (encrypted
     (encrypted "E" "🔒" "lock-outline" "Encrypted"))
    (calendar
     (calendar "c" "▦" "calendar" "Calendar invitation")))
  "Glyph variants grouped by their native message or action slot.")

(defconst tessera-mu4e-headers--auxiliary
  '((status trashed draft)
    (priority flagged)
    (secondary replied passed personal list))
  "Optional flags grouped by semantic slot, in display priority order.
These flags follow `mu4e-headers-visible-flags'.")

(defvar-local tessera-mu4e-headers--active nil
  "Whether presentation synchronization is enabled.")
(defvar-local tessera-mu4e-headers--updating nil
  "Whether Tessera is currently modifying the buffer.")
(defvar-local tessera-mu4e-headers--dirty t
  "Whether native edits require another render.")
(defvar-local tessera-mu4e-headers--appearance nil
  "Last rendered appearance settings.")
(defvar-local tessera-mu4e-headers--leading-width 0
  "Common icon prefix width for the current result set.")
(defvar-local tessera-mu4e-headers--native-header-line nil
  "Native column header to restore when disabling Tessera.")
(defvar-local tessera-mu4e-headers--saved-settings nil
  "Snapshot of layout and logical navigation settings.")
(defvar-local tessera-mu4e-headers--saved-hl-line nil
  "Whether native line highlighting was enabled.")

;;;; Thread contexts from native result rows

(defvar-local tessera-mu4e-headers--threads nil
  "Native docids mapped to shared thread contexts.")

(defun tessera-mu4e-headers--thread-context (message)
  "Return the shared thread context for native MESSAGE."
  (when (and mu4e-search-threads
             tessera-mu4e-headers--threads)
    (gethash (plist-get message :docid)
             tessera-mu4e-headers--threads)))

(defun tessera-mu4e-headers--build-threads ()
  "Build contexts using native root boundaries and display levels.
The first hidden row represents the visible native fold summary
for padding purposes.  Threading follows `mu4e-search-threads'."
  (let (entries stack representative)
    (when mu4e-search-threads
      (save-excursion
        (goto-char (point-min))
        (while (< (point) (point-max))
          (when-let* ((message (get-text-property (point) 'msg))
                      (id (plist-get message :docid))
                      ;; The footer may inherit the preceding msg.
                      (_ (looking-at
                          (regexp-quote mu4e~headers-docid-pre))))
            (let* ((meta (plist-get message :meta))
                   (level (or (plist-get meta :level) 0))
                   (fold (tessera-mu4e-thread-fold-at (point))))
              (when (or (= level 0) (null representative)
                        (mu4e~headers-thread-root-p message))
                (setq representative nil stack nil))
              (while (and stack (>= (caar stack) level))
                (pop stack))
              (push (list id (or (cdar stack) representative)
                          (tessera-mu4e-headers--unread-p message)
                          (or (null fold)
                              (= (point) (overlay-start fold))))
                    entries)
              (unless representative (setq representative id))
              (push (cons level id) stack)))
          (forward-line 1))))
    (setq tessera-mu4e-headers--threads
          (tessera-thread-build-contexts (nreverse entries)))))

;;;; Header elements

(defun tessera-mu4e-headers--context (object buffer window)
  "Build a context for native OBJECT in BUFFER and WINDOW."
  (make-tessera-entry-context
   :backend 'mu4e-headers :object object
   :buffer buffer :window window
   :thread (tessera-mu4e-headers--thread-context object)))

(defun tessera-mu4e-headers--mark (message)
  "Return the native pending mark and target for MESSAGE."
  (when (hash-table-p mu4e--mark-map)
    (gethash (plist-get message :docid) mu4e--mark-map)))

(defun tessera-mu4e-headers--states (slot context)
  "Return active states of SLOT in CONTEXT, ordered by priority."
  (let* ((message (tessera-entry-context-object context))
         (flags (plist-get message :flags))
         (priority (plist-get message :priority))
         (optional
          (cl-remove-if-not
           (lambda (flag)
             (and (memq flag flags)
                  (memq flag mu4e-headers-visible-flags)))
           (cdr (assq slot tessera-mu4e-headers--auxiliary)))))
    (pcase slot
      ('status
       (append optional
               (or (append (when (memq 'new flags) '(new))
                           (when (memq 'unread flags) '(unread)))
                   '(seen))))
      ('priority
       (append (when (eq priority 'high) '(high)) optional
               (when (eq priority 'low) '(low))))
      ('secondary optional)
      ('operation
       (when-let* ((mark (car (tessera-mu4e-headers--mark message))))
         (list (if (eq mark 'unread) 'mark-unread mark))))
      (_ (and (memq slot flags)
              (memq slot mu4e-headers-visible-flags) (list slot))))))

(defun tessera-mu4e-headers--state (slot context)
  "Select the highest-priority state of SLOT in CONTEXT."
  (car (tessera-mu4e-headers--states slot context)))

(defun tessera-mu4e-headers--help
    (slot label window object position)
  "Describe SLOT at POSITION in OBJECT or WINDOW, defaulting to LABEL.
Include all active states hidden by the selected icon, respecting
visibility settings.  Pending operations also show their target."
  (let ((buffer (if (bufferp object) object
                  (and (window-live-p window)
                       (window-buffer window)))))
    (if (and (buffer-live-p buffer)
             (integer-or-marker-p position))
        (with-current-buffer buffer
          (if-let* ((message (get-text-property position 'msg)))
              (if (eq slot 'operation)
                  (let ((mark (tessera-mu4e-headers--mark message)))
                    (if (cdr mark)
                        (format "%s: %s" label (cdr mark)) label))
                (mapconcat
                 (lambda (state)
                   (nth 4 (assq state
                                (cdr (assq
                                      slot
                                      tessera-mu4e-headers--icons)))))
                 (tessera-mu4e-headers--states
                  slot (tessera-mu4e-headers--context
                        message buffer window))
                 "; "))
            label))
      label)))

(defun tessera-mu4e-headers--glyph (spec)
  "Make the shared renderer glyph for SPEC."
  (make-tessera-glyph
   :ascii (nth 1 spec) :unicode (nth 2 spec)
   :nerd-icons
   (list :function 'nerd-icons-mdicon
         :name (concat "nf-md-"
                       (replace-regexp-in-string "-" "_"
                                                 (nth 3 spec))))
   :semantic 'neutral))

(defun tessera-mu4e-headers--slot (name)
  "Make semantic or content glyph slot NAME."
  (make-tessera-glyph-slot
   :name name :width 2 :align 'center
   :selector (apply-partially #'tessera-mu4e-headers--state name)
   :glyphs
   (mapcar
    (lambda (spec)
      (list (car spec) :glyph (tessera-mu4e-headers--glyph spec)
            :face (tessera-mu4e-headers--glyph-face name (car spec))
            :help-echo
            (if (memq name '(status priority secondary operation))
                (apply-partially #'tessera-mu4e-headers--help
                                 name (nth 4 spec))
              (nth 4 spec))))
    (cdr (assq name tessera-mu4e-headers--icons)))))

(defun tessera-mu4e-headers--contact (message)
  "Return contact names for MESSAGE, falling back to email addresses.
Use recipients for personal outgoing mail, as native mu4e does."
  (let* ((from (plist-get message :from))
         (address (plist-get (car from) :email))
         (contacts (if (and address (mu4e-personal-address-p address))
                       (plist-get message :to)
                     from)))
    (if contacts
        (mapconcat
         (lambda (contact)
           (let ((name (plist-get contact :name)))
             (or (and name (not (string-empty-p name)) name)
                 (plist-get contact :email) "?")))
         contacts ", ")
      "?")))

(defun tessera-mu4e-headers--overflow-help (window object position)
  "Describe the clipped message at POSITION in OBJECT or WINDOW."
  (when-let* ((buffer (if (bufferp object) object
                        (and (window-live-p window)
                             (window-buffer window)))))
    (with-current-buffer buffer
      (when-let* ((message (get-text-property position 'msg))
                  (node (tessera-mu4e-headers--thread-context
                         message)))
        (let* ((parent (tessera-thread-context-parent node))
               (from (and parent
                          (mu4e~headers-field-for-docid
                           parent :from))))
          (format
           "%s\n%s\nDepth: %d\nReply to: %s"
           (tessera-mu4e-headers--contact message)
           (plist-get message :subject)
           (length (tessera--thread-path-tail node))
           (if from
               (format "%s (#%s)"
                       (tessera-mu4e-headers--contact
                        (list :from from)) parent)
             (or parent "Root"))))))))

(defun tessera-mu4e-headers--field (role context)
  "Return a native message element for ROLE in CONTEXT."
  (let* ((message (tessera-entry-context-object context))
         (thread (tessera-entry-context-thread context)))
    (pcase role
      ('thread-tree
       (when-let* ((text (tessera-thread-prefix context)))
         (propertize text
                     'face 'tessera-mu4e-headers-thread-tree-face
                     'tessera--overflow-help
                     #'tessera-mu4e-headers--overflow-help
                     'mouse-face 'tessera-entry-hover-face)))
      ('thread-count
       (when-let* ((text (tessera-thread-count context)))
         (propertize
          text 'mouse-face 'tessera-entry-hover-face
          'face (if (> (tessera-thread-context-unread thread) 0)
                    'tessera-mu4e-headers-thread-unread-count-face
                  'tessera-mu4e-headers-thread-count-face))))
      ('labels
       (mapconcat
        (lambda (label)
          (propertize
           (tessera-mu4e-headers--styled-text 'label message label)
           'help-echo
           (format "%s (%s)" label
                   (string-join
                    (append
                     (when (member label (plist-get message :labels))
                       '("label"))
                     (when (member label (plist-get message :tags))
                       '("tag"))) ", "))
           'mouse-face 'tessera-entry-hover-face))
        (delete-dups (append (plist-get message :labels)
                             (plist-get message :tags) nil))
        (propertize "," 'face 'default 'mouse-face 'default)))
      ('target
       (let ((mark (tessera-mu4e-headers--mark message)))
         (when (and mu4e-headers-show-target (cdr mark))
           (propertize (format "→ %s" (cdr mark))
                       'face 'tessera-mu4e-headers-operation-face
                       'help-echo
                       (format "%s: %s" (car mark) (cdr mark))
                       'mouse-face 'tessera-entry-hover-face))))
      (_
       (let* ((text
               (pcase role
                 ('subject (let ((subject
                                  (plist-get message :subject)))
                             (if (or (null subject)
                                     (string-empty-p subject))
                                 "(no subject)" subject)))
                 ('contact (tessera-mu4e-headers--contact message))
                 ('date (mu4e~headers-human-date message))))
              (help (if (eq role 'contact)
                        (format "From: %S\nTo: %S"
                                (plist-get message :from)
                                (plist-get message :to))
                      text)))
         (propertize (tessera-mu4e-headers--styled-text
                      role message text thread)
                     'help-echo help
                     'mouse-face 'tessera-entry-hover-face))))))

;;;; Layout registration

(defun tessera-mu4e-headers--prefix (kind extra)
  "Return semantic slots for KIND and EXTRA line."
  (cond
   ((eq kind 'thread) '(priority operation secondary status))
   ((eq kind 'single-line) '(operation priority secondary status))
   (extra '(priority secondary))
   (t '(operation status))))

(defun tessera-mu4e-headers--width (_context)
  "Return the common prefix width for the current result set."
  tessera-mu4e-headers--leading-width)

(defun tessera-mu4e-headers--register ()
  "Register the native mu4e headers backend."
  (let* ((content '(:slots (attach :optional t) (signed :optional t)
                           (encrypted :optional t)
                           (calendar :optional t)))
         (subject '(subject :grow t :min-width 4 :truncate tail))
         (entry-subject (append subject '(:point t)))
         (target '(target :grow t :max-width 24 :min-width 0
                          :truncate tail :optional t))
         (labels '(labels :grow t :max-width 24 :min-width 0
                          :truncate tail :priority -1 :optional t)))
    (tessera-entry-register
     'mu4e-headers :context #'tessera-mu4e-headers--context
     :segments
     (mapcar (lambda (role)
               (cons role (apply-partially
                           #'tessera-mu4e-headers--field role)))
             '(subject contact date labels target
                       thread-tree thread-count))
     :glyph-slots
     (mapcar (lambda (spec)
               (tessera-mu4e-headers--slot (car spec)))
             tessera-mu4e-headers--icons)
     :thread-layout
     (let ((leading (tessera-mu4e-headers--prefix 'thread nil))
           (left (list 'thread-tree
                       '(contact :grow t :min-width 4 :truncate tail
                                 :point t)
                       content target))
           (right (list labels 'date)))
       (make-tessera-thread-layout
        :head
        (make-tessera-entry-layout
         :glyph-slots-align 'right
         :leading-width #'tessera-mu4e-headers--width
         :main-leading-segments '(thread-count)
         :main-left-segments (list subject)
         :extra-glyph-slots leading
         :extra-left-segments left :extra-right-segments right)
        :child
        (make-tessera-entry-layout
         :glyph-slots-align 'right
         :leading-width #'tessera-mu4e-headers--width
         :main-glyph-slots leading
         :main-left-segments left :main-right-segments right)))
     :layouts
     (list
      (cons 'single-line
            (make-tessera-entry-layout
             :glyph-slots-align 'right
             :leading-width #'tessera-mu4e-headers--width
             :main-glyph-slots
             (tessera-mu4e-headers--prefix 'single-line nil)
             :main-left-segments (list entry-subject target content)
             :main-right-segments
             (list labels '(contact :max-width 20 :truncate tail
                                    :optional t) 'date)))
      (cons 'two-line
            (make-tessera-entry-layout
             :glyph-slots-align 'right
             :leading-width #'tessera-mu4e-headers--width
             :main-glyph-slots
             (tessera-mu4e-headers--prefix 'two-line nil)
             :extra-glyph-slots
             (tessera-mu4e-headers--prefix 'two-line t)
             :main-left-segments (list entry-subject target)
             :main-right-segments (list labels)
             :extra-left-segments
             (list '(contact :grow t :min-width 4 :truncate tail)
                   content)
             :extra-right-segments '(date)))))))

;;;; Native row synchronization

(defun tessera-mu4e-headers--body-start ()
  "Return the native body start on this logical line, or nil."
  (save-excursion
    (goto-char (line-beginning-position))
    (when (and (looking-at (regexp-quote mu4e~headers-docid-pre))
               (search-forward mu4e~headers-docid-post
                               (line-end-position) t))
      (+ (point) mu4e--mark-fringe-len))))

(defun tessera-mu4e-headers--measure ()
  "Measure registered semantic slots, enlarged for thread counts."
  (let ((width 0)
        (kind (if mu4e-search-threads 'thread tessera-entry-layout))
        (slots (tessera--entry-backend-glyph-slots
                (tessera--find-entry-backend 'mu4e-headers))))
    (dolist (extra (if (eq kind 'two-line) '(nil t) '(nil)))
      (setq width
            (max width
                 (cl-loop
                  for name in
                  (tessera-mu4e-headers--prefix kind extra)
                  sum (tessera-glyph-slot-width
                       (cl-find name slots
                                :key #'tessera-glyph-slot-name))))))
    (when mu4e-search-threads
      (maphash
       (lambda (_id thread)
         (when (tessera-thread-context-first thread)
           (let ((count
                  (format "%d/%d"
                          (tessera-thread-context-unread thread)
                          (tessera-thread-context-total thread))))
             (setq width (max width (length count))))))
       tessera-mu4e-headers--threads))
    width))

(defun tessera-mu4e-headers--sync-line (native &optional force)
  "Render the changed row, or restore it when NATIVE is non-nil.
FORCE also redraws rows whose message and thread state are unchanged."
  (when-let* ((body (tessera-mu4e-headers--body-start)))
    (let* ((start (line-beginning-position))
           (end (line-end-position))
           (fringe (- body mu4e--mark-fringe-len))
           (message (get-text-property start 'msg))
           (saved (get-text-property body 'tessera-mu4e-native))
           (original (or saved (buffer-substring body end)))
           (state (unless native
                    (list message (tessera-mu4e-headers--mark message)
                          (tessera-thread-context-key
                           (tessera-mu4e-headers--thread-context
                            message))))))
      (when (if native saved
              (or force (not saved)
                  ;; Reapplying a mark replaces its hidden text too.
                  (not (equal (get-text-property fringe 'display) ""))
                  (not (equal state (get-text-property
                                     body 'tessera-mu4e-state)))))
        ;; Native mark edits can move old padding into the fringe.
        ;; Clear the whole row so no clipped decoration survives.
        (tessera-entry-clear-layout start (1+ end))
        (remove-overlays start (1+ end) 'mu4e-mark t)
        (delete-region body end)
        (goto-char body)
        (if native
            (progn
              (insert original)
              (remove-text-properties
               start (1+ (point))
               '(tessera-mu4e-native nil
                                     tessera-mu4e-state nil
                                     tessera--entry-layout nil
                                     tessera--layout-overlay nil))
              (remove-text-properties fringe body '(display nil)))
          (let ((text (tessera-entry-render 'mu4e-headers message)))
            (put-text-property 0 (length text)
                               'tessera-mu4e-native original text)
            (put-text-property 0 (length text) 'tessera-mu4e-state
                               (list (copy-tree (car state))
                                     (copy-tree (cadr state))
                                     (caddr state))
                               text)
            (insert text)
            (put-text-property
             start body 'tessera--entry-layout
             (get-text-property body 'tessera--entry-layout))
            (put-text-property fringe body 'display "")))
        (add-text-properties
         start (1+ (point))
         (list 'docid (plist-get message :docid) 'msg message))
        (when native
          (when-let* ((mark (tessera-mu4e-headers--mark message)))
            (goto-char start)
            (mu4e-mark-at-point (car mark) (cdr mark)))))
      (unless native
        ;; Even unchanged rows must share this generation's paths.
        ;; Only native messages and marks need mutable-data copies.
        (when-let* ((snapshot (get-text-property
                               body 'tessera-mu4e-state)))
          (setf (nth 2 snapshot) (nth 2 state)))
        (if (and mu4e-search-threads
                 (tessera-mu4e-thread-fold-at start))
            (when (tessera-entry-layout-applied-p body)
              (tessera-entry-clear-layout
               body (1+ (line-end-position))))
          (unless (tessera-entry-layout-applied-p body)
            (tessera-entry-apply-layout
             body (line-end-position))))))))

(defun tessera-mu4e-headers--hide-footer ()
  "Hide the native end-of-results notice with a removable overlay."
  (save-excursion
    (goto-char (point-max))
    (let ((start (line-beginning-position)))
      (when (and (eq (get-text-property start 'face)
                     'mu4e-system-face)
                 (equal (buffer-substring-no-properties
                         start (point-max)) mu4e~end-of-results))
        (let ((overlay (make-overlay start (point-max))))
          (overlay-put overlay 'tessera-mu4e-footer t)
          (overlay-put overlay 'display ""))))))

(defun tessera-mu4e-headers--sync (native &optional force)
  "Synchronize the result buffer, using NATIVE presentation if set.
FORCE also redraws unchanged messages after presentation changes."
  (let ((tessera-mu4e-headers--updating t)
        (inhibit-read-only t)
        (inhibit-modification-hooks t)
        (saved-point (tessera-entry-save-point)))
    (unwind-protect
        (progn
          (tessera-entry-clear-current)
          (when native (tessera-entry-clear-layout))
          (remove-overlays nil nil 'tessera-mu4e-footer t)
          (unless native
            (tessera-mu4e-headers--build-threads)
            (let ((width (tessera-mu4e-headers--measure)))
              (unless (= width tessera-mu4e-headers--leading-width)
                (setq force t))
              (setq tessera-mu4e-headers--leading-width width)))
          (goto-char (point-min))
          (while (< (point) (point-max))
            (tessera-mu4e-headers--sync-line native force)
            (forward-line 1))
          (when tessera-mu4e-headers--active
            (if mu4e-search-threads
                (tessera-mu4e-thread-pad-folds
                 tessera-mu4e-headers--threads)
              (remove-overlays nil nil 'tessera-mu4e-fold-padding t))
            (tessera-mu4e-headers--hide-footer)))
      (tessera-entry-restore-point saved-point))))

(defun tessera-mu4e-headers--appearance ()
  "Return native and shared options affecting the presentation."
  (list (when-let* ((window (get-buffer-window (current-buffer))))
          (window-body-width window))
        mu4e-search-threads
        tessera-thread-outer-top-padding
        tessera-thread-outer-bottom-padding
        tessera-thread-inner-top-padding
        tessera-thread-inner-bottom-padding
        mu4e-headers-visible-flags
        mu4e-headers-show-target
        mu4e-headers-date-format mu4e-headers-time-format
        custom-enabled-themes tessera-entry-layout
        tessera-glyph-style tessera-glyph-color
        tessera-entry-safe-gap tessera-entry-left-padding
        tessera-entry-right-padding tessera-entry-top-padding
        tessera-entry-bottom-padding tessera-entry-segment-gap
        tessera-entry-flex-gap-min-width))

(defun tessera-mu4e-headers--changed (&rest _arguments)
  "Record native buffer changes without reacting to our own edits."
  (unless tessera-mu4e-headers--updating
    (tessera-mu4e-thread-invalidate)
    (setq tessera-mu4e-headers--dirty t)))

(defun tessera-mu4e-headers--refresh (&optional _window)
  "Synchronize changed messages before display or after a command."
  (when (and tessera-mu4e-headers--active
             (not tessera-mu4e-headers--updating))
    (let* ((tessera-mu4e-headers--updating t)
           (appearance (tessera-mu4e-headers--appearance))
           (force (not (equal appearance
                              tessera-mu4e-headers--appearance))))
      (when (or tessera-mu4e-headers--dirty force)
        (tessera-mu4e-headers--sync nil force)
        (setq tessera-mu4e-headers--dirty nil
              tessera-mu4e-headers--appearance appearance))
      (when header-line-format
        (setq tessera-mu4e-headers--native-header-line
              header-line-format))
      (setq header-line-format nil)
      (when hl-line-mode (hl-line-mode -1))
      (if (tessera-mu4e-thread-fold-at (point))
          (tessera-entry-clear-current)
        (tessera-entry-highlight-current)))))

;;;; Native navigation

(defun tessera-mu4e-headers--position-point ()
  "Place point at the selected message's layout anchor.
Use the subject in flat layouts and the contact in thread layouts.
Resolve the related headers buffer when called from a message view
or a native search hook.  Preserve folded rows and native selection."
  (when-let* ((buffer (mu4e-get-headers-buffer)))
    (with-current-buffer buffer
      (when tessera-mu4e-headers--active
        (tessera-mu4e-headers--refresh)
        (when-let* ((_ (not (tessera-mu4e-thread-fold-at (point))))
                    (position (tessera-entry-point)))
          (goto-char position)
          (dolist (window (get-buffer-window-list buffer nil t))
            (set-window-point window position)))))))

(defun tessera-mu4e-headers--moved (result)
  "Normalize a successful native navigation RESULT's layout anchor.
Return RESULT unchanged, including native docids and positions."
  (when (numberp result)
    (tessera-mu4e-headers--position-point))
  result)

(defun tessera-mu4e-headers--command (function &rest arguments)
  "Normalize interactive navigation by FUNCTION with ARGUMENTS.
Thread commands also serve native fold-boundary calculations;
those noninteractive calls must retain their original positions."
  (let ((interactive (called-interactively-p 'any)))
    (prog1 (apply function arguments)
      (when interactive (tessera-mu4e-headers--position-point)))))

(defun tessera-mu4e-headers--update (function &rest arguments)
  "Preserve a layout anchor across native FUNCTION with ARGUMENTS.
Native updates replace rows and restore a column.  Follow the old
message only while it remains selected after the native update."
  (let* ((buffer (mu4e-get-headers-buffer))
         (docid
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (when (and tessera-mu4e-headers--active
                         (get-text-property
                          (point) 'tessera-entry-point))
                (mu4e~headers-docid-at-point))))))
    (prog1 (apply function arguments)
      (when (and docid (buffer-live-p buffer))
        (with-current-buffer buffer
          (when (eql docid (mu4e~headers-docid-at-point))
            (tessera-mu4e-headers--position-point)))))))

(defun tessera-mu4e-headers--navigation (enable)
  "Install navigation integration when ENABLE is non-nil, or remove."
  (tessera-mu4e-thread-track enable)
  (dolist (function '(mu4e~headers-move
                      mu4e~headers-prev-or-next-unread
                      mu4e-headers-goto-message-id))
    (if enable
        (advice-add function :filter-return
                    #'tessera-mu4e-headers--moved)
      (advice-remove function #'tessera-mu4e-headers--moved)))
  (dolist (function '(mu4e-headers-prev-thread
                      mu4e-headers-next-thread
                      mu4e-thread-goto-root
                      mu4e-thread-fold-goto-next
                      mu4e-thread-unfold-goto-next
                      mu4e-thread-fold-toggle-goto-next
                      mu4e-view-headers-prev-thread
                      mu4e-view-headers-next-thread))
    (if enable
        (advice-add function :around
                    #'tessera-mu4e-headers--command)
      (advice-remove function #'tessera-mu4e-headers--command)))
  (if enable
      (progn
        (advice-add 'mu4e~headers-update-handler :around
                    #'tessera-mu4e-headers--update)
        (add-hook 'mu4e-headers-found-hook
                  #'tessera-mu4e-headers--position-point t))
    (advice-remove 'mu4e~headers-update-handler
                   #'tessera-mu4e-headers--update)
    (remove-hook 'mu4e-headers-found-hook
                 #'tessera-mu4e-headers--position-point)))

;;;; Adapter lifecycle

(defun tessera-mu4e-headers--enable ()
  "Enable reversible layout synchronization in this headers buffer."
  (unless tessera-mu4e-headers--active
    (setq tessera-mu4e-headers--active t
          tessera-mu4e-headers--dirty t
          tessera-mu4e-headers--native-header-line header-line-format
          tessera-mu4e-headers--saved-hl-line hl-line-mode
          tessera-mu4e-headers--saved-settings
          (tessera--save-settings
           '(tessera-entry-layout line-move-ignore-invisible)))
    (setq-local tessera-entry-layout 'two-line)
    ;; Mu4e skips folded messages itself.  Its logical line motion
    ;; must not stop at the visual newlines in padding overlays.
    (setq-local line-move-ignore-invisible nil)
    (tessera-mu4e-thread-invalidate)
    (add-hook 'after-change-functions
              #'tessera-mu4e-headers--changed nil t)
    (add-hook 'tessera-mu4e-thread-change-hook
              #'tessera-mu4e-headers--changed nil t)
    (add-hook 'post-command-hook
              #'tessera-mu4e-headers--refresh t t)
    (add-hook 'pre-redisplay-functions
              #'tessera-mu4e-headers--refresh nil t)
    (add-hook 'change-major-mode-hook
              #'tessera-mu4e-headers--disable nil t)
    (tessera-mu4e-headers--navigation t)
    (tessera-mu4e-headers--refresh)
    (tessera-mu4e-headers--position-point)))

(defun tessera-mu4e-headers--disable ()
  "Restore native text, marks, highlight, and header line."
  (when tessera-mu4e-headers--active
    (setq tessera-mu4e-headers--active nil)
    (remove-hook 'after-change-functions
                 #'tessera-mu4e-headers--changed t)
    (remove-hook 'tessera-mu4e-thread-change-hook
                 #'tessera-mu4e-headers--changed t)
    (remove-hook 'post-command-hook
                 #'tessera-mu4e-headers--refresh t)
    (remove-hook 'pre-redisplay-functions
                 #'tessera-mu4e-headers--refresh t)
    (remove-hook 'change-major-mode-hook
                 #'tessera-mu4e-headers--disable t)
    (tessera-mu4e-headers--sync t)
    (setq header-line-format tessera-mu4e-headers--native-header-line)
    (hl-line-mode (if tessera-mu4e-headers--saved-hl-line 1 -1))
    (tessera--restore-settings tessera-mu4e-headers--saved-settings)
    (unless (seq-some
             (lambda (buffer)
               (buffer-local-value
                'tessera-mu4e-headers--active buffer))
             (buffer-list))
      (tessera-mu4e-headers--navigation nil))))

(provide 'tessera-mu4e-headers)
;;; tessera-mu4e-headers.el ends here
