;;; tessera-mu4e-headers.el --- Tessera mu4e Headers UI  -*- lexical-binding: t; -*-

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

;; Two-row flat-entry presentation for native mu4e Headers buffers.
;; The native one-message-per-line buffer structure remains intact.

;;; Code:

(require 'cl-lib)
(require 'mu4e-headers)
(require 'mu4e-context)
(require 'mu4e-message)
(require 'seq)
(require 'tessera-mu4e)
(require 'tessera-mu4e-status)
(require 'tessera-ui)

(declare-function mu4e-server-last-query "mu4e-server")

(defface tessera-mu4e-headers-glyph
  '((t :inherit tessera-ui-glyph))
  "Face used when mu4e Headers glyph colors are uniform."
  :group 'tessera-mu4e)

(defface tessera-mu4e-headers-glyph-attention
  '((t :inherit tessera-ui-glyph-attention))
  "Face for mu4e Headers glyphs that need attention."
  :group 'tessera-mu4e)

(defface tessera-mu4e-headers-glyph-error
  '((t :inherit tessera-ui-glyph-error))
  "Face for erroneous mu4e Headers states."
  :group 'tessera-mu4e)

(defface tessera-mu4e-headers-glyph-workflow
  '((t :inherit tessera-ui-glyph-workflow))
  "Face for mu4e Headers workflow glyphs."
  :group 'tessera-mu4e)

(defface tessera-mu4e-headers-glyph-availability
  '((t :inherit tessera-ui-glyph-availability))
  "Face for mu4e Headers availability glyphs."
  :group 'tessera-mu4e)

(defface tessera-mu4e-headers-glyph-security
  '((t :inherit tessera-ui-glyph-security))
  "Face for mu4e Headers security glyphs."
  :group 'tessera-mu4e)

(defvar tessera-mu4e-headers--enabled-p nil
  "Non-nil when the Tessera mu4e Headers interface is enabled.")

(defvar-local tessera-mu4e-headers--installed-p nil
  "Non-nil when Tessera owns the current Headers presentation.")

(defun tessera-mu4e-headers--set-option (symbol value)
  "Set Headers option SYMBOL to VALUE and refresh mu4e buffers."
  (set-default symbol value)
  (tessera-mu4e--refresh-buffers))

(defcustom tessera-mu4e-headers-label-limit 4
  "Maximum number of labels displayed in one Headers entry.

When more labels are present, append `+'.  A value of nil displays
every label, while zero displays only the overflow marker."
  :type '(choice
          (const :tag "No limit" nil)
          (natnum :tag "Maximum labels"))
  :set #'tessera-mu4e-headers--set-option
  :group 'tessera-mu4e)

(defun tessera-mu4e-headers--set-month-option (symbol value)
  "Set month grouping option SYMBOL to VALUE and rebuild buffers."
  (set-default symbol value)
  (when (bound-and-true-p tessera-mu4e-headers--enabled-p)
    (dolist (buffer
             (match-buffers '(derived-mode . mu4e-headers-mode)))
      (with-current-buffer buffer
        (when tessera-mu4e-headers--installed-p
          (tessera-mu4e-headers--clear-presentations)
          (tessera-mu4e-headers--month-rebuild)
          (tessera-mu4e-headers-refresh))))))

(defcustom tessera-mu4e-headers-group-by-month t
  "Whether mu4e Headers entries are grouped by calendar month."
  :type 'boolean
  :set #'tessera-mu4e-headers--set-month-option
  :group 'tessera-mu4e)

(defcustom tessera-mu4e-headers-presentation-delay 0.08
  "Idle delay before rebuilding window-local Headers presentation."
  :type 'number
  :group 'tessera-mu4e)

(defconst tessera-mu4e-headers--status-specs
  '((new "N" "◉" (mdicon . "nf-md-new_box")
         tessera-mu4e-headers-glyph-attention "New")
    (unread "U" "●" (mdicon . "nf-md-email")
            tessera-mu4e-headers-glyph-attention "Unread")
    (seen "S" "○" (mdicon . "nf-md-email_open")
          tessera-mu4e-headers-glyph "Seen")
    (flagged "F" "★" (mdicon . "nf-md-star")
             tessera-mu4e-headers-glyph-attention "Flagged")
    (replied "R" "↩" (mdicon . "nf-md-reply")
             tessera-mu4e-headers-glyph-workflow "Replied")
    (passed "P" "↪" (mdicon . "nf-md-forward")
            tessera-mu4e-headers-glyph-workflow "Forwarded")
    (draft "D" "✎" (mdicon . "nf-md-file_document_edit")
           tessera-mu4e-headers-glyph-workflow "Draft")
    (trashed "T" "×" (mdicon . "nf-md-delete")
             tessera-mu4e-headers-glyph-error "Trashed"))
  "Glyph specifications for mu4e message states.")

(defconst tessera-mu4e-headers--feature-specs
  '((encrypted "E" "🔒" (mdicon . "nf-md-lock")
               tessera-mu4e-headers-glyph-security "Encrypted")
    (signed "S" "✎" (mdicon . "nf-md-certificate_outline")
            tessera-mu4e-headers-glyph-security "Signed")
    (attach "A" "📎" (mdicon . "nf-md-paperclip")
            tessera-mu4e-headers-glyph-availability "Attachment")
    (calendar "C" "▦" (mdicon . "nf-md-calendar")
              tessera-mu4e-headers-glyph-workflow "Calendar")
    (personal "P" "◎" (mdicon . "nf-md-account")
              tessera-mu4e-headers-glyph-attention "Personal")
    (list "L" "☷" (mdicon . "nf-md-format_list_bulleted")
          tessera-mu4e-headers-glyph "Mailing list"))
  "Glyph specifications for mu4e content features.")

(defun tessera-mu4e-headers--mark-slot (flag)
  "Return the semantic mark slot for mu4e FLAG."
  (pcase flag
    ((or 'new 'unread 'seen) 'main)
    ((or 'replied 'passed) 'secondary)
    ('flagged 'tertiary)
    ((or 'draft 'trashed) 'other)))

(let (definitions)
  (dolist (spec tessera-mu4e-headers--status-specs)
    (let ((flag (nth 0 spec)))
      (push (list :kind 'mark
                  :fact flag
                  :slot
                  (tessera-mu4e-headers--mark-slot flag)
                  :priority
                  (pcase flag
                    ('new 300)
                    ('unread 200)
                    ('replied 200)
                    ('trashed 200)
                    (_ 100))
                  :ascii (nth 1 spec)
                  :unicode (nth 2 spec)
                  :nerd-icon (nth 3 spec)
                  :face (nth 4 spec)
                  :description (nth 5 spec))
            definitions)))
  (dolist (spec tessera-mu4e-headers--feature-specs)
    (let ((flag (nth 0 spec)))
      (push (list :kind 'feature
                  :fact flag
                  :slot
                  (if (memq flag '(encrypted attach personal))
                      'primary
                    'secondary)
                  :priority
                  (pcase flag
                    ((or 'encrypted 'signed) 300)
                    ((or 'attach 'calendar) 200)
                    (_ 100))
                  :ascii (nth 1 spec)
                  :unicode (nth 2 spec)
                  :nerd-icon (nth 3 spec)
                  :face (nth 4 spec)
                  :description (nth 5 spec))
            definitions)))
  (push (list :kind 'feature
              :fact 'overflow
              :slot 'overflow
              :priority 0
              :ascii "+"
              :unicode "⋯"
              :nerd-icon
              '(mdicon . "nf-md-dots_horizontal_circle_outline")
              :face 'tessera-mu4e-headers-glyph-attention
              :description "More features")
        definitions)
  (tessera-ui-glyph-register-source 'mu4e-headers (nreverse definitions)))

(defvar-local tessera-mu4e-headers--presentations nil
  "Window-local presentation state in this Headers buffer.")

(defvar-local tessera-mu4e-headers--rail nil
  "Shared rail specification for flat Headers entries.")

(defvar-local tessera-mu4e-headers--metrics nil
  "Cached metrics for the current Headers result.")

(defvar-local tessera-mu4e-headers--thread-groups nil
  "Message-to-thread lookup for the current native search result.")

(defun tessera-mu4e-headers--thread-id (message)
  "Return the native thread identifier for MESSAGE."
  (let* ((meta (mu4e-message-field message :meta))
         (path (plist-get meta :path)))
    (or (and (stringp path)
             (string-match "\\`\\([[:xdigit:]]+\\)" path)
             (match-string 1 path))
        (format "message-%s"
                (mu4e-message-field message :docid)))))

(defun tessera-mu4e-headers--thread-connector (message ancestors)
  "Return the semantic connector for MESSAGE and ANCESTORS."
  (let* ((meta (mu4e-message-field message :meta))
         (level (max 0 (or (plist-get meta :level) 0))))
    (tessera-ui-thread-connector-create
     :ancestors ancestors
     :kind
     (cond
      ((or (zerop level) (plist-get meta :root)) 'root)
      ((plist-get meta :orphan) 'orphan)
      ((plist-get meta :last-child) 'last)
      (t 'branch)))))

(defun tessera-mu4e-headers--thread-unread-p (message)
  "Return non-nil when native mu4e MESSAGE is unread."
  (let ((flags (mu4e-message-field message :flags)))
    (or (memq 'new flags)
        (memq 'unread flags))))

(defun tessera-mu4e-headers--thread-ancestor-state (state level)
  "Return STATE normalized for a member at LEVEL."
  (let ((ancestor-count (max 0 (1- level))))
    (seq-take (append state
                      (make-list (max 0 (- ancestor-count (length state))) nil))
              ancestor-count)))

(defun tessera-mu4e-headers--thread-member (message connector)
  "Return a semantic member for MESSAGE using CONNECTOR."
  (let* ((face
          (tessera-mu4e-headers--message-face message))
         (author-face
          (tessera-ui-entry-author-face face
                                        (tessera-mu4e-headers--thread-unread-p message)))
         (features
          (tessera-mu4e-headers--features message face))
         (label-variants
          (tessera-mu4e-headers--label-variants message)))
    (tessera-ui-thread-member-create
     :key (mu4e-message-field message :docid)
     :slots
     (tessera-mu4e-headers--main-slots message face)
     :connector connector
     :left-segments
     (list (tessera-ui-make-segment
            'thread.member.author
            (tessera-mu4e-headers--author message)
            'truncate author-face)
           (tessera-ui-make-segment 'thread.member.features features 'hide face
                                    'entry.separator))
     :right-segments
     (list (tessera-ui-make-segment
            'thread.member.labels
            (car label-variants) 'truncate
            'font-lock-constant-face nil label-variants)
           (tessera-ui-make-segment 'thread.member.timestamp
                                    (format-time-string "%B %-d, %Y %I:%M %p"
                                                        (mu4e-message-field message :date))
                                    'preserve 'tessera-ui-entry-timestamp
                                    'entry.separator)))))

(defun tessera-mu4e-headers--thread-members (messages)
  "Return semantic thread members for native MESSAGES."
  (let (members state)
    (dolist (message messages (nreverse members))
      (let* ((meta (mu4e-message-field message :meta))
             (level
              (max 0 (or (plist-get meta :level) 0))))
        (when (zerop level)
          (setq state nil))
        (setq state
              (tessera-mu4e-headers--thread-ancestor-state state level))
        (push (tessera-mu4e-headers--thread-member
               message
               (tessera-mu4e-headers--thread-connector message state))
              members)
        (when (and (plist-get meta :has-child)
                   (not (plist-get meta :last-child)))
          (setq state (append state '(t))))))))

(cl-defmethod tessera-ui-make-thread ((_source (eql mu4e-headers)) messages)
  "Create a semantic thread from native mu4e MESSAGES."
  (unless messages
    (error "Cannot create an empty mu4e thread"))
  (let* ((visible (length messages))
         (unread
          (seq-count #'tessera-mu4e-headers--thread-unread-p
                     messages))
         (root (car messages)))
    (tessera-ui-thread-create
     :source 'mu4e-headers
     :key (tessera-mu4e-headers--thread-id root)
     :statistic
     (tessera-ui-thread-statistic-create
      :unread unread
      :visible visible
      :known visible
      :exactness 'exact)
     :main-left-segments
     (list (tessera-ui-make-segment
            'thread.subject
            (mu4e-message-field root :subject)
            'truncate
            (list :inherit
                  (if (> unread 0)
                      'mu4e-unread-face
                    'mu4e-header-face)
                  :weight 'bold)))
     :members
     (tessera-mu4e-headers--thread-members messages))))

(defvar-keymap tessera-mu4e-headers--month-map
  :doc "Keymap for month headings in mu4e Headers."
  "RET" #'tessera-mu4e-headers--month-toggle
  "TAB" #'tessera-mu4e-headers--month-toggle
  "<tab>" #'tessera-mu4e-headers--month-toggle
  "<mouse-1>" #'tessera-mu4e-headers--month-mouse-toggle
  "<mouse-2>" #'tessera-mu4e-headers--month-mouse-toggle)

(defvar-local tessera-mu4e-headers--month-groups nil
  "Standard month groups in the current Headers buffer.")

(defvar-local tessera-mu4e-headers--month-overlays nil
  "Month fold overlays in the current Headers buffer.")

(defvar-local tessera-mu4e-headers--month-collapsed nil
  "Calendar month keys collapsed in the current Headers buffer.")

(defun tessera-mu4e-headers--month-unread-p (message)
  "Return non-nil when mu4e MESSAGE is unread."
  (let ((flags (mu4e-message-field message :flags)))
    (or (memq 'new flags) (memq 'unread flags))))

(defun tessera-mu4e-headers--month-at-time (time)
  "Return the local calendar month containing TIME."
  (if time
      (let ((decoded (decode-time time)))
        (list (decoded-time-year decoded)
              (decoded-time-month decoded)
              (format-time-string "%B" time)))
    '(nil nil "Unknown date")))

(defun tessera-mu4e-headers--month-latest-time (messages)
  "Return the latest valid date among MESSAGES."
  (let (latest)
    (dolist (message messages latest)
      (let ((date (mu4e-message-field message :date)))
        (when (and date
                   (or (not latest)
                       (time-less-p latest date)))
          (setq latest date))))))

(defun tessera-mu4e-headers--month-messages ()
  "Return native messages in the current Headers display order."
  (let (messages)
    (mu4e-headers-for-each (lambda (message)
                             (push message messages)))
    (nreverse messages)))

(defun tessera-mu4e-headers--month-items (messages)
  "Return flat or threaded source items from MESSAGES."
  (if (not (and (boundp 'mu4e-search-threads)
                mu4e-search-threads
                (fboundp 'tessera-mu4e-headers--thread-id)))
      (mapcar #'list messages)
    (let (items current current-id)
      (dolist (message messages)
        (let ((thread-id (tessera-mu4e-headers--thread-id message)))
          (unless (equal thread-id current-id)
            (when current
              (push (nreverse current) items))
            (setq current nil
                  current-id thread-id))
          (push message current)))
      (when current
        (push (nreverse current) items))
      (nreverse items))))

(defun tessera-mu4e-headers--month-groups (buffer)
  "Derive standard month groups from mu4e Headers BUFFER."
  (with-current-buffer buffer
    (let ((items
           (tessera-mu4e-headers--month-items (tessera-mu4e-headers--month-messages)))
          groups)
      (dolist (item items)
        (let* ((month
                (tessera-mu4e-headers--month-at-time (tessera-mu4e-headers--month-latest-time item)))
               (key (butlast month))
               (unread
                (seq-count #'tessera-mu4e-headers--month-unread-p item))
               (total (length item))
               (current (car groups)))
          (if (and current
                   (equal key
                          (tessera-ui-month-group-key current)))
              (let ((statistics
                     (tessera-ui-month-group-statistics current)))
                (setf (tessera-ui-month-group-items current)
                      (cons item
                            (tessera-ui-month-group-items current))
                      (tessera-ui-month-statistics-unread statistics)
                      (+ (tessera-ui-month-statistics-unread statistics)
                         unread)
                      (tessera-ui-month-statistics-total statistics)
                      (+ (tessera-ui-month-statistics-total statistics)
                         total)))
            (push (tessera-ui-month-group-create
                   :source 'mu4e-headers
                   :key key
                   :year (car month)
                   :month-name (nth 2 month)
                   :statistics
                   (tessera-ui-month-statistics-create
                    :unread unread :total total)
                   :items (list item))
                  groups))))
      (dolist (group groups)
        (setf (tessera-ui-month-group-items group)
              (nreverse (tessera-ui-month-group-items group))))
      (nreverse groups))))

(tessera-ui-month-register 'mu4e-headers #'tessera-mu4e-headers--month-groups)

(defun tessera-mu4e-headers--month-group (key)
  "Return the current standard month group identified by KEY."
  (seq-find (lambda (group)
              (equal key (tessera-ui-month-group-key group)))
            tessera-mu4e-headers--month-groups))

(defun tessera-mu4e-headers--month-collapsed-p (key)
  "Return non-nil when the month identified by KEY is collapsed."
  (and (member key tessera-mu4e-headers--month-collapsed) t))

(defun tessera-mu4e-headers--month-help (collapsed-p)
  "Return month heading help for COLLAPSED-P."
  (format "mouse-1, RET, TAB: %s month"
          (if collapsed-p "Expand" "Collapse")))

(defun tessera-mu4e-headers--month-heading-display (key window)
  "Return the heading for month KEY in WINDOW."
  (when-let* ((group (tessera-mu4e-headers--month-group key)))
    (let* ((collapsed-p
            (tessera-mu4e-headers--month-collapsed-p key))
           (display
            (tessera-ui-format-month-heading group collapsed-p window)))
      (add-text-properties 0 (length display)
                           (list 'keymap tessera-mu4e-headers--month-map
                                 'mouse-face 'tessera-ui-month-heading-highlight
                                 'help-echo (tessera-mu4e-headers--month-help collapsed-p)
                                 'tessera-ui--month-key key)
                           display)
      display)))

(defun tessera-mu4e-headers--month-delete-overlays ()
  "Delete month fold overlays in the current buffer."
  (mapc #'delete-overlay tessera-mu4e-headers--month-overlays)
  (setq tessera-mu4e-headers--month-overlays nil))

(defun tessera-mu4e-headers--month-hidden-p (position)
  "Return non-nil when POSITION is inside a month fold."
  (seq-some (lambda (overlay)
              (and (overlay-buffer overlay)
                   (overlay-get overlay 'tessera-ui--month-fold)
                   (<= (overlay-start overlay) position)
                   (< position (overlay-end overlay))))
            tessera-mu4e-headers--month-overlays))

(defun tessera-mu4e-headers--month-delete-headings ()
  "Delete inserted month headings in the current buffer."
  (let ((inhibit-read-only t)
        (position (point-min)))
    (while (< position (point-max))
      (if (get-text-property position 'tessera-ui--month-key)
          (save-excursion
            (goto-char position)
            (delete-region (line-beginning-position)
                           (min (point-max) (1+ (line-end-position)))))
        (setq position
              (next-single-property-change position 'tessera-ui--month-key nil (point-max)))))))

(defun tessera-mu4e-headers--month-clear ()
  "Remove every month group presentation from this buffer."
  (tessera-mu4e-headers--month-delete-overlays)
  (tessera-mu4e-headers--month-delete-headings)
  (setq tessera-mu4e-headers--month-groups nil)
  (remove-from-invisibility-spec 'tessera-mu4e-month))

(defun tessera-mu4e-headers--month-message-positions ()
  "Return native message positions keyed by document ID."
  (let ((positions (make-hash-table :test #'eql)))
    (mu4e-headers-for-each (lambda (message)
                             (puthash (mu4e-message-field message :docid)
                                      (line-beginning-position)
                                      positions)))
    positions))

(defun tessera-mu4e-headers--month-heading-bounds (key)
  "Return the heading bounds for month KEY."
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
              (min (point-max) (1+ (line-end-position))))))))

(defun tessera-mu4e-headers--month-body-bounds (key)
  "Return message body bounds for month KEY."
  (when-let* ((heading
               (tessera-mu4e-headers--month-heading-bounds key)))
    (let ((start (cdr heading))
          (position (cdr heading)))
      (while (and (< position (point-max))
                  (not (get-text-property position 'tessera-ui--month-key)))
        (setq position
              (next-single-property-change position 'tessera-ui--month-key nil (point-max))))
      (cons start position))))

(defun tessera-mu4e-headers--month-make-fold (key)
  "Make the fold overlay for month KEY."
  (when-let* ((bounds (tessera-mu4e-headers--month-body-bounds key)))
    (let ((overlay (make-overlay (car bounds) (cdr bounds))))
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'invisible 'tessera-mu4e-month)
      (overlay-put overlay 'tessera-ui--month-key key)
      (overlay-put overlay 'tessera-ui--month-fold t)
      (push overlay tessera-mu4e-headers--month-overlays)
      overlay)))

(defun tessera-mu4e-headers--month-rebuild ()
  "Rebuild standard month groups in this Headers buffer."
  (tessera-mu4e-headers--month-delete-overlays)
  (tessera-mu4e-headers--month-delete-headings)
  (if (not tessera-mu4e-headers-group-by-month)
      (progn
        (setq tessera-mu4e-headers--month-groups nil)
        (remove-from-invisibility-spec 'tessera-mu4e-month))
    (let ((positions (tessera-mu4e-headers--month-message-positions))
          entries)
      (setq tessera-mu4e-headers--month-groups
            (tessera-ui-make-month-groups 'mu4e-headers (current-buffer)))
      (dolist (group tessera-mu4e-headers--month-groups)
        (when-let* ((item
                     (car (tessera-ui-month-group-items group)))
                    (message (car item))
                    (position
                     (gethash (mu4e-message-field message :docid)
                              positions)))
          (push (cons position group) entries)))
      (save-excursion
        (let ((inhibit-read-only t))
          (dolist (entry
                   (sort entries
                         (lambda (left right)
                           (> (car left) (car right)))))
            (goto-char (car entry))
            (let* ((group (cdr entry))
                   (key (tessera-ui-month-group-key group))
                   (collapsed-p
                    (tessera-mu4e-headers--month-collapsed-p key))
                   (start (point))
                   (heading
                    (tessera-ui-format-month-heading group collapsed-p)))
              (insert heading "\n")
              (add-text-properties start (point)
                                   (list 'keymap tessera-mu4e-headers--month-map
                                         'mouse-face 'tessera-ui-month-heading-highlight
                                         'help-echo
                                         (tessera-mu4e-headers--month-help collapsed-p)
                                         'tessera-ui--month-key key))))))
      (unless (or (eq buffer-invisibility-spec t)
                  (memq 'tessera-mu4e-month
                        buffer-invisibility-spec))
        (add-to-invisibility-spec 'tessera-mu4e-month))
      (dolist (key tessera-mu4e-headers--month-collapsed)
        (tessera-mu4e-headers--month-make-fold key)))))

(defun tessera-mu4e-headers--month-toggle ()
  "Toggle the mu4e month heading at point."
  (interactive)
  (let ((key (get-text-property (point) 'tessera-ui--month-key)))
    (unless key
      (user-error "Point is not on a month heading"))
    (if (tessera-mu4e-headers--month-collapsed-p key)
        (setq tessera-mu4e-headers--month-collapsed
              (delete key tessera-mu4e-headers--month-collapsed))
      (push key tessera-mu4e-headers--month-collapsed))
    (tessera-mu4e-headers--month-rebuild)
    (when (fboundp 'tessera-mu4e-headers-refresh)
      (tessera-mu4e-headers-refresh))))

(defun tessera-mu4e-headers--month-mouse-toggle (event)
  "Toggle the mu4e month heading clicked by mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (tessera-mu4e-headers--month-toggle))

(defun tessera-mu4e-headers--glyph (spec context-face)
  "Render registered glyph SPEC beside CONTEXT-FACE."
  (tessera-ui-glyph-render spec tessera-mu4e-symbol-style tessera-mu4e-glyph-color-style context-face))

(defun tessera-mu4e-headers--message-face (message)
  "Return the native content face for MESSAGE."
  (let ((flags (mu4e-message-field message :flags)))
    (cond
     ((memq 'trashed flags) 'mu4e-trashed-face)
     ((memq 'draft flags) 'mu4e-draft-face)
     ((or (memq 'unread flags) (memq 'new flags))
      'mu4e-unread-face)
     ((memq 'flagged flags) 'mu4e-flagged-face)
     ((memq 'replied flags) 'mu4e-replied-face)
     ((memq 'passed flags) 'mu4e-forwarded-face)
     (t 'mu4e-header-face))))

(defun tessera-mu4e-headers--main-slots (message face)
  "Return every selected mark slot for MESSAGE beside FACE."
  (tessera-ui-glyph-make-mark-slots 'mu4e-headers
                                    (mu4e-message-field message :flags)
                                    (lambda (spec)
                                      (tessera-mu4e-headers--glyph spec face))))

(defun tessera-mu4e-headers--features (message face)
  "Return the feature string for MESSAGE beside FACE."
  (let* ((selection
          (tessera-ui-glyph-select-features 'mu4e-headers
                                            (mu4e-message-field message :flags)))
         (hidden
          (tessera-ui-glyph-selection-hidden selection))
         glyphs)
    (dolist (spec (tessera-ui-glyph-selection-specs selection))
      (let* ((fact (tessera-ui-glyph-spec-fact spec))
             (glyph
              (tessera-mu4e-headers--glyph spec face)))
        (add-text-properties 0 (length glyph)
                             (list 'tessera-ui--feature fact
                                   'tessera-ui--hidden-features
                                   (and (eq fact 'overflow) hidden)
                                   'help-echo
                                   (if (eq fact 'overflow)
                                       (format "Hidden features: %s"
                                               (mapconcat (lambda (hidden-fact)
                                                            (tessera-ui-glyph-spec-description (tessera-ui-glyph-spec 'mu4e-headers 'feature hidden-fact)))
                                                          hidden ", "))
                                     (tessera-ui-glyph-spec-description spec)))
                             glyph)
        (push glyph glyphs)))
    (mapconcat #'identity (nreverse glyphs)
               (propertize " "
                           'tessera-ui--element
                           'entry.features.inline-gap))))

(defun tessera-mu4e-headers--author (message)
  "Return the display author for MESSAGE."
  (let ((contact
         (car (mu4e-message-field message :from))))
    (or (plist-get contact :name)
        (plist-get contact :email)
        "Unknown sender")))

(defun tessera-mu4e-headers--label-variants (message)
  "Return widest-to-narrowest label strings for MESSAGE."
  (let* ((labels (mu4e-message-field message :labels))
         (limit tessera-mu4e-headers-label-limit)
         (visible
          (if (numberp limit)
              (seq-take labels limit)
            labels))
         (limited
          (and (numberp limit)
               (nthcdr limit labels)))
         variants)
    (dotimes (offset (1+ (length visible)))
      (let* ((count (- (length visible) offset))
             (shown (seq-take visible count))
             (hidden (append (nthcdr count visible) limited))
             (text
              (mapconcat (lambda (label) (concat "@" label))
                         shown " ")))
        (when hidden
          (let ((overflow
                 (propertize "+"
                             'help-echo
                             (format "Hidden labels: %s"
                                     (string-join hidden ", "))
                             'tessera-mu4e-headers--hidden-labels hidden
                             'tessera-ui--element
                             'entry.labels.overflow)))
            (setq text
                  (if (string-empty-p text)
                      overflow
                    (concat text " " overflow)))))
        (push text variants)))
    (nreverse variants)))

(cl-defmethod tessera-ui-make-flat-entry ((_source (eql mu4e-headers)) message)
  "Create a flat entry from mu4e Headers MESSAGE."
  (let* ((face (tessera-mu4e-headers--message-face message))
         (features
          (tessera-mu4e-headers--features message face))
         (label-variants
          (tessera-mu4e-headers--label-variants message))
         (labels (car label-variants)))
    (tessera-ui-flat-entry-create
     :source 'mu4e-headers
     :key (mu4e-message-field message :docid)
     :main-slots
     (tessera-mu4e-headers--main-slots message face)
     :main-left-segments
     (list (tessera-ui-make-segment
            'entry.subject
            (mu4e-message-field message :subject)
            'truncate
            (list :inherit face :weight 'bold)))
     :extra-slots nil
     :extra-left-segments
     (list (tessera-ui-make-segment
            'entry.author
            (tessera-mu4e-headers--author message)
            'truncate face)
           (tessera-ui-make-segment 'entry.features features 'hide face
                                    'entry.separator))
     :extra-right-segments
     (list (tessera-ui-make-segment
            'entry.labels labels 'truncate
            'font-lock-constant-face nil label-variants)
           (tessera-ui-make-segment 'entry.timestamp
                                    (format-time-string "%B %-d, %Y %I:%M %p"
                                                        (mu4e-message-field message :date))
                                    'preserve 'tessera-ui-entry-timestamp
                                    'entry.separator)))))

(defun tessera-mu4e-headers--message-counts ()
  "Return visible and unread message counts for this buffer."
  (let ((visible 0)
        (unread 0))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (when-let* ((message (get-text-property (point) 'msg)))
          (setq visible (1+ visible))
          (let ((flags (mu4e-message-field message :flags)))
            (when (or (memq 'new flags) (memq 'unread flags))
              (setq unread (1+ unread)))))
        (forward-line 1)))
    (cons visible unread)))

(defun tessera-mu4e-headers--update-metrics ()
  "Update semantic metrics for the current Headers result."
  (let* ((counts (tessera-mu4e-headers--message-counts))
         (visible (car counts))
         (unread (cdr counts))
         (query-info (mu4e-server-last-query))
         (found (plist-get query-info :found))
         (maximum (plist-get query-info :maxnum))
         (total (if (numberp found) found visible))
         (incomplete
          (or (not (numberp found))
              (and (numberp maximum)
                   (> maximum 0)
                   (>= total maximum)))))
    (setq tessera-mu4e-headers--metrics
          (tessera-ui-header-line-standard-metrics unread visible 'visible "visible" total
                                                   'exact 'exact
                                                   (if incomplete 'lower-bound 'exact)))))

(defun tessera-mu4e-headers--query ()
  "Return the current Headers filter condition."
  (or (and (stringp list-buffers-directory)
           list-buffers-directory)
      (plist-get (mu4e-server-last-query) :query)
      ""))

(defun tessera-mu4e-headers--header-status-segment (buffer)
  "Derive the registered status segment from Headers BUFFER."
  (with-current-buffer buffer
    (tessera-ui-header-line-status-segment (tessera-mu4e-status))))

(defun tessera-mu4e-headers--header-query-segment (buffer)
  "Derive the registered query segment from Headers BUFFER."
  (with-current-buffer buffer
    (tessera-ui-header-line-query-segment (tessera-ui-header-scope-create
                                           :kind 'query
                                           :label "QUERY"
                                           :value (tessera-mu4e-headers--query)))))

(defun tessera-mu4e-headers--header-context-segment (buffer)
  "Derive the current-context-name segment from Headers BUFFER."
  (with-current-buffer buffer
    (tessera-ui-header-line-context-segment (when-let* ((context (mu4e-context-current)))
                                              (mu4e-context-name context)))))

(defun tessera-mu4e-headers--header-statistics-segment (buffer)
  "Derive the registered statistics segment from Headers BUFFER."
  (with-current-buffer buffer
    (tessera-ui-header-line-statistics-segment (or tessera-mu4e-headers--metrics
                                                   (progn
                                                     (tessera-mu4e-headers--update-metrics)
                                                     tessera-mu4e-headers--metrics)))))

(tessera-ui-header-line-register 'mu4e-headers
                                 :left
                                 '((status . tessera-mu4e-headers--header-status-segment)
                                   (query . tessera-mu4e-headers--header-query-segment))
                                 :right
                                 '((current-context-name .
                                                         tessera-mu4e-headers--header-context-segment)
                                   (statistics .
                                               tessera-mu4e-headers--header-statistics-segment)))

(defun tessera-mu4e-headers--rail ()
  "Return the shared rail for flat Headers entries."
  (or tessera-mu4e-headers--rail
      (setq tessera-mu4e-headers--rail
            (tessera-ui-make-entry-rail tessera-mu4e-symbol-style))))

(defun tessera-mu4e-headers--presentation-state ()
  "Return window presentation state for the current Headers buffer."
  (or tessera-mu4e-headers--presentations
      (setq tessera-mu4e-headers--presentations
            (tessera-ui-window-presentations-create))))

(defun tessera-mu4e-headers--delete-overlays ()
  "Delete Tessera presentation overlays in the current buffer."
  (tessera-ui-window-presentations-delete (tessera-mu4e-headers--presentation-state)))

(defun tessera-mu4e-headers--clear-presentations ()
  "Cancel and delete every Headers presentation."
  (tessera-ui-window-presentations-cancel (tessera-mu4e-headers--presentation-state))
  (tessera-mu4e-headers--delete-overlays))

(defun tessera-mu4e-headers--make-thread-groups ()
  "Return a message lookup for native mu4e threads."
  (let ((groups (make-hash-table :test #'equal))
        (lookup (make-hash-table :test #'eql)))
    (mu4e-headers-for-each (lambda (message)
                             (let ((thread-id
                                    (tessera-mu4e-headers--thread-id message)))
                               (puthash thread-id
                                        (cons message (gethash thread-id groups))
                                        groups))))
    (maphash (lambda (_thread-id reversed)
               (let ((messages (nreverse reversed))
                     (index 0))
                 (dolist (message messages)
                   (puthash (mu4e-message-field message :docid)
                            (cons messages index)
                            lookup)
                   (setq index (1+ index)))))
             groups)
    lookup))

(defun tessera-mu4e-headers--thread-group (message)
  "Return MESSAGE's native thread group and member index."
  (unless tessera-mu4e-headers--thread-groups
    (setq tessera-mu4e-headers--thread-groups
          (tessera-mu4e-headers--make-thread-groups)))
  (gethash (mu4e-message-field message :docid)
           tessera-mu4e-headers--thread-groups))

(defun tessera-mu4e-headers--thread-rows (window message)
  "Return the main and member rows for MESSAGE in WINDOW."
  (when-let* ((group
               (tessera-mu4e-headers--thread-group message))
              (messages (car group))
              (index (cdr group))
              (thread
               (tessera-ui-make-thread 'mu4e-headers messages))
              (lines
               (tessera-ui-thread-window-lines thread window
                                               (tessera-mu4e-headers--rail)
                                               tessera-mu4e-symbol-style))
              (member-line (nth (1+ index) lines)))
    (add-text-properties 0 (length member-line) (list 'tessera-ui--parent-element 'thread.member) member-line)
    (let ((main-line (and (zerop index) (car lines))))
      (when main-line
        (add-text-properties 0 (length main-line) (list 'tessera-ui--parent-element 'thread.main) main-line))
      (cons main-line member-line))))

(defun tessera-mu4e-headers--flat-display (window message)
  "Return the two-row flat display for MESSAGE in WINDOW."
  (let* ((entry
          (tessera-ui-make-flat-entry 'mu4e-headers message))
         (lines
          (tessera-ui-flat-entry-window-lines entry window (tessera-mu4e-headers--rail))))
    (concat (car lines) "\n" (cadr lines))))

(defun tessera-mu4e-headers--present-message (window start end message)
  "Present MESSAGE from START to END in WINDOW."
  (let* ((threaded-p
          (and (boundp 'mu4e-search-threads)
               mu4e-search-threads
               (fboundp 'tessera-mu4e-headers--thread-id)))
         (thread-rows
          (and threaded-p
               (tessera-mu4e-headers--thread-rows window message)))
         (overlay
          (if thread-rows
              (tessera-ui-make-virtual-row-overlay start end window (cdr thread-rows)
                                                   :main-line (car thread-rows)
                                                   :properties
                                                   '(tessera-mu4e-presentation t))
            (tessera-ui-make-window-overlay start end window
                                            :display ""
                                            :before-string
                                            (tessera-mu4e-headers--flat-display window message)
                                            :properties
                                            '(tessera-mu4e-presentation t)))))
    (tessera-ui-window-presentations-add (tessera-mu4e-headers--presentation-state)
                                         overlay)))

(defun tessera-mu4e-headers--present-month-heading (window start end key)
  "Present month KEY from START to END in WINDOW."
  (when-let* ((display
               (tessera-mu4e-headers--month-heading-display key window)))
    (tessera-ui-window-presentations-add (tessera-mu4e-headers--presentation-state)
                                         (tessera-ui-make-window-overlay start end window
                                                                         :display ""
                                                                         :before-string display
                                                                         :properties
                                                                         '(tessera-mu4e-presentation t)))))

(defun tessera-mu4e-headers--present-window (window)
  "Present visible message rows in WINDOW."
  (let ((position (window-start window))
        limit)
    (save-excursion
      (goto-char position)
      (vertical-motion (+ (window-body-height window) 4) window)
      (setq limit (point)))
    (while (< position limit)
      (let* ((start
              (save-excursion
                (goto-char position)
                (line-beginning-position)))
             (end
              (save-excursion
                (goto-char position)
                (line-end-position)))
             (month-key
              (get-text-property start 'tessera-ui--month-key))
             (message (get-text-property start 'msg)))
        (cond
         (month-key
          (tessera-mu4e-headers--present-month-heading window start end month-key))
         ((and message
               (not (tessera-mu4e-headers--month-hidden-p start)))
          (tessera-mu4e-headers--present-message window start end message)))
        (setq position
              (min limit (1+ end)))))
    (tessera-ui-window-presentations-record-start (tessera-mu4e-headers--presentation-state)
                                                  window (window-start window))))

(defun tessera-mu4e-headers--refresh-now ()
  "Rebuild the current Headers presentation immediately."
  (tessera-ui-window-presentations-cancel (tessera-mu4e-headers--presentation-state))
  (tessera-ui-header-line-install 'mu4e-headers)
  (tessera-mu4e-headers--update-metrics)
  (setq tessera-mu4e-headers--rail nil
        tessera-mu4e-headers--thread-groups nil)
  (tessera-mu4e-headers--delete-overlays)
  (dolist (window
           (get-buffer-window-list (current-buffer) nil t))
    (set-window-hscroll window 0)
    (tessera-mu4e-headers--present-window window)))

(defun tessera-mu4e-headers-refresh ()
  "Refresh the Tessera presentation in this Headers buffer."
  (when tessera-mu4e-headers--installed-p
    (tessera-ui-window-presentations-update (tessera-mu4e-headers--presentation-state)
                                            #'tessera-mu4e-headers--refresh-now)))

(defun tessera-mu4e-headers--window-scrolled (window display-start)
  "Refresh after WINDOW scrolls to DISPLAY-START."
  (when
      (tessera-ui-window-presentations-start-changed-p (tessera-mu4e-headers--presentation-state)
                                                       window display-start)
    (tessera-mu4e-headers--schedule-refresh)))

(defun tessera-mu4e-headers--schedule-refresh (&rest _args)
  "Schedule a window-local Headers presentation refresh."
  (when tessera-mu4e-headers--installed-p
    (tessera-ui-window-presentations-schedule (tessera-mu4e-headers--presentation-state)
                                              tessera-mu4e-headers-presentation-delay
                                              #'tessera-mu4e-headers--refresh-now
                                              (current-buffer))))

(defun tessera-mu4e-headers--native-found ()
  "Rebuild month groups after native mu4e search results arrive."
  (tessera-mu4e-headers--clear-presentations)
  (tessera-mu4e-headers--month-rebuild)
  (tessera-mu4e-headers-refresh))

(defun tessera-mu4e-headers--install ()
  "Install Tessera in the current mu4e Headers buffer."
  (unless tessera-mu4e-headers--installed-p
    (setq tessera-mu4e-headers--installed-p t)
    (tessera-ui-header-line-install 'mu4e-headers)
    (setq-local truncate-lines t)
    (setq-local auto-hscroll-mode nil)
    (add-hook 'mu4e-headers-found-hook #'tessera-mu4e-headers--native-found nil t)
    (add-hook 'window-configuration-change-hook #'tessera-mu4e-headers-refresh nil t)
    (add-hook 'window-scroll-functions #'tessera-mu4e-headers--window-scrolled nil t)
    (add-hook 'text-scale-mode-hook #'tessera-mu4e-headers--schedule-refresh nil t)
    (add-hook 'kill-buffer-hook #'tessera-mu4e-headers--clear-presentations nil t)
    (tessera-mu4e-headers--month-rebuild)
    (tessera-mu4e-headers-refresh)))

(defun tessera-mu4e-headers--restore ()
  "Restore the native current mu4e Headers presentation."
  (when tessera-mu4e-headers--installed-p
    (remove-hook 'mu4e-headers-found-hook #'tessera-mu4e-headers--native-found t)
    (remove-hook 'window-configuration-change-hook #'tessera-mu4e-headers-refresh t)
    (remove-hook 'window-scroll-functions #'tessera-mu4e-headers--window-scrolled t)
    (remove-hook 'text-scale-mode-hook #'tessera-mu4e-headers--schedule-refresh t)
    (remove-hook 'kill-buffer-hook #'tessera-mu4e-headers--clear-presentations t)
    (tessera-mu4e-headers--clear-presentations)
    (tessera-mu4e-headers--month-clear)
    (kill-local-variable 'auto-hscroll-mode)
    (tessera-ui-header-line-restore)
    (setq tessera-mu4e-headers--metrics nil
          tessera-mu4e-headers--thread-groups nil
          tessera-mu4e-headers--installed-p nil)))

(defun tessera-mu4e-headers--after-update (&rest _args)
  "Refresh Headers buffers after a native message update."
  (dolist (buffer
           (match-buffers '(derived-mode . mu4e-headers-mode)))
    (with-current-buffer buffer
      (tessera-mu4e-headers--clear-presentations)
      (tessera-mu4e-headers--month-rebuild)
      (tessera-mu4e-headers-refresh))))

(defun tessera-mu4e-headers-enable ()
  "Enable Tessera in existing and future Headers buffers."
  (unless tessera-mu4e-headers--enabled-p
    (setq tessera-mu4e-headers--enabled-p t)
    (add-hook 'mu4e-headers-mode-hook #'tessera-mu4e-headers--install)
    (advice-add 'mu4e~headers-update-handler :after #'tessera-mu4e-headers--after-update)
    (dolist (buffer
             (match-buffers '(derived-mode . mu4e-headers-mode)))
      (with-current-buffer buffer
        (tessera-mu4e-headers--install)))))

(defun tessera-mu4e-headers-disable ()
  "Disable Tessera in existing and future Headers buffers."
  (when tessera-mu4e-headers--enabled-p
    (setq tessera-mu4e-headers--enabled-p nil)
    (remove-hook 'mu4e-headers-mode-hook #'tessera-mu4e-headers--install)
    (advice-remove 'mu4e~headers-update-handler #'tessera-mu4e-headers--after-update)
    (dolist (buffer
             (match-buffers '(derived-mode . mu4e-headers-mode)))
      (with-current-buffer buffer
        (tessera-mu4e-headers--restore)))))

(provide 'tessera-mu4e-headers)
;;; tessera-mu4e-headers.el ends here
