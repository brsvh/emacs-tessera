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

;; Reversible non-thread presentation over native mu4e result rows.
;; The native docid cookie, mark fringe, and message identity remain.

;;; Code:

(require 'cl-lib)
(require 'hl-line)
(require 'subr-x)
(require 'tessera-mu4e-faces)

(defvar mu4e-search-threads)
(defvar mu4e-headers-visible-flags)
(defvar mu4e-headers-show-target)
(defvar mu4e-headers-from-or-to-prefix)
(defvar mu4e-headers-date-format)
(defvar mu4e-headers-time-format)
(defvar mu4e--mark-map)
(defvar mu4e--mark-fringe-len)
(defvar mu4e~headers-docid-pre)
(defvar mu4e~headers-docid-post)
(defvar mu4e~end-of-results)

(declare-function mu4e~headers-from-or-to "mu4e-headers")
(declare-function mu4e~headers-human-date "mu4e-headers")
(declare-function mu4e-mark-at-point "mu4e-mark")

(defconst tessera-mu4e-headers--icons
  '((new "N" "✦" "new-box" "New")
    (unread "u" "●" "email" "Unread")
    (seen "S" "○" "email-open-outline" "Read")
    (draft "D" "✎" "email-edit-outline" "Draft")
    (trashed "T" "⌫" "trash-can-outline" "Trashed")
    (flagged "F" "★" "star" "Flagged")
    (replied "R" "↩" "reply" "Replied")
    (passed "P" "↪" "forward" "Forwarded")
    (list "l" "≡" "format-list-bulleted" "Mailing list")
    (personal "p" "♙" "account-outline" "Personal")
    (attach "a" "📎" "paperclip" "Attachment present")
    (signed "S" "✍" "file-sign" "Signed; not verified")
    (encrypted "E" "🔒" "lock-outline" "Encrypted")
    (calendar "c" "▦" "calendar" "Calendar invitation")
    (high "H" "↑" "arrow-up-bold" "High priority")
    (low "L" "↓" "arrow-down-bold" "Low priority")
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
  "Glyphs for native message flags and pending operations.")

(defconst tessera-mu4e-headers--auxiliary
  '(draft trashed flagged replied passed list personal)
  "Independent auxiliary flags, in display order.")

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
(defvar-local tessera-mu4e-headers--saved-layout nil
  "Previous locality and value of the shared entry layout.")
(defvar-local tessera-mu4e-headers--saved-hl-line nil
  "Whether native line highlighting was enabled.")

(defun tessera-mu4e-headers--context (object buffer window)
  "Build a context for native OBJECT in BUFFER and WINDOW."
  (make-tessera-entry-context
   :backend 'mu4e-headers :object object
   :buffer buffer :window window))

(defun tessera-mu4e-headers--mark (message)
  "Return the native pending mark and target for MESSAGE."
  (when (hash-table-p mu4e--mark-map)
    (gethash (plist-get message :docid) mu4e--mark-map)))

(defun tessera-mu4e-headers--state (slot context)
  "Select SLOT from CONTEXT, retaining independent auxiliary flags."
  (let* ((message (tessera-entry-context-object context))
         (flags (plist-get message :flags)))
    (pcase slot
      ('status (cond ((memq 'new flags) 'new)
                     ((memq 'unread flags) 'unread)
                     (t 'seen)))
      ('priority (let ((value (plist-get message :priority)))
                   (and (memq value '(high low)) value)))
      ('operation
       (let ((mark (car (tessera-mu4e-headers--mark message))))
         (if (eq mark 'unread) 'mark-unread mark)))
      (_ (and (memq slot flags)
              (memq slot mu4e-headers-visible-flags) slot)))))

(defun tessera-mu4e-headers--help
    (label operation window object position)
  "Describe LABEL at POSITION in OBJECT or WINDOW.
For OPERATION icons, include the native pending mark target."
  (let ((buffer (if (bufferp object) object
                  (and (window-live-p window)
                       (window-buffer window)))))
    (if (and operation (buffer-live-p buffer))
        (with-current-buffer buffer
          (let* ((message (get-text-property position 'msg))
                 (mark (tessera-mu4e-headers--mark message)))
            (if (cdr mark) (format "%s: %s" label (cdr mark)) label)))
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
  "Make an independent glyph slot NAME."
  (make-tessera-glyph-slot
   :name name :width 2 :align 'center
   :selector (apply-partially #'tessera-mu4e-headers--state name)
   :glyphs
   (mapcar
    (lambda (spec)
      (list (car spec) :glyph (tessera-mu4e-headers--glyph spec)
            :face (tessera-mu4e-faces--glyph name (car spec))
            :help-echo
            (apply-partially #'tessera-mu4e-headers--help
                             (nth 4 spec) (eq name 'operation))))
    (cl-remove-if-not
     (lambda (spec)
       (pcase name
         ('operation
          (memq (car spec)
                '(move refile trash untrash delete flag unflag read
                       mark-unread label unlabel action something)))
         ('priority (memq (car spec) '(high low)))
         ('status (memq (car spec) '(new unread seen)))
         (_ (eq name (car spec)))))
     tessera-mu4e-headers--icons))))

(defun tessera-mu4e-headers--field (role context)
  "Return a native message element for ROLE in CONTEXT."
  (let* ((message (tessera-entry-context-object context))
         (mark (tessera-mu4e-headers--mark message)))
    (pcase role
      ('labels
       (mapconcat
        (lambda (label)
          (propertize
           (tessera-mu4e-faces--text 'label message label)
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
       (when (and mu4e-headers-show-target (cdr mark))
         (propertize (format "→ %s" (cdr mark))
                     'face 'tessera-mu4e-headers-operation-face
                     'help-echo
                     (format "%s: %s" (car mark) (cdr mark))
                     'mouse-face 'tessera-entry-hover-face)))
      (_
       (let* ((text
               (pcase role
                 ('subject (let ((subject
                                  (plist-get message :subject)))
                             (if (or (null subject)
                                     (string-empty-p subject))
                                 "(no subject)" subject)))
                 ('contact (mu4e~headers-from-or-to message))
                 ('date (mu4e~headers-human-date message))))
              (help (if (eq role 'contact)
                        (format "From: %S\nTo: %S"
                                (plist-get message :from)
                                (plist-get message :to))
                      text)))
         (propertize (tessera-mu4e-faces--text role message text)
                     'help-echo help
                     'mouse-face 'tessera-entry-hover-face))))))

(defun tessera-mu4e-headers--prefix (kind extra)
  "Return packed icon references for KIND and EXTRA line."
  (mapcar
   (lambda (name) (list name :optional t))
   (cond
    ((eq kind 'single-line)
     (append '(operation priority) tessera-mu4e-headers--auxiliary
             '(status)))
    (extra (cons 'priority tessera-mu4e-headers--auxiliary))
    (t '(operation status)))))

(defun tessera-mu4e-headers--width (_context)
  "Return the common prefix width for the current result set."
  tessera-mu4e-headers--leading-width)

(defun tessera-mu4e-headers--register ()
  "Register the native mu4e headers backend."
  (let* ((content '(:slots (attach :optional t) (signed :optional t)
                           (encrypted :optional t)
                           (calendar :optional t)))
         (subject '(subject :grow t :min-width 4 :truncate tail))
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
             '(subject contact date labels target))
     :glyph-slots
     (mapcar #'tessera-mu4e-headers--slot
             (append '(operation priority status)
                     tessera-mu4e-headers--auxiliary
                     '(attach signed encrypted calendar)))
     :layouts
     (list
      (cons 'single-line
            (make-tessera-entry-layout
             :glyph-slots-align 'right
             :leading-width #'tessera-mu4e-headers--width
             :main-leading-segments
             (list (cons :slots (tessera-mu4e-headers--prefix
                                 'single-line nil)))
             :main-left-segments (list subject target content)
             :main-right-segments
             (list labels '(contact :max-width 20 :truncate tail
                                    :optional t) 'date)))
      (cons 'two-line
            (make-tessera-entry-layout
             :glyph-slots-align 'right
             :leading-width #'tessera-mu4e-headers--width
             :main-leading-segments
             (list (cons :slots (tessera-mu4e-headers--prefix
                                 'two-line nil)))
             :extra-leading-segments
             (list (cons :slots (tessera-mu4e-headers--prefix
                                 'two-line t)))
             :main-left-segments (list subject target)
             :main-right-segments (list labels)
             :extra-left-segments
             (list '(contact :grow t :min-width 4 :truncate tail)
                   content)
             :extra-right-segments '(date)))))))

(defun tessera-mu4e-headers--body-start ()
  "Return the native body start on this logical line, or nil."
  (save-excursion
    (goto-char (line-beginning-position))
    (when (and (looking-at (regexp-quote mu4e~headers-docid-pre))
               (search-forward mu4e~headers-docid-post
                               (line-end-position) t))
      (+ (point) mu4e--mark-fringe-len))))

(defun tessera-mu4e-headers--measure ()
  "Measure the maximum visible prefix width for this result set."
  (let ((width 0))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (when (tessera-mu4e-headers--body-start)
          (let ((context (tessera-mu4e-headers--context
                          (get-text-property (point) 'msg)
                          (current-buffer) nil)))
            (dolist (extra (if (eq tessera-entry-layout 'two-line)
                               '(nil t) '(nil)))
              (setq width
                    (max width
                         (* 2 (cl-count-if
                               (lambda (reference)
                                 (tessera-mu4e-headers--state
                                  (car reference) context))
                               (tessera-mu4e-headers--prefix
                                tessera-entry-layout extra))))))))
        (forward-line 1)))
    width))

(defun tessera-mu4e-headers--sync-line (native)
  "Render the current row, or restore it when NATIVE is non-nil."
  (when-let* ((body (tessera-mu4e-headers--body-start)))
    (let* ((start (line-beginning-position))
           (end (line-end-position))
           (message (get-text-property start 'msg))
           (saved (get-text-property body 'tessera-mu4e-native))
           (original (or saved (buffer-substring body end))))
      (when (or saved (not native))
        (remove-overlays start (1+ end) 'mu4e-mark t)
        (delete-region body end)
        (goto-char body)
        (if native
            (progn
              (insert original)
              (remove-text-properties
               start (1+ (point))
               '(tessera-mu4e-native nil tessera--entry-layout nil))
              (remove-text-properties
               (- body mu4e--mark-fringe-len) body '(display nil)))
          (let ((text (tessera-entry-render 'mu4e-headers message)))
            (put-text-property 0 (length text)
                               'tessera-mu4e-native original text)
            (insert text)
            (put-text-property
             start body 'tessera--entry-layout
             (get-text-property body 'tessera--entry-layout))
            (put-text-property
             (- body mu4e--mark-fringe-len) body 'display "")))
        (add-text-properties
         start (1+ (point))
         (list 'docid (plist-get message :docid) 'msg message))
        (if native
            (when-let* ((mark (tessera-mu4e-headers--mark message)))
              (goto-char start)
              (mu4e-mark-at-point (car mark) (cdr mark)))
          (tessera-entry-apply-layout body (point)))))))

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

(defun tessera-mu4e-headers--sync (native)
  "Synchronize the result buffer, using NATIVE presentation if set."
  (let ((tessera-mu4e-headers--updating t)
        (inhibit-read-only t)
        (inhibit-modification-hooks t)
        (origin (copy-marker (line-beginning-position)))
        (offset (- (point) (line-beginning-position))))
    (unwind-protect
        (progn
          (tessera-entry-clear-current)
          (tessera-entry-clear-layout)
          (remove-overlays nil nil 'tessera-mu4e-footer t)
          (unless native
            (setq tessera-mu4e-headers--leading-width
                  (tessera-mu4e-headers--measure)))
          (goto-char (point-min))
          (while (< (point) (point-max))
            (tessera-mu4e-headers--sync-line native)
            (forward-line 1))
          (when tessera-mu4e-headers--active
            (tessera-mu4e-headers--hide-footer)))
      (goto-char origin)
      (goto-char (min (+ (point) offset) (line-end-position)))
      (set-marker origin nil))))

(defun tessera-mu4e-headers--appearance ()
  "Return native and shared options affecting the presentation."
  (list (when-let* ((window (get-buffer-window (current-buffer))))
          (window-body-width window))
        mu4e-search-threads mu4e-headers-visible-flags
        mu4e-headers-show-target mu4e-headers-from-or-to-prefix
        mu4e-headers-date-format mu4e-headers-time-format
        custom-enabled-themes tessera-entry-layout
        tessera-glyph-style tessera-glyph-color
        tessera-entry-safe-gap tessera-entry-left-padding
        tessera-entry-right-padding tessera-entry-top-padding
        tessera-entry-bottom-padding tessera-entry-segment-gap
        tessera-entry-flex-gap-min-width))

(defun tessera-mu4e-headers--changed (_start _end _old)
  "Record native buffer changes without reacting to our own edits."
  (unless tessera-mu4e-headers--updating
    (setq tessera-mu4e-headers--dirty t)))

(defun tessera-mu4e-headers--refresh (&optional _window)
  "Synchronize changed messages before display or after a command."
  (when (and tessera-mu4e-headers--active
             (not tessera-mu4e-headers--updating))
    (let* ((tessera-mu4e-headers--updating t)
           (appearance (tessera-mu4e-headers--appearance)))
      (when (or tessera-mu4e-headers--dirty
                (not (equal appearance
                            tessera-mu4e-headers--appearance)))
        (tessera-mu4e-headers--sync mu4e-search-threads)
        (setq tessera-mu4e-headers--dirty nil
              tessera-mu4e-headers--appearance appearance))
      (if mu4e-search-threads
          (progn
            (if header-line-format
                (setq tessera-mu4e-headers--native-header-line
                      header-line-format)
              (setq header-line-format
                    tessera-mu4e-headers--native-header-line))
            (hl-line-mode
             (if tessera-mu4e-headers--saved-hl-line 1 -1)))
        (when header-line-format
          (setq tessera-mu4e-headers--native-header-line
                header-line-format))
        (setq header-line-format nil)
        (hl-line-mode -1)
        (tessera-entry-highlight-current)))))

(defun tessera-mu4e-headers--enable ()
  "Enable reversible layout synchronization in this headers buffer."
  (unless tessera-mu4e-headers--active
    (setq tessera-mu4e-headers--active t
          tessera-mu4e-headers--dirty t
          tessera-mu4e-headers--native-header-line header-line-format
          tessera-mu4e-headers--saved-hl-line hl-line-mode
          tessera-mu4e-headers--saved-layout
          (list (local-variable-p 'tessera-entry-layout)
                tessera-entry-layout))
    (setq-local tessera-entry-layout 'two-line)
    (add-hook 'after-change-functions
              #'tessera-mu4e-headers--changed nil t)
    (add-hook 'post-command-hook
              #'tessera-mu4e-headers--refresh t t)
    (add-hook 'pre-redisplay-functions
              #'tessera-mu4e-headers--refresh nil t)
    (add-hook 'change-major-mode-hook
              #'tessera-mu4e-headers--disable nil t)
    (tessera-mu4e-headers--refresh)))

(defun tessera-mu4e-headers--disable ()
  "Restore native text, marks, highlight, and header line."
  (when tessera-mu4e-headers--active
    (setq tessera-mu4e-headers--active nil)
    (remove-hook 'after-change-functions
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
    (if (car tessera-mu4e-headers--saved-layout)
        (setq-local tessera-entry-layout
                    (cadr tessera-mu4e-headers--saved-layout))
      (kill-local-variable 'tessera-entry-layout))))

(provide 'tessera-mu4e-headers)
;;; tessera-mu4e-headers.el ends here
