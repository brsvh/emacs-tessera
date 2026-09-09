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

;; Render Gnus summary articles with Tessera.  Four native marks
;; remain at fixed offsets and share the first status glyph.
;; The visible entry is refreshed from those marks after changes.

;;; Code:

(require 'tessera)
(require 'tessera-gnus-data)
(require 'gnus-sum)
(require 'gnus-spec)
(require 'subr-x)
(require 'seq)

(defvar gnus-tmp-unread)
(defvar gnus-tmp-replied)
(defvar gnus-tmp-downloaded)
(defvar gnus-tmp-score-char)
(defvar gnus-tmp-from)

(defgroup tessera-gnus-summary nil
  "Tessera entries in Gnus summary buffers."
  :group 'tessera
  :prefix "tessera-gnus-summary-")

(defface tessera-gnus-summary-subject-face
  '((t :inherit gnus-summary-normal-read :extend nil))
  "Face for read article subjects."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-unread-subject-face
  '((t :inherit gnus-summary-normal-unread :extend nil))
  "Face for unread article subjects."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-author-face
  '((t :inherit font-lock-variable-name-face))
  "Face for article authors."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-date-face
  '((t :inherit font-lock-constant-face))
  "Face for article dates."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-label-face
  '((t :inherit font-lock-keyword-face))
  "Face for article labels from every supported source."
  :group 'tessera-gnus-summary)

(defconst tessera-gnus-summary--states
  '((status 0
            (unread gnus-unread-mark "●" "email" accent "Unread")
            (ticked gnus-ticked-mark "★" "star" attention "Ticked")
            (dormant gnus-dormant-mark "◇" "sleep" muted "Dormant")
            (expirable gnus-expirable-mark
                       "◷" "calendar-clock-outline"
                       warning "Expirable")
            (spam gnus-spam-mark "!" "shield-alert-outline"
                  negative "Spam")
            (downloadable gnus-downloadable-mark "↓" "download"
                          informational "Downloadable")
            (unsendable gnus-unsendable-mark "↛" "email-off-outline"
                        negative "Unsendable")
            (killed gnus-killed-mark "×" "close-circle-outline"
                    muted "Killed")
            (kill-file gnus-kill-file-mark "⊗" "filter-remove"
                       muted "Killed by rule")
            (low-score gnus-low-score-mark "⇣" "filter-check-outline"
                       muted "Read by score")
            (catchup gnus-catchup-mark "✓" "playlist-check"
                     muted "Caught up")
            (ancient gnus-ancient-mark
                     "◌" "file-clock-outline"
                     muted "Ancient")
            (sparse gnus-sparse-mark "⋯" "file-hidden"
                    muted "Sparse")
            (canceled gnus-canceled-mark "⊘" "cancel"
                      negative "Canceled")
            (duplicate gnus-duplicate-mark "⧉" "content-copy"
                       muted "Duplicate")
            (del gnus-del-mark "○" "email-open-outline"
                 muted "Marked as read")
            (read gnus-read-mark "○" "email-open-outline"
                  muted "Read"))
    (secondary 1
               (processable gnus-process-mark
                            "◆" "clipboard-clock-outline"
                            attention "Marked for processing")
               (cached gnus-cached-mark "▣" "database"
                       positive "Cached")
               (replied gnus-replied-mark "↩" "reply"
                        positive "Replied")
               (forwarded gnus-forwarded-mark "↪" "forward"
                          informational "Forwarded")
               (saved gnus-saved-mark "▣" "content-save"
                      positive "Saved")
               (unseen gnus-unseen-mark "✦" "new-box"
                       accent "Unseen"))
    (availability 2
                  (undownloaded gnus-undownloaded-mark
                                "↓" "cloud-outline"
                                muted "Not downloaded")
                  (downloaded gnus-downloaded-mark
                              "✓" "cloud-download"
                              positive "Downloaded"))
    (score 3
           (low gnus-score-below-mark "↓" "arrow-down-bold"
                muted "Below default score")
           (high gnus-score-over-mark "↑" "arrow-up-bold"
                 attention "Above default score")))
  "Native mark variables and visual variants, grouped by slot.")

(defvar tessera-gnus-summary--metadata nil
  "Metadata dynamically bound while rendering one article.")

(defvar-local tessera-gnus-summary--active nil
  "Whether Tessera is active in this summary buffer.")

(defvar-local tessera-gnus-summary--saved-settings nil
  "Original values and locality of settings replaced by Tessera.")

(defvar-local tessera-gnus-summary--dirty nil
  "Whether native buffer changes need synchronization.")

(defvar-local tessera-gnus-summary--updating nil
  "Non-nil while Tessera is updating its own presentation.")

(defvar-local tessera-gnus-summary--folds nil
  "Native thread hiding overlays seen at the last synchronization.")

(defvar-local tessera-gnus-summary--appearance nil
  "Appearance used for the last synchronized entry rendering.")

(defun tessera-gnus-summary--context (header buffer window)
  "Return the entry context for HEADER in BUFFER and WINDOW."
  (make-tessera-entry-context
   :backend 'gnus-summary :object header
   :buffer buffer :window window
   :metadata tessera-gnus-summary--metadata))

(defun tessera-gnus-summary--state (slot context)
  "Return the native state of SLOT in CONTEXT."
  (let* ((spec (assq slot tessera-gnus-summary--states))
         (marks (plist-get (tessera-entry-context-metadata context)
                           :marks))
         (mark (aref marks (cadr spec)))
         (variant
          (seq-find
           (lambda (entry)
             (eq mark (symbol-value (nth 1 entry))))
           (cddr spec))))
    (cond (variant (car variant))
          ((eq mark gnus-no-mark) nil)
          (t 'unknown))))

(defun tessera-gnus-summary--slot (spec)
  "Build a native status slot from SPEC."
  (make-tessera-glyph-slot
   :name (car spec) :width 2 :align 'center
   :selector (apply-partially #'tessera-gnus-summary--state
                              (car spec))
   :glyphs
   (append
    (mapcar
     (lambda (entry)
       (pcase-let ((`(,id ,mark ,unicode ,icon ,semantic ,help)
                    entry))
         (list id
               :glyph (make-tessera-glyph
                       :ascii (char-to-string (symbol-value mark))
                       :unicode unicode
                       :nerd-icons
                       (list :function 'nerd-icons-mdicon
                             :name
                             (concat "nf-md-"
                                     (string-replace "-" "_" icon)))
                       :semantic semantic)
               :help-echo help)))
     (cddr spec))
    (list
     (list 'unknown :glyph
           (make-tessera-glyph
            :ascii "?" :unicode "?"
            :nerd-icons '(:function nerd-icons-mdicon
                                    :name "nf-md-help_circle")
            :semantic 'warning)
           :help-echo "Unrecognized Gnus mark")))))

(defun tessera-gnus-summary--subject (context)
  "Return the article subject in CONTEXT."
  (let ((subject (mail-header-subject
                  (tessera-entry-context-object context))))
    (propertize
     (if (string-empty-p subject) "(no subject)" subject)
     'face
     (if (memq (tessera-gnus-summary--state 'status context)
               '(unread ticked dormant))
         'tessera-gnus-summary-unread-subject-face
       'tessera-gnus-summary-subject-face)
     'mouse-face 'highlight 'help-echo subject)))

(defun tessera-gnus-summary--author (context)
  "Return the native author representation in CONTEXT."
  (propertize
   (plist-get (tessera-entry-context-metadata context) :author)
   'face 'tessera-gnus-summary-author-face
   'help-echo (mail-header-from
               (tessera-entry-context-object context))))

(defun tessera-gnus-summary--date (context)
  "Return the native formatted date in CONTEXT."
  (let ((date (mail-header-date
               (tessera-entry-context-object context))))
    (propertize
     (condition-case nil (gnus-user-date date)
       (error date))
     'face 'tessera-gnus-summary-date-face 'help-echo date)))

(defun tessera-gnus-summary--labels (context)
  "Return the labels in CONTEXT as one optional text segment."
  (when-let* ((labels (tessera-gnus-data-labels
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
  (let ((state (plist-get
                (tessera-gnus-data-content
                 (tessera-entry-context-object context)) key)))
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
        (let* ((content (tessera-gnus-data-content (car entry)))
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
  "Return independent attachment, signature, and encryption slots."
  (mapcar
   (lambda (spec)
     (pcase-let* ((`(,name ,key ,ascii ,unicode ,icon ,label) spec)
                  (glyph
                   (make-tessera-glyph
                    :ascii ascii :unicode unicode
                    :nerd-icons (list :function 'nerd-icons-mdicon
                                      :name icon)
                    :semantic 'informational))
                  (help (apply-partially
                         #'tessera-gnus-summary--content-help
                         key label)))
       (make-tessera-glyph-slot
        :name name :width 2 :align 'center
        :selector (apply-partially
                   #'tessera-gnus-summary--content-state key)
        :glyphs
        (append
         (list (list 'present :glyph glyph :help-echo help))
         (unless (eq key :attachment)
           (list
            (list 'processed :glyph glyph :help-echo help)
            (list 'error
                  :glyph
                  (make-tessera-glyph
                   :ascii "!" :unicode "!"
                   :nerd-icons
                   '(:function nerd-icons-mdicon
                               :name "nf-md-alert_circle_outline")
                   :semantic 'negative)
                  :help-echo help)))))))
   '((attachment :attachment "a" "📎" "nf-md-paperclip" "Attachment")
     (signature :signature "S" "✍" "nf-md-file_sign" "Signature")
     (encryption :encryption "E" "🔒" "nf-md-lock_outline"
                 "Encrypted content"))))

(defun tessera-gnus-summary--register ()
  "Register the Gnus summary entry backend."
  (tessera-entry-register
   'gnus-summary :context #'tessera-gnus-summary--context
   :segments
   '((subject . tessera-gnus-summary--subject)
     (author . tessera-gnus-summary--author)
     (date . tessera-gnus-summary--date)
     (labels . tessera-gnus-summary--labels))
   :glyph-slots
   (append (mapcar #'tessera-gnus-summary--slot
                   tessera-gnus-summary--states)
           (tessera-gnus-summary--content-slots))
   :layouts
   (list
    (cons 'single-line
          (make-tessera-entry-layout
           :glyph-slots-align 'right
           :main-glyph-slots
           '(score availability secondary status)
           :main-left-segments
           '((author :max-width 20 :truncate tail :optional t)
             (subject :grow t :min-width 4 :truncate tail)
             (:slots (attachment :optional t)
                     (signature :optional t)
                     (encryption :optional t)))
           :main-right-segments
           '((labels :grow t :max-width 24 :min-width 0
                     :truncate tail :priority -1 :optional t)
             date)))
    (cons 'two-line
          (make-tessera-entry-layout
           :glyph-slots-align 'right
           :main-glyph-slots '(secondary status)
           :main-left-segments
           '((subject :grow t :min-width 4 :truncate tail))
           :main-right-segments
           '((labels :grow t :max-width 24 :min-width 0
                     :truncate tail :priority -1 :optional t))
           :extra-glyph-slots '(score availability)
           :extra-left-segments
           '((author :grow t :min-width 4 :truncate tail)
             (:slots (attachment :optional t)
                     (signature :optional t)
                     (encryption :optional t)))
           :extra-right-segments '(date))))))

(defun tessera-gnus-summary--render (header metadata)
  "Render HEADER with its native METADATA."
  (let* ((tessera-gnus-summary--metadata metadata)
         (prefix (propertize (copy-sequence
                              (plist-get metadata :marks))
                             'display ""))
         (result
          (tessera-entry-render
           'gnus-summary header
           (get-buffer-window (current-buffer)) prefix)))
    ;; The native mark prefix also carries the first visible glyph.
    ;; A fully hidden prefix before a display newline confuses
    ;; Emacs's backward visual-line motion.
    (let ((glyph (substring result 4 5)))
      (add-text-properties 0 4 (text-properties-at 4 result) result)
      (if-let* ((display (get-text-property 0 'display glyph)))
          (put-text-property 0 1 'display display result)
        (remove-text-properties 0 1 '(display nil) result))
      (compose-string result 0 1 (aref glyph 0))
      (put-text-property 1 5 'display "" result))
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

(defun tessera-gnus-summary-format-entry (header)
  "Return a Tessera Gnus summary representation of HEADER.
The first four characters retain Gnus's native status marks.
Their first character displays the first composed glyph, preserving
mark discovery, in-place updates, and visual-line navigation."
  (tessera-gnus-summary--render
   header
   (list :marks (string gnus-tmp-unread gnus-tmp-replied
                        gnus-tmp-downloaded gnus-tmp-score-char)
         :author
         (gnus-summary-from-or-to-or-newsgroups
          header gnus-tmp-from)
         )))

;; Gnus user format names are part of its public format protocol.
(defalias 'gnus-user-format-function-tessera
  #'tessera-gnus-summary-format-entry)

(defun tessera-gnus-summary--appearance ()
  "Return the current width and shared appearance settings."
  (list
   (when-let* ((window (get-buffer-window (current-buffer))))
     (window-body-width window))
   tessera-entry-layout tessera-glyph-style tessera-glyph-color
   tessera-entry-safe-gap tessera-entry-left-padding
   tessera-entry-right-padding tessera-entry-top-padding
   tessera-entry-bottom-padding tessera-entry-segment-gap
   tessera-entry-flex-gap-min-width))

(defun tessera-gnus-summary--restore-faces (start end)
  "Restore Tessera content faces between START and END.
Gnus applies its native row face before running the update hook."
  (tessera-entry-clear-current)
  (with-silent-modifications
    (let ((inhibit-read-only t))
      (while (< start end)
        (let ((next (next-single-property-change
                     start 'tessera-gnus-summary-face nil end))
              (saved (get-text-property
                      start 'tessera-gnus-summary-face)))
          (when saved
            (put-text-property start next 'face (car saved)))
          (setq start next))))))

(defun tessera-gnus-summary--folds ()
  "Return the native thread hiding overlays in buffer order."
  (sort
   (seq-filter
    (lambda (overlay)
      (eq (overlay-get overlay 'invisible) 'gnus-sum))
    (overlays-in (point-min) (point-max)))
   (lambda (left right)
     (< (overlay-start left) (overlay-start right)))))

(defun tessera-gnus-summary--sync-buffer (&optional force)
  "Synchronize all entries, preserving point within its article.
FORCE also redraws entries with unchanged marks."
  (let ((origin (copy-marker (line-beginning-position)))
        (offset (- (point) (line-beginning-position)))
        (tessera-gnus-summary--updating t))
    (unwind-protect
        (progn
          (goto-char (point-min))
          (while (< (point) (point-max))
            (tessera-gnus-summary--sync-line force)
            (forward-line 1)))
      (goto-char origin)
      (goto-char (min (+ (point) offset) (line-end-position)))
      (set-marker origin nil))))

(defun tessera-gnus-summary--sync-line (&optional force)
  "Synchronize the current logical article line.
FORCE also redraws entries whose native marks have not changed."
  (let* ((start (line-beginning-position))
         (end (line-end-position))
         (entry (get-text-property
                 start 'tessera-gnus-summary-entry)))
    (when (and entry (>= (- end start) 4))
      (let* ((marks (buffer-substring-no-properties
                     start (+ start 4)))
             (metadata (cdr entry))
             (inhibit-read-only t)
             (inhibit-modification-hooks t))
        (when (or force
                  (not (equal marks (plist-get metadata :marks))))
          (tessera-entry-clear-current)
          (tessera-entry-clear-layout start (1+ end))
          (let* ((updated (plist-put (copy-sequence metadata)
                                     :marks marks))
                 (rendered (tessera-gnus-summary--render
                            (car entry) updated))
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
            (setq end (point))
            (add-text-properties
             start (1+ end)
             (list 'gnus-number number 'gnus-intangible intangible))))
        (tessera-gnus-summary--restore-faces start end)
        (if (invisible-p start)
            (tessera-entry-clear-layout start (1+ end))
          (tessera-entry-apply-layout start end))))))

(defun tessera-gnus-summary--article-updated ()
  "Observe the displayed article and refresh its summary entry."
  ;; Gnus also runs its article preparation hook in the summary.
  (if (derived-mode-p 'gnus-summary-mode)
      (when (get-buffer gnus-article-buffer)
        (with-current-buffer gnus-article-buffer
          (tessera-gnus-summary--article-updated)))
    (when (and (derived-mode-p 'gnus-article-mode)
               gnus-summary-buffer
               (buffer-live-p (get-buffer gnus-summary-buffer)))
      (let ((handles gnus-article-mime-handles)
            (article-buffer (current-buffer)))
        (with-current-buffer gnus-summary-buffer
          (when (and tessera-gnus-summary--active
                     gnus-current-headers)
            (with-current-buffer article-buffer
              (add-hook 'post-command-hook
                        #'tessera-gnus-summary--article-updated t t))
            (when (tessera-gnus-data-observe
                   gnus-current-headers handles)
              (save-excursion
                (when-let* ((position
                             (text-property-any
                              (point-min) (point-max) 'gnus-number
                              (mail-header-number
                               gnus-current-headers))))
                  (goto-char position)
                  (let ((tessera-gnus-summary--updating t))
                    (tessera-gnus-summary--sync-line t))))
              (tessera-entry-highlight-current))))))))

(defun tessera-gnus-summary--update-line ()
  "Synchronize the article just updated by Gnus."
  (when (and tessera-gnus-summary--active
             (not tessera-gnus-summary--updating))
    (let ((tessera-gnus-summary--updating t))
      (save-excursion (tessera-gnus-summary--sync-line)))))

(defun tessera-gnus-summary--prepare ()
  "Attach entry layouts after Gnus has generated a summary."
  (when tessera-gnus-summary--active
    (tessera-entry-clear-current)
    (tessera-entry-clear-layout)
    (tessera-gnus-summary--sync-buffer)
    (setq tessera-gnus-summary--dirty nil
          tessera-gnus-summary--folds
          (tessera-gnus-summary--folds)
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
    (let* ((appearance (tessera-gnus-summary--appearance))
           (folds (tessera-gnus-summary--folds))
           (force (or (not (equal appearance
                                  tessera-gnus-summary--appearance))
                      (and (symbolp this-command)
                           (string-prefix-p
                            "gnus-registry-"
                            (symbol-name this-command))))))
      (when (or force tessera-gnus-summary--dirty
                (not (equal folds tessera-gnus-summary--folds)))
        (tessera-gnus-summary--sync-buffer force)
        (setq tessera-gnus-summary--appearance appearance
              tessera-gnus-summary--folds folds
              tessera-gnus-summary--dirty nil)))
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

(defun tessera-gnus-summary--enable ()
  "Enable Tessera in the current Gnus summary buffer."
  (unless tessera-gnus-summary--active
    (setq tessera-gnus-summary--saved-settings
          (mapcar
           (lambda (variable)
             (list variable (local-variable-p variable)
                   (symbol-value variable)))
           '(gnus-summary-line-format tessera-entry-layout)))
    (setq-local gnus-summary-line-format "%u&tessera;\n")
    (setq-local tessera-entry-layout 'two-line)
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
    (tessera-gnus-summary--refresh)))

(defun tessera-gnus-summary--disable ()
  "Restore the native summary presentation."
  (when tessera-gnus-summary--active
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
    (tessera-entry-clear-current)
    (tessera-entry-clear-layout)
    (dolist (setting tessera-gnus-summary--saved-settings)
      (pcase-let ((`(,variable ,local ,value) setting))
        (if local
            (set (make-local-variable variable) value)
          (kill-local-variable variable))))
    (setq tessera-gnus-summary--saved-settings nil
          tessera-gnus-summary--appearance nil
          tessera-gnus-summary--folds nil
          tessera-gnus-summary--dirty nil
          tessera-gnus-data--content-cache nil)
    (tessera-gnus-summary--refresh)))

(provide 'tessera-gnus-summary)
;;; tessera-gnus-summary.el ends here
