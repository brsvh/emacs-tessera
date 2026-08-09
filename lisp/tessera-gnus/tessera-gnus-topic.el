;;; tessera-gnus-topic.el --- Tessera interface for Gnus Topic  -*- lexical-binding: t; -*-

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

;; Tessera presentation for `gnus-topic-mode' in a Gnus Group buffer.

;;; Code:

(require 'gnus-topic)
(require 'subr-x)
(require 'tessera-gnus-group)
(require 'tessera-ui)

(defface tessera-gnus-topic-name
  '((t :weight bold))
  "Face for a Gnus topic name."
  :group 'tessera-gnus)

(defface tessera-gnus-topic-disclosure
  '((t :inherit shadow))
  "Face for a Gnus topic disclosure indicator."
  :group 'tessera-gnus)

(defface tessera-gnus-topic-statistics
  '((t :inherit shadow))
  "Face for collapsed Gnus topic statistics."
  :group 'tessera-gnus)

(defface tessera-gnus-topic-unread
  '((t :inherit (error tessera-gnus-topic-statistics)))
  "Face for a nonzero Gnus topic unread count."
  :group 'tessera-gnus)

(defun tessera-gnus-topic--set-tab-fold (symbol value)
  "Set Topic TAB option SYMBOL to VALUE in active Group buffers."
  (set-default symbol value)
  (when (fboundp 'tessera-gnus-topic--update-tab-map)
    (dolist (buffer
             (match-buffers '(derived-mode . gnus-group-mode)))
      (with-current-buffer buffer
        (when (bound-and-true-p tessera-gnus-topic--installed-p)
          (tessera-gnus-topic--update-tab-map))))))

(defcustom tessera-gnus-topic-tab-fold nil
  "Whether TAB folds a topic instead of nesting it.

When nil, preserve the native `gnus-topic-indent' command.  This
option affects TAB only on a topic line."
  :type 'boolean
  :set #'tessera-gnus-topic--set-tab-fold
  :group 'tessera-gnus)

(defconst tessera-gnus-topic--line-format
  "%u&tessera-gnus-topic;\n"
  "Gnus Topic line format installed by Tessera.")

(defconst tessera-gnus-topic--top-padding 6
  "Topic padding above its content, in pixels.")

(defconst tessera-gnus-topic--bottom-padding 6
  "Topic padding below its content, in pixels.")

(defconst tessera-gnus-topic--name-height 1.4
  "Height of the root topic name.")

(defconst tessera-gnus-topic--name-height-step 0.05
  "Topic name height decrease per native level.")

(defconst tessera-gnus-topic--minimum-name-height 1.0
  "Minimum topic name height.")

(defvar-local tessera-gnus-topic--installed-p nil
  "Non-nil when Tessera owns the current Topic presentation.")

(defvar-local tessera-gnus-topic--original-line-format nil
  "Topic line format saved before Tessera installation.")

(defvar-local tessera-gnus-topic--original-line-format-local-p nil
  "Non-nil when the saved Topic line format was buffer-local.")

(defvar-local tessera-gnus-topic--window-overlays nil
  "Window-local overlays used to truncate Topic names.")

(defvar-local
    tessera-gnus-topic--original-overriding-map-alist nil
  "Overriding minor-mode maps saved before Tessera installation.")

(defvar-local
    tessera-gnus-topic--original-overriding-map-alist-local-p nil
  "Non-nil when the saved overriding maps were buffer-local.")

(defvar tessera-gnus-topic--enabled-p nil
  "Non-nil when Tessera Topic integration is enabled.")

(defvar tessera-gnus-topic--mouse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'tessera-gnus-topic--mouse-fold)
    map)
  "Keymap used on a Tessera topic row.")

(defvar tessera-gnus-topic--tab-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'tessera-gnus-topic--tab)
    (define-key map (kbd "<tab>") #'tessera-gnus-topic--tab)
    map)
  "Keymap overriding TAB on a Tessera topic row.")

(defun tessera-gnus-topic--element (text element &optional face)
  "Return a copy of TEXT named ELEMENT and optionally using FACE."
  (let ((text (copy-sequence text)))
    (put-text-property
     0 (length text) 'tessera-element element text)
    (when face
      (add-face-text-property 0 (length text) face nil text))
    text))

(defun tessera-gnus-topic--height (topic-level)
  "Return the name height for native TOPIC-LEVEL."
  (max tessera-gnus-topic--minimum-name-height
       (- tessera-gnus-topic--name-height
          (* topic-level tessera-gnus-topic--name-height-step))))

(defun tessera-gnus-topic--name (topic-name topic-level)
  "Return TOPIC-NAME presented at native TOPIC-LEVEL."
  (let ((text
         (tessera-gnus-topic--element
          topic-name 'topic.name)))
    (add-text-properties
     0 (length text)
     (list 'face
           (list
            (list :height
                  (tessera-gnus-topic--height topic-level))
            'tessera-gnus-topic-name)
           'help-echo topic-name)
     text)
    text))

(defun tessera-gnus-topic--statistics (unread groups)
  "Return collapsed Topic statistics for UNREAD and GROUPS."
  (let* ((unread-count
          (tessera-gnus-topic--element
           (number-to-string unread)
           'topic.statistics.unread-count
           (if (> unread 0)
               'tessera-gnus-topic-unread
             'tessera-gnus-topic-statistics)))
         (unread-text
          (tessera-gnus-topic--element
           "unread" 'topic.statistics.unread-text))
         (separator
          (tessera-gnus-topic--element
           " " 'topic.statistics.separator))
         (special-separator
          (tessera-gnus-topic--element
           " · " 'topic.statistics.special-separator))
         (groups-count
          (tessera-gnus-topic--element
           (number-to-string groups)
           'topic.statistics.groups-count))
         (groups-text
          (tessera-gnus-topic--element
           (if (= groups 1) "group" "groups")
           'topic.statistics.groups-text))
         (text
          (concat unread-count separator unread-text
                  special-separator groups-count separator
                  groups-text)))
    (add-face-text-property
     0 (length text) 'tessera-gnus-topic-statistics t text)
    (put-text-property
     0 (length text) 'tessera-parent-element
     'topic.statistics text)
    text))

(defun tessera-gnus-topic--row
    (topic-name topic-level expanded unread groups)
  "Return a Topic row from native presentation values.

TOPIC-NAME and TOPIC-LEVEL identify the topic.  EXPANDED is non-nil
when its children are visible.  UNREAD and GROUPS include subtopics."
  (let* ((top-padding
          (tessera-ui-vertical-padding
           'tessera-gnus-topic-name 'topic.top-padding
           tessera-gnus-topic--top-padding 0))
         (safety-gap
          (tessera-gnus-topic--element
           (tessera-ui-entry-leading-safety-gap)
           'topic.safety-gap))
         (left-padding
          (tessera-gnus-topic--element
           (tessera-ui-entry-padding 'entry.left-padding)
           'topic.left-padding))
         (disclosure
          (tessera-gnus-topic--element
           (if expanded
               (propertize
                " " 'display
                `(space :width
                        (,(string-pixel-width
                           tessera-gnus-group--disclosure-glyph))))
             tessera-gnus-group--disclosure-glyph)
           'topic.disclosure
           'tessera-gnus-topic-disclosure))
         (separator
          (tessera-gnus-topic--element " " 'topic.separator))
         (display-name
          (tessera-gnus-topic--name topic-name topic-level))
         (statistics
          (and (not expanded)
               (tessera-gnus-topic--statistics unread groups)))
         (right-padding
          (tessera-gnus-topic--element
           (tessera-ui-entry-padding 'entry.right-padding)
           'topic.right-padding))
         (trailing-gap
          (tessera-gnus-topic--element
           (tessera-ui-entry-trailing-safety-gap)
           'topic.safety-gap))
         (right (concat statistics right-padding trailing-gap))
         (flex-gap
          (let ((gap (tessera-ui-entry-flex-gap right)))
            (put-text-property
             0 (length gap) 'tessera-element 'topic.flex-gap gap)
            gap))
         (bottom-padding
          (tessera-ui-vertical-padding
           'tessera-gnus-topic-name 'topic.bottom-padding
           0 tessera-gnus-topic--bottom-padding))
         (row
          (concat top-padding safety-gap left-padding disclosure
                  separator display-name flex-gap right
                  bottom-padding)))
    (add-text-properties
     0 (length row)
     (list 'tessera-parent-element 'topic
           'tessera-context topic-name
           'mouse-face 'highlight
           'keymap tessera-gnus-topic--mouse-map)
     row)
    row))

(defun gnus-user-format-function-tessera-gnus-topic (_header)
  "Return the Tessera row for the current native Gnus topic."
  (tessera-gnus-topic--row
   (symbol-value 'name)
   (symbol-value 'level)
   (string-empty-p (symbol-value 'visible))
   (symbol-value 'total-number-of-articles)
   (symbol-value 'total-number-of-groups)))

(defun tessera-gnus-topic--mouse-fold (event)
  "Fold the native Gnus topic selected by mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (when (gnus-group-topic-p)
    (call-interactively #'gnus-topic-select-group)))

(defun tessera-gnus-topic--tab ()
  "Fold a topic row or preserve native TAB elsewhere."
  (interactive)
  (if (gnus-group-topic-p)
      (call-interactively #'gnus-topic-select-group)
    (call-interactively #'gnus-topic-indent)))

(defun tessera-gnus-topic--update-tab-map ()
  "Update the buffer-local Topic mode overriding map."
  (if (not tessera-gnus-topic-tab-fold)
      (tessera-gnus-topic--restore-tab-map)
    (let* ((maps
            (copy-sequence
             tessera-gnus-topic--original-overriding-map-alist))
           (original-map
            (cdr (assq 'gnus-topic-mode maps))))
      (setq maps (assq-delete-all 'gnus-topic-mode maps))
      (push
       (cons
        'gnus-topic-mode
        (make-composed-keymap
         tessera-gnus-topic--tab-map
         (or original-map gnus-topic-mode-map)))
       maps)
      (setq-local minor-mode-overriding-map-alist maps))))

(defun tessera-gnus-topic--restore-tab-map ()
  "Restore the overriding maps saved before Tessera installation."
  (if tessera-gnus-topic--original-overriding-map-alist-local-p
      (setq-local
       minor-mode-overriding-map-alist
       (copy-sequence
        tessera-gnus-topic--original-overriding-map-alist))
    (kill-local-variable 'minor-mode-overriding-map-alist)))

(defun tessera-gnus-topic--delete-window-overlays
    (&optional window)
  "Delete Topic presentation overlays for WINDOW.

Delete every presentation overlay when WINDOW is nil."
  (let (remaining)
    (dolist (overlay tessera-gnus-topic--window-overlays)
      (if (or (not (overlay-buffer overlay))
              (not (window-live-p
                    (overlay-get overlay 'window)))
              (not window)
              (eq (overlay-get overlay 'window) window))
          (delete-overlay overlay)
        (push overlay remaining)))
    (setq tessera-gnus-topic--window-overlays
          (nreverse remaining))))

(defun tessera-gnus-topic--present-row (window)
  "Present the Topic row at point in WINDOW."
  (let* ((start (line-beginning-position))
         (end (line-end-position))
         (name-start
          (text-property-any start end 'tessera-element 'topic.name))
         (flex-start
          (text-property-any
           start end 'tessera-element 'topic.flex-gap)))
    (when (and name-start flex-start)
      (let* ((name-end
              (next-single-property-change
               name-start 'tessera-element nil end))
             (flex-end
              (next-single-property-change
               flex-start 'tessera-element nil end))
             (name
              (buffer-substring name-start name-end))
             (available
              (max
               0
               (- (window-body-width window t)
                  (string-pixel-width
                   (buffer-substring start name-start))
                  (string-pixel-width
                   (buffer-substring flex-end end)))))
             (display
              (tessera-ui-truncate-pixels name available)))
        (unless (string= name display)
          (let ((overlay (make-overlay name-start name-end)))
            (overlay-put overlay 'window window)
            (overlay-put overlay 'display display)
            (overlay-put overlay 'evaporate t)
            (overlay-put
             overlay 'help-echo
             (substring-no-properties name))
            (push overlay tessera-gnus-topic--window-overlays)))))))

(defun tessera-gnus-topic--present-rows (&optional window)
  "Present every Topic row for WINDOW."
  (when-let* ((window
               (or window
                   (get-buffer-window (current-buffer) t))))
    (tessera-gnus-topic--delete-window-overlays window)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (get-text-property (point) 'gnus-topic)
          (tessera-gnus-topic--present-row window))
        (forward-line 1)))))

(defun tessera-gnus-topic--present-visible-windows ()
  "Present Topic rows in every window showing this buffer."
  (dolist (window
           (get-buffer-window-list (current-buffer) nil t))
    (tessera-gnus-topic--present-rows window)))

(defun tessera-gnus-topic--window-state-change (window)
  "Present Topic rows after a state change in WINDOW."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer)))
    (tessera-gnus-topic--present-rows window)))

(defun tessera-gnus-topic-refresh ()
  "Refresh the Tessera presentation in the current Topic buffer."
  (when (and tessera-gnus-topic--installed-p
             (bound-and-true-p gnus-topic-mode))
    (gnus-update-format-specifications nil 'topic)
    (tessera-gnus-group-refresh)))

(defun tessera-gnus-topic--install ()
  "Install Tessera in the current Gnus Topic buffer."
  (when (and (not tessera-gnus-topic--installed-p)
             (derived-mode-p 'gnus-group-mode)
             (bound-and-true-p gnus-topic-mode))
    (setq
     tessera-gnus-topic--original-line-format-local-p
     (local-variable-p 'gnus-topic-line-format)
     tessera-gnus-topic--original-line-format
     gnus-topic-line-format
     tessera-gnus-topic--original-overriding-map-alist-local-p
     (local-variable-p 'minor-mode-overriding-map-alist)
     tessera-gnus-topic--original-overriding-map-alist
     (copy-sequence minor-mode-overriding-map-alist)
     tessera-gnus-topic--installed-p t)
    (setq-local gnus-topic-line-format
                tessera-gnus-topic--line-format)
    (gnus-update-format-specifications nil 'topic)
    (tessera-gnus-topic--update-tab-map)
    (add-hook 'gnus-group-prepare-hook
              #'tessera-gnus-topic--present-visible-windows t t)
    (add-hook 'window-state-change-functions
              #'tessera-gnus-topic--window-state-change t t)
    (tessera-gnus-topic-refresh)))

(defun tessera-gnus-topic--restore ()
  "Restore the native Topic presentation in the current buffer."
  (when tessera-gnus-topic--installed-p
    (remove-hook 'gnus-group-prepare-hook
                 #'tessera-gnus-topic--present-visible-windows t)
    (remove-hook 'window-state-change-functions
                 #'tessera-gnus-topic--window-state-change t)
    (tessera-gnus-topic--delete-window-overlays)
    (tessera-gnus-topic--restore-tab-map)
    (if tessera-gnus-topic--original-line-format-local-p
        (setq-local
         gnus-topic-line-format
         tessera-gnus-topic--original-line-format)
      (kill-local-variable 'gnus-topic-line-format))
    (gnus-update-format-specifications nil 'topic)
    (setq tessera-gnus-topic--installed-p nil
          tessera-gnus-topic--original-line-format nil
          tessera-gnus-topic--original-line-format-local-p nil
          tessera-gnus-topic--original-overriding-map-alist nil
          tessera-gnus-topic--original-overriding-map-alist-local-p
          nil)
    (when (derived-mode-p 'gnus-group-mode)
      (gnus-group-list-groups))))

(defun tessera-gnus-topic--mode-changed ()
  "Install or restore Tessera after native Topic mode changes."
  (if (bound-and-true-p gnus-topic-mode)
      (tessera-gnus-topic--install)
    (tessera-gnus-topic--restore)))

(defun tessera-gnus-topic-enable ()
  "Enable Tessera in existing and future Gnus Topic buffers."
  (unless tessera-gnus-topic--enabled-p
    (setq tessera-gnus-topic--enabled-p t)
    (add-hook 'gnus-topic-mode-hook
              #'tessera-gnus-topic--mode-changed)
    (dolist (buffer
             (match-buffers '(derived-mode . gnus-group-mode)))
      (with-current-buffer buffer
        (when (bound-and-true-p gnus-topic-mode)
          (tessera-gnus-topic--install))))))

(defun tessera-gnus-topic-disable ()
  "Disable Tessera in existing Gnus Topic buffers."
  (when tessera-gnus-topic--enabled-p
    (setq tessera-gnus-topic--enabled-p nil)
    (remove-hook 'gnus-topic-mode-hook
                 #'tessera-gnus-topic--mode-changed)
    (dolist (buffer
             (match-buffers '(derived-mode . gnus-group-mode)))
      (with-current-buffer buffer
        (tessera-gnus-topic--restore)))))

(provide 'tessera-gnus-topic)
;;; tessera-gnus-topic.el ends here
