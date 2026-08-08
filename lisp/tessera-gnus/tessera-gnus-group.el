;;; tessera-gnus-group.el --- Tessera interface for Gnus Group  -*- lexical-binding: t; -*-

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

;; Tessera header-line presentation for `gnus-group-mode'.

;;; Code:

(require 'gnus-group)
(require 'tessera-ui)

(defconst tessera-gnus-group--header-line-format
  '(:eval (tessera-gnus-group--header-line))
  "Header-line format installed in Gnus Group buffers.")

(defvar tessera-gnus-group--enabled-p nil
  "Non-nil when the Tessera Gnus Group interface is enabled.")

(defvar-local tessera-gnus-group--installed-p nil
  "Non-nil when Tessera is installed in this Group buffer.")

(defvar-local tessera-gnus-group--installed-header-line-format nil
  "Exact header-line format object installed by Tessera.")

(defvar-local tessera-gnus-group--original-header-line-format nil
  "Header-line format replaced by Tessera in this buffer.")

(defvar-local tessera-gnus-group--original-header-line-local-p nil
  "Non-nil when the original header-line format was buffer-local.")

(defvar-local tessera-gnus-group--status-state 'success
  "Current native update state presented in the Group header.")

(defvar-local tessera-gnus-group--statistics-cache nil
  "Cached presentation of statistics for all groups.")

(defconst tessera-gnus-group--update-functions
  '(gnus-group-get-new-news
    gnus-group-get-new-news-this-group)
  "Gnus functions which update native group data.")

(defvar tessera-gnus-group--status-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1]
                #'tessera-gnus-group--get-new-news)
    map)
  "Keymap used by the Gnus Group header status.")

(defun tessera-gnus-group--invalidate-statistics ()
  "Invalidate the cached all-group statistics."
  (setq tessera-gnus-group--statistics-cache nil))

(defun tessera-gnus-group--statistics ()
  "Return statistics for all groups known to Gnus."
  (or tessera-gnus-group--statistics-cache
      (let ((groups (length gnus-group-list))
            (total 0)
            (total-incomplete nil)
            (unread 0)
            (unread-incomplete nil))
        (dolist (group gnus-group-list)
          (let ((active (gnus-active group))
                (group-unread (gnus-group-unread group)))
            (if (numberp group-unread)
                (setq unread (+ unread (max 0 group-unread)))
              (setq unread-incomplete t))
            (if active
                (setq total
                      (+ total (range-length (list active))))
              (setq total-incomplete t))))
        (setq tessera-gnus-group--statistics-cache
              (tessera-ui-all-statistics
               unread groups total
               unread-incomplete total-incomplete)))))

(defun tessera-gnus-group--format-status ()
  "Return the presentation of the current Group update status."
  (let (face help-echo text)
    (pcase tessera-gnus-group--status-state
      ('processing
       (setq face 'tessera-header-status-processing
             help-echo "Gnus is fetching new articles"
             text "FETCHING"))
      ('fail
       (setq face 'tessera-header-status-fail
             help-echo
             (concat "The last fetch failed; "
                     "mouse-1: Get new articles")
             text "FETCH FAILED"))
      (_
       (setq face 'tessera-header-status-success
             help-echo "mouse-1: Get new articles"
             text "IDLE")))
    (propertize
     text
     'face face
     'help-echo help-echo
     'keymap tessera-gnus-group--status-map
     'mouse-face 'header-line-highlight)))

(defun tessera-gnus-group--header-line ()
  "Return the Tessera header for the current Group buffer."
  (tessera-ui-header-line
   (tessera-gnus-group--format-status)
   nil
   (tessera-gnus-group--statistics)))

(defun tessera-gnus-group--redraw-status ()
  "Redisplay the current Group status immediately."
  (force-mode-line-update)
  (redisplay))

(defun tessera-gnus-group--set-status (state)
  "Set the Group header status to STATE."
  (when (buffer-live-p gnus-group-buffer)
    (with-current-buffer gnus-group-buffer
      (when tessera-gnus-group--installed-p
        (setq tessera-gnus-group--status-state state)
        (unless (eq state 'processing)
          (tessera-gnus-group--invalidate-statistics))
        (tessera-gnus-group--redraw-status)))))

(defun tessera-gnus-group--update (function &rest args)
  "Call Gnus update FUNCTION with ARGS and present its status."
  (tessera-gnus-group--set-status 'processing)
  (condition-case err
      (prog1
          (apply function args)
        (tessera-gnus-group--set-status 'success))
    ((error quit)
     (tessera-gnus-group--set-status 'fail)
     (signal (car err) (cdr err)))))

(defun tessera-gnus-group--get-new-news (event)
  "Get new Gnus articles after mouse EVENT."
  (interactive "e")
  (mouse-select-window event)
  (if (eq tessera-gnus-group--status-state 'processing)
      (message "Gnus is already fetching new articles")
    (gnus-group-get-new-news)))

(defun tessera-gnus-group--install ()
  "Install Tessera in the current Gnus Group buffer."
  (unless tessera-gnus-group--installed-p
    (setq
     tessera-gnus-group--original-header-line-local-p
     (local-variable-p 'header-line-format)
     tessera-gnus-group--original-header-line-format
     header-line-format
     tessera-gnus-group--installed-header-line-format
     tessera-gnus-group--header-line-format
     tessera-gnus-group--installed-p t
     tessera-gnus-group--status-state 'success
     tessera-gnus-group--statistics-cache nil)
    (setq-local
     header-line-format
     tessera-gnus-group--installed-header-line-format)
    (add-hook 'gnus-group-prepare-hook
              #'tessera-gnus-group--invalidate-statistics nil t)
    (add-hook 'gnus-group-update-hook
              #'tessera-gnus-group--invalidate-statistics nil t)
    (add-hook 'gnus-group-update-group-hook
              #'tessera-gnus-group--invalidate-statistics nil t)
    (force-mode-line-update)))

(defun tessera-gnus-group--restore ()
  "Restore the native Group header in the current buffer."
  (when tessera-gnus-group--installed-p
    (remove-hook 'gnus-group-prepare-hook
                 #'tessera-gnus-group--invalidate-statistics t)
    (remove-hook 'gnus-group-update-hook
                 #'tessera-gnus-group--invalidate-statistics t)
    (remove-hook 'gnus-group-update-group-hook
                 #'tessera-gnus-group--invalidate-statistics t)
    (when (eq
           header-line-format
           tessera-gnus-group--installed-header-line-format)
      (if tessera-gnus-group--original-header-line-local-p
          (setq-local
           header-line-format
           tessera-gnus-group--original-header-line-format)
        (kill-local-variable 'header-line-format)))
    (setq tessera-gnus-group--installed-header-line-format nil
          tessera-gnus-group--original-header-line-format nil
          tessera-gnus-group--original-header-line-local-p nil
          tessera-gnus-group--statistics-cache nil
          tessera-gnus-group--installed-p nil)
    (force-mode-line-update)))

(defun tessera-gnus-group-enable ()
  "Enable Tessera in existing and future Gnus Group buffers."
  (unless tessera-gnus-group--enabled-p
    (setq tessera-gnus-group--enabled-p t)
    (dolist (function tessera-gnus-group--update-functions)
      (advice-add function :around #'tessera-gnus-group--update))
    (add-hook 'gnus-group-mode-hook #'tessera-gnus-group--install)
    (dolist (buffer
             (match-buffers '(derived-mode . gnus-group-mode)))
      (with-current-buffer buffer
        (tessera-gnus-group--install)))))

(defun tessera-gnus-group-disable ()
  "Disable Tessera in existing Gnus Group buffers."
  (when tessera-gnus-group--enabled-p
    (setq tessera-gnus-group--enabled-p nil)
    (dolist (function tessera-gnus-group--update-functions)
      (advice-remove function #'tessera-gnus-group--update))
    (remove-hook 'gnus-group-mode-hook
                 #'tessera-gnus-group--install)
    (dolist (buffer
             (match-buffers '(derived-mode . gnus-group-mode)))
      (with-current-buffer buffer
        (tessera-gnus-group--restore)))))

(provide 'tessera-gnus-group)
;;; tessera-gnus-group.el ends here
