;;; tessera-mu4e-status.el --- Shared mu4e status  -*- lexical-binding: t; -*-

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

;; Native mu4e update state mapped to Tessera header activity.

;;; Code:

(require 'mu4e-update)
(require 'tessera-ui)

(defvar tessera-mu4e-status--enabled-p nil
  "Non-nil when native mu4e update tracking is enabled.")

(defvar tessera-mu4e-status--state 'idle
  "Current standard mu4e update state.")

(defvar tessera-mu4e-status--failed-p nil
  "Non-nil when mail retrieval failed during the current update.")

(defun tessera-mu4e-status--refresh ()
  "Redisplay Tessera headers in all mu4e Headers buffers."
  (dolist (buffer
           (match-buffers '(derived-mode . mu4e-headers-mode)))
    (with-current-buffer buffer
      (force-mode-line-update))))

(defun tessera-mu4e-status--begin ()
  "Begin presenting a native mu4e update."
  (unless (eq tessera-mu4e-status--state 'working)
    (setq tessera-mu4e-status--failed-p nil))
  (setq tessera-mu4e-status--state 'working)
  (tessera-mu4e-status--refresh))

(defun tessera-mu4e-status--finish ()
  "Finish presenting a native mu4e update."
  (setq tessera-mu4e-status--state
        (if tessera-mu4e-status--failed-p 'error 'idle))
  (tessera-mu4e-status--refresh))

(defun tessera-mu4e-status--record-process-result (process _message)
  "Record the completion state of update PROCESS.

_MESSAGE is the native process sentinel message."
  (when (or (not (eq (process-status process) 'exit))
            (/= (process-exit-status process) 0))
    (setq tessera-mu4e-status--failed-p t
          tessera-mu4e-status--state 'error)
    (tessera-mu4e-status--refresh)))

(defun tessera-mu4e-status-update (event)
  "Start a native mu4e update from mouse EVENT."
  (interactive "e")
  (mouse-select-window event)
  (mu4e-update-mail-and-index t))

(defun tessera-mu4e-status ()
  "Return the current semantic mu4e update activity."
  (tessera-ui-header-activity-create
   :state tessera-mu4e-status--state
   :operation 'update
   :action #'tessera-mu4e-status-update
   :help-echo
   (pcase tessera-mu4e-status--state
     ('working "mu4e is updating mail and its index")
     ('error
      (concat "The last mu4e update failed; "
              "mouse-1: Update mail and index"))
     (_ "mouse-1: Update mail and index"))))

(defun tessera-mu4e-status-enable ()
  "Enable native mu4e update status tracking."
  (unless tessera-mu4e-status--enabled-p
    (setq tessera-mu4e-status--enabled-p t)
    (add-hook 'mu4e-update-pre-hook #'tessera-mu4e-status--begin)
    (add-hook 'mu4e-index-updated-hook #'tessera-mu4e-status--finish)
    (advice-add 'mu4e--update-sentinel-func :before #'tessera-mu4e-status--record-process-result)))

(defun tessera-mu4e-status-disable ()
  "Disable native mu4e update status tracking."
  (when tessera-mu4e-status--enabled-p
    (setq tessera-mu4e-status--enabled-p nil
          tessera-mu4e-status--state 'idle
          tessera-mu4e-status--failed-p nil)
    (remove-hook 'mu4e-update-pre-hook #'tessera-mu4e-status--begin)
    (remove-hook 'mu4e-index-updated-hook #'tessera-mu4e-status--finish)
    (advice-remove 'mu4e--update-sentinel-func #'tessera-mu4e-status--record-process-result)
    (tessera-mu4e-status--refresh)))

(provide 'tessera-mu4e-status)
;;; tessera-mu4e-status.el ends here
