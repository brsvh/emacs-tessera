;;; elfeed-x.el --- Extra features for Elfeed  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bingshan Chang <chang@bingshan.org>

;; Author: Bingshan Chang <chang@bingshan.org>
;; Maintainer: Bingshan Chang <chang@bingshan.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (elfeed "4.0.1"))
;; Keywords: convenience, news
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

;; Elfeed X provides extra features for Elfeed.  The `x' means extra.
;; Enable `elfeed-x-update-mode' to schedule feed updates after entry
;; into Elfeed.  Read the schedule with `elfeed-x-next-update-time'.
;; See `elfeed-x-search' for continuous reading and `elfeed-x-webkit'
;; for fitting WebKit widgets to article windows.
;; Loading these libraries does not activate their modes.

;;; Code:

(require 'timer)
(require 'seq)

(declare-function elfeed-queue-count-total "elfeed")
(declare-function elfeed-update "elfeed")

(defgroup elfeed-x nil
  "Extra features for Elfeed."
  :group 'applications
  :prefix "elfeed-x-")

(defvar elfeed-x--update-timer nil
  "Timer owned by `elfeed-x-update-mode'.")

(defvar elfeed-x--updating nil
  "Whether a scheduled update is dispatching its requests.")

(defvar elfeed-x-update-mode nil)

(defun elfeed-x-update-timer ()
  "Return the active Elfeed X update timer, or nil.
Treat the returned timer as read-only.  This function neither starts
nor repairs a schedule; it returns nil for a cancelled timer."
  (when (and (timerp elfeed-x--update-timer)
             (memq elfeed-x--update-timer timer-list))
    elfeed-x--update-timer))

(defun elfeed-x-next-update-time ()
  "Return the next scheduled update time, or nil without a timer.
The result is an Emacs time value for the next update attempt.
A busy Elfeed queue causes that attempt to be skipped."
  (when-let* ((timer (elfeed-x-update-timer)))
    (timer--time timer)))

(defun elfeed-x--stop-update-timer ()
  "Cancel the timer owned by Elfeed X."
  (when (timerp elfeed-x--update-timer)
    (cancel-timer elfeed-x--update-timer))
  (setq elfeed-x--update-timer nil))

(defun elfeed-x--set-update-interval (symbol value)
  "Set update interval SYMBOL to positive number VALUE.
Restart an active timer using VALUE as both delay and interval."
  (unless (and (numberp value) (> value 0))
    (error "Elfeed X update interval must be positive"))
  (set-default symbol value)
  (when (elfeed-x-update-timer)
    (elfeed-x--stop-update-timer)
    (elfeed-x--start-update-timer)))

(defcustom elfeed-x-update-interval 900
  "Seconds between scheduled Elfeed update attempts.
This positive number also specifies the delay before the first
attempt.  Customize or `setopt' restarts an active timer; changing
the value before entering Elfeed does not start a timer."
  :type '(restricted-sexp :match-alternatives
                          ((lambda (value)
                             (and (numberp value) (> value 0)))))
  :set #'elfeed-x--set-update-interval
  :group 'elfeed-x)

(defun elfeed-x--update ()
  "Update feeds unless Elfeed is already retrieving or dispatching."
  (when (and elfeed-x-update-mode
             (not elfeed-x--updating)
             (zerop (elfeed-queue-count-total)))
    (let ((elfeed-x--updating t))
      (elfeed-update))))

(defun elfeed-x--start-update-timer (&rest _)
  "Start the enabled update schedule without duplicating a timer."
  (when (and elfeed-x-update-mode (not (elfeed-x-update-timer)))
    (unless (and (numberp elfeed-x-update-interval)
                 (> elfeed-x-update-interval 0))
      (user-error "Elfeed X update interval must be positive"))
    (require 'elfeed)
    (setq elfeed-x--update-timer
          (run-at-time elfeed-x-update-interval
                       elfeed-x-update-interval
                       #'elfeed-x--update))))

;;;###autoload
(define-minor-mode elfeed-x-update-mode
  "Toggle scheduled Elfeed updates globally.
Start the schedule after the next `elfeed' command, or immediately
if an Elfeed search, article, or tree buffer already exists.
Repeated enabling preserves an active schedule.  Disabling cancels
the timer without interrupting ongoing retrieval."
  :global t
  :group 'elfeed-x
  (if elfeed-x-update-mode
      (let (completed)
        (unwind-protect
            (progn
              (advice-add 'elfeed :after
                          #'elfeed-x--start-update-timer)
              (when (seq-some
                     (lambda (buffer)
                       (with-current-buffer buffer
                         (derived-mode-p 'elfeed-search-mode
                                         'elfeed-show-mode
                                         'elfeed-tree-mode)))
                     (buffer-list))
                (elfeed-x--start-update-timer))
              (setq completed t))
          (unless completed
            (elfeed-x-update-mode -1))))
    (advice-remove 'elfeed #'elfeed-x--start-update-timer)
    (elfeed-x--stop-update-timer)))

(provide 'elfeed-x)
;;; elfeed-x.el ends here
