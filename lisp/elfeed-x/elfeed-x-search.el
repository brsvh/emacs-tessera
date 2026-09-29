;;; elfeed-x-search.el --- Continuous reading in Elfeed  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bingshan Chang <chang@bingshan.org>

;; Author: Bingshan Chang <chang@bingshan.org>
;; Keywords: convenience, news
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Follow native Search navigation in an existing article window.
;; The window layout remains under the user's control.

;;; Code:

(require 'elfeed-x)
(require 'seq)

(declare-function elfeed-search-selected "elfeed-search")
(declare-function elfeed-search-update-entry "elfeed-search")
(declare-function elfeed-untag "elfeed")
(declare-function elfeed-show-entry "elfeed-show")

(defvar elfeed-show-entry)
(defvar elfeed-show-entry-switch)

(defgroup elfeed-x-search nil
  "Extra features for Elfeed Search."
  :group 'elfeed-x
  :prefix "elfeed-x-search-")

(defcustom elfeed-x-search-follow-commands '(next-line previous-line)
  "Commands that update an existing article window after navigation.
These are command symbols, independent of their key bindings.
The default follows Elfeed Search's native `n' and `p' commands."
  :type '(repeat symbol)
  :group 'elfeed-x-search)

(defun elfeed-x-search--article-window ()
  "Return an article window on the selected frame, or nil.
Use the first non-dedicated article window after the selected one
in cyclic window order."
  (seq-find
   (lambda (window)
     (and (not (window-dedicated-p window))
          (with-current-buffer (window-buffer window)
            (derived-mode-p 'elfeed-show-mode))))
   (window-list)))

(defun elfeed-x-search--follow ()
  "Show the entry at point after a configured navigation command."
  (when (and (derived-mode-p 'elfeed-search-mode)
             (memq this-command elfeed-x-search-follow-commands))
    (when-let* ((window (elfeed-x-search--article-window))
                (entry (elfeed-search-selected t)))
      (unless (eq entry (buffer-local-value
                         'elfeed-show-entry (window-buffer window)))
        (condition-case err
            (save-selected-window
              (save-excursion
                (let ((elfeed-show-entry-switch
                       (lambda (buffer)
                         (set-window-buffer window buffer))))
                  (when (elfeed-untag entry 'unread)
                    (elfeed-search-update-entry entry))
                  (elfeed-show-entry entry))))
          (error
           (message "Elfeed X follow: %s"
                    (error-message-string err))))))))

(defun elfeed-x-search--enable ()
  "Install continuous reading in the current Search buffer."
  (add-hook 'post-command-hook #'elfeed-x-search--follow 90 t))

(defun elfeed-x-search--disable ()
  "Remove continuous reading from the current Search buffer."
  (remove-hook 'post-command-hook #'elfeed-x-search--follow t))

;;;###autoload
(define-minor-mode elfeed-x-search-follow-mode
  "Toggle continuous reading in all Elfeed Search buffers.
Follow `elfeed-x-search-follow-commands' only while a non-dedicated
article window exists on the selected frame.  Keep focus and point
in Search and mark the displayed entry as read.  Use Elfeed's
article renderer, including for feeds with external display handlers.
Disabling removes the hooks from existing and future Search buffers."
  :global t
  :group 'elfeed-x-search
  (if elfeed-x-search-follow-mode
      (add-hook 'elfeed-search-mode-hook #'elfeed-x-search--enable)
    (remove-hook 'elfeed-search-mode-hook #'elfeed-x-search--enable))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'elfeed-search-mode)
        (if elfeed-x-search-follow-mode
            (elfeed-x-search--enable)
          (elfeed-x-search--disable))))))

(provide 'elfeed-x-search)
;;; elfeed-x-search.el ends here
