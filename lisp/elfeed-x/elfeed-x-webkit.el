;;; elfeed-x-webkit.el --- Fit WebKit to Elfeed articles  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bingshan Chang <chang@bingshan.org>

;; Author: Bingshan Chang <chang@bingshan.org>
;; Keywords: convenience, news
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Fit existing widgets after rendering without selecting the article
;; window.  Configure the optional `elfeed-webkit' package separately.

;;; Code:

(require 'elfeed-x)

(declare-function xwidget-at "xwidget" (pos))
(declare-function xwidget-webkit-adjust-size-to-window
                  "xwidget" (xwidget &optional window))

(defgroup elfeed-x-webkit nil
  "Fit Elfeed WebKit widgets to their article windows."
  :group 'elfeed-x
  :prefix "elfeed-x-webkit-")

(defun elfeed-x-webkit--fit ()
  "Fit the current article's widget to a window displaying it.
Prefer a window on the selected frame, then any other visible frame.
Do nothing when the article is hidden or has no widget."
  (when (and (featurep 'xwidget-internal)
             (fboundp 'xwidget-at)
             (fboundp 'xwidget-webkit-adjust-size-to-window)
             (derived-mode-p 'elfeed-show-mode))
    (when-let* ((window (get-buffer-window (current-buffer) 'visible))
                (widget (xwidget-at (point-min))))
      (xwidget-webkit-adjust-size-to-window widget window))))

;;;###autoload
(define-minor-mode elfeed-x-webkit-mode
  "Toggle fitting Elfeed widgets to their article windows globally.
Fit after article rendering and fit already displayed widgets when
enabling.  This mode does not enable a renderer or change bindings.
Without xwidget support it has no effect.  Disabling removes its
render hook and leaves the renderer and window layout unchanged."
  :global t
  :group 'elfeed-x-webkit
  (if elfeed-x-webkit-mode
      (progn
        (add-hook 'elfeed-show-update-hook #'elfeed-x-webkit--fit 90)
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (elfeed-x-webkit--fit))))
    (remove-hook 'elfeed-show-update-hook #'elfeed-x-webkit--fit)))

(provide 'elfeed-x-webkit)
;;; elfeed-x-webkit.el ends here
