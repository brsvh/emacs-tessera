;;; tessera-gnus.el --- Tessera UI for Gnus  -*- lexical-binding: t; -*-

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

;; `tessera-gnus-mode' coordinates Tessera UI adapters for Gnus.

;;; Code:

(require 'tessera)

(defgroup tessera-gnus nil
  "Tessera interfaces for Gnus."
  :group 'tessera
  :prefix "tessera-gnus-")

(declare-function tessera-gnus-summary--register
                  "tessera-gnus-summary")
(declare-function tessera-gnus-summary--enable
                  "tessera-gnus-summary")
(declare-function tessera-gnus-summary--disable
                  "tessera-gnus-summary")

(declare-function tessera-gnus-summary--article-updated
                  "tessera-gnus-summary")

(defun tessera-gnus--map-summary-buffers (function)
  "Call FUNCTION in each live Gnus summary buffer."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'gnus-summary-mode)
        (funcall function)))))

(defun tessera-gnus--enable-summary ()
  "Enable Tessera in the current Gnus summary buffer."
  (require 'tessera-gnus-summary)
  (tessera-gnus-summary--register)
  (tessera-gnus-summary--enable))

;;;###autoload
(define-minor-mode tessera-gnus-mode
  "Toggle Tessera UI adapters for Gnus buffers."
  :global t
  :group 'tessera-gnus
  (if tessera-gnus-mode
      (progn
        (add-hook 'gnus-summary-mode-hook
                  #'tessera-gnus--enable-summary)
        (add-hook 'gnus-article-prepare-hook
                  #'tessera-gnus-summary--article-updated t)
        (tessera-gnus--map-summary-buffers
         #'tessera-gnus--enable-summary)
        (when (featurep 'tessera-gnus-summary)
          (dolist (buffer (buffer-list))
            (with-current-buffer buffer
              (tessera-gnus-summary--article-updated)))))
    (remove-hook 'gnus-summary-mode-hook
                 #'tessera-gnus--enable-summary)
    (remove-hook 'gnus-article-prepare-hook
                 #'tessera-gnus-summary--article-updated)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (remove-hook 'post-command-hook
                     #'tessera-gnus-summary--article-updated t)))
    (when (featurep 'tessera-gnus-summary)
      (tessera-gnus--map-summary-buffers
       #'tessera-gnus-summary--disable))))

(provide 'tessera-gnus)
;;; tessera-gnus.el ends here
