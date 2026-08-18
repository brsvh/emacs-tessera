;;; tessera-elfeed.el --- Tessera UI for Elfeed  -*- lexical-binding: t; -*-

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

;; `tessera-elfeed-mode' coordinates Tessera UI adapters for Elfeed.

;;; Code:

(require 'tessera)

(defgroup tessera-elfeed nil
  "Tessera interfaces for Elfeed."
  :group 'tessera
  :prefix "tessera-elfeed-")

(declare-function tessera-elfeed-search--disable
                  "tessera-elfeed-search")
(declare-function tessera-elfeed-search--enable
                  "tessera-elfeed-search")

(defun tessera-elfeed--map-search-buffers (function)
  "Call FUNCTION in every live Elfeed search buffer."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'elfeed-search-mode)
        (funcall function)))))

(defun tessera-elfeed--enable-search ()
  "Enable the Tessera adapter in the current Elfeed search buffer."
  (require 'tessera-elfeed-search)
  (tessera-elfeed-search--enable))

;;;###autoload
(define-minor-mode tessera-elfeed-mode
  "Toggle Tessera UI adapters for Elfeed buffers."
  :global t
  :group 'tessera
  (if tessera-elfeed-mode
      (progn
        (add-hook 'elfeed-search-mode-hook
                  #'tessera-elfeed--enable-search)
        (tessera-elfeed--map-search-buffers
         #'tessera-elfeed--enable-search))
    (remove-hook 'elfeed-search-mode-hook
                 #'tessera-elfeed--enable-search)
    (when (featurep 'tessera-elfeed-search)
      (tessera-elfeed--map-search-buffers
       #'tessera-elfeed-search--disable))))

(provide 'tessera-elfeed)
;;; tessera-elfeed.el ends here
