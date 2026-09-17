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

;; The Elfeed group and `tessera-elfeed-mode' live here.
;; Search rendering follows the upstream `elfeed-search' feature.

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
(declare-function tessera-elfeed-search--register
                  "tessera-elfeed-search")

;;;; Adapter lifecycle

(defun tessera-elfeed--map-search-buffers (function)
  "Call FUNCTION in every live Elfeed search buffer."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'elfeed-search-mode)
        (funcall function)))))

(defun tessera-elfeed--enable-search ()
  "Enable the Tessera adapter in the current Elfeed search buffer."
  (require 'tessera-elfeed-search)
  (tessera-elfeed-search--register)
  (tessera-elfeed-search--enable))

(declare-function tessera-elfeed-search--glyphs-changed
                  "tessera-elfeed-search")

(defun tessera-elfeed--glyphs-changed (option)
  "Forward changed glyph OPTION to an already loaded adapter."
  (when (featurep 'tessera-elfeed-search)
    (tessera-elfeed-search--glyphs-changed option)))

;;;###autoload
(define-minor-mode tessera-elfeed-mode
  "Toggle Tessera UI adapters for Elfeed buffers."
  :global t
  :group 'tessera-elfeed
  (if tessera-elfeed-mode
      (progn
        (add-hook 'tessera--glyph-change-functions
                  #'tessera-elfeed--glyphs-changed)
        (add-hook 'elfeed-search-mode-hook
                  #'tessera-elfeed--enable-search)
        (tessera-elfeed--map-search-buffers
         #'tessera-elfeed--enable-search))
    (remove-hook 'tessera--glyph-change-functions
                 #'tessera-elfeed--glyphs-changed)
    (remove-hook 'elfeed-search-mode-hook
                 #'tessera-elfeed--enable-search)
    (when (featurep 'tessera-elfeed-search)
      (tessera-elfeed--map-search-buffers
       #'tessera-elfeed-search--disable))))

(provide 'tessera-elfeed)
;;; tessera-elfeed.el ends here
