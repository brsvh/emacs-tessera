;;; tessera-mu4e.el --- Tessera UI for mu4e  -*- lexical-binding: t; -*-

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

;; Enable non-thread Tessera layouts in native mu4e headers buffers.

;;; Code:

(require 'tessera-mu4e-faces)

(declare-function tessera-mu4e-headers--enable "tessera-mu4e-headers")
(declare-function tessera-mu4e-headers--disable
                  "tessera-mu4e-headers")
(declare-function tessera-mu4e-headers--register
                  "tessera-mu4e-headers")

(defun tessera-mu4e--enable-headers ()
  "Enable Tessera in the current mu4e headers buffer."
  (require 'mu4e-headers)
  (require 'tessera-mu4e-headers)
  (tessera-mu4e-headers--register)
  (tessera-mu4e-headers--enable))

;;;###autoload
(define-minor-mode tessera-mu4e-mode
  "Toggle Tessera layouts in mu4e headers.
Non-thread results use `tessera-entry-layout'.  Threaded results
retain their native presentation."
  :global t
  :group 'tessera-mu4e
  (if tessera-mu4e-mode
      (add-hook 'mu4e-headers-mode-hook
                #'tessera-mu4e--enable-headers)
    (remove-hook 'mu4e-headers-mode-hook
                 #'tessera-mu4e--enable-headers))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'mu4e-headers-mode)
        (if tessera-mu4e-mode
            (tessera-mu4e--enable-headers)
          (when (featurep 'tessera-mu4e-headers)
            (tessera-mu4e-headers--disable)))))))

(provide 'tessera-mu4e)
;;; tessera-mu4e.el ends here
