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

;; The mu4e group and `tessera-mu4e-mode' live here.
;; Rendering and folding follow `mu4e-headers' and `mu4e-thread'.

;;; Code:

(require 'tessera)

(defgroup tessera-mu4e nil
  "Tessera interfaces for mu4e."
  :group 'tessera
  :prefix "tessera-mu4e-")

;;;; Adapter lifecycle

(declare-function tessera-mu4e-headers--enable "tessera-mu4e-headers")
(declare-function tessera-mu4e-headers--disable
                  "tessera-mu4e-headers")
(declare-function tessera-mu4e-headers--register
                  "tessera-mu4e-headers")
(declare-function tessera-mu4e-headers--navigation
                  "tessera-mu4e-headers")

(defvar tessera-mu4e-headers--bulk-deactivating)

(defvar tessera-mu4e--installed nil
  "Whether the global mu4e integration is fully installed.")

(defun tessera-mu4e--map-headers-buffers (function)
  "Call FUNCTION in every live mu4e headers buffer."
  (tessera--map-mode-buffers 'mu4e-headers-mode function))

(defun tessera-mu4e--enable-headers ()
  "Enable Tessera in the current mu4e headers buffer."
  (require 'mu4e-headers)
  (require 'tessera-mu4e-headers)
  (tessera-mu4e-headers--register)
  (tessera-mu4e-headers--enable))

(declare-function tessera-mu4e-headers--glyphs-changed
                  "tessera-mu4e-headers")
(declare-function tessera-mu4e-headers--months-changed
                  "tessera-mu4e-headers")

(defun tessera-mu4e--glyphs-changed (option)
  "Forward changed glyph OPTION to an already loaded adapter."
  (when (featurep 'tessera-mu4e-headers)
    (tessera-mu4e-headers--glyphs-changed option)))

(defun tessera-mu4e--months-changed (option)
  "Forward changed month OPTION to an already loaded adapter."
  (when (featurep 'tessera-mu4e-headers)
    (tessera-mu4e-headers--months-changed option)))

(defun tessera-mu4e--deactivate ()
  "Remove mu4e hooks and disable every active headers adapter."
  (remove-hook 'tessera--glyph-change-functions
               #'tessera-mu4e--glyphs-changed)
  (remove-hook 'tessera--month-change-functions
               #'tessera-mu4e--months-changed)
  (remove-hook 'mu4e-headers-mode-hook
               #'tessera-mu4e--enable-headers)
  (when (featurep 'tessera-mu4e-headers)
    (let ((tessera-mu4e-headers--bulk-deactivating t))
      (unwind-protect
          (tessera-mu4e--map-headers-buffers
           #'tessera-mu4e-headers--disable)
        (tessera-mu4e-headers--navigation nil)))))

;;;###autoload
(define-minor-mode tessera-mu4e-mode
  "Toggle Tessera layouts in mu4e headers.
Non-thread results use `tessera-entry-layout'.  Native threading
automatically selects the shared thread layout."
  :global t
  :group 'tessera-mu4e
  (if tessera-mu4e-mode
      (unless tessera-mu4e--installed
        (let (completed)
          (unwind-protect
              (progn
                (add-hook 'tessera--glyph-change-functions
                          #'tessera-mu4e--glyphs-changed)
                (add-hook 'tessera--month-change-functions
                          #'tessera-mu4e--months-changed)
                (add-hook 'mu4e-headers-mode-hook
                          #'tessera-mu4e--enable-headers)
                (tessera-mu4e--map-headers-buffers
                 #'tessera-mu4e--enable-headers)
                (setq tessera-mu4e--installed t
                      completed t))
            (unless completed
              (setq tessera-mu4e-mode nil
                    tessera-mu4e--installed nil)
              (condition-case nil
                  (tessera-mu4e--deactivate)
                ((error quit) nil))))))
    (unwind-protect
        (tessera-mu4e--deactivate)
      (setq tessera-mu4e--installed nil))))

(provide 'tessera-mu4e)
;;; tessera-mu4e.el ends here
