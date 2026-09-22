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

;; The Gnus customization group and `tessera-gnus-mode' live here.
;; Summary settings and rendering follow `gnus-sum'; article
;; observations follow `gnus-art'.

;;; Code:

(require 'tessera)

(defgroup tessera-gnus nil
  "Tessera interfaces for Gnus."
  :group 'tessera
  :prefix "tessera-gnus-")

;;;; Adapter lifecycle

(declare-function tessera-gnus-summary--register
                  "tessera-gnus-summary")
(declare-function tessera-gnus-summary--enable
                  "tessera-gnus-summary")
(declare-function tessera-gnus-summary--disable
                  "tessera-gnus-summary")

(declare-function tessera-gnus-summary--track-folds
                  "tessera-gnus-summary")
(declare-function tessera-gnus-summary--navigation
                  "tessera-gnus-summary")
(declare-function tessera-gnus-article--track-content
                  "tessera-gnus-article")

(declare-function tessera-gnus-article--updated
                  "tessera-gnus-article")

(defvar tessera-gnus--installed nil
  "Whether the global Gnus integration is fully installed.")

(defun tessera-gnus--map-summary-buffers (function)
  "Call FUNCTION in each live Gnus summary buffer."
  (tessera--map-mode-buffers 'gnus-summary-mode function))

(defun tessera-gnus--enable-summary ()
  "Enable Tessera in the current Gnus summary buffer."
  (require 'tessera-gnus-summary)
  (tessera-gnus-summary--register)
  (tessera-gnus-summary--enable))

(declare-function tessera-gnus-summary--glyphs-changed
                  "tessera-gnus-summary")

(defun tessera-gnus--glyphs-changed (option)
  "Forward changed glyph OPTION to an already loaded adapter."
  (when (featurep 'tessera-gnus-summary)
    (tessera-gnus-summary--glyphs-changed option)))

(defun tessera-gnus--deactivate ()
  "Remove Gnus hooks and disable every loaded adapter."
  (remove-hook 'tessera--glyph-change-functions
               #'tessera-gnus--glyphs-changed)
  (remove-hook 'gnus-summary-mode-hook
               #'tessera-gnus--enable-summary)
  (unwind-protect
      (when (featurep 'tessera-gnus-article)
        (tessera-gnus-article--track-content nil))
    (when (featurep 'tessera-gnus-summary)
      (unwind-protect
          (tessera-gnus-summary--track-folds nil)
        (unwind-protect
            (tessera-gnus-summary--navigation nil)
          (tessera-gnus--map-summary-buffers
           #'tessera-gnus-summary--disable))))))

;;;###autoload
(define-minor-mode tessera-gnus-mode
  "Toggle Tessera UI adapters for Gnus buffers."
  :global t
  :group 'tessera-gnus
  (if tessera-gnus-mode
      (unless tessera-gnus--installed
        (let (completed)
          (unwind-protect
              (progn
                (add-hook 'tessera--glyph-change-functions
                          #'tessera-gnus--glyphs-changed)
                (require 'tessera-gnus-article)
                (require 'tessera-gnus-summary)
                (tessera-gnus-summary--track-folds t)
                (tessera-gnus-summary--navigation t)
                (tessera-gnus-article--track-content t)
                (add-hook 'gnus-summary-mode-hook
                          #'tessera-gnus--enable-summary)
                (tessera-gnus--map-summary-buffers
                 #'tessera-gnus--enable-summary)
                (tessera--map-mode-buffers
                 'gnus-article-mode
                 #'tessera-gnus-article--updated)
                (setq tessera-gnus--installed t
                      completed t))
            (unless completed
              (setq tessera-gnus-mode nil
                    tessera-gnus--installed nil)
              (condition-case nil
                  (tessera-gnus--deactivate)
                ((error quit) nil))))))
    (unwind-protect
        (tessera-gnus--deactivate)
      (setq tessera-gnus--installed nil))))

(provide 'tessera-gnus)
;;; tessera-gnus.el ends here
