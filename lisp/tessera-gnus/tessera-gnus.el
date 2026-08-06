;;; tessera-gnus.el --- Tessera interface for Gnus  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bingshan Chang <chang@bingshan.org>

;; Author: Bingshan Chang <chang@bingshan.org>
;; Maintainer: Bingshan Chang <chang@bingshan.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (tessera "0.1.0"))
;; Keywords: convenience, mail, news

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

;; Tessera interface integration for Gnus.

;;; Code:

(require 'tessera)

(defgroup tessera-gnus nil
  "Tessera interfaces for Gnus."
  :group 'tessera
  :prefix "tessera-gnus-")

(declare-function tessera-gnus-summary-disable "tessera-gnus-summary")
(declare-function tessera-gnus-summary-enable "tessera-gnus-summary")

(defvar tessera-gnus-mode)

(defvar tessera-gnus--summary-load-pending-p nil
  "Non-nil when Summary activation is waiting for Gnus to load.")

(defun tessera-gnus--enable-summary ()
  "Enable Tessera when Gnus Summary is available."
  (if (featurep 'gnus-sum)
      (progn
        (require 'tessera-gnus-summary)
        (tessera-gnus-summary-enable))
    (unless tessera-gnus--summary-load-pending-p
      (setq tessera-gnus--summary-load-pending-p t)
      (with-eval-after-load 'gnus-sum
        (setq tessera-gnus--summary-load-pending-p nil)
        (when tessera-gnus-mode
          (tessera-gnus--enable-summary))))))

(defun tessera-gnus--disable-summary ()
  "Disable the Tessera Summary interface when it has been loaded."
  (when (featurep 'tessera-gnus-summary)
    (tessera-gnus-summary-disable)))

;;;###autoload
(define-minor-mode tessera-gnus-mode
  "Toggle Tessera interfaces for Gnus.

This is a global minor mode.  Enabling it does not start Gnus;
interfaces are installed as their corresponding Gnus features become
available."
  :global t
  :group 'tessera-gnus
  :lighter nil
  (if tessera-gnus-mode
      (tessera-gnus--enable-summary)
    (tessera-gnus--disable-summary)))

(provide 'tessera-gnus)
;;; tessera-gnus.el ends here
