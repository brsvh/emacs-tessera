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
(declare-function tessera-gnus-group-disable "tessera-gnus-group")
(declare-function tessera-gnus-group-enable "tessera-gnus-group")
(declare-function tessera-gnus-notify-install
                  "tessera-gnus-notify")
(declare-function tessera-gnus-notify-uninstall
                  "tessera-gnus-notify")

(defvar tessera-gnus-mode)

(defun tessera-gnus--set-notify-enable (symbol value)
  "Set notification option SYMBOL to VALUE."
  (set-default symbol value)
  (when (bound-and-true-p tessera-gnus-mode)
    (if value
        (when (fboundp 'tessera-gnus--enable-notify)
          (tessera-gnus--enable-notify))
      (when (fboundp 'tessera-gnus--disable-notify)
        (tessera-gnus--disable-notify)))))

(defcustom tessera-gnus-notify-enable 'inherit
  "Control notifications from `tessera-gnus-mode'.

The value `inherit' follows `tessera-notify-enable'.  A value of t
enables Gnus notifications independently, while nil disables them."
  :type '(choice
          (const :tag "Follow global setting" inherit)
          (const :tag "Enabled" t)
          (const :tag "Disabled" nil))
  :set #'tessera-gnus--set-notify-enable
  :group 'tessera-gnus)

(defcustom tessera-gnus-notify-limit 4
  "Maximum new articles sent as individual notifications."
  :type 'natnum
  :group 'tessera-gnus)

(defvar tessera-gnus--summary-load-pending-p nil
  "Non-nil when Summary activation is waiting for Gnus to load.")

(defvar tessera-gnus--group-load-pending-p nil
  "Non-nil when Group activation is waiting for Gnus to load.")

(defvar tessera-gnus--notify-load-pending-p nil
  "Non-nil when notification activation waits for Gnus to load.")

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

(defun tessera-gnus--enable-group ()
  "Enable Tessera when Gnus Group is available."
  (if (featurep 'gnus-group)
      (progn
        (require 'tessera-gnus-group)
        (tessera-gnus-group-enable))
    (unless tessera-gnus--group-load-pending-p
      (setq tessera-gnus--group-load-pending-p t)
      (with-eval-after-load 'gnus-group
        (setq tessera-gnus--group-load-pending-p nil)
        (when tessera-gnus-mode
          (tessera-gnus--enable-group))))))

(defun tessera-gnus--disable-group ()
  "Disable the Tessera Group interface when it has been loaded."
  (when (featurep 'tessera-gnus-group)
    (tessera-gnus-group-disable)))

(defun tessera-gnus--enable-notify ()
  "Enable notifications when the Gnus Group feature is available."
  (when tessera-gnus-notify-enable
    (if (featurep 'gnus-group)
        (progn
          (require 'tessera-gnus-notify)
          (tessera-gnus-notify-install))
      (unless tessera-gnus--notify-load-pending-p
        (setq tessera-gnus--notify-load-pending-p t)
        (with-eval-after-load 'gnus-group
          (setq tessera-gnus--notify-load-pending-p nil)
          (when (and tessera-gnus-mode
                     tessera-gnus-notify-enable)
            (tessera-gnus--enable-notify)))))))

(defun tessera-gnus--disable-notify ()
  "Disable Tessera Gnus notifications when they have loaded."
  (when (featurep 'tessera-gnus-notify)
    (tessera-gnus-notify-uninstall)))

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
      (progn
        (tessera-gnus--enable-group)
        (tessera-gnus--enable-summary)
        (tessera-gnus--enable-notify))
    (tessera-gnus--disable-group)
    (tessera-gnus--disable-summary)
    (tessera-gnus--disable-notify)))

(provide 'tessera-gnus)
;;; tessera-gnus.el ends here
