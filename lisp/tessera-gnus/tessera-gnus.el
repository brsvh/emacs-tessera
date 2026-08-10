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
(declare-function tessera-gnus-summary-refresh "tessera-gnus-summary")
(declare-function tessera-gnus-group-disable "tessera-gnus-group")
(declare-function tessera-gnus-group-enable "tessera-gnus-group")
(declare-function tessera-gnus-group-refresh "tessera-gnus-group")
(declare-function tessera-gnus-topic-disable "tessera-gnus-topic")
(declare-function tessera-gnus-topic-enable "tessera-gnus-topic")
(declare-function tessera-gnus-notify-install
                  "tessera-gnus-notify")
(declare-function tessera-gnus-notify-uninstall
                  "tessera-gnus-notify")

(defvar tessera-gnus-mode nil)

(defconst tessera-gnus--face-remap-functions
  '(face-remap-add-relative
    face-remap-remove-relative
    face-remap-set-base
    face-remap-reset-base)
  "Face-remapping functions that invalidate Gnus measurements.")

(defvar-local tessera-gnus--face-remap-function nil
  "Function that refreshes face measurements in this Gnus buffer.")

(defvar tessera-gnus--presentation-hooks-installed-p nil
  "Non-nil when Gnus presentation hooks are installed.")

(defun tessera-gnus--refresh-buffer ()
  "Refresh the Tessera presentation in the current Gnus buffer."
  (cond
   ((and (derived-mode-p 'gnus-summary-mode)
         (fboundp 'tessera-gnus-summary-refresh))
    (tessera-gnus-summary-refresh))
   ((and (derived-mode-p 'gnus-group-mode)
         (fboundp 'tessera-gnus-group-refresh))
    (tessera-gnus-group-refresh))))

(defun tessera-gnus--refresh-buffers (&rest _args)
  "Refresh Tessera presentations in all Gnus buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (tessera-gnus--refresh-buffer))))

(defun tessera-gnus--face-remap-changed (&rest _args)
  "Refresh measurements after a face remapping change."
  (when (functionp tessera-gnus--face-remap-function)
    (funcall tessera-gnus--face-remap-function)))

(defun tessera-gnus--install-presentation-hooks ()
  "Install hooks that refresh Gnus presentations."
  (unless tessera-gnus--presentation-hooks-installed-p
    (setq tessera-gnus--presentation-hooks-installed-p t)
    (require 'face-remap)
    (dolist (function tessera-gnus--face-remap-functions)
      (advice-add function :after #'tessera-gnus--face-remap-changed))
    (add-hook 'after-setting-font-hook #'tessera-gnus--refresh-buffers)
    (add-hook 'enable-theme-functions #'tessera-gnus--refresh-buffers)
    (add-hook 'disable-theme-functions #'tessera-gnus--refresh-buffers)))

(defun tessera-gnus--remove-presentation-hooks ()
  "Remove hooks that refresh Gnus presentations."
  (when tessera-gnus--presentation-hooks-installed-p
    (setq tessera-gnus--presentation-hooks-installed-p nil)
    (dolist (function tessera-gnus--face-remap-functions)
      (advice-remove function #'tessera-gnus--face-remap-changed))
    (remove-hook 'after-setting-font-hook #'tessera-gnus--refresh-buffers)
    (remove-hook 'enable-theme-functions #'tessera-gnus--refresh-buffers)
    (remove-hook 'disable-theme-functions #'tessera-gnus--refresh-buffers)))

(defun tessera-gnus--set-glyph-option (symbol value)
  "Set glyph option SYMBOL to VALUE and refresh Gnus buffers."
  (set-default symbol value)
  (tessera-gnus--refresh-buffers))

(defcustom tessera-gnus-symbol-style 'nerd-icons
  "Glyph style used for Gnus marks and features.

Individual Gnus buffers may override the global default."
  :type '(choice
          (const :tag "ASCII symbols" ascii)
          (const :tag "Unicode symbols" unicode)
          (const :tag "Nerd Icons" nerd-icons))
  :set #'tessera-gnus--set-glyph-option
  :group 'tessera-gnus)

(make-variable-buffer-local 'tessera-gnus-symbol-style)

(defcustom tessera-gnus-glyph-color-style t
  "Control the color style of Gnus marks and features.

When t, preserve the color chosen for each semantic role.  When nil,
glyphs follow adjacent text.  A color string applies that foreground
to every Gnus mark and feature glyph."
  :type '(choice
          (const :tag "Semantic colors" t)
          (const :tag "Follow adjacent text" nil)
          (color :tag "One color"))
  :set #'tessera-gnus--set-glyph-option
  :group 'tessera-gnus)

(make-variable-buffer-local 'tessera-gnus-glyph-color-style)

(defun tessera-gnus--nerd-icon (spec fallback)
  "Return the Nerd Icon described by SPEC, or FALLBACK."
  (let ((height 0.8)
        (function
         (and (consp spec)
              (symbolp (car spec))
              (intern (format "nerd-icons-%s" (car spec))))))
    (if (and function
             (stringp (cdr spec))
             (display-graphic-p)
             (require 'nerd-icons nil t)
             (fboundp function))
        (condition-case nil
            (funcall function (cdr spec)
                     :height height
                     :v-adjust (/ (- 1.0 height) 2))
          (error fallback))
      fallback)))

(defun tessera-gnus--render-glyph (value fallback)
  "Render glyph VALUE, using FALLBACK when it is unavailable."
  (cond
   ((stringp value) (copy-sequence value))
   ((consp value) (tessera-gnus--nerd-icon value fallback))
   (t fallback)))

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

(defvar tessera-gnus--topic-load-pending-p nil
  "Non-nil when Topic activation is waiting for Gnus to load.")

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

(defun tessera-gnus--enable-topic ()
  "Enable Tessera when Gnus Topic is available."
  (if (featurep 'gnus-topic)
      (progn
        (require 'tessera-gnus-topic)
        (tessera-gnus-topic-enable))
    (unless tessera-gnus--topic-load-pending-p
      (setq tessera-gnus--topic-load-pending-p t)
      (with-eval-after-load 'gnus-topic
        (setq tessera-gnus--topic-load-pending-p nil)
        (when tessera-gnus-mode
          (tessera-gnus--enable-topic))))))

(defun tessera-gnus--disable-topic ()
  "Disable the Tessera Topic interface when it has been loaded."
  (when (featurep 'tessera-gnus-topic)
    (tessera-gnus-topic-disable)))

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
        (tessera-gnus--install-presentation-hooks)
        (tessera-gnus--enable-group)
        (tessera-gnus--enable-topic)
        (tessera-gnus--enable-summary)
        (tessera-gnus--enable-notify))
    (tessera-gnus--disable-topic)
    (tessera-gnus--disable-group)
    (tessera-gnus--disable-summary)
    (tessera-gnus--disable-notify)
    (tessera-gnus--remove-presentation-hooks)))

(provide 'tessera-gnus)
;;; tessera-gnus.el ends here
