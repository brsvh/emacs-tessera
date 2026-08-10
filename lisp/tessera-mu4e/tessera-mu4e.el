;;; tessera-mu4e.el --- Tessera interface for mu4e  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bingshan Chang <chang@bingshan.org>

;; Author: Bingshan Chang <chang@bingshan.org>
;; Maintainer: Bingshan Chang <chang@bingshan.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (tessera "0.1.0")
;;                    (mu4e "1.14.2"))
;; Keywords: convenience, mail

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

;; Tessera interface integration for mu4e.

;;; Code:

(require 'tessera)

(defgroup tessera-mu4e nil
  "Tessera interfaces for mu4e."
  :group 'tessera
  :prefix "tessera-mu4e-")

(declare-function tessera-mu4e-headers-disable
                  "tessera-mu4e-headers")
(declare-function tessera-mu4e-headers-enable
                  "tessera-mu4e-headers")
(declare-function tessera-mu4e-headers-refresh
                  "tessera-mu4e-headers")
(declare-function tessera-mu4e-status-disable
                  "tessera-mu4e-status")
(declare-function tessera-mu4e-status-enable
                  "tessera-mu4e-status")

(defvar tessera-mu4e-mode nil)

(defun tessera-mu4e--refresh-buffers ()
  "Refresh Tessera presentations in all mu4e Headers buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'mu4e-headers-mode)
                 (fboundp 'tessera-mu4e-headers-refresh))
        (tessera-mu4e-headers-refresh)))))

(defun tessera-mu4e--set-glyph-option (symbol value)
  "Set glyph option SYMBOL to VALUE and refresh Headers buffers."
  (set-default symbol value)
  (tessera-mu4e--refresh-buffers))

(defcustom tessera-mu4e-symbol-style 'nerd-icons
  "Glyph style used for mu4e marks and features.

Individual mu4e Headers buffers may override the global default."
  :type '(choice
          (const :tag "ASCII symbols" ascii)
          (const :tag "Unicode symbols" unicode)
          (const :tag "Nerd Icons" nerd-icons))
  :set #'tessera-mu4e--set-glyph-option
  :group 'tessera-mu4e)

(make-variable-buffer-local 'tessera-mu4e-symbol-style)

(defcustom tessera-mu4e-glyph-color-style t
  "Control the color style of mu4e marks and features.

When t, use the color chosen for each semantic role.  When nil,
glyphs follow adjacent text.  A color string applies that foreground
to every mu4e mark and feature glyph."
  :type '(choice
          (const :tag "Semantic colors" t)
          (const :tag "Follow adjacent text" nil)
          (color :tag "One color"))
  :set #'tessera-mu4e--set-glyph-option
  :group 'tessera-mu4e)

(make-variable-buffer-local 'tessera-mu4e-glyph-color-style)

(defun tessera-mu4e--enable-headers ()
  "Enable the mu4e Headers interface when it is available."
  (when (featurep 'mu4e-headers)
    (require 'tessera-mu4e-headers)
    (tessera-mu4e-headers-enable)))

(defun tessera-mu4e--disable-headers ()
  "Disable the mu4e Headers interface when it is loaded."
  (when (fboundp 'tessera-mu4e-headers-disable)
    (tessera-mu4e-headers-disable)))

(defun tessera-mu4e--enable-status ()
  "Enable mu4e status tracking when it is available."
  (when (featurep 'mu4e-update)
    (require 'tessera-mu4e-status)
    (tessera-mu4e-status-enable)))

(defun tessera-mu4e--disable-status ()
  "Disable mu4e status tracking when it has loaded."
  (when (fboundp 'tessera-mu4e-status-disable)
    (tessera-mu4e-status-disable)))

(with-eval-after-load 'mu4e-headers
  (when tessera-mu4e-mode
    (tessera-mu4e--enable-headers)))

(with-eval-after-load 'mu4e-update
  (when tessera-mu4e-mode
    (tessera-mu4e--enable-status)))

;;;###autoload
(define-minor-mode tessera-mu4e-mode
  "Toggle Tessera interfaces for mu4e.

This is a global minor mode.  Enabling it does not start mu4e."
  :global t
  :group 'tessera-mu4e
  :lighter nil
  (if tessera-mu4e-mode
      (progn
        (tessera-mu4e--enable-status)
        (tessera-mu4e--enable-headers))
    (tessera-mu4e--disable-headers)
    (tessera-mu4e--disable-status)))

(provide 'tessera-mu4e)
;;; tessera-mu4e.el ends here
