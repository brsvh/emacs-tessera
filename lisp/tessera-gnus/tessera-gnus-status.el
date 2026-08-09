;;; tessera-gnus-status.el --- Shared Gnus status display  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bingshan Chang <chang@bingshan.org>

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

;; Shared status presentation for Tessera Gnus interfaces.

;;; Code:

(require 'tessera-ui)

(defun tessera-gnus-status
    (state progress failed help-echo keymap)
  "Return a Gnus status display for STATE.

PROGRESS is a current count or a current and total count cons.
FAILED is an optional failure count.  HELP-ECHO and KEYMAP define
header-line interaction."
  (let ((face
         (pcase state
           ('processing 'tessera-header-status-processing)
           ('fail 'tessera-header-status-fail)
           (_ 'tessera-header-status-success)))
        (text
         (pcase state
           ('processing
            (cond
             ((consp progress)
              (format "FETCHING %d/%d"
                      (car progress) (cdr progress)))
             ((numberp progress)
              (format "FETCHING %d" progress))
             (t "FETCHING")))
           ('fail
            (if failed
                (format "FETCH FAILED %d" failed)
              "FETCH FAILED"))
           (_ "IDLE"))))
    (propertize
     text
     'face face
     'help-echo help-echo
     'keymap keymap
     'mouse-face 'header-line-highlight)))

(provide 'tessera-gnus-status)
;;; tessera-gnus-status.el ends here
