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

(defun tessera-gnus-status (state progress failed help-echo action)
  "Return a Gnus header activity for STATE.

PROGRESS is a current count or a current and total count cons.
FAILED is an optional failure count.  HELP-ECHO describes ACTION."
  (tessera-ui-header-activity-create
   :state
   (pcase state
     ('processing 'working)
     ('fail 'error)
     (_ 'idle))
   :operation 'fetch
   :current (if (consp progress) (car progress) progress)
   :total (and (consp progress) (cdr progress))
   :failed failed
   :action action
   :help-echo help-echo))

(provide 'tessera-gnus-status)
;;; tessera-gnus-status.el ends here
