;;; tessera-mu4e-thread.el --- Mu4e fold presentation  -*- lexical-binding: t; -*-

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

;; Follow `mu4e-thread' for native fold overlays and their spacing.
;; The headers adapter supplies contexts from native result rows.
;; Mu4e retains ownership of fold commands and folded summary text.

;;; Code:

(require 'seq)
(require 'tessera)

(defvar-local tessera-mu4e-thread--folds t
  "Cached native folds, or t when the cache needs rebuilding.")

(defvar-local tessera-mu4e-thread-change-hook nil
  "Hook run after a native fold operation in this buffer.")

(defun tessera-mu4e-thread-invalidate ()
  "Invalidate cached folds after native buffer edits."
  (setq tessera-mu4e-thread--folds t))

(defun tessera-mu4e-thread--changed (&rest _arguments)
  "Record a native fold operation."
  (tessera-mu4e-thread-invalidate)
  (run-hooks 'tessera-mu4e-thread-change-hook))

(defun tessera-mu4e-thread-track (enable)
  "Track native folds when ENABLE is non-nil, or stop tracking."
  (dolist (function '(mu4e-thread-fold mu4e-thread-unfold
                                       mu4e-thread-unfold-all))
    (if enable
        (advice-add function :after #'tessera-mu4e-thread--changed)
      (advice-remove function #'tessera-mu4e-thread--changed))))

(defun tessera-mu4e-thread-folds ()
  "Return cached native fold overlays in buffer order."
  (when (eq tessera-mu4e-thread--folds t)
    (setq tessera-mu4e-thread--folds
          (sort
           (seq-filter (lambda (overlay)
                         (overlay-get overlay 'mu4e-thread-folded))
                       (overlays-in (point-min) (point-max)))
           (lambda (left right)
             (< (overlay-start left) (overlay-start right))))))
  tessera-mu4e-thread--folds)

(defun tessera-mu4e-thread-fold-at (position)
  "Return the native fold covering POSITION, or nil."
  (seq-find (lambda (overlay)
              (overlay-get overlay 'mu4e-thread-folded))
            (overlays-at position)))

(defun tessera-mu4e-thread-pad-folds (contexts)
  "Space native fold summaries using the supplied thread CONTEXTS."
  (remove-overlays nil nil 'tessera-mu4e-fold-padding t)
  (dolist (fold (tessera-mu4e-thread-folds))
    (when-let* ((node (gethash
                       (plist-get (get-text-property
                                   (overlay-start fold) 'msg) :docid)
                       contexts)))
      (let ((overlay (make-overlay (overlay-start fold)
                                   (overlay-end fold))))
        (overlay-put overlay 'tessera-entry-overlay t)
        (overlay-put overlay 'tessera-mu4e-fold-padding t)
        (overlay-put overlay 'evaporate t)
        (overlay-put overlay 'before-string
                     (tessera--padding-string
                      tessera-thread-inner-top-padding))
        (overlay-put overlay 'after-string
                     (tessera--padding-string
                      (if (tessera-thread-context-last node)
                          tessera-thread-outer-bottom-padding
                        tessera-thread-inner-bottom-padding)))))))

(provide 'tessera-mu4e-thread)
;;; tessera-mu4e-thread.el ends here
