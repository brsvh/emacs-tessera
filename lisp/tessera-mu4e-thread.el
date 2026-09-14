;;; tessera-mu4e-thread.el --- Mu4e thread contexts  -*- lexical-binding: t; -*-

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

;; Translate native result order and folding into shared contexts.
;; Mu4e owns threading, ordering, and fold commands.

;;; Code:

(require 'seq)
(require 'tessera-mu4e-faces)

(defvar mu4e-search-threads)
(defvar mu4e~headers-docid-pre)
(declare-function mu4e~headers-thread-root-p "mu4e-headers")

(defvar-local tessera-mu4e-thread--contexts nil
  "Native docids mapped to shared thread contexts.")

(defun tessera-mu4e-thread-folds ()
  "Return native fold overlays in buffer order."
  (sort
   (seq-filter (lambda (overlay)
                 (overlay-get overlay 'mu4e-thread-folded))
               (overlays-in (point-min) (point-max)))
   (lambda (left right)
     (< (overlay-start left) (overlay-start right)))))

(defun tessera-mu4e-thread-fold-at (position)
  "Return the native fold covering POSITION, or nil."
  (seq-find (lambda (overlay)
              (overlay-get overlay 'mu4e-thread-folded))
            (overlays-at position)))

(defun tessera-mu4e-thread-context (message)
  "Return the shared thread context for native MESSAGE."
  (when (and mu4e-search-threads tessera-mu4e-thread--contexts)
    (gethash (plist-get message :docid)
             tessera-mu4e-thread--contexts)))

(defun tessera-mu4e-thread-build ()
  "Build contexts using native root boundaries and display levels.
The first hidden row represents the visible native fold summary
for padding purposes.  Threading follows `mu4e-search-threads'."
  (let (entries stack representative)
    (when mu4e-search-threads
      (save-excursion
        (goto-char (point-min))
        (while (< (point) (point-max))
          (when-let* ((message (get-text-property (point) 'msg))
                      (id (plist-get message :docid))
                      ;; The footer may inherit the preceding msg.
                      (_ (looking-at
                          (regexp-quote mu4e~headers-docid-pre))))
            (let* ((meta (plist-get message :meta))
                   (level (or (plist-get meta :level) 0))
                   (fold (tessera-mu4e-thread-fold-at (point))))
              (when (or (= level 0) (null representative)
                        (mu4e~headers-thread-root-p message))
                (setq representative nil stack nil))
              (while (and stack (>= (caar stack) level))
                (pop stack))
              (push (list id (or (cdar stack) representative)
                          (tessera-mu4e-faces--unread-p message)
                          (or (null fold)
                              (= (point) (overlay-start fold))))
                    entries)
              (unless representative (setq representative id))
              (push (cons level id) stack)))
          (forward-line 1))))
    (setq tessera-mu4e-thread--contexts
          (tessera-thread-build-contexts (nreverse entries)))))

(defun tessera-mu4e-thread-pad-folds ()
  "Add removable spacing around visible native fold summaries."
  (dolist (fold (tessera-mu4e-thread-folds))
    (when-let* ((node (tessera-mu4e-thread-context
                       (get-text-property
                        (overlay-start fold) 'msg))))
      (let ((overlay (make-overlay (overlay-start fold)
                                   (overlay-end fold))))
        (overlay-put overlay 'tessera-entry-overlay t)
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
