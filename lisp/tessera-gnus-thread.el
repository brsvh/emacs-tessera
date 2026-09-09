;;; tessera-gnus-thread.el --- Gnus thread contexts  -*- lexical-binding: t; -*-

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

;; Translate the final native summary order into Tessera contexts.
;; No message retrieval or independent threading is performed here.

;;; Code:

(require 'tessera)
(require 'gnus-sum)

(defvar-local tessera-gnus-thread--contexts nil
  "Article numbers mapped to their displayed thread contexts.")

(defvar-local tessera-gnus-thread--width 8
  "Shared leading width for counts and four native status slots.")

(defun tessera-gnus-thread-leading-width (_context)
  "Return the leading width for the current summary's thread view."
  tessera-gnus-thread--width)

(defun tessera-gnus-thread-context (header)
  "Return the displayed thread context for native HEADER."
  (when (and gnus-show-threads tessera-gnus-thread--contexts)
    (gethash (mail-header-number header)
             tessera-gnus-thread--contexts)))

(defun tessera-gnus-thread-build ()
  "Rebuild thread contexts from the completed native summary.
Use native display levels, including Gnus's treatment of missing
parents and adopted roots.  Count all article rows, even when folded.
Threading is enabled solely by `gnus-show-threads'."
  (setq tessera-gnus-thread--contexts (make-hash-table :test #'eql)
        tessera-gnus-thread--width 8)
  (when gnus-show-threads
    (let ((levels (make-hash-table :test #'eql))
          (last-child (make-hash-table :test #'eql))
          (totals (make-hash-table :test #'eql))
          (unreads (make-hash-table :test #'eql))
          (last-visible (make-hash-table :test #'eql))
          nodes stack)
      (dolist (data gnus-newsgroup-data)
        (puthash (gnus-data-number data)
                 (gnus-data-level data) levels))
      (save-excursion
        (goto-char (point-min))
        (while (< (point) (point-max))
          (when (get-text-property
                 (point) 'tessera-gnus-summary-entry)
            (let* ((id (get-text-property (point) 'gnus-number))
                   (level (or (gethash id levels) 0)))
              (while (and stack (>= (caar stack) level))
                (pop stack))
              (let* ((parent (cdar stack))
                     (root (if parent
                               (tessera-thread-context-root parent)
                             id))
                     (node (make-tessera-thread-context
                            :id id :root root
                            :parent (and parent
                                         (tessera-thread-context-id
                                          parent))
                            :first (null parent))))
                (push node nodes)
                (push (cons level node) stack)
                (puthash root (1+ (gethash root totals 0)) totals)
                (when parent
                  (puthash (tessera-thread-context-id parent)
                           id last-child))
                (unless (gnus-read-mark-p (char-after))
                  (puthash root (1+ (gethash root unreads 0))
                           unreads))
                (unless (invisible-p (point))
                  (puthash root id last-visible))
                (puthash id node tessera-gnus-thread--contexts))))
          (forward-line 1)))
      (dolist (node (nreverse nodes))
        (let* ((root (tessera-thread-context-root node))
               (parent-id (tessera-thread-context-parent node))
               (parent (gethash parent-id
                                tessera-gnus-thread--contexts))
               (total (gethash root totals))
               (unread (gethash root unreads 0)))
          (setf (tessera-thread-context-total node) total
                (tessera-thread-context-unread node) unread
                (tessera-thread-context-last node)
                (eql (tessera-thread-context-id node)
                     (gethash root last-visible)))
          (when parent
            (setf (tessera-thread-context-path node)
                  (append
                   (tessera-thread-context-path parent)
                   (list (not (eql (tessera-thread-context-id node)
                                   (gethash parent-id
                                            last-child)))))))
          (when (tessera-thread-context-first node)
            (setq tessera-gnus-thread--width
                  (max tessera-gnus-thread--width
                       (length (format "%d/%d" unread total))))))))))

(provide 'tessera-gnus-thread)
;;; tessera-gnus-thread.el ends here
