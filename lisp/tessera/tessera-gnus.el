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

;; Public customization, faces, and `tessera-gnus-mode' live here,
;; following the public definitions in `gnus'.  Summary and article
;; adapters follow `gnus-sum' and `gnus-art', respectively.

;;; Code:

(require 'tessera)

(defgroup tessera-gnus nil
  "Tessera interfaces for Gnus."
  :group 'tessera
  :prefix "tessera-gnus-")

;;;; Experimental context snapshots

(defcustom tessera-x-gnus-body-policy 'download
  "How selected and subthread contexts obtain article bodies.
Download fetches missing bodies into the Agent.  Local-only keeps
metadata and an explicit note when the Agent has no body.
Today contexts always use local-only, regardless of this option."
  :type '(choice (const download) (const local-only))
  :group 'tessera-gnus)

(defcustom tessera-x-gnus-subthread-scope 'results
  "Default source of context subthread members.
Results includes folded articles; local-index also consults the
current group's Agent overview, which can itself be incomplete."
  :type '(choice (const results) (const local-index))
  :group 'tessera-gnus)

;;;; Public faces (gnus.el)

(defgroup tessera-gnus-summary nil
  "Tessera entries in Gnus summary buffers."
  :group 'tessera-gnus
  :prefix "tessera-gnus-summary-")

(defface tessera-gnus-summary-subject-face
  '((t :inherit gnus-header-subject :extend nil))
  "Base face for article subjects, below the native state face."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-unread-subject-face
  '((t :inherit (bold tessera-gnus-summary-subject-face)
       :extend nil))
  "Face for unread article subjects."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-thread-subject-face
  '((t :inherit (bold gnus-summary-normal-read) :extend nil))
  "Face for subjects of threads with no unread articles."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-thread-unread-subject-face
  '((t :inherit (bold gnus-summary-normal-unread) :extend nil))
  "Face for subjects of threads containing unread articles."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-author-face
  '((t :inherit (italic gnus-header-from)
       :weight normal :extend nil))
  "Face for article authors."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-read-author-face
  '((t :inherit gnus-summary-normal-read
       :weight normal :slant italic :extend nil))
  "Face for read authors outside thread layouts."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-unread-author-face
  '((t :inherit (bold gnus-summary-normal-unread)
       :slant italic :extend nil))
  "Face for unread authors outside thread layouts."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-date-face
  '((t :inherit gnus-summary-normal-read
       :weight normal :slant normal :extend nil))
  "Face for read article dates."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-unread-date-face
  '((t :inherit (bold gnus-summary-normal-unread)
       :slant normal :extend nil))
  "Face for unread article dates."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-label-face
  '((t :inherit gnus-header-content
       :weight normal :slant normal :extend nil))
  "Face for article labels from every supported source."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-status-face
  '((t :inherit tessera-glyph-accent-face :extend nil))
  "Article status icons."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-muted-face
  '((t :inherit tessera-glyph-muted-face :extend nil))
  "Read and inactive article status icons."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-important-face
  '((t :inherit tessera-glyph-attention-face :extend nil))
  "Ticked and processing status icons."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-positive-face
  '((t :inherit tessera-glyph-positive-face :extend nil))
  "Completed action and availability icons."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-informational-face
  '((t :inherit tessera-glyph-informational-face :extend nil))
  "Informational article status icons."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-warning-face
  '((t :inherit tessera-glyph-warning-face :extend nil))
  "Article states requiring attention."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-error-face
  '((t :inherit tessera-glyph-negative-face :extend nil))
  "Failed actions and content processing errors."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-spam-face
  '((t :inherit error :extend nil))
  "Spam article state, supplementing the native summary face."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-expirable-face
  '((t :inherit warning :extend nil))
  "Expirable article state, supplementing the native summary face."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-high-score-face
  '((t :inherit tessera-glyph-attention-face :extend nil))
  "Scores above the native threshold."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-low-score-face
  '((t :inherit tessera-glyph-muted-face :extend nil))
  "Scores below the native threshold."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-attachment-face
  '((t :inherit tessera-glyph-informational-face :extend nil))
  "Attachment presence, without implying trust."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-signature-face
  '((t :inherit tessera-glyph-informational-face :extend nil))
  "Signature presence, without implying verification."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-encryption-face
  '((t :inherit tessera-glyph-accent-face :extend nil))
  "Encrypted content, without implying decryption."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-thread-tree-face
  '((t :inherit shadow :extend nil))
  "Native thread branches."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-thread-count-face
  '((t :inherit (gnus-summary-normal-read shadow) :extend nil))
  "Thread counts with no unread articles."
  :group 'tessera-gnus-summary)

(defface tessera-gnus-summary-thread-unread-count-face
  '((t :inherit (gnus-summary-normal-unread bold) :extend nil))
  "Thread counts containing unread articles."
  :group 'tessera-gnus-summary)

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

(defun tessera-gnus--map-summary-buffers (function)
  "Call FUNCTION in each live Gnus summary buffer."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'gnus-summary-mode)
        (funcall function)))))

(defun tessera-gnus--enable-summary ()
  "Enable Tessera in the current Gnus summary buffer."
  (require 'tessera-gnus-summary)
  (tessera-gnus-summary--register)
  (tessera-gnus-summary--enable))

;;;###autoload
(define-minor-mode tessera-gnus-mode
  "Toggle Tessera UI adapters for Gnus buffers."
  :global t
  :group 'tessera-gnus
  (if tessera-gnus-mode
      (progn
        (require 'tessera-gnus-article)
        (require 'tessera-gnus-summary)
        (tessera-gnus-summary--track-folds t)
        (tessera-gnus-summary--navigation t)
        (tessera-gnus-article--track-content t)
        (add-hook 'gnus-summary-mode-hook
                  #'tessera-gnus--enable-summary)
        (tessera-gnus--map-summary-buffers
         #'tessera-gnus--enable-summary)
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (when (derived-mode-p 'gnus-article-mode)
              (tessera-gnus-article--updated)))))
    (remove-hook 'gnus-summary-mode-hook
                 #'tessera-gnus--enable-summary)
    (when (featurep 'tessera-gnus-article)
      (tessera-gnus-article--track-content nil))
    (when (featurep 'tessera-gnus-summary)
      (tessera-gnus-summary--track-folds nil)
      (tessera-gnus-summary--navigation nil)
      (tessera-gnus--map-summary-buffers
       #'tessera-gnus-summary--disable))))

(provide 'tessera-gnus)
;;; tessera-gnus.el ends here
