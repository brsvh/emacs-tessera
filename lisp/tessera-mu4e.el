;;; tessera-mu4e.el --- Tessera UI for mu4e  -*- lexical-binding: t; -*-

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

;; Public options, faces, and `tessera-mu4e-mode' live here.
;; Rendering and folding follow `mu4e-headers' and `mu4e-thread'.

;;; Code:

(require 'tessera)

(defgroup tessera-mu4e nil
  "Tessera interfaces for mu4e."
  :group 'tessera
  :prefix "tessera-mu4e-")

;;;; Advanced features

(defcustom tessera-mu4e-x-subthread-scope 'results
  "Default source of context subthread members.
Results includes folded rows.  Local index can supplement replies
outside the active filter, but cannot include unindexed mail."
  :type '(choice (const results) (const local-index))
  :group 'tessera-mu4e)

(defcustom tessera-mu4e-x-today-query-function
  'tessera-mu4e-x--today-query
  "Function returning the base query for today's context.
By default use the Headers query or the Main query item at point.
A custom function may supply an account-specific query."
  :type 'function
  :group 'tessera-mu4e)

;;;; Public faces

(defgroup tessera-mu4e-headers nil
  "Tessera message headers for mu4e."
  :group 'tessera-mu4e
  :prefix "tessera-mu4e-headers-")

(defface tessera-mu4e-headers-subject-face
  '((t :extend nil))
  "Subject adjustments composed over native message state."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-unread-subject-face
  '((t :inherit (bold tessera-mu4e-headers-subject-face)
       :extend nil))
  "Unread emphasis over the native subject state."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-read-contact-face
  '((t :inherit (mu4e-header-face default)
       :weight normal :slant italic :extend nil))
  "Read contacts, independent of special message states."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-unread-contact-face
  '((t :inherit (mu4e-unread-face default)
       :weight bold :slant italic :extend nil))
  "Unread contacts, independent of special message states."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-thread-subject-face
  '((t :inherit (mu4e-header-face default)
       :weight bold :slant normal :extend nil))
  "Subjects of threads with no unread messages."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-thread-unread-subject-face
  '((t :inherit (mu4e-unread-face default)
       :weight bold :slant normal :extend nil))
  "Subjects of threads containing unread messages."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-thread-contact-face
  '((t :slant italic :extend nil))
  "Contact adjustments over each message's native state."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-thread-unread-contact-face
  '((t :inherit tessera-mu4e-headers-thread-contact-face
       :weight bold :extend nil))
  "Unread contacts retaining their native state and italics."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-thread-tree-face
  '((t :inherit mu4e-header-face :extend nil))
  "Thread branches."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-thread-count-face
  '((t :inherit (mu4e-header-face default) :extend nil))
  "Counts of threads with no unread messages."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-thread-unread-count-face
  '((t :inherit (mu4e-unread-face default)
       :weight bold :extend nil))
  "Counts of threads containing unread messages."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-date-face
  '((t :inherit (mu4e-header-face default)
       :weight normal :slant normal :extend nil))
  "Read message dates."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-unread-date-face
  '((t :inherit (mu4e-unread-face default)
       :weight bold :slant normal :extend nil))
  "Unread message dates."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-label-face
  '((t :inherit mu4e-header-value-face
       :weight normal :slant normal :extend nil))
  "Individual classification labels, excluding separators."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-operation-face
  '((t :inherit tessera-glyph-attention-face :extend nil))
  "Pending actions, separate from current message state."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-destructive-operation-face
  '((t :inherit (tessera-glyph-negative-face
                 tessera-mu4e-headers-operation-face)
       :extend nil))
  "Pending trash and deletion actions."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-new-face
  '((t :inherit tessera-glyph-accent-face :extend nil))
  "New message status icons."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-unread-face
  '((t :inherit tessera-glyph-accent-face :extend nil))
  "Unread message status icons."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-read-face
  '((t :inherit tessera-glyph-muted-face :extend nil))
  "Read message status icons."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-draft-face
  '((t :inherit tessera-glyph-informational-face :extend nil))
  "Draft status icons."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-trashed-face
  '((t :inherit tessera-glyph-negative-face :extend nil))
  "Existing trashed flags, separate from pending deletion."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-flagged-face
  '((t :inherit tessera-glyph-attention-face :extend nil))
  "User-flagged message icons."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-replied-face
  '((t :inherit tessera-glyph-positive-face :extend nil))
  "Replied message icons."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-forwarded-face
  '((t :inherit tessera-glyph-informational-face :extend nil))
  "Forwarded message icons."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-high-priority-face
  '((t :inherit tessera-glyph-warning-face :extend nil))
  "High message priority, independent of user flagging."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-low-priority-face
  '((t :inherit tessera-glyph-muted-face :extend nil))
  "Low message priority."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-list-face
  '((t :inherit tessera-glyph-informational-face :extend nil))
  "Mailing list attribute icons."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-personal-face
  '((t :inherit tessera-glyph-accent-face :extend nil))
  "Personal message attribute icons."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-attachment-face
  '((t :inherit tessera-glyph-informational-face :extend nil))
  "Attachment presence, without implying a known count."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-signature-face
  '((t :inherit tessera-glyph-informational-face :extend nil))
  "Signature presence, without implying verification."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-encryption-face
  '((t :inherit tessera-glyph-accent-face :extend nil))
  "Encrypted content, without implying decryption."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-calendar-face
  '((t :inherit tessera-glyph-attention-face :extend nil))
  "Calendar invitation icons."
  :group 'tessera-mu4e-headers)

;;;; Adapter lifecycle

(declare-function tessera-mu4e-headers--enable "tessera-mu4e-headers")
(declare-function tessera-mu4e-headers--disable
                  "tessera-mu4e-headers")
(declare-function tessera-mu4e-headers--register
                  "tessera-mu4e-headers")
(declare-function tessera-mu4e-headers--navigation
                  "tessera-mu4e-headers")

(defun tessera-mu4e--enable-headers ()
  "Enable Tessera in the current mu4e headers buffer."
  (require 'mu4e-headers)
  (require 'tessera-mu4e-headers)
  (tessera-mu4e-headers--register)
  (tessera-mu4e-headers--enable))

;;;###autoload
(define-minor-mode tessera-mu4e-mode
  "Toggle Tessera layouts in mu4e headers.
Non-thread results use `tessera-entry-layout'.  Native threading
automatically selects the shared thread layout."
  :global t
  :group 'tessera-mu4e
  (if tessera-mu4e-mode
      (add-hook 'mu4e-headers-mode-hook
                #'tessera-mu4e--enable-headers)
    (remove-hook 'mu4e-headers-mode-hook
                 #'tessera-mu4e--enable-headers))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'mu4e-headers-mode)
        (if tessera-mu4e-mode
            (tessera-mu4e--enable-headers)
          (when (featurep 'tessera-mu4e-headers)
            (tessera-mu4e-headers--disable))))))
  (when (and (not tessera-mu4e-mode)
             (featurep 'tessera-mu4e-headers))
    (tessera-mu4e-headers--navigation nil)))

(provide 'tessera-mu4e)
;;; tessera-mu4e.el ends here
