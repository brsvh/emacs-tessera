;;; tessera-mu4e-vars.el --- Mu4e shared faces  -*- lexical-binding: t; -*-

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

;; Follow `mu4e-vars' for shared face declarations.
;; Public options and the mode live in `tessera-mu4e'.
;; Message-state composition belongs to the headers adapter.

;;; Code:

(require 'tessera-mu4e)

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
  '((t :inherit mu4e-header-marks-face :extend nil))
  "Pending actions, separate from current message state."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-new-face
  '((t :inherit (mu4e-unread-face default)
       :weight bold :extend nil))
  "New message status icons."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-unread-face
  '((t :inherit (mu4e-unread-face default)
       :weight bold :extend nil))
  "Unread message status icons."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-read-face
  '((t :inherit (mu4e-header-face default) :extend nil))
  "Read message status icons."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-draft-face
  '((t :inherit mu4e-draft-face :extend nil))
  "Draft status icons."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-trashed-face
  '((t :inherit mu4e-trashed-face :extend nil))
  "Existing trashed flags, separate from pending deletion."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-flagged-face
  '((t :inherit mu4e-flagged-face :extend nil))
  "User-flagged message icons."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-replied-face
  '((t :inherit mu4e-replied-face :extend nil))
  "Replied message icons."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-forwarded-face
  '((t :inherit mu4e-forwarded-face :extend nil))
  "Forwarded message icons."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-high-priority-face
  '((t :inherit mu4e-warning-face :extend nil))
  "High message priority, independent of user flagging."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-low-priority-face
  '((t :inherit shadow :extend nil))
  "Low message priority."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-list-face
  '((t :inherit mu4e-special-header-value-face :extend nil))
  "Mailing list attribute icons."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-personal-face
  '((t :inherit mu4e-special-header-value-face :extend nil))
  "Personal message attribute icons."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-attachment-face
  '((t :inherit shadow :extend nil))
  "Attachment presence, without implying a known count."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-signature-face
  '((t :inherit mu4e-special-header-value-face :extend nil))
  "Signature presence, without implying verification."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-encryption-face
  '((t :inherit mu4e-special-header-value-face :extend nil))
  "Encrypted content, without implying decryption."
  :group 'tessera-mu4e-headers)

(defface tessera-mu4e-headers-calendar-face
  '((t :inherit mu4e-special-header-value-face :extend nil))
  "Calendar invitation icons."
  :group 'tessera-mu4e-headers)

(provide 'tessera-mu4e-vars)
;;; tessera-mu4e-vars.el ends here
