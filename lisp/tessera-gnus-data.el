;;; tessera-gnus-data.el --- Gnus entry metadata  -*- lexical-binding: t; -*-

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

;; Read native labels and already dissected MIME data.  This module
;; never fetches articles, verifies signatures, or decrypts content.
;; Unknown content properties remain distinct from absent ones.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'nnheader)
(require 'mm-decode)

(defconst tessera-gnus-data--control-types
  '("application/pgp-signature" "application/pgp-encrypted"
    "application/pkcs7-signature" "application/x-pkcs7-signature")
  "MIME control parts that do not count as attachments.")

(defvar gnus-registry-db)
(declare-function gnus-registry-get-id-key "gnus-registry")

(defvar-local tessera-gnus-data--content-cache nil
  "Snapshots of observed MIME properties, keyed by article identity.")

(defun tessera-gnus-data--header (name header)
  "Return extra field NAME from native HEADER, ignoring case."
  (cdr (seq-find
        (lambda (pair)
          (string-equal-ignore-case (format "%s" (car pair)) name))
        (mail-header-extra header))))

(defun tessera-gnus-data--text (value)
  "Return VALUE as safe single-line label text."
  (string-trim
   (replace-regexp-in-string
    "[[:cntrl:]]+" " " (format "%s" value))))

(defun tessera-gnus-data--gmail-labels (value)
  "Decode Gmail label VALUE without evaluating it."
  (when (stringp value)
    (setq value
          (condition-case nil
              (let ((read-circle nil)) (car (read-from-string value)))
            (error nil))))
  (when (and (proper-list-p value)
             (seq-every-p (lambda (item)
                            (or (stringp item) (symbolp item)))
                          value))
    value))

(defun tessera-gnus-data-labels (header)
  "Return labels from HEADER and the enabled registry.
Each item is (TEXT . SOURCES); equal names share one display label."
  (let* ((id (mail-header-message-id header))
         (registry
          (when (and id (bound-and-true-p gnus-registry-db)
                     (fboundp 'gnus-registry-get-id-key))
            (gnus-registry-get-id-key id 'mark)))
         (gmail (tessera-gnus-data--gmail-labels
                 (tessera-gnus-data--header "X-GM-LABELS" header)))
         (keywords (tessera-gnus-data--header "Keywords" header))
         labels)
    (when (stringp keywords)
      (setq keywords (split-string keywords "," t "[[:space:]]+")))
    (dolist (source (list (cons "Registry" registry)
                          (cons "Gmail" gmail)
                          (cons "Keywords" keywords)))
      (dolist (value (cdr source))
        (let* ((text (tessera-gnus-data--text value))
               (existing (assoc text labels)))
          (unless (string-empty-p text)
            (if existing
                (cl-pushnew (car source) (cdr existing) :test #'equal)
              (push (list text (car source)) labels))))))
    (nreverse labels)))

(defun tessera-gnus-data--unknown ()
  "Return a fresh set of unknown content properties."
  (list :attachment 'unknown :signature 'unknown
        :encryption 'unknown))

(defun tessera-gnus-data--key (header)
  "Return an identity for HEADER within its summary buffer."
  (or (mail-header-message-id header) (mail-header-number header)))

(defun tessera-gnus-data-content (header)
  "Return observed content properties for HEADER, or header hints."
  (or (and tessera-gnus-data--content-cache
           (gethash (tessera-gnus-data--key header)
                    tessera-gnus-data--content-cache))
      (let* ((result (tessera-gnus-data--unknown))
             (value (tessera-gnus-data--header "Content-Type" header))
             (type (and (stringp value)
                        (car (mail-header-parse-content-type
                              value)))))
        (pcase type
          ("multipart/signed" (setq result
                                    (plist-put result :signature
                                               'present)))
          ("multipart/encrypted" (setq result
                                       (plist-put result :encryption
                                                  'present))))
        result)))

(defun tessera-gnus-data--merge-state (old new)
  "Combine independent observations OLD and NEW conservatively."
  (car (seq-filter
        (lambda (state) (memq state (list old new)))
        '(error present processed unknown))))

(defun tessera-gnus-data--mime-content (handles)
  "Inspect existing MIME HANDLES without changing or decoding them.
A processed security part has native result details, not necessarily
successful verification.  Never infer trust from a result string."
  (if (null handles)
      (tessera-gnus-data--unknown)
    (let ((result (list :attachment nil :signature nil
                        :encryption nil))
          opaque)
      (cl-labels
          ((observe
             (key state details)
             (setq result
                   (plist-put result key
                              (tessera-gnus-data--merge-state
                               (plist-get result key) state)))
             (when details
               (let ((field (if (eq key :signature)
                                :signature-details
                              :encryption-details)))
                 (setq result
                       (plist-put result field
                                  (cons details
                                        (plist-get result field)))))))
           (walk
             (part)
             (cond
              ((stringp (car-safe part))
               (let* ((type (substring-no-properties (car part)))
                      (protocol (get-text-property
                                 0 'protocol (car part)))
                      (key
                       (cond
                        ((or (equal type "multipart/signed")
                             (and (stringp protocol)
                                  (string-suffix-p "_signed-data"
                                                   protocol)))
                         :signature)
                        ((or (equal type "multipart/encrypted")
                             (and (stringp protocol)
                                  (string-suffix-p "_enveloped-data"
                                                   protocol)))
                         :encryption)))
                      (info (get-text-property
                             0 'gnus-info (car part)))
                      (error (get-text-property
                              0 'sec-error (car part))))
                 (when key
                   (observe key (cond (error 'error)
                                      (info 'processed)
                                      (t 'present))
                            (string-join
                             (delq nil
                                   (list info
                                         (get-text-property
                                          0 'gnus-details
                                          (car part))))
                             "\n"))
                   (when (and (eq key :encryption)
                              (or error (not info)))
                     (setq opaque t)))
                 (unless (and (eq key :encryption)
                              (or error (not info)))
                   (mapc #'walk (cdr part)))))
              ((bufferp (car-safe part))
               (let ((type (mm-handle-media-type part))
                     (disposition (mm-handle-disposition part)))
                 (unless (member
                          type tessera-gnus-data--control-types)
                   (when (or (equal (car disposition) "attachment")
                             (and (mm-handle-filename part)
                                  (not (equal (car disposition)
                                              "inline"))))
                     (setq result (plist-put result :attachment
                                             'present))))))
              ((consp part) (mapc #'walk part)))))
        (walk handles))
      (when opaque
        (dolist (key '(:attachment :signature))
          (unless (plist-get result key)
            (setq result (plist-put result key 'unknown)))))
      result)))

(defun tessera-gnus-data-observe (header handles)
  "Save properties of already parsed HANDLES for HEADER.
Return non-nil only when the observed properties have changed."
  (let* ((key (tessera-gnus-data--key header))
         (content (tessera-gnus-data--mime-content handles)))
    (unless tessera-gnus-data--content-cache
      (setq tessera-gnus-data--content-cache
            (make-hash-table :test #'equal)))
    (unless (equal content
                   (gethash key tessera-gnus-data--content-cache))
      (puthash key content tessera-gnus-data--content-cache)
      t)))

(provide 'tessera-gnus-data)
;;; tessera-gnus-data.el ends here
