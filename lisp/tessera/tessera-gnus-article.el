;;; tessera-gnus-article.el --- Gnus article observations  -*- lexical-binding: t; -*-

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

;; Follow `gnus-art' for prepared-article hooks and MIME handles.
;; Inspect existing handles without fetching, verifying or decrypting.
;; The summary adapter owns per-article caches and row rendering.

;;; Code:

(require 'tessera)
(require 'cl-lib)
(require 'gnus-art)
(require 'mm-decode)
(require 'seq)
(require 'subr-x)

(defvar tessera-gnus-summary--active)
(defvar tessera-gnus-summary--updating)
(declare-function tessera-gnus-summary--observe-content
                  "tessera-gnus-summary")
(declare-function tessera-gnus-summary--sync-line
                  "tessera-gnus-summary")

;;;; Observed MIME properties

(defvar tessera-gnus-article--control-types
  '("application/pgp-signature" "application/pgp-encrypted"
    "application/pkcs7-signature" "application/x-pkcs7-signature")
  "MIME control parts that do not count as attachments.")

(defun tessera-gnus-article--unknown-content ()
  "Return a fresh set of unknown content properties."
  (list :attachment 'unknown
        :signature 'unknown
        :encryption 'unknown))

(defun tessera-gnus-article--merge-state (old new)
  "Combine independent observations OLD and NEW conservatively."
  (seq-find (lambda (state) (or (eq state old) (eq state new)))
            '(error present processed unknown)))

(defun tessera-gnus-article--attachment-p (handle)
  "Return non-nil when MIME HANDLE declares an attachment."
  (let ((disposition (car (mm-handle-disposition handle))))
    (or (equal disposition "attachment")
        (and (mm-handle-filename handle)
             (not (equal disposition "inline"))))))

(defun tessera-gnus-article--remember-disposition (type _from)
  "Retain attachment declarations on multipart content TYPE.
Run before `mm-dissect-multipart' discards the container headers."
  (save-excursion
    (save-restriction
      (mail-narrow-to-head)
      (let* ((value (mail-fetch-field "Content-Disposition"))
             (disposition
              (and value (mail-header-parse-content-disposition
                          value)))
             (handle (mm-make-handle nil type nil nil disposition)))
        (when (tessera-gnus-article--attachment-p handle)
          (put-text-property 0 (length (car type))
                             'tessera-gnus-article--attachment t
                             (car type)))))))

(defun tessera-gnus-article--mime-content (handles)
  "Inspect existing MIME HANDLES without changing or decoding them.
A processed security part has native result details, not necessarily
successful verification.  Never infer trust from a result string."
  (if (null handles)
      (tessera-gnus-article--unknown-content)
    (let ((result (list :attachment nil
                        :signature nil
                        :encryption nil))
          opaque)
      (cl-labels
          ((observe (key state details)
             (setq result
                   (plist-put result key
                              (tessera-gnus-article--merge-state
                               (plist-get result key) state)))
             (when details
               (let ((field (if (eq key :signature)
                                :signature-details
                              :encryption-details)))
                 (setq result
                       (plist-put result field
                                  (cons details
                                        (plist-get result field)))))))
           (walk (part)
             (cond
              ((stringp (car-safe part))
               (let* ((type (substring-no-properties (car part)))
                      (protocol
                       (get-text-property 0 'protocol (car part)))
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
                      (info
                       (get-text-property 0 'gnus-info (car part)))
                      (error
                       (get-text-property 0 'sec-error (car part))))
                 (when (get-text-property
                        0 'tessera-gnus-article--attachment
                        (car part))
                   (setq result (plist-put result :attachment
                                           'present)))
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
                             "\n")))
                 (if (and (eq key :encryption) (or error (not info)))
                     (setq opaque t)
                   (mapc #'walk (cdr part)))))
              ((bufferp (car-safe part))
               (let ((type (mm-handle-media-type part)))
                 (unless (member
                          type tessera-gnus-article--control-types)
                   (when (tessera-gnus-article--attachment-p part)
                     (setq result (plist-put result :attachment
                                             'present))))))
              ((consp part) (mapc #'walk part)))))
        (walk handles))
      (when opaque
        (dolist (key '(:attachment :signature))
          (unless (plist-get result key)
            (setq result (plist-put result key 'unknown)))))
      result)))

;;;; Prepared article lifecycle

(defun tessera-gnus-article--track-content (enable)
  "Observe native MIME lifecycle events when ENABLE is non-nil."
  (if enable
      (progn
        (advice-add 'mm-dissect-multipart :before
                    #'tessera-gnus-article--remember-disposition)
        (add-hook 'gnus-article-prepare-hook
                  #'tessera-gnus-article--updated t)
        (add-hook 'gnus-part-display-hook
                  #'tessera-gnus-article--queue-update t))
    (advice-remove 'mm-dissect-multipart
                   #'tessera-gnus-article--remember-disposition)
    (remove-hook 'gnus-article-prepare-hook
                 #'tessera-gnus-article--updated)
    (remove-hook 'gnus-part-display-hook
                 #'tessera-gnus-article--queue-update)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (remove-hook 'post-command-hook
                     #'tessera-gnus-article--updated t))))
  ;; Security processing can mutate existing handle properties.
  ;; Observe it explicitly, including calls outside article commands.
  (if enable
      (advice-add 'gnus-mime-security-verify-or-decrypt :after
                  #'tessera-gnus-article--updated)
    (advice-remove 'gnus-mime-security-verify-or-decrypt
                   #'tessera-gnus-article--updated)))

(defun tessera-gnus-article--queue-update ()
  "Coalesce native part-display events into one MIME observation."
  (when (derived-mode-p 'gnus-article-mode)
    (add-hook 'post-command-hook
              #'tessera-gnus-article--updated t t)))

(defun tessera-gnus-article--updated (&rest _arguments)
  "Observe the displayed article and refresh its summary entry."
  (remove-hook 'post-command-hook
               #'tessera-gnus-article--updated t)
  ;; Gnus also runs its article preparation hook in the summary.
  (if (derived-mode-p 'gnus-summary-mode)
      (when (get-buffer gnus-article-buffer)
        (with-current-buffer gnus-article-buffer
          (tessera-gnus-article--updated)))
    (when (and (derived-mode-p 'gnus-article-mode)
               gnus-summary-buffer
               (buffer-live-p (get-buffer gnus-summary-buffer)))
      (let ((handles gnus-article-mime-handles))
        (with-current-buffer gnus-summary-buffer
          (save-restriction
            (widen)
            (when (and tessera-gnus-summary--active
                       gnus-current-headers)
              (when (tessera-gnus-summary--observe-content
                     gnus-current-headers handles)
                (let ((saved-point (tessera-entry-save-point))
                      (article
                       (mail-header-number gnus-current-headers)))
                  (unwind-protect
                      (when-let* ((position
                                   (text-property-any
                                    (point-min) (point-max)
                                    'gnus-number article)))
                        (goto-char position)
                        (let ((tessera-gnus-summary--updating t))
                          (tessera-gnus-summary--sync-line t)))
                    (tessera-entry-restore-point saved-point)))
                (tessera-entry-highlight-current)))))))))

(provide 'tessera-gnus-article)
;;; tessera-gnus-article.el ends here
