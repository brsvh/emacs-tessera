;;; tessera-gnus-summary.el --- Tessera interface for Gnus Summary  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bingshan Chang <chang@bingshan.org>

;; Author: Bingshan Chang <chang@bingshan.org>
;; Maintainer: Bingshan Chang <chang@bingshan.org>

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

;; Header-line presentation and lifecycle for native Gnus Summary
;; buffers.

;;; Code:

(require 'gnus-sum)
(require 'tessera-ui)

(defconst tessera-gnus-summary--header-line-format
  '(:eval (tessera-gnus-summary--header-line))
  "Header-line format installed in Gnus Summary buffers.")

(defvar tessera-gnus-summary--enabled-p nil
  "Non-nil when the Tessera Gnus Summary interface is enabled.")

(defvar-local tessera-gnus-summary--installed-p nil
  "Non-nil when Tessera owns the current Summary presentation.")

(defvar-local tessera-gnus-summary--installed-header-line-format nil
  "Exact header-line format object installed by Tessera.")

(defvar-local tessera-gnus-summary--original-header-line-format nil
  "Header-line format saved before installing Tessera.")

(defvar-local tessera-gnus-summary--original-header-line-local-p nil
  "Non-nil when `header-line-format' was originally buffer-local.")

(defvar-local tessera-gnus-summary--status-state 'success
  "Semantic status of the current Summary operation.")

(defvar-local tessera-gnus-summary--fetch-current nil
  "Number of headers parsed during the current fetch operation.")

(defvar-local tessera-gnus-summary--fetch-total nil
  "Number of headers requested by the current fetch operation.")

(defvar-local tessera-gnus-summary--fetch-failed nil
  "Number of reliably missing headers in the last fetch operation.")

(defvar-local tessera-gnus-summary--fetch-redraw-step 1
  "Number of parsed headers between fetch progress redraws.")

(defvar-local tessera-gnus-summary--fetch-next-redraw 1
  "Next parsed header count that requests a progress redraw.")

(defvar tessera-gnus-summary--fetch-buffer nil
  "Summary buffer whose synchronous Gnus fetch is active.")

(defun tessera-gnus-summary--total ()
  "Return Gnus's estimated total for the current group."
  (if gnus-newsgroup-active
      (range-length (list gnus-newsgroup-active))
    (length gnus-newsgroup-articles)))

(defun tessera-gnus-summary--statistics ()
  "Return statistics for the current native Gnus Summary snapshot."
  (let ((unread (length gnus-newsgroup-unreads))
        (visible (length gnus-newsgroup-data))
        (total (tessera-gnus-summary--total)))
    (tessera-ui-statistics unread visible total)))

(defun tessera-gnus-summary--format-status ()
  "Return the presentation of the current Summary operation status."
  (pcase tessera-gnus-summary--status-state
    ('processing
     (propertize
      (if tessera-gnus-summary--fetch-total
          (format "FETCHING %d/%d"
                  tessera-gnus-summary--fetch-current
                  tessera-gnus-summary--fetch-total)
        "FETCHING")
      'face 'tessera-header-status-processing
      'help-echo "Gnus is fetching article headers"))
    ('fail
     (propertize
      (if tessera-gnus-summary--fetch-failed
          (format "FETCH FAILED %d" tessera-gnus-summary--fetch-failed)
        "FETCH FAILED")
      'face 'tessera-header-status-fail
      'help-echo "The last Gnus header fetch failed"))
    (_
     (propertize
      "IDLE"
      'face 'tessera-header-status-success
      'help-echo "Gnus is idle"))))

(defun tessera-gnus-summary--header-line ()
  "Return the Tessera header for the current Summary buffer."
  (tessera-ui-header-line
   (tessera-gnus-summary--format-status)
   (tessera-ui-query "GROUP" gnus-newsgroup-name)
   (tessera-gnus-summary--statistics)))

(defun tessera-gnus-summary--redraw-status ()
  "Redisplay the current Summary status immediately."
  (force-mode-line-update)
  (redisplay))

(defun tessera-gnus-summary--begin-fetch (total)
  "Begin presenting a fetch of TOTAL article headers."
  (setq tessera-gnus-summary--status-state 'processing
        tessera-gnus-summary--fetch-current 0
        tessera-gnus-summary--fetch-total total
        tessera-gnus-summary--fetch-failed nil
        tessera-gnus-summary--fetch-redraw-step
        (max 1 (ceiling total 100))
        tessera-gnus-summary--fetch-next-redraw 1)
  (tessera-gnus-summary--redraw-status))

(defun tessera-gnus-summary--finish-fetch (completed reliable-p)
  "Finish a fetch with COMPLETED headers.

When RELIABLE-P is non-nil, present the number of missing headers."
  (let* ((failed (and reliable-p
                      (- tessera-gnus-summary--fetch-total completed)))
         (failed-p (and failed (> failed 0))))
    (setq tessera-gnus-summary--status-state
          (if failed-p 'fail 'success)
          tessera-gnus-summary--fetch-current nil
          tessera-gnus-summary--fetch-total nil
          tessera-gnus-summary--fetch-failed
          (and failed-p failed)))
  (tessera-gnus-summary--redraw-status))

(defun tessera-gnus-summary--fail-fetch ()
  "Finish presenting the current fetch as failed."
  (setq tessera-gnus-summary--status-state 'fail
        tessera-gnus-summary--fetch-current nil
        tessera-gnus-summary--fetch-total nil
        tessera-gnus-summary--fetch-failed nil)
  (tessera-gnus-summary--redraw-status))

(defun tessera-gnus-summary--record-header (header)
  "Record one native parse during an active fetch and return HEADER."
  (when (and header
             (buffer-live-p tessera-gnus-summary--fetch-buffer))
    (with-current-buffer tessera-gnus-summary--fetch-buffer
      (when (and (eq tessera-gnus-summary--status-state 'processing)
                 (< tessera-gnus-summary--fetch-current
                    tessera-gnus-summary--fetch-total))
        (setq tessera-gnus-summary--fetch-current
              (1+ tessera-gnus-summary--fetch-current))
        (when (or (>= tessera-gnus-summary--fetch-current
                      tessera-gnus-summary--fetch-next-redraw)
                  (= tessera-gnus-summary--fetch-current
                     tessera-gnus-summary--fetch-total))
          (setq tessera-gnus-summary--fetch-next-redraw
                (+ tessera-gnus-summary--fetch-current
                   tessera-gnus-summary--fetch-redraw-step))
          (tessera-gnus-summary--redraw-status)))))
  header)

(defun tessera-gnus-summary--track-fetch
    (orig-fun articles &optional limit force-new dependencies)
  "Call ORIG-FUN for ARTICLES while displaying fetch progress.

LIMIT, FORCE-NEW, and DEPENDENCIES are passed to
`gnus-fetch-headers'."
  (let ((buffer (current-buffer)))
    (if tessera-gnus-summary--installed-p
        (let ((tessera-gnus-summary--fetch-buffer buffer))
          (tessera-gnus-summary--begin-fetch (length articles))
          (condition-case err
              (let ((headers
                     (funcall orig-fun articles limit force-new dependencies)))
                (with-current-buffer buffer
                  (tessera-gnus-summary--finish-fetch
                   (length headers)
                   force-new))
                headers)
            ((error quit)
             (with-current-buffer buffer
               (tessera-gnus-summary--fail-fetch))
             (signal (car err) (cdr err)))))
      (funcall orig-fun articles limit force-new dependencies))))

(defun tessera-gnus-summary--add-fetch-advice ()
  "Add advice used to present native Gnus fetch progress."
  (advice-add 'gnus-fetch-headers :around
              #'tessera-gnus-summary--track-fetch)
  (advice-add 'nnheader-parse-nov :filter-return
              #'tessera-gnus-summary--record-header)
  (advice-add 'nnheader-parse-head :filter-return
              #'tessera-gnus-summary--record-header))

(defun tessera-gnus-summary--remove-fetch-advice ()
  "Remove advice used to present native Gnus fetch progress."
  (advice-remove 'gnus-fetch-headers
                 #'tessera-gnus-summary--track-fetch)
  (advice-remove 'nnheader-parse-nov
                 #'tessera-gnus-summary--record-header)
  (advice-remove 'nnheader-parse-head
                 #'tessera-gnus-summary--record-header))

(defun tessera-gnus-summary--install ()
  "Install Tessera in the current Gnus Summary buffer."
  (unless tessera-gnus-summary--installed-p
    (setq tessera-gnus-summary--original-header-line-local-p (local-variable-p 'header-line-format)
          tessera-gnus-summary--original-header-line-format header-line-format
          tessera-gnus-summary--installed-header-line-format tessera-gnus-summary--header-line-format
          tessera-gnus-summary--installed-p t
          tessera-gnus-summary--status-state 'success
          tessera-gnus-summary--fetch-current nil
          tessera-gnus-summary--fetch-total nil
          tessera-gnus-summary--fetch-failed nil)
    (setq-local header-line-format tessera-gnus-summary--installed-header-line-format)
    (force-mode-line-update)))

(defun tessera-gnus-summary--restore ()
  "Restore the native header line in the current Gnus Summary buffer."
  (when tessera-gnus-summary--installed-p
    (when (eq header-line-format tessera-gnus-summary--installed-header-line-format)
      (if tessera-gnus-summary--original-header-line-local-p
          (setq-local header-line-format tessera-gnus-summary--original-header-line-format)
        (kill-local-variable 'header-line-format)))
    (setq tessera-gnus-summary--installed-p nil
          tessera-gnus-summary--installed-header-line-format nil
          tessera-gnus-summary--original-header-line-format nil
          tessera-gnus-summary--original-header-line-local-p nil)
    (force-mode-line-update)))

(defun tessera-gnus-summary-enable ()
  "Enable Tessera in existing and future Summary buffers."
  (unless tessera-gnus-summary--enabled-p
    (setq tessera-gnus-summary--enabled-p t)
    (tessera-gnus-summary--add-fetch-advice)
    (add-hook 'gnus-summary-mode-hook #'tessera-gnus-summary--install)
    (dolist (buffer (match-buffers '(derived-mode . gnus-summary-mode)))
      (with-current-buffer buffer
        (tessera-gnus-summary--install)))))

(defun tessera-gnus-summary-disable ()
  "Disable Tessera presentation in existing Gnus Summary buffers."
  (when tessera-gnus-summary--enabled-p
    (setq tessera-gnus-summary--enabled-p nil)
    (tessera-gnus-summary--remove-fetch-advice)
    (remove-hook 'gnus-summary-mode-hook #'tessera-gnus-summary--install)
    (dolist (buffer (match-buffers '(derived-mode . gnus-summary-mode)))
      (with-current-buffer buffer
        (tessera-gnus-summary--restore)))))

(provide 'tessera-gnus-summary)
;;; tessera-gnus-summary.el ends here
