;;; tessera-x-elfeed.el --- Experimental Tessera features for Elfeed  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bingshan Chang <chang@bingshan.org>

;; Author: Bingshan Chang <chang@bingshan.org>
;; Maintainer: Bingshan Chang <chang@bingshan.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (tessera "0.1.0")
;;                   (tessera-x "0.1.0") (elfeed "4.0.1"))
;; Keywords: convenience, mail, news
;; URL: https://github.com/brsvh/emacs-tessera

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

;; Experimental Tessera features for Elfeed.
;; The `x' in this package family stands for experimental.
;; Currently provides context snapshots independent of layout modes.

;;; Code:

(require 'tessera-x)
(require 'tessera-elfeed)
(require 'elfeed-search)
(require 'url-http)

(defvar elfeed-tree-filter)
(defvar url-http-response-status)
(defvar url-http-end-of-headers)

(defgroup tessera-x-elfeed nil
  "Experimental Tessera features for Elfeed."
  :group 'tessera-x
  :group 'tessera-elfeed
  :prefix "tessera-x-elfeed-")

;;;; Context options

(defcustom tessera-x-elfeed-fetch-linked-content t
  "Whether selected-entry contexts fetch linked HTTP content.
Today contexts always use locally stored feed bodies."
  :type 'boolean
  :group 'tessera-x-elfeed)

(defcustom tessera-x-elfeed-fetch-minimum-characters nil
  "Skip context HTTP retrieval for stored bodies of this size.
Nil means fetch every selected HTTP link when fetching is enabled."
  :type '(choice (const nil) natnum)
  :group 'tessera-x-elfeed)

(defcustom tessera-x-elfeed-fetch-timeout 15
  "Seconds to fetch a context body before using stored feed content."
  :type 'natnum
  :group 'tessera-x-elfeed)

(defcustom tessera-x-elfeed-fetch-concurrency 4
  "Maximum simultaneous linked-page requests for one context."
  :type 'natnum
  :group 'tessera-x-elfeed)

;;;; Context snapshots

(cl-defstruct tessera-x-elfeed--request
  "Bounded HTTP work for a context snapshot."
  context queue active dispatching (limit 4) timeout)

(cl-defstruct tessera-x-elfeed--fetch
  "One HTTP transfer, completed at most once."
  request item buffer timer done)

(defun tessera-x-elfeed--item (entry)
  "Snapshot ENTRY and its stored feed content."
  (let* ((feed (elfeed-entry-feed entry))
         (content (or (elfeed-deref (elfeed-entry-content entry)) ""))
         (item
          (make-tessera-x-item
           :id (copy-tree (elfeed-entry-id entry))
           :subject (elfeed-entry-title entry)
           :date (seconds-to-time (elfeed-entry-date entry))
           :group (elfeed-feed-title feed)
           :data (elfeed-entry-link entry)
           :metadata
           (list (cons "URL" (elfeed-entry-link entry))
                 (cons "Feed URL" (elfeed-feed-url feed))
                 (cons "Authors" (elfeed-meta entry :authors))
                 (cons "Enclosures"
                       (mapconcat
                        (lambda (enclosure)
                          (format "%s (%s)" (car enclosure)
                                  (cadr enclosure)))
                        (elfeed-entry-enclosures entry) "; "))
                 (cons "Labels"
                       (mapconcat #'symbol-name
                                  (elfeed-entry-tags entry) ",")))
           :body (if (eq (elfeed-entry-content-type entry) 'html)
                     (tessera-x-html-text content)
                   (substring-no-properties content)))))
    (when (string-empty-p (tessera-x-item-body item))
      (setf (tessera-x-item-note item) "No stored feed body"))
    item))

(defun tessera-x-elfeed--group-feeds (items)
  "Group ITEMS by feed identity, retaining their order per feed."
  (let ((groups (make-hash-table :test #'equal)) order)
    (dolist (item items)
      (let ((key (cdr (assoc "Feed URL"
                             (tessera-x-item-metadata item)))))
        (unless (gethash key groups) (push key order))
        (push item (gethash key groups))))
    (cl-mapcan (lambda (key) (nreverse (gethash key groups)))
               (nreverse order))))

(defun tessera-x-elfeed--stop-fetch (fetch)
  "Release FETCH's timer, response buffers and redirected transfers."
  (when (timerp (tessera-x-elfeed--fetch-timer fetch))
    (cancel-timer (tessera-x-elfeed--fetch-timer fetch)))
  (let ((buffer (tessera-x-elfeed--fetch-buffer fetch)))
    (while (buffer-live-p buffer)
      (let ((next (buffer-local-value 'url-redirect-buffer buffer)))
        (when-let* ((process (get-buffer-process buffer)))
          (delete-process process))
        (kill-buffer buffer)
        (setq buffer next))))
  (setf (tessera-x-elfeed--fetch-timer fetch) nil
        (tessera-x-elfeed--fetch-buffer fetch) nil))

(defun tessera-x-elfeed--cancel (request)
  "Cancel every pending transfer in REQUEST."
  (setf (tessera-x-elfeed--request-queue request) nil)
  (dolist (fetch (tessera-x-elfeed--request-active request))
    (setf (tessera-x-elfeed--fetch-done fetch) t)
    (tessera-x-elfeed--stop-fetch fetch))
  (setf (tessera-x-elfeed--request-active request) nil))

(defun tessera-x-elfeed--complete (fetch text note)
  "Complete FETCH with TEXT or feed fallback and NOTE exactly once."
  (unless (tessera-x-elfeed--fetch-done fetch)
    (setf (tessera-x-elfeed--fetch-done fetch) t)
    (let* ((request (tessera-x-elfeed--fetch-request fetch))
           (context (tessera-x-elfeed--request-context request))
           (item (tessera-x-elfeed--fetch-item fetch)))
      (setf (tessera-x-elfeed--request-active request)
            (delq fetch (tessera-x-elfeed--request-active request)))
      (tessera-x-elfeed--stop-fetch fetch)
      (when (tessera-x-context-pending-p context)
        (if (and text (not (string-empty-p text)))
            (setf (tessera-x-item-body item) text
                  (tessera-x-item-note item) "Fetched linked page")
          (setf (tessera-x-item-note item)
                (concat "Using stored feed content: " note)))
        (tessera-x-elfeed--dispatch request)))))

(defun tessera-x-elfeed--timeout (fetch)
  "Complete timed out FETCH using its stored feed content."
  (tessera-x-elfeed--complete fetch nil "HTTP timeout"))

(defun tessera-x-elfeed--response (status fetch)
  "Handle URL STATUS for FETCH without selecting its source."
  (if (tessera-x-elfeed--fetch-done fetch)
      (kill-buffer (current-buffer))
    (setf (tessera-x-elfeed--fetch-buffer fetch) (current-buffer))
    (condition-case err
        (progn
          (unless (and (not (plist-get status :error))
                       (numberp url-http-response-status)
                       (<= 200 url-http-response-status 299)
                       (markerp url-http-end-of-headers))
            (error "HTTP failure %s" url-http-response-status))
          (let* ((headers (buffer-substring-no-properties
                           (point-min) url-http-end-of-headers))
                 (case-fold-search t)
                 (type (if (string-match
                            "Content-Type: *\\([^;\r\n]+\\)" headers)
                           (downcase (match-string 1 headers))
                         "text/html"))
                 (charset
                  (when (string-match
                         "charset=[\"']?\\([^;\"' \r\n]+\\)" headers)
                    (mm-charset-to-coding-system
                     (intern (downcase (match-string 1 headers))))))
                 (text (buffer-substring-no-properties
                        url-http-end-of-headers (point-max))))
            (unless (member type '("text/html" "text/plain"
                                   "application/xhtml+xml"))
              (error "Unsupported linked content type %s" type))
            (unless (multibyte-string-p text)
              (setq text (decode-coding-string
                          text (or charset 'utf-8))))
            (tessera-x-elfeed--complete
             fetch (if (equal type "text/plain")
                       (string-trim text)
                     (tessera-x-html-text text))
             "Empty linked page")))
      (error (tessera-x-elfeed--complete
              fetch nil (error-message-string err))))))

(defun tessera-x-elfeed--start-fetch (request item)
  "Start REQUEST's transfer for ITEM, falling back on startup errors."
  (let ((fetch (make-tessera-x-elfeed--fetch
                :request request
                :item item)))
    (push fetch (tessera-x-elfeed--request-active request))
    (condition-case err
        (let ((buffer (url-retrieve
                       (tessera-x-item-data item)
                       #'tessera-x-elfeed--response
                       (list fetch) t t)))
          (unless (tessera-x-elfeed--fetch-done fetch)
            (unless buffer
              (error "Unable to start HTTP request"))
            (setf (tessera-x-elfeed--fetch-buffer fetch) buffer
                  (tessera-x-elfeed--fetch-timer fetch)
                  (run-at-time
                   (tessera-x-elfeed--request-timeout request)
                   nil #'tessera-x-elfeed--timeout fetch))))
      (error (tessera-x-elfeed--complete
              fetch nil (error-message-string err))))))

(defun tessera-x-elfeed--dispatch (request)
  "Start bounded transfers from REQUEST, or publish when done."
  (let ((context (tessera-x-elfeed--request-context request)))
    (when (and (tessera-x-context-pending-p context)
               (not (tessera-x-elfeed--request-dispatching request)))
      (setf (tessera-x-elfeed--request-dispatching request) t)
      (unwind-protect
          (while (and (tessera-x-elfeed--request-queue request)
                      (< (length
                          (tessera-x-elfeed--request-active request))
                         (tessera-x-elfeed--request-limit request)))
            (tessera-x-elfeed--start-fetch
             request (pop (tessera-x-elfeed--request-queue request))))
        (setf (tessera-x-elfeed--request-dispatching request) nil))
      (unless (or (tessera-x-elfeed--request-active request)
                  (tessera-x-elfeed--request-queue request))
        (tessera-x-context-finish context)))))

(defun tessera-x-elfeed--build-context (entries scope local-only)
  "Build a context from ENTRIES in SCOPE.
Suppress HTTP retrieval when LOCAL-ONLY is set."
  (let* ((items (tessera-x-elfeed--group-feeds
                 (mapcar #'tessera-x-elfeed--item entries)))
         (context (tessera-x-context-start 'elfeed scope items))
         (threshold tessera-x-elfeed-fetch-minimum-characters)
         (request
          (make-tessera-x-elfeed--request
           :context context
           :limit (max 1 tessera-x-elfeed-fetch-concurrency)
           :timeout tessera-x-elfeed-fetch-timeout
           :queue
           (unless (or local-only
                       (not tessera-x-elfeed-fetch-linked-content))
             (cl-remove-if-not
              (lambda (item)
                (and (stringp (tessera-x-item-data item))
                     (string-match-p "\\`https?://"
                                     (tessera-x-item-data item))
                     (or (null threshold)
                         (< (length (tessera-x-item-body item))
                            threshold))))
              items)))))
    (push (apply-partially #'tessera-x-elfeed--cancel request)
          (tessera-x-context-cleanup context))
    (tessera-x-elfeed--dispatch request)
    context))

;;;###autoload
(defun tessera-x-elfeed-prepare-context ()
  "Prepare marked entries, active region, or the entry at point.
Fetch linked HTTP content according to the Elfeed context options.
Return the request; its ready hook runs when fetching completes."
  (interactive)
  (unless (derived-mode-p 'elfeed-search-mode)
    (user-error "Run this command in Elfeed Search"))
  (let ((entries (elfeed-search-selected)))
    (unless entries (user-error "No Elfeed entries selected"))
    (tessera-x-elfeed--build-context entries "Selected entries" nil)))

;;;###autoload
(defun tessera-x-elfeed-prepare-today-context ()
  "Prepare today's local entries in the current Search or Tree scope.
Never fetch linked pages.  In Tree, use the native filter at point."
  (interactive)
  (let* ((scope
          (cond
           ((derived-mode-p 'elfeed-search-mode) elfeed-search-filter)
           ((derived-mode-p 'elfeed-tree-mode)
            (concat elfeed-tree-filter " "
                    (or (get-text-property
                         (line-beginning-position) 'elfeed-filter)
                        (user-error "No Elfeed filter at point"))))
           (t (user-error "Run in Elfeed Search or Tree"))))
         (filter (elfeed-search-parse-filter scope))
         (bounds (tessera-x-today-bounds))
         (start (float-time (car bounds)))
         (end (float-time (cdr bounds)))
         (now (float-time))
         (count 0)
         entries)
    (elfeed-db-visit (entry feed)
                     (let ((date (elfeed-entry-date entry)))
                       (when (< date start) (elfeed-db-return))
                       (when (and (< date end)
                                  (elfeed-search-filter
                                   filter entry feed count now))
                         (cl-incf count)
                         (push entry entries))))
    (tessera-x-elfeed--build-context
     entries (format "Today, local feed database; filter: %s" scope)
     t)))

(provide 'tessera-x-elfeed)
;;; tessera-x-elfeed.el ends here
