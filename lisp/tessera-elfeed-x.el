;;; tessera-elfeed-x.el --- Elfeed context snapshots  -*- lexical-binding: t; -*-

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

;; Optional Elfeed context construction independent of layout modes.

;;; Code:

(require 'tessera-x)
(require 'tessera-elfeed)
(require 'elfeed-search)
(require 'url-http)

(defvar elfeed-tree-filter)
(defvar url-http-response-status)
(defvar url-http-end-of-headers)

(cl-defstruct tessera-elfeed-x--request
  "Bounded HTTP work for a context snapshot."
  context queue active dispatching (limit 4) timeout)

(cl-defstruct tessera-elfeed-x--fetch
  "One HTTP transfer, completed at most once."
  request item buffer timer done)

(defun tessera-elfeed-x--item (entry)
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

(defun tessera-elfeed-x--group-feeds (items)
  "Group ITEMS by feed identity, retaining their order per feed."
  (let ((groups (make-hash-table :test #'equal)) order)
    (dolist (item items)
      (let ((key (cdr (assoc "Feed URL"
                             (tessera-x-item-metadata item)))))
        (unless (gethash key groups) (push key order))
        (push item (gethash key groups))))
    (cl-mapcan (lambda (key) (nreverse (gethash key groups)))
               (nreverse order))))

(defun tessera-elfeed-x--stop-fetch (fetch)
  "Release FETCH's timer, response buffers and redirected transfers."
  (when (timerp (tessera-elfeed-x--fetch-timer fetch))
    (cancel-timer (tessera-elfeed-x--fetch-timer fetch)))
  (let ((buffer (tessera-elfeed-x--fetch-buffer fetch)))
    (while (buffer-live-p buffer)
      (let ((next (buffer-local-value 'url-redirect-buffer buffer)))
        (when-let* ((process (get-buffer-process buffer)))
          (delete-process process))
        (kill-buffer buffer)
        (setq buffer next))))
  (setf (tessera-elfeed-x--fetch-timer fetch) nil
        (tessera-elfeed-x--fetch-buffer fetch) nil))

(defun tessera-elfeed-x--cancel (request)
  "Cancel every pending transfer in REQUEST."
  (setf (tessera-elfeed-x--request-queue request) nil)
  (dolist (fetch (tessera-elfeed-x--request-active request))
    (setf (tessera-elfeed-x--fetch-done fetch) t)
    (tessera-elfeed-x--stop-fetch fetch))
  (setf (tessera-elfeed-x--request-active request) nil))

(defun tessera-elfeed-x--complete (fetch text note)
  "Complete FETCH with TEXT or feed fallback and NOTE exactly once."
  (unless (tessera-elfeed-x--fetch-done fetch)
    (setf (tessera-elfeed-x--fetch-done fetch) t)
    (let* ((request (tessera-elfeed-x--fetch-request fetch))
           (context (tessera-elfeed-x--request-context request))
           (item (tessera-elfeed-x--fetch-item fetch)))
      (setf (tessera-elfeed-x--request-active request)
            (delq fetch (tessera-elfeed-x--request-active request)))
      (tessera-elfeed-x--stop-fetch fetch)
      (when (tessera-x-context-pending-p context)
        (if (and text (not (string-empty-p text)))
            (setf (tessera-x-item-body item) text
                  (tessera-x-item-note item) "Fetched linked page")
          (setf (tessera-x-item-note item)
                (concat "Using stored feed content: " note)))
        (tessera-elfeed-x--dispatch request)))))

(defun tessera-elfeed-x--timeout (fetch)
  "Complete timed out FETCH using its stored feed content."
  (tessera-elfeed-x--complete fetch nil "HTTP timeout"))

(defun tessera-elfeed-x--response (status fetch)
  "Handle URL STATUS for FETCH without selecting its source."
  (if (tessera-elfeed-x--fetch-done fetch)
      (kill-buffer (current-buffer))
    (setf (tessera-elfeed-x--fetch-buffer fetch) (current-buffer))
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
            (tessera-elfeed-x--complete
             fetch (if (equal type "text/plain")
                       (string-trim text)
                     (tessera-x-html-text text))
             "Empty linked page")))
      (error (tessera-elfeed-x--complete
              fetch nil (error-message-string err))))))

(defun tessera-elfeed-x--start-fetch (request item)
  "Start REQUEST's transfer for ITEM, falling back on startup errors."
  (let ((fetch (make-tessera-elfeed-x--fetch
                :request request :item item)))
    (push fetch (tessera-elfeed-x--request-active request))
    (condition-case err
        (let ((buffer (url-retrieve
                       (tessera-x-item-data item)
                       #'tessera-elfeed-x--response
                       (list fetch) t t)))
          (unless (tessera-elfeed-x--fetch-done fetch)
            (unless buffer
              (error "Unable to start HTTP request"))
            (setf (tessera-elfeed-x--fetch-buffer fetch) buffer
                  (tessera-elfeed-x--fetch-timer fetch)
                  (run-at-time
                   (tessera-elfeed-x--request-timeout request)
                   nil #'tessera-elfeed-x--timeout fetch))))
      (error (tessera-elfeed-x--complete
              fetch nil (error-message-string err))))))

(defun tessera-elfeed-x--dispatch (request)
  "Start bounded transfers from REQUEST, or publish when done."
  (let ((context (tessera-elfeed-x--request-context request)))
    (when (and (tessera-x-context-pending-p context)
               (not (tessera-elfeed-x--request-dispatching request)))
      (setf (tessera-elfeed-x--request-dispatching request) t)
      (unwind-protect
          (while (and (tessera-elfeed-x--request-queue request)
                      (< (length
                          (tessera-elfeed-x--request-active request))
                         (tessera-elfeed-x--request-limit request)))
            (tessera-elfeed-x--start-fetch
             request (pop (tessera-elfeed-x--request-queue request))))
        (setf (tessera-elfeed-x--request-dispatching request) nil))
      (unless (or (tessera-elfeed-x--request-active request)
                  (tessera-elfeed-x--request-queue request))
        (tessera-x-context-finish context)))))

(defun tessera-elfeed-x--build (entries scope local-only)
  "Build ENTRIES in SCOPE, suppressing HTTP when LOCAL-ONLY is set."
  (let* ((items (tessera-elfeed-x--group-feeds
                 (mapcar #'tessera-elfeed-x--item entries)))
         (context (tessera-x-context-start 'elfeed scope items))
         (threshold tessera-elfeed-x-fetch-minimum-characters)
         (request
          (make-tessera-elfeed-x--request
           :context context
           :limit (max 1 tessera-elfeed-x-fetch-concurrency)
           :timeout tessera-elfeed-x-fetch-timeout
           :queue
           (unless (or local-only
                       (not tessera-elfeed-x-fetch-linked-content))
             (cl-remove-if-not
              (lambda (item)
                (and (stringp (tessera-x-item-data item))
                     (string-match-p "\\`https?://"
                                     (tessera-x-item-data item))
                     (or (null threshold)
                         (< (length (tessera-x-item-body item))
                            threshold))))
              items)))))
    (push (apply-partially #'tessera-elfeed-x--cancel request)
          (tessera-x-context-cleanup context))
    (tessera-elfeed-x--dispatch request)
    context))

;;;###autoload
(defun tessera-elfeed-x-prepare-context ()
  "Prepare marked entries, active region, or the entry at point.
Fetch linked HTTP content according to the Elfeed context options.
Return the request; its ready hook runs when fetching completes."
  (interactive)
  (unless (derived-mode-p 'elfeed-search-mode)
    (user-error "Run this command in Elfeed Search"))
  (let ((entries (elfeed-search-selected)))
    (unless entries (user-error "No Elfeed entries selected"))
    (tessera-elfeed-x--build entries "Selected entries" nil)))

;;;###autoload
(defun tessera-elfeed-x-prepare-today-context ()
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
    (tessera-elfeed-x--build
     entries (format "Today, local feed database; filter: %s" scope)
     t)))

(provide 'tessera-elfeed-x)
;;; tessera-elfeed-x.el ends here
