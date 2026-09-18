;;; tessera-x-elfeed.el --- Experimental Tessera features for Elfeed  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bingshan Chang <chang@bingshan.org>

;; Author: Bingshan Chang <chang@bingshan.org>
;; Maintainer: Bingshan Chang <chang@bingshan.org>
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
;; The `x' in the package name stands for experimental.
;; Currently provides context snapshots independent of layout modes.

;;; Code:

(require 'tessera-x)
(require 'avl-tree)
(require 'bytecomp)
(require 'dom)
(require 'mail-parse)
(require 'mail-utils)
(require 'seq)
(require 'url-http)

(defvar elfeed-db-index)
(defvar elfeed-search-filter)

(declare-function elfeed-db-ensure "elfeed-db" ())
(declare-function elfeed-db-get-entry "elfeed-db" (id))
(declare-function elfeed-deref "elfeed-db" (ref))
(declare-function elfeed-entry-content "elfeed-db" (entry))
(declare-function elfeed-entry-content-type "elfeed-db" (entry))
(declare-function elfeed-entry-date "elfeed-db" (entry))
(declare-function elfeed-entry-enclosures "elfeed-db" (entry))
(declare-function elfeed-entry-feed "elfeed-db" (entry))
(declare-function elfeed-entry-id "elfeed-db" (entry))
(declare-function elfeed-entry-link "elfeed-db" (entry))
(declare-function elfeed-entry-tags "elfeed-db" (entry))
(declare-function elfeed-entry-title "elfeed-db" (entry))
(declare-function elfeed-feed-title "elfeed-db" (feed))
(declare-function elfeed-feed-url "elfeed-db" (feed))
(declare-function elfeed-meta
                  "elfeed-db" (thing key &optional default))
(declare-function elfeed-search-compile-filter
                  "elfeed-search" (filter))
(declare-function elfeed-search-parse-filter "elfeed-search" (filter))
(declare-function elfeed-search-selected
                  "elfeed-search" (&optional ignore-region))

(defvar elfeed-tree-filter)
(defvar url-http-response-status)
(defvar url-http-end-of-headers)

(defgroup tessera-x-elfeed nil
  "Experimental Tessera features for Elfeed."
  :group 'tessera-x
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
      (let ((key
             (cdr (assoc "Feed URL" (tessera-x-item-metadata item)))))
        (unless (gethash key groups) (push key order))
        (push item (gethash key groups))))
    (cl-mapcan (lambda (key) (nreverse (gethash key groups)))
               (nreverse order))))

(defun tessera-x-elfeed--stop-fetch (fetch)
  "Release FETCH's timer, response buffers and redirected transfers."
  (when (timerp (tessera-x-elfeed--fetch-timer fetch))
    (cancel-timer (tessera-x-elfeed--fetch-timer fetch)))
  ;; URL may collect earlier redirect buffers before the response.
  ;; Callback arguments retain ownership even when that chain breaks.
  (let ((buffers (list (tessera-x-elfeed--fetch-buffer fetch))))
    (dolist (buffer (buffer-list))
      (when (and (local-variable-p 'url-callback-arguments buffer)
                 (eq (buffer-local-value
                      'url-callback-function buffer)
                     #'tessera-x-elfeed--response)
                 (eq (cadr (buffer-local-value
                            'url-callback-arguments buffer))
                     fetch))
        (push buffer buffers)))
    (dolist (buffer buffers)
      (while (buffer-live-p buffer)
        (let ((next (buffer-local-value 'url-redirect-buffer buffer)))
          (when-let* ((process (get-buffer-process buffer)))
            (delete-process process))
          (kill-buffer buffer)
          (setq buffer next)))))
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

(defun tessera-x-elfeed--bom-charset ()
  "Return the coding system identified by the HTTP body's BOM."
  (save-excursion
    (goto-char url-http-end-of-headers)
    (cond
     ((looking-at "\xEF\xBB\xBF") 'utf-8-with-signature)
     ((looking-at "\xFE\xFF") 'utf-16be-with-signature)
     ((looking-at "\xFF\xFE") 'utf-16le-with-signature))))

(defun tessera-x-elfeed--meta-charset (meta)
  "Return the coding system declared by the HTML META element."
  (let* ((pragma (dom-attr meta 'http-equiv))
         (content (dom-attr meta 'content))
         (charset
          (or (dom-attr meta 'charset)
              (and pragma content
                   (equal (downcase pragma) "content-type")
                   (mail-content-type-get
                    (mail-header-parse-content-type content)
                    'charset)))))
    (when charset
      (mm-charset-to-coding-system
       (intern (downcase (string-trim charset)))))))

(defun tessera-x-elfeed--document-charset (type)
  "Return the coding system declared in the HTTP body of TYPE."
  (when (member type '("text/html" "application/xhtml+xml"))
    (save-excursion
      (save-restriction
        (narrow-to-region url-http-end-of-headers (point-max))
        (goto-char (point-min))
        (let ((size (- (point-max) (point-min))))
          (or (and (equal type "application/xhtml+xml")
                   (save-excursion
                     (sgml-xml-auto-coding-function size)))
              (let* ((bytes
                      (buffer-substring-no-properties
                       (point-min) (point-max)))
                     ;; Preserve ASCII before decoding the body.
                     (document
                      (with-temp-buffer
                        (insert (decode-coding-string
                                 bytes 'iso-latin-1))
                        (libxml-parse-html-region
                         (point-min) (point-max)))))
                (seq-some #'tessera-x-elfeed--meta-charset
                          (dom-by-tag document 'meta)))))))))

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
          (let* ((header
                  (save-restriction
                    (narrow-to-region
                     (point-min) url-http-end-of-headers)
                    (mail-fetch-field "Content-Type")))
                 (content-type
                  (mail-header-parse-content-type
                   (or header "text/html")))
                 (type (car content-type))
                 (encoding (mail-content-type-get
                            content-type 'charset))
                 (charset
                  (or (tessera-x-elfeed--bom-charset)
                      (when encoding
                        (mm-charset-to-coding-system
                         (intern (downcase encoding))))
                      (tessera-x-elfeed--document-charset type)))
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
      (quit
       (let ((context (tessera-x-elfeed--request-context request)))
         (when (tessera-x-context-pending-p context)
           (with-current-buffer (tessera-x-context-source context)
             (tessera-x-cancel-context))))
       (signal (car err) (cdr err)))
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
  (require 'elfeed-search)
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
  (require 'elfeed-search)
  (let* ((scope
          (cond
           ((derived-mode-p 'elfeed-search-mode) elfeed-search-filter)
           ((derived-mode-p 'elfeed-tree-mode)
            (concat elfeed-tree-filter " "
                    (or (get-text-property
                         (line-beginning-position) 'elfeed-filter)
                        (user-error "No Elfeed filter at point"))))
           (t (user-error "Run in Elfeed Search or Tree"))))
         (filter (byte-compile
                  (elfeed-search-compile-filter
                   (elfeed-search-parse-filter scope))))
         (bounds (tessera-x-today-bounds))
         (start (float-time (car bounds)))
         (end (float-time (cdr bounds)))
         (now (float-time))
         (count 0)
         entries)
    (elfeed-db-ensure)
    ;; Visit newest first; native filters use this tag to stop early.
    (catch 'elfeed-db-done
      (avl-tree-mapc
       (lambda (id)
         (let* ((entry (elfeed-db-get-entry id))
                (date (elfeed-entry-date entry)))
           (when (< date start)
             (throw 'elfeed-db-done nil))
           (when (and (< date end)
                      (funcall
                       filter entry (elfeed-entry-feed entry)
                       count now))
             (cl-incf count)
             (push entry entries))))
       elfeed-db-index))
    (tessera-x-elfeed--build-context
     entries (format "Today, local feed database; filter: %s" scope)
     t)))

(provide 'tessera-x-elfeed)
;;; tessera-x-elfeed.el ends here
