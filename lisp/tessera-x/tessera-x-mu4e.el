;;; tessera-x-mu4e.el --- Experimental Tessera features for mu4e  -*- lexical-binding: t; -*-

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

;; Experimental Tessera features for mu4e.
;; The `x' in the package name stands for experimental.
;; Currently provides context snapshots independent of layout modes.

;;; Code:

(require 'tessera-x)

(defvar mu4e--mark-map)
(defvar mu4e-mu-binary)
(defvar mu4e-mu-home)
(defvar mu4e-search-threads)

(declare-function mu4e-headers-for-each "mu4e-headers" (func))
(declare-function mu4e-message-at-point
                  "mu4e-message" (&optional noerror))
(declare-function mu4e-message-readable-path
                  "mu4e-message" (&optional msg))
(declare-function mu4e-query-items
                  "mu4e-query-items" (&optional type refresh))
(declare-function mu4e~headers-thread-root-p
                  "mu4e-headers" (&optional msg))

(declare-function tessera-mu4e-headers-labels
                  "tessera-mu4e-headers" (message))

(defgroup tessera-x-mu4e nil
  "Experimental Tessera features for mu4e."
  :group 'tessera-x
  :prefix "tessera-x-mu4e-")

;;;; Context options

(defcustom tessera-x-mu4e-subthread-scope 'results
  "Default source of context subthread members.
Results includes folded rows.  Local index can supplement replies
outside the active filter, but cannot include unindexed mail."
  :type '(choice (const results) (const local-index))
  :group 'tessera-x-mu4e)

(defcustom tessera-x-mu4e-today-query-function
  'tessera-x-mu4e--today-query
  "Function returning the base query for today's context.
By default use the Headers query or the Main query item at point.
A custom function may supply an account-specific query."
  :type 'function
  :group 'tessera-x-mu4e)

;;;; Context snapshots

(defun tessera-x-mu4e--contacts (contacts)
  "Format CONTACTS with full names and addresses."
  (mapconcat
   (lambda (contact)
     (let ((name (plist-get contact :name))
           (email (plist-get contact :email)))
       (if (and name (not (string-empty-p name)))
           (format "%s <%s>" name (or email ""))
         (or email ""))))
   contacts ", "))

(defun tessera-x-mu4e--item (message)
  "Snapshot native MESSAGE without using rendered header text."
  (make-tessera-x-item
   :id (or (plist-get message :docid) (plist-get message :path))
   :message-id (car (tessera-x-message-ids
                     (plist-get message :message-id)))
   :references (tessera-x-message-ids (plist-get message :references))
   :subject (plist-get message :subject)
   :date (plist-get message :date)
   :data (list :path (plist-get message :path))
   :metadata
   (append
    (mapcar (lambda (field)
              (cons (capitalize (substring (symbol-name field) 1))
                    (tessera-x-mu4e--contacts
                     (plist-get message field))))
            '(:from :to :cc))
    (list (cons "Maildir" (plist-get message :maildir))
          (cons "Labels"
                (string-join
                 (tessera-mu4e-headers-labels message) ","))
          (cons "Flags" (plist-get message :flags))))))

(defun tessera-x-mu4e--items (&optional selected)
  "Read result messages, or SELECTED messages, including folded rows.
Inspect the full result buffer, preserving its narrowing and point."
  (unless (derived-mode-p 'mu4e-headers-mode)
    (user-error "Run this command in mu4e Headers"))
  (let* ((seen (make-hash-table))
         (marked (and selected mu4e--mark-map
                      (> (hash-table-count mu4e--mark-map) 0)))
         (region (and selected (use-region-p)))
         (begin (and region (region-beginning)))
         (end (and region (region-end)))
         (current (and selected
                       (plist-get (mu4e-message-at-point t) :docid)))
         stack items)
    (save-restriction
      (widen)
      (mu4e-headers-for-each
       (lambda (message)
         (let ((id (plist-get message :docid))
               (level
                (if (and mu4e-search-threads
                         (not (mu4e~headers-thread-root-p message)))
                    (or (plist-get (plist-get message :meta) :level)
                        0)
                  0)))
           (unless (gethash id seen)
             (puthash id t seen)
             (setq stack
                   (tessera-x-native-parent-stack
                    (or (car (tessera-x-message-ids
                              (plist-get message :message-id)))
                        id (plist-get message :path))
                    level stack))
             (when (cond
                    ((not selected) t)
                    (marked (gethash id mu4e--mark-map))
                    (region (and (< (line-beginning-position) end)
                                 (>= (line-end-position) begin)))
                    (t (equal id current)))
               (let ((item (tessera-x-mu4e--item message)))
                 (setf (tessera-x-item-parent item) (cdadr stack))
                 (push item items))))))))
    (nreverse items)))

(defun tessera-x-mu4e--read-body (item)
  "Fill ITEM from its readable local message file."
  (tessera-x-read-message
   item (lambda ()
          (insert-file-contents-literally
           (mu4e-message-readable-path (tessera-x-item-data item))))))

(defun tessera-x-mu4e--finish-context (context items)
  "Read local bodies in ITEMS and publish CONTEXT."
  (when (tessera-x-context-pending-p context)
    (dolist (item items) (tessera-x-mu4e--read-body item))
    (setf (tessera-x-context-items context)
          (tessera-x-group-threads items))
    (tessera-x-context-finish context)))

(defun tessera-x-mu4e--cancel-query (process output errors)
  "Stop PROCESS and release OUTPUT and ERRORS buffers."
  (when (process-live-p process) (delete-process process))
  (dolist (buffer (list output errors))
    (when (buffer-live-p buffer) (kill-buffer buffer))))

(defun tessera-x-mu4e--query-done (process _event)
  "Publish the context attached to finished PROCESS."
  (when (memq (process-status process) '(exit signal))
    (let ((context (process-get process 'context))
          (anchor (process-get process 'anchor)))
      (when (and context (tessera-x-context-pending-p context))
        (condition-case err
            (let ((items nil)
                  (status (process-status process))
                  (code (process-exit-status process)))
              ;; mu exits 2 when the query has no matches.
              (unless (and (eq status 'exit) (memq code '(0 2)))
                (error "mu find failed (%s %d): %s"
                       status code
                       (with-current-buffer
                           (process-get process 'errors)
                         (buffer-string))))
              (with-current-buffer (process-buffer process)
                (goto-char (point-min))
                (skip-chars-forward " \t\r\n")
                (while (not (eobp))
                  (let ((message (read (current-buffer))))
                    (unless (and (listp message)
                                 (stringp (plist-get message :path)))
                      (error "Unexpected mu message record"))
                    (push (tessera-x-mu4e--item message) items))
                  (skip-chars-forward " \t\r\n")))
              (setq items (nreverse items))
              (when anchor
                (let ((seen (make-hash-table :test #'equal)))
                  (setq items
                        (cl-remove-if
                         (lambda (item)
                           (let ((id (tessera-x--identity item)))
                             (prog1 (gethash id seen)
                               (puthash id t seen))))
                         (append (tessera-x-context-items context)
                                 items)))
                  (setq items
                        (tessera-x-subthread
                         items
                         (cl-find (tessera-x-item-message-id anchor)
                                  items :test #'equal
                                  :key
                                  #'tessera-x-item-message-id)))))
              (tessera-x-mu4e--finish-context context items))
          (error (tessera-x-context-fail
                  context (error-message-string err))))))))

(defun tessera-x-mu4e--query (context query &optional anchor)
  "Query the local index for CONTEXT using QUERY.
With ANCHOR, include related messages and keep descendants."
  (let ((output (generate-new-buffer " *Tessera mu output*"))
        (errors (generate-new-buffer " *Tessera mu errors*")))
    (condition-case err
        (let* ((command
                (append
                 (list mu4e-mu-binary "find" "--format=sexp"
                       "--skip-dups" "--sortfield=date")
                 (when mu4e-mu-home
                   (list (concat "--muhome="
                                 (expand-file-name mu4e-mu-home))))
                 (when anchor (list "--include-related"))
                 (list query)))
               (process
                (make-process :name "tessera-mu-context"
                              :command command
                              :buffer output
                              :stderr errors
                              :noquery t
                              :coding 'utf-8-unix
                              :connection-type 'pipe
                              :sentinel #'ignore)))
          (process-put process 'context context)
          (process-put process 'anchor anchor)
          (process-put process 'errors errors)
          (push (apply-partially #'tessera-x-mu4e--cancel-query
                                 process output errors)
                (tessera-x-context-cleanup context))
          (set-process-sentinel process #'tessera-x-mu4e--query-done)
          (tessera-x-mu4e--query-done process ""))
      (error
       (kill-buffer output)
       (kill-buffer errors)
       (tessera-x-context-fail context (error-message-string err)))))
  context)

;;;###autoload
(defun tessera-x-mu4e-prepare-context ()
  "Prepare marked messages, an active region, or the current message.
Use local mail files and preserve native marks and read state."
  (interactive)
  (require 'mu4e-headers)
  (require 'tessera-mu4e-headers)
  (let ((items (tessera-x-mu4e--items t)))
    (unless items (user-error "No mu4e messages selected"))
    (let ((context (tessera-x-context-start
                    'mu4e "Selected messages; local files" items)))
      (tessera-x-mu4e--finish-context context items)
      context)))

;;;###autoload
(defun tessera-x-mu4e-prepare-subthread-context
    (&optional local-index)
  "Prepare the current message and its replies from loaded results.
With prefix LOCAL-INDEX, supplement from the local mu index, ignoring
the current search filter.  This does not fetch mail from a server."
  (interactive "P")
  (require 'mu4e-headers)
  (require 'tessera-mu4e-headers)
  (let* ((items (tessera-x-mu4e--items))
         (id (plist-get (mu4e-message-at-point) :docid))
         (anchor (cl-find id items :key #'tessera-x-item-id))
         (expanded (or local-index
                       (eq tessera-x-mu4e-subthread-scope
                           'local-index))))
    (unless anchor (user-error "No current mu4e message"))
    (let ((context
           (tessera-x-context-start
            'mu4e (if expanded
                      "Subthread; local index, possibly incomplete"
                    "Subthread; current results, including folds")
            nil)))
      (if expanded
          (let ((message-id (tessera-x-item-message-id anchor)))
            (unless message-id
              (tessera-x-context-fail context "Missing Message-ID")
              (user-error "Current message has no Message-ID"))
            (setf (tessera-x-context-items context)
                  (tessera-x-subthread items anchor))
            (tessera-x-mu4e--query
             context (concat "msgid:" (prin1-to-string message-id))
             anchor))
        (tessera-x-mu4e--finish-context
         context (tessera-x-subthread items anchor)))
      context)))

(defun tessera-x-mu4e--today-query ()
  "Read a native Headers query or Main query-item at point."
  (cond
   ((derived-mode-p 'mu4e-headers-mode)
    (if (stringp list-buffers-directory)
        list-buffers-directory
      (user-error "No query associated with this Headers buffer")))
   ((derived-mode-p 'mu4e-main-mode)
    (let ((position (line-beginning-position))
          (end (line-end-position))
          query)
      (while (and (< position end) (not query))
        (let ((help (get-text-property position 'help-echo)))
          (when (and (stringp help)
                     (cl-find help (mu4e-query-items)
                              :key (lambda (item)
                                     (plist-get item :query))
                              :test #'equal))
            (setq query help)))
        (setq position (next-single-property-change
                        position 'help-echo nil end)))
      (or query (user-error "Point is not on a mu4e query item"))))
   (t (user-error "Run in mu4e Headers or Main"))))

;;;###autoload
(defun tessera-x-mu4e-prepare-today-context ()
  "Prepare today's locally indexed messages in the native query scope.
In Headers use its current query; in Main use the query item at point.
`mu4e-mu-home' selects the index.  No mailbox synchronization occurs."
  (interactive)
  (require 'mu4e-message)
  (require 'tessera-mu4e-headers)
  (require 'mu4e-server)
  (require 'mu4e-query-items)
  (let* ((base (funcall tessera-x-mu4e-today-query-function))
         (query (if (string-empty-p base) "date:today..now"
                  (format "(%s) AND (date:today..now)" base)))
         (context
          (tessera-x-context-start
           'mu4e (concat "Today; local index; " query) nil)))
    (tessera-x-mu4e--query context query)))

(provide 'tessera-x-mu4e)
;;; tessera-x-mu4e.el ends here
