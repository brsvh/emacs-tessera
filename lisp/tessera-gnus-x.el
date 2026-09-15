;;; tessera-gnus-x.el --- Gnus context snapshots  -*- lexical-binding: t; -*-

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

;; Optional Gnus context construction independent of layout modes.

;;; Code:

(require 'tessera-x)
(require 'tessera-gnus)
(require 'tessera-gnus-summary)
(require 'gnus-agent)
(require 'gnus-topic)
(require 'nnheader)

(defun tessera-gnus-x--item (header group)
  "Snapshot HEADER from GROUP, with complete names and labels."
  (make-tessera-x-item
   :id (cons group (mail-header-number header))
   :message-id (car (tessera-x-message-ids
                     (mail-header-message-id header)))
   :references (tessera-x-message-ids (mail-header-references header))
   :subject (mail-header-subject header)
   :date (ignore-errors (date-to-time (mail-header-date header)))
   :metadata
   (append
    (list (cons "From" (mail-header-from header))
          (cons "Newsgroup" group)
          (cons "Labels"
                (mapconcat #'car
                           (tessera-gnus-summary--label-data header)
                           ",")))
    (mapcar (lambda (field)
              (cons field
                    (tessera-gnus-summary--header-field
                     field header)))
            '("To" "Cc")))))

(defun tessera-gnus-x--items (&optional selected)
  "Snapshot Summary records, or SELECTED articles, including folds."
  (unless (derived-mode-p 'gnus-summary-mode)
    (user-error "Run this command in Gnus Summary"))
  (let* ((gnus-summary-buffer (current-buffer))
         (numbers (and selected
                       (save-excursion
                         (gnus-summary-work-articles nil))))
         (wanted (make-hash-table))
         stack items)
    (dolist (number numbers) (puthash number t wanted))
    (dolist (data gnus-newsgroup-data)
      (let ((header (gnus-data-header data)))
        (when (vectorp header)
          (let ((number (mail-header-number header)))
            (setq stack
                  (tessera-x-native-parent-stack
                   (or (car (tessera-x-message-ids
                             (mail-header-message-id header)))
                       (cons gnus-newsgroup-name number))
                   (if gnus-show-threads (gnus-data-level data) 0)
                   stack))
            (when (or (not selected) (gethash number wanted))
              (let ((item (tessera-gnus-x--item
                           header gnus-newsgroup-name)))
                (setf (tessera-x-item-parent item) (cdadr stack))
                (push item items)))))))
    (nreverse items)))

(defun tessera-gnus-x--agent-file (group name)
  "Return Agent file NAME belonging to GROUP."
  (let ((gnus-command-method (gnus-find-method-for-group group)))
    (gnus-agent-article-name name group)))

(defun tessera-gnus-x--available-p (item)
  "Return non-nil when ITEM has a nonempty Agent body."
  (pcase-let* ((`(,group . ,number) (tessera-x-item-id item))
               (file (tessera-gnus-x--agent-file
                      group (number-to-string number))))
    (and (file-readable-p file)
         (> (file-attribute-size (file-attributes file)) 0))))

(defun tessera-gnus-x--refresh-downloads (items)
  "Refresh native availability marks for newly cached ITEMS."
  (when (derived-mode-p 'gnus-summary-mode)
    (let ((numbers
           (sort
            (cl-loop for item in items
                     for id = (tessera-x-item-id item)
                     when (and (equal (car id) gnus-newsgroup-name)
                               (tessera-gnus-x--available-p item))
                     collect (cdr id))
            #'<)))
      (setq gnus-newsgroup-undownloaded
            (gnus-sorted-ndifference
             gnus-newsgroup-undownloaded numbers))
      (save-excursion
        (dolist (number numbers)
          (when (gnus-summary-goto-subject number nil t)
            (gnus-summary-update-download-mark number)))))))

(defun tessera-gnus-x--download (items)
  "Download missing ITEMS into their native Agent groups.
Leave failures as content notes so other bodies remain usable."
  (let ((missing (make-hash-table :test #'equal)))
    (dolist (item items)
      (unless (tessera-gnus-x--available-p item)
        (push item (gethash
                    (car (tessera-x-item-id item)) missing))))
    (maphash
     (lambda (group group-items)
       (condition-case err
           (let ((gnus-command-method
                  (gnus-find-method-for-group group)))
             (unless (gnus-agent-method-p gnus-command-method)
               (error "Gnus method is not agentized"))
             (gnus-agent-fetch-articles
              group (sort (mapcar
                           (lambda (item)
                             (cdr (tessera-x-item-id item)))
                           group-items) #'<)))
         (error
          (dolist (item group-items)
            (setf (tessera-x-item-note item)
                  (concat "Agent download: "
                          (error-message-string err))))))
       (tessera-gnus-x--refresh-downloads group-items))
     missing)))

(defun tessera-gnus-x--read-body (item)
  "Fill ITEM from the Agent, without displaying or marking it read."
  (if (tessera-gnus-x--available-p item)
      (progn
        (setf (tessera-x-item-note item) nil)
        (tessera-x-read-message
         item
         (lambda ()
           (pcase-let ((`(,group . ,number)
                        (tessera-x-item-id item)))
             (let ((gnus-agent-cache t))
               (unless (gnus-agent-request-article number group)
                 (error "Article absent from Agent")))))))
    (unless (tessera-x-item-note item)
      (setf (tessera-x-item-note item)
            "Body absent from local Agent"))))

(defun tessera-gnus-x--build (items scope local-only)
  "Prepare ITEMS for SCOPE, downloading unless LOCAL-ONLY forbids it."
  (unless gnus-agent (user-error "Gnus Agent is not enabled"))
  (let ((context (tessera-x-context-start 'gnus scope items)))
    (condition-case err
        (progn
          (unless (or local-only
                      (eq tessera-gnus-x-body-policy 'local-only))
            (save-window-excursion
              (save-excursion (tessera-gnus-x--download items))))
          (dolist (item items) (tessera-gnus-x--read-body item))
          (setf (tessera-x-context-items context)
                (tessera-x-group-threads items))
          (tessera-x-context-finish context))
      (error (tessera-x-context-fail
              context (error-message-string err))))
    context))

(defun tessera-gnus-x--overview (groups &optional bounds)
  "Read local Agent overview GROUPS within optional BOUNDS.
BOUNDS is a pair of Emacs times.  No articles or headers are fetched."
  (let (items)
    (dolist (group (delete-dups (copy-sequence groups)))
      (when (gnus-agent-method-p (gnus-find-method-for-group group))
        (let ((file (tessera-gnus-x--agent-file group ".overview")))
          (when (file-readable-p file)
            (with-temp-buffer
              (insert-file-contents file)
              (goto-char (point-min))
              (while (not (eobp))
                (let* ((header (save-excursion
                                 (nnheader-parse-nov)))
                       (item (tessera-gnus-x--item header group))
                       (date (tessera-x-item-date item)))
                  (when (and (> (mail-header-number header) 0)
                             (or (not bounds)
                                 (and date
                                      (not (time-less-p
                                            date (car bounds)))
                                      (time-less-p
                                       date (cdr bounds)))))
                    (push item items)))
                (forward-line 1)))))))
    (sort items
          (lambda (left right)
            (time-less-p (or (tessera-x-item-date left) 0)
                         (or (tessera-x-item-date right) 0))))))

;;;###autoload
(defun tessera-gnus-x-prepare-context ()
  "Prepare Gnus work articles using native region and process marks.
When neither is present, use the article at point.  The body policy
controls downloading to the Agent; no article is marked read."
  (interactive)
  (let ((items (tessera-gnus-x--items t)))
    (unless items (user-error "No Gnus articles selected"))
    (tessera-gnus-x--build items "Selected Gnus articles" nil)))

;;;###autoload
(defun tessera-gnus-x-prepare-subthread-context
    (&optional local-index)
  "Prepare the current article and replies from loaded results.
With prefix LOCAL-INDEX, supplement replies from this group's Agent
overview.  That local index may omit articles absent from the Agent."
  (interactive "P")
  (let* ((items (tessera-gnus-x--items))
         (id (save-excursion (gnus-summary-article-number)))
         (anchor
          (cl-find (cons gnus-newsgroup-name id) items
                   :key #'tessera-x-item-id :test #'equal))
         (expanded (or local-index
                       (eq tessera-gnus-x-subthread-scope
                           'local-index))))
    (unless anchor (user-error "No current Gnus article"))
    (when expanded
      (let ((seen (make-hash-table :test #'equal)))
        (dolist (item items)
          (puthash (tessera-x-item-id item) t seen))
        (dolist (item (tessera-gnus-x--overview
                       (list gnus-newsgroup-name)))
          (unless (gethash (tessera-x-item-id item) seen)
            (push item items)))))
    (tessera-gnus-x--build
     (tessera-x-subthread items anchor)
     (if expanded
         "Subthread; group Agent overview, possibly incomplete"
       "Subthread; current results, including folds")
     nil)))

(defun tessera-gnus-x--topic-groups (topic)
  "Return groups in TOPIC and its descendants."
  (cl-labels
      ((find-node (node)
         (if (equal topic (caar node)) node
           (cl-some #'find-node (cdr node))))
       (groups (node)
         (append (copy-sequence
                  (cdr (assoc (caar node) gnus-topic-alist)))
                 (cl-mapcan #'groups (cdr node)))))
    (let ((node (and gnus-topic-topology
                     (find-node gnus-topic-topology))))
      (if node (groups node)
        (copy-sequence (cdr (assoc topic gnus-topic-alist)))))))

;;;###autoload
(defun tessera-gnus-x-prepare-today-context ()
  "Prepare today's local Agent articles in the native scope.
Use the Summary group, or the Group buffer's group or topic at point.
Never download bodies or overview data in this command."
  (interactive)
  (let* ((position (line-beginning-position))
         (groups
          (cond
           ((derived-mode-p 'gnus-summary-mode)
            (list gnus-newsgroup-name))
           ((derived-mode-p 'gnus-group-mode)
            (cond
             ((get-text-property position 'gnus-group)
              (list (get-text-property position 'gnus-group)))
             ((get-text-property position 'gnus-topic)
              (tessera-gnus-x--topic-groups
               (get-text-property position 'gnus-topic)))
             (t (user-error
                 "Point is not on a Gnus group or topic"))))
           (t (user-error "Run in Gnus Summary or Group")))))
    (tessera-gnus-x--build
     (tessera-gnus-x--overview groups (tessera-x-today-bounds))
     (format "Today; local Agent only; groups: %s"
             (string-join groups ", "))
     t)))

(provide 'tessera-gnus-x)
;;; tessera-gnus-x.el ends here
