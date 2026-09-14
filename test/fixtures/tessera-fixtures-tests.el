;;; tessera-fixtures-tests.el --- Native Gnus fixture tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Run the interactive checks in the project application.
;; They prepare fixture messages and leave their summary visible.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'tessera-gnus-data)
(require 'tessera-gnus)
(require 'tessera-gnus-summary)
(require 'tessera-gnus-thread)
(require 'tessera-fixtures)

(defun tessera-fixtures-test-gnus-content ()
  "Check native MIME states, label hover, and repeatable generation."
  (interactive)
  (tessera-gnus-mode 1)
  (let (identity)
    (dotimes (_ 2)
      (tessera-fixtures-open-gnus-content)
      (let ((ids (mapcar #'mail-header-message-id
                         gnus-newsgroup-headers)))
        (when identity (cl-assert (equal identity ids)))
        (setq identity ids))
      (cl-loop
       for (name subject) in
       (tessera-fixtures--gnus-content-scenarios)
       for header = (seq-find
                     (lambda (h)
                       (equal (mail-header-subject h) subject))
                     gnus-newsgroup-headers)
       for data = (tessera-gnus-data-content header)
       for expected =
       (pcase name
         ("unknown" '(unknown unknown unknown))
         ("attachment" '(present nil nil))
         ("signed" '(nil present nil))
         ("encrypted" '(unknown unknown present))
         ("combined" '(present present present))
         ("error" '(nil error nil))
         (_ '(nil nil nil)))
       do
       (cl-assert header)
       (cl-assert
        (equal (mapcar (lambda (key) (plist-get data key))
                       '(:attachment :signature :encryption))
               expected)
        nil "Unexpected MIME state in %s: %S" name data)
       (when (equal name "error")
         (cl-assert
          (string-match-p
           "Unknown sign protocol"
           (car (plist-get data :signature-details)))))
       (when (equal name "labels")
         (cl-assert
          (equal (mapcar #'car (tessera-gnus-data-labels header))
                 '("Work" "Review" "Two words" "release")))
         (gnus-summary-goto-subject (mail-header-number header))
         (let ((end (line-end-position))
               found)
           (while (search-forward "," end t)
             (when (eq (get-text-property (1- (point)) 'mouse-face)
                       'default)
               (setq found t)
               (cl-assert
                (equal (get-text-property (- (point) 2) 'mouse-face)
                       '(tessera-entry-hover-face)))
               (cl-assert
                (equal (get-text-property (point) 'mouse-face)
                       '(tessera-entry-hover-face)))
               (cl-assert
                (eq (car (get-text-property
                          (1- (point)) 'tessera-gnus-summary-face))
                    'default))))
           (cl-assert found)))))
    (list :entries (length identity) :content-scenarios 11
          :native-mime 'passed :labels 'passed
          :independent-hover 'passed :regeneration 'passed)))

(defun tessera-fixtures-test-gnus-threads ()
  "Check native marks, folds, thread selection, and article reading.
Leave the expanded threaded summary visible for inspection."
  (interactive)
  (tessera-gnus-mode 1)
  (tessera-fixtures-open-gnus-marks)
  (with-current-buffer "*Summary nnmaildir+fixtures:level-1-critical*"
    (switch-to-buffer (current-buffer))
    (delete-other-windows)
    (gnus-summary-toggle-threads 1)
    (tessera-gnus-summary--post-command)
    (let* ((summary (current-buffer))
           (gnus-summary-buffer summary)
           (root (tessera-fixtures--gnus-article-number
                  "Planning the package release"))
           (child (car (gnus-summary-article-children root)))
           (total (tessera-thread-context-total
                   (gethash root tessera-gnus-thread--contexts))))
      (gnus-summary-goto-subject child)
      (let ((mark (char-after (line-beginning-position))))
        (unwind-protect
            (progn
              (gnus-summary-mark-article-as-unread gnus-unread-mark)
              (tessera-gnus-summary--post-command)
              (let ((before (tessera-thread-context-unread
                             (gethash
                              root tessera-gnus-thread--contexts))))
                (gnus-summary-mark-article-as-read gnus-read-mark)
                (tessera-gnus-summary--post-command)
                (cl-assert
                 (= (1- before)
                    (tessera-thread-context-unread
                     (gethash root tessera-gnus-thread--contexts))))))
          (if (gnus-read-mark-p mark)
              (gnus-summary-mark-article-as-read mark)
            (gnus-summary-mark-article-as-unread mark))
          (tessera-gnus-summary--post-command)))
      (gnus-summary-goto-subject root)
      (gnus-summary-hide-thread)
      (tessera-gnus-summary--post-command)
      (let ((node (gethash root tessera-gnus-thread--contexts)))
        (cl-assert (tessera-thread-context-last node))
        (cl-assert (= total (tessera-thread-context-total node))))
      (dolist (overlay (overlays-in (point-min) (point-max)))
        (when (overlay-get overlay 'tessera-entry-overlay)
          (cl-assert (not (invisible-p (overlay-start overlay))))))
      (gnus-summary-show-all-threads)
      (tessera-gnus-summary--post-command)
      (gnus-summary-toggle-threads -1)
      (tessera-gnus-summary--post-command)
      (cl-assert (= 0 (hash-table-count
                       tessera-gnus-thread--contexts)))
      (gnus-summary-goto-subject child)
      (cl-assert (cl-loop for p from (line-beginning-position)
                          below (line-end-position)
                          thereis
                          (equal (get-text-property p 'display)
                                 "\n")))
      (gnus-summary-toggle-threads 1)
      (tessera-gnus-summary--post-command)
      (gnus-summary-goto-subject child)
      (cl-assert (not (cl-loop for p from (line-beginning-position)
                               below (line-end-position)
                               thereis
                               (equal (get-text-property p 'display)
                                      "\n"))))
      (call-interactively (key-binding (kbd "RET")))
      (cl-assert (= gnus-current-article child))
      (switch-to-buffer summary)
      (delete-other-windows)
      (gnus-summary-goto-subject root)
      (tessera-gnus-summary--post-command)
      (recenter 0)
      (redisplay t)
      (list :root root :child child :total total
            :mark-count-update 'passed :folding 'passed
            :native-thread-toggle 'passed :native-ret 'passed))))

(defun tessera-fixtures-tests--elfeed-snapshot ()
  "Return sorted fixture entry values for repeatability checks."
  (let (entries)
    (maphash
     (lambda (_ entry)
       (when (string-prefix-p "fixture://"
                              (elfeed-entry-feed-id entry))
         (push (list (elfeed-entry-id entry)
                     (elfeed-entry-title entry)
                     (elfeed-entry-tags entry)
                     (elfeed-entry-date entry)
                     (elfeed-entry-link entry)
                     (elfeed-entry-enclosures entry)) entries)))
     elfeed-db-entries)
    (sort entries (lambda (a b)
                    (string< (prin1-to-string (car a))
                             (prin1-to-string (car b)))))))

(defun tessera-fixtures-test-layout-cases ()
  "Check fixture diversity, stable identity, and mixed thread states.
Prepare isolated fixture data and leave the named Gnus cases visible."
  (interactive)
  (tessera-fixtures-prepare-elfeed)
  (let ((before (tessera-fixtures-tests--elfeed-snapshot))
        (feeds (make-hash-table :test #'equal)))
    (cl-assert (= (length before) tessera-fixtures-entry-count))
    (dolist (row before)
      (let* ((feed (caar row))
             (state (if (memq 'unread (nth 2 row)) 'unread 'read)))
        (puthash feed (cons state (gethash feed feeds)) feeds)))
    (maphash (lambda (_ states)
               (cl-assert (memq 'read states))
               (cl-assert (memq 'unread states))) feeds)
    (cl-loop
     for (name . properties) in tessera-fixtures--layout-cases
     for index from (- tessera-fixtures-entry-count
                       (length tessera-fixtures--layout-cases))
     for id = (cons "fixture://engineering"
                    (format "entry-%03d" (1+ index)))
     for entry = (elfeed-db-get-entry id)
     do
     (cl-assert entry)
     (cl-assert (equal (elfeed-entry-title entry)
                       (plist-get properties :title)))
     (cl-assert (eq (and (memq 'unread (elfeed-entry-tags entry)) t)
                    (plist-get properties :unread)))
     (when (equal name "no-url")
       (cl-assert (null (elfeed-entry-link entry))))
     (when (equal name "untitled")
       (cl-assert (elfeed-entry-link entry)))
     (when (equal name "minimal")
       (cl-assert (null (elfeed-entry-tags entry))))
     (when (plist-get properties :attachments)
       (cl-assert (= 2 (length (elfeed-entry-enclosures entry))))))
    (tessera-fixtures-prepare-elfeed)
    (cl-assert (equal before
                      (tessera-fixtures-tests--elfeed-snapshot))))
  (tessera-gnus-mode 1)
  (tessera-fixtures-open-gnus-marks)
  (let ((ids (mapcar #'mail-header-message-id
                     gnus-newsgroup-headers)))
    (cl-assert (= (length ids)
                  (length (delete-dups (copy-sequence ids)))))
    (dolist (case tessera-fixtures--layout-cases)
      (let* ((id (format "<layout-%s.gnus@fixtures.tessera>"
                         (car case)))
             (header (seq-find
                      (lambda (header)
                        (equal id (mail-header-message-id header)))
                      gnus-newsgroup-headers)))
        (cl-assert header)
        (gnus-summary-goto-subject (mail-header-number header))
        (cl-assert (eq (not (gnus-read-mark-p
                             (char-after (line-beginning-position))))
                       (plist-get (cdr case) :unread)))))
    (let (months)
      (dolist (name '("thread-root" "thread-child"))
        (let* ((id (format "<%s.gnus@fixtures.tessera>" name))
               (header
                (seq-find
                 (lambda (h) (equal id (mail-header-message-id h)))
                 gnus-newsgroup-headers)))
          (cl-assert header)
          (gnus-summary-goto-subject (mail-header-number header))
          (cl-assert
           (eq (not (gnus-read-mark-p
                     (char-after (line-beginning-position))))
               (equal name "thread-child")))
          (push (format-time-string
                 "%Y-%m" (date-to-time (mail-header-date header)) t)
                months)))
      (cl-assert (equal (nreverse months) '("2025-01" "2025-02")))))
  (tessera-fixtures--create-layout-mail 'mu4e)
  (dolist (case tessera-fixtures--layout-cases)
    (let* ((directory (expand-file-name
                       "work/Inbox/" tessera-fixtures--mail-root))
           (file (tessera-fixtures--mail-file
                  directory (format "2600.layout-%s.fixture"
                                    (car case)))))
      (cl-assert (file-regular-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (cl-assert (search-forward
                    (format "<layout-%s.mu4e@fixtures.tessera>"
                            (car case)) nil t)))))
  (gnus-summary-goto-subject
   (tessera-fixtures--gnus-article-number
    "Interface review: unread reference"))
  (recenter 1)
  (list :cases (length tessera-fixtures--layout-cases)
        :elfeed-distribution 'passed :repeatability 'passed
        :gnus-read-root-unread-reply 'passed :cross-month 'passed
        :mail-identities 'passed))

(provide 'tessera-fixtures-tests)
;;; tessera-fixtures-tests.el ends here
