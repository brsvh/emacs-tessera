;;; tessera-fixtures-tests.el --- Native Gnus fixture tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Run `tessera-fixtures-test-gnus-content' in the project
;; application.
;; These checks read fixture messages and leave their summary visible.

;;; Code:

(require 'cl-lib)
(require 'tessera-gnus-data)
(require 'tessera-gnus)
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
                (eq (get-text-property (- (point) 2) 'mouse-face)
                    'tessera-entry-hover-face))
               (cl-assert
                (eq (get-text-property (point) 'mouse-face)
                    'tessera-entry-hover-face))
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
           (root (cl-loop for node being the hash-values
                          of tessera-gnus-thread--contexts
                          when (and (tessera-thread-context-first
                                     node)
                                    (> (tessera-thread-context-total
                                        node) 3))
                          return (tessera-thread-context-id node)))
           (child (car (gnus-summary-article-children root)))
           (total (tessera-thread-context-total
                   (gethash root tessera-gnus-thread--contexts))))
      (gnus-summary-goto-subject child)
      (let ((mark (char-after)))
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

(provide 'tessera-fixtures-tests)
;;; tessera-fixtures-tests.el ends here
