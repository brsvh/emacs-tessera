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

(provide 'tessera-fixtures-tests)
;;; tessera-fixtures-tests.el ends here
