;;; tessera-gnus-test-support.el --- Gnus test setup -*- lexical-binding: t; -*-

;;; Commentary:

;; Native summary rows shared by rendering and face regressions.

;;; Code:

(require 'cl-lib)
(require 'tessera-gnus-summary)

(defun tessera-tests--gnus-rows (&optional levels)
  "Insert native article rows with optional LEVELS and mixed marks."
  (setq-local gnus-newsgroup-data nil)
  (cl-loop
   for id from 1
   for level in (or levels '(0 1 2 1 0))
   for mark = (if (memq id '(1 3 5)) gnus-unread-mark gnus-read-mark)
   do
   (let* ((header (make-full-mail-header
                   id (format "Subject %d" id) "Author"
                   "Tue, 8 Sep 2026 12:00:00 +0800"
                   (format "<thread-%d@test.invalid>" id)))
          (start (point))
          (metadata (list :marks
                          (string mark gnus-no-mark
                                  gnus-no-mark gnus-no-mark)
                          :author "Author")))
     (insert (plist-get metadata :marks) "Author\n")
     (add-text-properties
      start (point) (list 'gnus-number id
                          'tessera-gnus-summary-entry
                          (cons header metadata)))
     (push (gnus-data-make id mark (1+ start) header level)
           gnus-newsgroup-data)))
  (setq gnus-newsgroup-data (nreverse gnus-newsgroup-data))
  (goto-char (point-min)))

(provide 'tessera-gnus-test-support)
;;; tessera-gnus-test-support.el ends here
