;;; tessera-gnus-article-tests.el --- MIME observations -*- lexical-binding: t; -*-

;;; Commentary:

;; Inspect native MIME handles without fetching or security actions.

;;; Code:

(require 'ert)
(require 'tessera-gnus-article)
(require 'tessera-gnus-test-support)

(ert-deftest
    tessera-gnus-article-opaque-content-retains-outer-signature ()
  (with-temp-buffer
    (let* ((payload (mm-make-handle
                     (current-buffer) '("application/octet-stream")
                     nil nil '("attachment" (filename . "data.gpg"))))
           (cipher (list "multipart/encrypted" payload))
           (signed (list "multipart/signed" cipher))
           (data (tessera-gnus-article--mime-content signed)))
      (should (eq (plist-get data :attachment) 'unknown))
      (should (eq (plist-get data :signature) 'present))
      (should (eq (plist-get data :encryption) 'present)))))

(ert-deftest tessera-gnus-article-all-content-properties-coexist ()
  (with-temp-buffer
    (let* ((attachment
            (mm-make-handle (current-buffer) '("application/pdf")
                            nil nil '("attachment")))
           (signed (list "multipart/signed" attachment))
           (cipher (list (propertize "multipart/encrypted"
                                     'gnus-info "OK") signed))
           (data (tessera-gnus-article--mime-content cipher)))
      (should (eq (plist-get data :attachment) 'present))
      (should (eq (plist-get data :signature) 'present))
      (should (eq (plist-get data :encryption) 'processed)))))

(ert-deftest
    tessera-gnus-article-does-not-infer-signature-trust-from-text ()
  (let* ((handle (list (propertize "multipart/signed"
                                   'gnus-info "BAD signature")))
         (data (tessera-gnus-article--mime-content handle)))
    (should (eq (plist-get data :signature) 'processed))
    (should (equal (plist-get data :signature-details)
                   '("BAD signature")))))

(ert-deftest tessera-gnus-article-native-error-takes-precedence ()
  (let ((handles
         (list "multipart/mixed"
               (list (propertize "multipart/signed" 'gnus-info "OK"))
               (list (propertize "multipart/signed"
                                 'sec-error t 'gnus-info "Failed")))))
    (should (eq (plist-get (tessera-gnus-article--mime-content
                            handles)
                           :signature) 'error))))

(ert-deftest tessera-gnus-article-reads-native-mime-handles ()
  (let ((mm-verify-option 'never)
        (mm-decrypt-option 'never)
        handles)
    (unwind-protect
        (with-temp-buffer
          (insert
           "MIME-Version: 1.0\n"
           "Content-Type: multipart/mixed; boundary=x\n\n"
           "--x\nContent-Type: text/plain\n\nBody\n"
           "--x\nContent-Type: application/pdf\n"
           "Content-Disposition: attachment; filename=report.pdf\n\n"
           "Fixture PDF bytes\n--x--\n")
          (setq handles (mm-dissect-buffer t))
          (let ((data (tessera-gnus-article--mime-content handles)))
            (should (eq (plist-get data :attachment) 'present))
            (should-not (plist-get data :signature))
            (should-not (plist-get data :encryption))))
      (when handles (mm-destroy-parts handles)))))

(ert-deftest tessera-gnus-article-update-preserves-summary-point ()
  (with-temp-buffer
    (let ((gnus-show-threads t)
          (tessera-gnus-summary--active t)
          (tessera-glyph-style 'ascii))
      (tessera-gnus-summary--register)
      (tessera-tests--gnus-rows)
      (tessera-gnus-summary--sync-buffer)
      (setq-local gnus-current-headers
                  (gnus-data-header (gnus-data-find 1)))
      (let ((summary (current-buffer)))
        (dolist (target '((1 nil) (3 nil) (1 10)))
          (setq tessera-gnus-summary--content-cache nil)
          (gnus-summary-goto-subject (car target))
          (when (cadr target)
            (goto-char (+ (line-beginning-position) (cadr target))))
          (with-temp-buffer
            (gnus-article-mode)
            (setq-local gnus-summary-buffer summary)
            (setq-local gnus-article-mime-handles
                        (mm-make-handle
                         (current-buffer) '("application/pdf")
                         nil nil '("attachment")))
            (tessera-gnus-article--updated))
          (should (= (gnus-summary-article-number) (car target)))
          (if (cadr target)
              (should (= (- (point) (line-beginning-position))
                         (cadr target)))
            (should (= (point) (tessera-entry-point)))))))))

(ert-deftest tessera-gnus-article-updates-outside-summary-narrowing ()
  (with-temp-buffer
    (let ((gnus-show-threads t)
          (tessera-gnus-summary--active t)
          (tessera-glyph-style 'ascii))
      (tessera-gnus-summary--register)
      (tessera-tests--gnus-rows)
      (tessera-gnus-summary--sync-buffer)
      (setq-local gnus-current-headers
                  (gnus-data-header (gnus-data-find 1)))
      (forward-line 2)
      (narrow-to-region (line-beginning-position)
                        (line-beginning-position 2))
      (let ((summary (current-buffer))
            (visible (buffer-string)))
        (with-temp-buffer
          (gnus-article-mode)
          (setq-local gnus-summary-buffer summary)
          (setq-local gnus-article-mime-handles
                      (mm-make-handle
                       (current-buffer) '("application/pdf")
                       nil nil '("attachment")))
          (tessera-gnus-article--updated))
        (should (buffer-narrowed-p))
        (should (equal (buffer-string) visible))
        (should (= (point) (point-min)))
        (should (= (gnus-summary-article-number) 3))
        (save-restriction
          (widen)
          (goto-char (point-min))
          (let* ((entry (get-text-property
                         (point) 'tessera-gnus-summary-entry))
                 (content (plist-get (cdr entry) :content)))
            (should (eq (plist-get content :attachment)
                        'present))))))))

(ert-deftest tessera-gnus-article-coalesces-native-content-events ()
  (with-temp-buffer
    (let ((summary (current-buffer))
          (calls 0)
          (original
           (symbol-function 'tessera-gnus-article--mime-content)))
      (setq-local tessera-gnus-summary--active t)
      (setq-local gnus-current-headers
                  (make-full-mail-header 1 "Test" "Author"))
      (with-temp-buffer
        (gnus-article-mode)
        (setq-local gnus-summary-buffer summary)
        (setq-local gnus-article-mime-handles
                    (list (copy-sequence "multipart/signed")))
        (tessera-gnus-article--track-content t)
        (unwind-protect
            (cl-letf (((symbol-function
                        'tessera-gnus-article--mime-content)
                       (lambda (handles)
                         (cl-incf calls)
                         (funcall original handles))))
              (run-hooks 'gnus-article-prepare-hook)
              (should (= calls 1))
              (dotimes (_ 10) (run-hooks 'post-command-hook))
              (should (= calls 1))
              ;; Metadata changes retain the same native handle.
              (put-text-property 0 1 'gnus-info "Processed"
                                 (car gnus-article-mime-handles))
              (dotimes (_ 10) (run-hooks 'gnus-part-display-hook))
              (should (= calls 1))
              (run-hooks 'post-command-hook)
              (should (= calls 2))
              (should (eq 'processed
                          (with-current-buffer summary
                            (plist-get
                             (tessera-gnus-summary--content-data
                              gnus-current-headers) :signature))))
              (dotimes (_ 10) (run-hooks 'post-command-hook))
              (should (= calls 2))
              (run-hooks 'gnus-part-display-hook))
          (tessera-gnus-article--track-content nil))
        (should-not (memq #'tessera-gnus-article--updated
                          post-command-hook))))))

(provide 'tessera-gnus-article-tests)
;;; tessera-gnus-article-tests.el ends here
