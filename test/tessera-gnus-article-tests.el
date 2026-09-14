;;; tessera-gnus-article-tests.el --- MIME observations -*- lexical-binding: t; -*-

;;; Commentary:

;; Inspect native MIME handles without fetching or security actions.

;;; Code:

(require 'ert)
(require 'tessera-gnus-article)

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

(provide 'tessera-gnus-article-tests)
;;; tessera-gnus-article-tests.el ends here
