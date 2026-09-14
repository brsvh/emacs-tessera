;;; tessera-gnus-data-tests.el --- Gnus metadata  -*- lexical-binding: t; -*-

;;; Commentary:

;; Test native label sources and MIME observations without fetching.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'tessera-gnus-data)

(defvar gnus-registry-db)

(defun tessera-data-tests--header (&optional extra)
  "Return a native header with EXTRA fields."
  (let ((header (make-full-mail-header
                 42 "Subject" "Author" "" "<metadata@test>")))
    (setf (mail-header-extra header) extra)
    header))

(ert-deftest tessera-data-labels-merge-sources ()
  (let ((gnus-registry-db t))
    (cl-letf (((symbol-function 'gnus-registry-get-id-key)
               (lambda (_id _key) '(Work Later))))
      (should
       (equal
        (tessera-gnus-data-labels
         (tessera-data-tests--header
          '((X-GM-LABELS . "(\"Work\" \"Two words\")")
            (Keywords . "Work, release,\n multi line"))))
        '(("Work" "Keywords" "Gmail" "Registry")
          ("Later" "Registry") ("Two words" "Gmail")
          ("release" "Keywords") ("multi line" "Keywords")))))))

(ert-deftest tessera-data-unknown-is-not-absent ()
  (let ((header (tessera-data-tests--header))
        (tessera-gnus-data--content-cache nil))
    (should (eq (plist-get (tessera-gnus-data-content header)
                           :attachment) 'unknown))
    (with-temp-buffer
      (let ((handle (mm-make-handle (current-buffer)
                                    '("text/plain"))))
        (should (tessera-gnus-data-observe header handle))
        (should-not (tessera-gnus-data-observe header handle))
        (should-not (plist-get (tessera-gnus-data-content header)
                               :attachment))))))

(ert-deftest tessera-data-opaque-content-retains-outer-signature ()
  (with-temp-buffer
    (let* ((payload (mm-make-handle
                     (current-buffer) '("application/octet-stream")
                     nil nil '("attachment" (filename . "data.gpg"))))
           (cipher (list "multipart/encrypted" payload))
           (signed (list "multipart/signed" cipher))
           (data (tessera-gnus-data--mime-content signed)))
      (should (eq (plist-get data :attachment) 'unknown))
      (should (eq (plist-get data :signature) 'present))
      (should (eq (plist-get data :encryption) 'present)))))

(ert-deftest tessera-data-all-content-properties-coexist ()
  (with-temp-buffer
    (let* ((attachment
            (mm-make-handle (current-buffer) '("application/pdf")
                            nil nil '("attachment")))
           (signed (list "multipart/signed" attachment))
           (cipher (list (propertize "multipart/encrypted"
                                     'gnus-info "OK") signed))
           (data (tessera-gnus-data--mime-content cipher)))
      (should (eq (plist-get data :attachment) 'present))
      (should (eq (plist-get data :signature) 'present))
      (should (eq (plist-get data :encryption) 'processed)))))

(ert-deftest tessera-data-does-not-infer-signature-trust-from-text ()
  (let* ((handle (list (propertize "multipart/signed"
                                   'gnus-info "BAD signature")))
         (data (tessera-gnus-data--mime-content handle)))
    (should (eq (plist-get data :signature) 'processed))
    (should (equal (plist-get data :signature-details)
                   '("BAD signature")))))

(ert-deftest tessera-data-native-error-takes-precedence ()
  (let ((handles
         (list "multipart/mixed"
               (list (propertize "multipart/signed" 'gnus-info "OK"))
               (list (propertize "multipart/signed"
                                 'sec-error t 'gnus-info "Failed")))))
    (should (eq (plist-get (tessera-gnus-data--mime-content handles)
                           :signature) 'error))))

(ert-deftest tessera-data-header-hints-preserve-unknowns ()
  (let ((data (tessera-gnus-data-content
               (tessera-data-tests--header
                '((Content-Type . "multipart/signed; boundary=x"))))))
    (should (eq (plist-get data :signature) 'present))
    (should (eq (plist-get data :attachment) 'unknown))
    (should (eq (plist-get data :encryption) 'unknown))))

(ert-deftest tessera-data-reads-native-mime-handles ()
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
          (let ((data (tessera-gnus-data--mime-content handles)))
            (should (eq (plist-get data :attachment) 'present))
            (should-not (plist-get data :signature))
            (should-not (plist-get data :encryption))))
      (when handles (mm-destroy-parts handles)))))

(provide 'tessera-gnus-data-tests)
;;; tessera-gnus-data-tests.el ends here
