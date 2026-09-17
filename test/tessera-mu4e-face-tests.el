;;; tessera-mu4e-face-tests.el --- Mu4e states -*- lexical-binding: t; -*-

;;; Commentary:

;; Check native state composition and unread color fallback.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'mu4e-headers)
(require 'tessera-mu4e-headers)

(ert-deftest tessera-mu4e-headers-faces-preserve-native-subject-state
    ()
  (dolist (message '((:flags (seen))
                     (:flags (unread flagged replied))
                     (:flags (new seen))
                     (:flags (unread draft flagged))
                     (:flags (unread trashed draft))
                     (:flags (seen flagged))
                     (:flags (seen replied passed))
                     (:flags (seen passed))
                     (:flags (seen) :meta (:related t))))
    (let* ((input (propertize "Subject" 'help-echo "Details"))
           (native (mu4e~headers-apply-flags
                    message (copy-sequence input)))
           (text (tessera-mu4e-headers--styled-text 'subject message
                                                    input))
           (faces (get-text-property 0 'face text)))
      (should (eq (cadr faces) (get-text-property 0 'face native)))
      (should (eq (car faces)
                  (if (or (memq 'unread (plist-get message :flags))
                          (memq 'new (plist-get message :flags)))
                      'tessera-mu4e-headers-unread-subject-face
                    'tessera-mu4e-headers-subject-face)))
      (should-not (get-text-property 0 'face input))
      (should (equal (get-text-property 0 'help-echo text) "Details"))
      ;; Restyling a previously unread subject must remove old faces.
      (let ((restyled (tessera-mu4e-headers--styled-text
                       'subject '(:flags (seen)) text)))
        (should
         (equal (get-text-property 0 'face restyled)
                '(tessera-mu4e-headers-subject-face
                  mu4e-header-face)))))))

(ert-deftest
    tessera-mu4e-headers-faces-isolate-contact-and-date-state ()
  (let* ((attributes '((default :foreground)
                       (mu4e-header-face :foreground)
                       (mu4e-unread-face :foreground :inherit)))
         (saved
          (mapcar
           (lambda (spec)
             (cons (car spec)
                   (cl-loop for attribute in (cdr spec)
                            append
                            (list attribute
                                  (face-attribute
                                   (car spec) attribute)))))
           attributes)))
    (unwind-protect
        (progn
          (set-face-attribute 'default nil :foreground "blue")
          (set-face-attribute 'mu4e-header-face nil :foreground "red")
          (set-face-attribute 'mu4e-unread-face nil
                              :foreground 'unspecified
                              :inherit nil)
          (dolist (flags '((seen draft) (seen trashed) (seen flagged)
                           (unread draft) (unread trashed)
                           (new seen)))
            (dolist (role '(contact date))
              (let* ((message (list :flags flags))
                     (unread (or (memq 'unread flags)
                                 (memq 'new flags)))
                     (text (tessera-mu4e-headers--styled-text
                            role message "Text"))
                     (face (get-text-property 0 'face text)))
                (should (equal (face-attribute face :foreground nil t)
                               (if unread "blue" "red")))
                (should (eq (face-attribute face :weight nil t)
                            (if unread 'bold 'normal)))
                (should (eq (face-attribute face :slant nil t)
                            (if (eq role 'contact)
                                'italic 'normal)))))))
      (dolist (spec saved)
        (apply #'set-face-attribute (car spec) nil (cdr spec))))))

(ert-deftest
    tessera-mu4e-headers-faces-thread-separates-aggregate-state ()
  (let* ((thread (make-tessera-thread-context :unread 1 :total 4))
         (root '(:flags (seen trashed))))
    (should
     (eq (get-text-property
          0 'face (tessera-mu4e-headers--styled-text
                   'subject root "Subject" thread))
         'tessera-mu4e-headers-thread-unread-subject-face))
    (setf (tessera-thread-context-unread thread) 0)
    (let* ((text (tessera-mu4e-headers--styled-text
                  'subject root "Subject" thread))
           (face (get-text-property 0 'face text)))
      (should (eq face 'tessera-mu4e-headers-thread-subject-face))
      (should (eq (face-attribute face :weight nil t) 'bold)))
    (dolist (flags '((seen trashed) (unread draft) (new flagged)
                     (seen replied) (unread passed)))
      (let* ((message (list :flags flags))
             (native (mu4e~headers-apply-flags
                      message (copy-sequence "Contact")))
             (text (tessera-mu4e-headers--styled-text
                    'contact message "Contact" thread))
             (faces (get-text-property 0 'face text))
             (unread (tessera-mu4e-headers--unread-p message)))
        (should (eq (cadr faces) (get-text-property 0 'face native)))
        (should (eq (face-attribute (car faces) :slant nil t)
                    'italic))
        (when unread
          (should (eq (face-attribute (car faces) :weight nil t)
                      'bold)))
        ;; Dates continue to depend on each message, not the thread.
        (should
         (eq (get-text-property
              0 'face (tessera-mu4e-headers--styled-text
                       'date message "Date" thread))
             (if unread 'tessera-mu4e-headers-unread-date-face
               'tessera-mu4e-headers-date-face)))))))

(provide 'tessera-mu4e-face-tests)
;;; tessera-mu4e-face-tests.el ends here
