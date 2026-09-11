;;; tessera-gnus-face-tests.el --- Gnus face composition -*- lexical-binding: t; -*-

;;; Commentary:
;; Verify native rules, role composition, and glyph color preferences.

;;; Code:

(require 'ert)
(require 'tessera-gnus-summary)
(require 'tessera-thread-tests)

(ert-deftest tessera-gnus-faces-use-native-rules-and-thresholds ()
  (let ((header (make-full-mail-header 1))
        (marks (string gnus-unread-mark ?\s ?\s ?\s))
        (gnus-summary-default-score 0)
        (gnus-summary-default-high-score 10)
        (gnus-summary-default-low-score -10)
        (gnus-summary-use-undownloaded-faces t)
        (gnus-newsgroup-undownloaded '(1))
        (gnus-newsgroup-cached nil)
        (gnus-newsgroup-scored '((1 . 20))))
    (should (eq (tessera-gnus-summary--native-face header marks)
                'gnus-summary-high-undownloaded))
    (setq gnus-newsgroup-cached '(1))
    (should (eq (tessera-gnus-summary--native-face header marks)
                'gnus-summary-high-unread))
    (setq gnus-newsgroup-scored '((1 . -20)))
    (should (eq (tessera-gnus-summary--native-face header marks)
                'gnus-summary-low-unread))
    (aset marks 0 gnus-ticked-mark)
    (should (eq (tessera-gnus-summary--native-face header marks)
                'gnus-summary-low-ticked))
    (let ((gnus-summary-highlight '((t . warning))))
      (should (eq (tessera-gnus-summary--native-face header marks)
                  'warning)))))

(ert-deftest tessera-gnus-faces-keep-state-on-thread-children ()
  (let* ((header (make-full-mail-header
                  1 "Subject" "Author" "Date" "<id@test>"))
         (context (make-tessera-entry-context
                   :object header
                   :thread (make-tessera-thread-context :first nil)
                   :metadata
                   (list :author "Author" :marks "    "
                         :native-face 'gnus-summary-low-ticked)))
         (text (tessera-gnus-summary--author context))
         (face (get-text-property 0 'face text)))
    (should (eq (car face) 'italic))
    (should (member '(:extend nil) face))
    (should (memq 'gnus-summary-low-ticked face))
    (should (memq 'tessera-gnus-summary-author-face face))))

(ert-deftest tessera-gnus-faces-thread-subject-tracks-all-unread ()
  (let* ((header (make-full-mail-header 1 "Subject" "Author"))
         (node (make-tessera-thread-context :first t :unread 1))
         (context (make-tessera-entry-context
                   :object header :thread node
                   :metadata
                   (list :author "Author" :marks "R   "
                         :native-face 'gnus-summary-high-read))))
    ;; A read root can represent a thread with unread replies.
    (should (eq (get-text-property
                 0 'face (tessera-gnus-summary--subject context))
                'tessera-gnus-summary-thread-unread-subject-face))
    (should (memq 'gnus-summary-high-read
                  (get-text-property
                   0 'face (tessera-gnus-summary--author context))))
    (setf (tessera-thread-context-unread node) 0)
    (should (eq (get-text-property
                 0 'face (tessera-gnus-summary--subject context))
                'tessera-gnus-summary-thread-subject-face))
    (dolist (face '(tessera-gnus-summary-thread-subject-face
                    tessera-gnus-summary-thread-unread-subject-face))
      (should (eq (face-attribute face :weight nil t) 'bold)))))

(ert-deftest tessera-gnus-faces-all-authors-are-italic ()
  (let* ((header (make-full-mail-header 1 "Subject" "Author"))
         (context (make-tessera-entry-context
                   :object header
                   :metadata
                   '(:author "Author" :marks "R   "
                             :native-face (:slant normal)))))
    (dolist (node (list nil
                        (make-tessera-thread-context :first t)
                        (make-tessera-thread-context :first nil)))
      (setf (tessera-entry-context-thread context) node)
      (should (eq (car (get-text-property
                        0 'face
                        (tessera-gnus-summary--author context)))
                  'italic)))))

(ert-deftest tessera-gnus-faces-special-states-and-unread-authors ()
  (let* ((header (make-full-mail-header 1 "Subject" "Author"))
         (marks (string gnus-read-mark ?\s ?\s ?\s))
         (context (make-tessera-entry-context
                   :object header
                   :metadata
                   (list :author "Author" :marks marks
                         :native-face 'gnus-summary-normal-read))))
    ;; Check every status using the same unread rule as thread counts.
    (dolist (spec (cddr (assq 'status tessera-gnus-summary--states)))
      (aset marks 0 (symbol-value (cadr spec)))
      (dolist (node (list nil
                          (make-tessera-thread-context :first t)
                          (make-tessera-thread-context :first nil)))
        (setf (tessera-entry-context-thread context) node)
        (let* ((face (get-text-property
                      0 'face (tessera-gnus-summary--author context)))
               (special
                (pcase (car spec)
                  ('spam 'tessera-gnus-summary-spam-face)
                  ('expirable 'tessera-gnus-summary-expirable-face))))
          (should (eq (car face) 'italic))
          (should (eq (and (memq 'bold face) t)
                      (and node
                           (not (gnus-read-mark-p (aref marks 0))))))
          (when special
            (if node
                (should (memq special face))
              (should-not (memq special face))
              (should (memq special
                            (get-text-property
                             0 'face
                             (tessera-gnus-summary--subject
                              context)))))
            (should
             (eq (tessera-gnus-summary--glyph-face
                  'status (car spec) nil) special))))))))

(ert-deftest tessera-gnus-faces-dates-follow-article-unread-state ()
  (let* ((header (make-full-mail-header
                  1 "Subject" "Author" "17 Feb 2025 00:00:00 +0000"))
         (marks (string gnus-read-mark ?\s ?\s ?\s))
         (context (make-tessera-entry-context
                   :object header :metadata (list :marks marks))))
    (dolist (spec (cddr (assq 'status tessera-gnus-summary--states)))
      (aset marks 0 (symbol-value (cadr spec)))
      (dolist (node (list nil
                          (make-tessera-thread-context
                           :first t :unread 1)
                          (make-tessera-thread-context
                           :first nil :unread 0)))
        (setf (tessera-entry-context-thread context) node)
        (let* ((read (gnus-read-mark-p (aref marks 0)))
               (face (get-text-property
                      0 'face (tessera-gnus-summary--date context))))
          (should (eq face
                      (if read 'tessera-gnus-summary-date-face
                        'tessera-gnus-summary-unread-date-face)))
          (should (equal (face-attribute face :inherit)
                         (if read 'gnus-summary-normal-read
                           '(bold gnus-summary-normal-unread))))
          (should (eq (face-attribute face :weight nil t)
                      (if read 'normal 'bold))))))))

(ert-deftest tessera-gnus-faces-theme-inheritance-stays-live ()
  (let ((old (face-attribute 'gnus-header-from :foreground)))
    (unwind-protect
        (progn
          (set-face-attribute 'gnus-header-from nil :foreground "red")
          (should (equal
                   (face-attribute 'tessera-gnus-summary-author-face
                                   :foreground nil t) "red"))
          (set-face-attribute 'gnus-header-from nil
                              :foreground "blue")
          (should (equal
                   (face-attribute 'tessera-gnus-summary-author-face
                                   :foreground nil t) "blue")))
      (set-face-attribute 'gnus-header-from nil :foreground old))))

(ert-deftest tessera-glyph-role-face-respects-color-preferences ()
  (let ((glyph (make-tessera-glyph
                :ascii "!" :unicode "!" :semantic 'negative
                :nerd-icons '(:function ignore :name "test")))
        (context (make-tessera-entry-context))
        (tessera-glyph-style 'ascii))
    (dolist (tessera-glyph-color '(t nil "blue"))
      (let* ((text (tessera-glyph-render
                    glyph context '(:face warning)))
             (face (get-text-property 0 'face text)))
        (pcase tessera-glyph-color
          ('t (should (memq 'warning (ensure-list face))))
          ('nil (should-not face))
          (_ (should (equal face '(:foreground "blue")))))))
    (should-error
     (tessera-glyph-render glyph context '(:face missing-face)))))

(ert-deftest tessera-gnus-faces-score-changes-refresh-without-marks ()
  (with-temp-buffer
    (let ((gnus-show-threads t)
          (tessera-entry-layout 'two-line)
          (tessera-glyph-style 'ascii)
          (gnus-newsgroup-scored nil)
          (gnus-summary-default-score 0)
          (gnus-summary-default-high-score 10))
      (tessera-gnus-summary--register)
      (tessera-thread-tests--rows)
      (tessera-gnus-summary--sync-buffer)
      (setq gnus-newsgroup-scored '((1 . 20)))
      (tessera-gnus-summary--sync-buffer)
      (should
       (eq (plist-get (cdr (get-text-property
                            (point) 'tessera-gnus-summary-entry))
                      :native-face)
           'gnus-summary-high-unread)))))

(provide 'tessera-gnus-face-tests)
;;; tessera-gnus-face-tests.el ends here
