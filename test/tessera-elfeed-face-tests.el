;;; tessera-elfeed-face-tests.el --- Elfeed face states -*- lexical-binding: t; -*-

;;; Commentary:
;; Check native colors and independent unread emphasis.

;;; Code:

(require 'ert)
(require 'elfeed-search)
(require 'tessera-elfeed-search)

(ert-deftest tessera-elfeed-faces-feed-and-date-follow-unread ()
  (let* ((feed (elfeed-feed--create :id "test-feed" :title "Feed"))
         (elfeed-db '(:version 4))
         (elfeed-db-feeds (make-hash-table :test #'equal))
         (entry (elfeed-entry--create
                 :feed-id "test-feed" :date 0
                 :link "https://example.invalid/entry"))
         (context (make-tessera-entry-context :object entry)))
    (puthash "test-feed" feed elfeed-db-feeds)
    (dolist (tags '(nil (foo) (unread) (unread foo)))
      (setf (elfeed-entry-tags entry) tags)
      (let* ((unread (memq 'unread tags))
             (feed-text (tessera-elfeed-search--feed context))
             (date-text (tessera-elfeed-search--date context))
             (url-text (tessera-elfeed-search--url context))
             (url-face (get-text-property 0 'face url-text))
             (feed-face (get-text-property 0 'face feed-text))
             (date-face (get-text-property 0 'face date-text)))
        (should (eq feed-face
                    (if unread 'tessera-elfeed-search-unread-feed-face
                      'tessera-elfeed-search-feed-face)))
        (should (eq date-face
                    (if unread 'tessera-elfeed-search-unread-date-face
                      'tessera-elfeed-search-date-face)))
        (should (eq url-face
                    (if unread 'tessera-elfeed-search-unread-url-face
                      'tessera-elfeed-search-url-face)))
        (should (eq (face-attribute url-face :slant nil t) 'italic))
        (should-not (face-attribute url-face :underline nil t))
        (should (equal (get-text-property 0 'follow-link url-text)
                       [elfeed-entry]))
        (dolist (face (list feed-face date-face url-face))
          (should (eq (face-attribute face :weight nil t)
                      (if unread 'bold 'normal))))
        (should (eq (face-attribute feed-face :slant nil t) 'italic))
        (should (eq (face-attribute date-face :slant nil t) 'normal))
        (should (eq (get-text-property 0 'mouse-face feed-text)
                    'highlight))
        (should (equal (get-text-property 0 'follow-link date-text)
                       [elfeed-date]))))))

(ert-deftest tessera-elfeed-faces-inherit-native-colors ()
  (let ((faces '(elfeed-search-title-face
                 elfeed-search-unread-title-face
                 elfeed-search-date-face elfeed-search-feed-face))
        saved)
    (dolist (face faces)
      (push (cons face (face-attribute face :foreground)) saved))
    (unwind-protect
        (progn
          (cl-mapc (lambda (face color)
                     (set-face-attribute face nil :foreground color))
                   faces '("red" "blue" "green" "purple"))
          (dolist (spec '((tessera-elfeed-search-date-face . "red")
                          (tessera-elfeed-search-unread-date-face
                           . "blue")
                          (tessera-elfeed-search-url-face . "red")
                          (tessera-elfeed-search-unread-url-face
                           . "blue")
                          (tessera-elfeed-search-feed-face . "purple")
                          (tessera-elfeed-search-unread-feed-face
                           . "purple")))
            (should (equal
                     (face-attribute (car spec) :foreground nil t)
                     (cdr spec))))
          ;; An unread face without a color uses the normal title.
          (set-face-attribute 'elfeed-search-unread-title-face nil
                              :foreground 'unspecified)
          (should (equal
                   (face-attribute
                    'tessera-elfeed-search-unread-date-face
                    :foreground nil t) "red")))
      (dolist (spec saved)
        (set-face-attribute (car spec) nil :foreground (cdr spec))))))

(provide 'tessera-elfeed-face-tests)
;;; tessera-elfeed-face-tests.el ends here
