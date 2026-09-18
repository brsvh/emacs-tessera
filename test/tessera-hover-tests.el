;;; tessera-hover-tests.el --- Element hover boundaries -*- lexical-binding: t; -*-

;;; Commentary:

;; Check element boundaries after decorative spaces leave buffer text.

;;; Code:

(require 'ert)
(require 'tessera-gnus-summary)
(require 'elfeed-search)
(require 'tessera-elfeed-search)

(defun tessera-hover-tests--elements (text elements)
  "Check that ELEMENTS in TEXT have distinct, uniform hover regions."
  (let ((case-fold-search nil))
    (dolist (element elements)
      (let* ((start (string-match (regexp-quote element) text))
             (face (and start
                        (get-text-property start 'mouse-face text))))
        (should start)
        (should face)
        (should-not (eq face 'default))
        (should-not
         (text-property-not-all start (+ start (length element))
                                'mouse-face face text))
        (should (= (or (next-single-property-change
                        start 'mouse-face text) (length text))
                   (+ start (length element))))))))

(ert-deftest tessera-hover-elfeed-elements-and-tag-separators ()
  (let* ((entry (elfeed-entry--create
                 :id '("feed" . "entry")
                 :feed-id "feed"
                 :title "TITLE"
                 :link "https://entry.invalid"
                 :date 0
                 :tags '(unread foo bar)
                 :enclosures '(("file" "text/plain" 1))))
         (elfeed-db '(:version 4))
         (elfeed-db-feeds (make-hash-table :test #'equal))
         (tessera-glyph-style 'ascii)
         (tessera-glyph-color nil))
    (puthash "feed" (elfeed-feed--create :id "feed" :title "FEED")
             elfeed-db-feeds)
    (tessera-elfeed-search--register)
    (cl-letf (((symbol-function 'elfeed-search-format-date)
               (lambda (_) "DATE")))
      (dolist (tessera-entry-layout '(single-line two-line))
        (let ((text (tessera-entry-render 'elfeed-search entry)))
          (tessera-hover-tests--elements text '("*" "TITLE" "DATE"))
          (when (eq tessera-entry-layout 'two-line)
            (tessera-hover-tests--elements
             text '("TITLE" "FEED" "https://entry.invalid"
                    "@" "foo" "bar" "DATE"))
            (dolist (delimiter '("(" "," ")"))
              (let ((p (string-match (regexp-quote delimiter) text)))
                (should (eq (get-text-property p 'mouse-face text)
                            'default))
                (should (eq (get-text-property p 'face text)
                            'default))))
            (let ((p (string-match "bar" text)))
              (should (eq (get-text-property
                           p 'elfeed-tag text) 'bar))
              (should (equal (get-text-property p 'follow-link text)
                             [elfeed-tag])))))))))

(ert-deftest tessera-hover-gnus-elements-in-all-layouts ()
  (let* ((header (make-full-mail-header
                  1 "SUBJECT" "AUTHOR" "DATE" "<id@test.invalid>"))
         (metadata (list :author "AUTHOR"
                         :marks
                         (string gnus-unread-mark gnus-replied-mark
                                 gnus-downloaded-mark
                                 gnus-score-over-mark)))
         (tessera-glyph-style 'ascii)
         (tessera-glyph-color nil)
         (tessera-entry-segment-gap 0))
    (tessera-gnus-summary--register)
    (cl-letf (((symbol-function 'tessera-gnus-summary-label-data)
               (lambda (_) '(("foo" "test") ("bar" "test"))))
              ((symbol-function 'tessera-gnus-summary--content-data)
               (lambda (_) '( :attachment present
                              :signature present
                              :encryption present)))
              ((symbol-function 'gnus-user-date)
               (lambda (_) "DATE")))
      (dolist (kind '(single-line two-line head child))
        (let* ((tessera-entry-layout
                (if (memq kind '(head child)) 'two-line kind))
               (node (when (memq kind '(head child))
                       (make-tessera-thread-context
                        :first (eq kind 'head)
                        :last t
                        :total 2
                        :unread 1
                        :path (when (eq kind 'child) '(nil)))))
               (text
                (cl-letf (((symbol-function
                            'tessera-gnus-summary--thread-context)
                           (lambda (_) node)))
                  (tessera-gnus-summary--render header metadata))))
          (tessera-hover-tests--elements
           text '("AUTHOR" "foo" "bar" "DATE"))
          (let ((start (string-match "aSE" text)))
            (should start)
            (tessera-hover-tests--elements
             (substring text start (+ start 3)) '("a" "S" "E")))
          (unless (eq kind 'child)
            (tessera-hover-tests--elements
             text '("SUBJECT" "AUTHOR")))
          (when (eq kind 'head)
            (tessera-hover-tests--elements text '("1/2" "SUBJECT")))
          (let ((p (string-match "," text)))
            (should (eq (get-text-property p 'mouse-face text)
                        'default))))))))

(ert-deftest tessera-hover-ellipsis-stays-with-its-element ()
  (dolist (method '(head middle tail))
    (let* ((source (propertize "Long Title" 'mouse-face 'highlight))
           (text (tessera--render-segment-group
                  (list (tessera--make-rendered-segment
                         :string source
                         :target-width 6
                         :truncate method
                         :visible t)))))
      (tessera-hover-tests--elements
       text (list (substring-no-properties text)))
      (should (equal (get-text-property 0 'mouse-face text)
                     '(highlight)))
      (should (eq (get-text-property 0 'mouse-face source)
                  'highlight)))))

(provide 'tessera-hover-tests)
;;; tessera-hover-tests.el ends here
