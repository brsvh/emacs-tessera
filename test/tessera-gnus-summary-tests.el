;;; tessera-gnus-summary-tests.el --- Gnus entry tests  -*- lexical-binding: t; -*-

;;; Commentary:

;; Regression tests for native mark identity and Tessera rendering.

;;; Code:

(require 'ert)
(require 'tessera-gnus-summary)
(require 'tessera-gnus-test-support)

(defvar gnus-registry-db)

(tessera-gnus-summary--register)

(defun tessera-gnus-tests--find (start end property value
                                       &optional string)
  "Find PROPERTY equal to VALUE between START and END in STRING."
  (cl-loop for position from start below end
           when (equal (get-text-property position property string)
                       value)
           return position))

(defun tessera-gnus-tests--header ()
  "Return a native article header for tests."
  (make-full-mail-header
   42 "A useful article" "Test Author <author@example.invalid>"
   "Tue, 8 Sep 2026 12:00:00 +0800" "<test@example.invalid>"))

(defun tessera-gnus-tests--metadata (&optional unread)
  "Return article metadata, optionally UNREAD."
  (list :marks
        (string (if unread gnus-unread-mark gnus-read-mark)
                gnus-no-mark gnus-no-mark gnus-no-mark)
        :author "Test Author" :group "nnmaildir+test:inbox"))

(defun tessera-gnus-tests--insert ()
  "Insert two test articles with native identifiers."
  (dotimes (index 2)
    (let ((start (point)))
      (insert (tessera-gnus-summary--render
               (tessera-gnus-tests--header)
               (tessera-gnus-tests--metadata)) "\n")
      (put-text-property start (point) 'gnus-number (+ 42 index))))
  (goto-char (point-min)))

(ert-deftest tessera-gnus-authors-use-names-with-address-fallback ()
  (with-temp-buffer
    (let ((gnus-summary-buffer (current-buffer))
          (gnus-summary-to-prefix "Recipient: ")
          (gnus-ignored-from-addresses "self@example.test"))
      (dolist (spec
               '(("Alex Jr. <alex@example.test>" nil "Alex Jr.")
                 ("<alex@example.test>" nil "alex@example.test")
                 ("Self <self@example.test>"
                  "=?UTF-8?B?5p2O5piO?= <li@example.test>" "李明")
                 ("Self <self@example.test>"
                  "<li@example.test>" "li@example.test")
                 ("Self <self@example.test>" nil "Self")))
        (let* ((header (tessera-gnus-tests--header))
               (from (car spec)))
          (setf (mail-header-from header) from
                (mail-header-extra header)
                (append (when (cadr spec) `((To . ,(cadr spec))))
                        '((Newsgroups . "example.news"))))
          (let* ((metadata
                  (plist-put (tessera-gnus-tests--metadata t) :author
                             (tessera-gnus-summary--author-name
                              header from)))
                 (rendered (tessera-gnus-summary--render
                            header metadata)))
            (should (equal (plist-get metadata :author) (nth 2 spec)))
            (should (string-match-p
                     (regexp-quote (nth 2 spec)) rendered))
            (when (cadr spec)
              (let ((help (tessera-gnus-tests--find
                           0 (length rendered) 'help-echo
                           (concat "From: " from
                                   "\nTo: " (cadr spec)) rendered)))
                (should help))))))
      (should (equal gnus-summary-to-prefix "Recipient: ")))))

(ert-deftest tessera-gnus-prefix-preserves-native-marks ()
  (let* ((tessera-glyph-style 'ascii)
         (tessera-entry-layout 'two-line)
         (metadata (tessera-gnus-tests--metadata t))
         (result (tessera-gnus-summary--render
                  (tessera-gnus-tests--header) metadata)))
    (should (equal (substring-no-properties result 0 4)
                   (plist-get metadata :marks)))
    (should (get-text-property 0 'composition result))
    (should (equal (get-text-property 1 'display result) ""))
    (should-not (string-match-p "\n" result))
    (should (tessera-gnus-tests--find 4 (length result)
                                      'display "\n" result))
    (dolist (placement
             (car (get-text-property
                   0 'tessera--entry-layout result)))
      (should (or (= (car placement) 0)
                  (>= (car placement) 4))))))

(ert-deftest tessera-native-prefix-does-not-mutate-input ()
  (let* ((prefix (propertize "ABCD" 'display ""))
         (tessera-entry-top-padding 0.5)
         (rendered (tessera--entry-content "Title" prefix)))
    (should-not (get-text-property 0 'tessera--entry-layout prefix))
    (should (= (caaar (get-text-property
                       0 'tessera--entry-layout rendered)) 0))))

(ert-deftest tessera-gnus-native-state-channels ()
  (let ((context (make-tessera-entry-context
                  :metadata
                  (list :marks
                        (string gnus-ticked-mark gnus-process-mark
                                gnus-undownloaded-mark
                                gnus-score-over-mark)))))
    (should (eq (tessera-gnus-summary--state 'status context)
                'ticked))
    (should (eq (tessera-gnus-summary--state 'secondary context)
                'processable))
    (should (eq (tessera-gnus-summary--state 'availability context)
                'undownloaded))
    (should (eq (tessera-gnus-summary--state 'score context) 'high))))

(ert-deftest tessera-gnus-mark-update-keeps-native-identity ()
  (with-temp-buffer
    (let ((tessera-glyph-style 'ascii)
          (tessera-entry-layout 'two-line))
      (tessera-gnus-tests--insert)
      (tessera-gnus-summary--sync-buffer)
      ;; Simulate Gnus replacing one native mark in place.
      (let ((mark (buffer-substring (point) (1+ (point)))))
        (aset mark 0 gnus-ticked-mark)
        (delete-char 1)
        (insert mark))
      (tessera-gnus-summary--sync-line)
      (let* ((start (line-beginning-position))
             (entry (get-text-property
                     start 'tessera-gnus-summary-entry)))
        (should (= (aref (plist-get (cdr entry) :marks) 0)
                   gnus-ticked-mark))
        (should (= (get-text-property (+ start 5) 'gnus-number) 42))
        (should (tessera-gnus-tests--find
                 start (line-end-position) 'help-echo "Ticked")))
      (forward-line 1)
      (should (= (get-text-property (point) 'gnus-number) 43)))))

(ert-deftest tessera-gnus-restores-faces-after-native-highlighting ()
  (with-temp-buffer
    (let ((tessera-glyph-style 'ascii)
          (tessera-entry-layout 'two-line)
          (tessera-gnus-summary--active t))
      (tessera-gnus-tests--insert)
      (put-text-property (point-min) (line-end-position)
                         'face 'gnus-summary-normal-read)
      (tessera-gnus-summary--update-line)
      (let ((position
             (tessera-gnus-tests--find (point-min) (line-end-position)
                                       'help-echo
                                       "A useful article")))
        (should (memq 'tessera-gnus-summary-subject-face
                      (get-text-property position 'face)))))))

(ert-deftest tessera-gnus-navigation-selects-subject-in-flat-layouts
    ()
  (dolist (tessera-entry-layout '(single-line two-line))
    (with-temp-buffer
      (let ((gnus-show-threads nil)
            (gnus-summary-buffer (current-buffer))
            (gnus-summary-check-current nil)
            (gnus-summary-goto-unread t)
            (gnus-auto-select-same nil)
            (gnus-auto-center-summary nil)
            (tessera-glyph-style 'ascii)
            opened)
        (tessera-tests--gnus-rows)
        (tessera-gnus-summary--sync-buffer)
        (gnus-summary-goto-subject 1)
        (should (looking-at "Subject 1"))
        (cl-letf (((symbol-function 'gnus-summary-display-article)
                   (lambda (article &optional _all)
                     (push article opened) t)))
          (dolist (step '((gnus-summary-next-unread-article 3)
                          (gnus-summary-prev-unread-article 1)))
            (setq opened nil)
            (funcall (car step))
            (should (= (gnus-summary-article-number) (cadr step)))
            (should (looking-at (format "Subject %d" (cadr step))))
            (should (= (point) (tessera-entry-point)))
            (should (get-text-property (point) 'gnus-position))
            (should (equal opened (list (cadr step))))))
        (let ((tessera-entry-segment-gap 3))
          (tessera-gnus-summary--sync-buffer t)
          (should (looking-at "Subject 1"))
          (should (= (point) (tessera-entry-point))))))))

(ert-deftest tessera-gnus-redraw-preserves-point-offset ()
  (with-temp-buffer
    (let ((tessera-glyph-style 'ascii)
          (tessera-entry-layout 'two-line))
      (tessera-gnus-tests--insert)
      (forward-line 1)
      (forward-char 10)
      (tessera-gnus-summary--sync-buffer t)
      (should (= (- (point) (line-beginning-position)) 10))
      (should (= (get-text-property (point) 'gnus-number) 43)))))

(ert-deftest tessera-gnus-folding-removes-hidden-layout ()
  (with-temp-buffer
    (let ((tessera-glyph-style 'ascii)
          (tessera-entry-layout 'two-line)
          (tessera-entry-top-padding 0.5)
          (tessera-entry-bottom-padding 0.2)
          (tessera-gnus-summary--active t))
      (tessera-gnus-tests--insert)
      (tessera-gnus-summary--prepare)
      (forward-line 1)
      (let ((start (point))
            (overlay (make-overlay (point) (point-max))))
        (overlay-put overlay 'invisible 'gnus-sum)
        (goto-char (point-min))
        (tessera-gnus-summary--post-command)
        (should-not
         (seq-some
          (lambda (item) (overlay-get item 'tessera-entry-overlay))
          (overlays-in start (point-max))))
        (delete-overlay overlay)
        (tessera-gnus-summary--post-command)
        (should
         (seq-some
          (lambda (item) (overlay-get item 'tessera-entry-overlay))
          (overlays-in start (point-max))))))))

(ert-deftest tessera-gnus-layout-update-is-idempotent ()
  (with-temp-buffer
    (let ((tessera-glyph-style 'ascii)
          (tessera-entry-layout 'two-line)
          (tessera-entry-top-padding 0.5)
          (tessera-entry-bottom-padding 0.2))
      (tessera-gnus-tests--insert)
      (tessera-gnus-summary--sync-buffer)
      (let ((count (length (overlays-in (point-min) (point-max)))))
        (dotimes (_ 3) (tessera-gnus-summary--sync-buffer t))
        (should (= count (length (overlays-in
                                  (point-min) (point-max)))))))))

(ert-deftest tessera-gnus-enable-restores-setting-locality ()
  (dolist (local '(nil t))
    (with-temp-buffer
      (let ((gnus-newsgroup-headers nil)
            (gnus-summary-line-format "Native format\n"))
        (when local
          (setq-local gnus-summary-line-format "Local format\n"))
        (let ((original gnus-summary-line-format))
          (tessera-gnus-summary--enable)
          (tessera-gnus-summary--enable)
          (should (equal gnus-summary-line-format "%u&tessera;\n"))
          (tessera-gnus-summary--disable)
          (should (equal gnus-summary-line-format original))
          (should (eq (local-variable-p 'gnus-summary-line-format)
                      local))
          (should-not (memq #'tessera-gnus-summary--post-command
                            post-command-hook)))))))

(defun tessera-gnus-tests--metadata-header (&optional extra)
  "Return a native header with EXTRA fields."
  (let ((header (make-full-mail-header
                 42 "Subject" "Author" "" "<metadata@test>")))
    (setf (mail-header-extra header) extra)
    header))

(ert-deftest tessera-gnus-summary-labels-merge-sources ()
  (let ((gnus-registry-db t))
    (cl-letf (((symbol-function 'gnus-registry-get-id-key)
               (lambda (_id _key) '(Work Later))))
      (should
       (equal
        (tessera-gnus-summary--label-data
         (tessera-gnus-tests--metadata-header
          '((X-GM-LABELS . "(\"Work\" \"Two words\")")
            (Keywords . "Work, release,\n multi line"))))
        '(("Work" "Keywords" "Gmail" "Registry")
          ("Later" "Registry") ("Two words" "Gmail")
          ("release" "Keywords") ("multi line" "Keywords")))))))

(ert-deftest tessera-gnus-summary-unknown-is-not-absent ()
  (let ((header (tessera-gnus-tests--metadata-header))
        (tessera-gnus-summary--content-cache nil))
    (should (eq (plist-get (tessera-gnus-summary--content-data header)
                           :attachment) 'unknown))
    (with-temp-buffer
      (let ((handle (mm-make-handle (current-buffer)
                                    '("text/plain"))))
        (should (tessera-gnus-summary--observe-content header handle))
        (should-not (tessera-gnus-summary--observe-content
                     header handle))
        (should-not (plist-get (tessera-gnus-summary--content-data
                                header)
                               :attachment))))))

(ert-deftest tessera-gnus-summary-header-hints-preserve-unknowns ()
  (let ((data (tessera-gnus-summary--content-data
               (tessera-gnus-tests--metadata-header
                '((Content-Type . "multipart/signed; boundary=x"))))))
    (should (eq (plist-get data :signature) 'present))
    (should (eq (plist-get data :attachment) 'unknown))
    (should (eq (plist-get data :encryption) 'unknown))))

(provide 'tessera-gnus-summary-tests)
;;; tessera-gnus-summary-tests.el ends here
