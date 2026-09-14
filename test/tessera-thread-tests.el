;;; tessera-thread-tests.el --- Thread layout tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercise generic layout selection and native summary snapshots.

;;; Code:

(require 'ert)
(require 'tessera-gnus-summary)
(require 'tessera-gnus-test-support)

(ert-deftest tessera-thread-selects-layout-from-context ()
  (let* ((plain (make-tessera-entry-layout))
         (head (make-tessera-entry-layout))
         (child (make-tessera-entry-layout))
         (definition (tessera--make-entry-backend
                      :layouts (list (cons 'two-line plain))
                      :thread-layout (make-tessera-thread-layout
                                      :head head :child child)))
         (tessera-entry-layout 'two-line)
         (node (make-tessera-thread-context :first t))
         (context (make-tessera-entry-context :thread node)))
    (should (eq head (tessera--find-entry-layout definition context)))
    (setf (tessera-thread-context-first node) nil)
    (should (eq child (tessera--find-entry-layout
                       definition context)))
    (setf (tessera-entry-context-thread context) nil)
    (should (eq plain (tessera--find-entry-layout
                       definition context)))))

(ert-deftest tessera-thread-rejects-mixed-leading-regions ()
  (should-error
   (tessera--validate-layout
    (make-tessera-entry-layout
     :main-glyph-slots '(status) :main-leading-segments '(count))
    '(count) '(status) "Test"))
  (should-error
   (tessera--validate-layout
    (make-tessera-entry-layout :leading-width -1) nil nil "Test")))

(ert-deftest tessera-thread-prefix-preserves-branches-and-bounds-depth
    ()
  (let* ((tessera-glyph-style 'unicode)
         (tessera-entry-segment-gap 1)
         (node (make-tessera-thread-context :path '(t nil t)))
         (context (make-tessera-entry-context :thread node))
         (prefix (tessera-thread-prefix context)))
    (should (string-prefix-p "│" prefix))
    (should (string-match-p "├─" prefix))
    (should (= 8 (string-width prefix)))
    (setf (tessera-thread-context-path node) '(nil))
    (should (string-prefix-p "└─" (tessera-thread-prefix context)))
    (setf (tessera-thread-context-path node) (make-list 50 nil))
    (should (string-prefix-p "…" (tessera-thread-prefix context)))
    (should (< (string-width (tessera-thread-prefix context)) 20))
    (let ((tessera-glyph-style 'ascii))
      (should (string-prefix-p ":" (tessera-thread-prefix context)))))
  (should-not (tessera-thread-prefix (make-tessera-entry-context))))

(ert-deftest tessera-thread-branches-align-with-parent-text ()
  (dolist (tessera-glyph-style '(ascii unicode nerd-icons))
    (dolist (tessera-entry-segment-gap '(0 1 3))
      (dolist (path '((t) (nil) (t nil) (nil t)))
        (let* ((node (make-tessera-thread-context :path path))
               (context (make-tessera-entry-context :thread node))
               (parent (tessera-thread-prefix context))
               (author-column (+ (string-width parent)
                                 tessera-entry-segment-gap)))
          (setf (tessera-thread-context-path node)
                (append path '(nil)))
          (let* ((child (tessera-thread-prefix context))
                 (branch (string-match "[`└]" child)))
            (should branch)
            (should (= author-column
                       (string-width
                        (substring child 0 branch))))))))))

(ert-deftest tessera-thread-native-order-counts-and-paths ()
  (with-temp-buffer
    (let ((gnus-show-threads t))
      (tessera-tests--gnus-rows)
      (tessera-gnus-thread-build)
      (let ((head (gethash 1 tessera-gnus-thread--contexts))
            (child (gethash 2 tessera-gnus-thread--contexts))
            (nested (gethash 3 tessera-gnus-thread--contexts))
            (last (gethash 4 tessera-gnus-thread--contexts)))
        (should (tessera-thread-context-first head))
        (should (= 4 (tessera-thread-context-total head)))
        (should (= 2 (tessera-thread-context-unread head)))
        (should (equal '(t) (tessera-thread-context-path child)))
        (should (equal '(t nil) (tessera-thread-context-path nested)))
        (should (equal '(nil) (tessera-thread-context-path last)))
        (should (tessera-thread-context-last last))
        (should (tessera-thread-context-first
                 (gethash 5 tessera-gnus-thread--contexts)))))))

(ert-deftest tessera-thread-folding-retains-counts-and-moves-padding
    ()
  (with-temp-buffer
    (let ((gnus-show-threads t))
      (tessera-tests--gnus-rows)
      (add-to-invisibility-spec 'gnus-sum)
      (let* ((start (line-end-position))
             (end (save-excursion (forward-line 3)
                                  (line-end-position)))
             (overlay (make-overlay start end)))
        (overlay-put overlay 'invisible 'gnus-sum)
        (tessera-gnus-thread-build)
        (let ((head (gethash 1 tessera-gnus-thread--contexts)))
          (should (tessera-thread-context-last head))
          (should (= 4 (tessera-thread-context-total head)))
          (should (= 2 (tessera-thread-context-unread head))))))))

(ert-deftest tessera-thread-native-switch-and-missing-parent ()
  (with-temp-buffer
    (let ((gnus-show-threads t))
      ;; A missing ancestor may leave a nonzero native starting level.
      (tessera-tests--gnus-rows '(2 3 2))
      (tessera-gnus-thread-build)
      (should (tessera-thread-context-first
               (gethash 1 tessera-gnus-thread--contexts)))
      (should (equal '(nil) (tessera-thread-context-path
                             (gethash
                              2 tessera-gnus-thread--contexts))))
      (should (tessera-thread-context-first
               (gethash 3 tessera-gnus-thread--contexts)))
      (setq gnus-show-threads nil)
      (tessera-gnus-thread-build)
      (should (= 0 (hash-table-count
                    tessera-gnus-thread--contexts))))))

(ert-deftest tessera-thread-mark-update-refreshes-head-count ()
  (with-temp-buffer
    (let ((gnus-show-threads t)
          (tessera-entry-layout 'two-line)
          (tessera-glyph-style 'ascii))
      (tessera-gnus-summary--register)
      (tessera-tests--gnus-rows)
      (tessera-gnus-summary--sync-buffer)
      (should (string-match-p
               "2/4" (buffer-substring-no-properties
                      (point-min) (line-end-position))))
      (forward-line 2)
      (let ((mark (buffer-substring (point) (1+ (point)))))
        (aset mark 0 gnus-read-mark)
        (delete-char 1)
        (insert mark))
      (tessera-gnus-summary--sync-buffer)
      (goto-char (point-min))
      (should (string-match-p
               "1/4" (buffer-substring-no-properties
                      (point) (line-end-position))))
      (should (= 5 (count-lines (point-min) (point-max))))
      (dolist (data gnus-newsgroup-data)
        (should (= (gnus-data-number data)
                   (get-text-property (gnus-data-pos data)
                                      'gnus-number))))
      (forward-line 1)
      (should-not
       (cl-loop for p from (point) below (line-end-position)
                thereis (equal (get-text-property p 'display) "\n")))
      (should-not (search-forward "Subject" (line-end-position) t)))))

(ert-deftest tessera-thread-uses-inner-and-outer-overlay-padding ()
  (with-temp-buffer
    (let ((gnus-show-threads t)
          (tessera-entry-layout 'two-line)
          (tessera-glyph-style 'ascii)
          (tessera-thread-outer-top-padding 0.2)
          (tessera-thread-outer-bottom-padding 0.3)
          (tessera-thread-inner-top-padding 0.05)
          (tessera-thread-inner-bottom-padding 0.07))
      (tessera-gnus-summary--register)
      (tessera-tests--gnus-rows)
      (tessera-gnus-summary--sync-buffer)
      (cl-loop
       for (top bottom) in
       '((0.2 0.07) (0.05 0.07) (0.05 0.07) (0.05 0.3) (0.2 0.3))
       do
       (let* ((layout (get-text-property
                       (point) 'tessera--entry-layout))
              (above (nth 2 (caar layout))))
         (should (= bottom (cadr layout)))
         (should (= top (plist-get
                         (car (get-text-property 0 'face above))
                         :height)))
         (should (seq-some
                  (lambda (o) (overlay-get o 'tessera-entry-overlay))
                  (overlays-at (point)))))
       (forward-line 1)))))

(provide 'tessera-thread-tests)
;;; tessera-thread-tests.el ends here
