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
      (tessera-gnus-summary--build-threads)
      (let ((head (gethash 1 tessera-gnus-summary--threads))
            (child (gethash 2 tessera-gnus-summary--threads))
            (nested (gethash 3 tessera-gnus-summary--threads))
            (last (gethash 4 tessera-gnus-summary--threads)))
        (should (tessera-thread-context-first head))
        (should (= 4 (tessera-thread-context-total head)))
        (should (= 2 (tessera-thread-context-unread head)))
        (should (equal '(t) (tessera-thread-context-path child)))
        (should (equal '(t nil) (tessera-thread-context-path nested)))
        (should (equal '(nil) (tessera-thread-context-path last)))
        (should (tessera-thread-context-last last))
        (should (tessera-thread-context-first
                 (gethash 5 tessera-gnus-summary--threads)))))))

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
        (tessera-gnus-summary--build-threads)
        (let ((head (gethash 1 tessera-gnus-summary--threads)))
          (should (tessera-thread-context-last head))
          (should (= 4 (tessera-thread-context-total head)))
          (should (= 2 (tessera-thread-context-unread head))))))))

(ert-deftest tessera-thread-native-switch-and-missing-parent ()
  (with-temp-buffer
    (let ((gnus-show-threads t))
      ;; A missing ancestor may leave a nonzero native starting level.
      (tessera-tests--gnus-rows '(2 3 2))
      (tessera-gnus-summary--build-threads)
      (should (tessera-thread-context-first
               (gethash 1 tessera-gnus-summary--threads)))
      (should (equal '(nil) (tessera-thread-context-path
                             (gethash
                              2 tessera-gnus-summary--threads))))
      (should (tessera-thread-context-first
               (gethash 3 tessera-gnus-summary--threads)))
      (setq gnus-show-threads nil)
      (tessera-gnus-summary--build-threads)
      (should (= 0 (hash-table-count
                    tessera-gnus-summary--threads))))))

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

(ert-deftest tessera-thread-navigation-uses-native-author-position ()
  (with-temp-buffer
    (let ((gnus-show-threads t)
          (gnus-summary-buffer (current-buffer))
          (gnus-summary-check-current nil)
          (gnus-summary-goto-unread t)
          (gnus-auto-select-same nil)
          (gnus-auto-center-summary nil)
          (tessera-glyph-style 'ascii)
          opened)
      (tessera-tests--gnus-rows)
      (tessera-gnus-summary--sync-buffer)
      (should (= (gnus-summary-goto-subject 1) 1))
      (should (looking-at "Author"))
      (should (get-text-property (point) 'gnus-position))
      ;; Retrieval is external; keep native selection and movement.
      (cl-letf (((symbol-function 'gnus-summary-display-article)
                 (lambda (article &optional _all)
                   (push article opened) t)))
        (dolist (step
                 '((gnus-summary-next-unread-article 3 t)
                   (gnus-summary-prev-unread-article 1 t)
                   (gnus-summary-next-article 2 t)
                   (gnus-summary-prev-article 1 t)
                   (gnus-summary-next-unread-subject 3 nil)
                   (gnus-summary-prev-unread-subject 1 nil)))
          (setq opened nil)
          (if (nth 2 step)
              (funcall (car step))
            (funcall (car step) 1))
          (should (= (gnus-summary-article-number) (cadr step)))
          (should (looking-at "Author"))
          (should (= (point) (tessera-entry-point)))
          (should (equal opened
                         (when (nth 2 step) (list (cadr step)))))))
      (should (= (gnus-summary-next-thread 1) 0))
      (should (= (gnus-summary-article-number) 5))
      (should (looking-at "Author"))
      (should (= (gnus-summary-prev-thread 1) 0))
      (should (= (gnus-summary-article-number) 1))
      (should (looking-at "Author")))))

(ert-deftest tessera-thread-navigation-survives-redraw-and-folds ()
  (with-temp-buffer
    (let ((gnus-show-threads t)
          (gnus-summary-buffer (current-buffer))
          (gnus-auto-center-summary nil)
          (tessera-gnus-summary--active t)
          (tessera-glyph-style 'ascii))
      (tessera-tests--gnus-rows)
      (tessera-gnus-summary--prepare)
      (gnus-summary-goto-subject 1)
      (let ((header (gnus-data-header (gnus-data-find 1)))
            (old-offset (- (point) (line-beginning-position))))
        (setf (mail-header-subject header)
              "Changed subject: substantially longer than before")
        (tessera-gnus-summary--sync-buffer t)
        (should (looking-at "Author"))
        (should (/= old-offset
                    (- (point) (line-beginning-position))))
        ;; Manual positions retain their ordinary character offset.
        (goto-char (+ (line-beginning-position) 8))
        (tessera-gnus-summary--sync-buffer t)
        (should (= (- (point) (line-beginning-position)) 8)))
      (gnus-summary-goto-subject 1)
      (add-to-invisibility-spec 'gnus-sum)
      (gnus-summary-hide-thread)
      (tessera-gnus-summary--post-command)
      (should (looking-at "Author"))
      (should-not (invisible-p (point)))
      (should (invisible-p (gnus-data-pos (gnus-data-find 2))))
      (gnus-summary-show-thread)
      (tessera-gnus-summary--post-command)
      (should (looking-at "Author"))
      (should-not (invisible-p (gnus-data-pos (gnus-data-find 2))))
      (beginning-of-line)
      (let ((this-command 'gnus-summary-top-thread))
        (gnus-summary-top-thread)
        (tessera-gnus-summary--post-command))
      (should (looking-at "Author"))
      (setq gnus-show-threads nil)
      (tessera-gnus-summary--sync-buffer t)
      (should (looking-at "Changed subject:"))
      (should (= (point) (tessera-entry-point)))
      (should (get-text-property (point) 'gnus-position)))))

(ert-deftest tessera-thread-position-follows-visible-author-text ()
  (with-temp-buffer
    (let ((gnus-show-threads t)
          (tessera-glyph-style 'unicode))
      (tessera-tests--gnus-rows '(2 3))
      (let* ((entry (get-text-property
                     (point) 'tessera-gnus-summary-entry))
             (header (car entry)))
        (setf (mail-header-subject header) "Author: test subject")
        (dolist (author '("李明 / José Álvarez" "solo@example.test"))
          (setcdr entry (plist-put (cdr entry) :author author))
          (tessera-gnus-summary--build-threads)
          (let* ((text (tessera-gnus-summary--render
                        header (cdr entry)))
                 (position (tessera-entry-point text)))
            (should position)
            (should (eq (aref text position) (aref author 0)))
            (should (get-text-property position 'gnus-position text))
            (should-not (get-text-property
                         (1+ position) 'tessera-entry-point text))
            (should
             (cl-loop for pos below position
                      thereis (equal (get-text-property
                                      pos 'display text)
                                     "\n")))))))))

(ert-deftest tessera-thread-deep-paths-share-ancestors ()
  (let* ((size 2000)
         (nodes
          (tessera-thread-build-contexts
           (cl-loop for id from 1 to size
                    collect (list id (and (> id 1) (1- id)) nil t))))
         (context (make-tessera-entry-context)))
    (cl-loop
     for id from 2 to size
     for node = (gethash id nodes)
     for parent = (gethash (1- id) nodes)
     do
     (should (eq (cdr (tessera-thread-context-reverse-path node))
                 (tessera-thread-context-reverse-path parent)))
     (setf (tessera-entry-context-thread context) node)
     (should (<= (string-width (tessera-thread-prefix context)) 12))
     (tessera-thread-context-key node)
     ;; Rendering must never materialize the full forward path.
     (should-not (tessera-thread-context-forward-path node)))
    (let ((last (gethash size nodes)))
      (should (= (1- size)
                 (length (tessera-thread-context-path last))))
      (setf (tessera-thread-context-path last) '(t nil))
      (should (equal '(t nil) (tessera-thread-context-path last)))
      (setf (tessera-entry-context-thread context) last)
      (should-not
       (string-prefix-p "…" (tessera-thread-prefix context))))))

(provide 'tessera-thread-tests)
;;; tessera-thread-tests.el ends here
