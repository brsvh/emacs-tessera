;;; tessera-mu4e-thread-tests.el --- Native threads -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercise native hierarchy, folding, and message updates together.

;;; Code:

(require 'ert)
(require 'mu4e-headers)
(require 'mu4e-thread)
(require 'mu4e-view)
(require 'tessera-mu4e)
(require 'tessera-mu4e-headers)
(require 'tessera-test-support)

(defvar tessera-mu4e-tests--thread-metadata
  '((:level 0 :root t :has-child t)
    (:level 1 :first-child t :has-child t)
    (:level 2 :first-child t :last-child t)
    (:level 1 :last-child t)
    (:level 0 :root t))
  "Native hierarchy used by the thread fixture.")

(defmacro tessera-mu4e-tests--with-thread (&rest body)
  "Run BODY with native rows containing branches and mixed states."
  (declare (indent 0) (debug t))
  `(let ((mu4e-search-threads t)
         (mu4e-headers-mode-hook nil)
         (mu4e-headers-fields '((:subject)))
         (mu4e-search-hide-enabled nil)
         (mu4e-thread-fold-unread t)
         (mu4e-thread-fold-single-children t)
         (tessera-glyph-style 'ascii))
     (with-temp-buffer
       (mu4e-headers-mode)
       (let ((inhibit-read-only t)
             (buffer (current-buffer)))
         (cl-letf (((symbol-function 'mu4e-get-headers-buffer)
                    (lambda (&rest _) buffer)))
           (cl-loop
            for id from 1
            for meta in tessera-mu4e-tests--thread-metadata
            do (mu4e~headers-insert-header
                (list :docid id
                      :subject (format "Subject %d" id)
                      :from (list (list :name (format "Author %d" id)
                                        :email "a@example.test"))
                      :date '(27000 0)
                      :priority 'high
                      :flags (if (= id 3) '(unread draft attach)
                               '(seen flagged))
                      :meta meta)
                (point-max)))
           (insert (propertize mu4e~end-of-results
                               'face 'mu4e-system-face))
           (goto-char (point-min))
           (tessera-mu4e--enable-headers)
           (unwind-protect
               (progn ,@body)
             (tessera-mu4e-headers--disable)))))))

(ert-deftest tessera-mu4e-narrowing-keeps-full-buffer-state ()
  (tessera-mu4e-tests--with-thread
    (tessera-mu4e-headers--disable)
    (let ((native
           (buffer-substring-no-properties (point-min) (point-max))))
      (tessera-mu4e--enable-headers)
      (mu4e~headers-goto-docid 2)
      (narrow-to-region (line-beginning-position)
                        (save-excursion (forward-line 1) (point)))
      (setq tessera-mu4e-headers--dirty t)
      (tessera-mu4e-headers--refresh)
      (should (buffer-narrowed-p))
      (should (= 5 (hash-table-count tessera-mu4e-headers--threads)))
      (should (= 1 (tessera-thread-context-parent
                    (gethash 2 tessera-mu4e-headers--threads))))
      (tessera-mu4e-headers--disable)
      (should (buffer-narrowed-p))
      (should (= 2 (mu4e~headers-docid-at-point)))
      (widen)
      (should (equal native (buffer-substring-no-properties
                             (point-min) (point-max))))
      (should-not (text-property-not-all
                   (point-min) (point-max) 'tessera-mu4e-native nil))
      (should-not
       (seq-some (lambda (overlay)
                   (overlay-get overlay 'tessera-entry-overlay))
                 (overlays-in (point-min) (point-max)))))))

(ert-deftest tessera-mu4e-month-click-restores-thread-layout ()
  (tessera-mu4e-tests--with-thread
    (let ((mu4e-headers-open-after-move nil))
      (dotimes (index 5)
        (mu4e~headers-goto-docid (1+ index))
        (let ((message (copy-sequence (mu4e-message-at-point))))
          (setf (plist-get message :date)
                (encode-time 0 0 12 1 (if (< index 4) 9 8) 2026))
          (mu4e~headers-update-handler message nil nil)))
      (tessera-mu4e-headers--refresh)
      (tessera-entry-clear-current)
      (save-window-excursion
        (set-window-buffer (selected-window) (current-buffer))
        (tessera--month-window-change (selected-window))
        (tessera-mu4e-headers--refresh)
        (mu4e~headers-goto-docid 1)
        (tessera-entry-clear-current)
        (cl-labels
            ((snapshot ()
               ;; Include both adjacent thread subjects.  Their
               ;; decoration positions and faces must survive,
               ;; regardless of whether overlays are reused.
               (cl-sort
                (cl-loop
                 for overlay in (overlays-in (point-min) (point-max))
                 when (overlay-get overlay 'tessera-entry-overlay)
                 collect
                 (list (overlay-start overlay) (overlay-end overlay)
                       (overlay-get overlay 'priority)
                       (copy-sequence
                        (overlay-get overlay 'before-string))
                       (copy-sequence
                        (overlay-get overlay 'after-string))))
                #'string< :key #'prin1-to-string)))
          (let ((before (snapshot))
                (transient-mark-mode t))
            (goto-char (tessera-entry-point))
            (tessera-entry-highlight-current)
            (push-mark (point-max) t t)
            (tessera-tests--click-month '(2026 9))
            (should (= (mu4e~headers-docid-at-point) 5))
            (tessera-entry-highlight-current)
            (tessera-tests--click-month '(2026 9))
            (should (= (mu4e~headers-docid-at-point) 5))
            (should (= (point) (tessera-entry-point)))
            (should-not mark-active)
            (tessera-entry-clear-current)
            (should
             (equal-including-properties before (snapshot)))))))))

(ert-deftest tessera-mu4e-isearch-reveals-folded-month ()
  (tessera-mu4e-tests--with-thread
    (dotimes (index 5)
      (mu4e~headers-goto-docid (1+ index))
      (let ((message (copy-sequence (mu4e-message-at-point))))
        (setf (plist-get message :date)
              (encode-time 0 0 12 1 (if (< index 4) 9 8) 2026))
        (mu4e~headers-update-handler message nil nil)))
    (tessera-mu4e-headers--refresh)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (mu4e~headers-goto-docid 1)
      (tessera--month-toggle '(2026 8))
      (should (gethash '(2026 8) tessera--month-folds))
      (let ((search-invisible 'open)
            (isearch-lazy-highlight nil)
            (isearch-lazy-count nil))
        (unwind-protect
            (execute-kbd-macro (kbd "C-s Subject SPC 5 RET"))
          (when isearch-mode (isearch-done))))
      (should (= 5 (mu4e~headers-docid-at-point)))
      (should-not (gethash '(2026 8) tessera--month-folds))
      (should-not (invisible-p (point))))))

(ert-deftest tessera-mu4e-thread-tree-survives-narrow-allocation ()
  (tessera-mu4e-headers--register)
  (let* ((definition (tessera--find-entry-backend 'mu4e-headers))
         (context (make-tessera-entry-context
                   :thread (make-tessera-thread-context
                            :path (make-list 27 t))))
         (layout (tessera--find-entry-layout definition context))
         (tree (tessera--render-segment
                (car (tessera-entry-layout-main-left-segments layout))
                definition context)))
    (tessera--allocate-segment-widths (list tree) nil 8 20)
    (should (equal (tessera--render-segment-group (list tree))
                   (tessera-mu4e-headers--field
                    'thread-tree context)))))

(ert-deftest tessera-mu4e-thread-overflow-keeps-message-and-help ()
  (save-window-excursion
    (tessera-mu4e-tests--with-thread
      (set-window-buffer (selected-window) (current-buffer))
      (mu4e~headers-goto-docid 5)
      (forward-line 1)
      (cl-loop
       for id from 6
       for level in (append (number-sequence 1 27) '(27 26))
       do
       (mu4e~headers-insert-header
        (list :docid id
              :subject (format "Subject %d" id)
              :from (list (list :name (format "Author %d" id)
                                :email "a@example.test"))
              :date '(27000 0)
              :flags '(seen)
              :meta (list :level level
                          :first-child (not (memq id '(33 34)))
                          :last-child (not (memq id '(31 32)))
                          :has-child (< id 32)))
        (point)))
      (let ((width 200))
        (cl-letf (((symbol-function 'window-body-width)
                   (lambda (&rest _) width)))
          (tessera-mu4e-headers--sync nil t)
          (mu4e~headers-goto-docid 32)
          (goto-char (tessera-entry-point))
          (should (looking-at "Author 32"))
          (setq width 50)
          (tessera-mu4e-headers--sync nil t)
          (should (eq (char-after) ?…))
          (should (= 32 (mu4e~headers-docid-at-point)))
          (mu4e~headers-move 1)
          (should (= 33 (mu4e~headers-docid-at-point)))
          (should (eq (char-after) ?…))
          (let* ((position (point))
                 (help (funcall (get-text-property (point) 'help-echo)
                                (selected-window) (current-buffer)
                                (point))))
            (should (string-match-p "Author 33\nSubject 33" help))
            (should (string-match-p "Depth: 27" help))
            (should (string-match-p "Reply to: Author 31 (#31)" help))
            (should (= position (point))))
          (setq width 200)
          (tessera-mu4e-headers--sync nil t)
          (should (= 33 (mu4e~headers-docid-at-point)))
          (should (looking-at "Author 33"))
          (let ((node (gethash 33 tessera-mu4e-headers--threads)))
            (should (= 31 (tessera-thread-context-parent node)))
            (should (= 27 (length (tessera--thread-path-tail node))))
            (should-not (car (tessera--thread-path-tail node)))
            (should (cadr (tessera--thread-path-tail node)))))))))

(ert-deftest tessera-mu4e-thread-native-paths-and-updates ()
  (tessera-mu4e-tests--with-thread
    (let ((head (gethash 1 tessera-mu4e-headers--threads)))
      (should (= 4 (tessera-thread-context-total head)))
      (should (= 1 (tessera-thread-context-unread head)))
      (should (equal '(t nil)
                     (tessera-thread-context-path
                      (gethash 3 tessera-mu4e-headers--threads))))
      (should (equal '(nil)
                     (tessera-thread-context-path
                      (gethash 4 tessera-mu4e-headers--threads)))))
    (should (string-match-p "1/4Subject 1" (buffer-string)))
    (should-not (string-match-p "Subject 2" (buffer-string)))
    (should (= 6 (count-lines (point-min) (point-max))))
    (mu4e~headers-goto-docid 3)
    (goto-char (tessera-entry-point))
    (let ((message (copy-sequence (mu4e-message-at-point))))
      (setq message (plist-put message :flags '(seen draft attach)))
      (mu4e~headers-update-handler message nil nil)
      (tessera-mu4e-headers--refresh)
      (should (= 3 (mu4e~headers-docid-at-point)))
      (should (= (point) (tessera-entry-point)))
      (should (= 0 (tessera-thread-context-unread
                    (gethash 1 tessera-mu4e-headers--threads)))))
    (mu4e~headers-goto-docid 2)
    (mu4e-mark-at-point 'move "/archive")
    (tessera-mu4e-headers--refresh)
    (should (string-match-p "Hm" (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position))))
    (should (string-match-p "> /archive" (buffer-string)))
    (mu4e-mark-at-point 'unmark nil)
    (setq mu4e-search-threads nil)
    (tessera-mu4e-headers--refresh)
    (should (= 0 (hash-table-count tessera-mu4e-headers--threads)))
    (should (string-match-p "Subject 2" (buffer-string)))
    (tessera-mu4e-headers--disable)))

(ert-deftest tessera-mu4e-thread-snapshots-share-current-paths ()
  (let ((tessera-mu4e-tests--thread-metadata
         (cl-loop for level below 40
                  collect (list :level level
                                :root (zerop level)
                                :first-child (> level 0)
                                :last-child (> level 0)
                                :has-child (< level 39)))))
    (tessera-mu4e-tests--with-thread
      (dotimes (index 40)
        (mu4e~headers-goto-docid (1+ index))
        (let ((message (copy-sequence (mu4e-message-at-point))))
          (setf (plist-get message :subject)
                (format "Updated subject %d" index))
          (mu4e~headers-update-handler message nil nil))
        (tessera-mu4e-headers--refresh))
      (let ((cells (make-hash-table :test #'eq)))
        (dotimes (index 40)
          (let ((id (1+ index)))
            (mu4e~headers-goto-docid id)
            (let* ((body (tessera-mu4e-headers--body-start))
                   (snapshot (get-text-property
                              body 'tessera-mu4e-state))
                   (context (get-text-property
                             body 'tessera-entry-context))
                   (node (gethash id tessera-mu4e-headers--threads))
                   (path (tessera-thread-context-reverse-path node)))
              (should (eq node
                          (tessera-entry-context-thread context)))
              (should (eq (nth 4 (nth 2 snapshot)) path))
              (while (and path (not (gethash path cells)))
                (puthash path t cells)
                (setq path (cdr path))))))
        (should (= 39 (hash-table-count cells)))))))

(ert-deftest tessera-mu4e-thread-snapshots-detect-native-mutations ()
  (tessera-mu4e-tests--with-thread
    (mu4e~headers-goto-docid 3)
    (let ((mark (cons 'move "/before")))
      (puthash 3 mark mu4e--mark-map)
      (tessera-mu4e-headers--sync nil)
      (let* ((message (mu4e-message-at-point))
             (snapshot (get-text-property
                        (tessera-mu4e-headers--body-start)
                        'tessera-mu4e-state)))
        (plist-put message :flags '(seen draft attach))
        (setcdr mark "/after")
        (should (memq 'unread (plist-get (car snapshot) :flags)))
        (should (equal (cdr (cadr snapshot)) "/before"))
        (tessera-mu4e-headers--sync nil)
        (setq snapshot (get-text-property
                        (tessera-mu4e-headers--body-start)
                        'tessera-mu4e-state))
        (should-not (memq 'unread (plist-get (car snapshot) :flags)))
        (should (equal (cdr (cadr snapshot)) "/after"))
        (should (string-match-p "> /after" (buffer-string)))
        (should (= 0 (tessera-thread-context-unread
                      (gethash 1 tessera-mu4e-headers--threads))))))))

(ert-deftest tessera-mu4e-thread-current-excludes-virtual-subject ()
  (tessera-mu4e-tests--with-thread
    (tessera-mu4e-headers--disable)
    (mu4e~headers-goto-docid 1)
    (hl-line-highlight)
    (should (overlay-buffer hl-line-overlay))
    (tessera-mu4e--enable-headers)
    ;; Native commands can request highlighting directly as well.
    (hl-line-highlight)
    (should-not (and hl-line-overlay
                     (overlay-buffer hl-line-overlay)))
    (goto-char (tessera-entry-point))
    (tessera-entry-highlight-current)
    (should (memq 'tessera-entry-current-face
                  (get-text-property (point) 'face)))
    (beginning-of-line)
    (search-forward "1/4Subject 1")
    (should-not (memq 'tessera-entry-current-face
                      (ensure-list
                       (get-text-property (1- (point)) 'face))))
    (should (get-text-property (point) 'tessera--thread-heading))
    (mu4e~headers-goto-docid 2)
    (hl-line-highlight)
    (should-not (and hl-line-overlay
                     (overlay-buffer hl-line-overlay)))
    (goto-char (tessera-entry-point))
    (tessera-entry-highlight-current)
    (should (memq 'tessera-entry-current-face
                  (get-text-property (point) 'face)))))

(ert-deftest tessera-mu4e-thread-folding-keeps-native-control ()
  (tessera-mu4e-tests--with-thread
    (mu4e-thread-fold t)
    (let* ((fold (mu4e-thread-is-folded))
           (display (overlay-get fold 'display)))
      (should fold)
      (tessera-mu4e-headers--refresh)
      (should (equal display (overlay-get fold 'display)))
      (should (= 1 (tessera-thread-context-unread
                    (gethash 1 tessera-mu4e-headers--threads))))
      ;; Only the fold summary receives padding in the hidden region.
      (let ((padding
             (seq-filter
              (lambda (overlay)
                (overlay-get overlay 'tessera-entry-overlay))
              (overlays-in (overlay-start fold) (overlay-end fold)))))
        (should (= 1 (length padding)))
        (should (equal (overlay-get (car padding) 'after-string)
                       (tessera--padding-string
                        tessera-thread-outer-bottom-padding))))
      (mu4e~headers-goto-docid 1)
      (should (= 5 (mu4e-headers-next)))
      (should (= (point) (tessera-entry-point)))
      (should (= 1 (mu4e-headers-prev)))
      (should (= (point) (tessera-entry-point)))
      (tessera-mu4e-headers--disable)
      (should (overlay-buffer fold))
      (should (equal display (overlay-get fold 'display)))
      (should-not
       (seq-some (lambda (overlay)
                   (overlay-get overlay 'tessera-entry-overlay))
                 (overlays-in (point-min) (point-max))))
      (tessera-mu4e--enable-headers)
      (mu4e-thread-unfold t)
      (tessera-mu4e-headers--refresh)
      (should-not (tessera-mu4e-thread-folds)))
    ;; Mu4e stops before pending operations even when folding unread.
    (mu4e~headers-goto-docid 3)
    (mu4e-mark-at-point 'move "/archive")
    (mu4e~headers-goto-docid 1)
    (mu4e-thread-fold t)
    (tessera-mu4e-headers--refresh)
    (mu4e~headers-goto-docid 3)
    (should-not (tessera-mu4e-thread-fold-at (point)))
    (mu4e-mark-at-point 'unmark nil)
    (mu4e~headers-goto-docid 1)
    (mu4e-thread-unfold t)
    (let ((mu4e-thread-fold-unread nil))
      (mu4e-thread-fold t)
      (tessera-mu4e-headers--refresh)
      (mu4e~headers-goto-docid 3)
      (should-not (tessera-mu4e-thread-fold-at (point))))
    (tessera-mu4e-headers--disable)))

(ert-deftest tessera-mu4e-thread-orphan-and-missing-parent ()
  (tessera-mu4e-tests--with-thread
    (dolist (spec '((1 :level 2 :orphan t :first-child t)
                    (2 :level 4)
                    (3 :level 2 :orphan t :last-child t)
                    (4 :level 0 :root t)))
      (mu4e~headers-goto-docid (car spec))
      (let ((message (copy-sequence (mu4e-message-at-point))))
        (setq message (plist-put message :meta (cdr spec)))
        (put-text-property (line-beginning-position)
                           (line-end-position) 'msg message)))
    (tessera-mu4e-headers--refresh)
    (let ((head (gethash 1 tessera-mu4e-headers--threads)))
      (should (tessera-thread-context-first head))
      (should (= 3 (tessera-thread-context-total head)))
      (should (= 1 (tessera-thread-context-parent
                    (gethash 2 tessera-mu4e-headers--threads)))))
    (should (tessera-thread-context-first
             (gethash 4 tessera-mu4e-headers--threads)))
    (tessera-mu4e-headers--disable)))

(ert-deftest tessera-mu4e-thread-navigation-selects-contact ()
  (tessera-mu4e-tests--with-thread
    (let ((mu4e-headers-open-after-move nil))
      (should (looking-at "Author 1"))
      (dolist (id '(2 3 4 5))
        (should (= id (call-interactively (key-binding "n"))))
        (should (looking-at (format "Author %d" id))))
      (dolist (id '(4 3 2 1))
        (should (= id (call-interactively (key-binding "p"))))
        (should (looking-at (format "Author %d" id))))
      (should (= 3 (mu4e-headers-next 2)))
      (should (looking-at "Author 3"))
      (should (= 1 (mu4e-headers-prev 2)))
      (mu4e-headers-next-unread)
      (should (looking-at "Author 3"))
      (call-interactively #'mu4e-headers-next-thread)
      (should (looking-at "Author 5"))
      (call-interactively #'mu4e-headers-prev-thread)
      (should (looking-at "Author 1"))
      ;; Native identity lookup still returns the actual line start.
      (should (= (mu4e~headers-goto-docid 2)
                 (line-beginning-position)))
      (call-interactively #'mu4e-thread-goto-root)
      (should (looking-at "Author 1"))
      (forward-char 2)
      (let ((point (point)))
        (call-interactively #'mu4e-thread-goto-root)
        (should (= (point) point))
        (should (= 1 (mu4e~headers-docid-at-point))))
      (mu4e~headers-goto-docid 5)
      (tessera-mu4e-headers--position-point)
      (let ((point (point)))
        (should-not (call-interactively
                     #'mu4e-headers-next-thread))
        (should (= (point) point))
        (should (= 5 (mu4e~headers-docid-at-point)))
        (should-not (mu4e-headers-next))
        (should (= (point) point))
        (should (= 5 (mu4e~headers-docid-at-point)))
        (should (looking-at "Author 5")))
      (let ((point (point))
            (states (copy-tree mu4e-thread--docids))
            (folds
             (seq-count
              (lambda (overlay)
                (overlay-get overlay 'mu4e-thread-folded))
              (overlays-in (point-min) (point-max)))))
        (should-not
         (call-interactively
          #'mu4e-thread-fold-toggle-goto-next))
        (should (= (point) point))
        (should (equal mu4e-thread--docids states))
        (should
         (= folds
            (seq-count
             (lambda (overlay)
               (overlay-get overlay 'mu4e-thread-folded))
             (overlays-in (point-min) (point-max))))))
      (mu4e~headers-goto-docid 1)
      (tessera-mu4e-headers--position-point)
      (let ((point (point)))
        (should-not (call-interactively
                     #'mu4e-headers-prev-thread))
        (should (= (point) point))
        (should (= 1 (mu4e~headers-docid-at-point)))))))

(ert-deftest tessera-mu4e-fold-navigation-reuses-next-thread ()
  (tessera-mu4e-tests--with-thread
    (let ((calls 0)
          (native (symbol-function 'mu4e-thread-next)))
      (cl-letf (((symbol-function 'mu4e-thread-next)
                 (lambda ()
                   (setq calls (1+ calls))
                   (funcall native))))
        (call-interactively
         #'mu4e-thread-fold-toggle-goto-next))
      (should (= calls 1))
      (should (= 5 (mu4e~headers-docid-at-point))))))

(ert-deftest tessera-mu4e-thread-redraw-preserves-contact-or-offset ()
  (tessera-mu4e-tests--with-thread
    (let ((message (copy-sequence (mu4e-message-at-point)))
          (offset (- (point) (line-beginning-position))))
      (setq message (plist-put
                     message :subject "A much longer root subject"))
      (mu4e~headers-update-handler message nil nil)
      (should (looking-at "Author 1"))
      (should (/= offset (- (point) (line-beginning-position))))
      (forward-char 2)
      (setq offset (- (point) (line-beginning-position)))
      (let ((tessera-entry-segment-gap 3))
        (tessera-mu4e-headers--refresh)
        (should (= offset (- (point) (line-beginning-position)))))
      (setq mu4e-search-threads nil)
      (tessera-mu4e-headers--refresh)
      (should (tessera-entry-point))
      (should (= 2 (mu4e-headers-next)))
      (should (looking-at "Subject 2"))
      (tessera-mu4e-headers--disable)
      (should-not (advice-member-p
                   #'tessera-mu4e-headers--move
                   'mu4e~headers-move))
      (should-not (advice-member-p
                   #'tessera-mu4e-headers--move-interactively
                   'mu4e-headers-next-thread))
      (should-not (advice-member-p
                   #'tessera-mu4e-headers--move-to-identity
                   'mu4e-thread-goto-root))
      (should-not
       (advice-member-p
        #'tessera-mu4e-headers--fold-move-to-identity
        'mu4e-thread-fold-toggle-goto-next))
      (should-not (advice-member-p
                   #'tessera-mu4e-headers--view-prev-or-next
                   'mu4e--view-prev-or-next))
      (should-not (text-property-any
                   (point-min) (point-max) 'tessera-entry-point t)))))

(ert-deftest tessera-mu4e-thread-navigation-keeps-preview-and-windows
    ()
  (tessera-mu4e-tests--with-thread
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (let ((other (split-window-right))
            viewed)
        (setq-local mu4e~headers-view-win other)
        (cl-letf (((symbol-function 'mu4e-headers-view-message)
                   (lambda ()
                     (push (mu4e~headers-docid-at-point) viewed))))
          (let ((mu4e-headers-open-after-move t))
            (should (= 2 (mu4e-headers-next)))
            (should (equal viewed '(2)))
            (should (= (point) (tessera-entry-point)))
            (dolist (window (get-buffer-window-list (current-buffer)))
              (should (= (window-point window) (point)))))
          (let ((mu4e-headers-open-after-move nil))
            (should (= 3 (mu4e-headers-next)))
            (should (equal viewed '(2))))
          (delete-window other)
          (let ((mu4e-headers-open-after-move t))
            (should (= 4 (mu4e-headers-next)))
            (should (equal viewed '(2)))))))))

(ert-deftest tessera-mu4e-view-navigation-requires-a-target ()
  (tessera-mu4e-tests--with-thread
    (let (selected viewed)
      (cl-letf (((symbol-function 'mu4e-select-other-view)
                 (lambda () (push t selected)))
                ((symbol-function 'mu4e-headers-view-message)
                 (lambda ()
                   (push t viewed)
                   42)))
        (should-not
         (call-interactively #'mu4e-view-headers-prev-unread))
        (should-not selected)
        (should-not viewed)
        (should (= 1 (mu4e~headers-docid-at-point)))
        (should
         (= 42
            (call-interactively #'mu4e-view-headers-next-unread)))
        (should (= 3 (mu4e~headers-docid-at-point)))
        (should (= (point) (tessera-entry-point)))
        (should (equal selected '(t)))
        (should (equal viewed '(t)))))))

(ert-deftest tessera-mu4e-view-navigation-keeps-inactive-headers ()
  (tessera-mu4e-tests--with-thread
    ;; The outer buffer keeps the shared advice installed.
    (tessera-mu4e-tests--with-thread
      (tessera-mu4e-headers--disable)
      (should
       (advice-member-p
        #'tessera-mu4e-headers--view-prev-or-next
        'mu4e--view-prev-or-next))
      (mu4e~headers-goto-docid 1)
      (let (selected viewed)
        (cl-letf (((symbol-function 'mu4e-select-other-view)
                   (lambda () (push t selected)))
                  ((symbol-function 'mu4e-headers-view-message)
                   (lambda () (push t viewed) 42)))
          (should
           (= 42
              (call-interactively #'mu4e-view-headers-prev-unread)))
          (should (= 1 (mu4e~headers-docid-at-point)))
          (should (equal selected '(t)))
          (should (equal viewed '(t))))))))

(ert-deftest tessera-mu4e-thread-update-from-another-window ()
  (tessera-mu4e-tests--with-thread
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (let ((headers (current-buffer))
            (other (split-window-right)))
        (select-window other)
        (with-temp-buffer
          (switch-to-buffer (current-buffer))
          (with-current-buffer headers
            ;; Exercise graphical measurement in batch tests too.
            (cl-letf (((symbol-function 'display-graphic-p)
                       (lambda (&optional _) t))
                      ((symbol-function 'tessera--string-pixel-width)
                       (lambda (string &optional _buffer)
                         (string-width string))))
              (let ((message (copy-sequence (mu4e-message-at-point))))
                (setq message (plist-put message :flags '(unread)))
                (mu4e~headers-update-handler message nil nil)))
            (should (looking-at "Author 1"))
            (should (= 1 (mu4e~headers-docid-at-point)))
            (should (= (point) (tessera-entry-point)))
            (should (= 6 (count-lines (point-min) (point-max))))
            (dolist (id '(1 2 3 4 5))
              (should (mu4e~headers-goto-docid id))
              (should (= id (plist-get
                             (mu4e-message-at-point) :docid)))
              (should (< (- (line-end-position)
                            (line-beginning-position)) 100)))))))))

(ert-deftest tessera-mu4e-updates-only-affected-rows ()
  (tessera-mu4e-tests--with-thread
    (dolist (mu4e-search-threads '(nil t))
      (dolist (tessera-entry-layout '(single-line two-line))
        (tessera-mu4e-headers--refresh)
        (mu4e~headers-goto-docid 5)
        (let ((overlay (get-text-property
                        (tessera-mu4e-headers--body-start)
                        'tessera--layout-overlay))
              (render (symbol-function 'tessera-entry-render))
              (count 0))
          (mu4e~headers-goto-docid 3)
          (let* ((message (copy-tree (mu4e-message-at-point)))
                 (flags (plist-get message :flags)))
            (setf (plist-get message :flags)
                  (if (memq 'unread flags)
                      '(seen draft attach) '(unread draft attach)))
            (cl-letf (((symbol-function 'tessera-entry-render)
                       (lambda (&rest arguments)
                         (cl-incf count)
                         (apply render arguments))))
              (mu4e~headers-update-handler message nil nil)
              (tessera-mu4e-headers--refresh)))
          (should (= count (if mu4e-search-threads 2 1)))
          (should (overlay-buffer overlay))
          (mu4e~headers-goto-docid 5)
          (should (eq overlay (get-text-property
                               (tessera-mu4e-headers--body-start)
                               'tessera--layout-overlay))))))))

(ert-deftest tessera-mu4e-flags-keep-prefix-capacity ()
  (tessera-mu4e-tests--with-thread
    (mu4e~headers-goto-docid 3)
    (mu4e-mark-at-point 'move "/archive")
    (let ((mu4e-headers-visible-flags
           '(draft trashed flagged replied passed list personal)))
      (dolist (mu4e-search-threads '(nil t))
        (dolist (tessera-entry-layout '(single-line two-line))
          (tessera-mu4e-headers--refresh)
          (dolist (state
                   '(((unread) nil 8 4)
                     ((seen flagged replied) high 8 4)
                     ((seen draft trashed flagged replied passed
                            list personal) high 8 4)
                     ((seen) nil 8 4)))
            (mu4e~headers-goto-docid 3)
            (let ((message (copy-tree (mu4e-message-at-point))))
              (setf (plist-get message :flags) (car state)
                    (plist-get message :priority) (cadr state))
              (mu4e~headers-update-handler message nil nil))
            (tessera-mu4e-headers--refresh)
            (should
             (= (nth (if (and (not mu4e-search-threads)
                              (eq tessera-entry-layout 'two-line))
                         3 2)
                     state)
                tessera-mu4e-headers--leading-width))))))))

(ert-deftest tessera-mu4e-prefix-width-uses-thread-contexts ()
  (tessera-mu4e-tests--with-thread
    (let* ((head (gethash 1 tessera-mu4e-headers--threads))
           (saved-point (point)))
      (setf (tessera-thread-context-total head) 100000
            (tessera-thread-context-unread head) 12345)
      (should (= 12 (tessera-mu4e-headers--measure)))
      (let ((mu4e-search-threads nil))
        (dolist (spec '((single-line . 8) (two-line . 4)))
          (let ((tessera-entry-layout (car spec)))
            (should (= (cdr spec)
                       (tessera-mu4e-headers--measure))))))
      (should (= saved-point (point))))))

(ert-deftest tessera-mu4e-clean-refresh-does-no-global-overlay-scan ()
  (tessera-mu4e-tests--with-thread
    (mu4e-thread-fold t)
    (tessera-mu4e-headers--refresh)
    (cl-letf (((symbol-function 'overlays-in)
               (lambda (&rest _)
                 (ert-fail "Scanned all overlays"))))
      (dotimes (_ 10) (tessera-mu4e-headers--refresh)))
    (mu4e-thread-unfold-all)
    (should tessera-mu4e-headers--dirty)
    (tessera-mu4e-headers--refresh)
    (should-not (tessera-mu4e-thread-folds))))

(provide 'tessera-mu4e-thread-tests)
;;; tessera-mu4e-thread-tests.el ends here
