;;; tessera-mu4e-thread-tests.el --- Native threads -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercise native hierarchy, folding, and message updates together.

;;; Code:

(require 'ert)
(require 'mu4e-headers)
(require 'mu4e-thread)
(require 'tessera-mu4e)
(require 'tessera-mu4e-headers)

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
            for meta in '((:level 0 :root t :has-child t)
                          (:level 1 :first-child t :has-child t)
                          (:level 2 :first-child t :last-child t)
                          (:level 1 :last-child t)
                          (:level 0 :root t))
            do (mu4e~headers-insert-header
                (list :docid id :subject (format "Subject %d" id)
                      :from (list (list :name (format "Author %d" id)
                                        :email "a@example.test"))
                      :date '(27000 0) :priority 'high
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
    (should (string-match-p "→ /archive" (buffer-string)))
    (mu4e-mark-at-point 'unmark nil)
    (setq mu4e-search-threads nil)
    (tessera-mu4e-headers--refresh)
    (should (= 0 (hash-table-count tessera-mu4e-headers--threads)))
    (should (string-match-p "Subject 2" (buffer-string)))
    (tessera-mu4e-headers--disable)))

(ert-deftest tessera-mu4e-thread-current-excludes-virtual-subject ()
  (tessera-mu4e-tests--with-thread
    (mu4e~headers-goto-docid 1)
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
      (mu4e~headers-goto-docid 5)
      (should-not (mu4e-headers-next))
      (should-not (mu4e~headers-docid-at-point)))))

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
                   #'tessera-mu4e-headers--moved
                   'mu4e~headers-move))
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
                      ((symbol-function 'string-pixel-width)
                       #'string-width))
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
               (lambda (&rest _) (ert-fail "Scanned all overlays")))
              ((symbol-function 'hl-line-mode)
               (lambda (&rest _) (ert-fail "Retoggled hl-line"))))
      (dotimes (_ 10) (tessera-mu4e-headers--refresh)))
    (mu4e-thread-unfold-all)
    (should tessera-mu4e-headers--dirty)
    (tessera-mu4e-headers--refresh)
    (should-not (tessera-mu4e-thread-folds))))

(provide 'tessera-mu4e-thread-tests)
;;; tessera-mu4e-thread-tests.el ends here
