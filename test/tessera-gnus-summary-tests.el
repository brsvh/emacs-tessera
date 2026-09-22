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
        :author "Test Author"
        :group "nnmaildir+test:inbox"))

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
                  "=?UTF-8?B?5p2O5piO?= <li@example.test>" "Self")
                 ("Self <self@example.test>"
                  "<li@example.test>" "Self")
                 ("<self@example.test>"
                  "<li@example.test>" "self@example.test")
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
                 (rendered
                  (tessera-gnus-summary--render header metadata)))
            (should (equal (plist-get metadata :author) (nth 2 spec)))
            (should (string-match-p
                     (regexp-quote (nth 2 spec)) rendered))
            (when (cadr spec)
              (let ((help (tessera-gnus-tests--find
                           0 (length rendered) 'help-echo
                           (concat "From: " from
                                   "\nTo: " (cadr spec)) rendered)))
                (should help))))))
      (should (equal gnus-summary-to-prefix "Recipient: "))
      (should (equal gnus-ignored-from-addresses
                     "self@example.test")))))

(ert-deftest tessera-gnus-prefix-preserves-native-marks ()
  (let* ((tessera-glyph-style 'ascii)
         (tessera-entry-layout 'two-line)
         (metadata (tessera-gnus-tests--metadata t))
         (result (tessera-gnus-summary--render
                  (tessera-gnus-tests--header) metadata)))
    (should (equal (substring-no-properties result 0 4)
                   (plist-get metadata :marks)))
    (should (equal (get-text-property 0 'display result)
                   '(space :width 0)))
    (should (equal (get-text-property 0 'mouse-face result)
                   '(:inherit nil)))
    (should-not (get-text-property 0 'composition result))
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

(ert-deftest tessera-gnus-custom-marks-register-all-native-slots ()
  (dolist (spec tessera-gnus-summary--states)
    (dolist (entry (cddr spec))
      (dolist (character '(?~ ?•))
        (let* ((mark (cadr entry))
               (tessera--entry-backends (make-hash-table :test #'eq))
               (marks (apply #'string
                             (cl-loop for index below 4
                                      collect (if (eq index
                                                      (cadr spec))
                                                  character
                                                gnus-no-mark))))
               (context (make-tessera-entry-context
                         :metadata (list :marks marks))))
          (cl-progv (list mark) (list character)
            (tessera-gnus-summary--register)
            (let* ((backend
                    (gethash 'gnus-summary tessera--entry-backends))
                   (slot (seq-find
                          (lambda (slot)
                            (eq (tessera-glyph-slot-name slot)
                                (car spec)))
                          (tessera--entry-backend-glyph-slots
                           backend)))
                   (variant (assq (car entry)
                                  (tessera-glyph-slot-glyphs slot)))
                   (glyph (plist-get (cdr variant) :glyph))
                   (expected (if (= character ?~) ?~
                               (eval (car (get mark 'standard-value))
                                     t)))
                   (tessera-glyph-style 'ascii))
              (should (eq (funcall (tessera-glyph-slot-selector slot)
                                   context)
                          (car entry)))
              (should (equal (tessera-glyph-render glyph context)
                             (char-to-string expected)))
              (should (= (symbol-value mark) character)))))))))

(ert-deftest tessera-gnus-custom-marks-preserve-rendering-and-state ()
  (let ((gnus-unread-mark ?•)
        (gnus-read-mark ?○)
        (gnus-process-mark ?◆)
        (gnus-downloaded-mark ?✓)
        (gnus-score-over-mark ?↑)
        (tessera--entry-backends (make-hash-table :test #'eq)))
    (tessera-gnus-summary--register)
    (cl-letf (((symbol-function 'display-graphic-p)
               (lambda (&optional _display) t))
              ((symbol-function 'char-displayable-p)
               (lambda (_character) t))
              ((symbol-function 'tessera--nerd-icons-available-p)
               (lambda () t))
              ((symbol-function 'nerd-icons-mdicon)
               (lambda (name)
                 (if (equal name "nf-md-email") "N" "I"))))
      (dolist (style '(ascii unicode nerd-icons))
        (let* ((tessera-glyph-style style)
               (metadata (tessera-gnus-tests--metadata t))
               (marks (string gnus-unread-mark gnus-process-mark
                              gnus-downloaded-mark
                              gnus-score-over-mark))
               (context (make-tessera-entry-context
                         :metadata (list :marks marks)))
               (expected (pcase style
                           ('ascii " ") ('unicode "●") (_ "N"))))
          (setq metadata (plist-put metadata :marks marks))
          (let* ((text (tessera-gnus-summary--render
                        (tessera-gnus-tests--header) metadata))
                 (position (tessera-gnus-tests--find
                            4 (length text) 'help-echo "Unread"
                            text)))
            (should position)
            (should (equal (substring text position (1+ position))
                           expected))
            (should (equal (substring-no-properties text 0 4)
                           marks)))
          (should (tessera-gnus-summary--unread-p context))
          (should (eq (tessera-gnus-summary--state 'secondary context)
                      'processable))
          (setf (tessera-entry-context-metadata context)
                (tessera-gnus-tests--metadata))
          (should-not (tessera-gnus-summary--unread-p context)))))))

(ert-deftest tessera-gnus-custom-marks-restore-existing-and-new
    ()
  (dolist (character '(?~ ?•))
    (let ((gnus-unread-mark character)
          (gnus-summary-mode-hook nil)
          (gnus-summary-line-format "Native format\n")
          (tessera-gnus-mode nil)
          (tessera--entry-backends (make-hash-table :test #'eq))
          buffers saved native-local)
      (unwind-protect
          (progn
            (dotimes (index 2)
              (let ((buffer (generate-new-buffer " *Gnus existing*")))
                (push buffer buffers)
                (with-current-buffer buffer
                  (gnus-summary-mode)
                  (when (= index 0)
                    (setq native-local
                          (local-variable-p
                           'gnus-summary-line-format)))
                  (when (= index 1)
                    (setq-local gnus-summary-line-format "Local\n"))
                  (push (list buffer gnus-summary-line-format
                              (local-variable-p
                               'gnus-summary-line-format))
                        saved))))
            (dotimes (cycle 2)
              (tessera-gnus-mode 1)
              ;; Exercise an explicit repeated enable once; the next
              ;; cycle exercises activation after full restoration.
              (when (zerop cycle)
                (tessera-gnus-mode 1))
              (let ((buffer (generate-new-buffer " *Gnus future*")))
                (push buffer buffers)
                (with-current-buffer buffer (gnus-summary-mode)))
              (dolist (buffer buffers)
                (with-current-buffer buffer
                  (should tessera-gnus-summary--active)
                  (should (equal gnus-summary-line-format
                                 "%u&tessera;\n"))))
              (tessera-gnus-mode -1)
              (when (zerop cycle)
                (tessera-gnus-mode -1))
              (dolist (buffer buffers)
                (with-current-buffer buffer
                  (should-not tessera-gnus-summary--active)
                  (should-not tessera-gnus-summary--saved-settings)
                  (unless (assq buffer saved)
                    (should (equal gnus-summary-line-format
                                   "Native format\n"))
                    (should
                     (eq (local-variable-p 'gnus-summary-line-format)
                         native-local)))))
              (dolist (item saved)
                (with-current-buffer (car item)
                  (should (equal gnus-summary-line-format
                                 (nth 1 item)))
                  (should (eq (local-variable-p
                               'gnus-summary-line-format)
                              (nth 2 item)))))
              (should (= gnus-unread-mark character))))
        (tessera-gnus-mode -1)
        (mapc #'kill-buffer buffers)))))

(ert-deftest tessera-gnus-current-highlights-entry ()
  (with-temp-buffer
    (let ((gnus-show-threads t)
          (gnus-newsgroup-headers nil)
          (tessera-glyph-style 'ascii))
      (tessera-gnus-summary--register)
      (tessera-tests--gnus-rows)
      (unwind-protect
          (progn
            (tessera-gnus-summary--enable)
            (tessera-gnus-summary--prepare)
            (goto-char (tessera-entry-point))
            (run-hooks 'post-command-hook)
            (should (memq 'tessera-entry-current-face
                          (get-char-property (point) 'face)))
            (save-excursion
              (beginning-of-line)
              (search-forward "Subject 1")
              (should
               (eq
                'tessera-gnus-summary-thread-unread-subject-face
                (get-char-property (1- (point)) 'face)))))
        (tessera-gnus-summary--disable)))))

(ert-deftest tessera-gnus-major-mode-change-cleans-layout ()
  (with-temp-buffer
    (let ((gnus-show-threads t)
          (gnus-newsgroup-headers nil)
          (tessera-glyph-style 'ascii))
      (setq-local major-mode 'gnus-summary-mode)
      (tessera-gnus-summary--register)
      (tessera-tests--gnus-rows)
      (tessera-gnus-summary--enable)
      (tessera-gnus-summary--prepare)
      (let ((overlays (overlays-in (point-min) (point-max)))
            (markers (seq-take tessera--current-entry 2)))
        (should overlays)
        (should markers)
        (fundamental-mode)
        (should-not (seq-some #'overlay-buffer overlays))
        (should-not (seq-some #'marker-buffer markers))))))

(ert-deftest tessera-gnus-custom-mark-rejects-invalid-default ()
  (let ((gnus-unread-mark ?•)
        (tessera--entry-backends (make-hash-table :test #'eq)))
    (dolist (default '(nil (?•) ("x")))
      (cl-letf (((get 'gnus-unread-mark 'standard-value) default))
        (should-error (tessera-gnus-summary--register) :type 'error)
        (should-not (gethash 'gnus-summary tessera--entry-backends))
        (should (= gnus-unread-mark ?•))))))

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
             (entry
              (get-text-property start 'tessera-gnus-summary-entry)))
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

(ert-deftest tessera-gnus-navigation-keeps-native-partial-subject ()
  (save-window-excursion
    (with-temp-buffer
      (let ((gnus-show-threads nil)
            (gnus-summary-buffer (current-buffer))
            (gnus-summary-check-current nil)
            (gnus-auto-center-summary nil)
            (tessera-glyph-style 'ascii)
            (tessera-entry-layout 'two-line)
            (tessera-gnus-summary--active t))
        (setq major-mode 'gnus-summary-mode)
        (set-window-buffer (selected-window) (current-buffer))
        (tessera-tests--gnus-rows '(0 0 0))
        (tessera-gnus-summary--sync-buffer)
        (unwind-protect
            (progn
              (tessera-gnus-summary--navigation t)
              (gnus-summary-goto-subject 1)
              (let ((this-command 'gnus-summary-next-subject))
                (should (= 3 (gnus-summary-next-subject 5))))
              (should (= 3 (gnus-summary-article-number)))
              (should (= (point) (tessera-entry-point)))
              (gnus-summary-goto-subject 3)
              (let ((point (point)))
                (let ((this-command 'gnus-summary-next-subject))
                  (should (= 1 (gnus-summary-next-subject 1))))
                (should (= (point) point))
                (should (= 3 (gnus-summary-article-number)))))
          (tessera-gnus-summary--navigation nil))
        (should-not
         (advice-member-p
          #'tessera-gnus-summary--navigate
          'gnus-summary-next-subject))
        (should-not
         (advice-member-p
          #'tessera-gnus-summary--navigate-next-article
          'gnus-summary-next-article))))))

(ert-deftest tessera-gnus-navigation-stays-before-first-article ()
  (save-window-excursion
    (with-temp-buffer
      (let ((gnus-show-threads t)
            (gnus-newsgroup-name "test.group")
            (gnus-newsgroup-begin 1)
            (gnus-newsgroup-unfetched nil)
            (gnus-newsgroup-unreads '(1 3))
            (gnus-group-buffer (current-buffer))
            (gnus-summary-buffer (current-buffer))
            (gnus-summary-check-current nil)
            (gnus-auto-center-summary nil)
            (gnus-auto-extend-newsgroup t)
            (gnus-auto-select-next t)
            (tessera-gnus-summary-boundary-navigation nil)
            (tessera-gnus-summary--active t)
            extended)
        (setq major-mode 'gnus-summary-mode)
        (set-window-buffer (selected-window) (current-buffer))
        (tessera-tests--gnus-rows '(0 1 0))
        (gnus-summary-goto-subject 1)
        (let ((point (point)))
          (cl-letf (((symbol-function 'gnus-summary-goto-article)
                     (lambda (&rest _)
                       (setq extended t)
                       (gnus-summary-goto-subject 3)))
                    ((symbol-function 'gnus-summary-jump-to-group)
                     #'ignore)
                    ((symbol-function 'gnus-summary-search-group)
                     #'ignore)
                    ((symbol-function 'gnus-ephemeral-group-p)
                     (lambda (&rest _) t)))
            (unwind-protect
                (progn
                  (tessera-gnus-summary--navigation t)
                  (let ((this-command
                         'gnus-summary-prev-article))
                    (gnus-summary-prev-article))
                  (should-not extended)
                  (should (= 1 (gnus-summary-article-number)))
                  (should (= point (point))))
              (tessera-gnus-summary--navigation nil))))))))

(ert-deftest tessera-gnus-navigation-keeps-fold-at-first-article ()
  (with-temp-buffer
    (let ((gnus-show-threads t)
          (gnus-newsgroup-name "test.group")
          (gnus-newsgroup-unfetched nil)
          (gnus-newsgroup-unreads '(1 3 5))
          (gnus-summary-buffer (current-buffer))
          (gnus-auto-center-summary nil)
          (tessera-gnus-summary--active t)
          displayed)
      (setq major-mode 'gnus-summary-mode)
      (tessera-tests--gnus-rows)
      (tessera-gnus-summary--prepare)
      (gnus-summary-goto-subject 1)
      (add-to-invisibility-spec 'gnus-sum)
      (gnus-summary-hide-thread)
      (cl-letf (((symbol-function 'gnus-summary-display-article)
                 (lambda (&rest _)
                   (setq displayed t))))
        (unwind-protect
            (progn
              (tessera-gnus-summary--navigation t)
              (dolist (command
                       '(gnus-summary-first-unread-article
                         gnus-summary-first-article))
                (let ((this-command command))
                  (should-not (funcall command)))
                (should-not displayed)
                (should (= 1 (gnus-summary-article-number)))
                (should
                 (invisible-p
                  (gnus-data-pos (gnus-data-find 2))))))
          (tessera-gnus-summary--navigation nil))))))

(ert-deftest tessera-gnus-navigation-calls-first-in-empty-summary ()
  (with-temp-buffer
    (let ((gnus-newsgroup-data nil)
          (gnus-newsgroup-name "test.group")
          (gnus-summary-buffer (current-buffer))
          (tessera-gnus-summary--active t)
          (this-command 'gnus-summary-first-article)
          called)
      (setq major-mode 'gnus-summary-mode)
      (tessera-gnus-summary--navigate-first-article
       (lambda ()
         (setq called t)))
      (should called))))

(ert-deftest tessera-gnus-navigation-gates-native-boundary-options ()
  (save-window-excursion
    (with-temp-buffer
      (let ((gnus-newsgroup-name "test.group")
            (gnus-newsgroup-unfetched nil)
            (gnus-newsgroup-unreads '(1 3))
            (gnus-summary-buffer (current-buffer))
            (gnus-summary-check-current nil)
            (tessera-gnus-summary--active t)
            calls)
        (setq major-mode 'gnus-summary-mode)
        (set-window-buffer (selected-window) (current-buffer))
        (tessera-tests--gnus-rows '(0 1 0))
        (gnus-summary-goto-subject 1)
        (let ((this-command 'gnus-summary-prev-article)
              (native
               (lambda (&rest _)
                 (push (list gnus-auto-extend-newsgroup
                             gnus-auto-select-next)
                       calls))))
          (let ((gnus-auto-extend-newsgroup t)
                (gnus-auto-select-next 'quietly)
                (tessera-gnus-summary-boundary-navigation nil))
            (tessera-gnus-summary--navigate-next-article
             native nil nil t)
            (should (equal (pop calls) '(nil nil))))
          (let ((this-command 'gnus-summary-next-page)
                (gnus-auto-extend-newsgroup t)
                (gnus-auto-select-next 'quietly)
                (tessera-gnus-summary-boundary-navigation nil))
            (tessera-gnus-summary--navigate-next-article native)
            (should (equal (pop calls) '(nil nil))))
          (let ((gnus-auto-extend-newsgroup nil)
                (gnus-auto-select-next nil)
                (tessera-gnus-summary-boundary-navigation t))
            (tessera-gnus-summary--navigate-next-article
             native nil nil t)
            (should (equal (pop calls) '(nil nil))))
          (let ((gnus-auto-extend-newsgroup t)
                (gnus-auto-select-next nil)
                (tessera-gnus-summary-boundary-navigation t))
            (tessera-gnus-summary--navigate-next-article
             native nil nil t)
            (should (equal (pop calls) '(t nil))))
          (let ((gnus-auto-extend-newsgroup nil)
                (gnus-auto-select-next 'quietly)
                (tessera-gnus-summary-boundary-navigation t))
            (tessera-gnus-summary--navigate-next-article
             native nil nil t)
            (should (equal (pop calls) '(nil quietly)))))))))

(ert-deftest tessera-gnus-navigation-restores-related-group-point ()
  (let ((group (generate-new-buffer " *tessera-gnus-group*")))
    (unwind-protect
        (with-temp-buffer
          (with-current-buffer group
            (insert "first\nsecond\n")
            (goto-char (point-min)))
          (let ((gnus-group-buffer group)
                (gnus-newsgroup-name "test.group")
                (gnus-summary-buffer (current-buffer))
                (gnus-auto-extend-newsgroup nil)
                (gnus-auto-select-next nil)
                (tessera-gnus-summary--active t)
                this-command
                tessera-gnus-summary-boundary-navigation)
            (setq major-mode 'gnus-summary-mode)
            (insert (propertize "first\n" 'gnus-number 1))
            (goto-char (point-min))
            (dolist (spec '((t gnus-summary-prev-article)
                            (nil gnus-summary-next-page)))
              (setq tessera-gnus-summary-boundary-navigation
                    (nth 0 spec)
                    this-command (nth 1 spec))
              (with-current-buffer group
                (goto-char (point-min)))
              (tessera-gnus-summary--navigate-next-article
               (lambda (&rest _arguments)
                 (with-current-buffer group
                   (goto-char (point-max)))
                 nil))
              (with-current-buffer group
                (should (= (point) (point-min)))))))
      (kill-buffer group))))

(ert-deftest tessera-gnus-navigation-skips-nonnavigation-callers ()
  (with-temp-buffer
    (let ((tessera-gnus-summary--active t)
          (gnus-newsgroup-name "test.group")
          (this-command 'gnus-summary-kill-thread))
      (setq major-mode 'gnus-summary-mode)
      (insert (propertize "first" 'gnus-number 1) "\n")
      (goto-char (point-min))
      (tessera-gnus-summary--navigate
       (lambda ()
         (forward-char 1)
         nil))
      (should (= (point) (1+ (point-min)))))))

(ert-deftest tessera-gnus-navigation-wraps-custom-n-and-p-commands ()
  (save-window-excursion
    (with-temp-buffer
      (let ((gnus-show-threads nil)
            (gnus-summary-buffer (current-buffer))
            (gnus-summary-check-current nil)
            (gnus-auto-center-summary nil)
            (tessera-glyph-style 'ascii)
            (tessera-entry-layout 'two-line)
            (tessera-gnus-summary--active t))
        (setq major-mode 'gnus-summary-mode)
        (use-local-map (copy-keymap gnus-summary-mode-map))
        (set-window-buffer (selected-window) (current-buffer))
        (tessera-tests--gnus-rows '(0 0 0))
        (tessera-gnus-summary--sync-buffer)
        (let ((next
               (lambda ()
                 (interactive)
                 (gnus-summary-next-subject 1)))
              (previous
               (lambda ()
                 (interactive)
                 (gnus-summary-prev-subject 1))))
          (local-set-key (kbd "n") next)
          (local-set-key (kbd "p") previous)
          (unwind-protect
              (progn
                (tessera-gnus-summary--navigation t)
                (gnus-summary-goto-subject 3)
                (forward-char 2)
                (let ((point (point))
                      (this-command next))
                  (funcall next)
                  (should (= (point) point))
                  (should (= 3 (gnus-summary-article-number))))
                (gnus-summary-goto-subject 1)
                (forward-char 2)
                (let ((point (point))
                      (this-command previous))
                  (funcall previous)
                  (should (= (point) point))
                  (should (= 1 (gnus-summary-article-number)))))
            (tessera-gnus-summary--navigation nil)))))))

(ert-deftest tessera-gnus-navigation-ignores-unrelated-buffer-switch
    ()
  (let ((summary (generate-new-buffer " *tessera-gnus-summary*"))
        (other (generate-new-buffer " *tessera-gnus-other*")))
    (unwind-protect
        (with-current-buffer summary
          (setq major-mode 'gnus-summary-mode
                gnus-newsgroup-name "test.group")
          (insert (propertize "first" 'gnus-number 1) "\n")
          (goto-char (point-min))
          (let ((identity
                 (tessera-gnus-summary--navigation-identity summary)))
            (with-current-buffer other
              (let ((gnus-summary-buffer summary))
                (should-not
                 (tessera-gnus-summary--navigation-changed-p
                  summary identity))))))
      (kill-buffer summary)
      (kill-buffer other))))

(ert-deftest tessera-gnus-navigation-keeps-horizontal-position ()
  (save-window-excursion
    (with-temp-buffer
      (let ((gnus-show-threads t)
            (gnus-newsgroup-headers nil)
            (gnus-summary-buffer (current-buffer))
            (gnus-article-buffer " *tessera-absent-article*")
            (gnus-summary-check-current nil)
            (gnus-auto-center-summary 2)
            (tessera-glyph-style 'ascii))
        (setq major-mode 'gnus-summary-mode)
        (set-window-buffer (selected-window) (current-buffer))
        (tessera-tests--gnus-rows '(0 1 0))
        (setf (mail-header-subject
               (gnus-data-header (gnus-data-find 3)))
              (make-string 200 ?x))
        (unwind-protect
            (cl-letf (((symbol-function 'window-end)
                       (lambda (&rest _) (point-max))))
              (tessera-gnus-mode 1)
              (tessera-gnus-summary--prepare)
              (gnus-summary-goto-subject 2)
              (set-window-hscroll (selected-window) 0)
              (gnus-summary-next-subject 1)
              (should (= 3 (gnus-summary-article-number)))
              (should (looking-at "Author"))
              (should (> (current-column) (/ (window-width) 2)))
              (should (= 0 (window-hscroll)))
              (should (= gnus-auto-center-summary 2))
              ;; Explicit horizontal scrolling remains available.
              (set-window-hscroll (selected-window) 3)
              (gnus-summary-recenter)
              (should (= 3 (window-hscroll)))
              ;; The same native call still works without Tessera.
              (let ((tessera-gnus-summary--active nil))
                (set-window-hscroll (selected-window) 0)
                (gnus-summary-recenter)
                (should (> (window-hscroll) 0)))
              (tessera-gnus-mode -1)
              (should-not
               (advice-member-p
                #'tessera-gnus-summary--horizontal-recenter
                'gnus-horizontal-recenter)))
          (tessera-gnus-mode -1))))))

(ert-deftest tessera-gnus-navigation-preserves-vertical-centering ()
  (save-window-excursion
    (delete-other-windows)
    (with-temp-buffer
      (let ((summary (current-buffer)))
        (with-temp-buffer
          (let ((gnus-article-buffer (current-buffer))
                (window (selected-window))
                (article-window (split-window-below)))
            (set-window-buffer article-window (current-buffer))
            (with-current-buffer summary
              (set-window-buffer window summary)
              (dotimes (_ 80)
                (let ((start (point)))
                  (insert (make-string 120 ?x) "\n")
                  (put-text-property (+ start 60) (+ start 61)
                                     'gnus-position t)))
              (let ((tessera-gnus-summary--active t))
                (unwind-protect
                    (progn
                      (tessera-gnus-summary--navigation t)
                      (dolist (setting '(2 t vertical nil))
                        (let ((gnus-auto-center-summary setting)
                              expected)
                          (goto-char (point-min))
                          (forward-line 20)
                          (forward-char 60)
                          (set-window-start window (point-min) t)
                          (set-window-hscroll window 0)
                          (setq expected
                                (if setting
                                    (save-excursion
                                      (forward-line
                                       (- (if (numberp setting)
                                              setting
                                            (/ (1- (window-height))
                                               2))))
                                      (point))
                                  (point-min)))
                          (gnus-summary-recenter)
                          (should (= (window-start window) expected))
                          (should (= (window-hscroll window) 0))
                          (should (eq gnus-auto-center-summary
                                      setting)))))
                  (tessera-gnus-summary--navigation nil))))))))))

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
        (tessera-gnus-summary--fold-changed)
        (goto-char (point-min))
        (tessera-gnus-summary--post-command)
        (should-not
         (seq-some
          (lambda (item) (overlay-get item 'tessera-entry-overlay))
          (overlays-in start (point-max))))
        (delete-overlay overlay)
        (tessera-gnus-summary--fold-changed)
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

(ert-deftest tessera-gnus-enable-failure-restores-state ()
  (let ((gnus-summary-line-format "Native format\n")
        (tessera-entry-layout 'single-line)
        (refresh-count 0)
        restored
        error-data)
    (cl-letf (((symbol-function 'tessera-gnus-summary--refresh)
               (lambda ()
                 (if (= (cl-incf refresh-count) 1)
                     (error "Refresh failed")
                   (setq restored
                         (equal gnus-summary-line-format
                                "Native format\n"))))))
      (with-temp-buffer
        (setq major-mode 'gnus-summary-mode)
        (condition-case error
            (tessera-gnus-summary--enable)
          (error (setq error-data error)))
        (should (equal error-data '(error "Refresh failed")))
        (should (= refresh-count 2))
        (should restored)
        (should-not tessera-gnus-summary--active)
        (should-not tessera-gnus-summary--saved-settings)
        (should-not (local-variable-p 'gnus-summary-line-format))
        (should-not (local-variable-p 'tessera-entry-layout))
        (should-not (memq #'tessera-gnus-summary--post-command
                          post-command-hook))))))

(ert-deftest tessera-gnus-disable-failure-restores-state ()
  (let ((gnus-summary-line-format "Native format\n")
        (tessera-entry-layout 'single-line)
        (refresh-count 0)
        error-data)
    (cl-letf (((symbol-function 'tessera-gnus-summary--refresh)
               (lambda ()
                 (when (= (cl-incf refresh-count) 2)
                   (error "Refresh failed")))))
      (with-temp-buffer
        (setq major-mode 'gnus-summary-mode)
        (tessera-gnus-summary--enable)
        (condition-case error
            (tessera-gnus-summary--disable)
          (error (setq error-data error)))
        (should (equal error-data '(error "Refresh failed")))
        (should (= refresh-count 3))
        (should-not tessera-gnus-summary--active)
        (should-not tessera-gnus-summary--saved-settings)
        (should-not (local-variable-p 'gnus-summary-line-format))
        (should-not (local-variable-p 'tessera-entry-layout))
        (should-not (memq #'tessera-gnus-summary--post-command
                          post-command-hook))))))

(ert-deftest tessera-gnus-mode-rolls-back-all-buffers ()
  (let ((first (generate-new-buffer " *tessera-gnus-mode-1*"))
        (second (generate-new-buffer " *tessera-gnus-mode-2*"))
        (tessera-gnus-mode nil)
        (tessera-gnus--installed nil)
        (gnus-summary-mode-hook nil)
        (tessera--glyph-change-functions nil)
        enabled disabled error-data)
    (unwind-protect
        (progn
          (dolist (buffer (list first second))
            (with-current-buffer buffer
              (setq major-mode 'gnus-summary-mode)))
          (cl-letf (((symbol-function 'tessera-gnus--enable-summary)
                     (lambda ()
                       (setq tessera-gnus-summary--active t)
                       (push (current-buffer) enabled)
                       (when (eq (current-buffer) first)
                         (error "Enable failed"))))
                    ((symbol-function
                      'tessera-gnus-summary--disable)
                     (lambda ()
                       (setq tessera-gnus-summary--active nil)
                       (push (current-buffer) disabled)))
                    ((symbol-function
                      'tessera-gnus-summary--track-folds)
                     #'ignore)
                    ((symbol-function
                      'tessera-gnus-summary--navigation)
                     #'ignore)
                    ((symbol-function
                      'tessera-gnus-article--track-content)
                     #'ignore))
            (condition-case error
                (tessera-gnus-mode 1)
              (error (setq error-data error))))
          (should (equal error-data '(error "Enable failed")))
          (should-not tessera-gnus-mode)
          (should (= (length enabled) 2))
          (should (= (length disabled) 2))
          (should-not (memq #'tessera-gnus--enable-summary
                            gnus-summary-mode-hook))
          (dolist (buffer (list first second))
            (with-current-buffer buffer
              (should-not tessera-gnus-summary--active))))
      (kill-buffer first)
      (kill-buffer second))))

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
        (tessera-gnus-summary-label-data
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
      (let ((handle
             (mm-make-handle (current-buffer) '("text/plain"))))
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

(ert-deftest tessera-gnus-batch-reindexes-native-positions-once ()
  (with-temp-buffer
    (let ((gnus-show-threads nil)
          (tessera-glyph-style 'ascii)
          (tessera-entry-layout 'two-line))
      (tessera-tests--gnus-rows (make-list 100 0))
      (tessera-gnus-summary--sync-buffer t)
      (gnus-summary-goto-subject 50)
      (let ((tessera-entry-layout 'single-line))
        (cl-letf (((symbol-function 'gnus-data-update-list)
                   (lambda (&rest _)
                     (ert-fail "Batch updated a suffix"))))
          (tessera-gnus-summary--sync-buffer t)))
      (should (= 50 (gnus-summary-article-number)))
      (should (= (point) (tessera-entry-point)))
      (dolist (data gnus-newsgroup-data)
        (goto-char (1- (gnus-data-pos data)))
        (should (bolp))
        (should (= (gnus-data-number data)
                   (gnus-summary-article-number)))))))

(ert-deftest tessera-gnus-mark-keeps-unrelated-layout-overlays ()
  (with-temp-buffer
    (let ((gnus-show-threads nil)
          (tessera-glyph-style 'ascii)
          (tessera-gnus-summary--active t)
          (count 0)
          (apply-layout
           (symbol-function 'tessera-entry-apply-layout)))
      (tessera-tests--gnus-rows (make-list 100 0))
      (tessera-gnus-summary--prepare)
      (gnus-summary-goto-subject 2)
      (let ((overlay (get-text-property
                      (line-beginning-position)
                      'tessera--layout-overlay)))
        (goto-char (point-min))
        (subst-char-in-region (point) (1+ (point))
                              gnus-unread-mark gnus-read-mark)
        (cl-letf (((symbol-function 'tessera-entry-apply-layout)
                   (lambda (&rest arguments)
                     (cl-incf count)
                     (apply apply-layout arguments))))
          (tessera-gnus-summary--sync-buffer))
        (should (= 1 count))
        (should (overlay-buffer overlay))
        (gnus-summary-goto-subject 2)
        (should (eq overlay (get-text-property
                             (line-beginning-position)
                             'tessera--layout-overlay)))))))

(ert-deftest tessera-gnus-fold-events-invalidate-without-polling ()
  (with-temp-buffer
    (let ((gnus-show-threads t)
          (gnus-summary-buffer (current-buffer))
          (gnus-auto-center-summary nil)
          (tessera-gnus-summary--active t))
      (tessera-tests--gnus-rows)
      (add-to-invisibility-spec 'gnus-sum)
      (tessera-gnus-summary--track-folds t)
      (unwind-protect
          (progn
            (tessera-gnus-summary--prepare)
            (gnus-summary-hide-thread)
            (should tessera-gnus-summary--dirty)
            (tessera-gnus-summary--post-command)
            (gnus-summary-goto-subject 1)
            (should (tessera-thread-context-last
                     (gethash 1 tessera-gnus-summary--threads)))
            (cl-letf (((symbol-function 'overlays-in)
                       (lambda (&rest _)
                         (ert-fail "Clean command scanned"))))
              (tessera-gnus-summary--post-command))
            (gnus-summary-show-all-threads)
            (should tessera-gnus-summary--dirty)
            (tessera-gnus-summary--post-command)
            (should-not (tessera-thread-context-last
                         (gethash 1 tessera-gnus-summary--threads))))
        (tessera-gnus-summary--track-folds nil)))))

(ert-deftest tessera-gnus-content-slots-share-one-observation ()
  (let ((calls 0)
        (original
         (symbol-function 'tessera-gnus-summary--content-data)))
    (cl-letf (((symbol-function 'tessera-gnus-summary--content-data)
               (lambda (header)
                 (cl-incf calls)
                 (funcall original header))))
      (tessera-gnus-summary--render
       (tessera-gnus-tests--header) (tessera-gnus-tests--metadata)))
    (should (= 1 calls))))

(provide 'tessera-gnus-summary-tests)
;;; tessera-gnus-summary-tests.el ends here
