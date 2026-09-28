;;; tessera-header-line-tests.el --- Header contracts -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercise populations, async observations and header ownership.

;;; Code:

(require 'ert)
(require 'tessera-test-support)
(require 'tessera)
(require 'elfeed)
(require 'tessera-elfeed-search)
(require 'tessera-gnus-summary)
(require 'mu4e-headers)
(require 'mu4e-update)
(require 'tessera-mu4e-headers)

(defvar tessera-header-tests--action nil)
(defvar tessera-header-tests--info nil)
(defvar tessera-header-tests--extra nil)
(defvar tessera-header-tests--statistics nil)

(defmacro tessera-header-tests--with-buffer (&rest body)
  "Run BODY in a buffer whose normal lifecycle hooks are enabled."
  (declare (indent 0) (debug t))
  `(let ((buffer (generate-new-buffer " *Header test*")))
     (unwind-protect
         (with-current-buffer buffer ,@body)
       (when (buffer-live-p buffer) (kill-buffer buffer)))))

(defun tessera-header-tests--enable (reader)
  "Attach test providers with state READER in the current buffer."
  (tessera--header-line-enable
   'test reader
   '((action . tessera-header-tests--action)
     (info . tessera-header-tests--info)
     (extra . tessera-header-tests--extra)
     (statistics . tessera-header-tests--statistics))))

(defun tessera-header-tests--context (&optional state)
  "Return a rendering context with STATE for the selected window."
  (make-tessera-header-line-context
   :view 'test
   :buffer (current-buffer)
   :window (selected-window)
   :state state
   :now (seconds-to-time 100000)))

(ert-deftest tessera-header-duration-boundaries ()
  (cl-loop for seconds in '(0 59 60 119 300 3599 3600 7200 259200)
           for expected in '("<1 minute" "<1 minute" "1 minute"
                             "1 minute" "5 minutes" "59 minutes"
                             "1 hour" "2 hours" "3 days")
           do (should (equal (tessera-header-line-duration seconds)
                             expected))))

(ert-deftest tessera-header-restore-locality-and-native-replacement ()
  (dolist (local '(nil t))
    (tessera-header-tests--with-buffer
      (let ((original header-line-format)
            (tessera-header-line-enabled t))
        (when local (setq-local header-line-format '("Native")))
        (tessera-header-tests--enable #'ignore)
        (tessera-header-tests--enable #'ignore)
        (should (eq header-line-format tessera--header-line-format))
        (tessera--header-line-disable)
        (should (eq (local-variable-p 'header-line-format) local))
        (should (equal header-line-format
                       (if local '("Native") original)))
        (tessera-header-tests--enable #'ignore)
        (setq-local header-line-format '("New native search"))
        (tessera--header-line-prepare)
        (tessera--header-line-disable)
        (should (equal header-line-format '("New native search")))))))

(ert-deftest tessera-header-toggle-and-kill-release-resources ()
  (let ((tessera--header-line-buffers nil)
        (tessera--header-line-timer nil))
    (tessera-header-tests--with-buffer
      (setq-local header-line-format "Native")
      (setq-local tessera-header-line-enabled t)
      (tessera-header-tests--enable #'ignore)
      (should (timerp tessera--header-line-timer))
      (setq-local tessera-header-line-enabled nil)
      (tessera--header-line-prepare)
      (should (equal header-line-format "Native"))
      (setq-local tessera-header-line-enabled t)
      (tessera--header-line-prepare)
      (should (eq header-line-format tessera--header-line-format)))
    (should-not tessera--header-line-buffers)
    (should-not tessera--header-line-timer)))

(ert-deftest tessera-header-redisplay-reuses-statistics ()
  (let ((calls 0))
    (tessera-header-tests--with-buffer
      (tessera-header-tests--enable
       (lambda () (cl-incf calls) (list :shown (buffer-size))))
      (dotimes (_ 4)
        (tessera--header-line-prepare)
        (tessera--header-line-display))
      (should (= calls 1))
      (insert "one row")
      (tessera--header-line-prepare)
      (should (= calls 2))
      (should (= (plist-get tessera--header-line-state :shown) 7)))))

(ert-deftest tessera-header-graphical-format-preserves-percent ()
  (skip-unless (display-graphic-p))
  (let ((tessera-header-tests--info (lambda (_) "100% complete")))
    (tessera-header-tests--with-buffer
      (tessera-header-tests--enable #'ignore)
      (should (string-match-p
               "100% complete"
               (format-mode-line header-line-format 'header-line
                                 (selected-window)
                                 (current-buffer)))))))

(ert-deftest tessera-header-providers-retain-properties-and-help ()
  (let* ((context (tessera-header-tests--context))
         (keymap (make-sparse-keymap))
         (source (concat
                  (propertize "A" 'face 'bold 'help-echo "Detail"
                              'keymap keymap)
                  "B"))
         (text (tessera--header-line-region 'info source context)))
    (should (eq (get-text-property 0 'keymap text) keymap))
    (should (equal (get-text-property 0 'help-echo text) "Detail"))
    (should (equal (get-text-property 1 'help-echo text) "AB"))
    (should (get-text-property 1 'mouse-face text))
    (should-not (get-text-property 1 'help-echo source))
    (should-not (tessera--header-line-region 'info nil context))
    (should-error
     (tessera--header-line-region 'info "a\nb" context))))

(ert-deftest tessera-header-fit-measures-styled-suffix ()
  (let ((text (propertize "abcdef" 'face '(:height 3.0)
                          'help-echo "Full query")))
    (cl-letf (((symbol-function 'tessera--header-line-width)
               (lambda (string)
                 (cl-loop for index below (length string)
                          sum (if (get-text-property
                                   index 'face string) 3 1)))))
      (let ((fitted (tessera--header-line-fit text 8)))
        (should (equal fitted "a…"))
        (should (<= (tessera--header-line-width fitted) 8))
        (dolist (property '(face help-echo))
          (should (equal (get-text-property 1 property fitted)
                         (get-text-property 0 property text)))))
      (should-not (tessera--header-line-fit text 2))
      (should-not (tessera--header-line-fit text 0)))))

(ert-deftest tessera-header-graphical-fit-respects-pixel-budget ()
  (skip-unless (display-graphic-p))
  (dolist (text
           (list
            (concat (propertize "Query: " 'face 'bold)
                    (propertize "abcdefghijklmnopqrstuv"
                                'face '(:height 3.0)))
            (concat (propertize "A" 'face '(:height 5.0))
                    (propertize "B" 'face '(:height 1.0))
                    (propertize "C" 'face '(:height 5.0))
                    (propertize "D" 'face '(:height 1.0))
                    (propertize "E" 'face '(:height 5.0)))))
    (put-text-property 0 (length text) 'help-echo "Full text" text)
    (let* ((candidate
            (concat (substring text 0 2)
                    (apply #'propertize "…"
                           (text-properties-at 1 text))))
           (width (tessera-tests--pixel-width candidate 'header-line))
           (fitted (tessera--header-line-fit text width)))
      (should fitted)
      (should (>= (length fitted) (length candidate)))
      (should (<= (tessera-tests--pixel-width fitted 'header-line)
                  width))
      (should (equal (get-text-property
                      (1- (length fitted)) 'help-echo fitted)
                     "Full text")))))

(ert-deftest tessera-header-fit-handles-changing-suffix-width ()
  (let ((text (concat (propertize "A" 'face '(:height 5))
                      (propertize "B" 'face '(:height 1))
                      (propertize "C" 'face '(:height 5))
                      (propertize "D" 'face '(:height 1))
                      (propertize "E" 'face '(:height 5)))))
    (cl-letf (((symbol-function 'tessera--header-line-width)
               (lambda (string)
                 (cl-loop for index below (length string)
                          sum (plist-get
                               (get-text-property index 'face string)
                               :height)))))
      (should (equal (tessera--header-line-fit text 7) "AB…"))
      (should (equal (tessera--header-line-fit text 13) "ABCD…")))))

(ert-deftest tessera-header-fields-use-inert-spaces ()
  (let ((tessera-header-tests--action
         (lambda (_) (tessera-header-line-button "Update" #'ignore)))
        (tessera-header-tests--info
         (lambda (_) "Query: 100%  complete"))
        (tessera-header-tests--extra (lambda (_) "Work"))
        (tessera-header-tests--statistics (lambda (_) "10 / 20")))
    (tessera-header-tests--with-buffer
      (tessera-header-tests--enable #'ignore)
      (let* ((text (tessera--header-line-render (selected-window)))
             (left-gap (string-match " Query" text))
             (right-gap (string-match " 10 / 20" text)))
        (should left-gap)
        (should right-gap)
        (should-not (string-match-p "│" text))
        (should (string-match-p "Update Query: 100%  complete" text))
        (should (string-match-p "Work 10 / 20" text))
        (dolist (gap (list left-gap right-gap))
          (should-not (get-text-property gap 'keymap text))
          (should-not (get-text-property gap 'mouse-face text)))))))

(ert-deftest tessera-header-click-runs-in-event-window ()
  (tessera-header-tests--with-buffer
    (save-window-excursion
      (let* ((original (selected-window))
             (target (split-window-below))
             (buffer (current-buffer))
             (called nil)
             (text (tessera-header-line-button
                    "Update" (lambda ()
                               (interactive)
                               (setq called
                                     (list (current-buffer)
                                           (selected-window)))))))
        (set-window-buffer target buffer)
        (tessera--header-line-click
         (list 'mouse-1
               (list target 'header-line '(0 . 0) 0 (cons text 0))))
        (should (equal called (list buffer target)))
        (should (eq (selected-window) original))))))

(ert-deftest tessera-header-statistics-have-separate-hover-targets ()
  (let* ((context (tessera-header-tests--context
                   '(:shown 500 :matched 1284 :unread 37 :feeds 14)))
         (text (tessera--header-line-region
                'statistics (tessera-header-line-statistics context)
                context))
         (previous nil))
    (cl-loop
     for value in '("37" "500" "1284" "14")
     for label in '("Unread:" "Displayed:" "Query results:" "Feeds:")
     do
     (let* ((index (string-match value text))
            (end (+ index (length value)))
            (hover (get-text-property index 'mouse-face text)))
       (should (string-prefix-p
                label (get-text-property index 'help-echo text)))
       (should (memq 'tessera-header-line-hover-face hover))
       (should-not (eq hover previous))
       (cl-loop for position from index below end
                do (should (eq (get-text-property
                                position 'mouse-face text) hover))
                do (should (equal (get-text-property
                                   position 'help-echo text)
                                  (get-text-property
                                   index 'help-echo text))))
       (should-not (eq (get-text-property end 'mouse-face text)
                       hover))
       (setq previous hover)))
    (let ((slash (string-match "/" text)))
      (should (string-match-p "500/1284" text))
      (should-not (string-match-p "  " text))
      (should (equal (get-text-property slash 'help-echo text) ""))
      (should (equal (get-text-property slash 'mouse-face text)
                     '(:inherit nil))))
    (should (eq (face-attribute 'tessera-header-line-hover-face
                                :inherit nil)
                'tessera-entry-hover-face))))

(ert-deftest tessera-header-update-next-unknown-and-overdue ()
  (let ((context (tessera-header-tests--context))
        (tessera-header-line-update-time 'auto))
    (should (string-prefix-p
             "Update: —" (tessera-header-line-update
                          context nil nil nil #'ignore "Scope")))
    (should (string-prefix-p
             "Next Update: <1 minute"
             (tessera-header-line-update
              context nil (seconds-to-time 100030) nil #'ignore "")))
    (should (string-prefix-p
             "Next Update: due"
             (tessera-header-line-update
              context nil (seconds-to-time 99999) nil #'ignore "")))
    (let ((tessera-header-line-update-time 'last))
      (should (string-prefix-p
               "Update: 1 minute ago"
               (tessera-header-line-update
                context (seconds-to-time 99940)
                (seconds-to-time 100030) nil #'ignore ""))))))

(ert-deftest tessera-header-elfeed-counts-inserted-set ()
  (let* ((first (elfeed-entry--create :id '("a" . "1")
                                      :feed-id "a" :tags '(unread)))
         (second (elfeed-entry--create :id '("b" . "2")
                                       :feed-id "b"))
         (third (elfeed-entry--create :id '("c" . "3")
                                      :feed-id "c" :tags '(unread)))
         (elfeed-search-entries (list first second third))
         (elfeed-search-filter "#3"))
    (tessera-header-tests--with-buffer
      (insert (propertize "One\n" 'elfeed-entry first 'invisible t)
              (propertize "Two\n" 'elfeed-entry second)
              "Truncated\n")
      (narrow-to-region (point-min) 5)
      (let ((state (tessera-elfeed-search--header-line-state)))
        (should (= (plist-get state :shown) 2))
        (should (= (plist-get state :unread) 1))
        (should (= (plist-get state :feeds) 2))
        (should (= (plist-get state :matched) 3))))))

(ert-deftest tessera-header-gnus-limits-do-not-change-loaded-count ()
  (let* ((one (make-full-mail-header 1))
         (two (make-full-mail-header 2))
         (three (make-full-mail-header 3))
         (gnus-newsgroup-headers (list one two three))
         (gnus-newsgroup-data
          (list (gnus-data-make 1 gnus-unread-mark 1 one 0)
                (gnus-data-make 2 gnus-read-mark 8 two 0)
                (gnus-data-make 0 gnus-unread-mark 1 '(missing) 0)))
         (gnus-newsgroup-name "test.group")
         (state (tessera-gnus-summary--header-line-state)))
    (should (= (plist-get state :shown) 2))
    (should (= (plist-get state :unread) 1))
    (should (= (plist-get state :loaded) 3))
    (let* ((tessera-glyph-style 'ascii)
           (text (tessera-header-line-statistics
                  (tessera-header-tests--context state)))
           (index (string-match "2/3" text)))
      (should index)
      (should (string-match-p
               "Loaded article headers"
               (get-text-property (+ index 2) 'help-echo text))))))

(ert-deftest tessera-header-mu4e-count-excludes-footer ()
  (let* ((message '(:docid 17 :flags (unread)))
         (mu4e--search-last-query "subject:100%")
         (mu4e~headers-docid-pre "\376"))
    (tessera-header-tests--with-buffer
      (insert (propertize "\37617 Message\n" 'msg message
                          'invisible t)
              (propertize "Repeated properties\nEnd\n" 'msg message))
      (let ((state (tessera-mu4e-headers--header-line-state)))
        (should (= (plist-get state :shown) 1))
        (should (= (plist-get state :unread) 1))
        (should (equal (plist-get state :query) "subject:100%"))))))

(ert-deftest tessera-header-mu4e-uses-native-index-timestamp ()
  (let ((mu4e-index-update-status
         (list :tstamp (seconds-to-time 99940)))
        (mu4e--server-indexing nil)
        (mu4e--update-buffer nil)
        (tessera-header-line-update-time 'last))
    (should (string-prefix-p
             "Update: 1 minute ago"
             (tessera-mu4e-headers-header-line-action
              (tessera-header-tests--context))))))

(ert-deftest tessera-header-gnus-records-only-completed-rescans ()
  (let ((tessera-gnus-summary--update-times
         (make-hash-table :test #'equal))
        (gnus-newsgroup-name "test.group"))
    (tessera-gnus-summary--observe-rescan #'ignore nil nil)
    (should-not (gethash "test.group"
                         tessera-gnus-summary--update-times))
    (tessera-gnus-summary--observe-rescan #'ignore nil t)
    (let ((last (plist-get
                 (gethash "test.group"
                          tessera-gnus-summary--update-times) :last)))
      (should last)
      (should-error
       (tessera-gnus-summary--observe-rescan
        (lambda (&rest _) (error "Scan failed")) nil t))
      (let ((state (gethash "test.group"
                            tessera-gnus-summary--update-times)))
        (should (plist-get state :failed))
        (should-not (plist-get state :running))
        (should (equal (plist-get state :last) last))))))

(ert-deftest tessera-header-elfeed-observes-background-completion ()
  (let ((tessera-elfeed-search--requests nil)
        (tessera-elfeed-search--last-update nil)
        (tessera-elfeed-search--update-failed nil)
        (native-hooks 0)
        callbacks)
    (let ((elfeed-fetch-functions
           (list (lambda (_url callback)
                   (push callback callbacks)
                   t)))
          (elfeed-update-hook
           (list (lambda (_) (cl-incf native-hooks))))
          (tracked (advice-member-p
                    #'tessera-elfeed-search--observe-update
                    'elfeed--update-feed)))
      (unwind-protect
          (progn
            (tessera-elfeed-search--header-line-track t)
            (elfeed--update-feed "feed-one" t)
            (elfeed--update-feed "feed-two" t)
            (should (= (length tessera-elfeed-search--requests) 2))
            (funcall (pop callbacks) :error)
            (should-not tessera-elfeed-search--last-update)
            (funcall (pop callbacks) :success)
            (should-not tessera-elfeed-search--requests)
            (should tessera-elfeed-search--last-update)
            (should tessera-elfeed-search--update-failed)
            (should (= native-hooks 0)))
        (tessera-elfeed-search--header-line-track tracked)))))

(ert-deftest tessera-header-elfeed-notifies-registered-buffers ()
  (let* ((buffers (cl-loop repeat 4 collect
                           (generate-new-buffer " *feed notify*")))
         (tessera--header-line-buffers (butlast buffers))
         (tessera-elfeed-search--batch-depth 0)
         (tessera-elfeed-search--requests nil)
         (tessera-elfeed-search--update-failed nil)
         visited)
    (unwind-protect
        (progn
          (dolist (buffer buffers)
            (with-current-buffer buffer
              (setq-local tessera--header-line-view 'elfeed-search
                          tessera-elfeed-search--active t
                          tessera-header-line-enabled t)))
          (with-current-buffer (nth 1 buffers)
            (setq-local tessera-header-line-enabled nil))
          (with-current-buffer (nth 2 buffers)
            (setq-local tessera--header-line-view 'gnus-summary))
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda (&rest _) (ert-fail "Global scan")))
                    ((symbol-function 'force-mode-line-update)
                     (lambda (&rest _) (push (current-buffer)
                                             visited))))
            (tessera-elfeed-search--observe-batch
             (lambda ()
               (dotimes (_ 5)
                 (tessera-elfeed-search--update-notify))
               (tessera-elfeed-search--observe-batch
                #'tessera-elfeed-search--update-notify)))
            (should (equal visited (list (car buffers))))))
      (mapc #'kill-buffer buffers))))

(ert-deftest tessera-header-elfeed-sync-failure-stays-in-batch ()
  (let ((tessera-elfeed-search--requests nil)
        (tessera-elfeed-search--last-update nil)
        (tessera-elfeed-search--update-failed nil)
        (elfeed-update-hook nil)
        (elfeed-fetch-functions
         (list (lambda (url callback)
                 (funcall callback (if (equal url "bad")
                                       :error :success))
                 t)))
        (tracked (advice-member-p
                  #'tessera-elfeed-search--observe-update
                  'elfeed--update-feed)))
    (unwind-protect
        (progn
          (tessera-elfeed-search--header-line-track t)
          (tessera-elfeed-search--observe-batch
           (lambda ()
             (elfeed--update-feed "bad")
             (elfeed--update-feed "good")))
          (should-not tessera-elfeed-search--requests)
          (should tessera-elfeed-search--last-update)
          (should tessera-elfeed-search--update-failed))
      (tessera-elfeed-search--header-line-track tracked))))

(ert-deftest tessera-header-elfeed-records-native-parse-outcome ()
  (dolist (valid '(nil t))
    (let* ((url "https://example.invalid/feed")
           (feed (elfeed-feed--create :id url :url url))
           (tessera-elfeed-search--requests nil)
           (tessera-elfeed-search--last-update nil)
           (tessera-elfeed-search--update-failed nil)
           (tessera-elfeed-search--batch-depth 0)
           (native-errors 0)
           (elfeed-parse-error-hook
            (list (lambda (&rest _) (cl-incf native-errors))))
           (elfeed-fetch-functions
            (list (lambda (_url callback)
                    (with-temp-buffer
                      (insert (if valid
                                  "<rss><channel/></rss>"
                                "This is not a feed"))
                      (goto-char (point-min))
                      (funcall callback :parse))
                    t))))
      (cl-letf (((symbol-function 'elfeed-db-get-feed)
                 (lambda (_) feed))
                ((symbol-function 'elfeed-db-add) #'ignore)
                ((symbol-function 'elfeed-log) #'ignore))
        (tessera-elfeed-search--observe-update
         #'elfeed--update-feed url t))
      (should (eq tessera-elfeed-search--update-failed (not valid)))
      (should (= native-errors (if valid 0 1)))
      (should-not tessera-elfeed-search--requests)
      (should tessera-elfeed-search--last-update))))

(provide 'tessera-header-line-tests)
;;; tessera-header-line-tests.el ends here
