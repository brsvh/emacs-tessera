;;; tessera-elfeed-search-tests.el --- Tessera Elfeed search tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bingshan Chang <chang@bingshan.org>

;; Author: Bingshan Chang <chang@bingshan.org>
;; Keywords: convenience, news, test

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Test the initial Tessera adapter for Elfeed search buffers.

;;; Code:

(require 'ert)
(require 'tessera-test-support)
(require 'tessera-elfeed)

(require 'elfeed-search)
(require 'tessera-elfeed-search)

(tessera-elfeed-search--register)

(defun tessera-elfeed-search-tests--entry
    (&optional tags enclosures id)
  "Return an Elfeed entry with TAGS, ENCLOSURES, and optional ID."
  (elfeed-entry--create
   :id (or id '("https://example.invalid/feed" . "entry"))
   :title "A useful Elfeed entry"
   :link "https://example.invalid/entry"
   :date 1755475200.0
   :enclosures enclosures
   :tags tags
   :feed-id "https://example.invalid/feed"))

(defun tessera-elfeed-search-tests--insert-entries (months)
  "Insert native entries for MONTHS in an enabled search adapter.
Return the entries, also recording them as native search results."
  (setq-local
   elfeed-search-entries
   (cl-loop for month in months
            for index from 0
            collect
            (let ((entry
                   (tessera-elfeed-search-tests--entry
                    nil nil (cons "feed" index))))
              (setf (elfeed-entry-date entry)
                    (float-time
                     (encode-time 0 0 12 1 month 2026)))
              entry)))
  (dolist (entry elfeed-search-entries)
    (elfeed-search--print-entry entry)
    (insert "\n"))
  (tessera-elfeed-search--apply-layout)
  elfeed-search-entries)

(ert-deftest tessera-elfeed-search-undated-fields-remain-readable ()
  (dolist (date '(nil invalid 0))
    (dolist (enabled '(nil t))
      (let ((entry (tessera-elfeed-search-tests--entry))
            (tessera--month-enabled enabled)
            (tessera-entry-layout 'single-line)
            (tessera-glyph-style 'ascii))
        (setf (elfeed-entry-date entry) date)
        (let* ((text (tessera-entry-render 'elfeed-search entry))
               (context (get-text-property
                         0 'tessera-entry-context text))
               (field (tessera-elfeed-search--date context)))
          (should (eq (tessera-entry-context-month-undated context)
                      (and enabled (not (equal date 0)))))
          (if (equal date 0)
              (should (equal (get-text-property 0 'follow-link field)
                             [elfeed-date]))
            (should (equal (substring-no-properties field) "Unknown"))
            (should-not (get-text-property 0 'follow-link field)))
          (should (equal (elfeed-entry-date entry) date)))))))

(ert-deftest tessera-elfeed-search-renders-two-line-layout ()
  (let* ((entry
          (tessera-elfeed-search-tests--entry
           '(unread emacs)
           '(("https://example.invalid/notes.pdf"
              "application/pdf" 4096))))
         (elfeed-search-print-entry-function
          #'tessera-elfeed-search-print-entry)
         (elfeed-db '(:version 4))
         (elfeed-db-feeds (make-hash-table :test #'equal))
         (feed
          (elfeed-feed--create
           :id (elfeed-entry-feed-id entry)
           :title "Example Feed"))
         (tessera-entry-layout 'two-line)
         (tessera-glyph-style 'ascii))
    (puthash (elfeed-entry-feed-id entry) feed elfeed-db-feeds)
    (with-temp-buffer
      (elfeed-search--print-entry entry)
      (should-not (string-match-p "\n" (buffer-string)))
      (let ((rendered (buffer-substring (point-min) (point-max))))
        (should
         (= (tessera-entry-point rendered)
            (string-match "A useful Elfeed entry" rendered)))
        (should
         (tessera-tests--property-position 'display "\n" rendered))
        (should (string-match-p "\\*" rendered))
        (should (string-match-p "Example Feed" rendered))
        (should (string-match-p
                 "https://example.invalid/entry" rendered))
        (should (string-match-p "@" rendered))
        (should (string-match-p "(emacs)" rendered))
        (should-not (string-match-p "unread" rendered))
        (should
         (tessera-tests--property-position
          'help-echo "Enclosure: application/pdf" rendered)))
      (goto-char (point-min))
      (search-forward "A useful Elfeed entry")
      (should (equal (get-text-property (match-beginning 0) 'face)
                     '(tessera-elfeed-search-unread-title-face
                       tessera-elfeed-search-title-face)))
      (should (equal (get-text-property
                      (match-beginning 0) 'follow-link)
                     [elfeed-entry]))
      (search-forward "Example Feed")
      (should (eq (get-text-property (match-beginning 0) 'face)
                  'tessera-elfeed-search-unread-feed-face))
      (search-forward "https://example.invalid/entry")
      (should (eq (get-text-property (match-beginning 0) 'face)
                  'tessera-elfeed-search-unread-url-face))
      (should (eq (face-attribute
                   'tessera-elfeed-search-url-face
                   :slant nil 'default)
                  'italic))
      (search-forward "emacs")
      (should (eq (get-text-property (match-beginning 0) 'face)
                  'tessera-elfeed-search-tag-face))
      (goto-char (point-max))
      (re-search-backward "[[:digit:]]")
      (should (eq (get-text-property (point) 'face)
                  'tessera-elfeed-search-unread-date-face))
      (should (eq (get-text-property (point-min) 'elfeed-entry)
                  entry)))))

(ert-deftest tessera-elfeed-search-preserves-title-properties ()
  (dolist (source '(title custom-title link))
    (let* ((text (propertize "Entry title" 'language 'en))
           (original (copy-sequence text))
           (entry (tessera-elfeed-search-tests--entry '(unread)))
           (context (tessera-elfeed-search--context entry nil nil)))
      (pcase source
        ('title (setf (elfeed-entry-title entry) text))
        ('custom-title (setf (elfeed-meta entry :title) text))
        ('link
         (setf (elfeed-entry-title entry) ""
               (elfeed-entry-link entry) text)))
      (let ((rendered (tessera-elfeed-search--title context)))
        (should (equal-including-properties text original))
        (should (equal rendered text))
        (should (eq (get-text-property 0 'language rendered) 'en))
        (should (equal (get-text-property 0 'face rendered)
                       '(tessera-elfeed-search-unread-title-face
                         tessera-elfeed-search-title-face)))
        (should (eq (get-text-property 0 'mouse-face rendered)
                    'highlight))
        (should (equal (get-text-property 0 'follow-link rendered)
                       [elfeed-entry]))))))

(ert-deftest tessera-elfeed-search-fits-long-feed-names ()
  (let* ((entry (tessera-elfeed-search-tests--entry))
         (elfeed-db '(:version 4))
         (elfeed-db-feeds (make-hash-table :test #'equal))
         (feed (elfeed-feed--create :id (elfeed-entry-feed-id entry)))
         (tessera-entry-layout 'two-line)
         (tessera-glyph-style 'ascii)
         (allocator
          (symbol-function 'tessera--allocate-segment-widths)))
    (puthash (elfeed-entry-feed-id entry) feed elfeed-db-feeds)
    (dolist (title (list "Short Feed" (make-string 60 ?F)
                         (make-string 120 ?F)
                         (make-string 60 ?界)))
      (setf (elfeed-feed-title feed) title)
      (dolist (width '(30 60 80 160))
        (let ((lines 0) title-width feed-width)
          (cl-letf
              (((symbol-function 'window-body-width)
                (lambda (&rest _) width))
               ((symbol-function 'tessera--allocate-segment-widths)
                (lambda (left right slots available)
                  (funcall allocator left right slots available)
                  (cl-incf lines)
                  (should (<= (tessera--single-line-width
                               left right slots)
                              available))
                  (when (= lines 1)
                    (setq title-width
                          (tessera--rendered-segment-target-width
                           (car left))
                          feed-width
                          (tessera--rendered-segment-target-width
                           (car right)))))))
            (let ((text (tessera-entry-render
                         'elfeed-search entry (selected-window))))
              (should (= lines 2))
              (when (< feed-width (string-width title))
                (should (= title-width 4)))
              (when (= width 160)
                (should (string-match-p
                         (elfeed-entry-title entry) text))
                (should (string-match-p title text)))
              (when (and (= width 80) (= (string-width title) 60))
                (should (string-match-p title text))
                (should-not (string-match-p
                             (elfeed-entry-title entry) text))))))))))

(ert-deftest tessera-elfeed-search-shrinks-second-line-in-order ()
  (let* ((entry
          (tessera-elfeed-search-tests--entry
           '(unread emacs development accessibility)
           '(("https://example.invalid/notes.pdf"
              "application/pdf" 4096))))
         (elfeed-db '(:version 4))
         (elfeed-db-feeds (make-hash-table :test #'equal))
         (feed
          (elfeed-feed--create
           :id (elfeed-entry-feed-id entry)
           :title "A Very Long Example Feed"))
         (tessera-entry-layout 'two-line)
         (tessera-glyph-style 'ascii))
    (puthash (elfeed-entry-feed-id entry) feed elfeed-db-feeds)
    (cl-labels
        ((render-at
           (width)
           (cl-letf (((symbol-function 'window-body-width)
                      (lambda (&rest _) width)))
             (substring-no-properties
              (tessera-entry-render
               'elfeed-search entry (selected-window))))))
      (let ((wide (render-at 80))
            (medium (render-at 60))
            (narrow (render-at 30)))
        (should-not (string-match-p
                     "https://example.invalid/entry" wide))
        (should (string-match-p
                 "(emacs,development,accessibility)" wide))
        (should (string-match-p "@" wide))
        (should-not (string-match-p
                     "(emacs,development,accessibility)" medium))
        (should (string-match-p "@" medium))
        (should-not (string-match-p "@" narrow))
        (should (string-match-p "2025-08-18" narrow))))))

(ert-deftest tessera-elfeed-search-selects-read-status ()
  (let ((unread-context
         (tessera-elfeed-search--context
          (tessera-elfeed-search-tests--entry '(unread))
          (current-buffer) nil))
        (read-context
         (tessera-elfeed-search--context
          (tessera-elfeed-search-tests--entry nil)
          (current-buffer) nil)))
    (should (eq (tessera-elfeed-search--select-status unread-context)
                'unread))
    (should (eq (tessera-elfeed-search--select-status read-context)
                'read))))

(ert-deftest tessera-elfeed-search-glyphs-are-configurable ()
  (let ((original tessera-elfeed-search-glyphs)
        (tessera--glyph-change-functions
         '(tessera-elfeed--glyphs-changed))
        (entry (tessera-elfeed-search-tests--entry '(unread)))
        (tessera-entry-layout 'single-line)
        (tessera-glyph-style 'ascii))
    (unwind-protect
        (progn
          (tessera-elfeed-search--set-glyphs
           'tessera-elfeed-search-glyphs
           '((status-unread :ascii "U" :unicode "◉")))
          (should (string-match-p
                   "U"
                   (tessera-entry-render 'elfeed-search entry))))
      (tessera-elfeed-search--set-glyphs
       'tessera-elfeed-search-glyphs original))))

(ert-deftest tessera-elfeed-search-hidden-enclosure-omits-segment ()
  (let* ((entry
          (tessera-elfeed-search-tests--entry
           nil '(("https://example.invalid/a" "text/plain" 10))))
         (context (tessera-elfeed-search--context entry nil nil))
         (tessera-elfeed-search-glyphs '((enclosure :hidden t))))
    (should-not (tessera-elfeed-search--enclosure context))
    (should-not
     (tessera--render-segment
      '(enclosure :optional t)
      (gethash 'elfeed-search tessera--entry-backends) context))))

(ert-deftest tessera-elfeed-search-omits-missing-enclosure ()
  (let* ((entry (tessera-elfeed-search-tests--entry))
         (context
          (tessera-elfeed-search--context
           entry (current-buffer) nil)))
    (should-not (tessera-elfeed-search--enclosure context))))

(ert-deftest tessera-elfeed-search-restores-setting-locality ()
  (dolist (local '(nil t))
    (let ((updates 0)
          (elfeed-search-print-entry-function #'ignore)
          (tessera-entry-layout 'single-line))
      (cl-letf (((symbol-function 'elfeed-search-update)
                 (lambda (&rest _)
                   (setq updates (1+ updates)))))
        (with-temp-buffer
          (setq major-mode 'elfeed-search-mode)
          (when local
            (setq-local elfeed-search-print-entry-function #'ignore)
            (setq-local tessera-entry-layout 'single-line))
          (tessera-elfeed-search--enable)
          (should (eq elfeed-search-print-entry-function
                      #'tessera-elfeed-search-print-entry))
          (should (eq tessera-entry-layout 'two-line))
          (should (memq
                   #'tessera-elfeed-search--apply-layout
                   elfeed-search-update-hook))
          (should (memq #'tessera-elfeed-search--post-command
                        post-command-hook))
          (should (memq #'tessera-elfeed-search--sync-months
                        pre-redisplay-functions))
          (tessera-elfeed-search--disable)
          (should-not (memq #'tessera-elfeed-search--post-command
                            post-command-hook))
          (should-not tessera--current-entry)
          (should-not (memq #'tessera-elfeed-search--sync-months
                            pre-redisplay-functions))
          (should (eq (local-variable-p
                       'elfeed-search-print-entry-function) local))
          (should (eq elfeed-search-print-entry-function #'ignore))
          (should (eq (local-variable-p 'tessera-entry-layout) local))
          (should (eq tessera-entry-layout 'single-line))
          (should-not
           (memq #'tessera-elfeed-search--apply-layout
                 elfeed-search-update-hook))
          (should (= updates 2)))))))

(ert-deftest tessera-elfeed-search-lifecycle-is-idempotent ()
  (let ((elfeed-search-mode-hook nil)
        (tessera-elfeed-search--navigation-users 0)
        (tessera--entry-backends (make-hash-table :test #'eq))
        (emulation-mode-map-alists
         (copy-sequence emulation-mode-map-alists)))
    (cl-letf (((symbol-function 'elfeed-search-update) #'ignore))
      (with-temp-buffer
        (elfeed-search-mode)
        (tessera-elfeed--enable-search)
        (tessera-elfeed--enable-search)
        (should tessera-elfeed-search--active)
        (should (gethash 'elfeed-search tessera--entry-backends))
        (should (= tessera-elfeed-search--navigation-users 1))
        (tessera-elfeed-search--disable)
        (tessera-elfeed-search--disable)
        (should-not tessera-elfeed-search--active)
        (should (zerop
                 tessera-elfeed-search--navigation-users))))))

(ert-deftest tessera-elfeed-mode-rolls-back-all-buffers ()
  (let ((first (generate-new-buffer " *tessera-elfeed-mode-1*"))
        (second (generate-new-buffer " *tessera-elfeed-mode-2*"))
        (tessera-elfeed-mode nil)
        (tessera-elfeed--installed nil)
        (elfeed-search-mode-hook nil)
        (tessera--glyph-change-functions nil)
        (tessera--month-change-functions nil)
        enabled disabled error-data)
    (unwind-protect
        (progn
          (dolist (buffer (list first second))
            (with-current-buffer buffer
              (setq major-mode 'elfeed-search-mode)))
          (cl-letf (((symbol-function 'tessera-elfeed--enable-search)
                     (lambda ()
                       (setq tessera-elfeed-search--active t)
                       (push (current-buffer) enabled)
                       (when (eq (current-buffer) first)
                         (error "Enable failed"))))
                    ((symbol-function
                      'tessera-elfeed-search--disable)
                     (lambda ()
                       (setq tessera-elfeed-search--active nil)
                       (push (current-buffer) disabled))))
            (condition-case error
                (tessera-elfeed-mode 1)
              (error (setq error-data error))))
          (should (equal error-data '(error "Enable failed")))
          (should-not tessera-elfeed-mode)
          (should (= (length enabled) 2))
          (should (= (length disabled) 2))
          (should-not (memq #'tessera-elfeed--enable-search
                            elfeed-search-mode-hook))
          (should-not (memq #'tessera-elfeed--months-changed
                            tessera--month-change-functions))
          (dolist (buffer (list first second))
            (with-current-buffer buffer
              (should-not tessera-elfeed-search--active))))
      (kill-buffer first)
      (kill-buffer second))))

(ert-deftest tessera-elfeed-search-major-mode-change-cleans-layout ()
  (let ((elfeed-search-mode-hook nil)
        (elfeed-db '(:version 4))
        (elfeed-db-feeds (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'elfeed-search-update) #'ignore))
      (with-temp-buffer
        (elfeed-search-mode)
        (tessera-elfeed-search--enable)
        (let ((inhibit-read-only t))
          (elfeed-search--print-entry
           (tessera-elfeed-search-tests--entry))
          (insert "\n"))
        (goto-char (point-min))
        (tessera-elfeed-search--apply-layout)
        (let ((overlays (overlays-in (point-min) (point-max)))
              (markers (seq-take tessera--current-entry 2)))
          (should overlays)
          (should markers)
          (fundamental-mode)
          (should-not (seq-some #'overlay-buffer overlays))
          (should-not (seq-some #'marker-buffer markers)))))))

(ert-deftest tessera-elfeed-search-enable-failure-restores-state ()
  (let ((elfeed-search-print-entry-function #'ignore)
        (tessera-entry-layout 'single-line)
        (elfeed-search-separator-date-format "%Y")
        (tessera-elfeed-search--navigation-users 0)
        (emulation-mode-map-alists
         (copy-sequence emulation-mode-map-alists))
        (refresh-count 0)
        error-data)
    (cl-letf (((symbol-function 'elfeed-search-update)
               (lambda (&rest _)
                 (let ((inhibit-read-only t))
                   (erase-buffer)
                   (if (= (cl-incf refresh-count) 1)
                       (progn
                         (insert "Partial Tessera row")
                         (error "Refresh failed"))
                     (insert "Native row"))))))
      (with-temp-buffer
        (setq major-mode 'elfeed-search-mode)
        (condition-case error
            (tessera-elfeed-search--enable)
          (error (setq error-data error)))
        (should (equal error-data '(error "Refresh failed")))
        (should (= refresh-count 2))
        (should (equal (buffer-string) "Native row"))
        (should-not tessera-elfeed-search--active)
        (should-not
         tessera-elfeed-search--emulation-map-alist)
        (should (zerop
                 tessera-elfeed-search--navigation-users))
        (should-not
         (memq 'tessera-elfeed-search--emulation-map-alist
               emulation-mode-map-alists))
        (should-not (local-variable-p
                     'elfeed-search-print-entry-function))
        (should-not (local-variable-p 'tessera-entry-layout))
        (should-not
         (local-variable-p
          'elfeed-search-separator-date-format))
        (should-not tessera-elfeed-search--saved-settings)))))

(ert-deftest tessera-elfeed-search-disable-failure-restores-state ()
  (let ((elfeed-search-print-entry-function #'ignore)
        (tessera-entry-layout 'single-line)
        (elfeed-search-separator-date-format "%Y")
        (tessera-elfeed-search--navigation-users 0)
        (emulation-mode-map-alists
         (copy-sequence emulation-mode-map-alists))
        (refresh-count 0)
        error-data)
    (cl-letf (((symbol-function 'elfeed-search-update)
               (lambda (&rest _)
                 (when (= (cl-incf refresh-count) 2)
                   (error "Refresh failed")))))
      (with-temp-buffer
        (setq major-mode 'elfeed-search-mode)
        (tessera-elfeed-search--enable)
        (condition-case error
            (tessera-elfeed-search--disable)
          (error (setq error-data error)))
        (should (equal error-data '(error "Refresh failed")))
        (should (= refresh-count 3))
        (should-not tessera-elfeed-search--active)
        (should-not
         tessera-elfeed-search--emulation-map-alist)
        (should (zerop
                 tessera-elfeed-search--navigation-users))
        (should-not
         (memq 'tessera-elfeed-search--emulation-map-alist
               emulation-mode-map-alists))
        (should-not (local-variable-p
                     'elfeed-search-print-entry-function))
        (should-not (local-variable-p 'tessera-entry-layout))
        (should-not
         (local-variable-p
          'elfeed-search-separator-date-format))
        (should-not tessera-elfeed-search--saved-settings)))))

(ert-deftest tessera-elfeed-search-adapts-native-navigation ()
  (dolist (tessera-month-grouping '(nil t))
    (let ((elfeed-db '(:version 4))
          (elfeed-db-feeds (make-hash-table :test #'equal)))
      (cl-letf (((symbol-function 'elfeed-search-update) #'ignore))
        (with-temp-buffer
          (setq major-mode 'elfeed-search-mode)
          (use-local-map (copy-keymap elfeed-search-mode-map))
          (unwind-protect
              (progn
                (tessera-elfeed-search--enable)
                (let ((entries
                       (tessera-elfeed-search-tests--insert-entries
                        '(9 8 7))))
                  (should (eq (and tessera--month-groups t)
                              tessera-month-grouping))
                  (goto-char (point-min))
                  (forward-char 4)
                  (should (eq (key-binding (kbd "n"))
                              #'tessera-elfeed-search--next))
                  (should (eq (key-binding (kbd "p"))
                              #'tessera-elfeed-search--previous))
                  (let ((position (point)))
                    (call-interactively (key-binding (kbd "p")))
                    (should (= position (point))))
                  (call-interactively (key-binding (kbd "n")))
                  (should
                   (eq (cadr entries)
                       (tessera-elfeed-search--entry-at-point)))
                  (should (= (point) (tessera-entry-point)))
                  (call-interactively (key-binding (kbd "n")))
                  (should
                   (eq (nth 2 entries)
                       (tessera-elfeed-search--entry-at-point)))
                  (let ((position (point)))
                    (call-interactively (key-binding (kbd "n")))
                    (should (= position (point))))
                  (let ((current-prefix-arg 2))
                    (call-interactively (key-binding (kbd "p"))))
                  (should
                   (eq (car entries)
                       (tessera-elfeed-search--entry-at-point)))
                  (let ((position (point))
                        (current-prefix-arg 3))
                    (call-interactively (key-binding (kbd "n")))
                    (should (= position (point))))
                  (dolist (count '(0 1 2 3 4))
                    (goto-char (point-max))
                    (let ((current-prefix-arg count))
                      (call-interactively (key-binding (kbd "p"))))
                    (if (memq count '(0 4))
                        (should (eobp))
                      (should
                       (eq (nth (- 3 count) entries)
                           (tessera-elfeed-search--entry-at-point)))
                      (should (= (point) (tessera-entry-point)))))))
            (tessera-elfeed-search--disable))
          (should (eq (key-binding (kbd "n"))
                      (lookup-key elfeed-search-mode-map (kbd "n"))))
          (should (eq (key-binding (kbd "p"))
                      (lookup-key elfeed-search-mode-map
                                  (kbd "p")))))))))

(ert-deftest tessera-elfeed-search-navigation-keeps-other-window ()
  (save-window-excursion
    (delete-other-windows)
    (let* ((buffer
            (generate-new-buffer " *tessera-elfeed-windows*"))
           (first-window (selected-window))
           (second-window (split-window-right))
           (elfeed-db '(:version 4))
           (elfeed-db-feeds (make-hash-table :test #'equal))
           first-position
           third-position)
      (unwind-protect
          (cl-letf (((symbol-function 'elfeed-search-update)
                     #'ignore))
            (with-current-buffer buffer
              (setq major-mode 'elfeed-search-mode)
              (use-local-map (copy-keymap elfeed-search-mode-map))
              (tessera-elfeed-search--enable)
              (tessera-elfeed-search-tests--insert-entries '(9 8 7))
              (goto-char (point-min))
              (setq first-position (tessera-entry-point))
              (forward-line 2)
              (setq third-position (tessera-entry-point)))
            (set-window-buffer first-window buffer)
            (set-window-buffer second-window buffer)
            (set-window-point second-window third-position)
            (select-window first-window)
            (set-window-point first-window first-position)
            (with-current-buffer buffer
              (goto-char first-position)
              (call-interactively (key-binding (kbd "n"))))
            (should (= (window-point first-window)
                       (with-current-buffer buffer
                         (save-excursion
                           (goto-char (point-min))
                           (forward-line 1)
                           (tessera-entry-point)))))
            (should (= (window-point second-window) third-position))
            (with-current-buffer buffer
              (tessera-elfeed-search--disable)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest tessera-elfeed-search-shares-navigation-integration ()
  (let ((first (generate-new-buffer " *tessera-elfeed-first*"))
        (second (generate-new-buffer " *tessera-elfeed-second*"))
        (tessera-elfeed-search--navigation-users 0)
        (emulation-mode-map-alists
         (copy-sequence emulation-mode-map-alists)))
    (unwind-protect
        (cl-letf (((symbol-function 'elfeed-search-update) #'ignore))
          (dolist (buffer (list first second))
            (with-current-buffer buffer
              (setq major-mode 'elfeed-search-mode)
              (tessera-elfeed-search--enable)))
          (should (= 2 tessera-elfeed-search--navigation-users))
          (with-current-buffer first
            (tessera-elfeed-search--disable))
          (should (= 1 tessera-elfeed-search--navigation-users))
          (should
           (advice-member-p #'tessera-elfeed-search--update-entries
                            'elfeed-search-update-entry))
          (should
           (memq 'tessera-elfeed-search--emulation-map-alist
                 emulation-mode-map-alists))
          (kill-buffer second)
          (should (zerop tessera-elfeed-search--navigation-users))
          (should-not
           (advice-member-p #'tessera-elfeed-search--update-entries
                            'elfeed-search-update-entry))
          (should-not
           (memq 'tessera-elfeed-search--emulation-map-alist
                 emulation-mode-map-alists)))
      (when (buffer-live-p first) (kill-buffer first))
      (when (buffer-live-p second) (kill-buffer second)))))

(ert-deftest tessera-elfeed-rebuilds-layout-on-single-update ()
  (let ((elfeed-db '(:version 4))
        (elfeed-db-feeds (make-hash-table :test #'equal)))
    (with-temp-buffer
      (setq major-mode 'elfeed-search-mode)
      (cl-letf (((symbol-function 'elfeed-search-update) #'ignore))
        (unwind-protect
            (progn
              (tessera-elfeed-search--enable)
              (setq-local
               elfeed-search-entries
               (cl-loop for month in '(9 8 7)
                        collect
                        (let ((entry
                               (tessera-elfeed-search-tests--entry
                                '(unread) nil (cons "feed" month))))
                          (setf (elfeed-entry-date entry)
                                (float-time
                                 (encode-time 0 0 12 1 month 2026)))
                          entry)))
              (dolist (entry elfeed-search-entries)
                (elfeed-search--print-entry entry)
                (insert "\n"))
              (tessera-elfeed-search--apply-layout)
              (goto-char (point-min))
              (forward-line 1)
              (tessera--month-toggle '(2026 9))
              (dolist (tags '(nil (unread long-user-label)))
                (setf (elfeed-entry-tags
                       (car elfeed-search-entries)) tags)
                (let ((syncs 0)
                      (sync (symbol-function 'tessera-month-sync)))
                  (cl-letf (((symbol-function 'tessera-month-sync)
                             (lambda ()
                               (cl-incf syncs)
                               (funcall sync))))
                    (apply #'elfeed-search-update-entry
                           (seq-take elfeed-search-entries 2)))
                  (should (= syncs 1)))
                (should (gethash '(2026 9) tessera--month-folds))
                (should (= (tessera--month-group-unread
                            (car tessera--month-groups))
                           (if tags 1 0)))
                (should
                 (equal
                  (mapcar #'tessera--month-group-start
                          tessera--month-groups)
                  (mapcar #'tessera--month-entry-start
                          (tessera--month-scan-entries))))
                (goto-char (point-max))
                (forward-line -1)
                (tessera-elfeed-search--previous 1)
                (should (= (line-number-at-pos) 2))
                (tessera-elfeed-search--previous 1)
                (should (= (line-number-at-pos) 2)))
              (tessera--month-toggle '(2026 9))
              (goto-char (point-min))
              (dotimes (_ 3)
                (should (tessera-entry-layout-applied-p (point)))
                (forward-line 1)))
          (tessera-elfeed-search--disable))))))

(ert-deftest tessera-elfeed-preserves-native-separator-faces ()
  (with-temp-buffer
    (insert "Entry\n")
    (let* ((overlay (make-overlay (point-min) (point-min)))
           (original
            (propertize "Month\n"
                        'face 'elfeed-search-separator-face)))
      (overlay-put overlay 'category 'elfeed-search-separator)
      (overlay-put overlay 'before-string original)
      (dotimes (_ 2)
        (tessera-elfeed-search--style-separators))
      (should (equal (get-text-property
                      0 'face (overlay-get overlay 'before-string))
                     '(elfeed-search-separator-face default)))
      (tessera-elfeed-search--restore-separators)
      (should (eq original (overlay-get overlay 'before-string)))
      (should-not (overlay-get
                   overlay 'tessera-elfeed-search-separator)))))

(ert-deftest tessera-elfeed-search-restores-date-separators ()
  (let* ((standard
          (get 'elfeed-search-separator-date-format
               'standard-value))
         (native (eval (car standard) t)))
    (cl-letf (((symbol-function 'elfeed-search-update) #'ignore))
      (dolist (tessera-month-grouping '(nil t))
        (pcase-dolist (`(,local ,format)
                       `((nil ,native)
                         (nil "custom separator")
                         (t nil)
                         (t "%Y-%m")))
          (let ((elfeed-search-separator-date-format format))
            (with-temp-buffer
              (setq major-mode 'elfeed-search-mode)
              (when local
                (setq-local elfeed-search-separator-date-format
                            format))
              (tessera-elfeed-search--enable)
              (tessera-elfeed-search--enable)
              (should
               (equal elfeed-search-separator-date-format
                      (unless (and tessera-month-grouping
                                   (equal format native))
                        format)))
              (tessera-elfeed-search--disable)
              (should (eq local
                          (local-variable-p
                           'elfeed-search-separator-date-format)))
              (should (equal elfeed-search-separator-date-format
                             format)))))))))

(ert-deftest tessera-elfeed-search-navigation-skips-folded-months ()
  (let ((elfeed-db '(:version 4))
        (elfeed-db-feeds (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'elfeed-search-update) #'ignore))
      (with-temp-buffer
        (setq major-mode 'elfeed-search-mode)
        (use-local-map (copy-keymap elfeed-search-mode-map))
        (unwind-protect
            (progn
              (tessera-elfeed-search--enable)
              (tessera-elfeed-search-tests--insert-entries '(9 8 7))
              (goto-char (point-min))
              (tessera--month-toggle '(2026 8))
              (cl-letf
                  (((symbol-function 'tessera--month-scan-entries)
                    (lambda () (ert-fail "Navigation rescanned"))))
                (call-interactively (key-binding (kbd "n")))
                (should (= (line-number-at-pos) 3))
                (should (= (point) (tessera-entry-point)))
                (call-interactively (key-binding (kbd "p")))
                (should (= (line-number-at-pos) 1))
                (let ((current-prefix-arg 2)
                      (position (point)))
                  (call-interactively (key-binding (kbd "n")))
                  (should (= (point) position)))
                (goto-char (point-max))
                (call-interactively (key-binding (kbd "p")))
                (should (= (line-number-at-pos) 3))
                (goto-char (point-min))
                (tessera--month-toggle '(2026 7))
                (goto-char (point-max))
                (call-interactively (key-binding (kbd "p")))
                (should (= (line-number-at-pos) 1))
                (should (= (point) (tessera-entry-point)))))
          (tessera-elfeed-search--disable))))))

(provide 'tessera-elfeed-search-tests)
;;; tessera-elfeed-search-tests.el ends here
