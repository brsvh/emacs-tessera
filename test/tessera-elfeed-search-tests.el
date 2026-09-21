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

(ert-deftest tessera-elfeed-search-restores-local-printer ()
  (let ((updates 0))
    (cl-letf (((symbol-function 'elfeed-search-update)
               (lambda (&rest _)
                 (setq updates (1+ updates)))))
      (with-temp-buffer
        (setq major-mode 'elfeed-search-mode)
        (setq-local elfeed-search-print-entry-function #'ignore)
        (setq-local tessera-entry-layout 'single-line)
        (tessera-elfeed-search--enable)
        (should (eq elfeed-search-print-entry-function
                    #'tessera-elfeed-search-print-entry))
        (should (eq tessera-entry-layout 'two-line))
        (should (memq
                 #'tessera-elfeed-search--apply-layout
                 elfeed-search-update-hook))
        (should (memq #'tessera-entry-highlight-current
                      post-command-hook))
        (tessera-elfeed-search--disable)
        (should-not (memq #'tessera-entry-highlight-current
                          post-command-hook))
        (should-not tessera--current-entry)
        (should (local-variable-p
                 'elfeed-search-print-entry-function))
        (should (eq elfeed-search-print-entry-function #'ignore))
        (should (local-variable-p 'tessera-entry-layout))
        (should (eq tessera-entry-layout 'single-line))
        (should-not
         (memq #'tessera-elfeed-search--apply-layout
               elfeed-search-update-hook))
        (should (= updates 2))))))

(ert-deftest tessera-elfeed-search-restores-native-highlighting ()
  (dolist (enabled '(t nil))
    (let ((elfeed-search-mode-hook nil))
      (cl-letf (((symbol-function 'elfeed-search-update) #'ignore))
        (with-temp-buffer
          (elfeed-search-mode)
          (let ((inhibit-read-only t)) (insert "Native row\n"))
          (hl-line-mode (if enabled 1 -1))
          (when enabled (hl-line-highlight))
          (tessera-elfeed-search--enable)
          (tessera-elfeed-search--enable)
          (should-not hl-line-mode)
          (should-not (and (overlayp hl-line-overlay)
                           (overlay-buffer hl-line-overlay)))
          (tessera-elfeed-search--disable)
          (tessera-elfeed-search--disable)
          (should (eq (and hl-line-mode t) enabled)))))))

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

(ert-deftest tessera-elfeed-search-restores-global-settings ()
  (let ((elfeed-search-print-entry-function #'ignore)
        (tessera-entry-layout 'single-line))
    (cl-letf (((symbol-function 'elfeed-search-update) #'ignore))
      (with-temp-buffer
        (setq major-mode 'elfeed-search-mode)
        (tessera-elfeed-search--enable)
        (should (eq tessera-entry-layout 'two-line))
        (tessera-elfeed-search--disable)
        (should-not (local-variable-p
                     'elfeed-search-print-entry-function))
        (should (eq elfeed-search-print-entry-function #'ignore))
        (should-not (local-variable-p 'tessera-entry-layout))
        (should (eq tessera-entry-layout 'single-line))
        (should (eq (key-binding (kbd "C-n")) #'next-line))))))

(ert-deftest tessera-elfeed-search-proxies-local-navigation ()
  (let ((moves 0)
        (shows 0)
        (elfeed-db '(:version 4))
        (elfeed-db-feeds (make-hash-table :test #'equal))
        (elfeed-search-remain-on-entry '(show)))
    (cl-letf (((symbol-function 'elfeed-search-update) #'ignore)
              ((symbol-function 'elfeed-show-entry)
               (lambda (_entry)
                 (setq shows (1+ shows)))))
      (with-temp-buffer
        (setq major-mode 'elfeed-search-mode)
        (use-local-map (copy-keymap elfeed-search-mode-map))
        (let* ((next
                (lambda ()
                  (interactive)
                  (setq moves (1+ moves))
                  (ignore-errors (line-move 1))
                  (when-let*
                      ((entry
                        (tessera-elfeed-search--entry-at-point)))
                    (elfeed-search-show-entry entry))))
               (previous
                (lambda ()
                  (interactive)
                  (setq moves (1+ moves))
                  (elfeed-search-show-entry
                   (tessera-elfeed-search--entry-at-point))
                  (ignore-errors (line-move -1))))
               (replacement
                (lambda ()
                  (interactive)
                  (setq moves (1+ moves))
                  (ignore-errors (line-move 1))
                  (when-let*
                      ((entry
                        (tessera-elfeed-search--entry-at-point)))
                    (elfeed-search-show-entry entry))))
               entries)
          (local-set-key (kbd "n") next)
          (local-set-key (kbd "p") previous)
          (dotimes (index 3)
            (let ((entry
                   (tessera-elfeed-search-tests--entry
                    nil nil
                    (cons "https://example.invalid/feed"
                          (format "entry-%d" index)))))
              (push entry entries)
              (let ((start (point)))
                (insert
                 (propertize (format "  Entry %d" index)
                             'elfeed-entry entry))
                (put-text-property
                 (+ start 2) (+ start 3)
                 'tessera-entry-point t)
                (insert "\n"))))
          (setq entries (nreverse entries))
          (goto-char (point-min))
          (forward-char 4)
          (tessera-elfeed-search--enable)
          (let ((point (point)))
            (should-not
             (call-interactively (key-binding (kbd "p"))))
            (should (= (point) point))
            (should (eq (tessera-elfeed-search--entry-at-point)
                        (car entries)))
            (should (zerop moves))
            (should (zerop shows)))
          (call-interactively (key-binding (kbd "n")))
          (should (= moves 1))
          (should (= shows 1))
          (should (= (point) (tessera-entry-point)))
          (should (= (current-column) 2))
          (should (eq (tessera-elfeed-search--entry-at-point)
                      (cadr entries)))
          (local-set-key (kbd "n") replacement)
          (call-interactively (key-binding (kbd "n")))
          (should (= moves 2))
          (should (= shows 2))
          (should (eq (tessera-elfeed-search--entry-at-point)
                      (nth 2 entries)))
          (let ((point (point)))
            (should-not
             (call-interactively (key-binding (kbd "n"))))
            (should (= (point) point))
            (should (= moves 2))
            (should (= shows 2)))
          (call-interactively (key-binding (kbd "p")))
          (should (= moves 3))
          (should (= shows 3))
          (should (= (point) (tessera-entry-point)))
          (should (eq (tessera-elfeed-search--entry-at-point)
                      (cadr entries)))
          (tessera-elfeed-search--disable)
          (should (eq (key-binding (kbd "n")) replacement))
          (should (eq (key-binding (kbd "p")) previous))
          (should-not tessera-elfeed-search--active)
          (should-not
           (memq 'tessera-elfeed-search--emulation-map-alist
                 emulation-mode-map-alists))
          (should-not
           (advice-member-p
            #'tessera-elfeed-search--show-entry
            'elfeed-search-show-entry)))))))

(ert-deftest tessera-elfeed-search-preserves-command-continuity ()
  (let ((elfeed-search-remain-on-entry '(show))
        (last-command nil)
        observed)
    (cl-letf (((symbol-function 'elfeed-search-update) #'ignore))
      (save-window-excursion
        (with-temp-buffer
          (switch-to-buffer (current-buffer))
          (setq major-mode 'elfeed-search-mode)
          (use-local-map (copy-keymap elfeed-search-mode-map))
          (let ((command
                 (lambda ()
                   (interactive)
                   (push last-command observed)
                   (ignore-errors (line-move 1)))))
            (local-set-key (kbd "n") command)
            (dolist (id '(first second third))
              (insert
               (propertize
                "Entry\n" 'elfeed-entry
                (tessera-elfeed-search-tests--entry
                 nil nil (cons "feed" (symbol-name id))))))
            (goto-char (point-min))
            (tessera-elfeed-search--enable)
            (execute-kbd-macro (kbd "n n"))
            (should (equal (nreverse observed)
                           (list nil command)))
            (tessera-elfeed-search--disable)))))))

(ert-deftest tessera-elfeed-search-probes-custom-wrap-target ()
  (with-temp-buffer
    (setq major-mode 'elfeed-search-mode)
    (let* ((first
            (tessera-elfeed-search-tests--entry
             nil nil '("feed" . "first")))
           (second
            (tessera-elfeed-search-tests--entry
             nil nil '("feed" . "second")))
           (calls 0)
           (command
            (lambda ()
              (interactive)
              (setq calls (1+ calls))
              (goto-char (point-max))
              (forward-line -1))))
      (dolist (entry (list first second))
        (insert (propertize "Entry\n" 'elfeed-entry entry)))
      (goto-char (point-min))
      (should
       (tessera-elfeed-search--call-navigation
        command 'previous))
      (should (= calls 1))
      (should (eq second
                  (tessera-elfeed-search--entry-at-point))))))

(ert-deftest tessera-elfeed-search-probes-counted-line-target ()
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (setq major-mode 'elfeed-search-mode)
      (dotimes (index 2)
        (insert
         (propertize
          "Entry\n" 'elfeed-entry
          (tessera-elfeed-search-tests--entry
           nil nil (cons "feed" (number-to-string index))))))
      (goto-char (point-min))
      (let ((point (point))
            (current-prefix-arg 3))
        (should-not
         (tessera-elfeed-search--call-navigation
          #'next-line 'next))
        (should (= point (point)))))))

(ert-deftest tessera-elfeed-search-probes-custom-missing-target ()
  (let ((elfeed-db '(:version 4))
        (elfeed-db-feeds (make-hash-table :test #'equal))
        (elfeed-search-remain-on-entry '(show))
        (hook-calls 0)
        (elfeed-untag-hook
         (list (lambda (&rest _arguments)
                 (setq hook-calls (1+ hook-calls)))))
        (shows 0))
    (cl-letf (((symbol-function 'elfeed-search-update-entry)
               #'ignore)
              ((symbol-function 'elfeed-show-entry)
               (lambda (_entry)
                 (setq shows (1+ shows)))))
      (with-temp-buffer
        (setq major-mode 'elfeed-search-mode)
        (let* ((first
                (tessera-elfeed-search-tests--entry
                 '(unread) nil '("feed" . "first")))
               (second
                (tessera-elfeed-search-tests--entry
                 nil nil '("feed" . "second")))
               (calls 0)
               (command
                (lambda ()
                  (interactive)
                  (setq calls (1+ calls))
                  (elfeed-search-show-entry first))))
          (dolist (entry (list first second))
            (insert (propertize "Entry\n" 'elfeed-entry entry)))
          (goto-char (point-min))
          (should-not
           (tessera-elfeed-search--call-navigation command 'next))
          (should (zerop calls))
          (should (elfeed-tagged-p 'unread first))
          (should (zerop hook-calls))
          (should (zerop shows)))))))

(ert-deftest tessera-elfeed-search-finds-command-below-proxy ()
  (let ((native-map (make-sparse-keymap))
        (remap-map (make-sparse-keymap)))
    (define-key native-map (kbd "p")
                #'elfeed-search-previous-entry)
    (define-key remap-map
                [remap elfeed-search-previous-entry]
                #'previous-line)
    (cl-letf (((symbol-function 'current-active-maps)
               (lambda (&rest _arguments)
                 (list tessera-elfeed-search--navigation-map
                       tessera-elfeed-search--navigation-map
                       remap-map native-map))))
      (should
       (eq (tessera-elfeed-search--command (kbd "p"))
           #'previous-line)))))

(ert-deftest tessera-elfeed-search-preserves-display-call-order ()
  (let ((elfeed-db '(:version 4))
        (elfeed-db-feeds (make-hash-table :test #'equal))
        (elfeed-search-remain-on-entry '(show))
        shown)
    (cl-letf (((symbol-function 'elfeed-search-update) #'ignore)
              ((symbol-function 'elfeed-show-entry)
               (lambda (entry)
                 (push entry shown)
                 t)))
      (with-temp-buffer
        (setq major-mode 'elfeed-search-mode)
        (use-local-map (copy-keymap elfeed-search-mode-map))
        (let ((first
               (tessera-elfeed-search-tests--entry
                nil nil '("feed" . "first")))
              (second
               (tessera-elfeed-search-tests--entry
                nil nil '("feed" . "second"))))
          (dolist (entry (list first second))
            (let ((start (point)))
              (insert (propertize "  Entry" 'elfeed-entry entry))
              (put-text-property
               (+ start 2) (+ start 3) 'tessera-entry-point t)
              (insert "\n")))
          (goto-char (point-min))
          (tessera-elfeed-search--enable)
          (should
           (tessera-elfeed-search--call-navigation
            (lambda ()
              (interactive)
              (elfeed-search-show-entry second)
              (elfeed-search-show-entry first))
            'next))
          (should (eq (tessera-elfeed-search--entry-at-point)
                      first))
          (should (= (point) (tessera-entry-point)))
          (should (equal shown (list first second)))
          (setq shown nil)
          (should
           (tessera-elfeed-search--call-navigation
            (lambda ()
              (interactive)
              (forward-line 1)
              (elfeed-search-show-entry first)
              (elfeed-search-show-entry second))
            'next))
          (should (equal shown (list second first)))
          (should (eq (tessera-elfeed-search--entry-at-point)
                      second))
          (should (= (point) (tessera-entry-point)))
          (tessera-elfeed-search--disable))))))

(ert-deftest tessera-elfeed-search-preserves-custom-display-result ()
  (let* ((shown nil)
         (display
          (lambda (link)
            (setq shown link)
            'custom-result))
         (feed
          (elfeed-feed--create
           :id "https://example.invalid/feed"
           :meta (list :show-entry display)))
         (elfeed-db '(:version 4))
         (elfeed-db-feeds (make-hash-table :test #'equal))
         (elfeed-search-remain-on-entry '(show))
         (first
          (tessera-elfeed-search-tests--entry
           nil nil '("feed" . "first")))
         (second
          (tessera-elfeed-search-tests--entry
           nil nil '("feed" . "second"))))
    (puthash "https://example.invalid/feed" feed
             elfeed-db-feeds)
    (with-temp-buffer
      (setq major-mode 'elfeed-search-mode)
      (dolist (entry (list first second))
        (insert (propertize "  Entry\n" 'elfeed-entry entry)))
      (goto-char (point-min))
      (should-not
       (tessera-elfeed-search--call-navigation
        (lambda ()
          (interactive)
          (elfeed-search-show-entry first))
        'previous))
      (should-not shown)
      (should
       (eq
        'custom-result
        (tessera-elfeed-search--call-navigation
         (lambda ()
           (interactive)
           (forward-line 1)
           (elfeed-search-show-entry second))
         'next)))
      (should (equal shown "https://example.invalid/entry")))))

(ert-deftest tessera-elfeed-search-preserves-show-result-in-command ()
  (let* ((display (lambda (_link) 'custom-result))
         (feed
          (elfeed-feed--create
           :id "https://example.invalid/feed"
           :meta (list :show-entry display)))
         (elfeed-db '(:version 4))
         (elfeed-db-feeds (make-hash-table :test #'equal))
         (elfeed-search-remain-on-entry '(show))
         (first
          (tessera-elfeed-search-tests--entry
           nil nil '("feed" . "first")))
         (second
          (tessera-elfeed-search-tests--entry
           nil nil '("feed" . "second")))
         observed)
    (puthash "https://example.invalid/feed" feed
             elfeed-db-feeds)
    (with-temp-buffer
      (setq major-mode 'elfeed-search-mode)
      (dolist (entry (list first second))
        (insert (propertize "  Entry\n" 'elfeed-entry entry)))
      (goto-char (point-min))
      (tessera-elfeed-search--call-navigation
       (lambda ()
         (interactive)
         (setq observed
               (elfeed-search-show-entry first))
         (when observed
           (forward-line 1)))
       'next)
      (should (eq observed 'custom-result))
      (should (eq second
                  (tessera-elfeed-search--entry-at-point))))))

(ert-deftest tessera-elfeed-search-rolls-back-display-errors ()
  (let ((elfeed-db '(:version 4))
        (elfeed-db-feeds (make-hash-table :test #'equal))
        (elfeed-search-remain-on-entry '(show))
        (first
         (tessera-elfeed-search-tests--entry
          nil nil '("feed" . "first")))
        (second
         (tessera-elfeed-search-tests--entry
          nil nil '("feed" . "second"))))
    (with-temp-buffer
      (setq major-mode 'elfeed-search-mode)
      (dolist (entry (list first second))
        (insert (propertize "  Entry\n" 'elfeed-entry entry)))
      (goto-char (point-min))
      (let ((point (point)))
        (cl-letf (((symbol-function 'elfeed-search-update) #'ignore)
                  ((symbol-function 'elfeed-show-entry)
                   (lambda (&rest _)
                     (error "Display failed"))))
          (should-error
           (tessera-elfeed-search--call-navigation
            (lambda ()
              (interactive)
              (forward-line 1)
              (elfeed-search-show-entry second))
            'next)
           :type 'error))
        (should (= point (point)))
        (should (eq first
                    (tessera-elfeed-search--entry-at-point)))))))

(ert-deftest tessera-elfeed-search-runs-native-show-effects-in-command
    ()
  (let ((elfeed-db '(:version 4))
        (elfeed-db-feeds (make-hash-table :test #'equal))
        (elfeed-search-remain-on-entry '(show))
        observed
        shown)
    (cl-letf (((symbol-function 'elfeed-search-update-entry) #'ignore)
              ((symbol-function 'elfeed-show-entry)
               (lambda (entry)
                 (push entry shown))))
      (with-temp-buffer
        (setq major-mode 'elfeed-search-mode)
        (let ((first
               (tessera-elfeed-search-tests--entry
                '(unread) nil '("feed" . "first")))
              (second
               (tessera-elfeed-search-tests--entry
                '(unread) nil '("feed" . "second"))))
          (dolist (entry (list first second))
            (insert (propertize "  Entry\n" 'elfeed-entry entry)))
          (goto-char (point-min))
          (tessera-elfeed-search--call-navigation
           (lambda ()
             (interactive)
             (forward-line 1)
             (elfeed-search-show-entry second)
             (setq observed
                   (not (elfeed-tagged-p 'unread second))))
           'next)
          (should observed)
          (should (equal shown (list second))))))))

(ert-deftest tessera-elfeed-search-blocks-boundary-show-effects ()
  (let ((elfeed-db '(:version 4))
        (elfeed-db-feeds (make-hash-table :test #'equal))
        (elfeed-search-remain-on-entry '(show))
        (hook-calls 0)
        (elfeed-untag-hook
         (list (lambda (&rest _arguments)
                 (setq hook-calls (1+ hook-calls)))))
        shown)
    (cl-letf (((symbol-function 'elfeed-search-update) #'ignore)
              ((symbol-function 'elfeed-search-update-entry) #'ignore)
              ((symbol-function 'elfeed-show-entry)
               (lambda (entry)
                 (push entry shown))))
      (with-temp-buffer
        (setq major-mode 'elfeed-search-mode)
        (let ((entry
               (tessera-elfeed-search-tests--entry '(unread))))
          (insert (propertize "  Entry\n" 'elfeed-entry entry))
          (goto-char (point-min))
          (set-mark (point-min))
          (setq mark-ring (list (copy-marker (point-max))))
          (let ((ring (mapcar #'marker-position mark-ring)))
            (tessera-elfeed-search--call-navigation
             (lambda ()
               (interactive)
               (elfeed-search-show-entry entry)
               (push-mark (point) t))
             'previous)
            (should (equal ring
                           (mapcar #'marker-position mark-ring))))
          (should (elfeed-tagged-p 'unread entry))
          (should (zerop hook-calls))
          (should-not shown))))))

(ert-deftest tessera-elfeed-search-commits-native-untag-hook ()
  (let ((elfeed-db '(:version 4))
        (elfeed-db-feeds (make-hash-table :test #'equal))
        (elfeed-search-remain-on-entry '(show))
        calls
        observed-tags
        shown)
    (cl-letf (((symbol-function 'elfeed-search-update-entry)
               #'ignore)
              ((symbol-function 'elfeed-show-entry)
               (lambda (entry)
                 (push entry shown))))
      (with-temp-buffer
        (setq major-mode 'elfeed-search-mode)
        (let* ((first
                (tessera-elfeed-search-tests--entry
                 '(unread) nil '("feed" . "first")))
               (second
                (tessera-elfeed-search-tests--entry
                 '(unread) nil '("feed" . "second")))
               (elfeed-untag-hook
                (list
                 (lambda (entries tags)
                   (setq observed-tags
                         (copy-sequence
                          (elfeed-entry-tags (car entries))))
                   (setf (elfeed-entry-tags (car entries))
                         '(unread hook-added))
                   (push (list entries tags) calls)))))
          (dolist (entry (list first second))
            (insert (propertize "  Entry\n" 'elfeed-entry entry)))
          (goto-char (point-min))
          (tessera-elfeed-search--call-navigation
           (lambda ()
             (interactive)
             (forward-line 1)
             (elfeed-search-show-entry second))
           'next)
          (should (equal observed-tags '(unread)))
          (should (equal (elfeed-entry-tags second)
                         '(hook-added)))
          (should (equal shown (list second)))
          (should (equal calls
                         (list (list (list second) '(unread))))))))))

(ert-deftest tessera-elfeed-search-preserves-origin-hook-timing ()
  (let ((elfeed-db '(:version 4))
        (elfeed-db-feeds (make-hash-table :test #'equal))
        (elfeed-search-remain-on-entry '(show))
        observed-tags
        shown)
    (cl-letf (((symbol-function 'elfeed-search-update-entry)
               #'ignore)
              ((symbol-function 'elfeed-show-entry)
               (lambda (entry)
                 (push entry shown))))
      (with-temp-buffer
        (setq major-mode 'elfeed-search-mode)
        (let* ((first
                (tessera-elfeed-search-tests--entry
                 '(unread) nil '("feed" . "first")))
               (second
                (tessera-elfeed-search-tests--entry
                 nil nil '("feed" . "second")))
               (elfeed-untag-hook
                (list
                 (lambda (entries _tags)
                   (setq observed-tags
                         (copy-sequence
                          (elfeed-entry-tags (car entries))))))))
          (dolist (entry (list first second))
            (insert (propertize "  Entry\n" 'elfeed-entry entry)))
          (goto-char (point-min))
          (tessera-elfeed-search--call-navigation
           (lambda ()
             (interactive)
             (elfeed-search-show-entry first)
             (elfeed-tag first 'later)
             (forward-line 1))
           'next)
          (should (equal observed-tags '(unread)))
          (should (equal (elfeed-entry-tags first) '(later)))
          (should (equal shown (list first)))
          (should (eq second
                      (tessera-elfeed-search--entry-at-point))))))))

(ert-deftest tessera-elfeed-search-kill-releases-navigation ()
  (let ((buffer (generate-new-buffer " *tessera-elfeed-kill*")))
    (cl-letf (((symbol-function 'elfeed-search-update) #'ignore))
      (with-current-buffer buffer
        (setq major-mode 'elfeed-search-mode)
        (tessera-elfeed-search--enable)
        (should-not
         (advice-member-p
          #'tessera-elfeed-search--show-entry
          'elfeed-search-show-entry))
        (should
         (memq 'tessera-elfeed-search--emulation-map-alist
               emulation-mode-map-alists))
        (kill-buffer buffer)))
    (should-not (buffer-live-p buffer))
    (should-not
     (advice-member-p
      #'tessera-elfeed-search--show-entry
      'elfeed-search-show-entry))
    (should-not
     (memq 'tessera-elfeed-search--emulation-map-alist
           emulation-mode-map-alists))))

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
           (memq 'tessera-elfeed-search--emulation-map-alist
                 emulation-mode-map-alists))
          (kill-buffer second)
          (should (zerop tessera-elfeed-search--navigation-users))
          (should-not
           (memq 'tessera-elfeed-search--emulation-map-alist
                 emulation-mode-map-alists)))
      (when (buffer-live-p first) (kill-buffer first))
      (when (buffer-live-p second) (kill-buffer second)))))

(ert-deftest tessera-elfeed-registers-only-when-enabled ()
  (let ((tessera--entry-backends (make-hash-table :test #'eq)))
    (load-file (symbol-file 'tessera-elfeed-search--register 'defun))
    (should-not (gethash 'elfeed-search tessera--entry-backends))
    (cl-letf (((symbol-function 'elfeed-search-update) #'ignore))
      (with-temp-buffer
        (setq major-mode 'elfeed-search-mode)
        (tessera-elfeed--enable-search)
        (should (gethash 'elfeed-search tessera--entry-backends))
        (tessera-elfeed-search--disable)))))

(ert-deftest tessera-elfeed-rebuilds-layout-on-single-update ()
  (let ((elfeed-search-print-entry-function
         #'tessera-elfeed-search-print-entry)
        (tessera-entry-layout 'two-line)
        (tessera-entry-top-padding 0.2)
        (tessera-entry-bottom-padding 0.2)
        (elfeed-db '(:version 4))
        (elfeed-db-feeds (make-hash-table :test #'equal))
        (entry (tessera-elfeed-search-tests--entry)))
    (with-temp-buffer
      (setq tessera-elfeed-search--active t)
      (elfeed-search--print-entry entry)
      (insert "\n")
      (tessera-elfeed-search--apply-layout)
      (let ((count (length (overlays-in (point-min) (point-max)))))
        (dotimes (_ 3)
          (goto-char (point-min))
          (delete-region (point) (line-end-position))
          (elfeed-search--print-entry entry)
          (should (= count
                     (length (overlays-in (point-min) (point-max))))))
        (should (= 1 (count-lines (point-min) (point-max))))
        (should (eq entry (get-text-property
                           (point-min) 'elfeed-entry)))))))

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
  (let ((elfeed-search-separator-date-format "%b %Y"))
    (cl-letf (((symbol-function 'elfeed-search-update) #'ignore))
      (dolist (local '(nil t))
        (dolist (format '(nil "%Y-%m"))
          (with-temp-buffer
            (setq major-mode 'elfeed-search-mode)
            (when local
              (setq-local elfeed-search-separator-date-format format))
            (tessera-elfeed-search--enable)
            (tessera-elfeed-search--enable)
            (should-not elfeed-search-separator-date-format)
            (tessera-elfeed-search--disable)
            (should (eq local
                        (local-variable-p
                         'elfeed-search-separator-date-format)))
            (should (equal elfeed-search-separator-date-format
                           (if local format "%b %Y")))))))))

(provide 'tessera-elfeed-search-tests)
;;; tessera-elfeed-search-tests.el ends here
