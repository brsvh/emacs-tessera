;;; tessera-elfeed-search-tests.el --- Tessera Elfeed search tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bingshan Chang <chang@bingshan.org>

;; Author: Bingshan Chang <chang@bingshan.org>
;; Keywords: convenience, news, test

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Test the initial Tessera adapter for Elfeed search buffers.

;;; Code:

(require 'ert)
(require 'tessera-elfeed)

(require 'elfeed-search)
(require 'tessera-elfeed-search)

(tessera-elfeed-search--register)

(defvar tessera-elfeed-search-tests--root
  (expand-file-name ".."
                    (file-name-directory load-file-name))
  "Repository root for integration tests.")

(defun tessera-elfeed-search-tests--entry
    (&optional tags enclosures)
  "Return an Elfeed entry with TAGS and ENCLOSURES for tests."
  (elfeed-entry--create
   :id '("https://example.invalid/feed" . "entry")
   :title "A useful Elfeed entry"
   :link "https://example.invalid/entry"
   :date 1755475200.0
   :enclosures enclosures
   :tags tags
   :feed-id "https://example.invalid/feed"))

(defun tessera-elfeed-search-tests--property-position
    (property value string)
  "Return the position where PROPERTY equals VALUE in STRING."
  (cl-loop for position below (length string)
           when (equal (get-text-property position property string)
                       value)
           return position))

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
      (let ((rendered
             (buffer-substring (point-min) (point-max))))
        (should
         (tessera-elfeed-search-tests--property-position
          'display "\n" rendered))
        (should (string-match-p "\\*" rendered))
        (should (string-match-p "Example Feed" rendered))
        (should (string-match-p
                 "https://example.invalid/entry" rendered))
        (should (string-match-p "@" rendered))
        (should (string-match-p "(emacs)" rendered))
        (should-not (string-match-p "unread" rendered))
        (should
         (tessera-elfeed-search-tests--property-position
          'help-echo "Enclosure: application/pdf" rendered)))
      (goto-char (point-min))
      (search-forward "A useful Elfeed entry")
      (should (equal (get-text-property
                      (match-beginning 0) 'face)
                     '(tessera-elfeed-search-unread-title-face
                       tessera-elfeed-search-title-face)))
      (should (equal (get-text-property
                      (match-beginning 0) 'follow-link)
                     [elfeed-entry]))
      (search-forward "Example Feed")
      (should (eq (get-text-property
                   (match-beginning 0) 'face)
                  'tessera-elfeed-search-unread-feed-face))
      (search-forward "https://example.invalid/entry")
      (should (eq (get-text-property
                   (match-beginning 0) 'face)
                  'tessera-elfeed-search-unread-url-face))
      (should (eq (face-attribute
                   'tessera-elfeed-search-url-face
                   :slant nil 'default)
                  'italic))
      (search-forward "emacs")
      (should (eq (get-text-property
                   (match-beginning 0) 'face)
                  'tessera-elfeed-search-tag-face))
      (goto-char (point-max))
      (re-search-backward "[[:digit:]]")
      (should (eq (get-text-property (point) 'face)
                  'tessera-elfeed-search-unread-date-face))
      (should (eq (get-text-property (point-min) 'elfeed-entry)
                  entry)))))

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
  (let ((original tessera-elfeed-search-unread-glyph)
        (entry (tessera-elfeed-search-tests--entry '(unread)))
        (tessera-entry-layout 'single-line)
        (tessera-glyph-style 'ascii))
    (unwind-protect
        (progn
          (tessera-elfeed-search--set-glyph
           'tessera-elfeed-search-unread-glyph
           '("U" "◉" nerd-icons-mdicon "nf-md-email" accent))
          (should (string-match-p
                   "U"
                   (tessera-entry-render 'elfeed-search entry))))
      (tessera-elfeed-search--set-glyph
       'tessera-elfeed-search-unread-glyph original))))

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

(ert-deftest tessera-elfeed-registers-only-when-enabled ()
  (let ((tessera--entry-backends (make-hash-table :test #'eq)))
    (load-file
     (expand-file-name "lisp/tessera-elfeed-search.el"
                       tessera-elfeed-search-tests--root))
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
          (should (= count (length (overlays-in
                                    (point-min) (point-max))))))
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
