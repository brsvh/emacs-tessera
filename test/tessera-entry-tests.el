;;; tessera-entry-tests.el --- Tessera entry tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bingshan Chang <chang@bingshan.org>

;; Author: Bingshan Chang <chang@bingshan.org>
;; Keywords: convenience, mail, news, test

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Test the backend-neutral Tessera entry model and registration
;; protocol.

;;; Code:

(require 'ert)
(require 'tessera-test-support)
(require 'tessera)

(defun tessera-entry-tests--context (object buffer window)
  "Build a test context for OBJECT in BUFFER and WINDOW."
  (make-tessera-entry-context
   :backend 'tessera-entry-tests
   :object object
   :buffer buffer
   :window window
   :metadata '(:fixture t)))

(defun tessera-entry-tests--segment (context)
  "Return title content from CONTEXT."
  (let ((title
         (plist-get (tessera-entry-context-object context)
                    :title)))
    (if (and title
             (plist-get (tessera-entry-context-object context)
                        :local-hover))
        (propertize title 'mouse-face 'link)
      title)))

(defun tessera-entry-tests--date (context)
  "Return date content from CONTEXT."
  (plist-get (tessera-entry-context-object context) :date))

(defun tessera-entry-tests--author (context)
  "Return author content from CONTEXT."
  (plist-get (tessera-entry-context-object context) :author))

(defun tessera-entry-tests--count (context)
  "Return message-count content from CONTEXT."
  (plist-get (tessera-entry-context-object context) :count))

(defun tessera-entry-tests--low (_context)
  "Return low-priority optional test content."
  "LOW")

(defun tessera-entry-tests--high (_context)
  "Return high-priority optional test content."
  "HIGH")

(defun tessera-entry-tests--other-segment (_context)
  "Return alternate content for a test entry."
  "Other")

(defun tessera-entry-tests--invalid-segment (_context)
  "Return invalid test segment content."
  42)

(defun tessera-entry-tests--select (context)
  "Select a glyph variant from CONTEXT."
  (plist-get (tessera-entry-context-object context) :status))

(defun tessera-entry-tests--glyph ()
  "Return a valid test glyph."
  (make-tessera-glyph
   :ascii "*"
   :unicode "●"
   :nerd-icons '(:function nerd-icons-mdicon
                           :name "nf-md-circle")
   :semantic 'accent))

(defun tessera-entry-tests--nerd-icon (_name)
  "Return a stand-in Nerd Icons glyph."
  "N")

(defun tessera-entry-tests--slot ()
  "Return a valid test glyph slot."
  (make-tessera-glyph-slot
   :name 'status
   :selector #'tessera-entry-tests--select
   :width 3
   :align 'center
   :glyphs
   `((unread :glyph ,(tessera-entry-tests--glyph)
             :help-echo "Unread"))))

(defun tessera-entry-tests--layout ()
  "Return a valid test entry layout."
  (make-tessera-entry-layout
   :main-glyph-slots '(status)
   :main-left-segments '((title :grow t
                                :min-width 8
                                :truncate tail))
   :main-right-segments '(date)))

(defun tessera-entry-tests--two-line-layout ()
  "Return a valid two-line test entry layout."
  (make-tessera-entry-layout
   :main-glyph-slots '(status)
   :main-left-segments '((title :grow t
                                :min-width 8
                                :truncate tail))
   :main-right-segments '(date)
   :extra-glyph-slots '((status :reserve t))
   :extra-left-segments '((author :grow t
                                  :min-width 5
                                  :truncate middle))
   :extra-right-segments '(count)))

(defun tessera-entry-tests--register (backend)
  "Register a complete test BACKEND."
  (tessera-entry-register
   backend
   :context #'tessera-entry-tests--context
   :segments
   '((title . tessera-entry-tests--segment)
     (date . tessera-entry-tests--date))
   :glyph-slots (list (tessera-entry-tests--slot))
   :layouts `((single-line . ,(tessera-entry-tests--layout)))))

(defun tessera-entry-tests--register-two-line (backend)
  "Register a complete two-line test BACKEND."
  (tessera-entry-register
   backend
   :context #'tessera-entry-tests--context
   :segments
   '((title . tessera-entry-tests--segment)
     (date . tessera-entry-tests--date)
     (author . tessera-entry-tests--author)
     (count . tessera-entry-tests--count))
   :glyph-slots (list (tessera-entry-tests--slot))
   :layouts `((two-line . ,(tessera-entry-tests--two-line-layout)))))

(defun tessera-entry-tests--property-count (property value string)
  "Count positions where PROPERTY equals VALUE in STRING."
  (cl-loop for position below (length string)
           count (equal (get-text-property position property string)
                        value)))

(ert-deftest tessera-entry-register-replaces-atomically ()
  (let ((backend (make-symbol "tessera-test-backend")))
    (unwind-protect
        (progn
          (tessera-entry-tests--register backend)
          (let ((original (gethash backend tessera--entry-backends)))
            (should-error
             (tessera-entry-register
              backend
              :context #'tessera-entry-tests--context
              :segments
              '((title . tessera-entry-tests--other-segment))
              :glyph-slots (list (tessera-entry-tests--slot))
              :layouts
              `((single-line . ,(tessera-entry-tests--layout)))))
            (should (eq (gethash backend tessera--entry-backends)
                        original)))
          (should (eq (tessera-entry-tests--register backend)
                      backend)))
      (remhash backend tessera--entry-backends))))

(ert-deftest tessera-entry-register-rejects-duplicate-ids ()
  (let ((backend (make-symbol "tessera-test-backend")))
    (should-error
     (tessera-entry-register
      backend
      :context #'tessera-entry-tests--context
      :segments
      '((title . tessera-entry-tests--segment)
        (title . tessera-entry-tests--other-segment))
      :glyph-slots nil
      :layouts
      `((single-line
         . ,(make-tessera-entry-layout
             :main-left-segments '(title))))))))

(ert-deftest tessera-entry-register-rejects-unknown-references ()
  (let ((backend (make-symbol "tessera-test-backend")))
    (should-error
     (tessera-entry-register
      backend
      :context #'tessera-entry-tests--context
      :segments
      '((title . tessera-entry-tests--segment))
      :glyph-slots nil
      :layouts
      `((single-line
         . ,(make-tessera-entry-layout
             :main-left-segments '(missing))))))))

(ert-deftest tessera-entry-register-rejects-invalid-segment-policy ()
  (dolist (reference '((title :grow yes)
                       (title :min-width -1)
                       (title :max-width -1)
                       (title :min-width 2 :max-width 1)
                       (title :truncate side)
                       (title :priority high)
                       (title :optional yes)))
    (should-error
     (tessera--validate-segment-reference reference '(title)))))

(ert-deftest tessera-entry-register-rejects-invalid-slot-policy ()
  (dolist (reference '((status :reserve yes)
                       (status :unknown t)))
    (should-error
     (tessera--validate-glyph-slot-reference
      reference '(status) "Test layout"))))

(ert-deftest tessera-entry-register-rejects-invalid-glyphs ()
  (let* ((backend (make-symbol "tessera-test-backend"))
         (slot (tessera-entry-tests--slot))
         (glyph (plist-get
                 (cdr (car (tessera-glyph-slot-glyphs slot)))
                 :glyph)))
    (setf (tessera-glyph-semantic glyph) 'unknown)
    (should-error
     (tessera-entry-register
      backend
      :context #'tessera-entry-tests--context
      :segments
      '((title . tessera-entry-tests--segment))
      :glyph-slots (list slot)
      :layouts
      `((single-line
         . ,(make-tessera-entry-layout
             :main-glyph-slots '(status)
             :main-left-segments '(title))))))))

(defun tessera-entry-tests--overlay-property-p
    (property value rendered)
  "Check installed overlay strings for PROPERTY VALUE in RENDERED."
  (with-temp-buffer
    (insert rendered "\n")
    (tessera-entry-apply-layout (point-min) (1- (point-max)))
    (cl-some
     (lambda (overlay)
       (cl-some
        (lambda (name)
          (when-let* ((string (overlay-get overlay name)))
            (tessera-tests--property-position
             property value string)))
        '(before-string after-string)))
     (overlays-in (point-min) (point-max)))))

(ert-deftest tessera-entry-render-builds-one-logical-line ()
  (let ((tessera--entry-backends (make-hash-table :test #'eq))
        (tessera-entry-layout 'single-line)
        (tessera-glyph-style 'ascii)
        (tessera-entry-safe-gap 1)
        (tessera-entry-left-padding 2)
        (tessera-entry-right-padding 1)
        (tessera-entry-top-padding 0)
        (tessera-entry-bottom-padding 0))
    (tessera-entry-tests--register 'tessera-entry-tests)
    (let ((rendered
           (tessera-entry-render
            'tessera-entry-tests
            '(:title "Subject" :date "2026" :status unread))))
      (should (equal (substring-no-properties rendered)
                     "*Subject2026"))
      (should (equal (get-text-property 1 'mouse-face rendered)
                     '(tessera-entry-hover-face)))
      (should (tessera-entry-tests--overlay-property-p
               'display '(space :width 2) rendered))
      (should (tessera-entry-tests--overlay-property-p
               'display '(space :align-to (- right 6)) rendered)))))

(ert-deftest tessera-entry-truncates-each-direction ()
  (should (equal (tessera--truncate-string "abcdefgh" 5 'head)
                 "…efgh"))
  (should (equal (tessera--truncate-string "abcdefgh" 5 'middle)
                 "ab…gh"))
  (should (equal (tessera--truncate-string "abcdefgh" 5 'tail)
                 "abcd…")))

(ert-deftest tessera-entry-render-fits-a-narrow-window ()
  (let ((backend 'tessera-entry-tests)
        (tessera-entry-layout 'single-line)
        (tessera-glyph-style 'ascii)
        (tessera-entry-safe-gap 1)
        (tessera-entry-left-padding 1)
        (tessera-entry-right-padding 1)
        (tessera-entry-segment-gap 1)
        (tessera-entry-flex-gap-min-width 1))
    (unwind-protect
        (progn
          (tessera-entry-tests--register backend)
          (cl-letf (((symbol-function 'window-body-width)
                     (lambda (&rest _arguments) 21)))
            (let ((display
                   (tessera-entry-render
                    backend
                    '(:title "A very long subject"
                             :date "2026"
                             :status unread)
                    (selected-window))))
              (should (string-match-p "A very …" display))
              (should-not (string-match-p "long subject" display))
              (should
               (tessera-entry-tests--overlay-property-p
                'display '(space :align-to (- right 6)) display)))))
      (remhash backend tessera--entry-backends))))

(ert-deftest tessera-entry-render-honors-segment-maximum-width ()
  (let* ((backend 'tessera-entry-tests)
         (layout
          (make-tessera-entry-layout
           :main-left-segments
           '((title :max-width 5 :truncate middle))))
         (tessera-entry-layout 'single-line))
    (unwind-protect
        (progn
          (tessera-entry-register
           backend
           :context #'tessera-entry-tests--context
           :segments
           '((title . tessera-entry-tests--segment))
           :glyph-slots nil
           :layouts `((single-line . ,layout)))
          (cl-letf (((symbol-function 'window-body-width)
                     (lambda (&rest _arguments) 80)))
            (let ((display
                   (tessera-entry-render
                    backend
                    '(:title "abcdefgh")
                    (selected-window))))
              (should (string-match-p "ab…gh" display))
              (should-not (string-match-p "abcdefgh" display)))))
      (remhash backend tessera--entry-backends))))

(ert-deftest
    tessera-entry-render-hides-low-priority-optional-segment ()
  (let* ((backend 'tessera-entry-tests)
         (layout
          (make-tessera-entry-layout
           :main-left-segments
           '((title :grow t
                    :min-width 4
                    :truncate tail
                    :priority 100)
             (low :optional t :priority 1)
             (high :optional t :priority 10))))
         (tessera-entry-layout 'single-line)
         (tessera-entry-safe-gap 1)
         (tessera-entry-left-padding 1)
         (tessera-entry-right-padding 1)
         (tessera-entry-segment-gap 1)
         (tessera-entry-flex-gap-min-width 1))
    (unwind-protect
        (progn
          (tessera-entry-register
           backend
           :context #'tessera-entry-tests--context
           :segments
           '((title . tessera-entry-tests--segment)
             (low . tessera-entry-tests--low)
             (high . tessera-entry-tests--high))
           :glyph-slots nil
           :layouts `((single-line . ,layout)))
          (cl-letf (((symbol-function 'window-body-width)
                     (lambda (&rest _arguments) 16)))
            (let ((display
                   (tessera-entry-render
                    backend
                    '(:title "Subject")
                    (selected-window))))
              (should (string-match-p "Subje…" display))
              (should-not (string-match-p "LOW" display))
              (should (string-match-p "HIGH" display)))))
      (remhash backend tessera--entry-backends))))

(ert-deftest tessera-entry-render-uses-unicode-on-graphic-frames ()
  (let ((backend 'tessera-entry-tests)
        (tessera-glyph-style 'unicode))
    (unwind-protect
        (progn
          (tessera-entry-tests--register backend)
          (cl-letf (((symbol-function 'display-graphic-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'char-displayable-p)
                     (lambda (_character) t)))
            (should
             (string-match-p
              "●"
              (tessera-entry-render
               backend
               '(:title "Subject"
                        :date "2026"
                        :status unread))))))
      (remhash backend tessera--entry-backends))))

(ert-deftest tessera-entry-render-falls-back-to-ascii ()
  (let ((backend 'tessera-entry-tests)
        (tessera-glyph-style 'unicode))
    (unwind-protect
        (progn
          (tessera-entry-tests--register backend)
          (cl-letf (((symbol-function 'display-graphic-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'char-displayable-p)
                     (lambda (_character) nil)))
            (should
             (string-match-p
              (regexp-quote "*")
              (tessera-entry-render
               backend
               '(:title "Subject"
                        :date "2026"
                        :status unread))))))
      (remhash backend tessera--entry-backends))))

(ert-deftest tessera-entry-render-uses-nerd-icons ()
  (let ((backend 'tessera-entry-tests)
        (tessera-glyph-style 'nerd-icons))
    (unwind-protect
        (progn
          (tessera-entry-tests--register backend)
          (cl-letf (((symbol-function 'display-graphic-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'char-displayable-p)
                     (lambda (_character) t))
                    ((symbol-function 'featurep)
                     (lambda (feature &optional _subfeature)
                       (eq feature 'nerd-icons)))
                    ((symbol-function 'nerd-icons-mdicon)
                     #'tessera-entry-tests--nerd-icon))
            (should
             (string-match-p
              "N"
              (tessera-entry-render
               backend
               '(:title "Subject"
                        :date "2026"
                        :status unread))))))
      (remhash backend tessera--entry-backends))))

(ert-deftest tessera-glyph-render-supports-segment-glyphs ()
  (let* ((context
          (tessera-entry-tests--context
           '(:title "Subject") (current-buffer) nil))
         (tessera-glyph-style 'ascii)
         (glyph (tessera-entry-tests--glyph))
         (text
          (tessera-glyph-render
           glyph context '(:help-echo "Indicator"))))
    (should (equal text "*"))
    (should (equal (get-text-property 0 'help-echo text)
                   "Indicator"))
    (should (eq (get-text-property
                 0 'tessera-glyph-semantic text)
                'accent))))

(ert-deftest tessera-entry-render-falls-back-from-nerd-icons ()
  (let ((backend 'tessera-entry-tests)
        (tessera-glyph-style 'nerd-icons))
    (unwind-protect
        (progn
          (tessera-entry-tests--register backend)
          (cl-letf (((symbol-function 'display-graphic-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'char-displayable-p)
                     (lambda (_character) t))
                    ((symbol-function 'featurep)
                     (lambda (feature &optional _subfeature)
                       (eq feature 'nerd-icons)))
                    ((symbol-function 'nerd-icons-mdicon)
                     (lambda (_name)
                       (error "Missing Nerd Font"))))
            (should
             (string-match-p
              "●"
              (tessera-entry-render
               backend
               '(:title "Subject"
                        :date "2026"
                        :status unread))))))
      (remhash backend tessera--entry-backends))))

(ert-deftest tessera-entry-render-applies-semantic-color ()
  (let ((backend 'tessera-entry-tests)
        (tessera-glyph-style 'ascii)
        (tessera-glyph-color t))
    (unwind-protect
        (progn
          (tessera-entry-tests--register backend)
          (let* ((display
                  (tessera-entry-render
                   backend
                   '(:title "Subject"
                            :date "2026"
                            :status unread)))
                 (position (string-match (regexp-quote "*") display)))
            (should (eq (get-text-property position 'face display)
                        'tessera-glyph-accent-face))
            (should (eq (get-text-property
                         position 'tessera-glyph-semantic display)
                        'accent))
            (let ((hover (get-text-property
                          position 'mouse-face display)))
              (should (equal (cadr hover)
                             'tessera-entry-hover-face))
              (should (equal
                       (plist-get (car hover) :foreground)
                       (face-attribute 'tessera-glyph-accent-face
                                       :foreground nil 'default))))))
      (remhash backend tessera--entry-backends))))

(ert-deftest tessera-entry-render-supports-monochrome-glyphs ()
  (let ((backend 'tessera-entry-tests)
        (tessera-glyph-style 'ascii)
        (tessera-glyph-color nil))
    (unwind-protect
        (progn
          (tessera-entry-tests--register backend)
          (let* ((display
                  (tessera-entry-render
                   backend
                   '(:title "Subject"
                            :date "2026"
                            :status unread)))
                 (position (string-match (regexp-quote "*") display)))
            (should-not (get-text-property position 'face display))
            (should (eq (get-text-property
                         position 'tessera-glyph-semantic display)
                        'accent))))
      (remhash backend tessera--entry-backends))))

(ert-deftest tessera-entry-render-supports-uniform-glyph-color ()
  (let ((backend 'tessera-entry-tests)
        (tessera-glyph-style 'ascii)
        (tessera-glyph-color "red"))
    (unwind-protect
        (progn
          (tessera-entry-tests--register backend)
          (let* ((display
                  (tessera-entry-render
                   backend
                   '(:title "Subject"
                            :date "2026"
                            :status unread)))
                 (position (string-match (regexp-quote "*") display)))
            (should (equal (get-text-property position 'face display)
                           '(:foreground "red")))))
      (remhash backend tessera--entry-backends))))

(ert-deftest tessera-entry-render-applies-glyph-interaction ()
  (let* ((backend 'tessera-entry-tests)
         (tessera-glyph-style 'ascii)
         (slot (tessera-entry-tests--slot))
         (variant (car (tessera-glyph-slot-glyphs slot))))
    (setcdr variant
            (append (cdr variant)
                    '(:mouse-face mode-line-highlight
                                  :pointer hand
                                  :follow-link t)))
    (unwind-protect
        (progn
          (tessera-entry-register
           backend
           :context #'tessera-entry-tests--context
           :segments
           '((title . tessera-entry-tests--segment)
             (date . tessera-entry-tests--date))
           :glyph-slots (list slot)
           :layouts
           `((single-line . ,(tessera-entry-tests--layout))))
          (let* ((display
                  (tessera-entry-render
                   backend
                   '(:title "Subject"
                            :date "2026"
                            :status unread)))
                 (position (string-match (regexp-quote "*") display)))
            (should (equal (get-text-property
                            position 'mouse-face display)
                           '(mode-line-highlight
                             tessera-glyph-accent-face)))
            (should (eq (get-text-property position 'pointer display)
                        'hand))
            (should (eq (get-text-property
                         position 'follow-link display)
                        t))
            (should (equal (get-text-property
                            position 'help-echo display)
                           "Unread"))))
      (remhash backend tessera--entry-backends))))

(ert-deftest tessera-entry-render-preserves-local-hover ()
  (let ((backend 'tessera-entry-tests))
    (unwind-protect
        (progn
          (tessera-entry-tests--register backend)
          (let* ((display
                  (tessera-entry-render
                   backend
                   '(:title "Subject"
                            :date "2026"
                            :status unread
                            :local-hover t)))
                 (title-position
                  (string-match "Subject" display)))
            (should (equal (get-text-property title-position
                                              'mouse-face display)
                           '(link)))))
      (remhash backend tessera--entry-backends))))

(ert-deftest tessera-entry-render-keeps-an-empty-slot ()
  (let ((backend 'tessera-entry-tests))
    (unwind-protect
        (progn
          (tessera-entry-tests--register backend)
          (let ((display
                 (tessera-entry-render
                  backend
                  '(:title "Subject" :date nil))))
            (should-not (string-match-p "●" display))
            (should (tessera-entry-tests--overlay-property-p
                     'display '(space :width 3) display))
            (should (tessera-entry-tests--overlay-property-p
                     'display '(space :align-to (- right 2))
                     display))))
      (remhash backend tessera--entry-backends))))

(ert-deftest tessera-entry-render-rejects-invalid-segment-output ()
  (let ((backend 'tessera-entry-tests))
    (unwind-protect
        (progn
          (tessera-entry-register
           backend
           :context #'tessera-entry-tests--context
           :segments
           '((title . tessera-entry-tests--invalid-segment))
           :glyph-slots nil
           :layouts
           `((single-line
              . ,(make-tessera-entry-layout
                  :main-left-segments '(title)))))
          (should-error
           (tessera-entry-render backend '(:title "Subject"))))
      (remhash backend tessera--entry-backends))))

(ert-deftest tessera-entry-render-builds-two-visual-lines ()
  (let ((backend 'tessera-entry-tests)
        (tessera-entry-layout 'two-line)
        (tessera-glyph-style 'ascii)
        (tessera-entry-safe-gap 1)
        (tessera-entry-left-padding 1)
        (tessera-entry-right-padding 1)
        (tessera-entry-segment-gap 1)
        (tessera-entry-flex-gap-min-width 1)
        (tessera-entry-top-padding 0)
        (tessera-entry-bottom-padding 0))
    (unwind-protect
        (progn
          (tessera-entry-tests--register-two-line backend)
          (cl-letf (((symbol-function 'window-body-width)
                     (lambda (&rest _arguments) 21)))
            (let* ((display
                    (tessera-entry-render
                     backend
                     '(:title "A very long subject"
                              :date "2026"
                              :author "Alexandria Example"
                              :count "12"
                              :status unread)
                     (selected-window)))
                   (break
                    (tessera-tests--property-position
                     'display "\n" display))
                   (title-position (string-match "A very …" display))
                   (author-position
                    (string-match "Alexa…mple" display)))
              (should-not (string-match-p "[\n\r]" display))
              (should break)
              (should (= (tessera-entry-tests--property-count
                          'display "\n" display)
                         1))
              (should title-position)
              (should author-position)
              (should (< title-position break author-position))
              (should (equal (get-text-property
                              title-position 'mouse-face display)
                             '(tessera-entry-hover-face)))
              (should (equal (get-text-property
                              author-position 'mouse-face display)
                             '(tessera-entry-hover-face)))
              (should (= (cl-count ?* display) 1))
              (should (tessera-entry-tests--overlay-property-p
                       'display '(space :align-to (- right 6))
                       display))
              (should (tessera-entry-tests--overlay-property-p
                       'display '(space :align-to (- right 4))
                       display)))))
      (remhash backend tessera--entry-backends))))

(ert-deftest tessera-entry-aligns-mixed-font-text-by-pixels ()
  (let ((tessera--entry-backends (make-hash-table :test #'eq))
        (tessera-entry-safe-gap 1)
        (tessera-entry-right-padding 1))
    (dolist (tessera-entry-layout '(single-line two-line))
      (if (eq tessera-entry-layout 'single-line)
          (tessera-entry-tests--register 'tessera-entry-tests)
        (tessera-entry-tests--register-two-line 'tessera-entry-tests))
      (cl-letf (((symbol-function 'display-graphic-p)
                 (lambda (&optional _) t))
                ((symbol-function 'frame-char-width)
                 (lambda (&optional _) 10))
                ((symbol-function 'string-pixel-width)
                 (lambda (text)
                   (if (equal text "日本語,café") 73
                     (* 10 (string-width text))))))
        (let ((rendered
               (tessera-entry-render
                'tessera-entry-tests
                '(:title "Subject" :date "日本語,café"
                         :author "Author" :count "12")
                (selected-window))))
          (should (tessera-entry-tests--overlay-property-p
                   'display '(space :align-to (- right (93)))
                   rendered))
          (when (eq tessera-entry-layout 'two-line)
            (should (tessera-entry-tests--overlay-property-p
                     'display '(space :align-to (- right (40)))
                     rendered))))))))

(ert-deftest tessera-entry-reserves-glyph-slot-pixel-width ()
  (let ((backend 'tessera-entry-tests)
        (tessera-entry-layout 'two-line)
        (tessera-glyph-style 'ascii)
        measured)
    (unwind-protect
        (progn
          (tessera-entry-tests--register-two-line backend)
          (cl-letf (((symbol-function 'display-graphic-p)
                     (lambda (&optional _) t))
                    ((symbol-function 'string-pixel-width)
                     (lambda (string)
                       (push string measured)
                       41)))
            (let ((display
                   (tessera-entry-render
                    backend
                    '(:title "Subject"
                             :date "2026"
                             :author "Author"
                             :count "12"
                             :status unread))))
              (should (seq-some
                       (lambda (text) (string-match-p "\\*" text))
                       measured))
              (should
               (tessera-entry-tests--overlay-property-p
                'display '(space :width (41)) display)))))
      (remhash backend tessera--entry-backends))))

(ert-deftest tessera-entry-render-adds-vertical-padding ()
  (let ((tessera--entry-backends (make-hash-table :test #'eq))
        (tessera-entry-layout 'two-line)
        (tessera-entry-top-padding 0.2)
        (tessera-entry-bottom-padding 0.3))
    (tessera-entry-tests--register-two-line 'tessera-entry-tests)
    (let ((rendered
           (tessera-entry-render
            'tessera-entry-tests
            '(:title "Subject" :author "Author" :date "2026"))))
      (should-not (string-match-p "\n" rendered))
      (should (= 1 (tessera-entry-tests--property-count
                    'display "\n" rendered)))
      (should (tessera-entry-tests--overlay-property-p
               'face '((:height 0.2) default) rendered))
      (should (tessera-entry-tests--overlay-property-p
               'face '((:height 0.3) default) rendered))
      (with-temp-buffer
        (insert rendered "\n")
        (let ((original (buffer-string))
              (end (1- (point-max))))
          (tessera-entry-apply-layout (point-min) end)
          (let ((count
                 (length (overlays-in (point-min) (point-max)))))
            (tessera-entry-apply-layout (point-min) end)
            (should (= count (length (overlays-in
                                      (point-min) (point-max))))))
          (should-not (get-text-property end 'face))
          (should-not (get-text-property end 'line-height))
          (should (equal original (buffer-string)))
          (tessera-entry-clear-layout)
          (should-not (overlays-in (point-min) (point-max)))
          (should (equal original (buffer-string))))))))

(ert-deftest tessera-entry-render-budgets-actual-glyph-width ()
  (let ((tessera--entry-backends (make-hash-table :test #'eq))
        (tessera-entry-layout 'single-line)
        (tessera-glyph-style 'ascii)
        (tessera-entry-safe-gap 1)
        (tessera-entry-left-padding 1)
        (tessera-entry-right-padding 1))
    (tessera-entry-tests--register 'tessera-entry-tests)
    (cl-letf (((symbol-function 'display-graphic-p)
               (lambda (&optional _) t))
              ((symbol-function 'frame-char-width)
               (lambda (&optional _) 1))
              ((symbol-function 'string-pixel-width)
               (lambda (_) 5))
              ((symbol-function 'window-body-width)
               (lambda (&rest _) 23)))
      (let ((rendered
             (tessera-entry-render
              'tessera-entry-tests
              '(:title "A very long subject" :date "2026"
                       :status unread)
              (selected-window))))
        (should (equal (substring-no-properties rendered)
                       "*A very …2026"))))))

(defun tessera-entry-tests--insert-current-fixture ()
  "Insert two entries and install their overlay layouts."
  (tessera-entry-tests--register-two-line 'tessera-entry-tests)
  (dolist (title '("First" "Second"))
    (let ((start (point)))
      (insert (tessera-entry-render
               'tessera-entry-tests
               (list :title title :author "Author" :date "2026"
                     :count "12" :status 'unread)))
      (let ((end (point)))
        (insert "\n")
        (tessera-entry-apply-layout start end)))))

(ert-deftest tessera-entry-current-restores-native-properties ()
  (let ((tessera--entry-backends (make-hash-table :test #'eq))
        (tessera-entry-layout 'two-line)
        (tessera-entry-top-padding 0.5)
        (tessera-entry-bottom-padding 0.2))
    (with-temp-buffer
      (tessera-entry-tests--insert-current-fixture)
      (let ((original (buffer-string))
            (count (length (overlays-in (point-min) (point-max)))))
        (set-buffer-modified-p nil)
        (goto-char (point-min))
        (tessera-entry-highlight-current)
        (should-not (buffer-modified-p))
        (should (= count (length (overlays-in (point-min)
                                              (point-max)))))
        (should (memq 'tessera-entry-current-face
                      (get-text-property (point) 'face)))
        (should (equal (get-text-property (point) 'mouse-face)
                       (get-text-property 0 'mouse-face original)))
        (let ((state tessera--current-entry))
          (search-forward "Author")
          (tessera-entry-highlight-current)
          (should (eq state tessera--current-entry)))
        (dolist (overlay (overlays-at (point-min)))
          (when-let* ((string (overlay-get overlay 'before-string)))
            (should-not
             (memq 'tessera-entry-current-face
                   (get-text-property 0 'face string)))
            (should (memq 'tessera-entry-current-face
                          (get-text-property
                           (1- (length string)) 'face string)))))
        (forward-line 1)
        (tessera-entry-highlight-current)
        (should (equal (get-text-property (point-min) 'face)
                       (get-text-property 0 'face original)))
        (should (memq 'tessera-entry-current-face
                      (get-text-property (point) 'face)))
        (tessera-entry-clear-current)
        (should (equal-including-properties original (buffer-string)))
        (should-not (buffer-modified-p))))))

(ert-deftest tessera-entry-current-survives-entry-replacement ()
  (let ((tessera--entry-backends (make-hash-table :test #'eq))
        (tessera-entry-layout 'two-line))
    (with-temp-buffer
      (tessera-entry-tests--insert-current-fixture)
      (goto-char (point-min))
      (tessera-entry-highlight-current)
      ;; Elfeed retains the newline when replacing a single entry.
      (delete-region (point) (line-end-position))
      (tessera-entry-clear-layout (point) (1+ (point)))
      (insert (tessera-entry-render
               'tessera-entry-tests
               '(:title "Updated" :author "Author" :date "2026"
                        :count "12" :status unread)))
      (tessera-entry-apply-layout (point-min) (point))
      (goto-char (point-min))
      (tessera-entry-highlight-current)
      (tessera-entry-clear-current)
      (should-not (get-text-property (line-end-position) 'face))
      (should-not (text-property-not-all
                   (point-min) (point-max)
                   'tessera--current-face nil)))))

(ert-deftest tessera-entry-current-clears-on-empty-line ()
  (let ((tessera--entry-backends (make-hash-table :test #'eq))
        (tessera-entry-layout 'two-line))
    (with-temp-buffer
      (tessera-entry-tests--insert-current-fixture)
      (goto-char (point-min))
      (tessera-entry-highlight-current)
      (goto-char (point-max))
      (tessera-entry-highlight-current)
      (should-not tessera--current-entry)
      (should-not (text-property-not-all
                   (point-min) (point-max)
                   'tessera--current-face nil)))))

(ert-deftest tessera-entry-validates-glyphs-at-registration ()
  (let ((backend 'tessera-entry-tests)
        (tessera-glyph-style 'ascii))
    (unwind-protect
        (progn
          (tessera-entry-tests--register backend)
          (cl-letf (((symbol-function 'tessera--validate-glyph)
                     (lambda (&rest _)
                       (ert-fail "Glyph was revalidated"))))
            (should (string-match-p
                     "Title" (tessera-entry-render
                              backend '(:title "Title"
                                               :status unread)))))
          (should-error
           (tessera-glyph-render nil (make-tessera-entry-context)))
          (should-error
           (tessera-glyph-render (tessera-entry-tests--glyph)
                                 nil :unsupported t)))
      (remhash backend tessera--entry-backends))))

(provide 'tessera-entry-tests)
;;; tessera-entry-tests.el ends here
