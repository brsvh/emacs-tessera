;;; tessera-month-tests.el --- Month grouping tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bingshan Chang <chang@bingshan.org>

;; Author: Bingshan Chang <chang@bingshan.org>
;; Keywords: convenience, mail, news, test

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Test backend-neutral month grouping, folding, and warnings.

;;; Code:

(require 'ert)
(require 'tessera)
(require 'tessera-test-support)

(defun tessera-month-tests--context (object buffer window)
  "Return a test entry context for OBJECT in BUFFER and WINDOW."
  (make-tessera-entry-context
   :backend 'tessera-month-tests
   :object object
   :buffer buffer
   :window window
   :thread (plist-get object :thread)))

(defun tessera-month-tests--title (context)
  "Return CONTEXT's test title."
  (plist-get (tessera-entry-context-object context) :title))

(defun tessera-month-tests--date (context)
  "Return CONTEXT's test date."
  (plist-get (tessera-entry-context-object context) :time))

(defun tessera-month-tests--unread-p (context)
  "Return CONTEXT's test unread state."
  (plist-get (tessera-entry-context-object context) :unread))

(defun tessera-month-tests--glyph (state _context)
  "Return a test glyph for month count STATE."
  (make-tessera-glyph
   :ascii (if (eq state 'unread) "U" "R")
   :unicode nil
   :face nil))

(defun tessera-month-tests--warning-segment (_context)
  "Return the test warning segment."
  'title)

(defun tessera-month-tests--register ()
  "Register the month test backend."
  (tessera-entry-register
   'tessera-month-tests
   :context #'tessera-month-tests--context
   :month-date #'tessera-month-tests--date
   :month-unread-p #'tessera-month-tests--unread-p
   :month-glyph #'tessera-month-tests--glyph
   :month-warning-segment
   #'tessera-month-tests--warning-segment
   :segments '((title . tessera-month-tests--title))
   :glyph-slots nil
   :layouts
   `((single-line .
                  ,(make-tessera-entry-layout
                    :main-left-segments
                    '((title :grow t
                             :min-width 1
                             :truncate tail
                             :point t)))))))

(defun tessera-month-tests--time (year month day)
  "Return a local test time for YEAR, MONTH, and DAY."
  (encode-time 0 0 12 day month year))

(defun tessera-month-tests--cross-month-thread ()
  "Return fresh entries with a March root and April reply in 2026."
  (let ((root
         (make-tessera-thread-context
          :id 1 :root 1 :first t :total 2 :unread 1))
        (child
         (make-tessera-thread-context
          :id 2 :parent 1 :root 1 :last t
          :total 2 :unread 1)))
    (list
     (list :title "Root"
           :time (tessera-month-tests--time 2026 3 2)
           :thread root)
     (list :title "Child"
           :time (tessera-month-tests--time 2026 4 2)
           :thread child))))

(defun tessera-month-tests--pixel-width (string &optional _buffer)
  "Return a deterministic simulated pixel width for STRING."
  (let ((position 0)
        (width 0))
    (while (< position (length string))
      (let* ((display (get-text-property position 'display string))
             (space-width
              (and (eq (car-safe display) 'space)
                   (plist-get (cdr display) :width)))
             (face (get-text-property position 'face string))
             (title
              (or (eq face 'tessera-month-title-face)
                  (and (listp face)
                       (memq 'tessera-month-title-face face)))))
        (cl-incf width
                 (cond
                  ((consp space-width) (car space-width))
                  ((numberp space-width) (* 10 space-width))
                  (title 13)
                  (t 10))))
      (cl-incf position))
    width))

(defun tessera-month-tests--string-face-p (string face)
  "Return non-nil when STRING contains FACE."
  (and string
       (let ((position 0)
             found)
         (while (and (< position (length string)) (not found))
           (let ((value (get-text-property position 'face string)))
             (setq found
                   (or (eq value face)
                       (and (listp value) (memq face value))))
             (setq position
                   (next-single-property-change
                    position 'face string (length string)))))
         found)))

(defun tessera-month-tests--overlay-face-p (start end face)
  "Return non-nil when an overlay string from START to END has FACE."
  (cl-some
   (lambda (overlay)
     (or (tessera-month-tests--string-face-p
          (overlay-get overlay 'before-string) face)
         (tessera-month-tests--string-face-p
          (overlay-get overlay 'after-string) face)))
   (overlays-in start end)))

(ert-deftest tessera-month-default-heading-appearance ()
  (should (= (default-value 'tessera-month-left-padding) 2))
  (should (= (default-value 'tessera-month-bottom-padding) 0.5))
  (should
   (equal (face-attribute 'tessera-month-face :inherit)
          '(font-lock-keyword-face default)))
  (should
   (eq (face-attribute 'tessera-month-face :weight nil t)
       'bold)))

(defun tessera-month-tests--string-month-keys (string)
  "Return month keys in their displayed order in STRING."
  (let ((position 0)
        keys)
    (while (< position (length string))
      (let ((key (get-text-property
                  position 'tessera-month-key string)))
        (when (and key (not (equal key (car keys))))
          (push key keys)))
      (setq position
            (next-single-property-change
             position 'tessera-month-key string
             (length string))))
    (nreverse keys)))

(defun tessera-month-tests--header-for-key (key)
  "Return the installed month header containing KEY."
  (cl-find-if
   (lambda (overlay)
     (and (eq (overlay-get overlay 'tessera-month-overlay)
              'header)
          (member key
                  (tessera-month-tests--string-month-keys
                   (overlay-get overlay 'before-string)))))
   (overlays-in (point-min) (point-max))))

(defun tessera-month-tests--insert (objects)
  "Insert and lay out test OBJECTS."
  (dolist (object objects)
    (let ((start (point)))
      (insert (tessera-entry-render
               'tessera-month-tests object))
      (let ((end (point)))
        (insert "\n")
        (tessera-entry-apply-layout start end)))))

(defmacro tessera-month-tests--with-buffer (objects &rest body)
  "Evaluate BODY in a month-enabled buffer containing OBJECTS."
  (declare (indent 1) (debug t))
  `(let ((tessera--entry-backends
          (make-hash-table :test #'eq))
         (tessera-glyph-style 'ascii)
         (tessera-entry-layout 'single-line)
         (tessera-entry-top-padding 0)
         (tessera-entry-bottom-padding 0)
         (tessera-month-top-padding 0)
         (tessera-month-bottom-padding 0))
     (with-temp-buffer
       (tessera-month-tests--register)
       (setq-local tessera--month-enabled t
                   tessera--month-thread-date 'latest)
       (tessera-month-tests--insert ,objects)
       (goto-char (point-min))
       (tessera-month-sync)
       ,@body)))

(ert-deftest tessera-month-navigation-enters-nearest-visible-entry ()
  (with-temp-buffer
    (insert (make-string 30 ?x))
    (setq tessera--month-visible-entries
          (vconcat
           (cl-loop for start from 5 to 25 by 10
                    collect (make-tessera--month-entry
                             :start start :end (+ start 4)))))
    (pcase-dolist (`(,position ,direction ,count ,expected)
                   '((1 1 1 5) (1 -1 1 nil)
                     (7 1 1 15) (7 -1 1 nil)
                     (11 1 1 15) (11 -1 1 5)
                     (11 1 2 25) (11 -1 2 nil)
                     (16 1 1 25) (16 -1 1 5)
                     (21 1 1 25) (21 -1 1 15)
                     (29 1 1 nil) (29 -1 1 25)
                     (31 -1 2 15) (31 -1 4 nil)))
      (goto-char position)
      (should (eql (tessera--month-visible-entry-position
                    direction count)
                   expected)))
    (narrow-to-region 11 29)
    (goto-char (point-min))
    (should-not (tessera--month-visible-entry-position -1 1))
    (should (= (tessera--month-visible-entry-position 1 1) 15))
    (goto-char (point-max))
    (should (= (tessera--month-visible-entry-position -1 1) 25))
    (should-not (tessera--month-visible-entry-position -1 3))))

(ert-deftest tessera-month-groups-contiguous-entries ()
  (tessera-month-tests--with-buffer
      (list
       (list :title "April unread"
             :time (tessera-month-tests--time 2026 4 20)
             :unread t)
       (list :title "April read"
             :time (tessera-month-tests--time 2026 4 2))
       (list :title "March"
             :time (tessera-month-tests--time 2026 3 10)))
    (should (equal (mapcar #'tessera--month-group-key
                           tessera--month-groups)
                   '((2026 4) (2026 3))))
    (should-not
     (gethash '(2026 4) tessera--month-folds))
    (should (= (length
                (cl-remove-if-not
                 (lambda (overlay)
                   (eq (overlay-get overlay
                                    'tessera-month-overlay)
                       'header))
                 (overlays-in (point-min) (point-max))))
               2))))

(ert-deftest tessera-month-statistics-face-overrides-base ()
  (let* ((text
          (tessera--month-style-text
           (concat (propertize "U" 'face 'tessera-glyph-accent-face)
                   " 1")
           'tessera-month-statistics-face)))
    (should (equal (get-text-property 0 'face text)
                   '(tessera-glyph-accent-face
                     tessera-month-statistics-face
                     tessera-month-face)))
    (should (equal (get-text-property 2 'face text)
                   '(tessera-month-statistics-face
                     tessera-month-face)))))

(ert-deftest tessera-month-fold-shows-counts-and-hides-entries ()
  (tessera-month-tests--with-buffer
      (list
       (list :title "April unread"
             :time (tessera-month-tests--time 2026 4 20)
             :unread t)
       (list :title "April read"
             :time (tessera-month-tests--time 2026 4 2))
       (list :title "March"
             :time (tessera-month-tests--time 2026 3 10)))
    (goto-char (tessera--month-group-start
                (cadr tessera--month-groups)))
    (tessera--month-toggle '(2026 4))
    (should (gethash '(2026 4) tessera--month-folds))
    (let* ((group (car tessera--month-groups))
           (next (cadr tessera--month-groups))
           (fold
            (cl-find-if
             (lambda (overlay)
               (eq (overlay-get overlay
                                'tessera-month-overlay)
                   'fold))
             (overlays-in
              (tessera--month-group-start group)
              (1+ (tessera--month-group-start group)))))
           (header
            (tessera-month-tests--header-for-key '(2026 4)))
           (text (overlay-get header 'before-string)))
      (should (= (overlay-end fold)
                 (tessera--month-group-end group)))
      (should (= (overlay-start header)
                 (tessera--month-group-start next)))
      (should (string-match-p "U 1.*R 1" text))
      (let ((position
             (cl-loop for position below (length text)
                      when
                      (equal (get-text-property
                              position 'tessera-month-key text)
                             '(2026 4))
                      return position)))
        (should position)
        (should
         (equal (get-text-property position 'follow-link text)
                [mouse-1])))
      (should-not
       (tessera-month-entry-visible-p
        (tessera--month-group-start group))))))

(ert-deftest tessera-month-toggle-restores-highlight-and-point ()
  (tessera-month-tests--with-buffer
      (list
       (list :title "April"
             :time (tessera-month-tests--time 2026 4 20))
       (list :title "March"
             :time (tessera-month-tests--time 2026 3 20)))
    (let* ((april (car tessera--month-groups))
           (april-start (tessera--month-group-start april))
           (april-end
            (save-excursion
              (goto-char april-start)
              (line-end-position))))
      (goto-char april-start)
      (goto-char (tessera-entry-point))
      (tessera-entry-highlight-current)
      (should
       (tessera-month-tests--overlay-face-p
        april-start (line-end-position)
        'tessera-entry-current-face))
      (tessera--month-toggle '(2026 4))
      (tessera-entry-highlight-current)
      (tessera--month-toggle '(2026 4))
      (should (= (point) (tessera-entry-point)))
      (should-not
       (text-property-not-all
        april-start april-end
        'tessera--current-face nil))
      (should-not
       (tessera-month-tests--overlay-face-p
        april-start april-end
        'tessera-entry-current-face)))))

(ert-deftest tessera-month-mouse-toggle-deactivates-region ()
  (should
   (eq (lookup-key tessera--month-header-map [down-mouse-1])
       #'tessera--month-mouse-toggle))
  (should-not
   (lookup-key tessera--month-header-map [drag-mouse-1]))
  (tessera-month-tests--with-buffer
      (list
       (list :title "April"
             :time (tessera-month-tests--time 2026 4 20))
       (list :title "March"
             :time (tessera-month-tests--time 2026 3 20)))
    (let ((buffer (current-buffer)))
      (save-window-excursion
        (let ((window (selected-window)))
          (set-window-buffer window buffer)
          (goto-char (point-min))
          (setq-local transient-mark-mode t)
          (push-mark (point-max) t t)
          (should mark-active)
          (tessera-tests--click-month '(2026 4))
          (should (gethash '(2026 4) tessera--month-folds))
          (tessera-tests--click-month '(2026 4))
          (should-not (gethash '(2026 4) tessera--month-folds))
          (should-not mark-active)
          (should deactivate-mark))))))

(ert-deftest tessera-month-consecutive-folds-share-safe-anchor ()
  (tessera-month-tests--with-buffer
      (list
       (list :title "May"
             :time (tessera-month-tests--time 2026 5 20))
       (list :title "April"
             :time (tessera-month-tests--time 2026 4 20))
       (list :title "March"
             :time (tessera-month-tests--time 2026 3 20)))
    (goto-char (tessera--month-group-start
                (car (last tessera--month-groups))))
    (tessera--month-toggle '(2026 5))
    (tessera--month-toggle '(2026 4))
    (let* ((target (tessera--month-group-start
                    (car (last tessera--month-groups))))
           (headers
            (cl-remove-if-not
             (lambda (overlay)
               (eq (overlay-get overlay 'tessera-month-overlay)
                   'header))
             (overlays-in target (1+ target))))
           (header (car headers)))
      (should (= (length headers) 1))
      (should (= (overlay-get header 'priority) 0))
      (should
       (equal
        (tessera-month-tests--string-month-keys
         (overlay-get header 'before-string))
        '((2026 5) (2026 4) (2026 3)))))))

(ert-deftest tessera-month-folded-headings-reuse-visible-anchor ()
  (tessera-month-tests--with-buffer
      (cl-loop for year from 1900 below 2000
               collect (list :title "Entry"
                             :time (tessera-month-tests--time
                                    year 1 1)))
    (dolist (group (butlast tessera--month-groups))
      (puthash (tessera--month-group-key group)
               t tessera--month-folds))
    (let ((anchor (symbol-function 'tessera--month-group-anchor))
          (calls 0))
      (cl-letf (((symbol-function 'tessera--month-group-anchor)
                 (lambda (group)
                   (cl-incf calls)
                   (funcall anchor group))))
        (tessera--month-redisplay))
      (should (= calls 1)))
    (let ((header (tessera-month-tests--header-for-key '(1900 1))))
      (should
       (equal (tessera-month-tests--string-month-keys
               (overlay-get header 'before-string))
              (mapcar #'tessera--month-group-key
                      tessera--month-groups))))))

(ert-deftest tessera-month-heading-skips-native-invisible-prefix ()
  (tessera-month-tests--with-buffer
      (list
       (list :title "April"
             :time (tessera-month-tests--time 2026 4 20))
       (list :title "March"
             :time (tessera-month-tests--time 2026 3 20)))
    (let* ((next (cadr tessera--month-groups))
           (start (tessera--month-group-start next)))
      (setq-local buffer-invisibility-spec t)
      (put-text-property start (+ start 3) 'invisible t)
      (tessera-month-sync)
      (goto-char (+ start 3))
      (tessera--month-toggle '(2026 4))
      (let ((header
             (tessera-month-tests--header-for-key '(2026 4))))
        (should (= (overlay-start header) (+ start 3)))
        (should-not (invisible-p (overlay-start header)))))))

(ert-deftest tessera-month-pixel-heading-fits-window-width ()
  (tessera-month-tests--with-buffer
      (list
       (list :title "April unread"
             :time (tessera-month-tests--time 2026 4 20)
             :unread t)
       (list :title "April read"
             :time (tessera-month-tests--time 2026 4 2)))
    (let ((tessera-safe-gap 1)
          (tessera-month-left-padding 1)
          (tessera-month-right-padding 1)
          (tessera-flex-gap-min-width 1))
      (cl-letf (((symbol-function 'tessera--string-pixel-width)
                 #'tessera-month-tests--pixel-width))
        (let* ((heading
                (tessera--month-compose-header
                 (car tessera--month-groups) t 300 10 t))
               (line-end (string-match "\n" heading))
               (line (substring heading 0 line-end)))
          (should (= (tessera--string-pixel-width line) 300))
          (should (string-match-p "U 1.*R 1" line)))))))

(ert-deftest tessera-month-graphical-heading-respects-face-remapping
    ()
  (skip-unless (display-graphic-p))
  (tessera-month-tests--with-buffer
      (list (list :title "April"
                  :time (tessera-month-tests--time 2026 4 20)))
    (dolist (scale '(0.75 1.0 2.0))
      (setq-local face-remapping-alist
                  `((default (:height ,scale) default)))
      (dolist (width '(300 500))
        (dolist (collapsed '(nil t))
          (let ((heading
                 (tessera--month-compose-header
                  (car tessera--month-groups) collapsed width
                  (frame-char-width) t)))
            ;; Measure in a real buffer, independently of the helper.
            (with-temp-buffer
              (setq-local face-remapping-alist
                          `((default (:height ,scale) default)))
              (insert heading)
              (should (<= (car (buffer-text-pixel-size nil nil t))
                          width)))))))))

(ert-deftest tessera-month-remapping-refreshes-only-headings ()
  (tessera-month-tests--with-buffer
      (list (list :title "April"
                  :time (tessera-month-tests--time 2026 4 20)))
    (let ((header (tessera-month-tests--header-for-key '(2026 4))))
      (setq-local face-remapping-alist '((default (:height 2.0))))
      (cl-letf (((symbol-function 'tessera--month-scan-entries)
                 (lambda () (ert-fail "Remapping rescanned rows"))))
        (tessera--month-window-change))
      (should-not (overlay-buffer header))
      (should (tessera-month-tests--header-for-key '(2026 4))))))

(ert-deftest tessera-month-refuses-to-fold-last-expanded-group ()
  (tessera-month-tests--with-buffer
      (list
       (list :title "Only"
             :time (tessera-month-tests--time 2026 4 20)))
    (tessera--month-toggle '(2026 4))
    (should-not (gethash '(2026 4) tessera--month-folds))))

(ert-deftest tessera-month-absorbs-undated-entry-and-warns ()
  (tessera-month-tests--with-buffer
      (list
       (list :title "No date")
       (list :title "April"
             :time (tessera-month-tests--time 2026 4 20))
       (list :title "Also no date"))
    (should (= (length tessera--month-groups) 1))
    (should (equal (tessera--month-group-key
                    (car tessera--month-groups))
                   '(2026 4)))
    (goto-char (point-min))
    (should (search-forward "!No date" nil t))
    (should (search-forward "!Also no date" nil t))))

(ert-deftest tessera-month-disables-noncontiguous-order ()
  (tessera-month-tests--with-buffer
      (list
       (list :title "April one"
             :time (tessera-month-tests--time 2026 4 20))
       (list :title "March"
             :time (tessera-month-tests--time 2026 3 20))
       (list :title "April two"
             :time (tessera-month-tests--time 2026 4 2)))
    (should-not tessera--month-groups)
    (should tessera--month-order-warning)
    (should-not
     (cl-find-if
      (lambda (overlay)
        (overlay-get overlay 'tessera-month-overlay))
      (overlays-in (point-min) (point-max))))))

(ert-deftest tessera-month-thread-date-selects-root-or-latest ()
  (tessera-month-tests--with-buffer
      (tessera-month-tests--cross-month-thread)
    (should (equal (tessera--month-group-key
                    (car tessera--month-groups))
                   '(2026 4)))
    (setq-local tessera--month-thread-date 'root)
    (tessera-month-sync)
    (should (equal (tessera--month-group-key
                    (car tessera--month-groups))
                   '(2026 3)))
    (should (= (tessera--month-group-total
                (car tessera--month-groups))
               2))))

(ert-deftest tessera-month-native-fold-keeps-thread-date ()
  (tessera-month-tests--with-buffer
      (tessera-month-tests--cross-month-thread)
    (goto-char (point-min))
    (forward-line 1)
    (let ((fold (make-overlay
                 (line-beginning-position)
                 (min (point-max)
                      (1+ (line-end-position))))))
      (overlay-put fold 'invisible 'native-fold)
      (add-to-invisibility-spec 'native-fold)
      (tessera-month-sync)
      (should (equal (tessera--month-group-key
                      (car tessera--month-groups))
                     '(2026 4)))
      (setq-local tessera--month-thread-date 'root)
      (tessera-month-sync)
      (should (equal (tessera--month-group-key
                      (car tessera--month-groups))
                     '(2026 3))))))

(ert-deftest tessera-month-fold-state-survives-and-prunes ()
  (tessera-month-tests--with-buffer
      (list
       (list :title "April"
             :time (tessera-month-tests--time 2026 4 20))
       (list :title "March"
             :time (tessera-month-tests--time 2026 3 20)))
    (goto-char (tessera--month-group-start
                (cadr tessera--month-groups)))
    (tessera--month-toggle '(2026 4))
    (tessera-month-configure nil 'latest)
    (should (gethash '(2026 4) tessera--month-folds))
    (tessera-month-configure t 'latest)
    (should (gethash '(2026 4) tessera--month-folds))
    (tessera--month-clear-display)
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max)))
    (tessera-month-tests--insert
     (list
      (list :title "March"
            :time (tessera-month-tests--time 2026 3 20))))
    (goto-char (point-min))
    (tessera-month-sync)
    (should-not (gethash '(2026 4) tessera--month-folds))
    (should (equal (mapcar #'tessera--month-group-key
                           tessera--month-groups)
                   '((2026 3))))))

(ert-deftest tessera-month-preserves-native-invisibility-category ()
  (tessera-month-tests--with-buffer
      (list
       (list :title "April"
             :time (tessera-month-tests--time 2026 4 20)))
    (tessera-month-clear)
    (setq-local buffer-invisibility-spec
                '(tessera-month-hidden))
    (tessera-month-configure t 'latest)
    (should-not tessera--month-invisibility-installed)
    (tessera-month-clear)
    (should
     (memq 'tessera-month-hidden buffer-invisibility-spec))))

(ert-deftest tessera-month-sync-and-clear-preserve-narrowing ()
  (tessera-month-tests--with-buffer
      (list
       (list :title "April"
             :time (tessera-month-tests--time 2026 4 20))
       (list :title "March"
             :time (tessera-month-tests--time 2026 3 20)))
    (narrow-to-region (point-min) (line-end-position))
    (let ((end (point-max)))
      (tessera-month-sync)
      (should (= (length tessera--month-groups) 2))
      (should (= (point-max) end)))
    (tessera-month-clear)
    (widen)
    (should-not
     (cl-find-if
      (lambda (overlay)
        (overlay-get overlay 'tessera-month-overlay))
      (overlays-in (point-min) (point-max))))))

(ert-deftest tessera-month-fold-and-redraw-preserve-narrowing ()
  (tessera-month-tests--with-buffer
      (cl-loop for month from 5 downto 2
               collect
               (list :title (number-to-string month)
                     :time (tessera-month-tests--time
                            2026 month 20)))
    (let ((start (tessera--month-group-start
                  (nth 1 tessera--month-groups)))
          (end (tessera--month-group-end
                (nth 2 tessera--month-groups))))
      (narrow-to-region start end)
      (goto-char start)
      (tessera--month-toggle '(2026 4))
      (should (gethash '(2026 4) tessera--month-folds))
      (should (= (line-beginning-position)
                 (tessera--month-group-start
                  (nth 2 tessera--month-groups))))
      ;; May and February are expanded but inaccessible.
      (tessera--month-toggle '(2026 3))
      (should-not (gethash '(2026 3) tessera--month-folds))
      (tessera--month-refresh-headings)
      (goto-char start)
      (tessera-month-reveal-point)
      (should-not (gethash '(2026 4) tessera--month-folds))
      (should (= (point) start))
      (tessera--month-toggle '(2026 4))
      (let ((fold
             (cl-find-if
              (lambda (overlay)
                (eq (overlay-get overlay 'tessera-month-overlay)
                    'fold))
              tessera--month-overlays)))
        (tessera--month-open-isearch fold))
      (should-not (gethash '(2026 4) tessera--month-folds))
      (should (= (point-min) start))
      (should (= (point-max) end))
      (should (= (length tessera--month-groups) 4))
      ;; A restriction can start partway through its only entry.
      (narrow-to-region (1+ start) (+ start 2))
      (goto-char (point-min))
      (tessera--month-toggle '(2026 4))
      (should-not (gethash '(2026 4) tessera--month-folds)))))

(ert-deftest tessera-month-goto-falls-back-after-native-miss ()
  (tessera-month-tests--with-buffer
      (list
       (list :title "April"
             :time (tessera-month-tests--time 2026 4 20))
       (list :title "March"
             :time (tessera-month-tests--time 2026 3 20)))
    (setf (tessera--entry-backend-month-goto
           (gethash 'tessera-month-tests tessera--entry-backends))
          (lambda (_context) nil))
    (cl-letf (((symbol-function 'tessera--month-scan-entries)
               (lambda () (ert-fail "Fold rescanned entries"))))
      (tessera--month-toggle '(2026 4)))
    (should (= (line-beginning-position)
               (tessera--month-group-start
                (cadr tessera--month-groups))))
    (should (= (point) (tessera-entry-point)))))

(ert-deftest tessera-month-configure-rejects-invalid-policy ()
  (with-temp-buffer
    (dolist (enabled '(nil t))
      (should-error (tessera-month-configure enabled nil))
      (should-not tessera--month-enabled))))

(ert-deftest tessera-month-prunes-folds-without-dated-results ()
  (dolist (objects '(nil ((:title "Undated"))))
    (tessera-month-tests--with-buffer objects
      (puthash '(2026 4) t tessera--month-folds)
      (tessera-month-sync)
      (should-not tessera--month-groups)
      (should (= (hash-table-count tessera--month-folds) 0)))))

(ert-deftest tessera-month-click-rejects-drag-and-unrelated-input ()
  (let* ((window (selected-window))
         (position (list window 1 '(0 . 0) 0))
         (drag (list 'drag-mouse-1 position position))
         (outside (list 'mouse-1
                        (list (minibuffer-window) 1 '(0 . 0) 0))))
    (pcase-dolist
        (`(,events ,remaining)
         (list (list (list drag) nil)
               (list (list (list 'double-drag-mouse-1
                                 position position)) nil)
               (list (list (list 'triple-drag-mouse-1
                                 position position)) nil)
               (list (list outside) nil)
               (list (list ?n) (list ?n))
               (list (list (list 'wheel-up position))
                     (list (list 'wheel-up position)))))
      (let ((unread-command-events
             (mapcar (lambda (event) (cons t event)) events)))
        (should-not (tessera--month-click-release-p window))
        (should (equal unread-command-events
                       (mapcar (lambda (event) (cons t event))
                               remaining)))))))

(ert-deftest tessera-month-click-accepts-native-click-after-motion ()
  (let* ((window (selected-window))
         (position (list window 1 '(0 . 0) 0)))
    (dolist (release '(mouse-1 double-mouse-1 triple-mouse-1))
      (let ((unread-command-events
             (mapcar (lambda (event) (cons t event))
                     (list (list 'mouse-movement position)
                           (list release position)))))
        (should (tessera--month-click-release-p window))
        (should-not unread-command-events)))))

(ert-deftest tessera-month-repeated-clicks-toggle-once-per-release ()
  (tessera-month-tests--with-buffer
      (list (list :title "April"
                  :time (tessera-month-tests--time 2026 4 20))
            (list :title "March"
                  :time (tessera-month-tests--time 2026 3 20)))
    (save-window-excursion
      (set-window-buffer (selected-window) (current-buffer))
      (tessera--month-window-change)
      (dotimes (index 3)
        (tessera-tests--click-month '(2026 4) (1+ index))
        (should (eq (gethash '(2026 4) tessera--month-folds)
                    (zerop (% index 2))))))))

(ert-deftest tessera-month-headings-fit-each-window-on-resize ()
  (tessera-month-tests--with-buffer
      (list (list :title "April"
                  :time (tessera-month-tests--time 2026 4 20)))
    (save-window-excursion
      (delete-other-windows)
      (set-window-buffer (selected-window) (current-buffer))
      (let* ((first (selected-window))
             (second (split-window first nil 'right))
             (window-min-width 10))
        (cl-letf (((symbol-function 'display-graphic-p)
                   (lambda (&optional _display) t))
                  ((symbol-function 'tessera--string-pixel-width)
                   #'tessera-month-tests--pixel-width))
          (window-resize first -5 t)
          (dolist (change '(0 2))
            (unless (zerop change)
              (window-resize first change t))
            (tessera--month-window-change first)
            (dolist (window (list first second))
              (let* ((header
                      (cl-find window tessera--month-overlays
                               :key (lambda (overlay)
                                      (overlay-get overlay 'window))))
                     (text (overlay-get header 'before-string)))
                (should header)
                (should (= (tessera--string-pixel-width
                            (substring
                             text 0 (string-match "\n" text)))
                           (window-body-width window t)))))))))))

(ert-deftest tessera-month-warning-background-blends-theme-colors ()
  (let ((context (make-tessera-entry-context)))
    (cl-letf (((symbol-function 'tessera--glyph-frame)
               (lambda (_context) nil))
              ((symbol-function 'face-attribute)
               (lambda (face attribute &rest _arguments)
                 (pcase (list face attribute)
                   (`(warning :foreground) "#ff0000")
                   (`(default :background) "#000000")
                   (`(tessera-month-undated-face :background)
                    'unspecified)))))
      (let ((background
             (tessera--month-undated-background context)))
        (should (equal background "#260000"))))))

(ert-deftest tessera-month-disabled-omits-undated-warning ()
  (let ((tessera--entry-backends (make-hash-table :test #'eq))
        (tessera--month-enabled nil)
        (tessera-entry-layout 'single-line)
        (tessera-glyph-style 'ascii))
    (with-temp-buffer
      (tessera-month-tests--register)
      (let ((reads 0)
            (reader (symbol-function 'tessera-month-tests--date)))
        (cl-letf (((symbol-function 'tessera-month-tests--date)
                   (lambda (context)
                     (cl-incf reads)
                     (funcall reader context)))
                  ((symbol-function 'tessera-month-tests--unread-p)
                   (lambda (_context)
                     (cl-incf reads))))
          (tessera-month-tests--insert
           (list (list :title "No date"))))
        (should (zerop reads)))
      (should-not (string-match-p "!" (buffer-string)))
      (should-not
       (text-property-any
        (point-min) (point-max) 'face
        'tessera-month-undated-face)))))

(ert-deftest tessera-month-callbacks-finish-after-condition ()
  (dolist (condition '(error quit))
    (let* ((visited nil)
           (tessera--month-change-functions
            (list
             (lambda (_option)
               (push 'first visited)
               (signal condition '("Month callback failed")))
             (lambda (_option) (push 'second visited))))
           condition-data)
      (condition-case caught
          (tessera--run-month-change-functions nil)
        ((error quit) (setq condition-data caught)))
      (should (equal condition-data
                     (list condition "Month callback failed")))
      (should (equal visited '(second first))))))

(ert-deftest tessera-month-heading-options-do-not-rebuild-entries ()
  (tessera-month-tests--with-buffer
      (list (list :title "April"
                  :time (tessera-month-tests--time 2026 4 20)))
    (let ((tessera-month-format tessera-month-format)
          (groups tessera--month-groups)
          (tick (buffer-chars-modified-tick)))
      (cl-letf (((symbol-function 'tessera-month-sync)
                 (lambda () (ert-fail "Rebuilt month entries")))
                ((symbol-function
                  'tessera--run-month-change-functions)
                 (lambda (_) (ert-fail "Refreshed native backend"))))
        (tessera--set-month-option 'tessera-month-format "%Y/%m")
        (should
         (string-match-p
          "2026/04"
          (overlay-get
           (tessera-month-tests--header-for-key '(2026 4))
           'before-string)))
        ;; Direct assignments are detected by the heading hook too.
        (setq tessera-month-format "%m-%Y")
        (tessera--month-window-change)
        (should
         (string-match-p
          "04-2026"
          (overlay-get
           (tessera-month-tests--header-for-key '(2026 4))
           'before-string))))
      (should (eq groups tessera--month-groups))
      (should (= tick (buffer-chars-modified-tick))))))

(provide 'tessera-month-tests)
;;; tessera-month-tests.el ends here
