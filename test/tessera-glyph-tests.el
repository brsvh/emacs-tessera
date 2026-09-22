;;; tessera-glyph-tests.el --- Glyph options -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercise configuration merging, validation, and adapter lifecycle.

;;; Code:

(require 'ert)
(require 'tessera)
(require 'tessera-gnus-summary)
(require 'tessera-mu4e-headers)
(require 'tessera-elfeed-search)

(ert-deftest tessera-glyph-definitions-belong-to-their-view ()
  (dolist (view '(tessera-gnus-summary tessera-mu4e-headers
                                       tessera-elfeed-search))
    (let ((prefix (symbol-name view)))
      (dolist (suffix '("-glyphs" "--glyph-defaults"))
        (should
         (equal prefix
                (file-name-base
                 (symbol-file (intern (concat prefix suffix))
                              'defvar)))))
      (should
       (equal prefix
              (file-name-base
               (symbol-file (intern (concat prefix "--set-glyphs"))
                            'defun)))))))

(ert-deftest tessera-glyph-overrides-keep-defaults-immutable ()
  (let* ((defaults (copy-tree tessera-mu4e-headers--glyph-defaults))
         (original (copy-tree defaults))
         (glyph
          (tessera-glyph-resolve
           'status-unread defaults
           '((status-unread :unicode "◉"
                            :face nil
                            :nerd-icons (:name "nf-md-star"))))))
    (should (equal (tessera-glyph-ascii glyph) "u"))
    (should (equal (tessera-glyph-unicode glyph) "◉"))
    (should-not (tessera-glyph-face glyph))
    (should (equal (tessera-glyph-nerd-icons glyph)
                   '(:function nerd-icons-mdicon :name "nf-md-star")))
    (should (equal defaults original))
    (let ((plain
           (tessera-glyph-resolve
            'status-unread defaults
            '((status-unread :unicode nil :nerd-icons nil)))))
      (should-not (tessera-glyph-unicode plain))
      (should-not (tessera-glyph-nerd-icons plain))
      (should (eq (tessera-glyph-face plain)
                  'tessera-glyph-accent-face)))))

(ert-deftest tessera-glyph-overrides-reject-invalid-fields ()
  (dolist (value '(((unknown :ascii "?"))
                   ((status-unread :asci "?"))
                   ((status-unread :ascii "x" :ascii "y"))
                   ((status-unread) (status-unread :ascii "x"))
                   ((status-unread :ascii nil))
                   ((status-unread :ascii "●"))
                   ((status-unread :ascii "abc"))
                   ((status-unread :unicode "abc"))
                   ((status-unread :unicode ""))
                   ((status-unread :unicode "\n"))
                   ((status-unread :hidden yes))
                   ((status-unread :face not-a-face))
                   ((status-unread :nerd-icons (:function nil)))
                   ((status-unread :nerd-icons (:name 1)))
                   ((status-unread :nerd-icons (:font "Font")))
                   ((status-unread :nerd-icons
                                   (:name "x" :name "y")))))
    (should-error
     (tessera--validate-glyph-overrides
      tessera-mu4e-headers--glyph-defaults value 2))))

(ert-deftest tessera-glyph-setter-rejects-before-changing-state ()
  (let* ((tessera-mu4e-headers-glyphs nil)
         (definition (gethash 'mu4e-headers tessera--entry-backends))
         (calls 0)
         (tessera--glyph-change-functions
          (list (lambda (_option) (cl-incf calls)))))
    (should-error
     (tessera-mu4e-headers--set-glyphs
      'tessera-mu4e-headers-glyphs '((status-unread :face invalid))))
    (should-not tessera-mu4e-headers-glyphs)
    (should (eq definition
                (gethash 'mu4e-headers tessera--entry-backends)))
    (should (= calls 0))
    (tessera-mu4e-headers--set-glyphs
     'tessera-mu4e-headers-glyphs '((status-unread :face nil)))
    (should (= calls 1))
    (should (equal tessera-mu4e-headers-glyphs
                   '((status-unread :face nil))))))

(ert-deftest tessera-glyph-customize-accepts-partial-descriptors ()
  (let ((widget (widget-convert
                 (get 'tessera-mu4e-headers-glyphs 'custom-type))))
    (dolist (value '(nil ((status-unread :unicode "◉"))
                         ((status-unread :face nil :hidden t))
                         ((status-unread :nerd-icons nil))
                         ((status-unread
                           :nerd-icons (:name "test")))))
      (should (widget-apply widget :match value)))
    (should-not
     (widget-apply widget :match '((missing :ascii "?"))))))

(ert-deftest tessera-glyph-optional-forms-fall-back-to-ascii ()
  (let ((glyph (make-tessera-glyph :ascii "x"))
        (context (make-tessera-entry-context)))
    (cl-letf (((symbol-function 'display-graphic-p)
               (lambda (&optional _frame) t))
              ((symbol-function 'tessera--nerd-icons-available-p)
               (lambda () (ert-fail "Unneeded Nerd Icons lookup"))))
      (dolist (tessera-glyph-style '(ascii unicode nerd-icons))
        (should (equal (tessera-glyph-render glyph context) "x"))))))

(ert-deftest tessera-glyph-hidden-slots-respect-space-policy ()
  (let* ((glyph (make-tessera-glyph :ascii "x" :hidden t))
         (slot (make-tessera-glyph-slot
                :name 'test
                :width 2
                :align 'center
                :selector (lambda (_context) 'present)
                :glyphs `((present :glyph ,glyph))))
         (context (make-tessera-entry-context))
         (tessera-glyph-style 'ascii))
    (should (equal (tessera-glyph-render glyph context) ""))
    (should-not (tessera--render-glyph-slot slot context t))
    (should (equal
             (get-text-property
              0 'display (tessera--render-glyph-slot slot context))
             '(space :width 2)))
    (setf (tessera-glyph-hidden glyph) nil)
    (let ((text (tessera--render-glyph-slot slot context t)))
      (should (text-property-not-all
               0 (length text) 'tessera-glyph nil text))
      (should-not (text-property-not-all
                   0 (length text) 'face nil text)))))

(ert-deftest tessera-glyph-options-do-not-change-native-priority ()
  (let* ((tessera-mu4e-headers-glyphs
          '((status-unread
             :ascii "!"
             :face tessera-glyph-warning-face)
            (status-new :ascii "+")))
         (context
          (make-tessera-entry-context :object '(:flags (unread new))))
         (slot (tessera-mu4e-headers--slot 'status))
         (tessera-glyph-style 'ascii)
         (tessera-glyph-color t))
    (should (eq (tessera-mu4e-headers--state 'status context) 'new))
    (should (string-match-p
             (regexp-quote "+")
             (tessera--render-glyph-slot slot context)))
    (setf (tessera-entry-context-object context) '(:flags (unread)))
    (let* ((text (tessera--render-glyph-slot slot context))
           (position (string-match "!" text)))
      (should position)
      (should (eq (get-text-property position 'face text)
                  'tessera-glyph-warning-face)))))

(ert-deftest tessera-glyph-gnus-preserves-native-ascii-default ()
  (let* ((gnus-unread-mark ?U)
         (tessera-gnus-summary-glyphs nil)
         (spec (assq 'status tessera-gnus-summary--states)))
    (dolist (ascii '(nil "!"))
      (setq tessera-gnus-summary-glyphs
            (when ascii `((status-unread :ascii ,ascii))))
      (let* ((slot (tessera-gnus-summary--slot spec))
             (glyph (plist-get
                     (cdr (assq 'unread
                                (tessera-glyph-slot-glyphs slot)))
                     :glyph)))
        (should (equal (tessera-glyph-ascii glyph)
                       (or ascii "U")))))))

(ert-deftest tessera-glyph-thread-face-survives-adapter-hover ()
  (let* ((context
          (make-tessera-entry-context
           :thread (make-tessera-thread-context :path '(nil))))
         (tessera-thread-glyphs
          '((last :face tessera-glyph-negative-face)))
         (tessera-glyph-color t)
         (tessera-glyph-style 'ascii)
         (text (tessera-mu4e-headers--field 'thread-tree context))
         (hover (get-text-property 0 'mouse-face text)))
    (should (eq (get-text-property 0 'face text)
                'tessera-glyph-negative-face))
    (should (equal
             (plist-get (car hover) :foreground)
             (face-attribute 'tessera-glyph-negative-face
                             :foreground nil 'default)))))

(ert-deftest tessera-glyph-thread-overrides-preserve-geometry ()
  (tessera--validate-thread-glyphs
   '((branch :ascii "->") (last :ascii "\\-" :unicode nil)))
  (dolist (value '(((branch :ascii ">"))
                   ((last :unicode "..."))
                   ((vertical :unicode "||"))
                   ((branch :hidden t))
                   ((branch :nerd-icons
                            (:function ignore :name "x")))))
    (should-error (tessera--validate-thread-glyphs value))))

(ert-deftest tessera-glyph-ellipsis-preserves-clipped-anchor ()
  (let* ((tessera-entry-ellipsis "..")
         (text
          (propertize "Long thread" 'tessera--overflow-help "Help")))
    (put-text-property 10 11 'tessera-entry-point t text)
    (should (equal (tessera--truncate-string text 5 'head) "..ead"))
    (should (equal (tessera--truncate-string text 5 'tail) "Lon.."))
    (let ((clipped (tessera--clip-thread-content text 4)))
      (should (equal clipped "Lo.."))
      (should (= (tessera-entry-point clipped) 2))
      (should (equal (get-text-property 2 'help-echo clipped)
                     "Help"))))
  (let* ((tessera-entry-ellipsis "中")
         (text (propertize "long" 'tessera--overflow-help "Help"
                           'tessera-entry-point t))
         (clipped (tessera--clip-thread-content text 1)))
    (should (equal clipped "."))
    (should (= (tessera-entry-point clipped) 0))))

(ert-deftest tessera-glyph-callbacks-follow-mode-lifecycle ()
  (let ((tessera--glyph-change-functions nil)
        (tessera-gnus-mode nil)
        (tessera-gnus--installed nil)
        (tessera-mu4e-mode nil)
        (tessera-mu4e--installed nil)
        (tessera-elfeed-mode nil)
        (tessera-elfeed--installed nil)
        (gnus-summary-mode-hook nil)
        (mu4e-headers-mode-hook nil)
        (elfeed-search-mode-hook nil))
    (dolist (spec
             '((tessera-gnus-mode
                tessera-gnus--glyphs-changed
                tessera-gnus--installed
                tessera-gnus--map-summary-buffers)
               (tessera-mu4e-mode
                tessera-mu4e--glyphs-changed
                tessera-mu4e--installed
                tessera-mu4e--map-headers-buffers)
               (tessera-elfeed-mode
                tessera-elfeed--glyphs-changed
                tessera-elfeed--installed
                tessera-elfeed--map-search-buffers)))
      (unwind-protect
          (progn
            (funcall (nth 0 spec) 1)
            (cl-letf (((symbol-function (nth 3 spec))
                       (lambda (&rest _arguments)
                         (error "Repeated setup"))))
              (funcall (nth 0 spec) 1))
            (should (symbol-value (nth 0 spec)))
            (should (symbol-value (nth 2 spec)))
            (should (= (cl-count (nth 1 spec)
                                 tessera--glyph-change-functions)
                       1)))
        (funcall (nth 0 spec) -1))
      (should-not
       (memq (nth 1 spec) tessera--glyph-change-functions)))))

(ert-deftest tessera-glyph-require-installs-no-runtime-behavior ()
  (with-temp-buffer
    (let* ((libraries
            '(tessera-gnus-summary tessera-mu4e-headers
                                   tessera-elfeed-search))
           (form
            `(progn
               (setq load-path ',load-path load-prefer-newer t)
               (require 'cl-lib)
               (require 'tessera)
               (let ((original (symbol-function 'add-hook)))
                 (cl-letf
                     (((symbol-function 'add-hook)
                       (lambda (hook function &rest args)
                         (when
                             (or (string-prefix-p
                                  "tessera-" (symbol-name hook))
                                 (and (symbolp function)
                                      (string-prefix-p
                                       "tessera-"
                                       (symbol-name function))))
                           (error "Hook installed while loading"))
                         (apply original hook function args))))
                   (mapc #'require
                         '(tessera-gnus tessera-mu4e tessera-elfeed))
                   (dolist (option
                            '(tessera-gnus-summary-glyphs
                              tessera-mu4e-headers-glyphs
                              tessera-elfeed-search-glyphs
                              tessera-x-gnus-body-policy
                              tessera-x-mu4e-subthread-scope
                              tessera-x-elfeed-fetch-linked-content))
                     (when (boundp option)
                       (error "Entry point declared view option: %s"
                              option)))
                   (mapc #'require ',libraries)))
               (unless (and (null tessera--glyph-change-functions)
                            (= (hash-table-count
                                tessera--entry-backends) 0))
                 (error "Adapter activated while loading")))))
      (should
       (zerop (call-process
               (expand-file-name invocation-name invocation-directory)
               nil t nil "-Q" "--batch" "--eval"
               (prin1-to-string form)))))))

(provide 'tessera-glyph-tests)
;;; tessera-glyph-tests.el ends here
