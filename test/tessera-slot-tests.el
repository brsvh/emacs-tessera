;;; tessera-slot-tests.el --- Optional slot tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Batch tests for optional glyph references and empty inline groups.

;;; Code:

(require 'ert)
(require 'tessera)

(ert-deftest tessera-slot-optional-reference-collapses-empty-width ()
  (let* ((tessera-glyph-style 'ascii)
         (calls 0)
         (context (make-tessera-entry-context))
         (slot (make-tessera-glyph-slot
                :name 'status :width 2 :align 'center
                :selector (lambda (ctx)
                            (cl-incf calls)
                            (tessera-entry-context-object ctx))
                :glyphs
                (list (list 'present :glyph
                            (make-tessera-glyph
                             :ascii "x" :unicode "x"
                             :nerd-icons
                             '(:function nerd-icons-mdicon
                                         :name "nf-md-paperclip")
                             :semantic 'neutral)))))
         (definition
          (tessera--make-entry-backend
           :glyph-slots (list slot)
           :segments '((text . ignore))))
         (optional '((status :optional t))))
    (cl-letf (((symbol-function 'display-graphic-p)
               (lambda (&optional _frame) nil)))
      (should-not
       (tessera--render-slot-group optional definition context))
      (should (= calls 1))
      (should (= 2 (tessera--rendered-segment-width
                    (tessera--render-slot-group
                     '(status) definition context))))
      ;; A missing group must not leave a segment separator behind.
      (should
       (equal-including-properties
        (tessera--render-line nil '(text) nil definition context)
        (tessera--render-line
         optional '(text (:slots (status :optional t)))
         nil definition context)))
      (setf (tessera-entry-context-object context) 'present)
      (setq calls 0)
      (let ((shown (tessera--render-slot-group
                    optional definition context))
            (reserved (tessera--render-slot-group
                       '((status :optional t :reserve t))
                       definition context)))
        (should (= calls 2))
        (should (= 2 (tessera--rendered-segment-width shown)))
        (should (string-match-p
                 "x" (tessera--rendered-segment-string shown)))
        (should (= 2 (tessera--rendered-segment-width reserved)))
        (should-not (string-match-p
                     "x" (tessera--rendered-segment-string
                          reserved))))
      (dolist (state '(nil present))
        (setf (tessera-entry-context-object context) state)
        (dolist (align '(nil left right))
          (setq calls 0)
          (tessera--render-line
           optional nil nil definition context align)
          (should (= calls 1)))))))

(ert-deftest tessera-slot-optional-reference-validates-boolean ()
  (tessera--validate-glyph-slot-reference
   '(status :optional t) '(status) "Test")
  (should-error
   (tessera--validate-glyph-slot-reference
    '(status :optional sometimes) '(status) "Test")))

(ert-deftest tessera-slot-area-packs-icons-without-moving-content ()
  (let* ((tessera-glyph-style 'ascii)
         (references '(a b c d))
         (context (make-tessera-entry-context))
         (definition
          (tessera--make-entry-backend
           :glyph-slots
           (mapcar
            (lambda (name)
              (make-tessera-glyph-slot
               :name name :width 2 :align 'left
               :selector
               (lambda (ctx)
                 (when (memq name (tessera-entry-context-object ctx))
                   'present))
               :glyphs
               (list
                (list 'present :glyph
                      (make-tessera-glyph
                       :ascii (symbol-name name)
                       :unicode (symbol-name name)
                       :nerd-icons
                       '(:function nerd-icons-mdicon
                                   :name "nf-md-paperclip")
                       :semantic 'neutral)))))
            references))))
    (cl-labels
        ((columns
           (text)
           (apply #'concat
                  (cl-loop
                   for i below (length text)
                   for display = (get-text-property i 'display text)
                   collect
                   (if (eq (car-safe display) 'space)
                       (make-string (plist-get (cdr display) :width)
                                    ?\s)
                     (substring text i (1+ i)))))))
      (cl-letf (((symbol-function 'display-graphic-p)
                 (lambda (&optional _frame) nil)))
        (dolist (case '(((b d) "    b d ")
                        ((c) "      c ")
                        (nil "        ")
                        ((a b c d) "a b c d ")))
          (setf (tessera-entry-context-object context) (car case))
          (should
           (equal (columns (car (tessera--render-glyph-slots
                                 references definition
                                 context 'right)))
                  (cadr case))))
        (setf (tessera-entry-context-object context) '(b d))
        (should
         (equal (columns (car (tessera--render-glyph-slots
                               references definition context 'left)))
                "b d     "))
        ;; The default renderer retains the original fixed positions.
        (should
         (equal (columns (car (tessera--render-glyph-slots
                               references definition context)))
                "  b   d "))))))

(ert-deftest tessera-slot-area-validates-alignment ()
  (should-error
   (tessera--validate-layout
    (make-tessera-entry-layout :glyph-slots-align 'middle)
    nil nil "Test")))

(provide 'tessera-slot-tests)
;;; tessera-slot-tests.el ends here
