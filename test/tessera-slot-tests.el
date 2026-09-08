;;; tessera-slot-tests.el --- Optional slot tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Batch tests for optional glyph references and empty inline groups.

;;; Code:

(require 'ert)
(require 'tessera)

(ert-deftest tessera-slot-optional-reference-collapses-empty-width ()
  (let* ((tessera-glyph-style 'ascii)
         (context (make-tessera-entry-context))
         (slot (make-tessera-glyph-slot
                :name 'status :width 2 :align 'center
                :selector #'tessera-entry-context-object
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
      (let ((shown (tessera--render-slot-group
                    optional definition context))
            (reserved (tessera--render-slot-group
                       '((status :optional t :reserve t))
                       definition context)))
        (should (= 2 (tessera--rendered-segment-width shown)))
        (should (string-match-p
                 "x" (tessera--rendered-segment-string shown)))
        (should (= 2 (tessera--rendered-segment-width reserved)))
        (should-not (string-match-p
                     "x" (tessera--rendered-segment-string
                          reserved)))))))

(ert-deftest tessera-slot-optional-reference-validates-boolean ()
  (tessera--validate-glyph-slot-reference
   '(status :optional t) '(status) "Test")
  (should-error
   (tessera--validate-glyph-slot-reference
    '(status :optional sometimes) '(status) "Test")))

(provide 'tessera-slot-tests)
;;; tessera-slot-tests.el ends here
