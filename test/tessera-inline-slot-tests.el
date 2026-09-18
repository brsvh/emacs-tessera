;;; tessera-inline-slot-tests.el --- Inline slots  -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercise fixed-width groups inside segment regions.

;;; Code:

(require 'ert)
(require 'tessera)
(require 'tessera-entry-tests)

(ert-deftest tessera-inline-slots-reserve-empty-and-visible-width ()
  (let* ((tessera-glyph-style 'ascii)
         (slot (tessera-entry-tests--slot))
         (definition
          (tessera--make-entry-backend :glyph-slots (list slot)))
         (context (make-tessera-entry-context :object nil))
         (reference '(:slots status (status :reserve t))))
    (cl-letf (((symbol-function 'display-graphic-p)
               (lambda (&optional _frame) nil)))
      (let ((empty
             (tessera--render-segment reference definition context)))
        (should (= (tessera--rendered-segment-width empty) 6)))
      (setf (tessera-entry-context-object context) '(:status unread))
      (let ((filled
             (tessera--render-segment reference definition context)))
        (should (= (tessera--rendered-segment-width filled) 6))
        (should-not (tessera--rendered-segment-truncate filled))
        (should (= 1 (cl-count ?* (tessera--rendered-segment-string
                                   filled))))))))

(ert-deftest tessera-inline-slots-validate-all-regions ()
  (dolist (region '(:main-left-segments :main-right-segments
                                        :extra-left-segments
                                        :extra-right-segments))
    (should
     (progn
       (tessera--validate-layout
        (apply #'make-tessera-entry-layout
               (list region '((:slots status))))
        nil '(status) "Test") t))))

(ert-deftest tessera-inline-slots-reject-invalid-groups ()
  (dolist (reference '((:slots) (:slots missing)
                       (:slots (status :grow t))))
    (should-error
     (tessera--validate-layout
      (make-tessera-entry-layout
       :main-left-segments (list reference))
      nil '(status) "Test"))))

(provide 'tessera-inline-slot-tests)
;;; tessera-inline-slot-tests.el ends here
