;;; tessera-test-support.el --- Shared test helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; Backend-independent assertions and string inspection helpers.

;;; Code:

(require 'cl-lib)

(defun tessera-tests--property-position (property value string)
  "Return the position where PROPERTY equals VALUE in STRING."
  (cl-loop for position below (length string)
           when (equal (get-text-property position property string)
                       value)
           return position))

(provide 'tessera-test-support)
;;; tessera-test-support.el ends here
