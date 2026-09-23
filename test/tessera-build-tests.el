;;; tessera-build-tests.el --- Build regressions -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercise the checkout Makefile in an isolated fixture directory.

;;; Code:

(require 'ert)

(defvar tessera-build-tests--root
  (expand-file-name ".." (file-name-directory load-file-name))
  "Root directory of the checkout under test.")

(ert-deftest tessera-build-clean-removes-orphan-bytecode ()
  (let ((directory (make-temp-file "tessera-build-test-" t)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory directory)))
          (copy-file (expand-file-name "Makefile"
                                       tessera-build-tests--root)
                     "Makefile")
          (dolist (package '("tessera" "tessera-x"))
            (make-directory (concat "lisp/" package) t)
            (with-temp-file (format "lisp/%s/%s.el" package package)
              (insert ";;; Source fixture\n"))
            (with-temp-file (concat "lisp/" package "/removed.elc")
              (insert "Bytecode fixture\n")))
          (with-temp-buffer
            (let ((status
                   (call-process (or (getenv "MAKE") "make")
                                 nil t nil
                                 "PACKAGE=tessera" "clean")))
              (ert-info ((buffer-string))
                (should (equal status 0)))))
          (should-not (file-exists-p "lisp/tessera/removed.elc"))
          (should (file-exists-p "lisp/tessera/tessera.el"))
          (should (file-exists-p "lisp/tessera-x/tessera-x.el"))
          (should (file-exists-p "lisp/tessera-x/removed.elc")))
      (delete-directory directory t))))

(provide 'tessera-build-tests)
;;; tessera-build-tests.el ends here
