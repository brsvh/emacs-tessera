;;; run-tests.el --- Run Tessera regressions -*- lexical-binding: t; -*-

;;; Commentary:

;; Batch entry point.  Elfeed and mu4e must be on `load-path'.
;; Set TESSERA_TEST_PACKAGE_DIR to test installed bytecode.

;;; Code:

(require 'ert)
(require 'subr-x)

(unless noninteractive
  (user-error "Run this file in a batch Emacs process"))

(defconst tessera-tests--libraries
  '("tessera" "tessera-x"
    "tessera-gnus" "tessera-gnus-article" "tessera-gnus-summary"
    "tessera-gnus-x"
    "tessera-mu4e" "tessera-mu4e-thread" "tessera-mu4e-headers"
    "tessera-mu4e-x"
    "tessera-elfeed" "tessera-elfeed-search" "tessera-elfeed-x")
  "Production libraries expected in a complete Tessera package.")

(defvar tessera-tests--package-directory nil
  "Installed package directory under test, or nil for source tests.")

(defun tessera-tests--check-library-source (file)
  "Reject production FILE loaded outside the selected package."
  (when (and tessera-tests--package-directory
             (member (file-name-base file) tessera-tests--libraries))
    (let ((expected
           (expand-file-name
            (concat (file-name-base file) ".elc")
            tessera-tests--package-directory)))
      (unless (and (string-suffix-p ".elc" file)
                   (file-equal-p file expected))
        (error "Expected installed library %s, loaded %s"
               expected file)))))

(let* ((directory (file-name-directory load-file-name))
       (package-directory (getenv "TESSERA_TEST_PACKAGE_DIR"))
       (tessera-tests--package-directory
        (when package-directory
          (file-name-as-directory
           (file-truename package-directory))))
       (library-directory
        (or tessera-tests--package-directory
            (expand-file-name "../lisp" directory)))
       (load-prefer-newer (not package-directory))
       (load-no-native t)
       (native-comp-jit-compilation nil)
       (load-path (append (list library-directory directory)
                          load-path)))
  (when package-directory
    (when (string-empty-p package-directory)
      (error "TESSERA_TEST_PACKAGE_DIR is empty"))
    (dolist (library tessera-tests--libraries)
      (let ((file (locate-library library)))
        (unless file (error "Missing installed library: %s" library))
        (tessera-tests--check-library-source file)
        (unless (file-readable-p
                 (expand-file-name (concat library ".el")
                                   library-directory))
          (error "Missing installed source: %s.el" library))))
    (dolist (entry load-history)
      (when (stringp (car entry))
        (tessera-tests--check-library-source (car entry))))
    (dolist (library tessera-tests--libraries)
      (when (featurep (intern library))
        (error "Start installed tests before loading `%s'" library))))
  (let ((after-load-functions
         (cons #'tessera-tests--check-library-source
               after-load-functions)))
    (dolist (library tessera-tests--libraries)
      (require (intern library)))
    (message "Testing Tessera %s from %s"
             (if package-directory "bytecode" "source")
             library-directory)
    (dolist (file (directory-files directory t
                                   "\\`tessera-.*-tests\\.el\\'"))
      (require (intern (file-name-base file)) file))
    (ert-run-tests-batch-and-exit)))
;;; run-tests.el ends here
