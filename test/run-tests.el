;;; run-tests.el --- Run Tessera regressions -*- lexical-binding: t; -*-

;;; Commentary:

;; Batch entry point.  Elfeed and mu4e must be on `load-path'.
;; Set TESSERA_TEST_PACKAGE_DIRS to test installed bytecode.

;;; Code:

(require 'ert)
(require 'seq)
(require 'subr-x)

(unless noninteractive
  (user-error "Run this file in a batch Emacs process"))

(defvar tessera-tests--libraries
  '("tessera" "tessera-x"
    "tessera-gnus" "tessera-gnus-article" "tessera-gnus-summary"
    "tessera-x-gnus"
    "tessera-mu4e" "tessera-mu4e-thread" "tessera-mu4e-headers"
    "tessera-x-mu4e"
    "tessera-elfeed" "tessera-elfeed-search" "tessera-x-elfeed")
  "Production libraries expected across the Tessera packages.")

(defvar tessera-tests--expected-libraries nil
  "Alist of library names and exact production files under test.")

(defun tessera-tests--check-library-source (file)
  "Reject production FILE loaded outside the selected packages."
  (when (and tessera-tests--expected-libraries
             (member (file-name-base file) tessera-tests--libraries))
    (let ((expected (cdr (assoc (file-name-base file)
                                tessera-tests--expected-libraries))))
      (unless (file-equal-p file expected)
        (error "Expected library %s, loaded %s" expected file)))))

(let* ((directory (file-name-directory load-file-name))
       (package-path (or (getenv "TESSERA_TEST_PACKAGE_DIRS")
                         (getenv "TESSERA_TEST_PACKAGE_DIR")))
       (library-directories
        (if package-path
            (mapcar #'file-truename
                    (split-string package-path path-separator t))
          (seq-filter
           #'file-directory-p
           (directory-files (expand-file-name "../lisp" directory)
                            t "\\`[^.]"))))
       (tessera-tests--expected-libraries nil)
       (load-prefer-newer (not package-path))
       (load-no-native t)
       (native-comp-jit-compilation nil)
       (load-path
        (append library-directories (list directory) load-path)))
  (unless library-directories
    (error "No Tessera library directories selected"))
  (let ((suffix (if package-path ".elc" ".el")))
    (dolist (library tessera-tests--libraries)
      (let ((files
             (seq-filter
              #'file-readable-p
              (mapcar
               (lambda (dir)
                 (expand-file-name (concat library suffix) dir))
               library-directories))))
        (unless (= (length files) 1)
          (error "Expected one %s copy, found %S" library files))
        (push (cons library (car files))
              tessera-tests--expected-libraries)
        (when package-path
          (unless (file-readable-p
                   (concat (file-name-sans-extension (car files))
                           ".el"))
            (error "Missing installed source: %s.el" library)))))
    (when package-path
      (dolist (library tessera-tests--libraries)
        (tessera-tests--check-library-source
         (locate-library library))))
    (dolist (entry load-history)
      (when (stringp (car entry))
        (tessera-tests--check-library-source (car entry))))
    (dolist (library tessera-tests--libraries)
      (when (featurep (intern library))
        (error "Start tests before loading `%s'" library))))
  (let ((after-load-functions
         (cons #'tessera-tests--check-library-source
               after-load-functions)))
    (dolist (library tessera-tests--libraries)
      (require
       (intern library)
       (cdr (assoc library tessera-tests--expected-libraries))))
    (message "Testing Tessera %s from %s"
             (if package-path "bytecode" "source")
             (string-join library-directories ", "))
    (dolist (file (directory-files directory t
                                   "\\`tessera-.*-tests\\.el\\'"))
      (require (intern (file-name-base file)) file))
    (ert-run-tests-batch-and-exit)))
;;; run-tests.el ends here
