;;; run-tests.el --- Run Tessera regressions -*- lexical-binding: t; -*-

;;; Commentary:

;; Batch entry point.  Elfeed and mu4e must be on `load-path'.
;; Set TESSERA_TEST_PACKAGE_DIRS to test installed bytecode.
;; TESSERA_TEST_PACKAGE selects tessera (default) or tessera-x.

;;; Code:

(require 'ert)
(require 'seq)
(require 'subr-x)

(unless noninteractive
  (user-error "Run this file in a batch Emacs process"))

(defvar tessera-tests--package
  (or (getenv "TESSERA_TEST_PACKAGE") "tessera")
  "Package whose runtime regressions will run in this process.")

(unless (member tessera-tests--package '("tessera" "tessera-x"))
  (error "Unknown test package: %s" tessera-tests--package))

(defvar tessera-tests--libraries
  (append
   '("tessera"
     "tessera-gnus" "tessera-gnus-article" "tessera-gnus-summary"
     "tessera-mu4e" "tessera-mu4e-thread" "tessera-mu4e-headers"
     "tessera-elfeed" "tessera-elfeed-search")
   (when (equal tessera-tests--package "tessera-x")
     '("tessera-x" "tessera-x-gnus"
       "tessera-x-mu4e" "tessera-x-elfeed")))
  "Production libraries expected for the selected test package.")

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
       (package-path (getenv "TESSERA_TEST_PACKAGE_DIRS"))
       (library-directories
        (if package-path
            (mapcar #'file-truename
                    (split-string package-path path-separator t))
          (mapcar
           (lambda (package)
             (expand-file-name (concat "../lisp/" package) directory))
           (if (equal tessera-tests--package "tessera-x")
               '("tessera" "tessera-x")
             '("tessera")))))
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
    (message "Testing %s %s from %s" tessera-tests--package
             (if package-path "bytecode" "source")
             (string-join library-directories ", "))
    (dolist (file (directory-files directory t
                                   "\\`tessera-.*-tests\\.el\\'"))
      (let ((name (file-name-base file)))
        (when (if (equal tessera-tests--package "tessera-x")
                  (equal name "tessera-x-tests")
                (not (member name '("tessera-x-tests"
                                    "tessera-build-tests"
                                    "tessera-elfmt-tests"))))
          (require (intern name) file))))
    (ert-run-tests-batch-and-exit)))
;;; run-tests.el ends here
