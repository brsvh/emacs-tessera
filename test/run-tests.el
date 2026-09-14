;;; run-tests.el --- Run Tessera regressions -*- lexical-binding: t; -*-

;;; Commentary:

;; Batch entry point.  Elfeed must be available on `load-path'.

;;; Code:

(require 'ert)

(unless noninteractive
  (user-error "Run this file in a batch Emacs process"))

(setq load-prefer-newer t)

(let* ((directory (file-name-directory load-file-name))
       (load-path (cons directory
                        (cons (expand-file-name "../lisp" directory)
                              load-path))))
  (dolist (file (directory-files directory t
                                 "\\`tessera-.*-tests\\.el\\'"))
    (require (intern (file-name-base file)) file)))

(ert-run-tests-batch-and-exit)
;;; run-tests.el ends here
