;;; run-tool-tests.el --- Run tooling regressions -*- lexical-binding: t; -*-

;;; Commentary:

;; Run build and formatter checks independently of package tests.

;;; Code:

(require 'ert)

(unless noninteractive
  (user-error "Run this file in a batch Emacs process"))

(let ((directory (file-name-directory load-file-name)))
  (dolist (library '(tessera-build-tests tessera-elfmt-tests))
    (require library
             (expand-file-name
              (concat (symbol-name library) ".el") directory))))

(ert-run-tests-batch-and-exit)

;;; run-tool-tests.el ends here
