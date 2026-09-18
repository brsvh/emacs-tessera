;;; tessera-elfmt-tests.el --- Formatter regressions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bingshan Chang <chang@bingshan.org>

;;; Commentary:

;; Check that formatting preserves Lisp data.

;;; Code:

(require 'ert)
(require 'elfmt
         (expand-file-name "../tool/elfmt/elfmt.el"
                           (file-name-directory
                            (or load-file-name
                                (bound-and-true-p
                                 byte-compile-current-file)
                                buffer-file-name))))

(ert-deftest tessera-elfmt-preserves-multiline-strings ()
  "Both indentation styles preserve whitespace inside strings."
  (dolist (mode '(emacs-lisp-mode lisp-data-mode))
    (dolist (style '("space" "tab"))
      (with-temp-buffer
        (funcall mode)
        (insert "(sample \"first\n\tsecond\n        third\n\t\")\n")
        (let ((original (read (buffer-string)))
              (editorconfig-properties-hash (make-hash-table)))
          (puthash 'indent_style style editorconfig-properties-hash)
          (elfmt--indent-buffer)
          (should (equal original (read (buffer-string)))))))))

(ert-deftest tessera-elfmt-normalizes-code-indentation ()
  "Normalizing strings must still convert whitespace in code."
  (dolist (style '("space" "tab"))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(sample\n\t        value)\n")
      (let ((editorconfig-properties-hash (make-hash-table)))
        (puthash 'indent_style style editorconfig-properties-hash)
        (elfmt--normalize-indentation)
        (should
         (equal (buffer-string)
                (if (equal style "space")
                    "(sample\n                value)\n"
                  "(sample\n\t\tvalue)\n")))))))

(provide 'tessera-elfmt-tests)
;;; tessera-elfmt-tests.el ends here
