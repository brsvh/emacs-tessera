;;; run-x-tests.el --- Check independent Tessera X builds -*- lexical-binding: t; -*-

;;; Commentary:

;; Run in batch Emacs with the package directory as an argument.
;; Ignore site packages even when Emacs adds them to `load-path'.
;; Reject client and display libraries during compilation and loading,
;; including built-in Gnus client libraries.

;;; Code:

(require 'cl-lib)
(require 'bytecomp)
(require 'package)

(defun tessera-x-isolation--check-require (feature &rest _)
  "Reject dependencies outside the experimental package for FEATURE."
  (when (or (memq feature '(alert tessera nnheader))
            (string-match-p "\\`\\(?:gnus\\|mu4e\\|elfeed\\)"
                            (symbol-name feature))
            (and (string-prefix-p "tessera-" (symbol-name feature))
                 (not (string-prefix-p "tessera-x"
                                       (symbol-name feature)))))
    (error "Unexpected dependency: %s" feature)))

(let* ((source (expand-file-name (pop command-line-args-left)))
       (directory (make-temp-file "tessera-x-isolation-" t))
       (libraries '(tessera-x
                    tessera-x-gnus
                    tessera-x-mu4e
                    tessera-x-elfeed))
       (load-path
        (cons directory
              (cl-remove-if
               (lambda (path)
                 (and path (string-match-p "/site-lisp/" path)))
               load-path)))
       (byte-compile-error-on-warn t)
       (native-comp-jit-compilation nil)
       (load-no-native t))
  (unwind-protect
      (progn
        (dolist (library libraries)
          (let ((file (concat (symbol-name library) ".el")))
            (copy-file (expand-file-name file source)
                       (expand-file-name file directory))))
        (dolist (library '(tessera alert mu4e elfeed))
          (cl-assert (not (locate-library (symbol-name library)))))
        (advice-add 'require :before
                    #'tessera-x-isolation--check-require)
        (dolist (library libraries)
          (let ((file (expand-file-name
                       (concat (symbol-name library) ".el")
                       directory)))
            (cl-assert (byte-compile-file file))
            (require library)
            (cl-assert
             (assoc (concat file "c") load-history))))
        (with-temp-buffer
          (insert-file-contents (expand-file-name "tessera-x.el"
                                                  directory))
          (cl-assert
           (equal (package-desc-reqs (package-buffer-info))
                  '((emacs (30 1))))))
        (dolist (feature features)
          (tessera-x-isolation--check-require feature))
        (dolist (command '(tessera-x-gnus-prepare-context
                           tessera-x-mu4e-prepare-context
                           tessera-x-elfeed-prepare-context))
          (cl-assert (commandp command)))
        (dolist (option '(tessera-x-gnus-body-policy
                          tessera-x-mu4e-subthread-scope
                          tessera-x-elfeed-fetch-linked-content))
          (cl-assert (get option 'standard-value)))
        (princ "Independent compilation and loading passed\n"))
    (advice-remove 'require #'tessera-x-isolation--check-require)
    (delete-directory directory t)))

;;; run-x-tests.el ends here
