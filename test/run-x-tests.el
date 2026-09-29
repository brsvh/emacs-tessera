;;; run-x-tests.el --- Check Tessera X loads without clients -*- lexical-binding: t; -*-

;;; Commentary:

;; Run in batch Emacs with the Tessera X and core package directories.
;; Ignore site packages even when Emacs adds them to `load-path'.
;; Allow the core library while rejecting clients and display adapters
;; during loading, including built-in Gnus clients.

;;; Code:

(require 'cl-lib)
(require 'package)

(defun tessera-x-isolation--check-require (feature &rest _)
  "Reject native clients and display adapters required by FEATURE."
  (when (or (eq feature 'nnheader)
            (string-match-p "\\`\\(?:gnus\\|mu4e\\|elfeed\\)"
                            (symbol-name feature))
            (and (string-prefix-p "tessera-" (symbol-name feature))
                 (not (string-prefix-p "tessera-x"
                                       (symbol-name feature)))))
    (error "Unexpected dependency: %s" feature)))

(let* ((source (expand-file-name (pop command-line-args-left)))
       (core (expand-file-name (pop command-line-args-left)))
       (directory (make-temp-file "tessera-x-isolation-" t))
       (libraries '(tessera
                    tessera-x
                    tessera-x-gnus
                    tessera-x-mu4e
                    tessera-x-elfeed))
       (load-path
        (cons directory
              (cl-remove-if
               (lambda (path)
                 (and path (string-match-p "/site-lisp/" path)))
               load-path)))
       (native-comp-jit-compilation nil)
       (load-no-native t))
  (unwind-protect
      (progn
        (dolist (library libraries)
          (let ((file (concat (symbol-name library) ".el")))
            (copy-file (expand-file-name
                        file (if (eq library 'tessera) core source))
                       (expand-file-name file directory))))
        (dolist (library '(mu4e elfeed))
          (cl-assert (not (locate-library (symbol-name library)))))
        (advice-add 'require :before
                    #'tessera-x-isolation--check-require)
        (dolist (library libraries)
          (let ((file (expand-file-name
                       (concat (symbol-name library) ".el")
                       directory)))
            (require library)
            (cl-assert (assoc file load-history))))
        (with-temp-buffer
          (insert-file-contents (expand-file-name "tessera-x.el"
                                                  directory))
          (let* ((description (package-buffer-info))
                 (requirements (package-desc-reqs description))
                 (core-version
                  (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name "tessera.el" directory))
                    (package-desc-version (package-buffer-info)))))
            (cl-assert (package-desc-version description))
            (cl-assert (assq 'emacs requirements))
            (cl-assert (assq 'tessera requirements))
            (cl-assert
             (not (version-list-<
                   core-version (cadr (assq 'tessera requirements)))))
            (dolist (dependency requirements)
              (cl-assert (memq (car dependency) '(emacs tessera))))))
        (cl-assert (featurep 'tessera))
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
        (princ "Loading without clients passed\n"))
    (advice-remove 'require #'tessera-x-isolation--check-require)
    (delete-directory directory t)))

;;; run-x-tests.el ends here
