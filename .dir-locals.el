;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((auto-mode-alist . (("\\.el\\.in\\'" . emacs-lisp-mode)))
 (emacs-lisp-mode
  . ((eval
      . (progn
          (let* ((c-load-path
                  (copy-sequence
                   elisp-flymake-byte-compile-load-path))
                 (e-load-path (copy-sequence load-path))
                 (project-directory
                  (locate-dominating-file default-directory
                                          ".dir-locals.el"))
                 (lisp-path
                  (expand-file-name "lisp/" project-directory)))
            (setq-local load-path e-load-path
                        elisp-flymake-byte-compile-load-path c-load-path)
            (add-to-list 'elisp-flymake-byte-compile-load-path
                         lisp-path)
            (add-to-list 'load-path lisp-path))))))
 (nil . ((sentence-end-double-space . t))))
