;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((emacs-lisp-mode
  .
  ((eval
    .
    (progn
      (let* ((pdir (locate-dominating-file default-directory ".dir-locals.el"))
             (ldir (expand-file-name "lisp/" pdir))
             (cpath (copy-sequence elisp-flymake-byte-compile-load-path))
             (rpath (copy-sequence load-path))
             (paths (seq-filter #'file-directory-p
                                (directory-files ldir t "\\`[^.]"))))
        (setq-local elisp-flymake-byte-compile-load-path cpath
                    load-path rpath)
        (dolist (path paths)
          (add-to-list 'elisp-flymake-byte-compile-load-path path)
          (add-to-list 'load-path path)))))))

 (nil . ((sentence-end-double-space . t))))
