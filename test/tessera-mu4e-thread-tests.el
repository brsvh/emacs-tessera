;;; tessera-mu4e-thread-tests.el --- Native threads -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercise native hierarchy, folding, and message updates together.

;;; Code:

(require 'ert)
(require 'mu4e-thread)
(require 'tessera-mu4e)
(require 'tessera-mu4e-headers)

(defmacro tessera-mu4e-tests--with-thread (&rest body)
  "Run BODY with native rows containing branches and mixed states."
  (declare (indent 0) (debug t))
  `(let ((mu4e-search-threads t)
         (mu4e-headers-mode-hook nil)
         (mu4e-headers-fields '((:subject)))
         (mu4e-search-hide-enabled nil)
         (mu4e-thread-fold-unread t)
         (mu4e-thread-fold-single-children t)
         (tessera-glyph-style 'ascii))
     (with-temp-buffer
       (mu4e-headers-mode)
       (let ((inhibit-read-only t)
             (buffer (current-buffer)))
         (cl-letf (((symbol-function 'mu4e-get-headers-buffer)
                    (lambda (&rest _) buffer)))
           (cl-loop
            for id from 1
            for meta in '((:level 0 :root t :has-child t)
                          (:level 1 :first-child t :has-child t)
                          (:level 2 :first-child t :last-child t)
                          (:level 1 :last-child t)
                          (:level 0 :root t))
            do (mu4e~headers-insert-header
                (list :docid id :subject (format "Subject %d" id)
                      :from (list (list :name (format "Author %d" id)
                                        :email "a@example.test"))
                      :date '(27000 0) :priority 'high
                      :flags (if (= id 3) '(unread draft attach)
                               '(seen flagged))
                      :meta meta)
                (point-max)))
           (insert (propertize mu4e~end-of-results
                               'face 'mu4e-system-face))
           (goto-char (point-min))
           (tessera-mu4e--enable-headers)
           ,@body)))))

(ert-deftest tessera-mu4e-thread-native-paths-and-updates ()
  (tessera-mu4e-tests--with-thread
    (let ((head (gethash 1 tessera-mu4e-headers--threads)))
      (should (= 4 (tessera-thread-context-total head)))
      (should (= 1 (tessera-thread-context-unread head)))
      (should (equal '(t nil)
                     (tessera-thread-context-path
                      (gethash 3 tessera-mu4e-headers--threads))))
      (should (equal '(nil)
                     (tessera-thread-context-path
                      (gethash 4 tessera-mu4e-headers--threads)))))
    (should (string-match-p "1/4Subject 1" (buffer-string)))
    (should-not (string-match-p "Subject 2" (buffer-string)))
    (should (= 6 (count-lines (point-min) (point-max))))
    (mu4e~headers-goto-docid 3)
    (let ((message (copy-sequence (mu4e-message-at-point)))
          (start (line-beginning-position)))
      (mu4e~headers-remove-header 3)
      (setq message (plist-put message :flags '(seen draft attach)))
      (mu4e~headers-insert-header message start)
      (tessera-mu4e-headers--refresh)
      (should (= 0 (tessera-thread-context-unread
                    (gethash 1 tessera-mu4e-headers--threads)))))
    (mu4e~headers-goto-docid 2)
    (mu4e-mark-at-point 'move "/archive")
    (tessera-mu4e-headers--refresh)
    (should (string-match-p "Hm" (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position))))
    (should (string-match-p "→ /archive" (buffer-string)))
    (mu4e-mark-at-point 'unmark nil)
    (setq mu4e-search-threads nil)
    (tessera-mu4e-headers--refresh)
    (should (= 0 (hash-table-count tessera-mu4e-headers--threads)))
    (should (string-match-p "Subject 2" (buffer-string)))
    (tessera-mu4e-headers--disable)))

(ert-deftest tessera-mu4e-thread-folding-keeps-native-control ()
  (tessera-mu4e-tests--with-thread
    (mu4e-thread-fold t)
    (let* ((fold (mu4e-thread-is-folded))
           (display (overlay-get fold 'display)))
      (should fold)
      (tessera-mu4e-headers--refresh)
      (should (equal display (overlay-get fold 'display)))
      (should (= 1 (tessera-thread-context-unread
                    (gethash 1 tessera-mu4e-headers--threads))))
      ;; Only the fold summary receives padding in the hidden region.
      (let ((padding
             (seq-filter
              (lambda (overlay)
                (overlay-get overlay 'tessera-entry-overlay))
              (overlays-in (overlay-start fold) (overlay-end fold)))))
        (should (= 1 (length padding)))
        (should (equal (overlay-get (car padding) 'after-string)
                       (tessera--padding-string
                        tessera-thread-outer-bottom-padding))))
      (mu4e~headers-goto-docid 1)
      (should (= 5 (mu4e-headers-next)))
      (should (= 1 (mu4e-headers-prev)))
      (tessera-mu4e-headers--disable)
      (should (overlay-buffer fold))
      (should (equal display (overlay-get fold 'display)))
      (should-not
       (seq-some (lambda (overlay)
                   (overlay-get overlay 'tessera-entry-overlay))
                 (overlays-in (point-min) (point-max))))
      (tessera-mu4e--enable-headers)
      (mu4e-thread-unfold t)
      (tessera-mu4e-headers--refresh)
      (should-not (tessera-mu4e-thread-folds)))
    ;; Mu4e stops before pending operations even when folding unread.
    (mu4e~headers-goto-docid 3)
    (mu4e-mark-at-point 'move "/archive")
    (mu4e~headers-goto-docid 1)
    (mu4e-thread-fold t)
    (tessera-mu4e-headers--refresh)
    (mu4e~headers-goto-docid 3)
    (should-not (tessera-mu4e-thread-fold-at (point)))
    (mu4e-mark-at-point 'unmark nil)
    (mu4e~headers-goto-docid 1)
    (mu4e-thread-unfold t)
    (let ((mu4e-thread-fold-unread nil))
      (mu4e-thread-fold t)
      (tessera-mu4e-headers--refresh)
      (mu4e~headers-goto-docid 3)
      (should-not (tessera-mu4e-thread-fold-at (point))))
    (tessera-mu4e-headers--disable)))

(ert-deftest tessera-mu4e-thread-orphan-and-missing-parent ()
  (tessera-mu4e-tests--with-thread
    (dolist (spec '((1 :level 2 :orphan t :first-child t)
                    (2 :level 4)
                    (3 :level 2 :orphan t :last-child t)
                    (4 :level 0 :root t)))
      (mu4e~headers-goto-docid (car spec))
      (let ((message (copy-sequence (mu4e-message-at-point))))
        (setq message (plist-put message :meta (cdr spec)))
        (put-text-property (line-beginning-position)
                           (line-end-position) 'msg message)))
    (tessera-mu4e-headers--refresh)
    (let ((head (gethash 1 tessera-mu4e-headers--threads)))
      (should (tessera-thread-context-first head))
      (should (= 3 (tessera-thread-context-total head)))
      (should (= 1 (tessera-thread-context-parent
                    (gethash 2 tessera-mu4e-headers--threads)))))
    (should (tessera-thread-context-first
             (gethash 4 tessera-mu4e-headers--threads)))
    (tessera-mu4e-headers--disable)))

(provide 'tessera-mu4e-thread-tests)
;;; tessera-mu4e-thread-tests.el ends here
