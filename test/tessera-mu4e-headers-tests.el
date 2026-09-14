;;; tessera-mu4e-headers-tests.el --- Native rows -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercise native row identity, mark edits, and reversible layouts.

;;; Code:

(require 'ert)
(require 'mu4e-headers)
(require 'tessera-mu4e)
(require 'tessera-mu4e-headers)

(ert-deftest tessera-mu4e-headers-preserve-native-operations ()
  (let ((mu4e-search-threads nil)
        (mu4e-headers-mode-hook nil)
        (mu4e-headers-fields '((:subject)))
        (mu4e-search-hide-enabled nil)
        (message '(:docid 42 :subject "Native subject"
                          :from ((:name "Sender"
                                        :email "a@example.test"))
                          :date (27000 0) :flags (unread attach)
                          :priority high :tags ("foo" "bar"))))
    (with-temp-buffer
      (mu4e-headers-mode)
      (let ((inhibit-read-only t)
            (buffer (current-buffer)))
        (cl-letf (((symbol-function 'mu4e-get-headers-buffer)
                   (lambda (&rest _) buffer)))
          (mu4e~headers-insert-header message (point-min))
          (goto-char (point-min))
          (mu4e-mark-at-point 'unmark nil)
          (let ((native (buffer-string)))
            (tessera-mu4e--enable-headers)
            (dolist (layout '(single-line two-line))
              (setq-local tessera-entry-layout layout)
              (tessera-mu4e-headers--refresh)
              (should (= 1 (count-lines (point-min) (point-max))))
              (should (mu4e~headers-goto-docid 42))
              (should (equal message (mu4e-message-at-point)))
              (should (get-text-property
                       (tessera-mu4e-headers--body-start)
                       'tessera-mu4e-native)))
            (mu4e-mark-at-point 'move "/archive")
            (tessera-mu4e-headers--refresh)
            (should (equal (gethash 42 mu4e--mark-map)
                           '(move . "/archive")))
            (should (string-match-p "→ /archive" (buffer-string)))
            (tessera-mu4e-headers--disable)
            (should (cl-some (lambda (overlay)
                               (overlay-get overlay 'mu4e-mark))
                             (overlays-in (point-min) (point-max))))
            (mu4e-mark-at-point 'unmark nil)
            (should (equal-including-properties
                     native (buffer-string)))
            (tessera-mu4e--enable-headers)
            (let ((mu4e-search-threads t))
              (tessera-mu4e-headers--refresh)
              (should (string-match-p "1/1Native subject"
                                      (buffer-string))))
            (tessera-mu4e-headers--refresh)
            (mu4e~headers-remove-header 42)
            (setq message (plist-put message :flags '(seen)))
            (mu4e~headers-insert-header message (point-min))
            (goto-char (point-min))
            (tessera-mu4e-headers--refresh)
            (should (equal message (mu4e-message-at-point)))
            (tessera-mu4e-headers--disable)
            (dolist (overlay (overlays-in (point-min) (point-max)))
              (should (eq (overlay-get overlay 'face)
                          hl-line-face)))))))))

(ert-deftest tessera-mu4e-headers-labels-keep-neutral-separators ()
  (let* ((mu4e--mark-map (make-hash-table))
         (message '(:flags (seen) :labels ("foo" "bar")
                           :tags ("bar" "baz")))
         (context (tessera-mu4e-headers--context message nil nil))
         (text (tessera-mu4e-headers--field 'labels context)))
    (should (equal text "foo,bar,baz"))
    (should (equal (get-text-property 4 'help-echo text)
                   "bar (label, tag)"))
    (dolist (position '(3 7))
      (should (eq (get-text-property position 'mouse-face text)
                  'default))
      (should (eq (get-text-property position 'face text) 'default)))
    (should (equal (plist-get message :labels) '("foo" "bar")))))

(provide 'tessera-mu4e-headers-tests)
;;; tessera-mu4e-headers-tests.el ends here
