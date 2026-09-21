;;; tessera-mu4e-headers-tests.el --- Native rows -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercise native row identity, mark edits, and reversible layouts.

;;; Code:

(require 'ert)
(require 'mu4e-headers)
(require 'tessera-mu4e)
(require 'tessera-mu4e-headers)

(ert-deftest tessera-mu4e-semantic-slots-retain-coexisting-states ()
  (let* ((tessera-glyph-style 'ascii)
         (tessera-glyph-color t)
         (mu4e-headers-visible-flags
          '(draft trashed flagged replied passed personal list
                  attach signed encrypted calendar))
         (mu4e--mark-map (make-hash-table))
         (message '( :docid 42
                     :priority high
                     :flags (new unread draft trashed flagged
                                 replied passed personal list
                                 attach signed encrypted
                                 calendar)))
         (original (copy-tree message))
         (context (tessera-mu4e-headers--context message nil nil)))
    (puthash 42 '(move . "/archive") mu4e--mark-map)
    (with-temp-buffer
      (insert (propertize "Message" 'msg message))
      (dolist (spec
               '((status ?T tessera-glyph-negative-face
                         ("Trashed" "Draft" "New" "Unread") negative)
                 (priority ?H tessera-glyph-warning-face
                           ("High priority" "Flagged") warning)
                 (secondary ?R tessera-glyph-positive-face
                            ("Replied" "Forwarded" "Personal"
                             "Mailing list") positive)
                 (operation ?m tessera-glyph-attention-face
                            ("Move: /archive") attention)))
        (let* ((slot (tessera-mu4e-headers--slot (car spec)))
               (text (tessera--render-glyph-slot slot context t))
               (position (text-property-not-all
                          0 (length text)
                          'tessera-glyph nil text))
               (help (get-text-property position 'help-echo text)))
          (should (= (aref text position) (nth 1 spec)))
          (should (equal (get-text-property position 'face text)
                         (nth 2 spec)))
          (should (eq (get-text-property position 'tessera-glyph text)
                      t))
          (should (equal (funcall help nil (current-buffer) 1)
                         (string-join (nth 3 spec) "; ")))))
      (dolist (slot '(attach signed encrypted calendar))
        (should (eq slot (tessera-mu4e-headers--state slot context))))
      (let ((mu4e-headers-visible-flags nil))
        (should (eq 'new (tessera-mu4e-headers--state
                          'status context)))
        (should (eq 'high (tessera-mu4e-headers--state
                           'priority context)))
        (should-not (tessera-mu4e-headers--state 'secondary context))
        (should-not (tessera-mu4e-headers--state 'attach context))
        (should (equal (tessera-mu4e-headers--help
                        'status "New" nil (current-buffer) 1)
                       "New; Unread")))
      ;; Destructive pending marks must stand out from a normal move.
      (dolist (action '(trash delete))
        (puthash 42 (list action) mu4e--mark-map)
        (let* ((slot (tessera-mu4e-headers--slot 'operation))
               (text (tessera--render-glyph-slot slot context t))
               (position (text-property-not-all
                          0 (length text)
                          'tessera-glyph nil text)))
          (should (eq (get-text-property position 'tessera-glyph text)
                      t))
          (should
           (eq (get-text-property position 'face text)
               'tessera-glyph-negative-face))))
      (puthash 42 '(unread) mu4e--mark-map)
      (should (eq 'mark-unread (tessera-mu4e-headers--state
                                'operation context)))
      (remhash 42 mu4e--mark-map)
      (should-not (tessera-mu4e-headers--state 'operation context))
      (should (equal original message)))))

(ert-deftest tessera-mu4e-semantic-slots-select-fallbacks ()
  (let ((mu4e-headers-visible-flags
         '(draft trashed flagged replied passed personal list)))
    (dolist (spec '(((:flags (unread draft)) (draft nil nil))
                    ((:flags (unread)) (unread nil nil))
                    ((:flags (seen flagged) :priority low)
                     (seen flagged nil))
                    ((:flags (seen passed personal list))
                     (seen nil passed))
                    ((:flags (seen personal list))
                     (seen nil personal))
                    ((:flags (seen list)) (seen nil list))
                    ((:flags (seen) :priority low) (seen low nil))))
      (let ((context
             (tessera-mu4e-headers--context (car spec) nil nil)))
        (should
         (equal (mapcar
                 (lambda (slot)
                   (tessera-mu4e-headers--state slot context))
                 '(status priority secondary))
                (cadr spec)))))))

(ert-deftest tessera-mu4e-contacts-use-names-with-address-fallback ()
  (let ((mu4e-headers-from-or-to-prefix '("From: " . "To: ")))
    (cl-letf (((symbol-function 'mu4e-server-properties)
               (lambda () '(:personal-addresses
                            ("self@example.test")))))
      (dolist (spec
               '(((:name "Alex Jr." :email "alex@example.test")
                  nil "Alex Jr.")
                 ((:email "alex@example.test")
                  nil "alex@example.test")
                 ((:name "" :email "alex@example.test")
                  nil "alex@example.test")
                 ((:name "Self" :email "self@example.test")
                  ((:name "李明" :email "li@example.test")
                   (:name "José" :email "jose@example.test"))
                  "Self")
                 ((:name "Self" :email "self@example.test")
                  ((:email "li@example.test")) "Self")
                 ((:name "Self" :email "self@example.test")
                  nil "Self")
                 ((:email "self@example.test")
                  ((:email "li@example.test")) "self@example.test")))
        (let* ((message (list :from (list (car spec))
                              :to (cadr spec)
                              :flags '(unread)))
               (context
                (tessera-mu4e-headers--context message nil nil))
               (text (tessera-mu4e-headers--field 'contact context)))
          (should (equal text (nth 2 spec)))
          (should (equal (get-text-property 0 'face text)
                         'tessera-mu4e-headers-unread-contact-face))
          (should (string-match-p
                   (regexp-quote (plist-get (car spec) :email))
                   (get-text-property 0 'help-echo text)))
          (dolist (recipient (cadr spec))
            (should (string-match-p
                     (regexp-quote (plist-get recipient :email))
                     (get-text-property 0 'help-echo text))))
          (should (eq (get-text-property 0 'mouse-face text)
                      'tessera-entry-hover-face))))
      (should (equal mu4e-headers-from-or-to-prefix
                     '("From: " . "To: "))))))

(ert-deftest tessera-mu4e-headers-preserve-native-operations ()
  (let ((mu4e-search-threads nil)
        (mu4e-headers-mode-hook nil)
        (mu4e-headers-fields '((:subject)))
        (mu4e-search-hide-enabled nil)
        (message '( :docid 42
                    :subject "Native subject"
                    :from (( :name "Sender"
                             :email "a@example.test"))
                    :date (27000 0)
                    :flags (unread attach)
                    :priority high
                    :tags ("foo" "bar"))))
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
            (should (string-match-p "> /archive" (buffer-string)))
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
            (setq message (plist-put (copy-sequence message)
                                     :flags '(seen)))
            (mu4e~headers-insert-header message (point-min))
            (goto-char (point-min))
            (tessera-mu4e-headers--refresh)
            (should (equal message (mu4e-message-at-point)))
            (tessera-mu4e-headers--disable)
            (dolist (overlay (overlays-in (point-min) (point-max)))
              (should (eq (overlay-get overlay 'face)
                          hl-line-face)))))))))

(ert-deftest tessera-mu4e-marks-keep-layout-out-of-native-prefix ()
  (dolist (settings '((nil single-line) (nil two-line) (t two-line)
                      (nil single-line (flagged draft))
                      (t two-line (flagged draft))))
    (let ((mu4e-search-threads (car settings))
          (mu4e-headers-mode-hook nil)
          (mu4e-headers-fields '((:subject)))
          (mu4e-search-hide-enabled nil)
          (mu4e-headers-visible-flags '(flagged draft))
          (mu4e-headers-show-target t))
      (with-temp-buffer
        (mu4e-headers-mode)
        (let ((inhibit-read-only t)
              (buffer (current-buffer))
              width untouched)
          (cl-letf (((symbol-function 'mu4e-get-headers-buffer)
                     (lambda (&rest _) buffer)))
            (dotimes (index 4)
              (mu4e~headers-insert-header
               (list :docid (1+ index)
                     :subject "Subject"
                     :from '((:name "Sender" :email "a@example.test"))
                     :date '(27000 0)
                     :flags (cons 'unread (nth 2 settings))
                     :meta '(:level 0 :root t))
               (point-max)))
            (goto-char (point-min))
            (tessera-mu4e--enable-headers)
            (unwind-protect
                (progn
                  (setq-local tessera-entry-layout (cadr settings))
                  (tessera-mu4e-headers--refresh)
                  (setq width tessera-mu4e-headers--leading-width
                        untouched
                        (save-excursion
                          (mu4e~headers-goto-docid 4)
                          (get-text-property
                           (tessera-mu4e-headers--body-start)
                           'tessera--layout-overlay)))
                  (dolist (mark '((move . "/archive")
                                  (move . "/archive")
                                  (move . "/other")
                                  (unmark) (unmark)))
                    (dotimes (index 3)
                      (mu4e~headers-goto-docid (1+ index))
                      (mu4e-mark-at-point (car mark) (cdr mark))
                      (tessera-mu4e-headers--refresh)
                      (should
                       (= width tessera-mu4e-headers--leading-width))
                      ;; Mark edits must not redraw the whole column.
                      (should (overlay-buffer untouched))
                      (save-excursion
                        (goto-char (point-min))
                        (dotimes (row 4)
                          (should (= (1+ row)
                                     (mu4e~headers-docid-at-point)))
                          (let ((body
                                 (tessera-mu4e-headers--body-start)))
                            (should-not
                             (cl-some
                              (lambda (overlay)
                                (overlay-get
                                 overlay 'tessera-entry-overlay))
                              (overlays-in
                               (line-beginning-position) body)))
                            (dotimes (offset mu4e--mark-fringe-len)
                              (should
                               (equal
                                (get-text-property
                                 (- body 1 offset) 'display)
                                ""))))
                          (forward-line 1))))))
              (tessera-mu4e-headers--disable))))))))

(ert-deftest tessera-mu4e-headers-navigate-logical-entries ()
  (let ((mu4e-search-threads nil)
        (mu4e-headers-mode-hook nil)
        (mu4e-headers-fields '((:subject)))
        (mu4e-search-hide-enabled nil)
        (mu4e-headers-open-after-move nil))
    (with-temp-buffer
      (mu4e-headers-mode)
      (let ((inhibit-read-only t)
            (buffer (current-buffer)))
        (cl-letf (((symbol-function 'mu4e-get-headers-buffer)
                   (lambda (&rest _) buffer)))
          (dotimes (index 3)
            (mu4e~headers-insert-header
             (list :docid (1+ index)
                   :subject "Subject"
                   :from '((:name "Author" :email "a@example.test"))
                   :date '(27000 0)
                   :flags '(seen))
             (point-max)))
          (dolist (local '(nil t))
            (when local
              (setq-local line-move-ignore-invisible t))
            (let ((before line-move-ignore-invisible))
              (tessera-mu4e--enable-headers)
              (dolist (tessera-entry-layout '(single-line two-line))
                (tessera-mu4e-headers--refresh)
                (mu4e~headers-goto-docid 1)
                (tessera-mu4e-headers--position-point)
                (let ((point (point)))
                  (should-not
                   (call-interactively (key-binding "p")))
                  (should (= (point) point))
                  (should (= 1 (mu4e~headers-docid-at-point))))
                (mu4e~headers-goto-docid 3)
                (tessera-mu4e-headers--position-point)
                (let ((point (point)))
                  (should-not
                   (call-interactively (key-binding "n")))
                  (should (= (point) point))
                  (should (= 3 (mu4e~headers-docid-at-point))))
                (mu4e~headers-goto-docid 2)
                (should (= 1 (call-interactively (key-binding "p"))))
                (should (looking-at "Subject"))
                (should (= 2 (call-interactively (key-binding "n"))))
                (should (looking-at "Subject"))
                (should (= 3 (mu4e-headers-next)))
                (should (looking-at "Subject"))
                (should (= 1 (mu4e-headers-prev 2)))
                (should (looking-at "Subject"))
                (let* ((message
                        (copy-sequence (mu4e-message-at-point)))
                       (flags (if (memq 'seen
                                        (plist-get message :flags))
                                  '(unread replied flagged)
                                '(seen))))
                  (mu4e~headers-update-handler
                   (plist-put message :flags flags) nil nil)
                  (should (= 1 (mu4e~headers-docid-at-point)))
                  (should (looking-at "Subject"))
                  (should (= (point) (tessera-entry-point)))))
              (tessera-mu4e-headers--disable)
              (should (= 2 (mu4e-headers-next)))
              (should (= 2 (current-column)))
              (should (eq before line-move-ignore-invisible))
              (should (eq local (local-variable-p
                                 'line-move-ignore-invisible))))))))))

(ert-deftest tessera-mu4e-headers-labels-keep-neutral-separators ()
  (let* ((mu4e--mark-map (make-hash-table))
         (message '( :flags (seen)
                     :labels ("foo" "bar")
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
