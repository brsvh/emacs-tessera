;;; tessera-x-tests.el --- Experimental feature regressions -*- lexical-binding: t; -*-

;;; Commentary:

;; Context snapshot ownership, budgets, MIME decoding and scope.
;; Context snapshots are one feature of the experimental package.

;;; Code:

(require 'ert)
(require 'tessera-x)
(require 'tessera-x-elfeed)
(require 'tessera-x-mu4e)
(require 'tessera-x-gnus)
(require 'tessera-gnus-test-support)
(require 'gnus-agent)
(require 'gnus-topic)
(require 'elfeed-search)
(require 'mu4e-headers)

(defvar gnus-registry-db)

(ert-deftest tessera-x-compiles-and-loads-without-native-clients ()
  (with-temp-buffer
    (let ((status
           (call-process
            (expand-file-name invocation-name invocation-directory)
            nil (current-buffer) nil "-Q" "--batch"
            "-l" (locate-library "run-x-tests")
            (file-name-directory
             (symbol-file 'tessera-x-context-start 'defun))
            (file-name-directory
             (symbol-file 'tessera-entry-render 'defun)))))
      (ert-info ((buffer-string))
        (should (equal status 0))))))

(ert-deftest tessera-x-gnus-labels-retain-native-metadata ()
  (let ((header (make-full-mail-header
                 1 "Subject" "Sender" "" "<id>" "" 0 0 nil
                 '((x-gm-labels . "(shared \"Gmail\")")
                   (Keywords . "shared, News,  ")
                   (TO . "Recipient <to@example.invalid>"))))
        (gnus-registry-db t))
    (cl-letf (((symbol-function 'gnus-registry-get-id-key)
               (lambda (_id _key) '(shared " Registry\nlabel "))))
      (let ((metadata (tessera-x-item-metadata
                       (tessera-x-gnus--item header "group"))))
        (should (equal (cdr (assoc "Labels" metadata))
                       "shared,Registry label,Gmail,News"))
        (should (equal (cdr (assoc "To" metadata))
                       "Recipient <to@example.invalid>")))
      (dolist (value '("#1=(x . #1#)" "(x . y)" "((nested))"))
        (setcdr (assq 'x-gm-labels (mail-header-extra header)) value)
        (let ((metadata (tessera-x-item-metadata
                         (tessera-x-gnus--item header "group"))))
          (should (equal (cdr (assoc "Labels" metadata))
                         "shared,Registry label,News")))))))

(ert-deftest tessera-x-mu4e-labels-share-native-normalization ()
  (let* ((message (list :labels (list "Work" "Later" "Work")
                        :tags (list "Later" "News")))
         (saved (copy-tree message))
         (labels (tessera-mu4e-headers-labels message))
         (metadata (tessera-x-item-metadata
                    (tessera-x-mu4e--item message))))
    (should (equal labels '("Work" "Later" "News")))
    (should (equal (cdr (assoc "Labels" metadata)) "Work,Later,News"))
    (setcar labels "Changed")
    (should (equal message saved))))

(ert-deftest tessera-x-options-belong-to-their-feature ()
  (dolist (entry '((tessera-x-gnus
                    body-policy subthread-scope)
                   (tessera-x-mu4e
                    subthread-scope today-query-function)
                   (tessera-x-elfeed
                    fetch-linked-content fetch-minimum-characters
                    fetch-timeout fetch-concurrency)))
    (let ((prefix (symbol-name (car entry))))
      (dolist (suffix (cdr entry))
        (let ((option
               (intern (concat prefix "-" (symbol-name suffix)))))
          (should (get option 'standard-value))
          (should
           (equal prefix
                  (file-name-base (symbol-file option 'defvar))))
          (should (assq option (get (car entry) 'custom-group))))))))

(defun tessera-x-tests--item (id &optional references)
  "Create a context record with ID and REFERENCES."
  (make-tessera-x-item
   :id id
   :message-id id
   :references references
   :subject "Full subject"
   :date '(27000 0)
   :metadata '(("From" . "Full Name <full@example.invalid>"))
   :body (make-string 1000 ?x)))

(defmacro tessera-x-tests--with-snapshots (&rest body)
  "Run BODY and clean up every newly created context buffer."
  (declare (indent 0) (debug t))
  `(let ((existing (buffer-list)))
     (unwind-protect (progn ,@body)
       (dolist (buffer (buffer-list))
         (when (and (not (memq buffer existing))
                    (buffer-local-value
                     'tessera-x-current-context buffer))
           (kill-buffer buffer))))))

(ert-deftest tessera-x-snapshots-isolate-sources-and-pending-work ()
  (tessera-x-tests--with-snapshots
    (let ((other (generate-new-buffer " *Other context source*"))
          (calls nil) old first cancelled last)
      (unwind-protect
          (with-temp-buffer
            (let ((source (current-buffer))
                  (tessera-x-context-ready-hook
                   (list (lambda (context)
                           (push (cons (current-buffer) context)
                                 calls)))))
              (setq old (tessera-x-context-start 'test "old" nil))
              (push (lambda () (setq cancelled t))
                    (tessera-x-context-cleanup old))
              (setq first (tessera-x-context-start 'test "first" nil))
              (tessera-x-context-finish old)
              (should cancelled)
              (should-not calls)
              (tessera-x-context-finish first)
              (tessera-x-context-finish first)
              (should (= (length calls) 1))
              (should (eq (caar calls) source))
              (with-current-buffer other
                (setq last (tessera-x-context-start
                            'test "other" nil))
                (tessera-x-context-finish last))
              (should (eq tessera-x-current-context first))
              (should (buffer-live-p
                       (tessera-x-context-buffer first)))
              (should-not (eq (tessera-x-context-buffer first)
                              (tessera-x-context-buffer last)))
              (let ((next (tessera-x-context-start 'test "new" nil)))
                (tessera-x-context-finish next)
                (should (buffer-live-p
                         (tessera-x-context-buffer first))))))
        (kill-buffer other)))))

(ert-deftest tessera-x-ready-hook-preserves-source-region ()
  (tessera-x-tests--with-snapshots
    (dolist (condition '(nil error quit))
      (save-window-excursion
        (with-temp-buffer
          (insert "source text")
          (goto-char 4)
          (push-mark 8 t t)
          (let* ((context (tessera-x-context-start 'test "hook" nil))
                 (window (selected-window))
                 (tessera-x-context-ready-hook
                  (list
                   (lambda (ready)
                     (goto-char (point-max))
                     (set-mark (point-min))
                     (setq mark-active nil)
                     (set-window-buffer
                      window (tessera-x-context-buffer ready))
                     (when condition
                       (signal condition '("Consumer failed"))))))
                 caught)
            (condition-case err
                (tessera-x-context-finish context)
              (quit (setq caught (car err))))
            (should (eq caught (and (eq condition 'quit) 'quit)))
            (should (eq (tessera-x-context-state context) 'ready))
            (should (= (point) 4))
            (should (= (mark) 8))
            (should mark-active)
            (should (eq (window-buffer window)
                        (tessera-x-context-buffer context)))))))))

(ert-deftest tessera-x-interrupted-rendering-releases-resources ()
  (tessera-x-tests--with-snapshots
    (with-temp-buffer
      (let ((ready (tessera-x-context-start 'test "ready" nil))
            (cleanup-count 0)
            rendered)
        (tessera-x-context-finish ready)
        (dolist (condition '(quit error))
          (let ((pending (tessera-x-context-start 'test "next" nil)))
            (push (lambda () (cl-incf cleanup-count))
                  (tessera-x-context-cleanup pending))
            (cl-letf (((symbol-function 'tessera-x--context-render)
                       (lambda (_context)
                         (setq rendered (current-buffer))
                         (signal condition nil))))
              (should
               (eq (condition-case err
                       (tessera-x-context-finish pending)
                     ((error quit) (car err)))
                   condition)))
            (should-not (buffer-live-p rendered))
            (should-not tessera-x--pending-context)
            (should-not (tessera-x-context-buffer pending))
            (should (eq (tessera-x-context-state pending)
                        (if (eq condition 'quit) 'cancelled 'failed)))
            (tessera-x-context-finish pending)
            (should (eq tessera-x-current-context ready))))
        (should (= cleanup-count 2))))))

(ert-deftest tessera-x-backend-failures-release-requests ()
  (tessera-x-tests--with-snapshots
    (dolist (condition '(quit error))
      (pcase-dolist (`(,backend ,function)
                     '((gnus tessera-x-gnus--download)
                       (gnus tessera-x-gnus--read-body)
                       (gnus tessera-x-group-threads)
                       (mu4e tessera-x-mu4e--read-body)
                       (mu4e tessera-x-group-threads)))
        (with-temp-buffer
          (let ((previous (tessera-x-context-start backend "Old" nil))
                (items (list (tessera-x-tests--item "one")))
                (gnus-agent t)
                (tessera-x-gnus-body-policy 'download)
                (cleanup-count 0)
                interrupted)
            (tessera-x-context-finish previous)
            (cl-letf (((symbol-function 'tessera-x-gnus--download)
                       #'ignore)
                      ((symbol-function 'tessera-x-gnus--read-body)
                       #'ignore)
                      ((symbol-function 'tessera-x-mu4e--read-body)
                       #'ignore))
              (cl-letf (((symbol-function function)
                         (lambda (&rest _)
                           (setq interrupted
                                 tessera-x--pending-context)
                           (push
                            (lambda () (cl-incf cleanup-count))
                            (tessera-x-context-cleanup interrupted))
                           (signal condition
                                   '("Preparation failed")))))
                (should
                 (eq (condition-case err
                         (progn
                           (if (eq backend 'gnus)
                               (tessera-x-gnus--build-context
                                items "Interrupted" nil)
                             (tessera-x-mu4e--finish-context
                              (tessera-x-context-start
                               backend "New" items)
                              items))
                           nil)
                       (quit (car err)))
                     (if (eq condition 'quit) 'quit nil)))))
            (should
             (eq (tessera-x-context-state interrupted)
                 (if (eq condition 'quit) 'cancelled 'failed)))
            (should-not tessera-x--pending-context)
            (should-not (tessera-x-context-cleanup interrupted))
            (should (= cleanup-count 1))
            (should (eq tessera-x-current-context previous))
            (should (buffer-live-p
                     (tessera-x-context-buffer previous)))))))))

(ert-deftest tessera-x-discard-respects-refused-buffer-deletion ()
  (tessera-x-tests--with-snapshots
    (with-temp-buffer
      (let ((context (tessera-x-context-start 'test "discard" nil)))
        (tessera-x-context-finish context)
        (let ((buffer (tessera-x-context-buffer context)))
          (with-current-buffer buffer
            (setq-local kill-buffer-query-functions
                        (list (lambda () nil))))
          (tessera-x-discard-context)
          (should (buffer-live-p buffer))
          (should (eq tessera-x-current-context context))
          (save-window-excursion
            (tessera-x-show-context)
            (should (eq (window-buffer) buffer)))
          (with-current-buffer buffer
            (setq-local kill-buffer-query-functions nil))
          (tessera-x-discard-context)
          (should-not (buffer-live-p buffer))
          (should-not tessera-x-current-context))))))

(ert-deftest tessera-x-closed-source-cancels-and-never-publishes ()
  (let ((source (generate-new-buffer " *Doomed source*"))
        context cancelled)
    (with-current-buffer source
      (setq context (tessera-x-context-start 'test "closed" nil))
      (push (lambda () (setq cancelled t))
            (tessera-x-context-cleanup context)))
    (kill-buffer source)
    (tessera-x-context-finish context)
    (should cancelled)
    (should (eq (tessera-x-context-state context) 'cancelled))
    (should-not (tessera-x-context-buffer context))))

(ert-deftest tessera-x-mode-change-cancels-pending-work ()
  (tessera-x-tests--with-snapshots
    (with-temp-buffer
      (let* ((ready (tessera-x-context-start 'test "ready" nil))
             (cleanup-count 0)
             pending)
        (tessera-x-context-finish ready)
        (setq pending (tessera-x-context-start 'test "pending" nil))
        (push (lambda () (cl-incf cleanup-count))
              (tessera-x-context-cleanup pending))
        (special-mode)
        (tessera-x-context-finish pending)
        (tessera-x-context-fail pending "Late failure")
        (tessera-x-cancel-context)
        (should (= cleanup-count 1))
        (should (eq (tessera-x-context-state pending) 'cancelled))
        (should-not (tessera-x-context-cleanup pending))
        (should-not (tessera-x-context-buffer pending))
        (should-not tessera-x--pending-context)
        (should (buffer-live-p (tessera-x-context-buffer ready)))
        (should (eq (tessera-x-context-state ready) 'ready))))))

(ert-deftest tessera-x-budget-keeps-metadata-and-is-snapshotted ()
  (tessera-x-tests--with-snapshots
    (with-temp-buffer
      (let* ((tessera-x-context-max-characters 700)
             (items (mapcar #'tessera-x-tests--item '("a" "b")))
             (context (tessera-x-context-start 'test "budget" items)))
        (setq tessera-x-context-max-characters 10000)
        (tessera-x-context-finish context)
        (with-current-buffer (tessera-x-context-buffer context)
          (should (<= (buffer-size) 700))
          (should (string-match-p "Message-ID: a" (buffer-string)))
          (should (string-match-p "Message-ID: b" (buffer-string)))
          (should (string-match-p "Body truncated" (buffer-string)))
          (should buffer-read-only)
          (should-not (text-properties-at (point-min))))))))

(ert-deftest tessera-x-mime-preserves-unicode-quotes-and-attachments
    ()
  (let ((item (tessera-x-tests--item "mime")))
    (tessera-x-read-message
     item
     (lambda ()
       (insert
        "MIME-Version: 1.0\n"
        "Content-Type: multipart/mixed; boundary=mixed\n\n"
        "--mixed\n"
        "Content-Type: multipart/alternative; boundary=alt\n\n"
        "--alt\nContent-Type: text/plain; charset=utf-8\n"
        "Content-Transfer-Encoding: quoted-printable\n\n"
        "caf=C3=A9\n"
        "=E6=97=A5=E6=9C=AC=E8=AA=9E and =E4=B8=AD=E6=96=87\n"
        "> Keep the quoted context\n--=20\nJos=C3=A9\n"
        "--alt\nContent-Type: text/html; charset=utf-8\n\n"
        "<p>HTML alternative</p>\n--alt--\n"
        "--mixed\nContent-Type: application/pdf\n"
        "Content-Disposition: attachment; filename=review.pdf\n"
        "Content-Transfer-Encoding: base64\n\nJVBERi0xLjQK\n"
        "--mixed\nContent-Type: text/calendar; charset=utf-8\n"
        "Content-Disposition: attachment; filename=review.ics\n\n"
        "BEGIN:VCALENDAR\nEND:VCALENDAR\n--mixed--\n")))
    (let ((body (tessera-x-item-body item))
          (attachments (cdr (assoc "Attachments / MIME parts"
                                   (tessera-x-item-metadata item)))))
      (should (string-match-p "café" body))
      (should (string-match-p "日本語 and 中文" body))
      (should (string-match-p "> Keep the quoted context" body))
      (should (string-match-p "José" body))
      (should-not (string-match-p "HTML alternative" body))
      (should-not (string-match-p "VCALENDAR" body))
      (should (string-match-p "review.pdf" attachments))
      (should (string-match-p "review.ics" attachments)))))

(ert-deftest tessera-x-mime-disposition-controls-named-text ()
  (dolist (type '("plain" "html"))
    (pcase-dolist
        (`(,name ,disposition ,inline)
         `((nil "inline; filename=body.txt" t)
           (nil "inline; filename*=utf-8''body%20text.txt" t)
           ("body.txt" "inline" t)
           ;; Native dissection defaults plain text to inline.
           ("body.txt" nil ,(equal type "plain"))
           (nil "attachment; filename=body.txt" nil)
           (nil "attachment" nil)
           (nil "x-review" nil)
           (nil "X-Review; filename=body.txt" nil)))
      (ert-info ((format "%s: %S / %S" type name disposition))
        (let ((item (make-tessera-x-item :id "named-body")))
          (tessera-x-read-message
           item
           (lambda ()
             (insert "Content-Type: text/" type)
             (when name (insert "; name=" name))
             (insert "\n")
             (when disposition
               (insert "Content-Disposition: " disposition "\n"))
             (insert "\n" (if (equal type "html")
                              "<p>Inline body</p>"
                            "Inline body"))))
          (if inline
              (progn
                (should (equal (tessera-x-item-body item)
                               "Inline body"))
                (should-not (tessera-x-item-note item))
                (should-not (assoc "Attachments / MIME parts"
                                   (tessera-x-item-metadata item))))
            (should-not (tessera-x-item-body item))
            (should (equal (tessera-x-item-note item)
                           "No extractable text body"))
            (should (assoc "Attachments / MIME parts"
                           (tessera-x-item-metadata item)))))))))

(ert-deftest tessera-x-mime-inline-images-retain-filenames ()
  (let ((item (make-tessera-x-item :id "inline-image")))
    (tessera-x-read-message
     item
     (lambda ()
       (insert "Content-Type: multipart/related; boundary=parts\n\n"
               "--parts\nContent-Type: text/html\n\n"
               "<p>Inline body</p>\n"
               "--parts\nContent-Type: image/png\n"
               "Content-Disposition: inline; filename=logo.png\n\n"
               "Image payload\n--parts--\n")))
    (should (equal (tessera-x-item-body item) "Inline body"))
    (should-not (tessera-x-item-note item))
    (should (equal (cdr (assoc "Attachments / MIME parts"
                               (tessera-x-item-metadata item)))
                   "logo.png (image/png)"))))

(ert-deftest tessera-x-mime-alternatives-fall-back-to-readable-text ()
  (dolist (plain '("" " \t\n" "Readable plain text"))
    (let ((item (make-tessera-x-item :id "alternative"))
          (html-renderer (symbol-function 'tessera-x-html-text))
          (html-calls 0))
      (cl-letf (((symbol-function 'tessera-x-html-text)
                 (lambda (html)
                   (cl-incf html-calls)
                   (funcall html-renderer html))))
        (tessera-x-read-message
         item
         (lambda ()
           (insert
            "Content-Type: multipart/alternative; boundary=alt\n\n"
            "--alt\nContent-Type: text/html\n\n"
            "<p>Readable HTML text</p>\n"
            "--alt\nContent-Type: text/plain\n\n\n"
            "--alt\nContent-Type: text/plain\n\n"
            plain "\n--alt--\n"))))
      (if (string-blank-p plain)
          (progn
            (should (equal (tessera-x-item-body item)
                           "Readable HTML text"))
            (should (= html-calls 1)))
        (should (equal (tessera-x-item-body item) plain))
        (should (= html-calls 0)))
      (should-not (tessera-x-item-note item)))))

(ert-deftest tessera-x-mime-alternatives-try-nested-bodies ()
  (let ((item (make-tessera-x-item :id "nested")))
    (tessera-x-read-message
     item
     (lambda ()
       (insert
        "Content-Type: multipart/alternative; boundary=alt\n\n"
        "--alt\nContent-Type: multipart/mixed; boundary=empty\n\n"
        "--empty\nContent-Type: text/plain\n\n \n"
        "--empty\nContent-Type: text/plain\n\n \n--empty--\n"
        "--alt\nContent-Type: application/rtf\n\nRTF payload\n"
        "--alt\nContent-Type: multipart/related; boundary=rel\n\n"
        "--rel\nContent-Type: text/html\n\n"
        "<p>Nested HTML text</p>\n"
        "--rel\nContent-Type: image/png\n\nImage payload\n"
        "--rel--\n--alt--\n")))
    (should (equal (tessera-x-item-body item) "Nested HTML text"))
    (should-not (tessera-x-item-note item))
    (should (string-match-p
             "image/png"
             (cdr (assoc "Attachments / MIME parts"
                         (tessera-x-item-metadata item)))))))

(ert-deftest tessera-x-mime-alternatives-retain-attachment-metadata ()
  (dolist (html '("<p>Readable body</p>" "<p> </p>"))
    (let ((item (make-tessera-x-item :id "attachment")))
      (tessera-x-read-message
       item
       (lambda ()
         (insert
          "Content-Type: multipart/alternative; boundary=alt\n\n"
          "--alt\nContent-Type: text/plain\n"
          "Content-Disposition: attachment; filename=notes.txt\n\n"
          "Attachment text\n--alt\nContent-Type: text/html\n\n"
          html "\n--alt--\n")))
      (should (equal (cdr (assoc "Attachments / MIME parts"
                                 (tessera-x-item-metadata item)))
                     "notes.txt (text/plain)"))
      (if (string-match-p "Readable" html)
          (progn
            (should (equal (tessera-x-item-body item)
                           "Readable body"))
            (should-not (tessera-x-item-note item)))
        (should (string-empty-p (or (tessera-x-item-body item) "")))
        (should (equal (tessera-x-item-note item)
                       "No extractable text body"))))))

(ert-deftest tessera-x-mime-normalizes-wire-line-endings ()
  (dolist (multipart '(nil t))
    (dolist (ending '("\n" "\r\n"))
      (let* ((item (make-tessera-x-item :id "line-endings"))
             (body "First\rpart\ncafé")
             (part (concat "Content-Type: text/plain; charset=utf-8\n"
                           "Content-Transfer-Encoding: 8bit\n\n"
                           body "\n"))
             (raw
              (concat
               "From: Sender <sender@example.invalid>\n"
               "MIME-Version: 1.0\n"
               (if multipart
                   (concat
                    "Content-Type: multipart/mixed;"
                    " boundary=outer\n\n"
                    "--outer\n" part
                    "--outer\nContent-Type: application/pdf\n"
                    "Content-Disposition: attachment;\n"
                    " filename=review.pdf\n"
                    "Content-Transfer-Encoding: base64\n\n"
                    "JVBERi0xLjQK\n--outer--\n")
                 part)))
             (wire (encode-coding-string
                    (replace-regexp-in-string "\n" ending raw t t)
                    'utf-8)))
        (tessera-x-read-message item (lambda () (insert wire)))
        (should (equal (tessera-x-item-body item) body))
        (should-not (tessera-x-item-note item))
        (let ((metadata (tessera-x-item-metadata item)))
          (should (equal (cdr (assoc "From" metadata))
                         "Sender <sender@example.invalid>"))
          (should (equal (cdr (assoc "Attachments / MIME parts"
                                     metadata))
                         (and multipart
                              "review.pdf (application/pdf)"))))))))

(ert-deftest tessera-x-mime-decodes-utf8-and-encoded-headers ()
  (dolist (case
           '(("Sender" "Sender")
             ("作者" "作者")
             ("=?utf-8?B?5L2c6ICF?=" "作者")
             ("=?iso-8859-1?Q?Andr=E9?=" "André")
             ("作者 =?iso-8859-1?Q?Andr=E9?= 中文"
              "作者 André 中文")))
    (let* ((mail-parse-charset nil)
           (fields '("From" "To" "Cc" "Keywords" "X-GM-LABELS"))
           (metadata (mapcar (lambda (field)
                               (cons field (cadr case)))
                             fields))
           (item (make-tessera-x-item
                  :id "header-encoding"
                  :metadata (copy-tree metadata)))
           (headers (mapconcat (lambda (field)
                                 (concat field ": " (car case)))
                               fields "\n"))
           (wire (concat
                  (encode-coding-string
                   (concat headers "\nMIME-Version: 1.0\n"
                           "Content-Type: text/plain; "
                           "charset=iso-8859-1\n"
                           "Content-Transfer-Encoding: 8bit\n\n")
                   'utf-8)
                  (encode-coding-string "café\n" 'iso-8859-1))))
      (tessera-x-read-message item (lambda () (insert wire)))
      (should (equal (tessera-x-item-metadata item) metadata))
      (should (equal (tessera-x-item-body item) "café"))
      (should-not (tessera-x-item-note item)))))

(ert-deftest tessera-x-mime-failures-release-partial-buffers ()
  (dolist (condition '(nil error quit))
    (let ((item (make-tessera-x-item :id "interrupted"))
          (copy (symbol-function 'mm-copy-to-buffer))
          (unrelated (generate-new-buffer " *mm*"))
          (calls 0)
          buffers)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'mm-copy-to-buffer)
                       (lambda ()
                         (let ((buffer (funcall copy)))
                           (push buffer buffers)
                           (when (and (= (cl-incf calls) 3) condition)
                             (signal condition '("MIME interrupted")))
                           buffer))))
              (should
               (eq (condition-case err
                       (progn
                         (tessera-x-read-message
                          item
                          (lambda ()
                            (insert
                             "From: Sender <sender@example.invalid>\n"
                             "Content-Type: multipart/mixed;"
                             " boundary=outer\n\n"
                             "--outer\nContent-Type: text/plain\n\n"
                             "First body\n"
                             "--outer\nContent-Type: text/plain\n\n"
                             "Second body\n--outer--\n")))
                         nil)
                     (quit (car err)))
                   (and (eq condition 'quit) 'quit))))
            (should (= (length buffers) 3))
            (should-not (cl-some #'buffer-live-p buffers))
            (should (buffer-live-p unrelated))
            (should
             (equal
              (cdr (assoc "From" (tessera-x-item-metadata item)))
              "Sender <sender@example.invalid>"))
            (when (eq condition 'error)
              (should (string-match-p "MIME interrupted"
                                      (tessera-x-item-note item))))
            (unless condition
              (should (equal (tessera-x-item-body item)
                             "First body\n\nSecond body"))))
        (dolist (buffer (cons unrelated buffers))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest tessera-x-multipart-attachments-stay-out-of-bodies ()
  (pcase-dolist
      (`(,type ,header)
       '(("mixed" "multipart/mixed")
         ("alternative" "multipart/alternative")
         ("mixed" "(note) multipart/mixed")
         ("alternative" "(note) multipart/alternative")
         ("mixed" "(note)\n Multipart/Mixed")))
    (let ((item (tessera-x-tests--item "multipart")))
      (tessera-x-read-message
       item
       (lambda ()
         (insert
          "MIME-Version: 1.0\n"
          "Content-Type: multipart/mixed; boundary=outer\n\n"
          "--outer\nContent-Type: text/plain\n\nMain body\n"
          "--outer\nContent-Type: " header
          "; boundary=inner\nContent-Disposition: attachment;\n"
          " filename*=utf-8''attached%20mail.mime\n\n"
          "--inner\nContent-Type: text/plain\n\nAttachment body\n"
          "--inner--\n--outer--\n")))
      (should (equal "Main body" (tessera-x-item-body item)))
      (should
       (equal (cdr (assoc "Attachments / MIME parts"
                          (tessera-x-item-metadata item)))
              (format "attached mail.mime (multipart/%s)" type))))))

(ert-deftest tessera-x-multipart-dispositions-control-bodies ()
  (dolist (type '("mixed" "alternative"))
    (dolist (disposition '(nil "inline" "attachment" "x-review"))
      (let ((item (make-tessera-x-item :id "multipart")))
        (tessera-x-read-message
         item
         (lambda ()
           (insert "Content-Type: multipart/" type "; boundary=x\n")
           (when disposition
             (insert "Content-Disposition: " disposition "\n"))
           (insert "\n--x\nContent-Type: text/plain\n\n"
                   "Container body\n--x--\n")))
        (if (member disposition '(nil "inline"))
            (progn
              (should (equal (tessera-x-item-body item)
                             "Container body"))
              (should-not (assoc "Attachments / MIME parts"
                                 (tessera-x-item-metadata item))))
          (should-not (tessera-x-item-body item))
          (should
           (equal (cdr (assoc "Attachments / MIME parts"
                              (tessera-x-item-metadata item)))
                  (format "unnamed attachment (multipart/%s)"
                          type))))))))

(ert-deftest tessera-x-inline-multipart-filenames-preserve-bodies ()
  (dolist (type '("mixed" "alternative"))
    (let ((item (make-tessera-x-item :id "inline-multipart")))
      (tessera-x-read-message
       item
       (lambda ()
         (insert
          "Content-Type: multipart/" type "; boundary=inner\n"
          "Content-Disposition: inline; filename=body.mime\n\n"
          "--inner\nContent-Type: text/plain\n\nInline body\n"
          "--inner\nContent-Type: text/plain\n"
          "Content-Disposition: attachment; filename=notes.txt\n\n"
          "Attachment body\n--inner--\n")))
      (should (equal (tessera-x-item-body item) "Inline body"))
      (should-not (tessera-x-item-note item))
      (when (equal type "mixed")
        (should (equal (cdr (assoc "Attachments / MIME parts"
                                   (tessera-x-item-metadata item)))
                       "notes.txt (text/plain)"))))))

(ert-deftest tessera-x-mime-archives-remain-metadata ()
  (let* ((decoders '(("application/ms-tnef" t "tnef" "-f" "-" "-C")
                     ("application/zip" t "unzip" "-x" "%f" "-d")))
         (mm-archive-decoders (copy-tree decoders))
         (item (make-tessera-x-item :id "archives"))
         (calls 0))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (&rest _) "/available/decoder"))
              ((symbol-function 'mm-dissect-archive)
               (lambda (handle)
                 (cl-incf calls)
                 handle)))
      (tessera-x-read-message
       item
       (lambda ()
         (insert
          "Content-Type: multipart/mixed; boundary=outer\n\n"
          "--outer\nContent-Type: text/plain\n\nMain body\n"
          "--outer\nContent-Type: application/ms-tnef\n"
          "Content-Disposition: attachment; filename=winmail.dat\n\n"
          "TNEF payload\n"
          "--outer\nContent-Type: application/zip\n"
          "Content-Disposition: attachment; filename=archive.zip\n\n"
          "ZIP payload\n--outer--\n"))))
    (should (= calls 0))
    (should (equal mm-archive-decoders decoders))
    (should (equal (tessera-x-item-body item) "Main body"))
    (should-not (tessera-x-item-note item))
    (should
     (equal
      (cdr (assoc "Attachments / MIME parts"
                  (tessera-x-item-metadata item)))
      (concat "winmail.dat (application/ms-tnef), "
              "archive.zip (application/zip)")))))

(ert-deftest tessera-x-encrypted-body-never-runs-crypto ()
  (let ((item (tessera-x-tests--item "encrypted"))
        (mm-decrypt-option 'always))
    (cl-letf (((symbol-function 'mml2015-decrypt)
               (lambda (&rest _) (ert-fail "Unexpected decryption"))))
      (tessera-x-read-message
       item
       (lambda ()
         (insert
          "MIME-Version: 1.0\n"
          "Content-Type: multipart/encrypted; boundary=encrypted;\n"
          " protocol=\"application/pgp-encrypted\"\n\n"
          "--encrypted\nContent-Type: application/pgp-encrypted\n\n"
          "Version: 1\n"
          "--encrypted\nContent-Type: application/octet-stream\n\n"
          "Opaque test payload\n--encrypted--\n"))))
    (should (string-match-p "not decrypted"
                            (tessera-x-item-body item)))))

(ert-deftest tessera-x-message-ids-preserve-reference-boundaries ()
  (let* ((root (tessera-x-tests--item "root@example"))
         (child (tessera-x-tests--item
                 "child@example"
                 (tessera-x-message-ids
                  "(parent) <root@example><missing@example>")))
         (other (tessera-x-tests--item
                 "other@example"
                 (tessera-x-message-ids
                  "(parent) <unrelated@example>"))))
    (should (equal (tessera-x-item-references child)
                   '("root@example" "missing@example")))
    (should (equal (tessera-x-subthread (list root child other) root)
                   (list root child)))
    (setf (tessera-x-item-subject other) "Unrelated")
    (tessera-x-group-threads (list root child other))
    (should (equal (tessera-x-item-group other) "Unrelated")))
  (should
   (equal (tessera-x-message-ids
           ["(comment <fake@example>) <one@example>\r\n <two@example>"
            "bare@example" nil "(comment only)"])
          '("one@example" "two@example" "bare@example"))))

(ert-deftest tessera-x-subthreads-handle-missing-parents-and-cycles ()
  (let* ((root (tessera-x-tests--item "root"))
         (child (tessera-x-tests--item "child" '("root" "missing")))
         (grand (tessera-x-tests--item "grand" '("child")))
         (sibling (tessera-x-tests--item "sibling" '("root")))
         (other (tessera-x-tests--item "other"))
         (items (list grand child other sibling root)))
    (should (equal (tessera-x-subthread items child)
                   (list grand child)))
    (setf (tessera-x-item-references root) '("grand"))
    (should (= (length (tessera-x-subthread items root)) 4))
    (should (= (length (tessera-x-group-threads items)) 5))
    (should (equal (tessera-x-item-group other) "Full subject"))))

(ert-deftest tessera-x-mu4e-native-selection-includes-folded-rows ()
  (let ((mu4e-headers-mode-hook nil)
        (mu4e-headers-fields '((:subject)))
        (mu4e-search-threads t)
        (mu4e-search-hide-enabled nil))
    (with-temp-buffer
      (mu4e-headers-mode)
      (let ((inhibit-read-only t)
            (source (current-buffer)))
        (cl-letf (((symbol-function 'mu4e-get-headers-buffer)
                   (lambda (&rest _) source)))
          (dotimes (index 5)
            (mu4e~headers-insert-header
             (list :docid (1+ index)
                   :subject "Full subject"
                   :message-id (unless (= index 2)
                                 (format "%d" (1+ index)))
                   :meta (list :level (nth index '(0 1 0 1 2))
                               :root (memq index '(0 2)))
                   :date '(27000 0)
                   :flags '(unread))
             (point-max))))
        (goto-char (point-min))
        (let ((overlay (make-overlay (point-min) (point-max))))
          (overlay-put overlay 'invisible t))
        (puthash 2 '(flag . nil) mu4e--mark-map)
        (puthash 4 '(flag . nil) mu4e--mark-map)
        (let ((all (tessera-x-mu4e--items))
              (factory (symbol-function 'tessera-x-mu4e--item))
              (count 0) items)
          (cl-letf (((symbol-function 'tessera-x-mu4e--item)
                     (lambda (message)
                       (cl-incf count)
                       (funcall factory message))))
            (setq items (tessera-x-mu4e--items t)))
          (should (= (length all) 5))
          (should (= count 2))
          (should (equal (mapcar #'tessera-x-item-id items) '(2 4)))
          (should (equal items (list (nth 1 all) (nth 3 all))))
          (should (equal (mapcar #'tessera-x-item-parent items)
                         '("1" 3)))
          (should-not (tessera-x-item-references (cadr items))))
        (should (equal (gethash 2 mu4e--mark-map) '(flag)))
        (should (= (point) (point-min)))
        (save-restriction
          (narrow-to-region (line-beginning-position 3)
                            (line-beginning-position 4))
          (let ((start (point-min))
                (end (point-max))
                (position (point))
                (items (tessera-x-mu4e--items t)))
            (should (= (length (tessera-x-mu4e--items)) 5))
            (should (equal (mapcar #'tessera-x-item-id items) '(2 4)))
            (should (equal (mapcar #'tessera-x-item-parent items)
                           '("1" 3)))
            (should (= (point-min) start))
            (should (= (point-max) end))
            (should (= (point) position))))
        (goto-char (point-min))
        (clrhash mu4e--mark-map)
        (should (equal (mapcar #'tessera-x-item-id
                               (tessera-x-mu4e--items t)) '(1)))
        (let ((transient-mark-mode t))
          (forward-line 2)
          (push-mark (line-end-position 2) t t)
          (let ((before (point)) (mark-before (mark)))
            (should (equal (mapcar #'tessera-x-item-id
                                   (tessera-x-mu4e--items t)) '(3 4)))
            (should (= (point) before))
            (should (= (mark) mark-before))))))))

(ert-deftest tessera-x-gnus-download-marks-respect-narrowing ()
  (with-temp-buffer
    (gnus-summary-mode)
    (let ((inhibit-read-only t))
      (tessera-tests--gnus-rows '(0 0 0)))
    (setq-local gnus-newsgroup-name "group")
    (setq-local gnus-newsgroup-undownloaded '(1 2 3))
    (setq-local gnus-newsgroup-agentized t)
    (setq-local gnus-summary-mark-positions '((download . 2)))
    (let ((gnus-summary-default-score nil))
      (dolist (number '(1 2 3))
        (gnus-summary-goto-subject number nil t)
        (gnus-summary-update-download-mark number))
      (goto-char (point-min))
      (forward-line 1)
      (narrow-to-region (point) (line-beginning-position 2))
      (let ((start (point-min))
            (end (point-max))
            (position (point))
            (visible (buffer-string)))
        (cl-letf (((symbol-function 'tessera-x-gnus--available-p)
                   (lambda (_item) t)))
          (tessera-x-gnus--refresh-downloads
           (mapcar (lambda (number)
                     (make-tessera-x-item :id (cons "group" number)))
                   '(1 3))))
        (should (= (point-min) start))
        (should (= (point-max) end))
        (should (= (point) position))
        (should (equal (buffer-string) visible))
        (should (equal gnus-newsgroup-undownloaded '(2)))
        (save-restriction
          (widen)
          (dolist (number '(1 2 3))
            (gnus-summary-goto-subject number nil t)
            (should
             (eq (char-after (+ (line-beginning-position) 2))
                 (if (= number 2)
                     gnus-undownloaded-mark
                   gnus-downloaded-mark)))))))))

(ert-deftest tessera-x-gnus-agent-policy-keeps-local-today-offline ()
  (tessera-x-tests--with-snapshots
    (let* ((directory (make-temp-file "tessera-agent-" t))
           (gnus-agent t)
           (tessera-x-gnus-body-policy 'download)
           (item (tessera-x-tests--item "agent"))
           (missing (tessera-x-tests--item "missing"))
           fetched refreshed)
      (setf (tessera-x-item-id item) '("group" . 1)
            (tessera-x-item-id missing) '("group" . 2))
      (unwind-protect
          (cl-letf
              (((symbol-function 'tessera-x-gnus--agent-file)
                (lambda (_group name)
                  (expand-file-name name directory)))
               ((symbol-function 'gnus-find-method-for-group)
                (lambda (_) '(nnmaildir "fixture")))
               ((symbol-function 'gnus-agent-method-p)
                (lambda (_) t))
               ((symbol-function 'gnus-agent-fetch-articles)
                (lambda (_group numbers)
                  (setq fetched numbers)
                  (with-temp-file (expand-file-name "1" directory)
                    (insert
                     "Content-Type: text/plain; charset=utf-8\n"
                     "Content-Transfer-Encoding: quoted-printable\n\n"
                     "caf=C3=A9\n"))
                  (error "Second article unavailable")))
               ((symbol-function 'gnus-summary-update-download-mark)
                (lambda (number) (push number refreshed)))
               ((symbol-function 'gnus-agent-request-article)
                (lambda (_number _group)
                  (insert-file-contents-literally
                   (expand-file-name "1" directory)) t)))
            (with-temp-buffer
              (let ((context (tessera-x-gnus--build-context
                              (list item) "Today" t)))
                (should-not fetched)
                (should (eq (tessera-x-context-state context) 'ready))
                (should (string-match-p
                         "absent" (tessera-x-item-note item))))
              (setq-local major-mode 'gnus-summary-mode)
              (setq-local gnus-summary-buffer (current-buffer))
              (setq-local gnus-newsgroup-name "group")
              (tessera-tests--gnus-rows '(0 1))
              (setq-local gnus-newsgroup-undownloaded '(1 2))
              (tessera-x-gnus--build-context
               (list item missing) "Selected" nil)
              (should (equal fetched '(1 2)))
              (should (equal refreshed '(1)))
              (should (equal gnus-newsgroup-undownloaded '(2)))
              (should (= (point) (point-min)))
              (should-not (tessera-x-item-note item))
              (should (string-match-p
                       "Second article unavailable"
                       (tessera-x-item-note missing)))
              (should (string-match-p "café"
                                      (tessera-x-item-body item)))))
        (delete-directory directory t)))))

(ert-deftest tessera-x-gnus-expanded-subthreads-preserve-order ()
  (let* ((gnus-newsgroup-name "group")
         (items (cl-loop for id from 1 to 3
                         collect
                         (make-tessera-x-item
                          :id (cons "group" id)
                          :message-id (number-to-string id)
                          :references (and (> id 1) '("1"))
                          :subject (format "Subject %d" id)))))
    (dolist (loaded (list (list (car items))
                          (list (car items) (nth 2 items))))
      (cl-letf (((symbol-function 'tessera-x-gnus--items)
                 (lambda () (copy-sequence loaded)))
                ((symbol-function 'gnus-summary-article-number)
                 (lambda () 1))
                ((symbol-function 'tessera-x-gnus--overview)
                 (lambda (_groups)
                   (append items (list (cadr items)))))
                ((symbol-function 'tessera-x-gnus--build-context)
                 (lambda (selected _scope _local-only)
                   (tessera-x-group-threads selected))))
        (let ((selected (tessera-x-gnus-prepare-subthread-context t)))
          (should (equal (mapcar #'tessera-x-item-message-id selected)
                         (if (cdr loaded) '("1" "3" "2")
                           '("1" "2" "3"))))
          (dolist (item selected)
            (should (equal (tessera-x-item-group item)
                           "Subject 1"))))))))

(ert-deftest tessera-x-gnus-selection-uses-native-data-without-layout
    ()
  (with-temp-buffer
    (setq-local major-mode 'gnus-summary-mode)
    (setq-local gnus-summary-buffer (current-buffer))
    (setq-local gnus-newsgroup-name "example")
    (tessera-tests--gnus-rows '(0 1 0 1 2))
    (setq-local gnus-show-threads t)
    (setq-local gnus-newsgroup-processable '(1 4))
    (setq-local gnus-newsgroup-process-stack '((2 3)))
    (let ((all (tessera-x-gnus--items))
          (factory (symbol-function 'tessera-x-gnus--item))
          (count 0) items)
      (cl-letf (((symbol-function 'tessera-x-gnus--item)
                 (lambda (header group)
                   (cl-incf count)
                   (funcall factory header group))))
        (setq items (tessera-x-gnus--items t)))
      (should (= count 2))
      (should (equal gnus-newsgroup-process-stack '((2 3))))
      (should (equal gnus-newsgroup-processable '(1 4)))
      (should (equal (tessera-x-gnus--items t) items))
      (should (equal gnus-newsgroup-process-stack '((2 3))))
      (should (equal (mapcar (lambda (item)
                               (cdr (tessera-x-item-id item)))
                             items) '(1 4)))
      (should (equal items (list (car all) (nth 3 all))))
      (should (equal (tessera-x-item-parent (cadr items))
                     "thread-3@test.invalid"))
      (should-not (tessera-x-item-references (cadr items)))
      (should (equal (tessera-x-subthread all (nth 2 all))
                     (nthcdr 2 all))))
    (should (= (point) (point-min)))))

(ert-deftest tessera-x-elfeed-timeout-and-late-callback-finish-once ()
  (tessera-x-tests--with-snapshots
    (with-temp-buffer
      (let* ((item (tessera-x-tests--item "feed"))
             (context
              (tessera-x-context-start 'elfeed "test" (list item)))
             (request
              (make-tessera-x-elfeed--request :context context))
             (fetch (make-tessera-x-elfeed--fetch
                     :request request
                     :item item))
             (calls 0)
             (tessera-x-context-ready-hook
              (list (lambda (_) (cl-incf calls)))))
        (setf (tessera-x-elfeed--request-active request) (list fetch))
        (tessera-x-elfeed--timeout fetch)
        (should (= calls 1))
        (should (string-match-p "HTTP timeout"
                                (tessera-x-item-note item)))
        (tessera-x-elfeed--complete fetch "late response" nil)
        (should (= calls 1))
        (should (= (length (tessera-x-item-body item)) 1000))))))

(ert-deftest tessera-x-elfeed-redirects-release-all-transfers ()
  (pcase-dolist (`(,cancel ,reclaim)
                 '((nil nil) (t nil) (nil t) (t t)))
    (tessera-x-tests--with-snapshots
      (with-temp-buffer
        (let* ((item (tessera-x-tests--item "redirect"))
               (context (tessera-x-context-start
                         'elfeed "redirect" (list item)))
               (request
                (make-tessera-x-elfeed--request :context context))
               (url-dead-buffer-list nil)
               (buffers (cl-loop repeat 3 collect
                                 (generate-new-buffer " *Redirect*")))
               (process (make-pipe-process
                         :name "tessera-redirect"
                         :noquery t
                         :buffer (car (last buffers))))
               (fetch (make-tessera-x-elfeed--fetch
                       :request request
                       :item item
                       :buffer (car buffers))))
          (unwind-protect
              (progn
                (cl-loop for (buffer next) on buffers
                         do (with-current-buffer buffer
                              (setq-local
                               url-redirect-buffer next
                               url-callback-function
                               #'tessera-x-elfeed--response
                               url-callback-arguments
                               (list nil fetch))))
                (when reclaim
                  (url-mark-buffer-as-dead (car buffers))
                  (url-gc-dead-buffers)
                  (should-not (buffer-live-p (car buffers))))
                (setf (tessera-x-elfeed--request-active request)
                      (list fetch))
                (push (apply-partially
                       #'tessera-x-elfeed--cancel request)
                      (tessera-x-context-cleanup context))
                (if cancel
                    (tessera-x-cancel-context)
                  (tessera-x-elfeed--timeout fetch))
                (should (eq (tessera-x-context-state context)
                            (if cancel 'cancelled 'ready)))
                (should-not (tessera-x-elfeed--request-active
                             request))
                (should-not (cl-some #'buffer-live-p buffers))
                (should-not (process-live-p process))
                (tessera-x-elfeed--stop-fetch fetch))
            (when (process-live-p process) (delete-process process))
            (dolist (buffer buffers)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))))))))

(ert-deftest tessera-x-elfeed-stop-keeps-unrelated-transfers ()
  (with-temp-buffer
    (let* ((other (make-tessera-x-elfeed--fetch))
           (fetch (make-tessera-x-elfeed--fetch))
           (buffer (current-buffer)))
      (setq-local url-callback-function #'tessera-x-elfeed--response
                  url-callback-arguments (list nil other))
      (tessera-x-elfeed--stop-fetch fetch)
      (should (buffer-live-p buffer)))))

(ert-deftest tessera-x-elfeed-startup-quit-cancels-transfers ()
  (tessera-x-tests--with-snapshots
    (with-temp-buffer
      (let* ((previous (tessera-x-context-start 'elfeed "Old" nil))
             (items (cl-loop repeat 3 collect
                             (tessera-x-tests--item "feed")))
             (calls 0)
             buffers fetches timer)
        (tessera-x-context-finish previous)
        (let* ((context
                (tessera-x-context-start 'elfeed "New" items))
               (request (make-tessera-x-elfeed--request
                         :context context
                         :queue (copy-sequence items)
                         :timeout 60)))
          (push (apply-partially #'tessera-x-elfeed--cancel request)
                (tessera-x-context-cleanup context))
          (unwind-protect
              (progn
                (cl-letf (((symbol-function 'url-retrieve)
                           (lambda (_url callback arguments &rest _)
                             (cl-incf calls)
                             (push (car arguments) fetches)
                             (push (generate-new-buffer " *HTTP*")
                                   buffers)
                             (with-current-buffer (car buffers)
                               (setq-local
                                url-callback-function callback
                                url-callback-arguments
                                (cons nil arguments)))
                             (when (= calls 2)
                               (setq timer
                                     (tessera-x-elfeed--fetch-timer
                                      (cadr fetches)))
                               (signal 'quit nil))
                             (car buffers))))
                  (should
                   (eq (condition-case err
                           (tessera-x-elfeed--dispatch request)
                         (quit (car err)))
                       'quit)))
                (should (= calls 2))
                (should (eq (tessera-x-context-state context)
                            'cancelled))
                (should-not tessera-x--pending-context)
                (should-not (tessera-x-context-cleanup context))
                (should-not
                 (tessera-x-elfeed--request-active request))
                (should-not (tessera-x-elfeed--request-queue request))
                (should-not (tessera-x-elfeed--request-dispatching
                             request))
                (should (cl-every #'tessera-x-elfeed--fetch-done
                                  fetches))
                (should-not (cl-some #'buffer-live-p buffers))
                (should (timerp timer))
                (should-not (memq timer timer-list))
                (should (eq tessera-x-current-context previous))
                (should (buffer-live-p
                         (tessera-x-context-buffer previous))))
            (tessera-x-cancel-context)
            (dolist (buffer buffers)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))))))))

(ert-deftest tessera-x-elfeed-startup-failures-dont-recurse ()
  (tessera-x-tests--with-snapshots
    (with-temp-buffer
      (let* ((items (cl-loop repeat 1000 collect
                             (tessera-x-tests--item "feed")))
             (context
              (tessera-x-context-start 'elfeed "failure" items))
             (request (make-tessera-x-elfeed--request
                       :context context
                       :queue (copy-sequence items))))
        (cl-letf (((symbol-function 'url-retrieve)
                   (lambda (&rest _) (error "Offline"))))
          (tessera-x-elfeed--dispatch request))
        (should (eq (tessera-x-context-state context) 'ready))
        (should (string-match-p
                 "Offline" (tessera-x-item-note (car items))))))))

(ert-deftest tessera-x-mu-today-query-belongs-to-current-headers ()
  (let ((mu4e-headers-mode-hook nil)
        (mu4e-search-hide-enabled nil)
        (mu4e-search-threads nil))
    (with-temp-buffer
      (mu4e-headers-mode)
      (setq-local list-buffers-directory "maildir:/account-a/Inbox")
      (cl-letf (((symbol-function 'mu4e-server-last-query)
                 (lambda () '(:query "maildir:/account-b/Inbox"))))
        (with-temp-buffer
          (mu4e-headers-mode)
          (setq-local list-buffers-directory
                      "maildir:/account-b/Inbox")
          (should (equal (tessera-x-mu4e--today-query)
                         "maildir:/account-b/Inbox")))
        (should (equal (tessera-x-mu4e--today-query)
                       "maildir:/account-a/Inbox"))
        (setq-local list-buffers-directory nil)
        (should-error (tessera-x-mu4e--today-query)
                      :type 'user-error)
        (setq-local list-buffers-directory "")
        (should (equal (tessera-x-mu4e--today-query) ""))))))

(ert-deftest tessera-x-mu-query-startup-releases-resources ()
  (skip-unless (executable-find "sleep"))
  (tessera-x-tests--with-snapshots
    (dolist (failure '(quit error))
      (dolist (stage '(output errors process properties))
        (ert-info ((format "%s during %s" failure stage))
          (with-temp-buffer
            (let ((previous
                   (tessera-x-context-start 'mu4e "Previous" nil)))
              (tessera-x-context-finish previous)
              (let* ((context
                      (tessera-x-context-start 'mu4e "Query" nil))
                     (generate (symbol-function 'generate-new-buffer))
                     (make (symbol-function 'make-process))
                     (put (symbol-function 'process-put))
                     (mu4e-mu-binary "mu")
                     (mu4e-mu-home nil)
                     buffers process)
                (unwind-protect
                    (cl-letf
                        (((symbol-function 'generate-new-buffer)
                          (lambda (name &rest args)
                            (when (equal name
                                         (format " *Tessera mu %s*"
                                                 stage))
                              (signal failure nil))
                            (let ((buffer (apply generate name args)))
                              (push buffer buffers)
                              buffer)))
                         ((symbol-function 'make-process)
                          (lambda (&rest args)
                            (when (eq stage 'process)
                              (signal failure nil))
                            (setq process
                                  (apply make
                                         :command '("sleep" "5")
                                         args))))
                         ((symbol-function 'process-put)
                          (lambda (&rest args)
                            (when (eq stage 'properties)
                              (signal failure nil))
                            (apply put args))))
                      (if (eq failure 'quit)
                          (should
                           (eq (condition-case err
                                   (tessera-x-mu4e--query context "x")
                                 (quit (car err)))
                               'quit))
                        (tessera-x-mu4e--query context "x"))
                      (should
                       (eq (tessera-x-context-state context)
                           (if (eq failure 'quit)
                               'cancelled
                             'failed)))
                      (should-not tessera-x--pending-context)
                      (should-not (tessera-x-context-cleanup context))
                      (should (eq tessera-x-current-context previous))
                      (should-not (process-live-p process))
                      (should-not (cl-some #'buffer-live-p buffers)))
                  (tessera-x-cancel-context)
                  (when (process-live-p process)
                    (delete-process process))
                  (dolist (buffer buffers)
                    (when (buffer-live-p buffer)
                      (kill-buffer buffer))))))))))))

(ert-deftest tessera-x-mu-query-completion-releases-resources ()
  (skip-unless (executable-find "sleep"))
  (tessera-x-tests--with-snapshots
    (pcase-dolist (`(,command ,status ,code ,state)
                   '(("exit 0" exit 0 ready)
                     ("exit 2" exit 2 ready)
                     (nil signal 2 failed)
                     ("exit 0" exit 0 cancelled)))
      (with-temp-buffer
        (let ((previous
               (tessera-x-context-start 'mu4e "Previous" nil)))
          (tessera-x-context-finish previous)
          (let* ((calls 0)
                 (tessera-x-context-ready-hook
                  (list (lambda (_context) (cl-incf calls))))
                 (context (tessera-x-context-start 'mu4e "Query" nil))
                 (output (generate-new-buffer " *mu query output*"))
                 (errors (generate-new-buffer " *mu query errors*"))
                 (process
                  (make-process
                   :name "tessera-query-exit-test"
                   :command (if command
                                (list shell-file-name
                                      shell-command-switch command)
                              '("sleep" "5"))
                   :buffer output
                   :stderr errors
                   :noquery t
                   :connection-type 'pipe
                   :sentinel #'ignore)))
            (unwind-protect
                (progn
                  (process-put process 'context context)
                  (process-put process 'errors errors)
                  (push (apply-partially
                         #'tessera-x-mu4e--cancel-query
                         process output errors)
                        (tessera-x-context-cleanup context))
                  (when (eq status 'signal)
                    (interrupt-process process))
                  (let ((deadline (+ (float-time) 5)))
                    (while (and (process-live-p process)
                                (< (float-time) deadline))
                      (accept-process-output process 0.05)))
                  (should (eq (process-status process) status))
                  (should (= (process-exit-status process) code))
                  (if (eq state 'cancelled)
                      (progn
                        (with-current-buffer output
                          (insert "(:path \"/message\")"))
                        (cl-letf
                            (((symbol-function 'tessera-x-mu4e--item)
                              (lambda (_) (signal 'quit nil))))
                          (should
                           (eq (condition-case err
                                   (tessera-x-mu4e--query-done
                                    process "finished\n")
                                 (quit (car err)))
                               'quit))))
                    (tessera-x-mu4e--query-done process "finished\n"))
                  (should
                   (eq (tessera-x-context-state context) state))
                  (if (eq state 'ready)
                      (progn
                        (should
                         (eq tessera-x-current-context context))
                        (should (= calls 1)))
                    (should (eq tessera-x-current-context previous))
                    (should (= calls 0))
                    (should-not (tessera-x-context-buffer context))
                    (if (eq state 'failed)
                        (should (string-match-p
                                 "signal 2"
                                 (tessera-x-context-error context)))
                      (should-not (tessera-x-context-error context))))
                  (should-not tessera-x--pending-context)
                  (should-not (tessera-x-context-cleanup context))
                  (should-not (buffer-live-p output))
                  (should-not (buffer-live-p errors))
                  (tessera-x-mu4e--query-done process "late event\n")
                  (should (= calls (if (eq state 'ready) 1 0))))
              (tessera-x-cancel-context)
              (tessera-x-mu4e--cancel-query
               process output errors))))))))

(ert-deftest tessera-x-mu-query-uses-muhome-and-path-identities ()
  (tessera-x-tests--with-snapshots
    (let* ((directory (make-temp-file "tessera-mu-query-" t))
           (mu4e-mu-binary (expand-file-name "mu" directory))
           (mu4e-mu-home
            (expand-file-name "index with space" directory))
           (message-file (expand-file-name "message.eml" directory))
           (record (list :path message-file
                         :message-id "indexed"
                         :references '("root")
                         :subject "Local indexed mail"
                         :date '(27000 0)))
           (args (expand-file-name "args" directory)))
      (unwind-protect
          (progn
            (with-temp-file message-file
              (insert
               "Content-Type: text/plain; charset=utf-8\n"
               "Content-Transfer-Encoding: quoted-printable\n\n"
               "caf=C3=A9\n"))
            (with-temp-file mu4e-mu-binary
              (insert "#!/bin/sh\n"
                      "printf '%s\\n' \"$@\" > "
                      (shell-quote-argument args) "\n"
                      "printf '%s\\n' "
                      (shell-quote-argument (prin1-to-string record))
                      "\n"))
            (set-file-modes mu4e-mu-binary #o700)
            (with-temp-buffer
              (insert "point stays here")
              (goto-char 4)
              (let ((context (tessera-x-context-start
                              'mu4e "query test" nil)))
                (tessera-x-mu4e--query context "date:today..now")
                (let ((deadline (+ (float-time) 5)))
                  (while (and (tessera-x-context-pending-p context)
                              (< (float-time) deadline))
                    (accept-process-output nil 0.05)))
                (should (eq (tessera-x-context-state context) 'ready))
                (should (= (point) 4))
                (should (equal
                         (tessera-x-item-id
                          (car (tessera-x-context-items context)))
                         message-file))
                (should (string-match-p
                         "café"
                         (tessera-x-item-body
                          (car (tessera-x-context-items context)))))))
            ;; Keep native-only children and the canonical root when
            ;; the current results contain duplicate Message-IDs.
            (with-temp-buffer
              (let* ((root (tessera-x-tests--item "root"))
                     (anchor (copy-tessera-x-item root))
                     (child (tessera-x-tests--item "native"))
                     (context (tessera-x-context-start
                               'mu4e "subthread"
                               (list root anchor child))))
                (dolist (item (list root anchor child))
                  (setf (tessera-x-item-data item)
                        (list :path message-file)))
                (setf (tessera-x-item-id anchor) "duplicate"
                      (tessera-x-item-parent child) "root")
                (tessera-x-mu4e--query context "msgid:root" anchor)
                (let ((deadline (+ (float-time) 5)))
                  (while (and (tessera-x-context-pending-p context)
                              (< (float-time) deadline))
                    (accept-process-output nil 0.05)))
                (should (eq (tessera-x-context-state context) 'ready))
                (let ((items (tessera-x-context-items context)))
                  (should (eq (car items) root))
                  (should (equal (mapcar #'tessera-x-item-message-id
                                         items)
                                 '("root" "native" "indexed")))
                  (should (cl-every
                           (lambda (item)
                             (string-match-p
                              "café" (tessera-x-item-body item)))
                           items)))))
            (with-temp-buffer
              (insert-file-contents args)
              (should (string-match-p "--include-related"
                                      (buffer-string)))
              (should (string-match-p
                       (regexp-quote
                        (concat "--muhome=" mu4e-mu-home))
                       (buffer-string)))))
        (delete-directory directory t)))))

(ert-deftest tessera-x-elfeed-http-parses-content-and-charset ()
  (dolist (header
           (list
            "Content-Type: text/html; charset=iso-8859-1"
            "Content-Type: text/html ; charset=iso-8859-1"
            "Content-Type:\ttext/html; charset=iso-8859-1"
            "Content-Type: TEXT/HTML; CHARSET=\"ISO-8859-1\" \t"
            (concat "X-Content-Type: application/json\r\n"
                    "Content-Type: text/html; charset=iso-8859-1")
            (concat "Set-Cookie: charset=utf-8\r\n"
                    "Content-Type: text/html; charset=iso-8859-1")
            "Content-Type: text/html;\r\n\tcharset=iso-8859-1"))
    (ert-info ((format "Response header: %S" header))
      (tessera-x-tests--with-snapshots
        (with-temp-buffer
          (let* ((item (tessera-x-tests--item "feed"))
                 (context (tessera-x-context-start
                           'elfeed "http parser" (list item)))
                 (request
                  (make-tessera-x-elfeed--request :context context))
                 (fetch (make-tessera-x-elfeed--fetch
                         :request request
                         :item item))
                 (response (generate-new-buffer " *HTTP fixture*")))
            (setf (tessera-x-elfeed--request-active request)
                  (list fetch))
            (with-current-buffer response
              (set-buffer-multibyte nil)
              (insert "HTTP/1.1 200 OK\r\n" header "\r\n\r\n")
              (setq-local url-http-response-status 200)
              (setq-local url-http-end-of-headers (point-marker))
              (insert (encode-coding-string
                       "<p>café fetched body</p>" 'iso-latin-1))
              (tessera-x-elfeed--response nil fetch))
            (should (eq (tessera-x-context-state context) 'ready))
            (should-not (buffer-live-p response))
            (should (equal (tessera-x-item-body item)
                           "café fetched body"))
            (should (equal (tessera-x-item-note item)
                           "Fetched linked page"))))))))

(ert-deftest tessera-x-elfeed-http-detects-document-charset ()
  (pcase-dolist
      (`(,type ,declaration ,coding ,body . ,fragment)
       `(("text/html" "<meta charset=\"gb18030\">"
          chinese-gbk "中文正文")
         ("text/html" "<meta charset='iso-8859-1'>"
          iso-latin-1 "café")
         ("text/html"
          ,(concat "<meta http-equiv='Content-Type' "
                   "content='text/html; charset=iso-8859-1'>")
          iso-latin-1 "café")
         ("text/html"
          ,(concat "<meta content='text/html; charset=iso-8859-1' "
                   "http-equiv='Content-Type'>")
          iso-latin-1 "café")
         ("text/html" "<!doctype html><meta charset = 'iso-8859-1'>"
          iso-latin-1 "café" t)
         ("text/html" "<meta charset=iso-8859-1>"
          iso-latin-1 "café" t)
         ("text/html" "<META CHARSET\n=\t\"ISO-8859-1\">"
          iso-latin-1 "café" t)
         ("text/html"
          ,(concat "<meta content = 'text/html; charset = iso-8859-1'"
                   " http-equiv = 'CONTENT-TYPE'>")
          iso-latin-1 "café" t)
         ("text/html"
          ,(concat "<!-- <meta charset=utf-8> -->"
                   "<script>var x = '<meta charset=utf-8>';</script>"
                   "<meta data-note='<meta charset=utf-8>' "
                   "charset=iso-8859-1>")
          iso-latin-1 "café" t)
         ("text/html"
          ,(concat "<meta content='text/html; charset=utf-8'>"
                   "<meta charset=unknown-encoding>"
                   "<meta charset=iso-8859-1>")
          iso-latin-1 "café" t)
         ("text/html"
          ,(format "%1024s" "<meta charset=iso-8859-1>")
          iso-latin-1 "café" t)
         ("text/html"
          ,(format "%1025s" "<meta charset=iso-8859-15>")
          utf-8 "café" t)
         ("text/html"
          ,(concat (make-string 1024 ?\s)
                   "<meta charset=iso-8859-1>")
          utf-8 "café" t)
         ("application/xhtml+xml"
          "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>"
          iso-latin-1 "café")
         ("text/html; charset=utf-8"
          "<meta charset=\"iso-8859-1\">" utf-8 "café")
         ("text/html; charset=iso-8859-1" ""
          utf-8-with-signature "café")
         ("text/html" "<meta charset=iso-8859-1>"
          utf-8-with-signature "café")
         ("text/html" "" utf-16le-with-signature "中文正文")
         ("text/html; charset=utf-8" "<meta charset=utf-8>"
          utf-16be-with-signature "中文正文")
         ("application/xhtml+xml" "<?xml version=\"1.0\"?>"
          utf-16le-with-signature "café")
         ("application/xhtml+xml" "<?xml version=\"1.0\"?>"
          utf-16be-with-signature "café")
         ("text/plain" "" utf-8-with-signature "café")
         ("text/plain" "" utf-16le-with-signature "中文正文")
         ("text/plain" "" utf-16be-with-signature "中文正文")
         ("text/html" "" utf-8 "中文正文")
         ("text/plain" "<meta charset=\"iso-8859-1\">"
          utf-8 "café")))
    (ert-info ((format "%s: %s" type declaration))
      (tessera-x-tests--with-snapshots
        (with-temp-buffer
          (let* ((item (tessera-x-tests--item "feed"))
                 (context (tessera-x-context-start
                           'elfeed "document charset" (list item)))
                 (request
                  (make-tessera-x-elfeed--request :context context))
                 (fetch (make-tessera-x-elfeed--fetch
                         :request request
                         :item item))
                 (response (generate-new-buffer " *HTTP fixture*"))
                 (html
                  (cond
                   (fragment (concat declaration "<p>" body "</p>"))
                   ((string-prefix-p "<?xml" declaration)
                    (concat declaration "<html><body>"
                            body "</body></html>"))
                   (t (concat "<html><head>" declaration
                              "</head><body>" body
                              "</body></html>")))))
            (setf (tessera-x-elfeed--request-active request)
                  (list fetch))
            (with-current-buffer response
              (set-buffer-multibyte nil)
              (insert "HTTP/1.1 200 OK\r\nContent-Type: " type
                      "\r\nX-Charset: utf-8\r\n\r\n")
              (setq-local url-http-response-status 200)
              (setq-local url-http-end-of-headers (point-marker))
              (insert (encode-coding-string html coding))
              (tessera-x-elfeed--response nil fetch))
            (should (eq (tessera-x-context-state context) 'ready))
            (should-not (buffer-live-p response))
            (should (equal (tessera-x-item-body item)
                           (if (equal type "text/plain") html body)))
            (should (equal (tessera-x-item-note item)
                           "Fetched linked page"))))))))

(ert-deftest tessera-x-elfeed-charset-scan-is-bounded ()
  (let ((parse (symbol-function 'libxml-parse-html-region)) sizes)
    (cl-letf (((symbol-function 'libxml-parse-html-region)
               (lambda (start end &rest args)
                 (push (- end start) sizes)
                 (apply parse start end args))))
      (dolist (size '(2048 2097152))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert "HTTP/1.1 200 OK\r\n\r\n")
          (setq-local url-http-end-of-headers (point-marker))
          (insert "<meta charset=iso-8859-1><p>"
                  (make-string size ?x) "</p>")
          (should
           (coding-system-equal
            (tessera-x-elfeed--document-charset "text/html")
            'iso-latin-1)))))
    (should (= (length sizes) 2))
    (should (apply #'= sizes))))

(ert-deftest tessera-x-elfeed-today-keeps-filter-and-local-boundaries
    ()
  (tessera-x-tests--with-snapshots
    (let* ((elfeed-db '(:version 4))
           (elfeed-db-feeds (make-hash-table :test #'equal))
           (elfeed-db-entries (make-hash-table :test #'equal))
           (elfeed-db-index (avl-tree-create #'elfeed-db-compare))
           (bounds (tessera-x-today-bounds))
           (start (float-time (car bounds)))
           (end (float-time (cdr bounds)))
           (feed (elfeed-feed--create :id "feed" :title "Feed")))
      (puthash "feed" feed elfeed-db-feeds)
      (cl-loop for date in (list (1- start) start (1+ start)
                                 (+ start 2) end)
               for tags in '((keep) (keep) (drop) (keep) (keep))
               for index from 0
               do
               (let* ((id (cons "feed" (number-to-string index)))
                      (entry (elfeed-entry--create
                              :id id
                              :feed-id "feed"
                              :title "Entry"
                              :date date
                              :tags tags
                              :content "Body"
                              :link "https://example.invalid/item")))
                 (puthash id entry elfeed-db-entries)
                 (avl-tree-enter elfeed-db-index id)))
      (dolist (case '(("+keep" . ("1" "3"))
                      ("+keep #1" . ("3"))
                      ("+keep #0" . nil)))
        (with-temp-buffer
          (setq-local major-mode 'elfeed-search-mode)
          (setq-local elfeed-search-filter (car case))
          (cl-letf (((symbol-function 'url-retrieve)
                     (lambda (&rest _) (ert-fail "Unexpected HTTP"))))
            (let ((context (tessera-x-elfeed-prepare-today-context)))
              (should (eq (tessera-x-context-state context) 'ready))
              (should
               (equal (mapcar #'tessera-x-item-id
                              (tessera-x-context-items context))
                      (mapcar (lambda (id) (cons "feed" id))
                              (cdr case)))))))))))

(ert-deftest tessera-x-elfeed-today-obeys-both-age-bounds ()
  (tessera-x-tests--with-snapshots
    (let* ((elfeed-db '(:version 4))
           (elfeed-db-feeds (make-hash-table :test #'equal))
           (elfeed-db-entries (make-hash-table :test #'equal))
           (elfeed-db-index (avl-tree-create #'elfeed-db-compare))
           (start (car (tessera-x-today-bounds)))
           (now (+ (float-time start) 43200))
           (float-time-function (symbol-function 'float-time))
           (feed (elfeed-feed--create :id "feed" :title "Feed")))
      (puthash "feed" feed elfeed-db-feeds)
      (dolist (age '(1800 3600 7200 10800 14400))
        (let* ((id (cons "feed" (number-to-string age)))
               (entry (elfeed-entry--create
                       :id id
                       :feed-id "feed"
                       :title "Entry"
                       :date (- now age)
                       :content "Body")))
          (puthash id entry elfeed-db-entries)
          (avl-tree-enter elfeed-db-index id)))
      (dolist (case '(("@3-hours-ago--1-hour-ago" . ("10800" "7200"))
                      ("@3-hours-ago--1-hour-ago #1" . ("7200"))))
        (with-temp-buffer
          (setq-local major-mode 'elfeed-search-mode)
          (setq-local elfeed-search-filter (car case))
          (cl-letf (((symbol-function 'float-time)
                     (lambda (&optional time)
                       (if time (funcall float-time-function time)
                         now))))
            (let ((context (tessera-x-elfeed-prepare-today-context)))
              (should
               (equal (mapcar (lambda (item)
                                (cdr (tessera-x-item-id item)))
                              (tessera-x-context-items context))
                      (cdr case))))))))))

(ert-deftest tessera-x-gnus-overview-filters-dates-and-keeps-headers
    ()
  (let* ((file (make-temp-file "tessera-overview-"))
         (bounds (tessera-x-today-bounds))
         (start (car bounds))
         (end (cdr bounds))
         (read-labels
          (symbol-function 'tessera-gnus-summary-label-data))
         (parse-date (symbol-function 'date-to-time))
         (label-count 0)
         (date-count 0)
         (gnus-agent t))
    (unwind-protect
        (progn
          (with-temp-file file
            (cl-loop
             for date in (list (time-subtract start 1) start end)
             for number from 1
             do
             (nnheader-insert-nov
              (make-full-mail-header
               number "日本語 review" "Full Name <full@test.invalid>"
               (format-time-string "%a, %d %b %Y %T %z" date)
               (format "<%d@test.invalid>" number)
               "<root@test.invalid>" 100 5 nil
               '((Keywords . "design,review"))))))
          (cl-letf
              (((symbol-function 'tessera-x-gnus--agent-file)
                (lambda (&rest _) file))
               ((symbol-function 'gnus-find-method-for-group)
                (lambda (_) '(nnmaildir "fixture")))
               ((symbol-function 'gnus-agent-method-p)
                (lambda (_) t))
               ((symbol-function 'tessera-gnus-summary-label-data)
                (lambda (header)
                  (cl-incf label-count)
                  (funcall read-labels header)))
               ((symbol-function 'date-to-time)
                (lambda (date)
                  (cl-incf date-count)
                  (funcall parse-date date))))
            (let ((items
                   (tessera-x-gnus--overview '("group") bounds)))
              (should (= (length items) 1))
              (should (= label-count 1))
              (should (= date-count 3))
              (should (equal (tessera-x-item-id (car items))
                             '("group" . 2)))
              (should (equal (tessera-x-item-subject (car items))
                             "日本語 review"))
              (should (equal
                       (cdr
                        (assoc "Labels"
                               (tessera-x-item-metadata (car items))))
                       "design,review")))
            (setq label-count 0 date-count 0)
            (should (= (length (tessera-x-gnus--overview '("group")))
                       3))
            (should (= label-count 3))
            (should (= date-count 3))))
      (delete-file file))))

(provide 'tessera-x-tests)
;;; tessera-x-tests.el ends here
