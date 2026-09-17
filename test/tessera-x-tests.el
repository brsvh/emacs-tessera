;;; tessera-x-tests.el --- Context regressions -*- lexical-binding: t; -*-

;;; Commentary:

;; Snapshot ownership, budgets, MIME decoding and backend scope.

;;; Code:

(require 'ert)
(require 'tessera-x)
(require 'tessera-elfeed-x)
(require 'tessera-mu4e-x)
(require 'tessera-gnus-x)
(require 'tessera-gnus-test-support)

(defun tessera-x-tests--item (id &optional references)
  "Create a context record with ID and REFERENCES."
  (make-tessera-x-item
   :id id :message-id id :references references
   :subject "Full subject" :date '(27000 0)
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
            (insert "source text")
            (goto-char 4)
            (push-mark 8 t t)
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
              (should (= (point) 4))
              (should (= (mark) 8))
              (should mark-active)
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
             (list :docid (1+ index) :subject "Full subject"
                   :message-id (unless (= index 2)
                                 (format "%d" (1+ index)))
                   :meta (list :level (nth index '(0 1 0 1 2))
                               :root (memq index '(0 2)))
                   :date '(27000 0) :flags '(unread))
             (point-max))))
        (goto-char (point-min))
        (let ((overlay (make-overlay (point-min) (point-max))))
          (overlay-put overlay 'invisible t))
        (puthash 2 '(flag . nil) mu4e--mark-map)
        (puthash 4 '(flag . nil) mu4e--mark-map)
        (let ((all (tessera-mu4e-x--items))
              (factory (symbol-function 'tessera-mu4e-x--item))
              (count 0) items)
          (cl-letf (((symbol-function 'tessera-mu4e-x--item)
                     (lambda (message)
                       (cl-incf count)
                       (funcall factory message))))
            (setq items (tessera-mu4e-x--items t)))
          (should (= (length all) 5))
          (should (= count 2))
          (should (equal (mapcar #'tessera-x-item-id items) '(2 4)))
          (should (equal items (list (nth 1 all) (nth 3 all))))
          (should (equal (mapcar #'tessera-x-item-parent items)
                         '("1" 3)))
          (should-not (tessera-x-item-references (cadr items))))
        (should (equal (gethash 2 mu4e--mark-map) '(flag)))
        (should (= (point) (point-min)))
        (clrhash mu4e--mark-map)
        (should (equal (mapcar #'tessera-x-item-id
                               (tessera-mu4e-x--items t)) '(1)))
        (let ((transient-mark-mode t))
          (forward-line 2)
          (push-mark (line-end-position 2) t t)
          (let ((before (point)) (mark-before (mark)))
            (should (equal (mapcar #'tessera-x-item-id
                                   (tessera-mu4e-x--items t)) '(3 4)))
            (should (= (point) before))
            (should (= (mark) mark-before))))))))

(ert-deftest tessera-x-gnus-agent-policy-keeps-local-today-offline ()
  (tessera-x-tests--with-snapshots
    (let* ((directory (make-temp-file "tessera-agent-" t))
           (gnus-agent t)
           (tessera-gnus-x-body-policy 'download)
           (item (tessera-x-tests--item "agent"))
           (missing (tessera-x-tests--item "missing"))
           fetched refreshed)
      (setf (tessera-x-item-id item) '("group" . 1)
            (tessera-x-item-id missing) '("group" . 2))
      (unwind-protect
          (cl-letf
              (((symbol-function 'tessera-gnus-x--agent-file)
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
              (let ((context (tessera-gnus-x--build
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
              (tessera-gnus-x--build
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

(ert-deftest tessera-x-gnus-selection-uses-native-data-without-layout
    ()
  (with-temp-buffer
    (setq-local major-mode 'gnus-summary-mode)
    (setq-local gnus-summary-buffer (current-buffer))
    (setq-local gnus-newsgroup-name "example")
    (tessera-tests--gnus-rows '(0 1 0 1 2))
    (setq-local gnus-show-threads t)
    (setq-local gnus-newsgroup-processable '(1 4))
    (let ((all (tessera-gnus-x--items))
          (factory (symbol-function 'tessera-gnus-x--item))
          (count 0) items)
      (cl-letf (((symbol-function 'gnus-summary-save-process-mark)
                 #'ignore)
                ((symbol-function 'tessera-gnus-x--item)
                 (lambda (header group)
                   (cl-incf count)
                   (funcall factory header group))))
        (setq items (tessera-gnus-x--items t)))
      (should (= count 2))
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
             (context (tessera-x-context-start 'elfeed "test"
                                               (list item)))
             (request (make-tessera-elfeed-x--request
                       :context context))
             (fetch (make-tessera-elfeed-x--fetch
                     :request request :item item))
             (calls 0)
             (tessera-x-context-ready-hook
              (list (lambda (_) (cl-incf calls)))))
        (setf (tessera-elfeed-x--request-active request) (list fetch))
        (tessera-elfeed-x--timeout fetch)
        (should (= calls 1))
        (should (string-match-p "HTTP timeout"
                                (tessera-x-item-note item)))
        (tessera-elfeed-x--complete fetch "late response" nil)
        (should (= calls 1))
        (should (= (length (tessera-x-item-body item)) 1000))))))

(ert-deftest tessera-x-elfeed-redirects-release-all-transfers ()
  (dolist (cancel '(nil t))
    (tessera-x-tests--with-snapshots
      (with-temp-buffer
        (let* ((item (tessera-x-tests--item "redirect"))
               (context (tessera-x-context-start
                         'elfeed "redirect" (list item)))
               (request (make-tessera-elfeed-x--request
                         :context context))
               (buffers (cl-loop repeat 3 collect
                                 (generate-new-buffer " *Redirect*")))
               (process (make-pipe-process
                         :name "tessera-redirect" :noquery t
                         :buffer (car (last buffers))))
               (fetch (make-tessera-elfeed-x--fetch
                       :request request :item item
                       :buffer (car buffers))))
          (unwind-protect
              (progn
                (cl-loop for (buffer next) on buffers
                         do (with-current-buffer buffer
                              (setq-local url-redirect-buffer next)))
                (setf (tessera-elfeed-x--request-active request)
                      (list fetch))
                (push (apply-partially
                       #'tessera-elfeed-x--cancel request)
                      (tessera-x-context-cleanup context))
                (if cancel
                    (tessera-x-cancel-context)
                  (tessera-elfeed-x--timeout fetch))
                (should (eq (tessera-x-context-state context)
                            (if cancel 'cancelled 'ready)))
                (should-not (tessera-elfeed-x--request-active
                             request))
                (should-not (cl-some #'buffer-live-p buffers))
                (should-not (process-live-p process))
                (tessera-elfeed-x--stop-fetch fetch))
            (when (process-live-p process) (delete-process process))
            (dolist (buffer buffers)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))))))))

(ert-deftest tessera-x-elfeed-startup-failures-dont-recurse ()
  (tessera-x-tests--with-snapshots
    (with-temp-buffer
      (let* ((items (cl-loop repeat 1000 collect
                             (tessera-x-tests--item "feed")))
             (context (tessera-x-context-start
                       'elfeed "failure" items))
             (request (make-tessera-elfeed-x--request
                       :context context
                       :queue (copy-sequence items))))
        (cl-letf (((symbol-function 'url-retrieve)
                   (lambda (&rest _) (error "Offline"))))
          (tessera-elfeed-x--dispatch request))
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
          (should (equal (tessera-mu4e-x--today-query)
                         "maildir:/account-b/Inbox")))
        (should (equal (tessera-mu4e-x--today-query)
                       "maildir:/account-a/Inbox"))
        (setq-local list-buffers-directory nil)
        (should-error (tessera-mu4e-x--today-query)
                      :type 'user-error)
        (setq-local list-buffers-directory "")
        (should (equal (tessera-mu4e-x--today-query) ""))))))

(ert-deftest tessera-x-mu-query-uses-muhome-and-path-identities ()
  (tessera-x-tests--with-snapshots
    (let* ((directory (make-temp-file "tessera-mu-query-" t))
           (mu4e-mu-binary (expand-file-name "mu" directory))
           (mu4e-mu-home
            (expand-file-name "index with space" directory))
           (message-file (expand-file-name "message.eml" directory))
           (record (list :path message-file :message-id "indexed"
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
                (tessera-mu4e-x--query context "date:today..now")
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
                (tessera-mu4e-x--query context "msgid:root" anchor)
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
  (tessera-x-tests--with-snapshots
    (with-temp-buffer
      (let* ((item (tessera-x-tests--item "feed"))
             (context (tessera-x-context-start
                       'elfeed "http parser" (list item)))
             (request (make-tessera-elfeed-x--request
                       :context context))
             (fetch (make-tessera-elfeed-x--fetch
                     :request request :item item))
             (response (generate-new-buffer " *HTTP fixture*")))
        (setf (tessera-elfeed-x--request-active request) (list fetch))
        (with-current-buffer response
          (set-buffer-multibyte nil)
          (insert "HTTP/1.1 200 OK\r\n"
                  "Content-Type: text/html; "
                  "charset=iso-8859-1\r\n\r\n")
          (setq-local url-http-response-status 200)
          (setq-local url-http-end-of-headers (point-marker))
          (insert (encode-coding-string
                   "<p>café fetched body</p>" 'iso-latin-1))
          (tessera-elfeed-x--response nil fetch))
        (should (eq (tessera-x-context-state context) 'ready))
        (should-not (buffer-live-p response))
        (should (equal (tessera-x-item-body item)
                       "café fetched body"))
        (should (equal (tessera-x-item-note item)
                       "Fetched linked page"))))))

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
      (cl-loop for date in (list (1- start) start (1+ start) end)
               for tags in '((keep) (keep) (drop) (keep))
               for index from 0
               do
               (let* ((id (cons "feed" (number-to-string index)))
                      (entry (elfeed-entry--create
                              :id id :feed-id "feed" :title "Entry"
                              :date date :tags tags :content "Body"
                              :link "https://example.invalid/item")))
                 (puthash id entry elfeed-db-entries)
                 (avl-tree-enter elfeed-db-index id)))
      (with-temp-buffer
        (setq-local major-mode 'elfeed-search-mode)
        (setq-local elfeed-search-filter "+keep")
        (cl-letf (((symbol-function 'url-retrieve)
                   (lambda (&rest _) (ert-fail "Unexpected HTTP"))))
          (let ((context (tessera-elfeed-x-prepare-today-context)))
            (should (eq (tessera-x-context-state context) 'ready))
            (should (equal (mapcar #'tessera-x-item-id
                                   (tessera-x-context-items context))
                           '(("feed" . "1"))))))))))

(ert-deftest tessera-x-gnus-overview-filters-dates-and-keeps-headers
    ()
  (let* ((file (make-temp-file "tessera-overview-"))
         (bounds (tessera-x-today-bounds))
         (start (car bounds))
         (end (cdr bounds))
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
          (cl-letf (((symbol-function 'tessera-gnus-x--agent-file)
                     (lambda (&rest _) file))
                    ((symbol-function 'gnus-find-method-for-group)
                     (lambda (_) '(nnmaildir "fixture")))
                    ((symbol-function 'gnus-agent-method-p)
                     (lambda (_) t)))
            (let ((items (tessera-gnus-x--overview
                          '("group") bounds)))
              (should (= (length items) 1))
              (should (equal (tessera-x-item-id (car items))
                             '("group" . 2)))
              (should (equal (tessera-x-item-subject (car items))
                             "日本語 review"))
              (should (equal
                       (cdr (assoc "Labels"
                                   (tessera-x-item-metadata
                                    (car items))))
                       "design,review")))))
      (delete-file file))))

(provide 'tessera-x-tests)
;;; tessera-x-tests.el ends here
