;;; tessera-mu4e-fixture.el --- mu4e fixture for Tessera  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bingshan Chang <chang@bingshan.org>

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A development-only Maildir with representative mu4e messages,
;; threads, flags, labels, folders, bookmarks, and contexts.

;;; Code:

(require 'cl-lib)
(require 'mu4e)
(require 'subr-x)

(defconst tessera-mu4e-fixture-directory
  (expand-file-name "fixtures/mu4e/" user-emacs-directory)
  "Directory containing the generated mu4e fixture.")

(defconst tessera-mu4e-fixture-maildir
  (expand-file-name "mail/" tessera-mu4e-fixture-directory)
  "Root Maildir of the generated mu4e fixture.")

(defconst tessera-mu4e-fixture-mu-home
  (expand-file-name "mu-home/" tessera-mu4e-fixture-directory)
  "Mu home directory of the generated mu4e fixture.")

(defconst tessera-mu4e-fixture-addresses
  '((personal . "alex@personal.fixture.invalid")
    (work . "alex@work.fixture.invalid"))
  "Personal addresses used by the two fixture contexts.")

(defconst tessera-mu4e-fixture--authors
  '(("Ada Lovelace" . "ada@fixture.invalid")
    ("Alan Turing" . "alan@fixture.invalid")
    ("Grace Hopper" . "grace@fixture.invalid")
    ("Edsger Dijkstra" . "edsger@fixture.invalid")
    ("Margaret Hamilton" . "margaret@fixture.invalid")
    ("Barbara Liskov" . "barbara@fixture.invalid")
    ("Donald Knuth" . "donald@fixture.invalid")
    ("Frances Allen" . "frances@fixture.invalid")
    ("Ken Thompson" . "ken@fixture.invalid")
    ("Radia Perlman" . "radia@fixture.invalid"))
  "Fictional correspondents used by fixture messages.")

(defconst tessera-mu4e-fixture--thread-subjects
  '("Rendering status without copying backend state"
    "Accessible spacing for proportional fonts"
    "Reviewing the autumn interface release"
    "Offline indexing and cache synchronization"
    "Keyboard navigation across message groups"
    "Consistent notifications for new messages"
    "Maildir shortcuts for multiple accounts"
    "Theme-aware colors for semantic marks"
    "Calendar invitation rendering in headers"
    "Encrypted message indicators and fallback text"
    "Attachment layout in narrow windows"
    "Search bookmarks for the project archive"
    "Preserving labels during database rebuilds"
    "Thread connectors for branching discussions"
    "Handling long subjects without wrapping"
    "Context switching between work and personal mail"
    "Release notes for the message list refresh"
    "Improving sender alignment in compact frames"
    "Mailing-list metadata and personal messages"
    "Draft workflow and sent-message behavior")
  "Subjects used by the twenty fixture threads.")

(defconst tessera-mu4e-fixture--single-subjects
  '("Monthly account summary"
    "A short independent project update"
    "Travel arrangements for the community meeting"
    "Receipt for the documentation workshop"
    "Reminder to review the release checklist"
    "A deliberately long standalone subject for truncation"
    "Newsletter: interface design notes"
    "Follow-up requested for the accessibility review")
  "Subjects cycled through by standalone fixture messages.")

(defconst tessera-mu4e-fixture--labels
  '("archive" "emacs" "finance" "follow-up" "newsletter"
    "personal" "project-tessera" "review" "travel" "work")
  "Labels represented by fixture messages.")

(defconst tessera-mu4e-fixture--maildirs
  '("personal/Inbox" "personal/Archive" "personal/Sent"
    "personal/Drafts" "personal/Trash" "personal/Lists"
    "work/Inbox" "work/Archive" "work/Sent" "work/Drafts"
    "work/Trash" "work/Projects")
  "Maildirs represented by the fixture.")

(defun tessera-mu4e-fixture--address (context)
  "Return the fixture address for CONTEXT."
  (alist-get context tessera-mu4e-fixture-addresses))

(defun tessera-mu4e-fixture--message-id (number)
  "Return the fixture message ID for NUMBER."
  (format "tessera-mu4e-%03d@fixture.invalid" number))

(defun tessera-mu4e-fixture--thread-parent (position)
  "Return the parent position for thread POSITION."
  (pcase position
    (0 nil)
    ((or 1 3) 0)
    (2 1)
    (4 3)
    (5 4)
    (6 2)
    (7 6)))

(defun tessera-mu4e-fixture--thread-references (thread position)
  "Return references for POSITION in THREAD."
  (let (references)
    (while-let ((parent
                 (tessera-mu4e-fixture--thread-parent position)))
      (push (tessera-mu4e-fixture--message-id
             (+ (* thread 8) parent))
            references)
      (setq position parent))
    references))

(defun tessera-mu4e-fixture--date (number thread)
  "Return a stable date for message NUMBER and THREAD."
  (let* ((offset (mod (if thread thread number) 12))
         (month (1+ (mod (+ 8 offset) 12)))
         (year (if (< month 9) 2026 2025))
         (day (1+ (mod (+ number (* offset 2)) 24)))
         (hour (+ 8 (mod number 12)))
         (minute (mod (* number 7) 60)))
    (encode-time 0 minute hour day month year 28800)))

(defun tessera-mu4e-fixture--message-labels (number context)
  "Return labels for message NUMBER in CONTEXT."
  (unless (= (mod number 10) 9)
    (delete-dups (list (symbol-name context)
                       (nth (mod number
                                 (length tessera-mu4e-fixture--labels))
                            tessera-mu4e-fixture--labels)
                       (when (= (mod number 4) 0) "project-tessera")
                       (when (= (mod number 7) 0) "review")))))

(defun tessera-mu4e-fixture--maildir (number context outgoing)
  "Return the Maildir for NUMBER, CONTEXT, and OUTGOING."
  (let ((prefix (symbol-name context)))
    (concat
     prefix "/"
     (cond
      ((= (mod number 29) 0) "Drafts")
      ((= (mod number 31) 0) "Trash")
      (outgoing "Sent")
      ((= (mod number 7) 0)
       (if (eq context 'personal) "Lists" "Projects"))
      ((= (mod number 5) 0) "Archive")
      (t "Inbox")))))

(defun tessera-mu4e-fixture--maildir-flags (number maildir read)
  "Return Maildir flags for NUMBER in MAILDIR with READ state."
  (concat (when (string-suffix-p "/Drafts" maildir) "D")
          (when (= (mod number 11) 0) "F")
          (when (= (mod number 13) 0) "P")
          (when (= (mod number 17) 0) "R")
          (when read "S")
          (when (string-suffix-p "/Trash" maildir) "T")))

(defun tessera-mu4e-fixture--spec (number)
  "Return the fixture message specification for NUMBER."
  (let* ((thread (and (< number 160) (/ number 8)))
         (position (and thread (mod number 8)))
         (context
          (if thread
              (if (< thread 10) 'personal 'work)
            (if (cl-evenp number) 'personal 'work)))
         (personal (tessera-mu4e-fixture--address context))
         (author (nth (mod number
                           (length tessera-mu4e-fixture--authors))
                      tessera-mu4e-fixture--authors))
         (outgoing (= (mod number 6) 0))
         (public (= (mod number 9) 0))
         (maildir
          (tessera-mu4e-fixture--maildir number context outgoing))
         (new (and (= (mod number 10) 0)
                   (not (string-match-p "/Drafts\\|/Trash"
                                        maildir))))
         (read (and (not new) (/= (mod number 3) 0)))
         (subject
          (if thread
              (nth thread
                   tessera-mu4e-fixture--thread-subjects)
            (nth (mod number
                      (length tessera-mu4e-fixture--single-subjects))
                 tessera-mu4e-fixture--single-subjects)))
         (from (if outgoing personal (cdr author)))
         (to (cond
              (outgoing (cdr author))
              (public "tessera-list@lists.fixture.invalid")
              (t personal))))
    (list :number number
          :thread thread
          :position position
          :context context
          :author (if outgoing "Alex Example" (car author))
          :from from
          :to to
          :subject (if (and thread (> position 0))
                       (concat "Re: " subject)
                     subject)
          :date (tessera-mu4e-fixture--date number thread)
          :maildir maildir
          :new new
          :flags
          (tessera-mu4e-fixture--maildir-flags number maildir read)
          :feature (nth (mod number 7)
                        '(plain attach signed encrypted calendar
                                list combined))
          :labels
          (tessera-mu4e-fixture--message-labels number context))))

(defun tessera-mu4e-fixture--headers (spec)
  "Return the RFC 5322 headers for SPEC."
  (let* ((number (plist-get spec :number))
         (thread (plist-get spec :thread))
         (position (plist-get spec :position))
         (references
          (and thread
               (tessera-mu4e-fixture--thread-references thread position)))
         (parent (car (last references)))
         (feature (plist-get spec :feature)))
    (concat
     (format "From: %s <%s>\n"
             (plist-get spec :author)
             (plist-get spec :from))
     (format "To: <%s>\n" (plist-get spec :to))
     (format "Subject: %s\n" (plist-get spec :subject))
     (format-time-string "Date: %a, %d %b %Y %H:%M:%S %z\n"
                         (plist-get spec :date))
     (format "Message-ID: <%s>\n"
             (tessera-mu4e-fixture--message-id number))
     (when parent (format "In-Reply-To: <%s>\n" parent))
     (when references
       (format "References: %s\n"
               (mapconcat (lambda (id) (format "<%s>" id))
                          references " ")))
     (when (eq feature 'list)
       (concat "List-Id: Tessera Discussion "
               "<tessera.lists.fixture.invalid>\n"
               "List-Post: "
               "<mailto:tessera-list@lists.fixture.invalid>\n"
               "Precedence: list\n"))
     "MIME-Version: 1.0\n")))

(defun tessera-mu4e-fixture--body (spec)
  "Return the MIME body for SPEC."
  (let ((number (plist-get spec :number)))
    (pcase (plist-get spec :feature)
      ('attach
       (format (concat "Content-Type: multipart/mixed; "
                       "boundary=fixture-%d\n\n"
                       "--fixture-%d\nContent-Type: text/plain\n\n"
                       "Attachment fixture message %d.\n"
                       "--fixture-%d\n"
                       "Content-Type: application/octet-stream\n"
                       "Content-Disposition: attachment; "
                       "filename=notes-%03d.txt\n"
                       "Content-Transfer-Encoding: base64\n\n"
                       "VGVzc2VyYSBmaXh0dXJlIGF0dGFjaG1lbnQuCg==\n"
                       "--fixture-%d--\n")
               number number number number number number))
      ('signed
       (format (concat "Content-Type: multipart/signed; "
                       "boundary=signed-%d;\n"
                       " protocol=application/pgp-signature\n\n"
                       "--signed-%d\nContent-Type: text/plain\n\n"
                       "Signed fixture message %d.\n"
                       "--signed-%d\n"
                       "Content-Type: application/pgp-signature\n\n"
                       "-----BEGIN PGP SIGNATURE-----\n"
                       "dGVzc2VyYS1maXh0dXJl\n"
                       "-----END PGP SIGNATURE-----\n"
                       "--signed-%d--\n")
               number number number number number))
      ('encrypted
       (format (concat "Content-Type: multipart/encrypted; "
                       "boundary=encrypted-%d;\n"
                       " protocol=application/pgp-encrypted\n\n"
                       "--encrypted-%d\n"
                       "Content-Type: application/pgp-encrypted\n\n"
                       "Version: 1\n"
                       "--encrypted-%d\n"
                       "Content-Type: application/octet-stream\n\n"
                       "-----BEGIN PGP MESSAGE-----\n"
                       "dGVzc2VyYS1maXh0dXJl\n"
                       "-----END PGP MESSAGE-----\n"
                       "--encrypted-%d--\n")
               number number number number))
      ('calendar
       (format (concat "Content-Type: text/calendar; "
                       "method=REQUEST; charset=utf-8\n\n"
                       "BEGIN:VCALENDAR\nVERSION:2.0\n"
                       "BEGIN:VEVENT\nUID:fixture-%03d\n"
                       "SUMMARY:Tessera interface review\n"
                       "DTSTART:20260820T090000Z\n"
                       "END:VEVENT\nEND:VCALENDAR\n")
               number))
      ('combined
       (format (concat "Content-Type: multipart/mixed; "
                       "boundary=combined-%d\n\n"
                       "--combined-%d\nContent-Type: text/plain\n\n"
                       "Combined feature fixture message %d.\n"
                       "--combined-%d\n"
                       "Content-Type: text/calendar; method=REQUEST\n\n"
                       "BEGIN:VCALENDAR\nVERSION:2.0\nEND:VCALENDAR\n"
                       "--combined-%d\nContent-Type: image/png\n"
                       "Content-Disposition: attachment; "
                       "filename=preview.png\n"
                       "Content-Transfer-Encoding: base64\n\n"
                       "iVBORw0KGgo=\n--combined-%d--\n")
               number number number number number number))
      (_
       (format (concat "Content-Type: text/plain; charset=utf-8\n\n"
                       "This is deterministic fixture message %d.\n")
               number)))))

(defun tessera-mu4e-fixture--message (spec)
  "Return a complete message for SPEC."
  (concat (tessera-mu4e-fixture--headers spec)
          (tessera-mu4e-fixture--body spec)))

(defun tessera-mu4e-fixture--message-path (spec)
  "Return the generated message path for SPEC."
  (let* ((number (plist-get spec :number))
         (directory (if (plist-get spec :new) "new" "cur"))
         (flags (plist-get spec :flags))
         (filename
          (format "172000%04d.M%06dP1.fixture:2,%s"
                  number number flags)))
    (expand-file-name (concat (plist-get spec :maildir) "/" directory "/"
                              filename)
                      tessera-mu4e-fixture-maildir)))

(defun tessera-mu4e-fixture--make-maildir (directory)
  "Create Maildir DIRECTORY."
  (dolist (subdirectory '("cur" "new" "tmp"))
    (make-directory (expand-file-name subdirectory directory) t)))

(defun tessera-mu4e-fixture--write-message (spec labels-buffer)
  "Write SPEC and append its labels to LABELS-BUFFER."
  (let ((path (tessera-mu4e-fixture--message-path spec))
        (labels (delq nil (plist-get spec :labels))))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (insert (tessera-mu4e-fixture--message spec)))
    (when labels
      (with-current-buffer labels-buffer
        (insert "path:" path "\n"
                "message-id:"
                (tessera-mu4e-fixture--message-id (plist-get spec :number))
                "\nlabels:" (string-join labels ",") "\n\n")))))

(defun tessera-mu4e-fixture--run-mu (&rest arguments)
  "Run mu with ARGUMENTS or signal an error."
  (let ((binary (or mu4e-mu-binary (executable-find "mu"))))
    (unless binary (error "Cannot find the mu executable"))
    (with-temp-buffer
      (let ((status
             (apply #'call-process binary nil (list t t) nil
                    arguments)))
        (unless (zerop status)
          (error "Mu %s failed: %s"
                 (string-join arguments " ")
                 (string-trim (buffer-string))))))))

(defun tessera-mu4e-fixture--installed-p ()
  "Return non-nil when the generated fixture is installed."
  (and (file-directory-p tessera-mu4e-fixture-maildir)
       (file-directory-p tessera-mu4e-fixture-mu-home)))

(defun tessera-mu4e-fixture--generate ()
  "Generate and index the mu4e fixture."
  (when (file-directory-p tessera-mu4e-fixture-directory)
    (delete-directory tessera-mu4e-fixture-directory t))
  (tessera-mu4e-fixture--make-maildir tessera-mu4e-fixture-maildir)
  (dolist (maildir tessera-mu4e-fixture--maildirs)
    (tessera-mu4e-fixture--make-maildir (expand-file-name maildir tessera-mu4e-fixture-maildir)))
  (let ((labels-buffer (generate-new-buffer " *tessera-labels*"))
        (labels-file
         (expand-file-name "labels" tessera-mu4e-fixture-directory)))
    (unwind-protect
        (progn
          (dotimes (number 200)
            (tessera-mu4e-fixture--write-message (tessera-mu4e-fixture--spec number)
                                                 labels-buffer))
          (with-current-buffer labels-buffer
            (write-region nil nil labels-file nil 'silent))
          (tessera-mu4e-fixture--run-mu "init" "--quiet"
                                        "--muhome" tessera-mu4e-fixture-mu-home
                                        "--maildir" tessera-mu4e-fixture-maildir
                                        "--personal-address"
                                        (tessera-mu4e-fixture--address 'personal)
                                        "--personal-address"
                                        (tessera-mu4e-fixture--address 'work))
          (tessera-mu4e-fixture--run-mu "index" "--quiet" "--muhome" tessera-mu4e-fixture-mu-home)
          (tessera-mu4e-fixture--run-mu "labels" "import" labels-file
                                        "--muhome" tessera-mu4e-fixture-mu-home))
      (kill-buffer labels-buffer))))

(defun tessera-mu4e-fixture-context-match-p (context message)
  "Return non-nil when MESSAGE belongs to fixture CONTEXT."
  (when-let* ((message message)
              (maildir (mu4e-message-field message :maildir)))
    (string-prefix-p (concat "/" (symbol-name context) "/") maildir)))

(defun tessera-mu4e-fixture--context (context name)
  "Return a fixture CONTEXT named NAME."
  (let ((prefix (concat "/" (symbol-name context)))
        (address (tessera-mu4e-fixture--address context)))
    (make-mu4e-context :name name
                       :match-func
                       (lambda (message)
                         (tessera-mu4e-fixture-context-match-p context message))
                       :vars
                       `((user-mail-address . ,address)
                         (user-full-name . "Alex Example")
                         (mu4e-drafts-folder . ,(concat prefix "/Drafts"))
                         (mu4e-refile-folder . ,(concat prefix "/Archive"))
                         (mu4e-sent-folder . ,(concat prefix "/Sent"))
                         (mu4e-trash-folder . ,(concat prefix "/Trash"))))))

(defun tessera-mu4e-fixture-install ()
  "Install the mu4e fixture configuration."
  (unless (tessera-mu4e-fixture--installed-p)
    (tessera-mu4e-fixture--generate))
  (setq mu4e-mu-home tessera-mu4e-fixture-mu-home
        mu4e-context-policy 'pick-first
        mu4e-compose-context-policy 'pick-first
        mu4e-contexts
        (list (tessera-mu4e-fixture--context 'personal "Personal")
              (tessera-mu4e-fixture--context 'work "Work"))
        mu4e-bookmarks
        '((:name "Unread" :query "flag:unread" :key ?u)
          (:name "Flagged" :query "flag:flagged" :key ?f)
          (:name "Attachments" :query "flag:attach" :key ?a)
          (:name "Calendar" :query "flag:calendar" :key ?c)
          (:name "Personal context"
                 :query "maildir:/personal/*" :key ?p)
          (:name "Work context"
                 :query "maildir:/work/*" :key ?w)
          (:name "Tessera label"
                 :query "label:project-tessera" :key ?t)
          (:name "Review label" :query "label:review" :key ?r))
        mu4e-maildir-shortcuts
        '((:maildir "/personal/Inbox" :key ?i)
          (:maildir "/personal/Archive" :key ?A)
          (:maildir "/personal/Sent" :key ?s)
          (:maildir "/work/Inbox" :key ?I)
          (:maildir "/work/Archive" :key ?W)
          (:maildir "/work/Projects" :key ?P))))

(tessera-mu4e-fixture-install)

(provide 'tessera-mu4e-fixture)
;;; tessera-mu4e-fixture.el ends here
