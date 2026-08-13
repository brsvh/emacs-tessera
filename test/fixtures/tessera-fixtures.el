;;; tessera-fixtures.el --- Local Tessera fixtures  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bingshan Chang <chang@bingshan.org>

;; Author: Bingshan Chang <chang@bingshan.org>
;; Keywords: convenience, mail, news, test
;; Package-Requires: ((emacs "30.1"))

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;;; Commentary:

;; Build deterministic, local fixtures for exercising Tessera in
;; Elfeed, Gnus, and mu4e.  Generated state lives below
;; `tessera-fixtures-state-directory'.

;;; Code:

(require 'cl-lib)
(require 'elfeed)
(require 'elfeed-search)
(require 'gnus)
(require 'gnus-group)
(require 'gnus-sum)
(require 'mail-source)
(require 'message)
(require 'lorem-ipsum)
(require 'mu4e)
(require 'mu4e-icalendar)
(require 'mu4e-update)
(require 'subr-x)

(defvar tessera-fixtures--directory
  (file-name-directory
   (or load-file-name buffer-file-name))
  "Directory containing the fixture loader.")

(defvar tessera-fixtures-project-directory
  (file-name-as-directory
   (expand-file-name "../.." tessera-fixtures--directory))
  "Tessera project directory.")

(defvar tessera-fixtures-state-directory
  (expand-file-name "local/fixtures/"
                    tessera-fixtures-project-directory)
  "Directory containing generated fixture state.")

(defvar tessera-fixtures--mail-root
  (expand-file-name "mail/" tessera-fixtures-state-directory)
  "Root Maildir used by the mu4e fixtures.")

(defvar tessera-fixtures--gnus-root
  (expand-file-name "gnus/" tessera-fixtures-state-directory)
  "Root directory used by the Gnus fixtures.")

(defvar tessera-fixtures--mu-home
  (expand-file-name "mu/" tessera-fixtures-state-directory)
  "Mu database directory used by the mu4e fixtures.")

(defvar tessera-fixtures--elfeed-directory
  (expand-file-name "elfeed/" tessera-fixtures-state-directory)
  "Elfeed database directory used by the fixtures.")

(defvar tessera-fixtures--mu-flags
  '(attach calendar draft encrypted flagged list new passed personal
           replied seen signed trashed unread)
  "Mu flags covered by the fixture messages.")

(defvar tessera-fixtures-entry-count 240
  "Number of entries generated for each fixture application.")

(defvar tessera-fixtures--start-time
  (encode-time 0 0 0 1 1 2025 t)
  "Beginning of the fixture time range in UTC.")

(defvar tessera-fixtures--end-time
  (encode-time 59 59 23 12 8 2026 t)
  "End of the fixture time range in UTC.")

(defvar tessera-fixtures--people
  '("Aisha Rahman" "Akira Sato" "Alejandra Torres"
    "Amara Okafor" "Ana Petrović" "Arjun Mehta"
    "Camille Laurent" "Carlos Mendoza" "Chen Wei"
    "Chiamaka Nwosu" "Diego Álvarez" "Elena Rossi"
    "Emilia Kowalska" "Fatima Zahra" "Gabriel Silva"
    "Hana Kim" "Haruto Tanaka" "Inês Carvalho"
    "Iris van Dijk" "Isabella Romano" "Jamal Carter"
    "João Pereira" "Kavya Iyer" "Kenji Nakamura"
    "Layla Haddad" "Lena Hoffmann" "Lucía Fernández"
    "Malik Johnson" "María González" "Mateo Santos"
    "Mei Lin" "Mina Park" "Nadia Hassan" "Nikolai Petrov"
    "Noah Williams" "Nora Andersen" "Omar El-Sayed"
    "Priya Nair" "Rafael Costa" "Rina Suzuki"
    "Samira Diallo" "Santiago Ruiz" "Sofia Marin"
    "Tariq Mahmoud" "Thandiwe Dlamini" "Viktor Novak"
    "Yara Mansour" "Zoe Campbell")
  "People used as authors and correspondents.")

(defvar tessera-fixtures--topics
  '("accessibility audit" "API deprecation plan"
    "architecture decision record" "backup restoration drill"
    "benchmark results" "build farm capacity"
    "community meetup notes" "conference travel planning"
    "customer feedback review" "database migration"
    "dependency update" "design system refresh"
    "documentation sprint" "editor performance"
    "incident follow-up" "internationalization review"
    "keyboard navigation" "mail indexing regression"
    "mobile layout review" "monthly engineering digest"
    "on-call handoff" "package release checklist"
    "privacy assessment" "production rollout"
    "project roadmap" "quarterly planning"
    "release candidate" "rendering prototype"
    "security advisory" "service reliability report"
    "support rotation" "team retrospective"
    "thread visualization" "translation status"
    "user research summary" "weekly project update")
  "Topics used to construct realistic fixture subjects.")

(defvar tessera-fixtures--feed-catalog
  '(("engineering" "Tessera Engineering"
     (engineering emacs development))
    ("community" "Tessera Community"
     (community event people))
    ("security" "Security Dispatch"
     (security audit important))
    ("design" "Interface Notes" (design accessibility ui))
    ("releases" "Release Chronicle" (release packages emacs))
    ("research" "Research Notebook" (research usability data))
    ("operations" "Operations Weekly" (operations reliability))
    ("standards" "Open Standards Watch" (standards web protocol))
    ("lisp" "Lisp Systems Journal" (lisp programming emacs))
    ("privacy" "Privacy Engineering" (privacy security policy))
    ("localization" "Localization Report" (i18n language))
    ("independent" "Independent Software" (foss community)))
  "Feeds and their ordinary tags.")

(defvar tessera-fixtures--gnus-mark-scenarios
  '((unread nil "Review requested for the accessibility audit")
    (ticked tick "Keep the migration checklist close at hand")
    (dormant dormant "Waiting for the upstream release window")
    (del nil "Superseded draft of the quarterly roadmap")
    (read read "Minutes from the infrastructure sync")
    (expirable expire "Temporary notice for the build queue")
    (killed killed "Automated report filtered by topic")
    (spam spam "Unsolicited vendor partnership proposal")
    (kill-file killed-by-kill-file
               "Generated alert matched by the local kill file")
    (low-score low-score "Low priority digest from the test farm")
    (catchup catchup "Older discussion marked during catch-up")
    (ancient ancient "Archived design discussion from last year")
    (sparse sparse "Reply referring to an unavailable parent")
    (canceled canceled "Canceled announcement for the meetup")
    (duplicate duplicate "Duplicate copy of the release notice")
    (process nil "Messages selected for a batch operation")
    (replied reply "Re: Questions about the rendering prototype")
    (forwarded forward "Fwd: Notes from the standards meeting")
    (recent nil "Fresh results from the nightly benchmark")
    (cached cache "Offline copy of the incident report")
    (saved save "Saved research summary for later reading")
    (unseen unseen "A message not yet displayed in this session")
    (no-mark nil "Routine update with no secondary status")
    (undownloaded undownloaded "Article not downloaded by the agent")
    (downloaded downloaded "Article available in the local agent")
    (downloadable downloadable "Article queued for agent download")
    (unsendable unsendable "Draft excluded from agent sending")
    (score-over nil "Highly rated accessibility proposal" 100)
    (score-below nil "Low rated automated status digest" -100))
  "Natural Gnus articles exercising each summary state.")

(defvar tessera-fixtures--gnus-groups
  '("level-1-critical"
    "level-2-important"
    "level-3-primary"
    "level-4-normal"
    "level-5-subscribed"
    "level-6-low"
    "level-7-unsubscribed"
    "level-8-zombie"
    "level-9-killed")
  "Fixture groups covering all Gnus group levels.")

(defun tessera-fixtures--ensure-maildir (directory)
  "Create a Maildir at DIRECTORY."
  (dolist (subdirectory '("cur" "new" "tmp"))
    (make-directory (expand-file-name subdirectory directory) t)))

(defun tessera-fixtures--write-file (file contents)
  "Write CONTENTS to FILE unless it already has those contents."
  (make-directory (file-name-directory file) t)
  (unless (and (file-regular-p file)
               (with-temp-buffer
                 (insert-file-contents file)
                 (equal (buffer-string) contents)))
    (with-temp-file file
      (insert contents))))

(defun tessera-fixtures--random-state (seed)
  "Return a deterministic random state initialized with SEED."
  (cl-make-random-state seed))

(defun tessera-fixtures--random-item (items state)
  "Return a pseudo-random member of ITEMS using STATE."
  (nth (cl-random (length items) state) items))

(defun tessera-fixtures--time (index)
  "Return the fixture time at zero-based INDEX."
  (let* ((span
          (float-time
           (time-subtract tessera-fixtures--end-time
                          tessera-fixtures--start-time)))
         (offset
          (* span
             (/ (float index)
                (1- tessera-fixtures-entry-count)))))
    (time-add tessera-fixtures--start-time
              (seconds-to-time offset))))

(defun tessera-fixtures--date-header (index)
  "Return an RFC 5322 date for zero-based INDEX."
  (format-time-string "%a, %d %b %Y %T +0000"
                      (tessera-fixtures--time index) t))

(defun tessera-fixtures--person-address (person)
  "Return a fixture address for PERSON."
  (format "%s <person-%02d@example.test>"
          person
          (1+ (cl-position person tessera-fixtures--people
                           :test #'equal))))

(defun tessera-fixtures--lipsum (state &optional paragraphs)
  "Return PARAGRAPHS of community lorem ipsum using STATE."
  (mapconcat
   (lambda (_)
     (string-join
      (tessera-fixtures--random-item lorem-ipsum-text state)
      "  "))
   (number-sequence 1 (or paragraphs 1))
   "\n\n"))

(defun tessera-fixtures--subject (state &optional prefix)
  "Return a realistic subject using STATE and optional PREFIX."
  (concat prefix
          (capitalize
           (tessera-fixtures--random-item
            tessera-fixtures--topics state))))

(cl-defun tessera-fixtures--message
    (&key id subject from to date references in-reply-to
          extra-headers body content-type)
  "Return an RFC 5322 fixture message.

ID, SUBJECT, FROM, TO, and DATE define ordinary fields.
REFERENCES, IN-REPLY-TO, EXTRA-HEADERS, BODY, and CONTENT-TYPE
provide optional message content."
  (concat
   "From: " from "\n"
   "To: " to "\n"
   "Subject: " subject "\n"
   "Date: " date "\n"
   "Message-ID: <" id "@fixtures.tessera>\n"
   (when references
     (format "References: %s\n" references))
   (when in-reply-to
     (format "In-Reply-To: %s\n" in-reply-to))
   extra-headers
   "MIME-Version: 1.0\n"
   "Content-Type: " (or content-type "text/plain; charset=utf-8")
   "\n\n"
   (or body
       (format "Fixture body for %s.\n" subject))))

(defun tessera-fixtures--mail-file
    (maildir name &optional flags new)
  "Return fixture file NAME in MAILDIR with FLAGS.

When NEW is non-nil, return a file in the Maildir `new'
directory."
  (let* ((pattern
          (concat "\\`" (regexp-quote name)
                  "\\(?::2,.*\\)?\\'"))
         (existing
          (seq-some
           (lambda (subdirectory)
             (car (directory-files
                   (expand-file-name subdirectory maildir)
                   t pattern t)))
           '("cur" "new")))
         (filename
          (if new name (format "%s:2,%s" name (or flags "")))))
    (or existing
        (expand-file-name filename
                          (expand-file-name
                           (if new "new/" "cur/") maildir)))))

(defun tessera-fixtures--write-mu4e-mail
    (maildir name flags subject &rest options)
  "Write a mu4e fixture message to MAILDIR.

NAME and FLAGS determine the Maildir filename.  SUBJECT and
OPTIONS are passed to `tessera-fixtures--message'."
  (tessera-fixtures--ensure-maildir maildir)
  (tessera-fixtures--write-file
   (tessera-fixtures--mail-file maildir name flags)
   (apply #'tessera-fixtures--message
          :id name
          :subject subject
          options)))

(defun tessera-fixtures--create-mu4e-fixtures ()
  "Create Maildir messages covering mu4e display flags."
  (let ((personal-inbox
         (expand-file-name "personal/Inbox/"
                           tessera-fixtures--mail-root))
        (personal-sent
         (expand-file-name "personal/Sent/"
                           tessera-fixtures--mail-root))
        (personal-drafts
         (expand-file-name "personal/Drafts/"
                           tessera-fixtures--mail-root))
        (personal-archive
         (expand-file-name "personal/Archive/"
                           tessera-fixtures--mail-root))
        (personal-trash
         (expand-file-name "personal/Trash/"
                           tessera-fixtures--mail-root))
        (work-inbox
         (expand-file-name "work/Inbox/"
                           tessera-fixtures--mail-root))
        (work-drafts
         (expand-file-name "work/Drafts/"
                           tessera-fixtures--mail-root))
        (work-sent
         (expand-file-name "work/Sent/"
                           tessera-fixtures--mail-root))
        (work-trash
         (expand-file-name "work/Trash/"
                           tessera-fixtures--mail-root))
        (work-archive
         (expand-file-name "work/Archive/"
                           tessera-fixtures--mail-root))
        (date (tessera-fixtures--date-header 0)))
    (dolist (maildir
             (list personal-inbox personal-sent personal-drafts
                   personal-archive personal-trash work-inbox
                   work-sent work-drafts work-archive work-trash))
      (tessera-fixtures--ensure-maildir maildir))
    (tessera-fixtures--write-mu4e-mail
     personal-inbox "1001.mu.fixture" ""
     "Unread personal message"
     :from "Alex Friend <alex@example.test>"
     :to "Personal Fixture <personal@fixtures.test>"
     :date date)
    (tessera-fixtures--write-file
     (tessera-fixtures--mail-file
      personal-inbox "1014.mu.fixture" nil t)
     (tessera-fixtures--message
      :id "1014.mu.fixture"
      :subject "New personal message"
      :from "New Sender <new@example.test>"
      :to "Personal Fixture <personal@fixtures.test>"
      :date date))
    (tessera-fixtures--write-mu4e-mail
     personal-inbox "1002.mu.fixture" "FS"
     "Seen and flagged message"
     :from "Blair Friend <blair@example.test>"
     :to "Personal Fixture <personal@fixtures.test>"
     :date date)
    (tessera-fixtures--write-mu4e-mail
     personal-sent "1003.mu.fixture" "PRS"
     "Passed and replied message"
     :from "Personal Fixture <personal@fixtures.test>"
     :to "Casey Friend <casey@example.test>"
     :date date)
    (tessera-fixtures--write-mu4e-mail
     personal-trash "1004.mu.fixture" "ST"
     "Trashed message"
     :from "Dana Friend <dana@example.test>"
     :to "Personal Fixture <personal@fixtures.test>"
     :date date)
    (tessera-fixtures--write-mu4e-mail
     work-drafts "1005.mu.fixture" "DS"
     "Draft message"
     :from "Work Fixture <work@fixtures.test>"
     :to "Team <team@fixtures.test>"
     :date date)
    (tessera-fixtures--write-mu4e-mail
     work-inbox "1006.mu.fixture" "S"
     "Message with attachment"
     :from "Build Bot <build@example.test>"
     :to "Work Fixture <work@fixtures.test>"
     :date date
     :content-type
     "multipart/mixed; boundary=\"fixture-boundary\""
     :body
     (concat
      "--fixture-boundary\n"
      "Content-Type: text/plain; charset=utf-8\n\n"
      "The build report is attached.\n"
      "--fixture-boundary\n"
      "Content-Type: text/plain; name=\"report.txt\"\n"
      "Content-Disposition: attachment; filename=\"report.txt\"\n\n"
      "Fixture attachment.\n"
      "--fixture-boundary--\n"))
    (tessera-fixtures--write-mu4e-mail
     work-inbox "1007.mu.fixture" "S"
     "Mailing list message"
     :from "Tessera List <list@example.test>"
     :to "Work Fixture <work@fixtures.test>"
     :date date
     :extra-headers
     (concat
      "List-Id: Tessera Developers <tessera.example.test>\n"
      "List-Post: <mailto:list@example.test>\n"))
    (tessera-fixtures--write-mu4e-mail
     work-inbox "1008.mu.fixture" "S"
     "Calendar invitation"
     :from "Scheduler <calendar@example.test>"
     :to "Work Fixture <work@fixtures.test>"
     :date date
     :content-type "text/calendar; method=REQUEST; charset=utf-8"
     :body
     (concat
      "BEGIN:VCALENDAR\n"
      "METHOD:REQUEST\n"
      "BEGIN:VEVENT\n"
      "UID:tessera-calendar-fixture\n"
      "SUMMARY:Tessera fixture review\n"
      "END:VEVENT\n"
      "END:VCALENDAR\n"))
    (tessera-fixtures--write-mu4e-mail
     work-archive "1009.mu.fixture" "S"
     "Signed message"
     :from "Signer <signer@example.test>"
     :to "Work Fixture <work@fixtures.test>"
     :date date
     :content-type
     (concat
      "multipart/signed; protocol=\"application/pgp-signature\"; "
      "boundary=\"signed-boundary\"")
     :body
     (concat
      "--signed-boundary\n"
      "Content-Type: text/plain\n\n"
      "Signed fixture content.\n"
      "--signed-boundary\n"
      "Content-Type: application/pgp-signature\n\n"
      "-----BEGIN PGP SIGNATURE-----\n"
      "fixture\n"
      "-----END PGP SIGNATURE-----\n"
      "--signed-boundary--\n"))
    (tessera-fixtures--write-mu4e-mail
     work-archive "1010.mu.fixture" "S"
     "Encrypted message"
     :from "Cipher <cipher@example.test>"
     :to "Work Fixture <work@fixtures.test>"
     :date date
     :content-type
     (concat
      "multipart/encrypted; protocol=\"application/pgp-encrypted\"; "
      "boundary=\"encrypted-boundary\"")
     :body
     (concat
      "--encrypted-boundary\n"
      "Content-Type: application/pgp-encrypted\n\n"
      "Version: 1\n"
      "--encrypted-boundary\n"
      "Content-Type: application/octet-stream\n\n"
      "-----BEGIN PGP MESSAGE-----\n"
      "fixture\n"
      "-----END PGP MESSAGE-----\n"
      "--encrypted-boundary--\n"))
    (tessera-fixtures--write-mu4e-mail
     work-inbox "1011.mu.fixture" "S"
     "Thread: root message"
     :from "Root Author <root@example.test>"
     :to "Work Fixture <work@fixtures.test>"
     :date (tessera-fixtures--date-header 10))
    (tessera-fixtures--write-mu4e-mail
     work-inbox "1012.mu.fixture" ""
     "Re: Thread: root message"
     :from "Work Fixture <work@fixtures.test>"
     :to "Root Author <root@example.test>"
     :date (tessera-fixtures--date-header 11)
     :references "<1011.mu.fixture@fixtures.tessera>"
     :in-reply-to "<1011.mu.fixture@fixtures.tessera>")
    (tessera-fixtures--write-mu4e-mail
     work-inbox "1013.mu.fixture" "S"
     "Re: Thread: root message"
     :from "Reviewer <reviewer@example.test>"
     :to "Work Fixture <work@fixtures.test>"
     :date (tessera-fixtures--date-header 12)
     :references
     (concat "<1011.mu.fixture@fixtures.tessera> "
             "<1012.mu.fixture@fixtures.tessera>")
     :in-reply-to "<1012.mu.fixture@fixtures.tessera>")
    (let ((state (tessera-fixtures--random-state 20250101))
          (maildirs
           (vector personal-inbox personal-sent personal-drafts
                   personal-archive personal-trash work-inbox
                   work-sent work-drafts work-archive work-trash)))
      (cl-loop
       for index from 14 below tessera-fixtures-entry-count
       for serial = (+ 3000 index)
       for name = (format "%d.mu.fixture" serial)
       for person = (tessera-fixtures--random-item
                     tessera-fixtures--people state)
       for thread-position = (and (< index 174)
                                  (mod (- index 14) 4))
       for thread-root = (and thread-position
                              (- serial thread-position))
       for topic = (if thread-position
                       (nth (mod (/ (- index 14) 4)
                                 (length tessera-fixtures--topics))
                            tessera-fixtures--topics)
                     (tessera-fixtures--random-item
                      tessera-fixtures--topics state))
       for thread-sent = (and thread-position
                              (memq thread-position '(1 3)))
       for thread-work = (and thread-position
                              (cl-evenp (/ (- index 14) 4)))
       for maildir = (if thread-position
                         (cond
                          ((and thread-work thread-sent)
                           work-sent)
                          (thread-work work-inbox)
                          (thread-sent personal-sent)
                          (t personal-inbox))
                       (aref maildirs (mod index
                                           (length maildirs))))
       for work = (string-prefix-p
                   (expand-file-name
                    "work/" tessera-fixtures--mail-root)
                   maildir)
       for outgoing = (string-match-p
                       "/\\(?:Drafts\\|Sent\\)/" maildir)
       for own = (if work
                     "Work Fixture <work@fixtures.test>"
                   (concat
                    "Personal Fixture "
                    "<personal@fixtures.test>"))
       for correspondent = (tessera-fixtures--person-address
                            person)
       for references = (when (and thread-position
                                   (> thread-position 0))
                          (mapconcat
                           (lambda (offset)
                             (format
                              (concat
                               "<%d.mu.fixture"
                               "@fixtures.tessera>")
                              (+ thread-root offset)))
                           (number-sequence
                            0 (1- thread-position))
                           " "))
       for in-reply-to = (when (and thread-position
                                    (> thread-position 0))
                           (format
                            "<%d.mu.fixture@fixtures.tessera>"
                            (+ thread-root
                               (1- thread-position))))
       for flags = (cond
                    ((string-match-p "/Drafts/" maildir) "DS")
                    ((string-match-p "/Trash/" maildir) "ST")
                    ((string-match-p "/Sent/" maildir) "S")
                    ((zerop (mod index 5)) "F")
                    ((cl-evenp index) "S")
                    (t ""))
       do
       (tessera-fixtures--write-mu4e-mail
        maildir name flags
        (concat (when (and thread-position
                           (> thread-position 0))
                  "Re: ")
                (capitalize topic))
        :from (if outgoing own correspondent)
        :to (if outgoing correspondent own)
        :date (tessera-fixtures--date-header index)
        :references references
        :in-reply-to in-reply-to
        :body (tessera-fixtures--lipsum
               state (1+ (cl-random 3 state))))))))

(defun tessera-fixtures--gnus-message
    (name subject &optional references in-reply-to date from body)
  "Return Gnus fixture message NAME with SUBJECT.

REFERENCES and IN-REPLY-TO link threaded articles.  DATE, FROM,
and BODY customize the generated article."
  (tessera-fixtures--message
   :id (format "%s.gnus" name)
   :subject subject
   :from (or from
             (format "%s Author <%s@example.test>"
                     (capitalize name) name))
   :to "Gnus Fixture <gnus@fixtures.test>"
   :date (or date (tessera-fixtures--date-header 0))
   :references references
   :in-reply-to in-reply-to
   :body body))

(defun tessera-fixtures--create-gnus-fixtures ()
  "Create local Gnus groups, messages, threads, and mark files."
  (cl-loop
   for group in tessera-fixtures--gnus-groups
   for index from 0
   do
   (let ((maildir (expand-file-name group
                                    tessera-fixtures--gnus-root)))
     (tessera-fixtures--ensure-maildir maildir)
     (tessera-fixtures--write-file
      (tessera-fixtures--mail-file
       maildir (concat "2000." group ".fixture") "S")
      (tessera-fixtures--gnus-message
       group
       (format "%s: %s"
               (capitalize (replace-regexp-in-string
                            "-" " " group))
               (capitalize
                (nth index tessera-fixtures--topics)))
       nil nil (tessera-fixtures--date-header index)))))
  (let ((maildir
         (expand-file-name "level-1-critical/"
                           tessera-fixtures--gnus-root))
        (state (tessera-fixtures--random-state 20250102)))
    (tessera-fixtures--write-file
     (tessera-fixtures--mail-file
      maildir "2100.thread-root.fixture" "")
     (tessera-fixtures--gnus-message
      "thread-root" "Planning the package release"
      nil nil (tessera-fixtures--date-header 9)
      (tessera-fixtures--person-address "Priya Nair")
      (tessera-fixtures--lipsum state 2)))
    (tessera-fixtures--write-file
     (tessera-fixtures--mail-file
      maildir "2101.thread-child.fixture" "S")
     (tessera-fixtures--gnus-message
      "thread-child" "Re: Planning the package release"
      "<thread-root.gnus@fixtures.tessera>"
      "<thread-root.gnus@fixtures.tessera>"
      (tessera-fixtures--date-header 10)
      (tessera-fixtures--person-address "Akira Sato")
      (tessera-fixtures--lipsum state 1)))
    (tessera-fixtures--write-file
     (tessera-fixtures--mail-file
      maildir "2102.thread-grandchild.fixture" "S")
     (tessera-fixtures--gnus-message
      "thread-grandchild" "Re: Planning the package release"
      (concat "<thread-root.gnus@fixtures.tessera> "
              "<thread-child.gnus@fixtures.tessera>")
      "<thread-child.gnus@fixtures.tessera>"
      (tessera-fixtures--date-header 11)
      (tessera-fixtures--person-address "Samira Diallo")
      (tessera-fixtures--lipsum state 2)))
    (let ((serial 0)
          (date-index 11))
      (dolist (scenario tessera-fixtures--gnus-mark-scenarios)
        (cl-incf serial)
        (cl-incf date-index)
        (pcase-let ((`(,status ,storage ,subject . ,_)
                     scenario))
          (let* ((name (format "22%02d.%s.fixture"
                               serial status))
                 (flags
                  (if (eq status 'unread)
                      ""
                    (pcase storage
                      ('tick "F")
                      ('reply "R")
                      ('forward "P")
                      (_ "S")))))
            (tessera-fixtures--write-file
             (tessera-fixtures--mail-file maildir name flags)
             (tessera-fixtures--gnus-message
              (symbol-name status) subject nil nil
              (tessera-fixtures--date-header date-index)
              (tessera-fixtures--person-address
               (tessera-fixtures--random-item
                tessera-fixtures--people state))
              (tessera-fixtures--lipsum state 1)))))))
    (cl-loop
     for index from 41 below tessera-fixtures-entry-count
     for serial = (+ 4000 index)
     for name = (format "%d.article" serial)
     for thread-position = (and (< index 161)
                                (mod (- index 41) 4))
     for group-index = (if thread-position
                           (mod (/ (- index 41) 4) 9)
                         (mod index 9))
     for group = (nth group-index
                      tessera-fixtures--gnus-groups)
     for target = (expand-file-name group
                                    tessera-fixtures--gnus-root)
     for thread-root = (and thread-position
                            (- serial thread-position))
     for topic = (if thread-position
                     (nth (mod (/ (- index 41) 4)
                               (length tessera-fixtures--topics))
                          tessera-fixtures--topics)
                   (tessera-fixtures--random-item
                    tessera-fixtures--topics state))
     for references = (when (and thread-position
                                 (> thread-position 0))
                        (mapconcat
                         (lambda (offset)
                           (format
                            (concat
                             "<%d.article.gnus"
                             "@fixtures.tessera>")
                            (+ thread-root offset)))
                         (number-sequence 0 (1- thread-position))
                         " "))
     for in-reply-to = (when (and thread-position
                                  (> thread-position 0))
                         (format
                          "<%d.article.gnus@fixtures.tessera>"
                          (+ thread-root (1- thread-position))))
     do
     (tessera-fixtures--write-file
      (tessera-fixtures--mail-file
       target name (if (zerop (mod index 4)) "" "S"))
      (tessera-fixtures--gnus-message
       name
       (concat (when (and thread-position
                          (> thread-position 0))
                 "Re: ")
               (capitalize topic))
       references in-reply-to
       (tessera-fixtures--date-header index)
       (tessera-fixtures--person-address
        (tessera-fixtures--random-item
         tessera-fixtures--people state))
       (tessera-fixtures--lipsum
        state (1+ (cl-random 3 state))))))))

(defun tessera-fixtures--prepare-mu-index ()
  "Initialize and index the local mu4e fixture Maildir."
  (let ((mu (executable-find "mu")))
    (unless mu
      (error "The mu executable is required for mu4e fixtures"))
    (make-directory tessera-fixtures--mu-home t)
    (unless (file-directory-p
             (expand-file-name "xapian/" tessera-fixtures--mu-home))
      (unless (zerop
               (call-process
                mu nil nil nil
                "init"
                (format "--muhome=%s" tessera-fixtures--mu-home)
                (format "--maildir=%s" tessera-fixtures--mail-root)
                "--my-address=personal@fixtures.test"
                "--my-address=work@fixtures.test"))
        (error "Failed to initialize the fixture mu database")))
    (if (mu4e-running-p)
        (mu4e-update-index-nonlazy)
      (unless (zerop
               (call-process
                mu nil nil nil
                "index"
                (format "--muhome=%s"
                        tessera-fixtures--mu-home)))
        (error "Failed to index the fixture Maildirs")))))

(defun tessera-fixtures--elfeed-entry
    (feed-id base-tags index state)
  "Create an Elfeed entry for FEED-ID.

Use BASE-TAGS, zero-based INDEX, and random STATE to populate it."
  (let* ((id (format "entry-%03d" (1+ index)))
         (subject (tessera-fixtures--subject state))
         (paragraphs
          (split-string
           (tessera-fixtures--lipsum
            state (1+ (cl-random 3 state)))
           "\n\n" t))
         (tags
          (append base-tags
                  (unless (zerop (mod index 3)) '(unread))
                  (when (zerop (mod index 11)) '(starred))
                  (when (zerop (mod index 13)) '(archive))
                  (when (zerop (mod index 17)) '(important))
                  (when (zerop (mod index 19)) '(later)))))
    (elfeed-entry--create
     :id (cons feed-id id)
     :title subject
     :link (format "%s/%s" feed-id id)
     :date (float-time (tessera-fixtures--time index))
     :content
     (format "<p>%s</p>" (string-join paragraphs "</p><p>"))
     :content-type 'html
     :enclosures
     (when (zerop (mod index 23))
       (list
        (list (format "%s/%s/notes.pdf" feed-id id)
              "application/pdf" 4096)))
     :tags (elfeed-normalize-tags tags)
     :feed-id feed-id)))

(defun tessera-fixtures--prepare-elfeed ()
  "Create a deterministic local Elfeed database."
  (require 'elfeed)
  (setq elfeed-db-directory tessera-fixtures--elfeed-directory
        elfeed-default-directory tessera-fixtures--elfeed-directory
        elfeed-enclosure-default-dir
        (expand-file-name "enclosures/"
                          tessera-fixtures--elfeed-directory)
        elfeed-search-filter "@10years")
  (elfeed-db-load)
  (let ((state (tessera-fixtures--random-state 20250103))
        stale entries)
    (maphash
     (lambda (_ entry)
       (when (string-prefix-p
              "fixture://" (elfeed-entry-feed-id entry))
         (push entry stale)))
     elfeed-db-entries)
    (elfeed-db-delete stale)
    (cl-loop
     for index below tessera-fixtures-entry-count
     for feed-spec = (nth (mod index
                               (length
                                tessera-fixtures--feed-catalog))
                          tessera-fixtures--feed-catalog)
     do
     (pcase-let ((`(,slug ,title ,base-tags) feed-spec))
       (let* ((feed-id (format "fixture://%s" slug))
              (feed (elfeed-db-get-feed feed-id)))
         (setf (elfeed-feed-url feed) feed-id
               (elfeed-feed-title feed) title
               (elfeed-feed-author feed)
               (tessera-fixtures--random-item
                tessera-fixtures--people state))
         (push (tessera-fixtures--elfeed-entry
                feed-id base-tags index state)
               entries))))
    (elfeed-db-add (nreverse entries))
    (elfeed-db-save)))

(defun tessera-fixtures--mu-context-match (prefix message)
  "Return non-nil when MESSAGE's maildir has PREFIX."
  (and message
       (string-prefix-p
        prefix
        (or (plist-get message :maildir) ""))))

(defun tessera-fixtures--configure-mu4e ()
  "Configure mu4e for the local fixture database and contexts."
  (require 'mu4e)
  (make-directory
   (expand-file-name "attachments/" tessera-fixtures--mu-home)
   t)
  (make-directory
   (expand-file-name "tmp/" tessera-fixtures--mu-home)
   t)
  (setq mu4e-mu-home tessera-fixtures--mu-home
        mu4e-attachment-dir
        (expand-file-name "attachments/"
                          tessera-fixtures--mu-home)
        mu4e--temp-dir
        (expand-file-name "tmp/" tessera-fixtures--mu-home)
        mu4e-icalendar-diary-file
        (expand-file-name "diary" tessera-fixtures--mu-home)
        mu4e-context-policy 'pick-first
        mu4e-compose-context-policy 'pick-first
        mu4e-search-threads t
        mu4e-search-results-limit 500
        mu4e-headers-fields
        '((:flags . 14)
          (:maildir . 20)
          (:from-or-to . 24)
          (:thread-subject . nil))
        mu4e-contexts
        (list
         (make-mu4e-context
          :name "Personal"
          :match-func
          (apply-partially
           #'tessera-fixtures--mu-context-match "/personal")
          :vars
          '((user-mail-address . "personal@fixtures.test")
            (user-full-name . "Personal Fixture")
            (mu4e-sent-folder . "/personal/Sent")
            (mu4e-trash-folder . "/personal/Trash")
            (mu4e-drafts-folder . "/personal/Drafts")
            (mu4e-refile-folder . "/personal/Archive")))
         (make-mu4e-context
          :name "Work"
          :match-func
          (apply-partially
           #'tessera-fixtures--mu-context-match "/work")
          :vars
          '((user-mail-address . "work@fixtures.test")
            (user-full-name . "Work Fixture")
            (mu4e-sent-folder . "/work/Sent")
            (mu4e-trash-folder . "/work/Trash")
            (mu4e-drafts-folder . "/work/Drafts")
            (mu4e-refile-folder . "/work/Archive")))))
  (mu4e-context-switch t "Personal"))

(defun tessera-fixtures--gnus-group-name (level)
  "Return the full fixture Gnus group name for LEVEL."
  (format "nnmaildir+fixtures:level-%d-%s"
          level
          (nth (1- level)
               '("critical" "important" "primary" "normal"
                 "subscribed" "low" "unsubscribed" "zombie"
                 "killed"))))

(defun tessera-fixtures--configure-gnus ()
  "Configure Gnus for the local `nnmaildir' fixture server."
  (let* ((state
          (expand-file-name "state/"
                            tessera-fixtures--gnus-root))
         (articles (expand-file-name "articles/" state))
         (cache (expand-file-name "cache/" state))
         (messages (expand-file-name "messages/" state))
         (drafts (expand-file-name "drafts/" messages)))
    (dolist (directory
             (list state articles cache messages drafts))
      (make-directory directory t))
    (setq gnus-directory state
          gnus-home-directory state
          gnus-default-directory state
          gnus-init-file (expand-file-name "gnus-init.el" state)
          gnus-startup-file (expand-file-name "newsrc" state)
          gnus-dribble-directory state
          gnus-kill-files-directory state
          gnus-article-save-directory articles
          gnus-cache-directory cache
          message-directory messages
          message-auto-save-directory drafts
          mail-source-directory messages
          gnus-select-method '(nnnil "")
          gnus-secondary-select-methods
          `((nnmaildir "fixtures"
                       (directory ,tessera-fixtures--gnus-root)))
          gnus-use-dribble-file t
          gnus-always-read-dribble-file t
          gnus-save-newsrc-file t
          gnus-read-newsrc-file t
          gnus-check-new-newsgroups nil
          gnus-use-cache nil
          gnus-agent nil
          gnus-show-threads t
          gnus-fetch-old-headers t
          gnus-summary-default-score 0
          gnus-summary-mark-below nil
          gnus-summary-line-format
          "%U%R%O%z%e %5L: %-24,24f] %B%s\n")
    (add-hook 'gnus-started-hook
              #'tessera-fixtures--gnus-after-startup)))

(defun tessera-fixtures--gnus-after-startup ()
  "Expose all fixture groups after Gnus startup."
  (tessera-fixtures--gnus-set-levels)
  (gnus-group-list-groups 9 t 1))

(defun tessera-fixtures--gnus-set-levels ()
  "Set fixture Gnus groups to levels one through nine."
  (dotimes (index 9)
    (let* ((level (1+ index))
           (group (tessera-fixtures--gnus-group-name level))
           (method (gnus-find-method-for-group group))
           (entry (gnus-group-entry group)))
      (unless entry
        (gnus-subscribe-group group nil method)
        (setq entry (gnus-group-entry group)))
      (gnus-group-change-level entry level))))

(defun tessera-fixtures--gnus-article-number (subject)
  "Return the current Gnus article number with SUBJECT."
  (when-let* ((header
               (seq-find
                (lambda (candidate)
                  (equal subject (mail-header-subject candidate)))
                gnus-newsgroup-headers)))
    (mail-header-number header)))

(defun tessera-fixtures--gnus-install-marks ()
  "Apply every fixture mark in the current Gnus summary.

Persist backend-supported marks through `nnmaildir'.  Install
summary-only states, such as scores and agent marks, through the
  ordinary Gnus summary variables."
  (let (actions)
    (dolist (scenario tessera-fixtures--gnus-mark-scenarios)
      (when-let* ((storage-mark (nth 1 scenario))
                  (article
                   (tessera-fixtures--gnus-article-number
                    (nth 2 scenario))))
        (push (list (list article) 'set (list storage-mark))
              actions)))
    (when actions
      (gnus-request-set-mark gnus-newsgroup-name actions)))
  (let* ((article
          (lambda (status)
            (let ((scenario
                   (assq status
                         tessera-fixtures--gnus-mark-scenarios)))
              (or (tessera-fixtures--gnus-article-number
                   (nth 2 scenario))
                  (error "Missing Gnus fixture state: %s"
                         status)))))
         (unread (funcall article 'unread))
         (ticked (funcall article 'ticked))
         (dormant (funcall article 'dormant))
         (expirable (funcall article 'expirable))
         (spam (funcall article 'spam))
         (downloadable (funcall article 'downloadable))
         (unsendable (funcall article 'unsendable)))
    (setq gnus-newsgroup-unreads (list unread)
          gnus-newsgroup-marked (list ticked)
          gnus-newsgroup-dormant (list dormant)
          gnus-newsgroup-expirable (list expirable)
          gnus-newsgroup-spam-marked (list spam)
          gnus-newsgroup-killed
          (list (funcall article 'killed))
          gnus-newsgroup-ancient
          (list (funcall article 'ancient))
          gnus-newsgroup-sparse
          (list (funcall article 'sparse))
          gnus-newsgroup-reads
          (mapcar
           (lambda (spec)
             (cons (funcall article (car spec)) (cdr spec)))
           `((del . ,gnus-del-mark)
             (read . ,gnus-read-mark)
             (killed . ,gnus-killed-mark)
             (kill-file . ,gnus-kill-file-mark)
             (low-score . ,gnus-low-score-mark)
             (catchup . ,gnus-catchup-mark)
             (ancient . ,gnus-ancient-mark)
             (sparse . ,gnus-sparse-mark)
             (canceled . ,gnus-canceled-mark)
             (duplicate . ,gnus-duplicate-mark)))
          gnus-newsgroup-processable
          (list (funcall article 'process))
          gnus-newsgroup-replied
          (list (funcall article 'replied))
          gnus-newsgroup-forwarded
          (list (funcall article 'forwarded))
          gnus-newsgroup-cached
          (list (funcall article 'cached))
          gnus-newsgroup-saved
          (list (funcall article 'saved))
          gnus-newsgroup-unseen
          (list (funcall article 'unseen))
          gnus-newsgroup-undownloaded
          (list (funcall article 'undownloaded))
          gnus-newsgroup-downloadable (list downloadable)
          gnus-newsgroup-unsendable (list unsendable)
          gnus-newsgroup-scored
          (cl-loop
           for scenario in tessera-fixtures--gnus-mark-scenarios
           for score = (nth 3 scenario)
           when score
           collect (cons (funcall article (car scenario)) score))
          gnus-newsgroup-agentized t)
    (dolist (header gnus-newsgroup-headers)
      (let ((number (mail-header-number header)))
        (unless (or (memq number gnus-newsgroup-unreads)
                    (memq number gnus-newsgroup-marked)
                    (memq number gnus-newsgroup-dormant)
                    (memq number gnus-newsgroup-expirable)
                    (memq number gnus-newsgroup-spam-marked)
                    (memq number gnus-newsgroup-downloadable)
                    (memq number gnus-newsgroup-unsendable)
                    (assq number gnus-newsgroup-reads))
          (push (cons number gnus-read-mark)
                gnus-newsgroup-reads))))
    (gnus-summary-prepare)
    (when (gnus-summary-goto-subject
           (funcall article 'recent) nil t)
      (gnus-summary-update-mark gnus-recent-mark 'replied))))

;;;###autoload
(defun tessera-fixtures-prepare-elfeed ()
  "Prepare the local Elfeed fixtures."
  (interactive)
  (make-directory tessera-fixtures-state-directory t)
  (tessera-fixtures--prepare-elfeed)
  (message "Prepared Elfeed fixtures in %s"
           tessera-fixtures--elfeed-directory))

;;;###autoload
(defun tessera-fixtures-prepare-mu4e ()
  "Prepare the local mu4e fixtures."
  (interactive)
  (make-directory tessera-fixtures-state-directory t)
  (tessera-fixtures--create-mu4e-fixtures)
  (tessera-fixtures--prepare-mu-index)
  (tessera-fixtures--configure-mu4e)
  (message "Prepared mu4e fixtures in %s"
           tessera-fixtures--mu-home))

;;;###autoload
(defun tessera-fixtures-prepare-gnus ()
  "Prepare the local Gnus fixtures."
  (interactive)
  (make-directory tessera-fixtures-state-directory t)
  (tessera-fixtures--create-gnus-fixtures)
  (tessera-fixtures--configure-gnus)
  (message "Prepared Gnus fixtures in %s"
           tessera-fixtures--gnus-root))

;;;###autoload
(defun tessera-fixtures-prepare ()
  "Prepare all local Tessera fixtures."
  (interactive)
  (tessera-fixtures-prepare-elfeed)
  (tessera-fixtures-prepare-mu4e)
  (tessera-fixtures-prepare-gnus)
  (message "Prepared Tessera fixtures in %s"
           tessera-fixtures-state-directory))

;;;###autoload
(defun tessera-fixtures-open-elfeed ()
  "Prepare and display the local Elfeed fixtures."
  (interactive)
  (tessera-fixtures-prepare-elfeed)
  (elfeed-search nil)
  (elfeed-search-update :force))

;;;###autoload
(defun tessera-fixtures-open-mu4e ()
  "Prepare and display all local mu4e fixture messages."
  (interactive)
  (tessera-fixtures-prepare-mu4e)
  (mu4e-search "maildir:/personal/* OR maildir:/work/*"))

;;;###autoload
(defun tessera-fixtures-open-gnus ()
  "Prepare and display the local Gnus fixture groups."
  (interactive)
  (tessera-fixtures-prepare-gnus)
  (gnus)
  (tessera-fixtures--gnus-set-levels)
  (gnus-group-list-groups 9 t 1))

;;;###autoload
(defun tessera-fixtures-open-gnus-marks ()
  "Display the Gnus summary containing every fixture mark."
  (interactive)
  (tessera-fixtures-open-gnus)
  (gnus-group-read-group
   t t (tessera-fixtures--gnus-group-name 1))
  (tessera-fixtures--gnus-install-marks)
  (let* ((group gnus-newsgroup-name)
         (info (gnus-get-info group))
         (method (gnus-find-method-for-group group)))
    (gnus-request-update-info info method)
    (gnus-summary-exit-no-update t)
    (gnus-group-read-group t t group)
    (tessera-fixtures--gnus-install-marks)))

(provide 'tessera-fixtures)
;;; tessera-fixtures.el ends here
