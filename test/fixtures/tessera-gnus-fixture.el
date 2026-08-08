;;; tessera-gnus-fixture.el --- Gnus fixture for Tessera  -*- lexical-binding: t; -*-

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

;; A development-only nnml group with representative articles, marks,
;; scores, and MIME headers for inspecting Tessera in Emacs.

;;; Code:

(require 'cl-lib)
(require 'gnus-group)
(require 'gnus-sum)
(require 'nnml)
(require 'rfc2047)
(require 'subr-x)

(defconst tessera-gnus-fixture-server "tessera-fixture"
  "Name of the local fixture server.")

(defconst tessera-gnus-fixture-group
  "nnml+tessera-fixture:tessera.summary"
  "Full name of the local fixture group.")

(defconst tessera-gnus-fixture-directory
  (expand-file-name
   "fixtures/gnus/" user-emacs-directory)
  "Directory containing the local fixture server.")

(defconst tessera-gnus-fixture-address
  (if (string-empty-p (or user-mail-address ""))
      "tessera@fixture.invalid"
    user-mail-address)
  "Address used to identify personal fixture articles.")

(defconst tessera-gnus-fixture-method
  `(nnml ,tessera-gnus-fixture-server
         (nnml-directory ,tessera-gnus-fixture-directory)
         (nnml-active-file
          ,(expand-file-name "active" tessera-gnus-fixture-directory))
         (nnml-newsgroups-file
          ,(expand-file-name
            "newsgroups" tessera-gnus-fixture-directory))
         (nnml-get-new-mail nil))
  "Gnus select method for the local fixture server.")

(defconst tessera-gnus-fixture--extra-headers
  '(Content-Type Content-Disposition List-Id List-Post Mailing-List)
  "Extra headers required by the Tessera feature detector.")

(defconst tessera-gnus-fixture--primary-marks
  '(gnus-unsendable-mark
    gnus-downloadable-mark
    gnus-unread-mark
    gnus-ticked-mark
    gnus-spam-mark
    gnus-dormant-mark
    gnus-expirable-mark
    gnus-del-mark
    gnus-read-mark
    gnus-killed-mark
    gnus-kill-file-mark
    gnus-low-score-mark
    gnus-catchup-mark
    gnus-ancient-mark
    gnus-sparse-mark
    gnus-canceled-mark
    gnus-duplicate-mark
    gnus-recent-mark)
  "Primary marks represented by fixture articles.")

(defconst tessera-gnus-fixture--secondary-marks
  '(processable cached replied forwarded saved unseen nil)
  "Secondary marks represented by fixture articles.")

(defconst tessera-gnus-fixture--scores '(over below nil)
  "Score states represented by fixture articles.")

(defconst tessera-gnus-fixture--authors
  '(("Ada Lovelace" . "ada@fixture.invalid")
    ("Alan Turing" . "alan@fixture.invalid")
    ("Grace Hopper" . "grace@fixture.invalid")
    ("Edsger Dijkstra" . "edsger@fixture.invalid")
    ("Margaret Hamilton" . "margaret@fixture.invalid")
    ("Barbara Liskov" . "barbara@fixture.invalid")
    ("Donald Knuth" . "donald@fixture.invalid")
    ("Frances Allen" . "frances@fixture.invalid")
    ("Ken Thompson" . "ken@fixture.invalid")
    ("Radia Perlman" . "radia@fixture.invalid")
    ("Guido van Rossum" . "guido@fixture.invalid")
    ("Yukihiro Matsumoto" . "matz@fixture.invalid"))
  "Authors used by fixture articles.")

(defconst tessera-gnus-fixture--thread-subjects
  '("Security review for the offline message renderer"
    "Release planning for the autumn interface refresh"
    "Pixel alignment and accessibility in proportional fonts"
    "Community conference travel and mentoring coordination"
    "Incident report: delayed synchronization across mirrors")
  "Root subjects of the fixture threads.")

(defconst tessera-gnus-fixture--single-subjects
  (list
   (concat
    "A standalone announcement with a deliberately long subject "
    "for truncation")
   "Calendar invitation: Tessera interface review"
   "Signed release checklist"
   "Encrypted note for the maintainers"
   "Short update")
  "Subjects of the standalone fixture articles.")

(defconst tessera-gnus-fixture--monthly-articles
  '(("August 2026 archive: final flat-list review"
     "01 Aug 2026 09:15:00 +0800")
    ("July 2026 archive: keyboard navigation notes"
     "01 Jul 2026 09:15:00 +0800")
    ("June 2026 archive: compact window observations"
     "01 Jun 2026 09:15:00 +0800")
    ("May 2026 archive: attachment indicator review"
     "01 May 2026 09:15:00 +0800")
    ("April 2026 archive: semantic date discussion"
     "01 Apr 2026 09:15:00 +0800")
    ("March 2026 archive: sender alignment follow-up"
     "01 Mar 2026 09:15:00 +0800")
    ("February 2026 archive: unread state review"
     "01 Feb 2026 09:15:00 +0800")
    ("January 2026 archive: annual interface planning"
     "01 Jan 2026 09:15:00 +0800")
    ("December 2025 archive: year-end release notes"
     "01 Dec 2025 09:15:00 +0800")
    ("November 2025 archive: offline synchronization"
     "01 Nov 2025 09:15:00 +0800")
    ("October 2025 archive: theme compatibility report"
     "01 Oct 2025 09:15:00 +0800")
    ("September 2025 archive: proportional font study"
     "01 Sep 2025 09:15:00 +0800")
    ("August 2025 archive: accessibility improvements"
     "01 Aug 2025 09:15:00 +0800")
    ("July 2025 archive: message action feedback"
     "01 Jul 2025 09:15:00 +0800")
    ("June 2025 archive: status presentation review"
     "01 Jun 2025 09:15:00 +0800")
    ("May 2025 archive: feature glyph proposals"
     "01 May 2025 09:15:00 +0800")
    ("April 2025 archive: summary layout experiment"
     "01 Apr 2025 09:15:00 +0800")
    ("March 2025 archive: native mark inventory"
     "01 Mar 2025 09:15:00 +0800")
    ("February 2025 archive: initial fixture planning"
     "01 Feb 2025 09:15:00 +0800")
    ("January 2025 archive: project kickoff notes"
     "01 Jan 2025 09:15:00 +0800"))
  "Subjects and dates of the monthly fixture articles.")

(defconst tessera-gnus-fixture--thread-parents
  '(nil 0 1 1 2 2 3 5 5)
  "Parent indexes used to form each nine-article thread.")

(defconst tessera-gnus-fixture--thread-date-settings
  '(("01 Aug 2026 09:00:00 +0800" . 18)
    ("05 Aug 2026 09:00:00 +0800" . 3)
    ("30 Jul 2026 09:00:00 +0800" . 27)
    ("20 Jul 2026 09:00:00 +0800" . 6)
    ("28 Jun 2026 09:00:00 +0800" . 12))
  "Root dates and member intervals for fixture threads.")

(defconst tessera-gnus-fixture--feature-profiles
  `((plain
     :content-type "text/plain; charset=utf-8")
    (signed
     :content-type
     ,(concat
       "multipart/signed; protocol=\"application/pgp-signature\"; "
       "boundary=\"fixture-signed\""))
    (encrypted
     :content-type
     ,(concat
       "multipart/encrypted; protocol=\"application/pgp-encrypted\"; "
       "boundary=\"fixture-encrypted\""))
    (attachment
     :content-type
     ,(concat
       "multipart/mixed; boundary=\"fixture-attachment\"; "
       "name=\"briefing.eml\"")
     :disposition "attachment; filename=\"briefing.eml\"")
    (calendar
     :content-type "text/calendar; charset=utf-8; method=REQUEST")
    (mailing-list
     :content-type "text/plain; charset=utf-8"
     :list t)
    (personal
     :content-type "text/plain; charset=utf-8"
     :personal t)
    (overflow
     :content-type
     ,(concat
       "multipart/signed; protocol=\"application/pgp-signature\"; "
       "boundary=\"fixture-rich\"; name=\"report.eml\"")
     :disposition "attachment; filename=\"report.eml\""
     :list t
     :personal t)
    (encrypted-list
     :content-type
     ,(concat
       "multipart/encrypted; protocol=\"application/pgp-encrypted\"; "
       "boundary=\"fixture-encrypted\"")
     :list t
     :personal t)
    (calendar-list
     :content-type
     ,(concat
       "text/calendar; charset=utf-8; method=REQUEST; "
       "name=\"review.ics\"")
     :disposition "attachment; filename=\"review.ics\""
     :list t))
  "MIME and recipient profiles represented by fixture articles.")

(defun tessera-gnus-fixture--server-definitions ()
  "Return the server variables in `tessera-gnus-fixture-method'."
  (cddr tessera-gnus-fixture-method))

(defun tessera-gnus-fixture--profile (number)
  "Return the feature profile assigned to article NUMBER."
  (nth (mod (1- number)
            (length tessera-gnus-fixture--feature-profiles))
       tessera-gnus-fixture--feature-profiles))

(defun tessera-gnus-fixture--author (number)
  "Return the author assigned to article NUMBER."
  (nth (mod (1- number) (length tessera-gnus-fixture--authors))
       tessera-gnus-fixture--authors))

(defun tessera-gnus-fixture--message-id (thread index)
  "Return the fixture message ID for THREAD and INDEX."
  (format "<tessera-%s-%02d@fixture.invalid>" thread index))

(defun tessera-gnus-fixture--thread-references (thread index)
  "Return References for article INDEX in THREAD."
  (let (references)
    (while-let
        ((parent
          (nth index tessera-gnus-fixture--thread-parents)))
      (push
       (tessera-gnus-fixture--message-id thread parent)
       references)
      (setq index parent))
    references))

(defun tessera-gnus-fixture--thread-date (thread index)
  "Return the date of member INDEX in fixture THREAD."
  (pcase-let ((`(,root . ,interval)
               (nth (1- thread)
                    tessera-gnus-fixture--thread-date-settings)))
    (format-time-string
     "%a, %d %b %Y %T %z"
     (time-add
      (date-to-time root)
      (seconds-to-time (* index interval 3600)))
     (* 8 3600))))

(defun tessera-gnus-fixture--thread-specs ()
  "Return the 45 threaded fixture article specifications."
  (let ((number 0)
        specs)
    (cl-loop
     for subject in tessera-gnus-fixture--thread-subjects
     for thread from 1
     do
     (dotimes (index 9)
       (setq number (1+ number))
       (let ((references
              (tessera-gnus-fixture--thread-references thread index)))
         (push
          (list :number number
                :subject (if (zerop index)
                             subject
                           (concat "Re: " subject))
                :thread thread
                :index index
                :date
                (tessera-gnus-fixture--thread-date thread index)
                :message-id
                (tessera-gnus-fixture--message-id thread index)
                :in-reply-to (car (last references))
                :references references)
          specs))))
    (nreverse specs)))

(defun tessera-gnus-fixture--single-specs ()
  "Return the five standalone fixture article specifications."
  (cl-loop
   for subject in tessera-gnus-fixture--single-subjects
   for number from 46
   collect (list :number number
                 :subject subject
                 :message-id
                 (tessera-gnus-fixture--message-id "single" number))))

(defun tessera-gnus-fixture--monthly-specs ()
  "Return the 20 monthly fixture article specifications."
  (cl-loop
   for (subject date) in tessera-gnus-fixture--monthly-articles
   for number from 51
   collect (list :number number
                 :subject subject
                 :date date
                 :message-id
                 (tessera-gnus-fixture--message-id "month" number))))

(defun tessera-gnus-fixture--specs ()
  "Return all fixture article specifications."
  (append (tessera-gnus-fixture--thread-specs)
          (tessera-gnus-fixture--single-specs)
          (tessera-gnus-fixture--monthly-specs)))

(defun tessera-gnus-fixture--date (number)
  "Return a recent mail date for article NUMBER."
  (format-time-string
   "%a, %d %b %Y %T %z"
   (time-subtract (current-time)
                  (seconds-to-time (* (- 50 number) 7200)))))

(defun tessera-gnus-fixture--body (profile number)
  "Return a MIME body for PROFILE and article NUMBER."
  (pcase (car profile)
    ((or 'signed 'overflow)
     (let ((boundary (if (eq (car profile) 'overflow)
                         "fixture-rich"
                       "fixture-signed")))
       (concat
        "--" boundary "\nContent-Type: text/plain; charset=utf-8\n\n"
        (format "Signed fixture article %d.\n" number)
        "--" boundary "\nContent-Type: application/pgp-signature\n\n"
        "-----BEGIN PGP SIGNATURE-----\nfixture\n"
        "-----END PGP SIGNATURE-----\n--" boundary "--\n")))
    ((or 'encrypted 'encrypted-list)
     (concat
      "--fixture-encrypted\n"
      "Content-Type: application/pgp-encrypted\n\nVersion: 1\n"
      "--fixture-encrypted\n"
      "Content-Type: application/octet-stream\n\n"
      "-----BEGIN PGP MESSAGE-----\nfixture\n"
      "-----END PGP MESSAGE-----\n--fixture-encrypted--\n"))
    ('attachment
     (concat
      "--fixture-attachment\n"
      "Content-Type: text/plain; charset=utf-8\n\n"
      (format "Attachment fixture article %d.\n" number)
      "--fixture-attachment\n"
      "Content-Type: text/plain; name=\"notes.txt\"\n"
      "Content-Disposition: attachment; filename=\"notes.txt\"\n\n"
      "Fixture attachment.\n--fixture-attachment--\n"))
    ((or 'calendar 'calendar-list)
     (format
      (concat "BEGIN:VCALENDAR\nVERSION:2.0\nBEGIN:VEVENT\n"
              "UID:tessera-%d@fixture.invalid\n"
              "SUMMARY:Tessera fixture review\n"
              "DTSTART:20260808T090000Z\nDTEND:20260808T100000Z\n"
              "END:VEVENT\nEND:VCALENDAR\n")
      number))
    (_
     (format
      (concat "This is Tessera fixture article %d.\n\n"
              "It contains enough body text to open as an "
              "ordinary article while the Summary buffer "
              "exercises presentation state.\n")
      number))))

(defun tessera-gnus-fixture--insert-article (spec)
  "Insert the RFC message described by SPEC into the current buffer."
  (let* ((number (plist-get spec :number))
         (author (tessera-gnus-fixture--author number))
         (profile (tessera-gnus-fixture--profile number))
         (properties (cdr profile))
         (content-type (plist-get properties :content-type))
         (personal (plist-get properties :personal))
         (listp (plist-get properties :list))
         (references (plist-get spec :references)))
    (insert "From: " (rfc2047-encode-string (car author))
            " <" (cdr author) ">\n")
    (insert "To: "
            (if personal
                (format "Tessera Developer <%s>"
                        tessera-gnus-fixture-address)
              "Tessera Discussion <tessera-list@fixture.invalid>")
            "\n")
    (insert "Subject: "
            (rfc2047-encode-string (plist-get spec :subject)) "\n")
    (insert "Date: "
            (or (plist-get spec :date)
                (tessera-gnus-fixture--date number))
            "\n")
    (insert "Message-ID: " (plist-get spec :message-id) "\n")
    (when references
      (insert "References: " (string-join references " ") "\n"))
    (when-let* ((parent (plist-get spec :in-reply-to)))
      (insert "In-Reply-To: " parent "\n"))
    (when listp
      (insert "List-Id: Tessera fixture <tessera.fixture.invalid>\n"
              "List-Post: <mailto:tessera-list@fixture.invalid>\n"
              "Mailing-List: "
              "contact tessera-list-help@fixture.invalid\n"))
    (insert "MIME-Version: 1.0\n"
            "Content-Type: " content-type "\n")
    (when-let* ((disposition (plist-get properties :disposition)))
      (insert "Content-Disposition: " disposition "\n"))
    (insert "X-Tessera-Fixture: article-"
            (number-to-string number) "\n\n")
    (insert (tessera-gnus-fixture--body profile number))))

(defun tessera-gnus-fixture--install-articles ()
  "Install the fixture articles in the local nnml server."
  (make-directory tessera-gnus-fixture-directory t)
  (nnml-open-server tessera-gnus-fixture-server
                    (tessera-gnus-fixture--server-definitions))
  (nnml-request-create-group "tessera.summary"
                             tessera-gnus-fixture-server)
  (let ((newsgroups
         (expand-file-name "newsgroups"
                           tessera-gnus-fixture-directory)))
    (unless (file-exists-p newsgroups)
      (with-temp-file newsgroups
        (insert "tessera.summary Tessera Summary visual fixture\n"))))
  (let* ((group-directory
          (expand-file-name "tessera/summary/"
                            tessera-gnus-fixture-directory))
         (articles
          (and (file-directory-p group-directory)
               (directory-files group-directory nil "\\`[0-9]+\\'")))
         (installed (length articles))
         (all-specs (tessera-gnus-fixture--specs))
         (last-number (length all-specs))
         (specs (nthcdr installed all-specs)))
    (dolist (spec specs)
      (with-temp-buffer
        (tessera-gnus-fixture--insert-article spec)
        (nnml-request-accept-article
         "tessera.summary"
         tessera-gnus-fixture-server
         (= (plist-get spec :number) last-number)))))
  (nnml-close-server tessera-gnus-fixture-server))

(defun tessera-gnus-fixture--primary-mark (article)
  "Return the primary mark variable assigned to ARTICLE."
  (nth (mod (1- article)
            (length tessera-gnus-fixture--primary-marks))
       tessera-gnus-fixture--primary-marks))

(defun tessera-gnus-fixture--secondary-mark (article)
  "Return the secondary mark state assigned to ARTICLE."
  (nth (mod (1- article)
            (length tessera-gnus-fixture--secondary-marks))
       tessera-gnus-fixture--secondary-marks))

(defun tessera-gnus-fixture--score (article)
  "Return the score state assigned to ARTICLE."
  (nth (mod (1- article) (length tessera-gnus-fixture--scores))
       tessera-gnus-fixture--scores))

(defun tessera-gnus-fixture--mark-primary (article variable)
  "Give ARTICLE the primary mark named by VARIABLE."
  (let ((mark (symbol-value variable)))
    (if (memq variable
              '(gnus-unread-mark gnus-ticked-mark gnus-spam-mark
                                 gnus-dormant-mark))
        (gnus-mark-article-as-unread article mark)
      (gnus-mark-article-as-read article mark))
    (pcase variable
      ('gnus-unsendable-mark
       (push article gnus-newsgroup-unsendable))
      ('gnus-downloadable-mark
       (push article gnus-newsgroup-downloadable)))))

(defun tessera-gnus-fixture--mark-secondary (article state)
  "Give ARTICLE the secondary mark represented by STATE."
  (pcase state
    ('processable (push article gnus-newsgroup-processable))
    ('cached (push article gnus-newsgroup-cached))
    ('replied (push article gnus-newsgroup-replied))
    ('forwarded (push article gnus-newsgroup-forwarded))
    ('saved (push article gnus-newsgroup-saved))
    ('unseen (push article gnus-newsgroup-unseen))))

(defun tessera-gnus-fixture--mark-score (article state)
  "Give ARTICLE the score represented by STATE."
  (pcase state
    ('over (push (cons article 100) gnus-newsgroup-scored))
    ('below (push (cons article -100) gnus-newsgroup-scored))))

(defun tessera-gnus-fixture--apply-marks ()
  "Apply the fixture mark matrix before generating its Summary."
  (when (equal gnus-newsgroup-name tessera-gnus-fixture-group)
    (setq gnus-newsgroup-unreads nil
          gnus-newsgroup-marked nil
          gnus-newsgroup-spam-marked nil
          gnus-newsgroup-dormant nil
          gnus-newsgroup-expirable nil
          gnus-newsgroup-reads nil
          gnus-newsgroup-downloadable nil
          gnus-newsgroup-unsendable nil
          gnus-newsgroup-processable nil
          gnus-newsgroup-cached nil
          gnus-newsgroup-replied nil
          gnus-newsgroup-forwarded nil
          gnus-newsgroup-saved nil
          gnus-newsgroup-unseen nil
          gnus-newsgroup-scored nil)
    (setq-local gnus-summary-default-score 0)
    (setq-local gnus-summary-zcore-fuzz 0)
    (dolist (header gnus-newsgroup-headers)
      (let ((article (mail-header-number header)))
        (tessera-gnus-fixture--mark-primary
         article (tessera-gnus-fixture--primary-mark article))
        (tessera-gnus-fixture--mark-secondary
         article (tessera-gnus-fixture--secondary-mark article))
        (tessera-gnus-fixture--mark-score
         article (tessera-gnus-fixture--score article))))))

(defun tessera-gnus-fixture--summary-setup ()
  "Install fixture behavior in its Summary buffer."
  (when (equal gnus-newsgroup-name tessera-gnus-fixture-group)
    (add-hook 'gnus-summary-generate-hook
              #'tessera-gnus-fixture--apply-marks 50 t)))

(defun tessera-gnus-fixture-install ()
  "Install and register the local Tessera Gnus fixture."
  (interactive)
  (dolist (header tessera-gnus-fixture--extra-headers)
    (add-to-list 'gnus-extra-headers header)
    (add-to-list 'nnmail-extra-headers header))
  (add-to-list 'gnus-secondary-select-methods
               tessera-gnus-fixture-method t)
  (add-hook 'gnus-summary-mode-hook
            #'tessera-gnus-fixture--summary-setup)
  (tessera-gnus-fixture--install-articles))

(defun tessera-gnus-fixture-open ()
  "Open the local Tessera Gnus fixture group."
  (interactive)
  (unless (and (boundp 'gnus-group-buffer)
               (buffer-live-p gnus-group-buffer))
    (gnus))
  (with-current-buffer gnus-group-buffer
    (unless (gnus-group-entry tessera-gnus-fixture-group)
      (gnus-group-make-group "tessera.summary"
                             tessera-gnus-fixture-method))
    (gnus-activate-group tessera-gnus-fixture-group)
    (gnus-group-read-group
     (length (tessera-gnus-fixture--specs))
     t tessera-gnus-fixture-group)))

(tessera-gnus-fixture-install)

(provide 'tessera-gnus-fixture)
;;; tessera-gnus-fixture.el ends here
