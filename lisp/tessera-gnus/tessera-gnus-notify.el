;;; tessera-gnus-notify.el --- Gnus notifications for Tessera  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bingshan Chang <chang@bingshan.org>

;; Author: Bingshan Chang <chang@bingshan.org>
;; Maintainer: Bingshan Chang <chang@bingshan.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (tessera "0.1.0"))
;; Keywords: convenience, mail, news

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

;; Desktop notifications for articles found by native Gnus updates.

;;; Code:

(require 'cl-lib)
(require 'gnus-art)
(require 'gnus-group)
(require 'gnus-int)
(require 'gnus-sum)
(require 'image)
(require 'mail-extr)
(require 'seq)
(require 'subr-x)
(require 'tessera-notify)

(defvar gnus-level-subscribed)
(defvar gnus-newsgroup-name)
(defvar gnus-newsrc-alist)
(defvar nntp-server-buffer)
(defvar tessera-gnus-notify-enable)
(defvar tessera-gnus-notify-limit)

(defvar tessera-gnus-notify--installed-p nil
  "Non-nil when Tessera has installed Gnus notification support.")

(defvar tessera-gnus-notify--snapshot nil
  "Unread Gnus articles recorded before the current update.")

(defvar tessera-gnus-notify--update-depth 0
  "Current nesting depth of advised Gnus update commands.")

(defconst tessera-gnus-notify--update-functions
  '(gnus-group-get-new-news
    gnus-group-get-new-news-this-group)
  "Gnus update commands observed for failures.")

(defun tessera-gnus-notify--subscribed-group-p (group)
  "Return non-nil when GROUP is a subscribed Gnus group."
  (and (not (equal group "dummy.group"))
       (<= (gnus-group-level group) gnus-level-subscribed)))

(defun tessera-gnus-notify--unread-articles ()
  "Return unread articles for successfully read Gnus groups."
  (let (articles)
    (dolist (entry gnus-newsrc-alist)
      (let ((group (car entry)))
        (when (tessera-gnus-notify--subscribed-group-p group)
          (let ((count (gnus-group-unread group)))
            (when (numberp count)
              (if (<= count 0)
                  (push (list group) articles)
                (condition-case nil
                    (when-let* ((unread
                                 (gnus-list-of-unread-articles group)))
                      (push (cons group unread) articles))
                  (error nil))))))))
    articles))

(defun tessera-gnus-notify--begin-update ()
  "Record unread articles before Gnus checks for new news."
  (setq tessera-gnus-notify--snapshot
        (tessera-gnus-notify--unread-articles)))

(defun tessera-gnus-notify--new-articles ()
  "Return articles added since the current update began."
  (let (articles)
    (dolist (entry (tessera-gnus-notify--unread-articles))
      (let* ((group (car entry))
             (snapshot
              (assoc-string group
                            tessera-gnus-notify--snapshot)))
        (when snapshot
          (dolist (article (cdr entry))
            (unless (memq article (cdr snapshot))
              (push (cons group article) articles))))))
    (nreverse articles)))

(defun tessera-gnus-notify--group-visible-p (group)
  "Return non-nil when a Summary for GROUP is visible."
  (catch 'visible
    (dolist (frame (frame-list))
      (when (eq (frame-visible-p frame) t)
        (dolist (window (window-list frame 'nomini))
          (with-current-buffer (window-buffer window)
            (when (and (derived-mode-p 'gnus-summary-mode)
                       (equal gnus-newsgroup-name group))
              (throw 'visible t))))))))

(defun tessera-gnus-notify--article (group article)
  "Return notification data for ARTICLE in GROUP.

Return nil when the article header cannot be retrieved."
  (condition-case nil
      (when (buffer-live-p nntp-server-buffer)
        (with-current-buffer nntp-server-buffer
          (when (gnus-request-head article group)
            (article-decode-encoded-words)
            (let* ((from (or (mail-fetch-field "From") ""))
                   (address-parts
                    (mail-extract-address-components from))
                   (author (car address-parts))
                   (address (cadr address-parts)))
              (list :group group
                    :article article
                    :author (or author address group)
                    :address address
                    :subject
                    (or (mail-fetch-field "Subject")
                        "(no subject)"))))))
    (error nil)))

(defun tessera-gnus-notify--read (group article)
  "Open ARTICLE in GROUP using the native Gnus view."
  (gnus-fetch-group group (list article))
  (select-frame-set-input-focus (selected-frame)))

(defun tessera-gnus-notify--mark-read (group article)
  "Mark ARTICLE in GROUP as read using native Gnus state."
  (gnus-update-read-articles group
                             (delq article (gnus-list-of-unread-articles group)))
  (gnus-group-update-group group))

(defun tessera-gnus-notify--gnus-icon ()
  "Return the standard Gnus application icon."
  (image-search-load-path "gnus/gnus.png"))

(defun tessera-gnus-notify--icon (address)
  "Return an avatar for ADDRESS or the standard Gnus icon."
  (when tessera-notify-use-icons
    (or (tessera-notify-avatar address)
        (tessera-gnus-notify--gnus-icon))))

(defun tessera-gnus-notify--item (article)
  "Return an individual notification for ARTICLE data."
  (let ((group (plist-get article :group))
        (number (plist-get article :article)))
    (list :title (plist-get article :author)
          :message (plist-get article :subject)
          :category 'gnus
          :icon
          (tessera-gnus-notify--icon (plist-get article :address))
          :actions
          (list (list "read" "Read"
                      (apply-partially #'tessera-gnus-notify--read
                                       group number))
                (list "mark-read" "Mark as Read"
                      (apply-partially #'tessera-gnus-notify--mark-read
                                       group number))))))

(defun tessera-gnus-notify--summary (articles)
  "Return a summary notification for ARTICLES."
  (let ((count (length articles))
        (subjects
         (mapcar (lambda (article) (plist-get article :subject))
                 (seq-take articles 3))))
    (list :title "Gnus"
          :message
          (format "%d new article%s: %s"
                  count
                  (if (= count 1) "" "s")
                  (string-join subjects "; "))
          :category 'gnus
          :icon (tessera-gnus-notify--gnus-icon))))

(defun tessera-gnus-notify--error (message)
  "Notify the user of a Gnus update failure described by MESSAGE."
  (when (tessera-notify-enabled-p
         tessera-gnus-notify-enable)
    (tessera-notify (list :title "Gnus update failed"
                          :message message
                          :severity 'high
                          :category 'gnus
                          :icon (tessera-gnus-notify--gnus-icon)))))

(defun tessera-gnus-notify--notify-new-articles ()
  "Notify the user about articles found by the Gnus update."
  (let ((new-articles (tessera-gnus-notify--new-articles))
        (failed 0)
        articles)
    (setq tessera-gnus-notify--snapshot nil)
    (when (tessera-notify-enabled-p
           tessera-gnus-notify-enable)
      (dolist (entry new-articles)
        (let ((group (car entry)))
          (unless (tessera-gnus-notify--group-visible-p group)
            (let ((article
                   (tessera-gnus-notify--article group (cdr entry))))
              (if article
                  (push article articles)
                (cl-incf failed))))))
      (setq articles (nreverse articles))
      (when articles
        (if (<= (length articles) tessera-gnus-notify-limit)
            (dolist (article articles)
              (tessera-notify (tessera-gnus-notify--item article)))
          (tessera-notify (tessera-gnus-notify--summary articles))))
      (when (> failed 0)
        (tessera-gnus-notify--error (format "Could not retrieve %d new article header%s."
                                            failed (if (= failed 1) "" "s")))))))

(defun tessera-gnus-notify--finish-update ()
  "Finish the current Gnus notification update cycle."
  (condition-case error-data
      (tessera-gnus-notify--notify-new-articles)
    (error
     (message "Tessera Gnus notification error: %s"
              (error-message-string error-data))))
  (setq tessera-gnus-notify--snapshot nil))

(defun tessera-gnus-notify--around-update (function &rest args)
  "Call Gnus update FUNCTION with ARGS and report failures."
  (let ((outermost (zerop tessera-gnus-notify--update-depth)))
    (cl-incf tessera-gnus-notify--update-depth)
    (unwind-protect
        (condition-case error-data
            (apply function args)
          (error
           (when outermost
             (setq tessera-gnus-notify--snapshot nil)
             (condition-case nil
                 (tessera-gnus-notify--error (error-message-string error-data))
               (error nil)))
           (signal (car error-data) (cdr error-data))))
      (cl-decf tessera-gnus-notify--update-depth))))

(defun tessera-gnus-notify-install ()
  "Install Tessera notification support for Gnus updates."
  (unless tessera-gnus-notify--installed-p
    (setq tessera-gnus-notify--installed-p t)
    (add-hook 'gnus-get-new-news-hook #'tessera-gnus-notify--begin-update)
    (add-hook 'gnus-after-getting-new-news-hook #'tessera-gnus-notify--finish-update)
    (dolist (function tessera-gnus-notify--update-functions)
      (advice-add function :around #'tessera-gnus-notify--around-update))))

(defun tessera-gnus-notify-uninstall ()
  "Remove Tessera notification support from Gnus updates."
  (when tessera-gnus-notify--installed-p
    (setq tessera-gnus-notify--installed-p nil
          tessera-gnus-notify--snapshot nil
          tessera-gnus-notify--update-depth 0)
    (remove-hook 'gnus-get-new-news-hook #'tessera-gnus-notify--begin-update)
    (remove-hook 'gnus-after-getting-new-news-hook #'tessera-gnus-notify--finish-update)
    (dolist (function tessera-gnus-notify--update-functions)
      (advice-remove function #'tessera-gnus-notify--around-update))))

(provide 'tessera-gnus-notify)
;;; tessera-gnus-notify.el ends here
