;;; elfeed-x-tests.el --- Elfeed X runtime tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bingshan Chang <chang@bingshan.org>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Test scheduling, native navigation, and optional widget fitting.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'elfeed-x)
(require 'elfeed-x-search)
(require 'elfeed-x-webkit)

(defvar elfeed-x-tests--initial-state
  (list (featurep 'elfeed) (featurep 'elfeed-webkit)
        (featurep 'tessera) elfeed-x-auto-update-mode
        elfeed-x-search-follow-mode elfeed-x-webkit-mode
        (elfeed-x-update-timer)
        (advice-member-p #'elfeed-x--start-update-timer 'elfeed)
        (memq #'elfeed-x-search--enable
              (bound-and-true-p elfeed-search-mode-hook))
        (memq #'elfeed-x-webkit--fit
              (bound-and-true-p elfeed-show-update-hook)))
  "State after loading the package, before loading native clients.")

(require 'elfeed)

(ert-deftest elfeed-x-load-is-inactive-and-independent ()
  (should-not (seq-some #'identity elfeed-x-tests--initial-state)))

(defmacro elfeed-x-tests--with-schedule (&rest body)
  "Run BODY with an isolated schedule and an existing Search buffer."
  (declare (indent 0) (debug t))
  `(let ((elfeed-x--update-timer nil)
         (elfeed-x-auto-update-mode nil)
         (elfeed-x-update-interval 900))
     (with-temp-buffer
       (setq major-mode 'elfeed-search-mode)
       (unwind-protect
           (progn ,@body)
         (elfeed-x-auto-update-mode -1)))))

(ert-deftest elfeed-x-schedule-waits-for-entry ()
  (let ((elfeed-x--update-timer nil)
        (elfeed-x-auto-update-mode nil)
        (elfeed-entry-point #'ignore))
    (unwind-protect
        (progn
          (elfeed-x-auto-update-mode 1)
          (should-not (elfeed-x-update-timer))
          (elfeed)
          (should (timerp (elfeed-x-update-timer))))
      (elfeed-x-auto-update-mode -1))))

(ert-deftest elfeed-x-schedule-preserves-timer-and-cleans-up ()
  (elfeed-x-tests--with-schedule
    (let ((other (run-at-time 3600 nil #'ignore))
          (before (current-time)))
      (unwind-protect
          (progn
            (elfeed-x-auto-update-mode 1)
            (let ((timer (elfeed-x-update-timer))
                  (next (elfeed-x-next-update-time)))
              (should (time-less-p before next))
              (should (< (float-time (time-subtract next before))
                         901))
              (elfeed-x-auto-update-mode 1)
              (elfeed-x--start-update-timer)
              (should (eq timer (elfeed-x-update-timer)))
              (should (equal next (elfeed-x-next-update-time)))
              (elfeed-x-auto-update-mode -1)
              (should-not (memq timer timer-list))
              (should-not (elfeed-x-next-update-time))
              (should (memq other timer-list))
              (should-not
               (advice-member-p #'elfeed-x--start-update-timer
                                'elfeed))))
        (cancel-timer other)))))

(ert-deftest elfeed-x-schedule-recovers-cancelled-timer ()
  (elfeed-x-tests--with-schedule
    (elfeed-x-auto-update-mode 1)
    (let ((timer (elfeed-x-update-timer)))
      (cancel-timer timer)
      (should-not (elfeed-x-update-timer))
      (should-not (elfeed-x-next-update-time))
      (elfeed-x--start-update-timer)
      (should (elfeed-x-update-timer))
      (should-not (eq timer (elfeed-x-update-timer))))))

(ert-deftest elfeed-x-interval-restarts-only-an-active-schedule ()
  (elfeed-x-tests--with-schedule
    (setopt elfeed-x-update-interval 600)
    (should-not (elfeed-x-update-timer))
    (elfeed-x-auto-update-mode 1)
    (let ((timer (elfeed-x-update-timer)))
      (setopt elfeed-x-update-interval 300)
      (should-not (memq timer timer-list))
      (should (= 300 (timer--repeat-delay
                      (elfeed-x-update-timer))))
      (should-error
       (elfeed-x--set-update-interval 'elfeed-x-update-interval 0))
      (should (= elfeed-x-update-interval 300)))))

(ert-deftest elfeed-x-update-skips-busy-and-reentrant-dispatch ()
  (elfeed-x-tests--with-schedule
    (let ((queue 1)
          (calls 0))
      (cl-letf (((symbol-function 'elfeed-queue-count-total)
                 (lambda () queue))
                ((symbol-function 'elfeed-update)
                 (lambda ()
                   (cl-incf calls)
                   (elfeed-x--update))))
        (elfeed-x-auto-update-mode 1)
        (elfeed-x--update)
        (should (zerop calls))
        (setq queue 0)
        (elfeed-x--update)
        (should (= calls 1))
        (should-not elfeed-x--updating)
        (elfeed-x-auto-update-mode -1)
        (elfeed-x--update)
        (should (= calls 1))))))

(ert-deftest elfeed-x-update-error-retains-schedule ()
  (elfeed-x-tests--with-schedule
    (cl-letf (((symbol-function 'elfeed-queue-count-total)
               (lambda () 0))
              ((symbol-function 'elfeed-update)
               (lambda () (error "Fetch failed"))))
      (elfeed-x-auto-update-mode 1)
      (should-error (elfeed-x--update))
      (should-not elfeed-x--updating)
      (should (elfeed-x-next-update-time)))))

(defmacro elfeed-x-tests--with-reading (&rest body)
  "Run BODY with native entries in Search and article windows."
  (declare (indent 0) (debug t))
  `(save-window-excursion
     (let* ((search (generate-new-buffer " *elfeed-x-search*"))
            (article (generate-new-buffer " *elfeed-x-article*"))
            (entry (elfeed-entry--create
                    :id '("feed" . "entry")
                    :title "Elfeed X test"
                    :date 1755475200.0
                    :feed-id "feed"
                    :tags '(unread)))
            (elfeed-db '(:version 4))
            (elfeed-db-feeds (make-hash-table :test #'equal))
            (elfeed-show-mode-hook nil)
            (elfeed-show-update-hook nil)
            (elfeed-untag-hook nil)
            (elfeed-show-refresh-function #'ignore)
            this-command
            article-window)
       (unwind-protect
           (progn
             (delete-other-windows)
             (switch-to-buffer search)
             (setq major-mode 'elfeed-search-mode)
             (use-local-map elfeed-search-mode-map)
             (setq-local elfeed-search-entries (list entry))
             (elfeed-search--print-entry entry)
             (insert "\n")
             (setq this-command (key-binding (kbd "n")))
             (goto-char (point-min))
             (setq article-window (split-window-below))
             (set-window-buffer article-window article)
             (with-current-buffer article (elfeed-show-mode))
             (cl-letf
                 (((symbol-function 'elfeed-show--buffer-name)
                   (lambda (_) (buffer-name article))))
               ,@body))
         (elfeed-x-search-follow-mode -1)
         (kill-buffer search)
         (kill-buffer article)))))

(ert-deftest elfeed-x-follow-retains-search-focus-and-point ()
  (elfeed-x-tests--with-reading
    (let ((window (selected-window))
          (position (point))
          (elfeed-search-remain-on-entry nil)
          (elfeed-show-entry-switch #'switch-to-buffer))
      (elfeed-x-search-follow-mode 1)
      (goto-char (point-max))
      (let ((this-command (key-binding (kbd "p"))))
        (call-interactively this-command)
        (run-hooks 'post-command-hook))
      (should (eq (selected-window) window))
      (should (eq (current-buffer) search))
      (should (= (point) position))
      (should (= (length (window-list)) 2))
      (should-not (elfeed-tagged-p 'unread entry))
      (should (eq (buffer-local-value 'elfeed-show-entry article)
                  entry))
      (should (eq elfeed-show-entry-switch #'switch-to-buffer)))))

(ert-deftest elfeed-x-follow-ignores-other-commands-and-empty-rows ()
  (elfeed-x-tests--with-reading
    (let ((this-command 'isearch-forward)
          (last-command-event ?n))
      (elfeed-x-search--follow)
      (should (elfeed-tagged-p 'unread entry)))
    (goto-char (point-max))
    (elfeed-x-search--follow)
    (should-not (buffer-local-value 'elfeed-show-entry article))))

(ert-deftest elfeed-x-follow-requires-an-available-article-window ()
  (elfeed-x-tests--with-reading
    (set-window-dedicated-p article-window t)
    (elfeed-x-search--follow)
    (should (elfeed-tagged-p 'unread entry))
    (set-window-dedicated-p article-window nil)
    (delete-window article-window)
    (elfeed-x-search--follow)
    (should (= (length (window-list)) 1))
    (should (elfeed-tagged-p 'unread entry))))

(ert-deftest elfeed-x-follow-does-not-render-the-same-entry-again ()
  (elfeed-x-tests--with-reading
    (let* ((renders 0)
           (elfeed-show-refresh-function
            (lambda () (cl-incf renders))))
      (elfeed-x-search--follow)
      (elfeed-x-search--follow)
      (should (= renders 1)))))

(ert-deftest elfeed-x-follow-supports-unique-article-buffers ()
  (let ((native-name (symbol-function 'elfeed-show--buffer-name))
        (elfeed-show-unique-buffers t)
        created)
    (unwind-protect
        (elfeed-x-tests--with-reading
          (cl-letf (((symbol-function 'elfeed-show--buffer-name)
                     native-name))
            (elfeed-x-search--follow)
            (setq created (window-buffer article-window))
            (should-not (eq created article))
            (should (eq entry (buffer-local-value
                               'elfeed-show-entry created)))
            (should (eq (current-buffer) search))))
      (when (buffer-live-p created) (kill-buffer created)))))

(ert-deftest elfeed-x-follow-cleans-existing-and-future-buffers ()
  (elfeed-x-tests--with-reading
    (elfeed-x-search-follow-mode 1)
    (elfeed-x-search-follow-mode 1)
    (should (= 1 (cl-count #'elfeed-x-search--follow
                           post-command-hook)))
    (with-temp-buffer
      (setq major-mode 'elfeed-search-mode)
      (run-hooks 'elfeed-search-mode-hook)
      (should (memq #'elfeed-x-search--follow post-command-hook))
      (elfeed-x-search-follow-mode -1)
      (should-not (memq #'elfeed-x-search--follow post-command-hook)))
    (should-not (memq #'elfeed-x-search--follow post-command-hook))
    (should-not (memq #'elfeed-x-search--enable
                      elfeed-search-mode-hook))))

(ert-deftest elfeed-x-webkit-fits-article-without-selecting-window ()
  (elfeed-x-tests--with-reading
    (let ((features (cons 'xwidget-internal features))
          (selected (selected-window))
          fitted)
      (cl-letf (((symbol-function 'xwidget-at)
                 (lambda (_) 'widget))
                ((symbol-function
                  'xwidget-webkit-adjust-size-to-window)
                 (lambda (widget window)
                   (setq fitted (cons widget window)))))
        (unwind-protect
            (progn
              (elfeed-x-webkit-mode 1)
              (should (equal fitted (cons 'widget article-window)))
              (setq fitted nil)
              (with-current-buffer article (elfeed-show-refresh))
              (should (eq (cdr fitted) article-window))
              (should (eq selected (selected-window)))
              (elfeed-x-webkit-mode -1)
              (setq fitted nil)
              (with-current-buffer article (elfeed-show-refresh))
              (should-not fitted))
          (elfeed-x-webkit-mode -1))))))

(ert-deftest elfeed-x-webkit-skips-hidden-and-non-webkit-articles ()
  (elfeed-x-tests--with-reading
    (let ((features (cons 'xwidget-internal features)))
      (cl-letf (((symbol-function 'xwidget-at) #'ignore)
                ((symbol-function
                  'xwidget-webkit-adjust-size-to-window)
                 (lambda (&rest _) (ert-fail "Unexpected resize"))))
        (with-current-buffer article (elfeed-x-webkit--fit))
        (delete-window article-window)
        (cl-letf (((symbol-function 'xwidget-at)
                   (lambda (_) (ert-fail "Hidden article"))))
          (with-current-buffer article (elfeed-x-webkit--fit)))))))

(ert-deftest elfeed-x-webkit-without-xwidgets-is-inert ()
  (elfeed-x-tests--with-reading
    (let ((native-featurep (symbol-function 'featurep)))
      (cl-letf (((symbol-function 'featurep)
                 (lambda (feature &optional subfeature)
                   (unless (eq feature 'xwidget-internal)
                     (funcall native-featurep feature subfeature))))
                ((symbol-function 'xwidget-at)
                 (lambda (_) (ert-fail "Xwidgets unavailable"))))
        (unwind-protect
            (progn
              (elfeed-x-webkit-mode 1)
              (with-current-buffer article (elfeed-show-refresh)))
          (elfeed-x-webkit-mode -1))))))

(ert-deftest elfeed-x-webkit-does-not-require-the-optional-renderer ()
  (elfeed-x-tests--with-reading
    (let ((renderer elfeed-show-refresh-function))
      (cl-letf (((symbol-function 'xwidget-at) nil))
        (unwind-protect
            (progn
              (elfeed-x-webkit-mode 1)
              (with-current-buffer article (elfeed-show-refresh))
              (should (eq renderer elfeed-show-refresh-function))
              (should-not (featurep 'elfeed-webkit)))
          (elfeed-x-webkit-mode -1))))))

(provide 'elfeed-x-tests)
;;; elfeed-x-tests.el ends here
