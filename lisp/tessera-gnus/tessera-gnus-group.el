;;; tessera-gnus-group.el --- Tessera interface for Gnus Group  -*- lexical-binding: t; -*-

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

;; Tessera header-line presentation for `gnus-group-mode'.

;;; Code:

(require 'gnus-group)
(require 'gnus-int)
(require 'mail-utils)
(require 'nnheader)
(require 'tessera-gnus)
(require 'tessera-gnus-status)
(require 'tessera-ui)

(defvar gnus-tmp-group)
(defvar gnus-tmp-number-of-unread)
(defvar gnus-tmp-number-total)
(defvar gnus-tmp-qualified-group)
(defvar gnus-tmp-subscribed)
(defvar gnus-group-mark-positions)
(defvar nntp-server-buffer)
(defvar gnus-topic-mode)

(defvar tessera-gnus-group--fetch-buffer nil
  "Group buffer receiving native fetch progress updates.")

(defvar tessera-gnus-group--update-active-p nil
  "Non-nil while a tracked Group update command is active.")

(defface tessera-gnus-group-mark
  '((t :inherit shadow))
  "Face for neutral Gnus Group marks."
  :group 'tessera-gnus)

(defface tessera-gnus-group-mark-workflow
  '((t :inherit font-lock-constant-face))
  "Face for Gnus Group operation marks."
  :group 'tessera-gnus)

(defface tessera-gnus-group-mark-subscribed
  '((t :inherit success))
  "Face for subscribed Gnus groups."
  :group 'tessera-gnus)

(defface tessera-gnus-group-mark-warning
  '((t :inherit warning))
  "Face for Gnus Group states that warrant caution."
  :group 'tessera-gnus)

(defface tessera-gnus-group-mark-error
  '((t :inherit error))
  "Face for unavailable Gnus groups."
  :group 'tessera-gnus)

(defface tessera-gnus-group-node-count
  '((t :inherit shadow))
  "Face for Gnus Group node counts."
  :group 'tessera-gnus)

(defface tessera-gnus-group-node-unread
  '((t :inherit (error tessera-gnus-group-node-count)))
  "Face for nonzero unread counts in Gnus Group nodes."
  :group 'tessera-gnus)

(defface tessera-gnus-group-node-total
  '((t :inherit tessera-gnus-group-node-count))
  "Face for total counts in Gnus Group nodes."
  :group 'tessera-gnus)

(defface tessera-gnus-group-node-count-separator
  '((t :inherit tessera-gnus-group-node-count))
  "Face for separators between Gnus Group node counts."
  :group 'tessera-gnus)

(defface tessera-gnus-group-node-name
  '((t :weight bold))
  "Face appended to the native Gnus Group name face."
  :group 'tessera-gnus)

(defface tessera-gnus-group-node-timestamp
  '((t :inherit shadow))
  "Face for latest timestamps in Gnus Group nodes."
  :group 'tessera-gnus)

(defface tessera-gnus-group-heading
  '((t :inherit default :weight bold))
  "Face for the main heading in a Gnus Group buffer."
  :group 'tessera-gnus)

(defface tessera-gnus-group-empty
  '((t :inherit shadow))
  "Face for the empty Gnus Group presentation."
  :group 'tessera-gnus)

(defun tessera-gnus-group--set-option (symbol value)
  "Set Group option SYMBOL to VALUE and refresh Group buffers."
  (set-default symbol value)
  (when (fboundp 'tessera-gnus-group-refresh)
    (dolist (buffer
             (match-buffers '(derived-mode . gnus-group-mode)))
      (with-current-buffer buffer
        (tessera-gnus-group-refresh)))))

(defun tessera-gnus-group--set-count-digits (symbol value)
  "Set count width option SYMBOL to positive integer VALUE."
  (unless (and (integerp value) (> value 0))
    (error "Group count width must be a positive integer"))
  (tessera-gnus-group--set-option symbol value))

(defun tessera-gnus-group--set-latest-time-limit (symbol value)
  "Set latest date limit SYMBOL to positive integer VALUE."
  (unless (and (integerp value) (> value 0))
    (error "Latest date limit must be a positive integer"))
  (set-default symbol value)
  (when (fboundp 'tessera-gnus-group-refresh)
    (dolist (buffer
             (match-buffers '(derived-mode . gnus-group-mode)))
      (with-current-buffer buffer
        (when (and
               (boundp 'tessera-gnus-group--latest-times)
               (hash-table-p (symbol-value 'tessera-gnus-group--latest-times)))
          (clrhash (symbol-value 'tessera-gnus-group--latest-times)))
        (when (boundp 'tessera-gnus-group--latest-time-queue)
          (set 'tessera-gnus-group--latest-time-queue nil))
        (tessera-gnus-group-refresh)))))

(defcustom tessera-gnus-group-show-heading t
  "Whether to show the main heading in Gnus Group buffers."
  :type 'boolean
  :set #'tessera-gnus-group--set-option
  :group 'tessera-gnus)

(defcustom tessera-gnus-group-count-digits 3
  "Maximum number of exact digits shown in Group node counts.

Larger counts are displayed as the largest value plus a `+'."
  :type 'integer
  :set #'tessera-gnus-group--set-count-digits
  :group 'tessera-gnus)

(defcustom tessera-gnus-group-latest-time-limit 500
  "Maximum overview records inspected for a Group latest date."
  :type 'integer
  :set #'tessera-gnus-group--set-latest-time-limit
  :group 'tessera-gnus)

(defcustom tessera-gnus-group-mark-symbol-alist nil
  "Overrides for individual Gnus Group mark glyphs.

Keys are `process', `subscribed', `unsubscribed', `zombie', and
`killed'.  A value is either a literal string or a Nerd Icons
specification of the form \(FAMILY . ICON-NAME)."
  :type '(alist
          :key-type
          (choice
           (const process)
           (const subscribed)
           (const unsubscribed)
           (const zombie)
           (const killed))
          :value-type
          (choice
           (string :tag "Literal glyph")
           (cons :tag "Nerd icon"
                 (symbol :tag "Family")
                 (string :tag "Icon name"))))
  :set #'tessera-gnus-group--set-option
  :group 'tessera-gnus)

(defconst tessera-gnus-group--line-format
  (concat " %p"
          "%u&tessera-gnus-group-node-prefix;"
          "%u&tessera-gnus-group-mark;"
          "%(%u&tessera-gnus-group-name;%)"
          "%u&tessera-gnus-group-node-suffix;\n")
  "Gnus Group line format installed by Tessera.")

(defconst tessera-gnus-group--disclosure-glyph "▸"
  "Disclosure glyph used by Group and Topic headings.")

(defconst tessera-gnus-group--node-top-padding 4
  "Group node padding above its content, in pixels.")

(defconst tessera-gnus-group--node-bottom-padding 4
  "Group node padding below its content, in pixels.")

(defconst tessera-gnus-group--heading-top-padding 6
  "Group heading padding above its content, in pixels.")

(defconst tessera-gnus-group--heading-bottom-padding 6
  "Group heading padding below its content, in pixels.")

(defconst tessera-gnus-group--main-top-padding 2
  "Group interface padding above its main content, in pixels.")

(defconst tessera-gnus-group--main-bottom-padding 2
  "Group interface padding below its main content, in pixels.")

(defconst tessera-gnus-group--nodes-top-padding 2
  "Group node-list padding above its content, in pixels.")

(defconst tessera-gnus-group--nodes-bottom-padding 2
  "Group node-list padding below its content, in pixels.")

(defconst tessera-gnus-group--latest-time-timeout 1
  "Maximum seconds spent requesting one latest article date.")

(defconst tessera-gnus-group--latest-time-retry-delay 60
  "Seconds before retrying a failed latest article date request.")

(defconst tessera-gnus-group--unicode-mark-symbols
  '((process . "◆")
    (subscribed . "●")
    (unsubscribed . "○")
    (zombie . "↻")
    (killed . "⊗"))
  "Unicode symbols for Gnus Group marks.")

(defconst tessera-gnus-group--nerd-mark-icons
  '((process mdicon . "nf-md-playlist_edit")
    (subscribed mdicon . "nf-md-check_circle_outline")
    (unsubscribed mdicon . "nf-md-bookmark_minus_outline")
    (zombie mdicon . "nf-md-backup_restore")
    (killed mdicon . "nf-md-eye_off_outline"))
  "Nerd Icons specifications for Gnus Group marks.")

(defconst tessera-gnus-group--mark-faces
  '((process . tessera-gnus-group-mark-workflow)
    (subscribed . tessera-gnus-group-mark-subscribed)
    (unsubscribed . tessera-gnus-group-mark)
    (zombie . tessera-gnus-group-mark-warning)
    (killed . tessera-gnus-group-mark-error))
  "Faces used for semantic Gnus Group mark colors.")

(defvar tessera-gnus-group--enabled-p nil
  "Non-nil when the Tessera Gnus Group interface is enabled.")

(defvar-local tessera-gnus-group--installed-p nil
  "Non-nil when Tessera is installed in this Group buffer.")

(defvar-local tessera-gnus-group--installed-line-format nil
  "Exact group line format installed by Tessera.")

(defvar-local tessera-gnus-group--original-line-format nil
  "Group line format replaced by Tessera in this buffer.")

(defvar-local tessera-gnus-group--original-line-format-local-p nil
  "Non-nil when the original group line format was buffer-local.")

(defvar-local tessera-gnus-group--status-state 'success
  "Current native update state presented in the Group header.")

(defvar-local tessera-gnus-group--fetch-current nil
  "Number of groups processed by the current native update.")

(defvar-local tessera-gnus-group--fetch-total nil
  "Number of groups selected by the current native update.")

(defvar-local tessera-gnus-group--fetch-redraw-step 1
  "Number of processed groups between header redraws.")

(defvar-local tessera-gnus-group--fetch-next-redraw 1
  "Next processed group count that redraws the header.")

(defvar-local tessera-gnus-group--statistics-cache nil
  "Cached presentation of statistics for all groups.")

(defvar-local tessera-gnus-group--heading-overlay nil
  "Overlay displaying the layout before Group nodes.")

(defvar-local tessera-gnus-group--bottom-overlay nil
  "Overlay displaying the layout after Group nodes.")

(defvar-local tessera-gnus-group--window-overlays nil
  "Window-local overlays presenting Group nodes.")

(defvar-local tessera-gnus-group--latest-times nil
  "Cache of native latest article times by group and article.")

(defvar-local tessera-gnus-group--latest-time-queue nil
  "Groups awaiting retrieval of their latest article time.")

(defvar-local tessera-gnus-group--latest-time-timer nil
  "Idle timer retrieving native latest article times.")

(defvar-local tessera-gnus-group--relative-time-timer nil
  "Timer refreshing visible relative article times.")

(defvar-keymap tessera-gnus-group--layout-map
  :doc "Prevent structural Group layout from opening a group."
  "<mouse-1>" #'ignore
  "<mouse-2>" #'ignore
  "<mouse-3>" #'ignore)

(defun tessera-gnus-group--subscription-fact (mark)
  "Return the subscription fact represented by native MARK."
  (pcase mark
    (?U 'unsubscribed)
    (?Z 'zombie)
    (?K 'killed)
    (_ 'subscribed)))

(defun tessera-gnus-group--mark-fact (process subscription)
  "Return one native mark fact for PROCESS and SUBSCRIPTION."
  (if (eq process ?\s)
      (cons subscription
            (pcase subscription
              ('unsubscribed ?U)
              ('zombie ?Z)
              ('killed ?K)
              (_ ?\s)))
    (cons 'process process)))

(defun tessera-gnus-group--mark-glyph (fact native)
  "Return the displayed glyph for FACT with NATIVE fallback."
  (let* ((fallback
          (or (alist-get fact tessera-gnus-group--unicode-mark-symbols)
              (char-to-string native)))
         (value
          (or (alist-get fact
                         tessera-gnus-group-mark-symbol-alist)
              (pcase tessera-gnus-symbol-style
                ('ascii
                 (if (eq fact 'subscribed)
                     "S"
                   (char-to-string native)))
                ('unicode fallback)
                ('nerd-icons
                 (alist-get fact tessera-gnus-group--nerd-mark-icons))))))
    (tessera-gnus--render-glyph value fallback)))

(defun tessera-gnus-group--mark-token (fact native)
  "Return a marked Group token for FACT represented by NATIVE."
  (let* ((text (tessera-gnus-group--mark-glyph fact native))
         (face
          (cond
           ((stringp tessera-gnus-glyph-color-style)
            (list :foreground
                  tessera-gnus-glyph-color-style))
           ((null tessera-gnus-glyph-color-style)
            'tessera-gnus-group-mark)
           (t
            (or (alist-get fact tessera-gnus-group--mark-faces)
                'tessera-gnus-group-mark)))))
    (remove-text-properties 0 (length text) '(font-lock-face nil) text)
    (add-face-text-property 0 (length text) face nil text)
    (add-text-properties 0 (length text)
                         (list 'gnus-face t
                               'help-echo (symbol-name fact)
                               'tessera-ui--element 'node.mark
                               'tessera-gnus-group-mark fact)
                         text)
    text))

(defun tessera-gnus-group--mark-display (fact)
  "Return the visual mark field for native FACT."
  (when fact
    (concat (tessera-gnus-group--mark-token
             (car fact) (cdr fact))
            (propertize " " 'tessera-ui--element 'node.separator))))

(defun tessera-gnus-group--element (text element &optional face)
  "Return a copy of TEXT named ELEMENT and optionally using FACE."
  (let ((text (copy-sequence text)))
    (put-text-property 0 (length text) 'tessera-ui--element element text)
    (when face
      (add-face-text-property 0 (length text) face nil text))
    text))

(defun tessera-gnus-group--count-field (text element unreadp)
  "Return count TEXT named ELEMENT.

When UNREADP is non-nil, highlight a positive numeric count."
  (let ((text (copy-sequence text)))
    (add-text-properties 0 (length text)
                         (list 'face
                               (if (and unreadp
                                        (string-match-p "\\`[1-9][0-9]*\\+?\\'" text))
                                   'tessera-gnus-group-node-unread
                                 (if unreadp
                                     'tessera-gnus-group-node-count
                                   'tessera-gnus-group-node-total))
                               'tessera-ui--element element)
                         text)
    text))

(defun tessera-gnus-group--display-count (text)
  "Return a bounded presentation of count TEXT."
  (let ((limit
         (1- (expt 10 tessera-gnus-group-count-digits))))
    (if (and (string-match-p "\\`[0-9]+\\'" text)
             (> (string-to-number text) limit))
        (format "%d+" limit)
      text)))

(defun tessera-gnus-group--count-pair (unread total)
  "Return right-aligned UNREAD and TOTAL counts."
  (let* ((separator
          (tessera-gnus-group--element "/" 'node.special-separator 'tessera-gnus-group-node-count-separator))
         (counts (concat unread separator total))
         (rail
          (concat (make-string tessera-gnus-group-count-digits ?9)
                  "+/"
                  (make-string tessera-gnus-group-count-digits ?9)
                  "+"))
         (gap-width
          (max 0
               (- (string-pixel-width rail)
                  (string-pixel-width counts))))
         (gap
          (propertize " "
                      'display `(space :width (,gap-width))
                      'tessera-ui--element 'node.leading-gap)))
    (concat gap counts)))

(defun gnus-user-format-function-tessera-gnus-group-node-prefix (_header)
  "Return layout fields before the mark on a Gnus Group node."
  (let ((unread
         (tessera-gnus-group--count-field (tessera-gnus-group--display-count gnus-tmp-number-of-unread)
                                          'node.unread-count t))
        (total
         (tessera-gnus-group--count-field (tessera-gnus-group--display-count
                                           (number-to-string gnus-tmp-number-total))
                                          'node.total-count nil)))
    (concat
     (tessera-gnus-group--element (tessera-ui-vertical-padding 'default 'node.top-padding tessera-gnus-group--node-top-padding 0)
                                  'node.top-padding)
     (tessera-gnus-group--element (tessera-ui-entry-leading-safety-gap) 'node.safety-gap)
     (tessera-gnus-group--element (tessera-ui-entry-padding 'entry.left-padding)
                                  'node.left-padding)
     (tessera-gnus-group--count-pair unread total)
     (tessera-gnus-group--element " " 'node.leading-gap)
     (tessera-gnus-group--element " " 'node.separator))))

(defun gnus-user-format-function-tessera-gnus-group-name (_header)
  "Return the native name of the current Gnus Group node."
  (tessera-gnus-group--element gnus-tmp-qualified-group 'node.group-name))

(defun tessera-gnus-group--relative-time (time)
  "Return a compact relative description of TIME."
  (let* ((seconds
          (max 0 (floor (float-time (time-subtract nil time)))))
         (unit
          (cond
           ((< seconds 60) nil)
           ((< seconds 3600) (cons (/ seconds 60) "minute"))
           ((< seconds 86400) (cons (/ seconds 3600) "hour"))
           ((< seconds 2592000) (cons (/ seconds 86400) "day"))
           (t (cons (/ seconds 2592000) "month")))))
    (if (not unit)
        "now"
      (format "%d %s%s ago"
              (car unit) (cdr unit)
              (if (= (car unit) 1) "" "s")))))

(defun tessera-gnus-group--latest-time (group)
  "Return the cached latest article time for GROUP."
  (when-let* ((active (gnus-active group))
              (article (cdr active))
              (entry
               (and tessera-gnus-group--latest-times
                    (gethash group
                             tessera-gnus-group--latest-times))))
    (when (eql article (car entry))
      (nth 1 entry))))

(defun tessera-gnus-group--timestamp (group)
  "Return the latest article timestamp for GROUP."
  (let* ((time (tessera-gnus-group--latest-time group))
         (text
          (if time
              (tessera-gnus-group--relative-time time)
            " ")))
    (tessera-gnus-group--element text 'node.latest-timestamp 'tessera-gnus-group-node-timestamp)))

(defun tessera-gnus-group--latest-overview-time (group article)
  "Return the greatest overview date near ARTICLE in GROUP."
  (when-let* ((active (gnus-active group))
              (first
               (max (car active)
                    (1+ (- article
                           tessera-gnus-group-latest-time-limit))))
              (articles (number-sequence first article))
              ((eq (gnus-retrieve-headers articles group) 'nov))
              ((buffer-live-p nntp-server-buffer)))
    (with-current-buffer nntp-server-buffer
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let (latest)
          (while (not (eobp))
            (when (looking-at "[0-9]+\t")
              (when-let* ((header
                           (ignore-errors (nnheader-parse-nov)))
                          (date (mail-header-date header))
                          (time (ignore-errors (date-to-time date))))
                (when (or (not latest)
                          (time-less-p latest time))
                  (setq latest time))))
            (forward-line 1))
          latest)))))

(defun tessera-gnus-group--high-article-time (group article)
  "Return the native date of ARTICLE in GROUP."
  (when (gnus-request-head article group)
    (when (buffer-live-p nntp-server-buffer)
      (with-current-buffer nntp-server-buffer
        (save-restriction
          (widen)
          (goto-char (point-min))
          (when-let* ((date (mail-fetch-field "date")))
            (date-to-time date)))))))

(defun tessera-gnus-group--request-latest-time (group article)
  "Return the latest native date near ARTICLE in GROUP."
  (condition-case nil
      (with-timeout (tessera-gnus-group--latest-time-timeout nil)
        (or (save-current-buffer
              (tessera-gnus-group--latest-overview-time group article))
            (tessera-gnus-group--high-article-time group article)))
    (error nil)))

(defun tessera-gnus-group--schedule-latest-time ()
  "Schedule retrieval of the next latest article time."
  (unless (or tessera-gnus-group--latest-time-timer
              (not tessera-gnus-group--latest-time-queue))
    (setq tessera-gnus-group--latest-time-timer
          (run-with-idle-timer 0.05 nil #'tessera-gnus-group--fetch-latest-time
                               (current-buffer)))))

(defun tessera-gnus-group--fetch-latest-time (buffer)
  "Retrieve one latest article time for Group BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq tessera-gnus-group--latest-time-timer nil)
      (when tessera-gnus-group--installed-p
        (if-let* ((item (pop tessera-gnus-group--latest-time-queue)))
            (let ((time
                   (tessera-gnus-group--request-latest-time (car item) (cdr item))))
              (puthash (car item)
                       (list (cdr item) time (and (not time) (current-time)))
                       tessera-gnus-group--latest-times)
              (gnus-group-update-group (car item) t t)
              (tessera-gnus-group--present-visible-windows)
              (if tessera-gnus-group--latest-time-queue
                  (tessera-gnus-group--schedule-latest-time)
                (force-mode-line-update)))
          (force-mode-line-update))))))

(defun tessera-gnus-group--visible-groups (&optional window)
  "Return groups visible in WINDOW or every Group window."
  (let ((windows
         (if window
             (list window)
           (get-buffer-window-list (current-buffer) nil t)))
        groups)
    (dolist (group-window windows)
      (when (and (window-live-p group-window)
                 (eq (window-buffer group-window) (current-buffer)))
        (save-excursion
          (goto-char (window-start group-window))
          (let ((end (window-end group-window t)))
            (while (< (point) end)
              (when-let* ((group
                           (get-text-property (point) 'gnus-group)))
                (push group groups))
              (forward-line 1))))))
    (delete-dups (nreverse groups))))

(defun tessera-gnus-group--latest-time-stale-p (group article)
  "Return non-nil when GROUP at ARTICLE needs a date request."
  (let ((entry
         (gethash group tessera-gnus-group--latest-times)))
    (or (not entry)
        (not (eql article (car entry)))
        (and
         (not (nth 1 entry))
         (or
          (not (nth 2 entry))
          (>= (float-time (time-subtract nil (nth 2 entry)))
              tessera-gnus-group--latest-time-retry-delay))))))

(defun tessera-gnus-group--queue-latest-time (group)
  "Queue a stale latest article time for GROUP."
  (unless (hash-table-p tessera-gnus-group--latest-times)
    (setq tessera-gnus-group--latest-times
          (make-hash-table :test #'equal)))
  (when-let* ((active (gnus-active group))
              (article (cdr active)))
    (when (and
           (tessera-gnus-group--latest-time-stale-p group article)
           (not (assoc group
                       tessera-gnus-group--latest-time-queue)))
      (setq tessera-gnus-group--latest-time-queue
            (append tessera-gnus-group--latest-time-queue
                    (list (cons group article))))))
  (tessera-gnus-group--schedule-latest-time))

(defun tessera-gnus-group--queue-latest-times (&optional window)
  "Queue stale latest article times visible in WINDOW."
  (unless (hash-table-p tessera-gnus-group--latest-times)
    (setq tessera-gnus-group--latest-times
          (make-hash-table :test #'equal)))
  (let (queue)
    (dolist (group (tessera-gnus-group--visible-groups window))
      (when-let* ((active (gnus-active group))
                  (article (cdr active)))
        (when (tessera-gnus-group--latest-time-stale-p
               group article)
          (push (cons group article) queue))))
    (setq tessera-gnus-group--latest-time-queue
          (nreverse queue)))
  (tessera-gnus-group--schedule-latest-time))

(defun tessera-gnus-group--refresh-relative-times (buffer)
  "Refresh relative dates visible in Group BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when tessera-gnus-group--installed-p
        (dolist (group (tessera-gnus-group--visible-groups))
          (when (tessera-gnus-group--latest-time group)
            (gnus-group-update-group group t t)))
        (tessera-gnus-group--present-visible-windows)))))

(defun tessera-gnus-group--cancel-latest-time ()
  "Cancel retrieval of latest article times in this buffer."
  (when (timerp tessera-gnus-group--latest-time-timer)
    (cancel-timer tessera-gnus-group--latest-time-timer))
  (when (timerp tessera-gnus-group--relative-time-timer)
    (cancel-timer tessera-gnus-group--relative-time-timer))
  (setq tessera-gnus-group--latest-time-timer nil
        tessera-gnus-group--relative-time-timer nil
        tessera-gnus-group--latest-time-queue nil))

(defun gnus-user-format-function-tessera-gnus-group-node-suffix (_header)
  "Return layout fields after the name of a Gnus Group node."
  (let* ((timestamp
          (tessera-gnus-group--timestamp gnus-tmp-group))
         (right-padding
          (tessera-gnus-group--element (tessera-ui-entry-padding 'entry.right-padding)
                                       'node.right-padding))
         (safety-gap
          (tessera-gnus-group--element (tessera-ui-entry-trailing-safety-gap)
                                       'node.safety-gap))
         (bottom-padding
          (tessera-gnus-group--element (tessera-ui-vertical-padding
                                        'default 'node.bottom-padding 0
                                        tessera-gnus-group--node-bottom-padding)
                                       'node.bottom-padding))
         (right
          (concat timestamp right-padding
                  safety-gap bottom-padding))
         (flex-gap
          (tessera-gnus-group--element (tessera-ui-entry-flex-gap right) 'node.flex-gap)))
    (concat flex-gap right)))

(defun gnus-user-format-function-tessera-gnus-group-mark (_header)
  "Return the visual anchor for native Gnus Group marks."
  (propertize " "
              'tessera-ui--element 'node.mark
              'tessera-gnus-group-mark-anchor t
              'tessera-gnus-group-subscription
              (tessera-gnus-group--subscription-fact gnus-tmp-subscribed)))

(defun tessera-gnus-group--decorate-marks ()
  "Present native marks on the current Gnus Group line."
  (save-excursion
    (beginning-of-line)
    (let ((inhibit-read-only t))
      (when-let* ((group (gnus-group-group-name))
                  (process-offset
                   (cdr (assq 'process
                              gnus-group-mark-positions))))
        (let* ((start (line-beginning-position))
               (end (line-end-position))
               (process-position (+ start process-offset))
               (anchor
                (text-property-any start end 'tessera-gnus-group-mark-anchor t)))
          (when (and anchor (< process-position end))
            (let* ((process (char-after process-position))
                   (subscription
                    (get-text-property anchor 'tessera-gnus-group-subscription))
                   (display
                    (tessera-gnus-group--mark-display (tessera-gnus-group--mark-fact process subscription))))
              (put-text-property start (1+ start) 'display "")
              (put-text-property process-position (1+ process-position) 'display "")
              (put-text-property anchor (1+ anchor) 'face 'default)
              (put-text-property anchor (1+ anchor) 'display (or display ""))
              (put-text-property anchor (1+ anchor) 'tessera-ui--context group))))))))

(defun tessera-gnus-group--decorate-node-faces ()
  "Apply Tessera faces to the current Gnus Group node."
  (let* ((start (line-beginning-position))
         (end (line-end-position))
         (unread
          (gnus-group-unread (gnus-group-group-name)))
         (unread-face
          (if (and (numberp unread) (> unread 0))
              'tessera-gnus-group-node-unread
            'tessera-gnus-group-node-count))
         (inhibit-read-only t))
    (dolist (field
             `((node.unread-count ,unread-face)
               (node.special-separator
                tessera-gnus-group-node-count-separator)
               (node.total-count
                tessera-gnus-group-node-total)
               (node.group-name
                tessera-gnus-group-node-name t)
               (node.latest-timestamp
                tessera-gnus-group-node-timestamp)))
      (when-let* ((field-start
                   (text-property-any start end 'tessera-ui--element (car field)))
                  (field-end
                   (next-single-property-change field-start 'tessera-ui--element nil end)))
        (let ((face (cadr field))
              (append (caddr field))
              (faces
               (get-text-property field-start 'face)))
          (if append
              (unless
                  (or (eq face faces)
                      (and (listp faces) (memq face faces)))
                (add-face-text-property field-start field-end face t))
            (put-text-property field-start field-end 'face face)))))))

(defun tessera-gnus-group--make-window-overlay (start end display window)
  "Display text from START to END as DISPLAY in WINDOW."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'window window)
    (overlay-put overlay 'display display)
    (overlay-put overlay 'evaporate t)
    (push overlay tessera-gnus-group--window-overlays)
    overlay))

(defun tessera-gnus-group--delete-window-overlays (&optional window)
  "Delete Group presentation overlays for WINDOW.

Delete all presentation overlays when WINDOW is nil."
  (let (remaining)
    (dolist (overlay tessera-gnus-group--window-overlays)
      (if (or (not (overlay-buffer overlay))
              (not (window-live-p (overlay-get overlay 'window)))
              (not window)
              (eq (overlay-get overlay 'window) window))
          (delete-overlay overlay)
        (push overlay remaining)))
    (setq tessera-gnus-group--window-overlays
          (nreverse remaining))))

(defun tessera-gnus-group--present-node (window)
  "Present the current Gnus Group node in WINDOW."
  (let* ((start (line-beginning-position))
         (end (line-end-position))
         (name-start
          (text-property-any start end 'tessera-ui--element 'node.group-name))
         (timestamp-start
          (text-property-any start end 'tessera-ui--element
                             'node.latest-timestamp)))
    (when (and name-start timestamp-start)
      (let* ((name-end
              (next-single-property-change name-start 'tessera-ui--element nil end))
             (name
              (buffer-substring name-start name-end))
             (prefix-width
              (string-pixel-width (buffer-substring start name-start)))
             (right-width
              (string-pixel-width (buffer-substring timestamp-start end)))
             (safety-width
              (string-pixel-width (tessera-ui-entry-leading-safety-gap)))
             (available
              (max 0
                   (- (window-body-width window t)
                      prefix-width right-width safety-width)))
             (display
              (tessera-ui-truncate-pixels name available)))
        (unless (string= name display)
          (let ((overlay
                 (tessera-gnus-group--make-window-overlay name-start name-end display window)))
            (overlay-put overlay 'help-echo
                         (substring-no-properties name))))))))

(defun tessera-gnus-group--present-nodes (&optional window)
  "Present all Gnus Group nodes for WINDOW."
  (when-let* ((window
               (or window
                   (get-buffer-window (current-buffer) t))))
    (tessera-gnus-group--delete-window-overlays window)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (get-text-property (point) 'gnus-group)
          (tessera-gnus-group--present-node window))
        (forward-line 1)))))

(defun tessera-gnus-group--present-visible-windows ()
  "Present Group nodes in every window showing this buffer."
  (dolist (window (get-buffer-window-list (current-buffer) nil t))
    (tessera-gnus-group--present-nodes window)))

(defun tessera-gnus-group--line-visible-p ()
  "Return non-nil when point is visible in a Group window."
  (let ((windows
         (get-buffer-window-list (current-buffer) nil t))
        visible)
    (while (and windows (not visible))
      (setq visible
            (pos-visible-in-window-p (point) (pop windows) t)))
    visible))

(defun tessera-gnus-group--after-line-update (&rest _args)
  "Present the current Gnus Group node after a native update."
  (tessera-gnus-group--decorate-marks)
  (tessera-gnus-group--decorate-node-faces)
  (when-let* ((group (gnus-group-group-name)))
    (when (tessera-gnus-group--line-visible-p)
      (tessera-gnus-group--queue-latest-time group))))

(defun tessera-gnus-group--window-state-change (window)
  "Present Gnus Group nodes after a state change in WINDOW."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer)))
    (tessera-gnus-group--present-nodes window)
    (tessera-gnus-group--queue-latest-times window)))

(defun tessera-gnus-group--refresh-presentation (&rest _args)
  "Regenerate the current Group presentation."
  (when (and tessera-gnus-group--installed-p
             (derived-mode-p 'gnus-group-mode))
    (tessera-gnus-group-refresh)))

(defun tessera-gnus-group--after-mark-update (&rest _args)
  "Refresh the visual mark after Gnus updates its native mark."
  (tessera-gnus-group--decorate-marks))

(defun tessera-gnus-group--format-dead-groups (function &rest args)
  "Call FUNCTION with ARGS using the configured Group line format."
  (let ((gnus-group-listing-limit
         (if tessera-gnus-group--installed-p
             most-positive-fixnum
           gnus-group-listing-limit)))
    (apply function args)))

(defun tessera-gnus-group--invalidate-statistics ()
  "Invalidate the cached all-group statistics."
  (setq tessera-gnus-group--statistics-cache nil))

(defun tessera-gnus-group--statistics ()
  "Return semantic metrics for all groups known to Gnus."
  (or tessera-gnus-group--statistics-cache
      (let ((groups (length gnus-group-list))
            (total 0)
            (total-incomplete nil)
            (unread 0)
            (unread-incomplete nil))
        (dolist (group gnus-group-list)
          (let ((active (gnus-active group))
                (group-unread (gnus-group-unread group)))
            (if (numberp group-unread)
                (setq unread (+ unread (max 0 group-unread)))
              (setq unread-incomplete t))
            (if active
                (setq total
                      (+ total (range-length (list active))))
              (setq total-incomplete t))))
        (setq tessera-gnus-group--statistics-cache
              (tessera-ui-header-line-standard-metrics unread groups 'groups "groups" total
                                                       (if unread-incomplete 'lower-bound 'exact)
                                                       'exact
                                                       (if total-incomplete 'lower-bound 'exact))))))

(defun tessera-gnus-group--format-status ()
  "Return the presentation of the current Group update status."
  (tessera-gnus-status tessera-gnus-group--status-state
                       (if tessera-gnus-group--fetch-total
                           (cons tessera-gnus-group--fetch-current
                                 tessera-gnus-group--fetch-total)
                         tessera-gnus-group--fetch-current)
                       nil
                       (if (eq tessera-gnus-group--status-state 'fail)
                           (concat "The last fetch failed; "
                                   "mouse-1: Get new articles")
                         (if (eq tessera-gnus-group--status-state 'processing)
                             "Gnus is fetching new articles"
                           "mouse-1: Get new articles"))
                       #'tessera-gnus-group--get-new-news))

(defun tessera-gnus-group--header-status-segment (buffer)
  "Derive the registered status segment from Group BUFFER."
  (with-current-buffer buffer
    (tessera-ui-header-line-status-segment (tessera-gnus-group--format-status))))

(defun tessera-gnus-group--header-statistics-segment (buffer)
  "Derive the registered statistics segment from Group BUFFER."
  (with-current-buffer buffer
    (tessera-ui-header-line-statistics-segment (tessera-gnus-group--statistics))))

(tessera-ui-header-line-register 'gnus-group
                                 :left
                                 '((status . tessera-gnus-group--header-status-segment))
                                 :right
                                 '((statistics .
                                               tessera-gnus-group--header-statistics-segment)))

(defun tessera-gnus-group--redraw-status ()
  "Redisplay the current Group status immediately."
  (force-mode-line-update)
  (sit-for 0))

(defun tessera-gnus-group--begin-fetch (&optional total)
  "Begin presenting a native update of TOTAL groups."
  (setq tessera-gnus-group--status-state 'processing
        tessera-gnus-group--fetch-current (and total 0)
        tessera-gnus-group--fetch-total total
        tessera-gnus-group--fetch-redraw-step
        (if total (max 1 (ceiling total 100)) 1)
        tessera-gnus-group--fetch-next-redraw 1)
  (tessera-gnus-group--redraw-status))

(defun tessera-gnus-group--finish-fetch ()
  "Finish presenting the current native Group update."
  (setq tessera-gnus-group--status-state 'success
        tessera-gnus-group--fetch-current nil
        tessera-gnus-group--fetch-total nil)
  (tessera-gnus-group--invalidate-statistics)
  (tessera-gnus-group--redraw-status))

(defun tessera-gnus-group--fail-fetch ()
  "Finish presenting the current native Group update as failed."
  (setq tessera-gnus-group--status-state 'fail
        tessera-gnus-group--fetch-current nil
        tessera-gnus-group--fetch-total nil)
  (tessera-gnus-group--invalidate-statistics)
  (tessera-gnus-group--redraw-status))

(defun tessera-gnus-group--track-fetch (orig-fun &optional level dont-connect one-level)
  "Call ORIG-FUN while tracking the native Group update.

LEVEL, DONT-CONNECT, and ONE-LEVEL are passed to
`gnus-get-unread-articles'."
  (let ((buffer gnus-group-buffer))
    (if (and (buffer-live-p buffer)
             (buffer-local-value 'tessera-gnus-group--installed-p buffer))
        (let ((tessera-gnus-group--fetch-buffer buffer))
          (with-current-buffer buffer
            (tessera-gnus-group--begin-fetch))
          (condition-case err
              (prog1
                  (funcall orig-fun level dont-connect one-level)
                (unless tessera-gnus-group--update-active-p
                  (with-current-buffer buffer
                    (tessera-gnus-group--finish-fetch))))
            ((error quit)
             (unless tessera-gnus-group--update-active-p
               (with-current-buffer buffer
                 (tessera-gnus-group--fail-fetch)))
             (signal (car err) (cdr err)))))
      (funcall orig-fun level dont-connect one-level))))

(defun tessera-gnus-group--record-fetch (&rest _args)
  "Record one group processed by the current native update."
  (when (buffer-live-p tessera-gnus-group--fetch-buffer)
    (with-current-buffer tessera-gnus-group--fetch-buffer
      (when (or (not tessera-gnus-group--fetch-total)
                (< (or tessera-gnus-group--fetch-current 0)
                   tessera-gnus-group--fetch-total))
        (setq tessera-gnus-group--fetch-current
              (1+ (or tessera-gnus-group--fetch-current 0)))
        (when (or
               (>= tessera-gnus-group--fetch-current
                   tessera-gnus-group--fetch-next-redraw)
               (and tessera-gnus-group--fetch-total
                    (= tessera-gnus-group--fetch-current
                       tessera-gnus-group--fetch-total)))
          (setq tessera-gnus-group--fetch-next-redraw
                (+ tessera-gnus-group--fetch-current
                   tessera-gnus-group--fetch-redraw-step))
          (tessera-gnus-group--redraw-status))))))

(defun tessera-gnus-group--run-update (function args total)
  "Call Gnus update FUNCTION with ARGS and track TOTAL groups."
  (let ((tessera-gnus-group--update-active-p t)
        (tessera-gnus-group--fetch-buffer (current-buffer)))
    (tessera-gnus-group--begin-fetch total)
    (condition-case err
        (prog1
            (apply function args)
          (tessera-gnus-group--finish-fetch))
      ((error quit)
       (tessera-gnus-group--fail-fetch)
       (signal (car err) (cdr err))))))

(defun tessera-gnus-group--update (function &rest args)
  "Call the complete Gnus update FUNCTION with ARGS."
  (tessera-gnus-group--run-update function args nil))

(defun tessera-gnus-group--update-groups (function &rest args)
  "Call the selected-group Gnus update FUNCTION with ARGS."
  (tessera-gnus-group--run-update function args
                                  (length (gnus-group-process-prefix (car args)))))

(defun tessera-gnus-group--get-new-news (event)
  "Get new Gnus articles after mouse EVENT."
  (interactive "e")
  (mouse-select-window event)
  (if (eq tessera-gnus-group--status-state 'processing)
      (message "Gnus is already fetching new articles")
    (gnus-group-get-new-news)))

(defun tessera-gnus-group--heading ()
  "Return the main heading for a Gnus Group buffer."
  (let* ((leading-width
          (+ (string-pixel-width tessera-gnus-group--disclosure-glyph)
             (string-pixel-width " ")))
         (heading
          (concat (tessera-gnus-group--element
                   (tessera-ui-vertical-padding 'default 'heading.top-padding
                                                tessera-gnus-group--heading-top-padding 0)
                   'heading.top-padding)
                  (tessera-gnus-group--element (tessera-ui-entry-leading-safety-gap)
                                               'heading.safety-gap)
                  (tessera-gnus-group--element (tessera-ui-entry-padding 'entry.left-padding)
                                               'heading.left-padding)
                  (tessera-gnus-group--element (propertize " " 'display
                                                           `(space :width (,leading-width)))
                                               'heading.leading-gap)
                  (tessera-gnus-group--element "Gnus" 'heading.text '((:height 1.4) tessera-gnus-group-heading))
                  (tessera-gnus-group--element (tessera-ui-entry-padding 'entry.right-padding)
                                               'heading.right-padding)
                  (tessera-gnus-group--element (tessera-ui-vertical-padding
                                                'default 'heading.bottom-padding 0
                                                tessera-gnus-group--heading-bottom-padding)
                                               'heading.bottom-padding))))
    (put-text-property 0 (length heading) 'tessera-ui--parent-element 'heading heading)
    heading))

(defun tessera-gnus-group--empty-p ()
  "Return non-nil when the Group buffer has no displayed nodes."
  (not (or
        (text-property-not-all (point-min) (point-max) 'gnus-group nil)
        (text-property-not-all (point-min) (point-max) 'gnus-topic nil))))

(defun tessera-gnus-group--empty ()
  "Return the empty presentation for a Gnus Group buffer."
  (let ((text
         (concat (tessera-gnus-group--element
                  (tessera-ui-entry-padding 'entry.left-padding)
                  'view.empty)
                 (tessera-gnus-group--element gnus-no-groups-message 'view.empty.message 'tessera-gnus-group-empty)
                 "\n")))
    (put-text-property 0 (length text) 'tessera-ui--parent-element 'view.empty text)
    text))

(defun tessera-gnus-group--update-layout ()
  "Update the outer layout in the current Gnus Group buffer."
  (when (overlayp tessera-gnus-group--heading-overlay)
    (delete-overlay tessera-gnus-group--heading-overlay)
    (setq tessera-gnus-group--heading-overlay nil))
  (when (overlayp tessera-gnus-group--bottom-overlay)
    (delete-overlay tessera-gnus-group--bottom-overlay)
    (setq tessera-gnus-group--bottom-overlay nil))
  (when tessera-gnus-group--installed-p
    (let* ((empty
            (and (tessera-gnus-group--empty-p)
                 (tessera-gnus-group--empty)))
           (top
            (concat (tessera-ui-vertical-spacer 'main.top-padding tessera-gnus-group--main-top-padding)
                    (tessera-ui-vertical-spacer 'nodes.top-padding tessera-gnus-group--nodes-top-padding)
                    (and tessera-gnus-group-show-heading
                         (not (bound-and-true-p gnus-topic-mode))
                         (concat (tessera-gnus-group--heading) "\n"))
                    empty))
           (bottom
            (concat (tessera-ui-vertical-spacer 'nodes.bottom-padding tessera-gnus-group--nodes-bottom-padding)
                    (tessera-ui-vertical-spacer 'main.bottom-padding tessera-gnus-group--main-bottom-padding))))
      (add-text-properties 0 (length top) (list 'keymap tessera-gnus-group--layout-map) top)
      (add-text-properties 0 (length bottom) (list 'keymap tessera-gnus-group--layout-map) bottom)
      (setq tessera-gnus-group--heading-overlay
            (make-overlay (point-min) (point-min)))
      (overlay-put tessera-gnus-group--heading-overlay 'before-string top)
      (setq tessera-gnus-group--bottom-overlay
            (make-overlay (point-max) (point-max) nil t t))
      (overlay-put tessera-gnus-group--bottom-overlay 'after-string bottom))))

(defun tessera-gnus-group-refresh ()
  "Refresh the Tessera presentation in the current Group buffer."
  (when (and tessera-gnus-group--installed-p
             (derived-mode-p 'gnus-group-mode))
    (gnus-update-format-specifications nil 'group 'group-mode)
    (gnus-group-list-groups)))

(defun tessera-gnus-group--install ()
  "Install Tessera in the current Gnus Group buffer."
  (unless tessera-gnus-group--installed-p
    (setq tessera-gnus-group--original-line-format-local-p
          (local-variable-p 'gnus-group-line-format)
          tessera-gnus-group--original-line-format
          gnus-group-line-format
          tessera-gnus-group--installed-line-format
          tessera-gnus-group--line-format
          tessera-gnus-group--installed-p t
          tessera-gnus-group--status-state 'success
          tessera-gnus-group--statistics-cache nil
          tessera-gnus-group--latest-times
          (make-hash-table :test #'equal))
    (tessera-ui-header-line-install 'gnus-group)
    (setq-local gnus-group-line-format
                tessera-gnus-group--installed-line-format)
    (setq-local tessera-gnus--face-remap-function
                #'tessera-gnus-group--refresh-presentation)
    (gnus-update-format-specifications nil 'group 'group-mode)
    (gnus-update-group-mark-positions)
    (add-hook 'gnus-group-prepare-hook #'tessera-gnus-group--invalidate-statistics nil t)
    (add-hook 'gnus-group-prepare-hook #'tessera-gnus-group--update-layout nil t)
    (add-hook 'gnus-group-prepare-hook #'tessera-gnus-group--present-visible-windows nil t)
    (add-hook 'gnus-group-prepare-hook #'tessera-gnus-group--queue-latest-times nil t)
    (add-hook 'gnus-group-update-hook #'tessera-gnus-group--invalidate-statistics nil t)
    (add-hook 'gnus-group-update-hook #'tessera-gnus-group--after-line-update nil t)
    (add-hook 'gnus-group-update-group-hook #'tessera-gnus-group--invalidate-statistics nil t)
    (add-hook 'gnus-group-update-group-hook #'tessera-gnus-group--after-line-update nil t)
    (add-hook 'window-state-change-functions #'tessera-gnus-group--window-state-change nil t)
    (add-hook 'text-scale-mode-hook #'tessera-gnus-group--refresh-presentation nil t)
    (add-hook 'kill-buffer-hook #'tessera-gnus-group--cancel-latest-time nil t)
    (tessera-gnus-group--update-layout)
    (tessera-gnus-group--present-visible-windows)
    (tessera-gnus-group--queue-latest-times)
    (setq tessera-gnus-group--relative-time-timer
          (run-at-time 60 60 #'tessera-gnus-group--refresh-relative-times
                       (current-buffer)))
    (force-mode-line-update)))

(defun tessera-gnus-group--restore ()
  "Restore the native Group header in the current buffer."
  (when tessera-gnus-group--installed-p
    (remove-hook 'gnus-group-prepare-hook #'tessera-gnus-group--invalidate-statistics t)
    (remove-hook 'gnus-group-prepare-hook #'tessera-gnus-group--update-layout t)
    (remove-hook 'gnus-group-prepare-hook #'tessera-gnus-group--present-visible-windows t)
    (remove-hook 'gnus-group-prepare-hook #'tessera-gnus-group--queue-latest-times t)
    (remove-hook 'gnus-group-update-hook #'tessera-gnus-group--invalidate-statistics t)
    (remove-hook 'gnus-group-update-hook #'tessera-gnus-group--after-line-update t)
    (remove-hook 'gnus-group-update-group-hook #'tessera-gnus-group--invalidate-statistics t)
    (remove-hook 'gnus-group-update-group-hook #'tessera-gnus-group--after-line-update t)
    (remove-hook 'window-state-change-functions #'tessera-gnus-group--window-state-change t)
    (remove-hook 'text-scale-mode-hook #'tessera-gnus-group--refresh-presentation t)
    (remove-hook 'kill-buffer-hook #'tessera-gnus-group--cancel-latest-time t)
    (when (eq tessera-gnus--face-remap-function
              #'tessera-gnus-group--refresh-presentation)
      (kill-local-variable 'tessera-gnus--face-remap-function))
    (when (overlayp tessera-gnus-group--heading-overlay)
      (delete-overlay tessera-gnus-group--heading-overlay))
    (when (overlayp tessera-gnus-group--bottom-overlay)
      (delete-overlay tessera-gnus-group--bottom-overlay))
    (tessera-gnus-group--cancel-latest-time)
    (tessera-gnus-group--delete-window-overlays)
    (tessera-ui-header-line-restore)
    (when (eq gnus-group-line-format
              tessera-gnus-group--installed-line-format)
      (if tessera-gnus-group--original-line-format-local-p
          (setq-local gnus-group-line-format
                      tessera-gnus-group--original-line-format)
        (kill-local-variable 'gnus-group-line-format))
      (gnus-update-format-specifications nil 'group 'group-mode)
      (gnus-update-group-mark-positions))
    (setq tessera-gnus-group--installed-line-format nil
          tessera-gnus-group--original-line-format nil
          tessera-gnus-group--original-line-format-local-p nil
          tessera-gnus-group--statistics-cache nil
          tessera-gnus-group--fetch-current nil
          tessera-gnus-group--fetch-total nil
          tessera-gnus-group--heading-overlay nil
          tessera-gnus-group--bottom-overlay nil
          tessera-gnus-group--window-overlays nil
          tessera-gnus-group--latest-times nil
          tessera-gnus-group--installed-p nil)
    (gnus-group-list-groups)
    (force-mode-line-update)))

(defun tessera-gnus-group-enable ()
  "Enable Tessera in existing and future Gnus Group buffers."
  (unless tessera-gnus-group--enabled-p
    (setq tessera-gnus-group--enabled-p t)
    (advice-add 'gnus-get-unread-articles :around #'tessera-gnus-group--track-fetch)
    (advice-add 'gnus-get-unread-articles-in-group :after #'tessera-gnus-group--record-fetch)
    (advice-add 'gnus-group-mark-update :after #'tessera-gnus-group--after-mark-update)
    (advice-add 'gnus-group-get-new-news :around #'tessera-gnus-group--update)
    (advice-add 'gnus-group-get-new-news-this-group :around #'tessera-gnus-group--update-groups)
    (advice-add 'gnus-group-prepare-flat-list-dead :around #'tessera-gnus-group--format-dead-groups)
    (add-hook 'gnus-group-mode-hook #'tessera-gnus-group--install)
    (dolist (buffer
             (match-buffers '(derived-mode . gnus-group-mode)))
      (with-current-buffer buffer
        (tessera-gnus-group--install)
        (tessera-gnus-group-refresh)))))

(defun tessera-gnus-group-disable ()
  "Disable Tessera in existing Gnus Group buffers."
  (when tessera-gnus-group--enabled-p
    (setq tessera-gnus-group--enabled-p nil)
    (advice-remove 'gnus-get-unread-articles #'tessera-gnus-group--track-fetch)
    (advice-remove 'gnus-get-unread-articles-in-group #'tessera-gnus-group--record-fetch)
    (advice-remove 'gnus-group-mark-update #'tessera-gnus-group--after-mark-update)
    (advice-remove 'gnus-group-get-new-news #'tessera-gnus-group--update)
    (advice-remove 'gnus-group-get-new-news-this-group #'tessera-gnus-group--update-groups)
    (advice-remove 'gnus-group-prepare-flat-list-dead #'tessera-gnus-group--format-dead-groups)
    (remove-hook 'gnus-group-mode-hook #'tessera-gnus-group--install)
    (dolist (buffer
             (match-buffers '(derived-mode . gnus-group-mode)))
      (with-current-buffer buffer
        (tessera-gnus-group--restore)))))

(provide 'tessera-gnus-group)
;;; tessera-gnus-group.el ends here
