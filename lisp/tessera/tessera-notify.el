;;; tessera-notify.el --- Notifications for Tessera  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bingshan Chang <chang@bingshan.org>

;; Author: Bingshan Chang <chang@bingshan.org>
;; Maintainer: Bingshan Chang <chang@bingshan.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (alert "1.2"))
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

;; Shared desktop notification delivery for Tessera backends.

;;; Code:

(require 'alert)
(require 'gravatar)
(require 'notifications)
(require 'subr-x)

(defcustom tessera-notify-enable nil
  "Default notification setting for Tessera backends."
  :type 'boolean
  :group 'tessera)

(defcustom tessera-notify-use-icons t
  "Whether Tessera desktop notifications display icons."
  :type 'boolean
  :group 'tessera)

(defcustom tessera-notify-use-gravatar t
  "Whether Tessera uses Gravatar images for sender icons."
  :type 'boolean
  :group 'tessera)

(defcustom tessera-notify-avatar-cache-directory
  (locate-user-emacs-file "tessera/avatars/")
  "Directory used to cache notification avatars."
  :type 'directory
  :group 'tessera)

(defun tessera-notify-enabled-p (setting)
  "Return the effective notification state for SETTING.

The value `inherit' follows `tessera-notify-enable'."
  (if (eq setting 'inherit)
      tessera-notify-enable
    setting))

(defun tessera-notify-avatar (mail-address)
  "Return a cached Gravatar file for MAIL-ADDRESS, or nil."
  (when (and tessera-notify-use-icons
             tessera-notify-use-gravatar
             (stringp mail-address)
             (not (string-empty-p mail-address)))
    (let ((file
           (expand-file-name
            (gravatar-hash mail-address)
            tessera-notify-avatar-cache-directory)))
      (if (file-exists-p file)
          file
        (condition-case nil
            (let ((gravatar-default-image "404")
                  (gravatar-force-default nil)
                  (image
                   (gravatar-retrieve-synchronously
                    mail-address)))
              (unless (eq image 'error)
                (let ((data (image-property image :data))
                      (coding-system-for-write 'binary))
                  (when data
                    (make-directory
                     tessera-notify-avatar-cache-directory t)
                    (write-region data nil file nil 'silent)
                    file))))
          (error nil))))))

(defun tessera-notify--action-list (actions)
  "Return the desktop action list described by ACTIONS."
  (apply #'append
         (mapcar
          (lambda (action)
            (list (nth 0 action) (nth 1 action)))
          actions)))

(defun tessera-notify--run-action (actions _id key)
  "Run the action named KEY from ACTIONS."
  (let ((action (assoc-string key actions)))
    (when action
      (funcall (nth 2 action)))))

(defun tessera-notify--notify (info)
  "Send the notification described by alert INFO."
  (let* ((data (plist-get info :data))
         (actions (plist-get data :actions))
         (category (plist-get info :category)))
    (notifications-notify
     :title (plist-get info :title)
     :body (plist-get info :message)
     :app-name "Tessera"
     :app-icon (and tessera-notify-use-icons
                    (plist-get info :icon))
     :category
     (if (symbolp category) (symbol-name category) category)
     :timeout (if (plist-get info :persistent) 0 -1)
     :urgency
     (or (cdr (assq (plist-get info :severity)
                    alert-notifications-priorities))
         'normal)
     :actions (and actions
                   (tessera-notify--action-list actions))
     :on-action
     (and actions
          (apply-partially #'tessera-notify--run-action
                           actions)))))

(alert-define-style
 'tessera
 :title "Tessera desktop notification"
 :notifier #'tessera-notify--notify)

(defun tessera-notify (notification)
  "Send NOTIFICATION through the Tessera alert style.

NOTIFICATION is a plist with `:title' and `:message' strings.  Its
optional `:actions' value is a list of (KEY LABEL FUNCTION) lists.
It may also contain `:severity', `:category', `:icon', and
`:persistent' values accepted by `alert'."
  (alert
   (plist-get notification :message)
   :title (plist-get notification :title)
   :severity (or (plist-get notification :severity) 'normal)
   :category (plist-get notification :category)
   :icon (plist-get notification :icon)
   :persistent (plist-get notification :persistent)
   :data (list :actions (plist-get notification :actions))
   :style 'tessera))

(provide 'tessera-notify)
;;; tessera-notify.el ends here
