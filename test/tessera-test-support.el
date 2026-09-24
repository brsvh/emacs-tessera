;;; tessera-test-support.el --- Shared test helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; Backend-independent assertions and string inspection helpers.

;;; Code:

(require 'cl-lib)

(defun tessera-tests--property-position (property value string)
  "Return the position where PROPERTY equals VALUE in STRING."
  (cl-loop for position below (length string)
           when (equal (get-text-property position property string)
                       value)
           return position))

(defun tessera-tests--click-month (key &optional count)
  "Dispatch a Mouse-1 click on the month heading for KEY.
COUNT, when non-nil, is the native repeated-click count."
  (let ((window (selected-window))
        position)
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (and (eq (overlay-get overlay 'tessera-month-overlay)
                     'header)
                 (memq (overlay-get overlay 'window)
                       (list nil window)))
        (let* ((text (overlay-get overlay 'before-string))
               (index (tessera-tests--property-position
                       'tessera-month-key key text)))
          (when index
            (setq position
                  (list window (overlay-start overlay) '(0 . 0) 0
                        (cons text index)))))))
    (unless position (error "Missing month heading: %S" key))
    (let* ((press (pcase count
                    (2 'double-down-mouse-1)
                    (3 'triple-down-mouse-1)
                    (_ 'down-mouse-1)))
           (release (pcase count
                      (2 'double-mouse-1)
                      (3 'triple-mouse-1)
                      (_ 'mouse-1)))
           (unread-command-events
            (list (cons t (list release position)))))
      ;; Emacs falls back to the single-press binding for repeats,
      ;; retaining the repeated event's original modifiers.
      (funcall (key-binding [down-mouse-1] nil t position)
               (list press position))
      (when unread-command-events
        (error "Month click did not consume its release")))))

(provide 'tessera-test-support)
;;; tessera-test-support.el ends here
