(in-package #:clatter.ui.keymap)

;;;; ============================================================
;;;; CLOS Key Action System
;;;; ============================================================
;;;; Key actions are CLOS classes with execute-action methods.
;;;; This allows for extensibility and cleaner organization.

;;; Base key action class
(defclass key-action ()
  ((name :initarg :name :accessor action-name :initform "unnamed")
   (description :initarg :description :accessor action-description :initform ""))
  (:documentation "Base class for all key actions."))

;;; Generic function for executing actions
(defgeneric execute-action (action app)
  (:documentation "Execute a key action on the app."))

;;; ============================================================
;;; Buffer Navigation Actions
;;; ============================================================

(defclass buffer-next-action (key-action)
  ()
  (:default-initargs :name "buffer-next" :description "Switch to next buffer"))

(defmethod execute-action ((action buffer-next-action) app)
  (%buf-next app))

(defclass buffer-prev-action (key-action)
  ()
  (:default-initargs :name "buffer-prev" :description "Switch to previous buffer"))

(defmethod execute-action ((action buffer-prev-action) app)
  (%buf-prev app))

;;; ============================================================
;;; Scroll Actions
;;; ============================================================

(defclass scroll-action (key-action)
  ((amount :initarg :amount :accessor scroll-amount :initform 5))
  (:documentation "Base class for scroll actions."))

(defclass scroll-up-action (scroll-action)
  ()
  (:default-initargs :name "scroll-up" :description "Scroll up (newer messages)"))

(defmethod execute-action ((action scroll-up-action) app)
  (%scroll-up app (scroll-amount action)))

(defclass scroll-down-action (scroll-action)
  ()
  (:default-initargs :name "scroll-down" :description "Scroll down (older messages)"))

(defmethod execute-action ((action scroll-down-action) app)
  (%scroll-down app (scroll-amount action)))

;;; ============================================================
;;; Split Pane Actions
;;; ============================================================

(defclass toggle-split-action (key-action)
  ()
  (:default-initargs :name "toggle-split" :description "Toggle split pane mode"))

(defmethod execute-action ((action toggle-split-action) app)
  (%toggle-split app))

(defclass swap-panes-action (key-action)
  ()
  (:default-initargs :name "swap-panes" :description "Swap left and right panes"))

(defmethod execute-action ((action swap-panes-action) app)
  (%swap-panes app))

(defclass toggle-active-pane-action (key-action)
  ()
  (:default-initargs :name "toggle-active-pane" :description "Toggle active pane for input"))

(defmethod execute-action ((action toggle-active-pane-action) app)
  (%toggle-active-pane app))

(defclass set-right-buffer-action (key-action)
  ()
  (:default-initargs :name "set-right-buffer" :description "Set right pane to current buffer"))

(defmethod execute-action ((action set-right-buffer-action) app)
  (%split-set-right-buffer app))

;;; ============================================================
;;; UI Toggle Actions
;;; ============================================================

(defclass toggle-nicklist-action (key-action)
  ()
  (:default-initargs :name "toggle-nicklist" :description "Toggle nick list panel"))

(defmethod execute-action ((action toggle-nicklist-action) app)
  (%toggle-nicklist app))

(defclass redraw-action (key-action)
  ()
  (:default-initargs :name "redraw" :description "Force redraw/refresh"))

(defmethod execute-action ((action redraw-action) app)
  (clatter.ui.tui:create-layout-windows app (ui-screen (app-ui app)))
  (mark-dirty app :layout :chat :buflist :status :input))

;;; ============================================================
;;; Input Actions
;;; ============================================================

(defclass input-action (key-action)
  ()
  (:documentation "Base class for input line actions."))

(defclass backspace-action (input-action)
  ()
  (:default-initargs :name "backspace" :description "Delete character before cursor"))

(defmethod execute-action ((action backspace-action) app)
  (input-backspace app))

(defclass delete-action (input-action)
  ()
  (:default-initargs :name "delete" :description "Delete character at cursor"))

(defmethod execute-action ((action delete-action) app)
  (input-delete app))

(defclass move-left-action (input-action)
  ()
  (:default-initargs :name "move-left" :description "Move cursor left"))

(defmethod execute-action ((action move-left-action) app)
  (input-move-left app))

(defclass move-right-action (input-action)
  ()
  (:default-initargs :name "move-right" :description "Move cursor right"))

(defmethod execute-action ((action move-right-action) app)
  (input-move-right app))

(defclass move-home-action (input-action)
  ()
  (:default-initargs :name "move-home" :description "Move cursor to beginning"))

(defmethod execute-action ((action move-home-action) app)
  (input-move-home app))

(defclass move-end-action (input-action)
  ()
  (:default-initargs :name "move-end" :description "Move cursor to end"))

(defmethod execute-action ((action move-end-action) app)
  (input-move-end app))

(defclass history-prev-action (input-action)
  ()
  (:default-initargs :name "history-prev" :description "Previous history entry"))

(defmethod execute-action ((action history-prev-action) app)
  (input-history-prev app))

(defclass history-next-action (input-action)
  ()
  (:default-initargs :name "history-next" :description "Next history entry"))

(defmethod execute-action ((action history-next-action) app)
  (input-history-next app))

(defclass submit-action (input-action)
  ()
  (:default-initargs :name "submit" :description "Submit input line"))

(defmethod execute-action ((action submit-action) app)
  (input-submit-line app))

(defclass tab-complete-action (input-action)
  ()
  (:default-initargs :name "tab-complete" :description "Tab completion"))

(defmethod execute-action ((action tab-complete-action) app)
  (input-tab-complete app))

;;; ============================================================
;;; Action Registry - singleton instances for common actions
;;; ============================================================

(defvar *action-registry* (make-hash-table :test 'equal)
  "Registry of action instances by name.")

(defun register-action (name action)
  "Register an action instance under a name."
  (setf (gethash name *action-registry*) action))

(defun find-action (name)
  "Find an action by name."
  (gethash name *action-registry*))

;; Register singleton instances
(register-action "buffer-next" (make-instance 'buffer-next-action))
(register-action "buffer-prev" (make-instance 'buffer-prev-action))
(register-action "scroll-up" (make-instance 'scroll-up-action :amount 10))
(register-action "scroll-down" (make-instance 'scroll-down-action :amount 10))
(register-action "scroll-up-small" (make-instance 'scroll-up-action :amount 5))
(register-action "scroll-down-small" (make-instance 'scroll-down-action :amount 5))
(register-action "toggle-split" (make-instance 'toggle-split-action))
(register-action "swap-panes" (make-instance 'swap-panes-action))
(register-action "toggle-active-pane" (make-instance 'toggle-active-pane-action))
(register-action "set-right-buffer" (make-instance 'set-right-buffer-action))
(register-action "toggle-nicklist" (make-instance 'toggle-nicklist-action))
(register-action "redraw" (make-instance 'redraw-action))
(register-action "backspace" (make-instance 'backspace-action))
(register-action "delete" (make-instance 'delete-action))
(register-action "move-left" (make-instance 'move-left-action))
(register-action "move-right" (make-instance 'move-right-action))
(register-action "move-home" (make-instance 'move-home-action))
(register-action "move-end" (make-instance 'move-end-action))
(register-action "history-prev" (make-instance 'history-prev-action))
(register-action "history-next" (make-instance 'history-next-action))
(register-action "submit" (make-instance 'submit-action))
(register-action "tab-complete" (make-instance 'tab-complete-action))

;;; ============================================================
;;; Helper to create action handler for keybindings
;;; ============================================================

(defun make-action-handler (action-name app)
  "Create a lambda that executes the named action."
  (let ((action (find-action action-name)))
    (if action
        (lambda (o e)
          (declare (ignore o e))
          (execute-action action app))
        (lambda (o e)
          (declare (ignore o e))
          (format *error-output* "Unknown action: ~a~%" action-name)))))
