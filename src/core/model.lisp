(in-package #:clatter.core.model)

(defclass message ()
  ((ts        :initarg :ts :reader message-ts)
   (level     :initarg :level :reader message-level)
   (nick      :initarg :nick :reader message-nick)
   (text      :initarg :text :reader message-text)
   (highlight :initarg :highlight :initform nil :accessor message-highlight)))

(defun make-message (&key (ts (get-universal-time)) (level :chat) nick (text "") highlight)
  (make-instance 'message :ts ts :level level :nick nick :text text :highlight highlight))

(defclass buffer ()
  ((id              :initarg :id :accessor buffer-id)
   (kind            :initarg :kind :reader buffer-kind) ; :server :channel :query
   (title           :initarg :title :accessor buffer-title)
   (scrollback      :initform (make-ring :capacity 4000) :reader buffer-scrollback)
   (unread-count    :initform 0 :accessor buffer-unread-count)
   (highlight-count :initform 0 :accessor buffer-highlight-count)
   (scroll-offset   :initform 0 :accessor buffer-scroll-offset)
   (members         :initform (make-hash-table :test 'equal) :accessor buffer-members)))

(defun make-buffer (&key id (kind :channel) (title ""))
  (make-instance 'buffer :id id :kind kind :title title))

(defclass input-state ()
  ((text        :initform "" :accessor input-text)
   (cursor      :initform 0 :accessor input-cursor)
   (history     :initform (make-array 0 :adjustable t :fill-pointer 0) :accessor input-history)
   (history-pos :initform nil :accessor input-history-pos)))

(defun make-input-state () (make-instance 'input-state))

(defclass ui-state ()
  ((screen      :initform nil :accessor ui-screen)
   (win-buflist :initform nil :accessor ui-win-buflist)
   (win-chat    :initform nil :accessor ui-win-chat)
   (win-chat2   :initform nil :accessor ui-win-chat2)  ;; second pane for split view
   (win-status  :initform nil :accessor ui-win-status)
   (win-input   :initform nil :accessor ui-win-input)
   (term-w      :initform 0 :accessor ui-term-w)
   (term-h      :initform 0 :accessor ui-term-h)
   (buflist-w   :initform 24 :accessor ui-buflist-w)
   (input       :initform (make-input-state) :accessor ui-input)
   ;; Split pane state
   (split-mode  :initform nil :accessor ui-split-mode)  ;; nil or :horizontal
   (split-buffer-id :initform nil :accessor ui-split-buffer-id)  ;; buffer shown in second pane
   (active-pane :initform :left :accessor ui-active-pane)))  ;; :left or :right

(defun make-ui-state () (make-instance 'ui-state))

(defclass app ()
  ((ui                :initform (make-ui-state) :reader app-ui)
   (buffers           :initform (make-array 0 :adjustable t :fill-pointer 0) :accessor app-buffers)
   (current-buffer-id :initform 0 :accessor app-current-buffer-id)
   (dirty-flags       :initform (list :layout :chat :buflist :status :input) :accessor app-dirty-flags)
   (quit-requested    :initform nil :accessor app-quit-requested)))

(defun make-app ()
  (let ((a (make-instance 'app)))
    ;; seed only server buffer - channels created dynamically on JOIN
    (vector-push-extend (make-buffer :id 0 :kind :server :title "server") (app-buffers a))
    (setf (app-current-buffer-id a) 0)
    a))

(defun mark-dirty (app &rest flags)
  (dolist (f flags)
    (pushnew f (app-dirty-flags app)))
  app)

(defun dirty-p (app) (not (null (app-dirty-flags app))))

(defun clear-dirty (app) (setf (app-dirty-flags app) nil))

(defun find-buffer (app id)
  (aref (app-buffers app) id))

(defun current-buffer (app)
  (find-buffer app (app-current-buffer-id app)))

(defun active-buffer (app)
  "Return the buffer that should receive input (respects active pane in split mode)."
  (let ((ui (app-ui app)))
    (if (and ui (ui-split-mode ui) (eq (ui-active-pane ui) :right))
        (find-buffer app (ui-split-buffer-id ui))
        (current-buffer app))))
