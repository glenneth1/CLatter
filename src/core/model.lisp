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
   (network         :initarg :network :initform nil :accessor buffer-network)  ; network name for multi-network
   (scrollback      :initform (make-ring :capacity 4000) :reader buffer-scrollback)
   (unread-count    :initform 0 :accessor buffer-unread-count)
   (highlight-count :initform 0 :accessor buffer-highlight-count)
   (scroll-offset   :initform 0 :accessor buffer-scroll-offset)
   (members         :initform (make-hash-table :test 'equal) :accessor buffer-members)
   (typing-users    :initform (make-hash-table :test 'equalp) :accessor buffer-typing-users)
   (channel-modes   :initform "" :accessor buffer-channel-modes)  ; e.g., "+nt"
   (my-modes        :initform "" :accessor buffer-my-modes)))     ; my modes in this channel e.g., "@" or "+"

(defun make-buffer (&key id (kind :channel) (title "") network)
  (make-instance 'buffer :id id :kind kind :title title :network network))

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
   (win-nicklist :initform nil :accessor ui-win-nicklist)  ;; nick list panel
   (win-status  :initform nil :accessor ui-win-status)
   (win-input   :initform nil :accessor ui-win-input)
   (term-w      :initform 0 :accessor ui-term-w)
   (term-h      :initform 0 :accessor ui-term-h)
   (buflist-w   :initform 28 :accessor ui-buflist-w)
   (nicklist-w  :initform 20 :accessor ui-nicklist-w)  ;; nick list width
   (nicklist-visible :initform nil :accessor ui-nicklist-visible)  ;; toggle state
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
   (buffer-order      :initform nil :accessor app-buffer-order)  ; visual order of buffer IDs for navigation
   (connections       :initform (make-hash-table :test 'equal) :accessor app-connections)  ; network-name -> connection
   (dirty-flags       :initform (list :layout :chat :buflist :status :input) :accessor app-dirty-flags)
   (quit-requested    :initform nil :accessor app-quit-requested)
   (ignore-list       :initform (make-hash-table :test 'equalp) :accessor app-ignore-list)))

(defun make-app ()
  (let ((a (make-instance 'app)))
    ;; No default server buffer - created per-network when connections start
    a))

(defun create-server-buffer (app network-name)
  "Create a server buffer for a network. Returns the buffer."
  (let* ((buffers (app-buffers app))
         (buf (make-buffer :id (length buffers) :kind :server :title network-name :network network-name)))
    (vector-push-extend buf buffers)
    (when (= (length buffers) 1)
      ;; First buffer, make it current
      (setf (app-current-buffer-id app) 0))
    (mark-dirty app :buflist)
    buf))

(defun mark-dirty (app &rest flags)
  (dolist (f flags)
    (pushnew f (app-dirty-flags app)))
  app)

(defun dirty-p (app) (not (null (app-dirty-flags app))))

(defun clear-dirty (app) (setf (app-dirty-flags app) nil))

(defun find-buffer (app id)
  (let ((buffers (app-buffers app)))
    (when (and (>= id 0) (< id (length buffers)))
      (aref buffers id))))

(defun current-buffer (app)
  "Return the current buffer. If the buffer at current-buffer-id is nil,
   find the next valid buffer and update current-buffer-id."
  (let ((buf (find-buffer app (app-current-buffer-id app))))
    (if buf
        buf
        ;; Current buffer is nil, find next valid one
        (loop for i from 0 below (length (app-buffers app))
              for b = (aref (app-buffers app) i)
              when b do (setf (app-current-buffer-id app) i)
                        (return b)
              finally (return nil)))))

(defun active-buffer (app)
  "Return the buffer that should receive input (respects active pane in split mode)."
  (let ((ui (app-ui app)))
    (if (and ui (ui-split-mode ui) (eq (ui-active-pane ui) :right))
        (let ((right-buf (find-buffer app (ui-split-buffer-id ui))))
          (if right-buf
              right-buf
              ;; Right buffer is nil, fall back to current-buffer and disable split
              (progn
                (setf (ui-split-mode ui) nil)
                (current-buffer app))))
        (current-buffer app))))

(defun get-buffer-connection (app buf)
  "Get the IRC connection for a buffer based on its network."
  (when (and buf (buffer-network buf))
    (gethash (buffer-network buf) (app-connections app))))

(defun get-current-connection (app)
  "Get the IRC connection for the current/active buffer."
  (get-buffer-connection app (active-buffer app)))

(defun remove-buffer (app buffer-id)
  "Remove a buffer from the app. Cannot remove the server buffer (id 0).
   Returns t if removed, nil otherwise."
  (when (and (> buffer-id 0) (< buffer-id (length (app-buffers app))))
    (let ((buffers (app-buffers app))
          (ui (app-ui app)))
      ;; Mark the buffer slot as nil (we can't easily shrink the vector without breaking IDs)
      (setf (aref buffers buffer-id) nil)
      ;; If current buffer was removed, switch to server buffer
      (when (= (app-current-buffer-id app) buffer-id)
        (setf (app-current-buffer-id app) 0))
      ;; If split buffer was removed, disable split mode
      (when (and (ui-split-mode ui) (= (ui-split-buffer-id ui) buffer-id))
        (setf (ui-split-mode ui) nil
              (ui-split-buffer-id ui) nil))
      (mark-dirty app :buflist :chat :status)
      t)))

(defun find-buffer-by-title (app title)
  "Find a buffer by its title. Returns the buffer or nil."
  (loop for buf across (app-buffers app)
        when (and buf (string-equal (buffer-title buf) title))
        return buf))

(defun ignore-nick (app nick)
  "Add a nick to the ignore list."
  (setf (gethash nick (app-ignore-list app)) t))

(defun unignore-nick (app nick)
  "Remove a nick from the ignore list."
  (remhash nick (app-ignore-list app)))

(defun ignored-p (app nick)
  "Check if a nick is ignored."
  (gethash nick (app-ignore-list app)))

(defun list-ignored (app)
  "Return a list of all ignored nicks."
  (let ((nicks nil))
    (maphash (lambda (k v) (declare (ignore v)) (push k nicks))
             (app-ignore-list app))
    (sort nicks #'string-lessp)))

(defun get-typing-nicks (buf &optional (timeout 5))
  "Return list of nicks currently typing in buffer.
   Removes stale entries older than TIMEOUT seconds."
  (when buf
    (let ((typing-users (buffer-typing-users buf))
          (now (get-universal-time))
          (nicks nil)
          (stale nil))
      (maphash (lambda (nick ts)
                 (if (> (- now ts) timeout)
                     (push nick stale)
                     (push nick nicks)))
               typing-users)
      ;; Clean up stale entries
      (dolist (nick stale)
        (remhash nick typing-users))
      (sort nicks #'string-lessp))))
