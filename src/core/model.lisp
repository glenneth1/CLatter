(in-package #:clatter.core.model)

(defclass message ()
  ((ts        :initarg :ts :reader message-ts)
   (level     :initarg :level :reader message-level)
   (nick      :initarg :nick :reader message-nick)
   (text      :initarg :text :reader message-text)
   (highlight :initarg :highlight :initform nil :accessor message-highlight)))

(defun make-message (&key (ts (get-universal-time)) (level :chat) nick (text "") highlight)
  (make-instance 'message :ts ts :level level :nick nick :text text :highlight highlight))

;;;; ============================================================
;;;; Buffer Class Hierarchy
;;;; ============================================================

;;; Base buffer class - common slots for all buffer types
(defclass buffer ()
  ((id              :initarg :id :accessor buffer-id)
   (title           :initarg :title :accessor buffer-title)
   (network         :initarg :network :initform nil :accessor buffer-network)
   (scrollback      :initform (make-ring :capacity 4000) :reader buffer-scrollback)
   (unread-count    :initform 0 :accessor buffer-unread-count)
   (highlight-count :initform 0 :accessor buffer-highlight-count)
   (scroll-offset   :initform 0 :accessor buffer-scroll-offset))
  (:documentation "Base class for all IRC buffers."))

;;; Server buffer - for server messages, no members or typing
(defclass server-buffer (buffer)
  ()
  (:documentation "Buffer for server messages and notices."))

;;; Channel buffer - has members, typing, modes
(defclass channel-buffer (buffer)
  ((members         :initform (make-hash-table :test 'equal) :accessor buffer-members)
   (typing-users    :initform (make-hash-table :test 'equalp) :accessor buffer-typing-users)
   (channel-modes   :initform "" :accessor buffer-channel-modes)
   (my-modes        :initform "" :accessor buffer-my-modes))
  (:documentation "Buffer for IRC channels with member tracking."))

;;; Query buffer - private messages, has typing but no member list
(defclass query-buffer (buffer)
  ((typing-users    :initform (make-hash-table :test 'equalp) :accessor buffer-typing-users))
  (:documentation "Buffer for private messages (queries)."))

;;; DCC buffer - for DCC chat sessions
(defclass dcc-buffer (buffer)
  ((dcc-connection  :initarg :dcc-connection :initform nil :accessor buffer-dcc-connection))
  (:documentation "Buffer for DCC chat connections."))

;;; Generic function to get buffer kind (for backward compatibility)
(defgeneric buffer-kind (buffer)
  (:documentation "Return the kind of buffer (:server, :channel, :query, :dcc)."))

(defmethod buffer-kind ((buf server-buffer)) :server)
(defmethod buffer-kind ((buf channel-buffer)) :channel)
(defmethod buffer-kind ((buf query-buffer)) :query)
(defmethod buffer-kind ((buf dcc-buffer)) :dcc)

;;; Generic accessors with default methods for buffers that don't have these slots
(defgeneric buffer-members (buffer)
  (:documentation "Return the members hash table for a buffer."))

(defmethod buffer-members ((buf buffer))
  "Default: return empty hash table for buffers without members."
  (make-hash-table :test 'equal))

(defmethod buffer-members ((buf channel-buffer))
  (slot-value buf 'members))

(defgeneric buffer-typing-users (buffer)
  (:documentation "Return the typing-users hash table for a buffer."))

(defmethod buffer-typing-users ((buf buffer))
  "Default: return empty hash table for buffers without typing."
  (make-hash-table :test 'equalp))

(defmethod buffer-typing-users ((buf channel-buffer))
  (slot-value buf 'typing-users))

(defmethod buffer-typing-users ((buf query-buffer))
  (slot-value buf 'typing-users))

(defgeneric buffer-channel-modes (buffer)
  (:documentation "Return channel modes string."))

(defmethod buffer-channel-modes ((buf buffer)) "")
(defmethod buffer-channel-modes ((buf channel-buffer))
  (slot-value buf 'channel-modes))

(defgeneric (setf buffer-channel-modes) (value buffer)
  (:documentation "Set channel modes string."))

(defmethod (setf buffer-channel-modes) (value (buf buffer))
  (declare (ignore value)) nil)
(defmethod (setf buffer-channel-modes) (value (buf channel-buffer))
  (setf (slot-value buf 'channel-modes) value))

(defgeneric buffer-my-modes (buffer)
  (:documentation "Return my modes in this channel."))

(defmethod buffer-my-modes ((buf buffer)) "")
(defmethod buffer-my-modes ((buf channel-buffer))
  (slot-value buf 'my-modes))

(defgeneric (setf buffer-my-modes) (value buffer)
  (:documentation "Set my modes in this channel."))

(defmethod (setf buffer-my-modes) (value (buf buffer))
  (declare (ignore value)) nil)
(defmethod (setf buffer-my-modes) (value (buf channel-buffer))
  (setf (slot-value buf 'my-modes) value))

;;; Factory function - creates appropriate buffer subclass
(defun make-buffer (&key id (kind :channel) (title "") network)
  "Create a buffer of the appropriate type based on KIND."
  (ecase kind
    (:server (make-instance 'server-buffer :id id :title title :network network))
    (:channel (make-instance 'channel-buffer :id id :title title :network network))
    (:query (make-instance 'query-buffer :id id :title title :network network))
    (:dcc (make-instance 'dcc-buffer :id id :title title :network network))))

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
   Returns t if removed, nil otherwise.
   Automatically compacts buffer vector to prevent memory leaks."
  (when (and (> buffer-id 0) (< buffer-id (length (app-buffers app))))
    (let ((buffers (app-buffers app))
          (ui (app-ui app)))
      ;; Mark the buffer slot as nil
      (setf (aref buffers buffer-id) nil)
      ;; If current buffer was removed, switch to server buffer
      (when (= (app-current-buffer-id app) buffer-id)
        (setf (app-current-buffer-id app) 0))
      ;; If split buffer was removed, disable split mode
      (when (and (ui-split-mode ui) (= (ui-split-buffer-id ui) buffer-id))
        (setf (ui-split-mode ui) nil
              (ui-split-buffer-id ui) nil))
      ;; Compact buffers to reclaim memory
      (compact-buffers app)
      (mark-dirty app :buflist :chat :status)
      t)))

(defun compact-buffers (app)
  "Remove nil slots from buffer vector and update all buffer IDs.
   This prevents memory leaks from accumulated nil slots after buffer removal.
   Preserves buffer order and updates current-buffer-id and split-buffer-id."
  (let* ((old-buffers (app-buffers app))
         (old-len (length old-buffers))
         (ui (app-ui app))
         (old-current-id (app-current-buffer-id app))
         (old-split-id (when ui (ui-split-buffer-id ui)))
         ;; Build mapping from old IDs to new IDs
         (id-map (make-hash-table :test 'eql))
         (new-buffers (make-array 16 :adjustable t :fill-pointer 0))
         (new-id 0))
    ;; First pass: collect non-nil buffers and build ID mapping
    (loop for old-id from 0 below old-len
          for buf = (aref old-buffers old-id)
          when buf do
            (setf (gethash old-id id-map) new-id)
            (setf (buffer-id buf) new-id)
            (vector-push-extend buf new-buffers)
            (incf new-id))
    ;; Update current buffer ID
    (let ((new-current (gethash old-current-id id-map)))
      (setf (app-current-buffer-id app)
            (if new-current new-current 0)))
    ;; Update split buffer ID if in split mode
    (when (and ui old-split-id)
      (let ((new-split (gethash old-split-id id-map)))
        (if new-split
            (setf (ui-split-buffer-id ui) new-split)
            ;; Split buffer was removed, disable split mode
            (setf (ui-split-mode ui) nil
                  (ui-split-buffer-id ui) nil))))
    ;; Update buffer order list
    (let ((old-order (app-buffer-order app)))
      (when old-order
        (setf (app-buffer-order app)
              (loop for old-id in old-order
                    for new-id = (gethash old-id id-map)
                    when new-id collect new-id))))
    ;; Replace the buffer vector
    (setf (app-buffers app) new-buffers)))

(defun find-buffer-by-title (app title)
  "Find a buffer by its title. Returns the buffer or nil."
  (loop for buf across (app-buffers app)
        when (and buf (string-equal (buffer-title buf) title))
        return buf))

(defun find-buffer-by-network (app network-id target)
  "Find a buffer by network and target (channel/nick or :server).
   TARGET can be a string (channel/nick) or :server for the server buffer."
  (loop for buf across (app-buffers app)
        when (and buf
                  (equal (buffer-network buf) network-id)
                  (if (eq target :server)
                      (eq (buffer-kind buf) :server)
                      (string-equal (buffer-title buf) target)))
        return buf))

(defun create-buffer (app network-id title kind)
  "Create a new buffer for a network. Returns the buffer."
  (let* ((buffers (app-buffers app))
         (buf (make-buffer :id (length buffers) :kind kind :title title :network network-id)))
    (vector-push-extend buf buffers)
    (mark-dirty app :buflist)
    buf))

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

;;;; ============================================================
;;;; Buffer Member Management
;;;; ============================================================

(defun buffer-add-member (buf nick)
  "Add a nick to the buffer's member list."
  (when (and buf nick)
    (setf (gethash nick (buffer-members buf)) t)))

(defun buffer-remove-member (buf nick)
  "Remove a nick from the buffer's member list."
  (when (and buf nick)
    (remhash nick (buffer-members buf))))

(defun buffer-has-member-p (buf nick)
  "Check if nick is in the buffer's member list."
  (when (and buf nick)
    (gethash nick (buffer-members buf))))

(defun buffer-member-list (buf)
  "Return a sorted list of all members in the buffer."
  (when buf
    (let ((nicks nil))
      (maphash (lambda (k v) (declare (ignore v)) (push k nicks))
               (buffer-members buf))
      (sort nicks #'string-lessp))))

(defun app-buffers-list (app)
  "Return a list of all non-nil buffers in the app."
  (loop for buf across (app-buffers app)
        when buf collect buf))
