(in-package #:clatter.ui.render)

;;;; ============================================================
;;;; CLOS UI Rendering Methods
;;;; ============================================================
;;;; Generic functions specialized on buffer types for rendering.
;;;; This allows buffer-type-specific rendering behavior.

;;; ============================================================
;;; Generic Functions for Buffer Rendering
;;; ============================================================

(defgeneric render-buffer-title (buffer)
  (:documentation "Return the display title for a buffer in the buffer list."))

(defgeneric render-buffer-status (buffer)
  (:documentation "Return status info for a buffer (for status bar)."))

(defgeneric render-buffer-indicator (buffer)
  (:documentation "Return the type indicator character for a buffer."))

(defgeneric buffer-allows-input-p (buffer)
  (:documentation "Return t if the buffer accepts user input."))

(defgeneric buffer-has-members-p (buffer)
  (:documentation "Return t if the buffer has a member list."))

;;; ============================================================
;;; Default Methods (base buffer class)
;;; ============================================================

(defmethod render-buffer-title ((buffer clatter.core.model:buffer))
  "Default: just return the buffer title."
  (clatter.core.model:buffer-title buffer))

(defmethod render-buffer-status ((buffer clatter.core.model:buffer))
  "Default: return title only."
  (clatter.core.model:buffer-title buffer))

(defmethod render-buffer-indicator ((buffer clatter.core.model:buffer))
  "Default: no indicator."
  "")

(defmethod buffer-allows-input-p ((buffer clatter.core.model:buffer))
  "Default: no input allowed."
  nil)

(defmethod buffer-has-members-p ((buffer clatter.core.model:buffer))
  "Default: no member list."
  nil)

;;; ============================================================
;;; Server Buffer Methods
;;; ============================================================

(defmethod render-buffer-title ((buffer clatter.core.model:server-buffer))
  "Server buffers show network name."
  (clatter.core.model:buffer-title buffer))

(defmethod render-buffer-indicator ((buffer clatter.core.model:server-buffer))
  "Server indicator."
  "")

(defmethod buffer-allows-input-p ((buffer clatter.core.model:server-buffer))
  "Server buffers don't accept direct chat input."
  nil)

(defmethod buffer-has-members-p ((buffer clatter.core.model:server-buffer))
  "Server buffers have no member list."
  nil)

;;; ============================================================
;;; Channel Buffer Methods
;;; ============================================================

(defmethod render-buffer-title ((buffer clatter.core.model:channel-buffer))
  "Channel buffers show channel name."
  (clatter.core.model:buffer-title buffer))

(defmethod render-buffer-status ((buffer clatter.core.model:channel-buffer))
  "Channel status includes modes if set."
  (let ((title (clatter.core.model:buffer-title buffer))
        (modes (clatter.core.model:buffer-channel-modes buffer)))
    (if (and modes (> (length modes) 0))
        (format nil "~a [~a]" title modes)
        title)))

(defmethod render-buffer-indicator ((buffer clatter.core.model:channel-buffer))
  "Channel indicator."
  "#")

(defmethod buffer-allows-input-p ((buffer clatter.core.model:channel-buffer))
  "Channels accept chat input."
  t)

(defmethod buffer-has-members-p ((buffer clatter.core.model:channel-buffer))
  "Channels have member lists."
  t)

;;; ============================================================
;;; Query Buffer Methods
;;; ============================================================

(defmethod render-buffer-title ((buffer clatter.core.model:query-buffer))
  "Query buffers show nick name."
  (clatter.core.model:buffer-title buffer))

(defmethod render-buffer-indicator ((buffer clatter.core.model:query-buffer))
  "Query indicator."
  "@")

(defmethod buffer-allows-input-p ((buffer clatter.core.model:query-buffer))
  "Queries accept chat input."
  t)

(defmethod buffer-has-members-p ((buffer clatter.core.model:query-buffer))
  "Queries have no member list."
  nil)

;;; ============================================================
;;; DCC Buffer Methods
;;; ============================================================

(defmethod render-buffer-title ((buffer clatter.core.model:dcc-buffer))
  "DCC buffers show DCC type and nick."
  (format nil "DCC:~a" (clatter.core.model:buffer-title buffer)))

(defmethod render-buffer-indicator ((buffer clatter.core.model:dcc-buffer))
  "DCC indicator."
  "D")

(defmethod buffer-allows-input-p ((buffer clatter.core.model:dcc-buffer))
  "DCC chat buffers accept input."
  t)

(defmethod buffer-has-members-p ((buffer clatter.core.model:dcc-buffer))
  "DCC buffers have no member list."
  nil)

;;; ============================================================
;;; Buffer List Item Rendering
;;; ============================================================

(defgeneric render-buflist-item (buffer current-p indent)
  (:documentation "Render a buffer list item string."))

(defmethod render-buflist-item ((buffer clatter.core.model:buffer) current-p indent)
  "Render a buffer list item with marker, indent, title, and counts."
  (let* ((marker (if current-p ">" " "))
         (title (render-buffer-title buffer))
         (unread (clatter.core.model:buffer-unread-count buffer))
         (hi (clatter.core.model:buffer-highlight-count buffer)))
    (format nil "~a~a~a~@[ (~d)~]~@[ !~d~]"
            marker indent title
            (and (> unread 0) unread)
            (and (> hi 0) hi))))

(defmethod render-buflist-item ((buffer clatter.core.model:server-buffer) current-p indent)
  "Server buffers render without indent."
  (declare (ignore indent))
  (let* ((marker (if current-p ">" " "))
         (title (render-buffer-title buffer)))
    (format nil "~a~a" marker title)))

;;; ============================================================
;;; Status Bar Rendering
;;; ============================================================

(defgeneric render-status-info (buffer)
  (:documentation "Render status bar info for a buffer."))

(defmethod render-status-info ((buffer clatter.core.model:buffer))
  "Default status info."
  (let ((network (clatter.core.model:buffer-network buffer))
        (title (render-buffer-status buffer)))
    (if network
        (format nil "~a/~a" network title)
        title)))

(defmethod render-status-info ((buffer clatter.core.model:channel-buffer))
  "Channel status includes member count."
  (let* ((network (clatter.core.model:buffer-network buffer))
         (title (render-buffer-status buffer))
         (members (clatter.core.model:buffer-members buffer))
         (count (if members (hash-table-count members) 0)))
    (format nil "~a/~a (~d users)"
            (or network "?") title count)))

(defmethod render-status-info ((buffer clatter.core.model:query-buffer))
  "Query status shows network and nick."
  (let ((network (clatter.core.model:buffer-network buffer))
        (nick (clatter.core.model:buffer-title buffer)))
    (format nil "~a/~a (query)" (or network "?") nick)))

(defmethod render-status-info ((buffer clatter.core.model:dcc-buffer))
  "DCC status shows connection info."
  (let ((title (clatter.core.model:buffer-title buffer)))
    (format nil "DCC: ~a" title)))
