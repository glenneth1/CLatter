(in-package #:clatter.core.events)

;;;; ============================================================
;;;; Event Handlers
;;;; ============================================================
;;;; Each event type gets its own handle-event method.
;;;; These methods update app state and deliver messages to buffers.

;;; Helper to find or create a buffer for a target
(defun find-or-create-buffer (app conn target)
  "Find existing buffer or create one for target (channel or nick)."
  (let* ((network-id (clatter.net.irc:irc-network-id conn))
         (buf (clatter.core.model:find-buffer-by-network app network-id target)))
    (or buf
        ;; Create query buffer for private messages
        (when (and (> (length target) 0)
                   (not (char= (char target 0) #\#)))
          (clatter.core.model:create-buffer app network-id target :query)))))

;;; Helper to check if message mentions our nick
(defun message-highlights-p (conn text)
  "Check if text contains our nick (case-insensitive)."
  (let ((our-nick (clatter.net.irc:irc-nick conn)))
    (and our-nick
         (search our-nick text :test #'char-equal))))

;;;; ============================================================
;;;; PRIVMSG Handler
;;;; ============================================================

(defmethod handle-event ((event privmsg-event) app)
  "Handle a PRIVMSG - deliver to appropriate buffer."
  (let* ((conn (event-connection event))
         (sender (event-sender event))
         (target (event-target event))
         (text (event-text event))
         (server-time (event-server-time event))
         (our-nick (clatter.net.irc:irc-nick conn))
         ;; If target is our nick, it's a PM - use sender as buffer target
         (buf-target (if (string-equal target our-nick) sender target))
         (buf (find-or-create-buffer app conn buf-target))
         (highlightp (message-highlights-p conn text)))
    (when buf
      (let ((msg (clatter.core.model:make-message
                  :nick sender
                  :text text
                  :ts (or server-time (get-universal-time))
                  :level :chat)))
        (clatter.core.dispatch:deliver-message app buf msg :highlightp highlightp)))))

;;;; ============================================================
;;;; NOTICE Handler
;;;; ============================================================

(defmethod handle-event ((event notice-event) app)
  "Handle a NOTICE - deliver to server buffer or channel."
  (let* ((conn (event-connection event))
         (sender (event-sender event))
         (target (event-target event))
         (text (event-text event))
         (network-id (clatter.net.irc:irc-network-id conn))
         ;; Notices usually go to server buffer unless from a channel
         (buf (if (and target (char= (char target 0) #\#))
                  (clatter.core.model:find-buffer app network-id target)
                  (clatter.core.model:find-buffer app network-id :server))))
    (when buf
      (let ((msg (clatter.core.model:make-message
                  :nick sender
                  :text text
                  :level :notice)))
        (clatter.core.dispatch:deliver-message app buf msg)))))

;;;; ============================================================
;;;; ACTION Handler (/me)
;;;; ============================================================

(defmethod handle-event ((event action-event) app)
  "Handle a CTCP ACTION (/me) - deliver as action message."
  (let* ((conn (event-connection event))
         (sender (event-sender event))
         (target (event-target event))
         (text (event-text event))
         (our-nick (clatter.net.irc:irc-nick conn))
         (buf-target (if (string-equal target our-nick) sender target))
         (buf (find-or-create-buffer app conn buf-target))
         (highlightp (message-highlights-p conn text)))
    (when buf
      (let ((msg (clatter.core.model:make-message
                  :nick sender
                  :text (format nil "* ~a ~a" sender text)
                  :level :chat)))
        (clatter.core.dispatch:deliver-message app buf msg :highlightp highlightp)))))

;;;; ============================================================
;;;; JOIN Handler
;;;; ============================================================

(defmethod handle-event ((event join-event) app)
  "Handle a JOIN - show join message in channel."
  (let* ((conn (event-connection event))
         (channel (event-channel event))
         (nick (event-nick event))
         (account (event-account event))
         (network-id (clatter.net.irc:irc-network-id conn))
         (buf (clatter.core.model:find-buffer-by-network app network-id channel)))
    (when buf
      ;; Add to member list
      (clatter.core.model:buffer-add-member buf nick)
      ;; Show join message
      (let* ((account-str (if (and account (not (string= account "*")))
                              (format nil " (~a)" account)
                              ""))
             (msg (clatter.core.model:make-message
                   :nick nick
                   :text (format nil "has joined ~a~a" channel account-str)
                   :level :join)))
        (clatter.core.dispatch:deliver-message app buf msg)))))

;;;; ============================================================
;;;; PART Handler
;;;; ============================================================

(defmethod handle-event ((event part-event) app)
  "Handle a PART - show part message and remove from members."
  (let* ((conn (event-connection event))
         (channel (event-channel event))
         (nick (event-nick event))
         (message (event-message event))
         (network-id (clatter.net.irc:irc-network-id conn))
         (buf (clatter.core.model:find-buffer-by-network app network-id channel)))
    (when buf
      ;; Remove from member list
      (clatter.core.model:buffer-remove-member buf nick)
      ;; Show part message
      (let* ((reason (if message (format nil " (~a)" message) ""))
             (msg (clatter.core.model:make-message
                   :nick nick
                   :text (format nil "has left ~a~a" channel reason)
                   :level :part)))
        (clatter.core.dispatch:deliver-message app buf msg)))))

;;;; ============================================================
;;;; QUIT Handler
;;;; ============================================================

(defmethod handle-event ((event quit-event) app)
  "Handle a QUIT - show quit message in all shared channels."
  (let* ((conn (event-connection event))
         (nick (event-nick event))
         (message (event-message event))
         (network-id (clatter.net.irc:irc-network-id conn)))
    ;; Find all channel buffers where this nick was a member
    (dolist (buf (clatter.core.model:app-buffers-list app))
      (when (and (eq (clatter.core.model:buffer-network buf) network-id)
                 (eq (clatter.core.model:buffer-kind buf) :channel)
                 (clatter.core.model:buffer-has-member-p buf nick))
        ;; Remove from member list
        (clatter.core.model:buffer-remove-member buf nick)
        ;; Show quit message
        (let* ((reason (if message (format nil " (~a)" message) ""))
               (msg (clatter.core.model:make-message
                     :nick nick
                     :text (format nil "has quit~a" reason)
                     :level :quit)))
          (clatter.core.dispatch:deliver-message app buf msg))))))

;;;; ============================================================
;;;; NICK Handler
;;;; ============================================================

(defmethod handle-event ((event nick-event) app)
  "Handle a NICK change - update member lists and notify channels."
  (let* ((conn (event-connection event))
         (old-nick (event-old-nick event))
         (new-nick (event-new-nick event))
         (network-id (clatter.net.irc:irc-network-id conn)))
    ;; Update all channel buffers where this nick was a member
    (dolist (buf (clatter.core.model:app-buffers-list app))
      (when (and (eq (clatter.core.model:buffer-network buf) network-id)
                 (eq (clatter.core.model:buffer-kind buf) :channel)
                 (clatter.core.model:buffer-has-member-p buf old-nick))
        ;; Update member list
        (clatter.core.model:buffer-remove-member buf old-nick)
        (clatter.core.model:buffer-add-member buf new-nick)
        ;; Show nick change message
        (let ((msg (clatter.core.model:make-message
                    :nick old-nick
                    :text (format nil "is now known as ~a" new-nick)
                    :level :system)))
          (clatter.core.dispatch:deliver-message app buf msg))))))

;;;; ============================================================
;;;; AWAY Handler
;;;; ============================================================

(defmethod handle-event ((event away-event) app)
  "Handle AWAY status change - notify shared channels."
  (let* ((conn (event-connection event))
         (nick (event-nick event))
         (message (event-message event))
         (network-id (clatter.net.irc:irc-network-id conn)))
    ;; Show in channels where we share membership
    (dolist (buf (clatter.core.model:app-buffers-list app))
      (when (and (eq (clatter.core.model:buffer-network buf) network-id)
                 (eq (clatter.core.model:buffer-kind buf) :channel)
                 (clatter.core.model:buffer-has-member-p buf nick))
        (let* ((status (if message
                           (format nil "is now away: ~a" message)
                           "is no longer away"))
               (msg (clatter.core.model:make-message
                     :nick nick
                     :text status
                     :level :away)))
          (clatter.core.dispatch:deliver-message app buf msg))))))

;;;; ============================================================
;;;; MODE Handler
;;;; ============================================================

(defmethod handle-event ((event mode-event) app)
  "Handle MODE change - update channel modes and notify."
  (let* ((conn (event-connection event))
         (target (event-target event))
         (setter (event-setter event))
         (modes (event-modes event))
         (network-id (clatter.net.irc:irc-network-id conn)))
    (when (and target (char= (char target 0) #\#))
      (let ((buf (clatter.core.model:find-buffer app network-id target)))
        (when buf
          (let ((msg (clatter.core.model:make-message
                      :nick setter
                      :text (format nil "sets mode ~{~a~^ ~}" modes)
                      :level :mode)))
            (clatter.core.dispatch:deliver-message app buf msg)))))))

;;;; ============================================================
;;;; NAMES Handler (353)
;;;; ============================================================

(defmethod handle-event ((event names-event) app)
  "Handle NAMES reply - update channel member list."
  (let* ((conn (event-connection event))
         (channel (event-channel event))
         (names (event-names event))
         (network-id (clatter.net.irc:irc-network-id conn))
         (buf (clatter.core.model:find-buffer-by-network app network-id channel)))
    (when buf
      ;; Add each name to the member list
      (dolist (name names)
        (clatter.core.model:buffer-add-member buf name)))))

