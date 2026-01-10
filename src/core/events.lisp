(in-package #:clatter.core.events)

;;;; ============================================================
;;;; IRC Event Class Hierarchy
;;;; ============================================================
;;;; Base class and specialized event types for IRC messages.
;;;; Each event type can have its own handle-event method.

;;; Base event class - all events inherit from this
(defclass irc-event ()
  ((connection :initarg :connection :accessor event-connection
               :documentation "The IRC connection this event came from")
   (timestamp :initarg :timestamp :accessor event-timestamp
              :initform (get-universal-time)
              :documentation "When this event occurred")
   (raw-message :initarg :raw-message :accessor event-raw-message
                :initform nil
                :documentation "Original parsed IRC message if available"))
  (:documentation "Base class for all IRC events."))

;;; Generic function for handling events
(defgeneric handle-event (event app)
  (:documentation "Handle an IRC event, updating app state as needed."))

;;; Default method - log unknown events
(defmethod handle-event ((event irc-event) app)
  (declare (ignore app))
  ;; Default: do nothing for unknown event types
  nil)

;;;; ============================================================
;;;; Connection Events
;;;; ============================================================

(defclass connect-event (irc-event)
  ((server :initarg :server :accessor event-server)
   (nick :initarg :nick :accessor event-nick))
  (:documentation "Fired when successfully connected to server (001 received)."))

(defclass disconnect-event (irc-event)
  ((reason :initarg :reason :accessor event-reason :initform nil))
  (:documentation "Fired when disconnected from server."))

;;;; ============================================================
;;;; Message Events
;;;; ============================================================

(defclass message-event (irc-event)
  ((sender :initarg :sender :accessor event-sender
           :documentation "Nick of the message sender")
   (target :initarg :target :accessor event-target
           :documentation "Target channel or nick")
   (text :initarg :text :accessor event-text
         :documentation "Message text content")
   (server-time :initarg :server-time :accessor event-server-time
                :initform nil
                :documentation "Server-provided timestamp if available"))
  (:documentation "Base class for message events."))

(defclass privmsg-event (message-event)
  ()
  (:documentation "A PRIVMSG to a channel or user."))

(defclass notice-event (message-event)
  ()
  (:documentation "A NOTICE message."))

(defclass action-event (message-event)
  ()
  (:documentation "A CTCP ACTION (/me) message."))

;;;; ============================================================
;;;; Channel Events
;;;; ============================================================

(defclass channel-event (irc-event)
  ((channel :initarg :channel :accessor event-channel)
   (nick :initarg :nick :accessor event-nick))
  (:documentation "Base class for channel-related events."))

(defclass join-event (channel-event)
  ((account :initarg :account :accessor event-account :initform nil)
   (realname :initarg :realname :accessor event-realname :initform nil))
  (:documentation "User joined a channel."))

(defclass part-event (channel-event)
  ((message :initarg :message :accessor event-message :initform nil))
  (:documentation "User left a channel."))

(defclass quit-event (irc-event)
  ((nick :initarg :nick :accessor event-nick)
   (message :initarg :message :accessor event-message :initform nil))
  (:documentation "User quit the server."))

(defclass kick-event (channel-event)
  ((kicked-nick :initarg :kicked-nick :accessor event-kicked-nick)
   (reason :initarg :reason :accessor event-reason :initform nil))
  (:documentation "User was kicked from channel."))

(defclass nick-event (irc-event)
  ((old-nick :initarg :old-nick :accessor event-old-nick)
   (new-nick :initarg :new-nick :accessor event-new-nick))
  (:documentation "User changed their nick."))

(defclass topic-event (channel-event)
  ((topic :initarg :topic :accessor event-topic))
  (:documentation "Channel topic was changed."))

(defclass mode-event (irc-event)
  ((target :initarg :target :accessor event-target)
   (setter :initarg :setter :accessor event-setter)
   (modes :initarg :modes :accessor event-modes))
  (:documentation "Mode change on channel or user."))

;;;; ============================================================
;;;; Presence Events
;;;; ============================================================

(defclass away-event (irc-event)
  ((nick :initarg :nick :accessor event-nick)
   (message :initarg :message :accessor event-message :initform nil))
  (:documentation "User away status changed. Message nil means back."))

;;;; ============================================================
;;;; CTCP Events
;;;; ============================================================

(defclass ctcp-event (irc-event)
  ((sender :initarg :sender :accessor event-sender)
   (target :initarg :target :accessor event-target)
   (command :initarg :command :accessor event-command)
   (args :initarg :args :accessor event-args :initform nil))
  (:documentation "CTCP request or response."))

(defclass dcc-offer-event (ctcp-event)
  ((dcc-type :initarg :dcc-type :accessor event-dcc-type)
   (filename :initarg :filename :accessor event-filename :initform nil)
   (ip :initarg :ip :accessor event-ip)
   (port :initarg :port :accessor event-port)
   (filesize :initarg :filesize :accessor event-filesize :initform nil))
  (:documentation "DCC CHAT or SEND offer received."))

;;;; ============================================================
;;;; Server Events
;;;; ============================================================

(defclass names-event (irc-event)
  ((channel :initarg :channel :accessor event-channel)
   (names :initarg :names :accessor event-names))
  (:documentation "Channel member list received (353)."))

(defclass numeric-event (irc-event)
  ((numeric :initarg :numeric :accessor event-numeric)
   (params :initarg :params :accessor event-params))
  (:documentation "Generic numeric reply from server."))

;;;; ============================================================
;;;; Typing Events (IRCv3)
;;;; ============================================================

(defclass typing-event (irc-event)
  ((nick :initarg :nick :accessor event-nick)
   (target :initarg :target :accessor event-target)
   (state :initarg :state :accessor event-state))
  (:documentation "Typing indicator state change."))

;;;; ============================================================
;;;; Legacy compatibility - keep old struct for now
;;;; ============================================================

(defstruct (ev (:constructor ev (type &rest plist)))
  type
  plist)
