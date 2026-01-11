(in-package #:clatter.core.commands)

;;;; ============================================================
;;;; CLOS Command System
;;;; ============================================================
;;;; Commands are defined as classes with metadata (name, aliases, help).
;;;; Each command class has an execute-cmd method.

;;; Command registry - maps command names to command classes
(defvar *command-registry* (make-hash-table :test 'equal)
  "Hash table mapping command names (uppercase strings) to command class symbols.")

;;; Base command class
(defclass irc-command ()
  ((name :initarg :name :accessor command-name
         :documentation "Primary command name (e.g., \"JOIN\")")
   (aliases :initarg :aliases :accessor command-aliases :initform nil
            :documentation "List of alternative names")
   (help :initarg :help :accessor command-help :initform ""
         :documentation "Help text for /help display")
   (min-args :initarg :min-args :accessor command-min-args :initform 0
             :documentation "Minimum number of arguments required"))
  (:documentation "Base class for all IRC commands."))

;;; Generic function for executing commands
(defgeneric execute-cmd (command app conn args)
  (:documentation "Execute a command with the given app, connection, and arguments."))

;;; Default method - should not be called
(defmethod execute-cmd ((command irc-command) app conn args)
  (declare (ignore app conn args))
  (error "No execute-cmd method defined for command ~a" (command-name command)))

;;; Helper to register a command
(defun register-command (command-class)
  "Register a command class in the registry."
  (let ((cmd (make-instance command-class)))
    (setf (gethash (command-name cmd) *command-registry*) command-class)
    (dolist (alias (command-aliases cmd))
      (setf (gethash alias *command-registry*) command-class))))

;;; Helper to find a command by name
(defun find-command (name)
  "Find a command class by name. Returns nil if not found."
  (let ((class-sym (gethash (string-upcase name) *command-registry*)))
    (when class-sym
      (make-instance class-sym))))

;;; Helper to deliver a message to current buffer
(defun cmd-message (app text &key (level :system) (nick "*"))
  "Deliver a system message to the current buffer."
  (de.anvi.croatoan:submit
    (clatter.core.dispatch:deliver-message
     app (clatter.core.model:current-buffer app)
     (clatter.core.model:make-message :level level :nick nick :text text))))

;;; Helper to deliver an error message
(defun cmd-error (app text)
  "Deliver an error message to the current buffer."
  (cmd-message app text :level :error))

;;;; ============================================================
;;;; Command Definitions
;;;; ============================================================

;;; /join #channel [key]
(defclass join-command (irc-command)
  ()
  (:default-initargs
   :name "JOIN"
   :aliases '("J")
   :help "/join #channel [key] - Join a channel"
   :min-args 1))

(defmethod execute-cmd ((command join-command) app conn args)
  (when (> (length args) 0)
    (multiple-value-bind (channel key) (split-first-word args)
      (clatter.net.irc:irc-send conn
        (if (> (length key) 0)
            (clatter.core.protocol:irc-join channel key)
            (clatter.core.protocol:irc-join channel)))
      (add-to-autojoin app channel)))
  t)

;;; /part [#channel] [message]
(defclass part-command (irc-command)
  ()
  (:default-initargs
   :name "PART"
   :aliases '("LEAVE")
   :help "/part [#channel] [msg] - Leave channel"))

(defmethod execute-cmd ((command part-command) app conn args)
  (let* ((buf (clatter.core.model:active-buffer app))
         (target (if buf (clatter.core.model:buffer-title buf) ""))
         (channel-to-part nil))
    (if (> (length args) 0)
        (if (char= (char args 0) #\#)
            (multiple-value-bind (channel msg) (split-first-word args)
              (setf channel-to-part channel)
              (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-part channel msg)))
            (when (> (length target) 0)
              (setf channel-to-part target)
              (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-part target args))))
        (when (> (length target) 0)
          (setf channel-to-part target)
          (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-part target))))
    (when (and channel-to-part (> (length channel-to-part) 0))
      (remove-from-autojoin app channel-to-part)))
  t)

;;; /msg target message
(defclass msg-command (irc-command)
  ()
  (:default-initargs
   :name "MSG"
   :aliases '("PRIVMSG")
   :help "/msg target text - Send private message"
   :min-args 2))

(defmethod execute-cmd ((command msg-command) app conn args)
  (when (> (length args) 0)
    (multiple-value-bind (target text) (split-first-word args)
      (when (> (length text) 0)
        (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-privmsg target text))
        (let ((buf (clatter.net.irc::irc-find-or-create-buffer conn target)))
          (de.anvi.croatoan:submit
            (clatter.core.dispatch:deliver-message
             app buf
             (clatter.core.model:make-message :level :chat
                                              :nick (clatter.net.irc:irc-nick conn)
                                              :text text)))))))
  t)

;;; /me action
(defclass me-command (irc-command)
  ()
  (:default-initargs
   :name "ME"
   :help "/me action - Send action to current channel"
   :min-args 1))

(defmethod execute-cmd ((command me-command) app conn args)
  (let* ((buf (clatter.core.model:active-buffer app))
         (target (if buf (clatter.core.model:buffer-title buf) "")))
    (when (and buf (> (length args) 0) (> (length target) 0))
      (let ((action-text (format nil "~CACTION ~a~C" (code-char 1) args (code-char 1))))
        (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-privmsg target action-text))
        (de.anvi.croatoan:submit
          (clatter.core.dispatch:deliver-message
           app buf
           (clatter.core.model:make-message :level :chat
                                            :nick (format nil "* ~a" (clatter.net.irc:irc-nick conn))
                                            :text args))))))
  t)

;;; /nick newnick
(defclass nick-command (irc-command)
  ()
  (:default-initargs
   :name "NICK"
   :help "/nick newnick - Change nickname"
   :min-args 1))

(defmethod execute-cmd ((command nick-command) app conn args)
  (declare (ignore app))
  (when (> (length args) 0)
    (multiple-value-bind (newnick rest) (split-first-word args)
      (declare (ignore rest))
      (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-nick newnick))))
  t)

;;; /quit [message]
(defclass quit-command (irc-command)
  ()
  (:default-initargs
   :name "QUIT"
   :aliases '("Q")
   :help "/quit [message] - Disconnect"))

(defmethod execute-cmd ((command quit-command) app conn args)
  (when conn
    (setf (clatter.net.irc:irc-reconnect-enabled conn) nil)
    (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-quit
                                    (if (> (length args) 0) args "CLatter"))))
  (setf (clatter.core.model:app-quit-requested app) t)
  t)

;;; /ns command - NickServ shortcut
(defclass ns-command (irc-command)
  ()
  (:default-initargs
   :name "NS"
   :help "/ns command - Send to NickServ"
   :min-args 1))

(defmethod execute-cmd ((command ns-command) app conn args)
  (declare (ignore app))
  (when (> (length args) 0)
    (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-privmsg "NickServ" args)))
  t)

;;; /cs command - ChanServ shortcut
(defclass cs-command (irc-command)
  ()
  (:default-initargs
   :name "CS"
   :help "/cs command - Send to ChanServ"
   :min-args 1))

(defmethod execute-cmd ((command cs-command) app conn args)
  (declare (ignore app))
  (when (> (length args) 0)
    (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-privmsg "ChanServ" args)))
  t)

;;; /query nick - open query buffer
(defclass query-command (irc-command)
  ()
  (:default-initargs
   :name "QUERY"
   :help "/query nick - Open query with user"
   :min-args 1))

(defmethod execute-cmd ((command query-command) app conn args)
  (when (> (length args) 0)
    (multiple-value-bind (nick rest) (split-first-word args)
      (declare (ignore rest))
      (let ((buf (clatter.net.irc::irc-find-or-create-buffer conn nick)))
        (when buf
          ;; Switch to the new buffer and refresh UI
          (de.anvi.croatoan:submit
            (setf (clatter.core.model:app-current-buffer-id app)
                  (clatter.core.model:buffer-id buf))
            (clatter.core.model:mark-dirty app :buflist :chat :status))))))
  t)

;;; /raw command - send raw IRC line
(defclass raw-command (irc-command)
  ()
  (:default-initargs
   :name "RAW"
   :help "/raw line - Send raw IRC command"
   :min-args 1))

(defmethod execute-cmd ((command raw-command) app conn args)
  (declare (ignore app))
  (when (> (length args) 0)
    (clatter.net.irc:irc-send conn args))
  t)

;;; /whois nick
(defclass whois-command (irc-command)
  ()
  (:default-initargs
   :name "WHOIS"
   :help "/whois nick - Query user info"
   :min-args 1))

(defmethod execute-cmd ((command whois-command) app conn args)
  (declare (ignore app))
  (when (> (length args) 0)
    (multiple-value-bind (nick rest) (split-first-word args)
      (declare (ignore rest))
      (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-whois nick))))
  t)

;;; /topic [new topic]
(defclass topic-command (irc-command)
  ()
  (:default-initargs
   :name "TOPIC"
   :help "/topic [text] - View or set channel topic"))

(defmethod execute-cmd ((command topic-command) app conn args)
  (let* ((buf (clatter.core.model:active-buffer app))
         (target (if buf (clatter.core.model:buffer-title buf) "")))
    (when (> (length target) 0)
      (if (> (length args) 0)
          (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-topic target args))
          (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-topic target)))))
  t)

;;; /kick nick [reason]
(defclass kick-command (irc-command)
  ()
  (:default-initargs
   :name "KICK"
   :help "/kick nick [reason] - Kick user from channel"
   :min-args 1))

(defmethod execute-cmd ((command kick-command) app conn args)
  (when (> (length args) 0)
    (let* ((buf (clatter.core.model:active-buffer app))
           (channel (if buf (clatter.core.model:buffer-title buf) "")))
      (when (and (> (length channel) 0) (char= (char channel 0) #\#))
        (multiple-value-bind (nick reason) (split-first-word args)
          (clatter.net.irc:irc-send conn
            (if (> (length reason) 0)
                (clatter.core.protocol:irc-kick channel nick reason)
                (clatter.core.protocol:irc-kick channel nick)))))))
  t)

;;; /away [message]
(defclass away-command (irc-command)
  ()
  (:default-initargs
   :name "AWAY"
   :help "/away [message] - Set or clear away status"))

(defmethod execute-cmd ((command away-command) app conn args)
  (let ((msg (if (> (length args) 0) (string-trim " " args) nil)))
    (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-away msg))
    (cmd-message app (if msg
                         (format nil "You are now away: ~a" msg)
                         "You are no longer away")))
  t)

;;; /back - clear away status
(defclass back-command (irc-command)
  ()
  (:default-initargs
   :name "BACK"
   :help "/back - Clear away status"))

(defmethod execute-cmd ((command back-command) app conn args)
  (declare (ignore args))
  (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-away nil))
  (cmd-message app "You are no longer away")
  t)

;;; /help - show help
(defclass help-command (irc-command)
  ()
  (:default-initargs
   :name "HELP"
   :help "/help - Show this help"))

(defmethod execute-cmd ((command help-command) app conn args)
  (declare (ignore conn args))
  (show-help-from-registry app)
  t)

(defun show-help-from-registry (app)
  "Generate help from registered commands."
  (let ((buf (clatter.core.model:current-buffer app))
        (commands nil))
    ;; Collect unique commands (skip aliases)
    (maphash (lambda (name class-sym)
               (let ((cmd (make-instance class-sym)))
                 (when (string= name (command-name cmd))
                   (push cmd commands))))
             *command-registry*)
    ;; Sort by name
    (setf commands (sort commands #'string< :key #'command-name))
    ;; Display
    (de.anvi.croatoan:submit
      (clatter.core.dispatch:deliver-message
       app buf
       (clatter.core.model:make-message :level :system :nick "*help*" :text "CLatter Commands:"))
      (dolist (cmd commands)
        (when (> (length (command-help cmd)) 0)
          (clatter.core.dispatch:deliver-message
           app buf
           (clatter.core.model:make-message :level :system :nick "*help*" :text (command-help cmd)))))
      (clatter.core.dispatch:deliver-message
       app buf
       (clatter.core.model:make-message :level :system :nick "*help*" :text ""))
      (clatter.core.dispatch:deliver-message
       app buf
       (clatter.core.model:make-message :level :system :nick "*help*" 
                                        :text "Keys: Ctrl-P/N buffers | Ctrl-U/D scroll | Ctrl-W split | Ctrl-L refresh")))))

;;; /ban nick
(defclass ban-command (irc-command)
  ()
  (:default-initargs
   :name "BAN"
   :help "/ban nick - Ban user (nick!*@*)"
   :min-args 1))

(defmethod execute-cmd ((command ban-command) app conn args)
  (when (> (length args) 0)
    (let* ((buf (clatter.core.model:active-buffer app))
           (channel (if buf (clatter.core.model:buffer-title buf) "")))
      (when (and (> (length channel) 0) (char= (char channel 0) #\#))
        (multiple-value-bind (nick rest) (split-first-word args)
          (declare (ignore rest))
          (let ((banmask (format nil "~a!*@*" nick)))
            (clatter.net.irc:irc-send conn
              (clatter.core.protocol:irc-mode channel "+b" banmask)))))))
  t)

;;; /unban nick
(defclass unban-command (irc-command)
  ()
  (:default-initargs
   :name "UNBAN"
   :help "/unban nick - Remove ban"
   :min-args 1))

(defmethod execute-cmd ((command unban-command) app conn args)
  (when (> (length args) 0)
    (let* ((buf (clatter.core.model:active-buffer app))
           (channel (if buf (clatter.core.model:buffer-title buf) "")))
      (when (and (> (length channel) 0) (char= (char channel 0) #\#))
        (multiple-value-bind (nick rest) (split-first-word args)
          (declare (ignore rest))
          (let ((banmask (format nil "~a!*@*" nick)))
            (clatter.net.irc:irc-send conn
              (clatter.core.protocol:irc-mode channel "-b" banmask)))))))
  t)

;;; /mode [modes]
(defclass mode-command (irc-command)
  ()
  (:default-initargs
   :name "MODE"
   :help "/mode [modes] - View or set modes"))

(defmethod execute-cmd ((command mode-command) app conn args)
  (let* ((buf (clatter.core.model:active-buffer app))
         (target (if buf (clatter.core.model:buffer-title buf) "")))
    (if (> (length args) 0)
        (if (char= (char args 0) #\#)
            (clatter.net.irc:irc-send conn (format nil "MODE ~a" args))
            (clatter.net.irc:irc-send conn (format nil "MODE ~a ~a" target args)))
        (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-mode target))))
  t)

;;; /ctcp target [command]
(defclass ctcp-command (irc-command)
  ()
  (:default-initargs
   :name "CTCP"
   :help "/ctcp nick [cmd] - Send CTCP (VERSION, PING, TIME)"
   :min-args 1))

(defmethod execute-cmd ((command ctcp-command) app conn args)
  (when (> (length args) 0)
    (multiple-value-bind (target rest) (split-first-word args)
      (let ((ctcp-cmd (if (> (length rest) 0)
                          (string-upcase rest)
                          "VERSION")))
        (let ((ctcp-msg (format nil "~C~A~C" (code-char 1) ctcp-cmd (code-char 1))))
          (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-privmsg target ctcp-msg)))
        (cmd-message app (format nil "CTCP ~a sent to ~a" ctcp-cmd target)))))
  t)

;;; /close - close current buffer
(defclass close-command (irc-command)
  ()
  (:default-initargs
   :name "CLOSE"
   :help "/close - Close current buffer"))

(defmethod execute-cmd ((command close-command) app conn args)
  (declare (ignore conn args))
  (let* ((buf (clatter.core.model:active-buffer app))
         (buf-id (when buf (clatter.core.model:buffer-id buf)))
         (title (when buf (clatter.core.model:buffer-title buf))))
    (cond
      ((not buf)
       (cmd-error app "No buffer to close"))
      ((= buf-id 0)
       (cmd-error app "Cannot close server buffer"))
      (t
       (setf (clatter.core.model:app-current-buffer-id app) 0)
       (clatter.core.model:mark-dirty app :buflist :chat :status)
       (clatter.core.model:remove-buffer app buf-id)
       (cmd-message app (format nil "Closed buffer: ~a" title)))))
  t)

;;; /autojoin [list|add|remove] [#channel]
(defclass autojoin-command (irc-command)
  ()
  (:default-initargs
   :name "AUTOJOIN"
   :help "/autojoin [add|remove] [#channel] - Manage autojoin"))

(defmethod execute-cmd ((command autojoin-command) app conn args)
  (declare (ignore conn))
  (handle-autojoin-command app args)
  t)

;;; /log - view/search logs
(defclass log-command (irc-command)
  ()
  (:default-initargs
   :name "LOG"
   :help "/log - View recent logs | /log search <pattern>"))

(defmethod execute-cmd ((command log-command) app conn args)
  (declare (ignore conn))
  (handle-log-command app args)
  t)

;;; /dcc - DCC commands
(defclass dcc-command (irc-command)
  ()
  (:default-initargs
   :name "DCC"
   :help "/dcc chat|send|list|accept|reject - DCC commands"))

(defmethod execute-cmd ((command dcc-command) app conn args)
  (handle-dcc-command app conn args)
  t)

;;; /crafterbin <file>
(defclass crafterbin-command (irc-command)
  ()
  (:default-initargs
   :name "CRAFTERBIN"
   :help "/crafterbin <file> - Upload file to crafterbin"))

(defmethod execute-cmd ((command crafterbin-command) app conn args)
  (declare (ignore conn))
  (handle-crafterbin-command app args)
  t)

;;; /ignore [nick]
(defclass ignore-command (irc-command)
  ()
  (:default-initargs
   :name "IGNORE"
   :help "/ignore [nick] - Toggle ignore or list ignored"))

(defmethod execute-cmd ((command ignore-command) app conn args)
  (declare (ignore conn))
  (handle-ignore-command app args)
  t)

;;; /unignore nick
(defclass unignore-command (irc-command)
  ()
  (:default-initargs
   :name "UNIGNORE"
   :help "/unignore nick - Remove nick from ignore list"
   :min-args 1))

(defmethod execute-cmd ((command unignore-command) app conn args)
  (declare (ignore conn))
  (when (> (length args) 0)
    (let ((nick (string-trim " " args)))
      (clatter.core.model:unignore-nick app nick)
      (cmd-message app (format nil "Unignored: ~a" nick))))
  t)

;;; /history [count]
(defclass history-command (irc-command)
  ()
  (:default-initargs
   :name "HISTORY"
   :help "/history [count] - Request chat history from server"))

(defmethod execute-cmd ((command history-command) app conn args)
  (let* ((buf (clatter.core.model:active-buffer app))
         (target (when buf (clatter.core.model:buffer-title buf)))
         (count (if (> (length args) 0)
                    (parse-integer (string-trim " " args) :junk-allowed t)
                    50)))
    (if (and target (not (eq (clatter.core.model:buffer-kind buf) :server)))
        (if (or (member "chathistory" (clatter.net.irc:irc-cap-enabled conn) :test #'string-equal)
                (member "draft/chathistory" (clatter.net.irc:irc-cap-enabled conn) :test #'string-equal))
            (progn
              (clatter.net.irc:irc-request-chathistory conn target :limit (or count 50))
              (cmd-message app (format nil "Requesting ~d messages of history..." (or count 50))))
            (cmd-error app "Server does not support chathistory"))
        (cmd-error app "Use /history in a channel or query buffer")))
  t)

;;; /invite nick [#channel]
(defclass invite-command (irc-command)
  ()
  (:default-initargs
   :name "INVITE"
   :help "/invite nick [#channel] - Invite user to channel"
   :min-args 1))

(defmethod execute-cmd ((command invite-command) app conn args)
  (when (> (length args) 0)
    (let* ((parts (uiop:split-string (string-trim " " args) :separator " "))
           (nick (first parts))
           (buf (clatter.core.model:active-buffer app))
           (channel (or (second parts)
                        (when (and buf (eq (clatter.core.model:buffer-kind buf) :channel))
                          (clatter.core.model:buffer-title buf)))))
      (if channel
          (progn
            (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-invite nick channel))
            (cmd-message app (format nil "Invited ~a to ~a" nick channel)))
          (cmd-error app "Usage: /invite nick [#channel]"))))
  t)

;;; /names [#channel]
(defclass names-command (irc-command)
  ()
  (:default-initargs
   :name "NAMES"
   :help "/names [#channel] - Refresh member list"))

(defmethod execute-cmd ((command names-command) app conn args)
  (let* ((buf (clatter.core.model:active-buffer app))
         (channel (if (> (length args) 0)
                      (string-trim " " args)
                      (when (and buf (eq (clatter.core.model:buffer-kind buf) :channel))
                        (clatter.core.model:buffer-title buf)))))
    (if channel
        (progn
          (let ((target-buf (clatter.core.model:find-buffer-by-title app channel)))
            (when target-buf
              (clrhash (clatter.core.model:buffer-members target-buf))))
          (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-names channel))
          (cmd-message app (format nil "Refreshing member list for ~a" channel)))
        (cmd-error app "Usage: /names [#channel]")))
  t)

;;; /members - show channel member list
(defclass members-command (irc-command)
  ()
  (:default-initargs
   :name "MEMBERS"
   :help "/members - Show channel member list"))

(defmethod execute-cmd ((command members-command) app conn args)
  (declare (ignore conn args))
  (let* ((buf (clatter.core.model:active-buffer app)))
    (if (and buf (eq (clatter.core.model:buffer-kind buf) :channel))
        (let* ((members (clatter.core.model:buffer-members buf))
               (nick-list nil))
          (maphash (lambda (nick val)
                     (declare (ignore val))
                     (push nick nick-list))
                   members)
          (setf nick-list (sort nick-list #'string-lessp))
          (cmd-message app (format nil "Members (~d): ~{~a~^, ~}"
                                   (length nick-list) nick-list)))
        (cmd-error app "Not in a channel buffer")))
  t)

;;; /list - list channels on server
(defclass list-command (irc-command)
  ()
  (:default-initargs
   :name "LIST"
   :help "/list [pattern] - List channels on server"))

(defmethod execute-cmd ((command list-command) app conn args)
  (declare (ignore app))
  (let ((pattern (string-trim " " args)))
    (if (> (length pattern) 0)
        (clatter.net.irc:irc-send conn (format nil "LIST ~a" pattern))
        (clatter.net.irc:irc-send conn "LIST")))
  t)

;;; /who - query user information
(defclass who-command (irc-command)
  ()
  (:default-initargs
   :name "WHO"
   :help "/who <nick|#channel|mask> - Query user information"
   :min-args 1))

(defmethod execute-cmd ((command who-command) app conn args)
  (let ((target (string-trim " " args)))
    (if (> (length target) 0)
        (clatter.net.irc:irc-send conn (format nil "WHO ~a" target))
        (cmd-error app "Usage: /who <nick|#channel|mask>")))
  t)

;;; /monitor - track users online/offline
(defclass monitor-command (irc-command)
  ()
  (:default-initargs
   :name "MONITOR"
   :help "/monitor [+|-|c|l|s] [nicks] - Track users online/offline"))

(defmethod execute-cmd ((command monitor-command) app conn args)
  (let* ((trimmed (string-trim " " args))
         (space-pos (position #\Space trimmed))
         (subcmd (string-upcase (if space-pos (subseq trimmed 0 space-pos) trimmed)))
         (rest (if space-pos (string-trim " " (subseq trimmed (1+ space-pos))) "")))
    (cond
      ((string= subcmd "+")
       (if (> (length rest) 0)
           (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-monitor-add rest))
           (cmd-error app "Usage: /monitor + nick1,nick2,...")))
      ((string= subcmd "-")
       (if (> (length rest) 0)
           (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-monitor-remove rest))
           (cmd-error app "Usage: /monitor - nick1,nick2,...")))
      ((or (string= subcmd "C") (string= subcmd "CLEAR"))
       (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-monitor-clear))
       (cmd-message app "Monitor list cleared"))
      ((or (string= subcmd "L") (string= subcmd "LIST") (string= subcmd ""))
       (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-monitor-list)))
      ((or (string= subcmd "S") (string= subcmd "STATUS"))
       (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-monitor-status)))
      (t
       (cmd-error app "Usage: /monitor [+|-|c|l|s] [nicks]"))))
  t)

;;;; ============================================================
;;;; Theme Command
;;;; ============================================================

(defclass theme-command (irc-command)
  ()
  (:default-initargs
   :name "THEME"
   :help "/theme [name] - List themes or switch to named theme (dark, light, solarized, minimal, ascii, rounded)"
   :min-args 0))

(defmethod execute-cmd ((command theme-command) app conn args)
  (declare (ignore conn))
  (if (or (null args) (zerop (length args)))
      ;; List available themes
      (let ((themes (clatter.ui.render:list-themes)))
        (cmd-message app (format nil "Available themes: ~{~a~^, ~}" 
                                (mapcar #'string-downcase themes)))
        (cmd-message app (format nil "Current theme: ~a" 
                                (type-of (clatter.ui.render:current-theme)))))
      ;; Switch theme
      (let ((theme-class (clatter.ui.render:find-theme args)))
        (if theme-class
            (progn
              (clatter.ui.render:set-theme theme-class)
              (clatter.core.model:mark-dirty app :layout :chat :buflist :status :input)
              (cmd-message app (format nil "Theme switched to: ~a" args)))
            (cmd-error app (format nil "Unknown theme: ~a. Use /theme to list available themes." args)))))
  t)

;;;; ============================================================
;;;; Command Registration
;;;; ============================================================

(defun register-all-commands ()
  "Register all command classes."
  (clrhash *command-registry*)
  (register-command 'join-command)
  (register-command 'part-command)
  (register-command 'msg-command)
  (register-command 'me-command)
  (register-command 'nick-command)
  (register-command 'quit-command)
  (register-command 'ns-command)
  (register-command 'cs-command)
  (register-command 'query-command)
  (register-command 'raw-command)
  (register-command 'whois-command)
  (register-command 'topic-command)
  (register-command 'kick-command)
  (register-command 'away-command)
  (register-command 'back-command)
  (register-command 'help-command)
  (register-command 'ban-command)
  (register-command 'unban-command)
  (register-command 'mode-command)
  (register-command 'ctcp-command)
  (register-command 'close-command)
  (register-command 'autojoin-command)
  (register-command 'log-command)
  (register-command 'dcc-command)
  (register-command 'crafterbin-command)
  (register-command 'ignore-command)
  (register-command 'unignore-command)
  (register-command 'history-command)
  (register-command 'invite-command)
  (register-command 'names-command)
  (register-command 'members-command)
  (register-command 'list-command)
  (register-command 'who-command)
  (register-command 'monitor-command)
  (register-command 'theme-command))

;; Register commands when file loads
(register-all-commands)
