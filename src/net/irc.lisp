(in-package #:clatter.net.irc)

;;; Real IRC client using usocket + cl+ssl

(defclass irc-connection ()
  ((network-config :initarg :network-config :accessor irc-network-config)
   (socket         :initform nil :accessor irc-socket)
   (stream         :initform nil :accessor irc-stream)
   (state          :initform :disconnected :accessor irc-state) ; :disconnected :connecting :registering :connected
   (nick           :initform nil :accessor irc-nick)
   (thread         :initform nil :accessor irc-thread)
   (app            :initarg :app :accessor irc-app)
   (network-id     :initarg :network-id :accessor irc-network-id)
   (write-lock     :initform (bordeaux-threads:make-lock "irc-write") :accessor irc-write-lock)
   ;; SASL state
   (sasl-state     :initform nil :accessor irc-sasl-state)  ; nil :requested :authenticating :done
   (cap-negotiating :initform nil :accessor irc-cap-negotiating)
   ;; Reconnection state
   (reconnect-enabled :initform t :accessor irc-reconnect-enabled)
   (reconnect-attempts :initform 0 :accessor irc-reconnect-attempts)
   (max-reconnect-delay :initform 300 :accessor irc-max-reconnect-delay)  ; 5 minutes max
   ;; Health monitoring
   (last-activity  :initform (get-universal-time) :accessor irc-last-activity)
   (ping-sent-time :initform nil :accessor irc-ping-sent-time)))

(defun make-irc-connection (app network-id network-config)
  (make-instance 'irc-connection
                 :app app
                 :network-id network-id
                 :network-config network-config))

(defun irc-send (conn line)
  "Send a line to the IRC server (thread-safe)."
  (bordeaux-threads:with-lock-held ((irc-write-lock conn))
    (let ((stream (irc-stream conn)))
      (when stream
        (write-string line stream)
        (write-char #\Return stream)
        (write-char #\Linefeed stream)
        (force-output stream)))))

(defun irc-connect (conn)
  "Establish connection to IRC server."
  (let* ((cfg (irc-network-config conn))
         (server (clatter.core.config:network-config-server cfg))
         (port (clatter.core.config:network-config-port cfg))
         (use-tls (clatter.core.config:network-config-tls cfg)))
    (setf (irc-state conn) :connecting)
    (handler-case
        (let ((sock (usocket:socket-connect server port :element-type '(unsigned-byte 8))))
          (setf (irc-socket conn) sock)
          (let ((raw-stream (usocket:socket-stream sock)))
            (setf (irc-stream conn)
                  (if use-tls
                      (cl+ssl:make-ssl-client-stream
                       raw-stream
                       :hostname server
                       :external-format :utf-8)
                      (flexi-streams:make-flexi-stream
                       raw-stream
                       :external-format :utf-8))))
          (setf (irc-state conn) :registering)
          t)
      (error (e)
        (setf (irc-state conn) :disconnected)
        (irc-log-error conn "Connection failed: ~a" e)
        nil))))

(defun irc-disconnect (conn &optional message)
  "Disconnect from IRC server."
  (when (irc-stream conn)
    (ignore-errors
      (when message
        (irc-send conn (clatter.core.protocol:irc-quit message))))
    (ignore-errors (close (irc-stream conn))))
  (when (irc-socket conn)
    (ignore-errors (usocket:socket-close (irc-socket conn))))
  (setf (irc-stream conn) nil
        (irc-socket conn) nil
        (irc-state conn) :disconnected))

(defun base64-encode (string)
  "Encode a string to base64."
  (cl-base64:string-to-base64-string string))

(defun irc-register (conn)
  "Send registration commands, with SASL if configured."
  (let* ((cfg (irc-network-config conn))
         (nick (clatter.core.config:network-config-nick cfg))
         (sasl (clatter.core.config:network-config-sasl cfg)))
    (setf (irc-nick conn) nick)
    (if (and sasl (clatter.core.config:get-network-password cfg))
        ;; Start CAP negotiation for SASL
        (progn
          (setf (irc-cap-negotiating conn) t)
          (irc-send conn "CAP LS 302"))
        ;; No SASL - normal registration
        (irc-send-registration conn))))

(defun irc-send-registration (conn)
  "Send NICK and USER commands."
  (let* ((cfg (irc-network-config conn))
         (nick (clatter.core.config:network-config-nick cfg))
         (username (or (clatter.core.config:network-config-username cfg) nick))
         (realname (clatter.core.config:network-config-realname cfg))
         (password (clatter.core.config:network-config-password cfg)))
    (when password
      (irc-send conn (clatter.core.protocol:irc-pass password)))
    (irc-send conn (clatter.core.protocol:irc-nick nick))
    (irc-send conn (clatter.core.protocol:irc-user username realname))))

(defun irc-sasl-authenticate (conn)
  "Send SASL PLAIN authentication."
  (let* ((cfg (irc-network-config conn))
         (nick (clatter.core.config:network-config-nick cfg))
         (password (clatter.core.config:get-network-password cfg))
         ;; SASL PLAIN format: \0username\0password (base64 encoded)
         (auth-string (format nil "~c~a~c~a" #\Nul nick #\Nul password))
         (encoded (base64-encode auth-string)))
    (setf (irc-sasl-state conn) :authenticating)
    (irc-send conn (format nil "AUTHENTICATE ~a" encoded))))

(defun irc-handle-cap (conn params)
  "Handle CAP response for SASL negotiation."
  (let ((subcommand (second params))
        (caps (third params)))
    (cond
      ;; CAP LS response - check for SASL support
      ((string-equal subcommand "LS")
       (if (search "sasl" caps :test #'char-equal)
           (progn
             (setf (irc-sasl-state conn) :requested)
             (irc-send conn "CAP REQ :sasl"))
           (progn
             (irc-log-system conn "Server does not support SASL")
             (irc-send conn "CAP END")
             (irc-send-registration conn))))
      ;; CAP ACK - SASL accepted, start authentication
      ((string-equal subcommand "ACK")
       (when (search "sasl" caps :test #'char-equal)
         (irc-send conn "AUTHENTICATE PLAIN")
         (irc-send-registration conn)))
      ;; CAP NAK - SASL rejected
      ((string-equal subcommand "NAK")
       (irc-log-system conn "SASL capability rejected")
       (irc-send conn "CAP END")
       (irc-send-registration conn)))))

(defun irc-log-error (conn format-string &rest args)
  "Log error to server buffer."
  (let ((app (irc-app conn))
        (text (apply #'format nil format-string args)))
    (de.anvi.croatoan:submit
      (let ((buf (clatter.core.model:find-buffer app 0)))
        (clatter.core.dispatch:deliver-message
         app buf
         (clatter.core.model:make-message :level :error :nick "*error*" :text text))))))

(defun irc-log-system (conn format-string &rest args)
  "Log system message to server buffer."
  (let ((app (irc-app conn))
        (text (apply #'format nil format-string args)))
    (de.anvi.croatoan:submit
      (let ((buf (clatter.core.model:find-buffer app 0)))
        (clatter.core.dispatch:deliver-message
         app buf
         (clatter.core.model:make-message :level :system :nick "*" :text text))))))

(defun irc-handle-message (conn msg)
  "Handle a parsed IRC message."
  (let ((command (clatter.core.protocol:irc-message-command msg))
        (params (clatter.core.protocol:irc-message-params msg))
        (prefix (clatter.core.protocol:irc-message-prefix msg)))
    (cond
      ;; PING -> PONG
      ((string= command "PING")
       (setf (irc-last-activity conn) (get-universal-time))
       (irc-send conn (clatter.core.protocol:irc-pong (or (first params) ""))))
      
      ;; PONG - response to our health check ping
      ((string= command "PONG")
       (setf (irc-last-activity conn) (get-universal-time))
       (setf (irc-ping-sent-time conn) nil))
      
      ;; CAP response - handle SASL negotiation
      ((string= command "CAP")
       (irc-handle-cap conn params))
      
      ;; AUTHENTICATE response
      ((string= command "AUTHENTICATE")
       (when (string= (first params) "+")
         (irc-sasl-authenticate conn)))
      
      ;; 903 RPL_SASLSUCCESS
      ((string= command "903")
       (setf (irc-sasl-state conn) :done)
       (irc-log-system conn "SASL authentication successful")
       (irc-send conn "CAP END"))
      
      ;; 904, 905 SASL failures
      ((or (string= command "904") (string= command "905"))
       (irc-log-error conn "SASL authentication failed: ~a" (second params))
       (irc-send conn "CAP END"))
      
      ;; 001 RPL_WELCOME - registration complete
      ((string= command "001")
       (setf (irc-state conn) :connected)
       (irc-log-system conn "Connected to ~a as ~a"
                       (clatter.core.config:network-config-server (irc-network-config conn))
                       (irc-nick conn))
       ;; NickServ identify if configured AND we didn't use SASL
       (let ((ns-pw (clatter.core.config:get-network-password (irc-network-config conn))))
         (when (and ns-pw (not (eq (irc-sasl-state conn) :done)))
           (irc-send conn (clatter.core.protocol:irc-privmsg "NickServ"
                                                             (format nil "IDENTIFY ~a" ns-pw)))))
       ;; Autojoin channels
       (let ((channels (clatter.core.config:network-config-autojoin (irc-network-config conn))))
         (dolist (ch channels)
           (irc-send conn (clatter.core.protocol:irc-join ch)))))
      
      ;; 433 ERR_NICKNAMEINUSE
      ((string= command "433")
       (let ((new-nick (concatenate 'string (irc-nick conn) "_")))
         (setf (irc-nick conn) new-nick)
         (irc-send conn (clatter.core.protocol:irc-nick new-nick))
         (irc-log-system conn "Nick in use, trying ~a" new-nick)))
      
      ;; PRIVMSG
      ((string= command "PRIVMSG")
       (let* ((target (first params))
              (raw-text (second params))
              (text (clatter.core.protocol:strip-irc-formatting raw-text))
              (parsed-prefix (clatter.core.protocol:parse-prefix prefix))
              (sender-nick (clatter.core.protocol:prefix-nick parsed-prefix)))
         ;; Check for CTCP (starts and ends with \x01)
         (if (and (> (length raw-text) 1)
                  (char= (char raw-text 0) (code-char 1))
                  (char= (char raw-text (1- (length raw-text))) (code-char 1)))
             (irc-handle-ctcp conn sender-nick raw-text)
             (irc-deliver-chat conn target sender-nick text))))
      
      ;; NOTICE
      ((string= command "NOTICE")
       (let* ((target (first params))
              (text (clatter.core.protocol:strip-irc-formatting (second params)))
              (parsed-prefix (clatter.core.protocol:parse-prefix prefix))
              (sender-nick (or (clatter.core.protocol:prefix-nick parsed-prefix) "*")))
         (irc-deliver-notice conn target sender-nick text)))
      
      ;; JOIN
      ((string= command "JOIN")
       (let* ((channel (first params))
              (parsed-prefix (clatter.core.protocol:parse-prefix prefix))
              (nick (clatter.core.protocol:prefix-nick parsed-prefix)))
         (if (string= nick (irc-nick conn))
             ;; We joined - create buffer
             (irc-create-channel-buffer conn channel)
             ;; Someone else joined
             (irc-deliver-join conn channel nick))))
      
      ;; PART
      ((string= command "PART")
       (let* ((channel (first params))
              (message (second params))
              (parsed-prefix (clatter.core.protocol:parse-prefix prefix))
              (nick (clatter.core.protocol:prefix-nick parsed-prefix)))
         (irc-deliver-part conn channel nick message)))
      
      ;; QUIT
      ((string= command "QUIT")
       (let* ((message (first params))
              (parsed-prefix (clatter.core.protocol:parse-prefix prefix))
              (nick (clatter.core.protocol:prefix-nick parsed-prefix)))
         (irc-deliver-quit conn nick message)))
      
      ;; 353 RPL_NAMREPLY - channel member list
      ((string= command "353")
       ;; params: nick = | @ channel :nick1 nick2 @nick3 +nick4 ...
       (let* ((channel (third params))
              (names-str (fourth params)))
         (when (and channel names-str)
           (irc-add-members conn channel names-str))))
      
      ;; 366 RPL_ENDOFNAMES - end of names list (ignore)
      ((string= command "366")
       nil)
      
      ;; Other numerics - log to server buffer
      ((every #'digit-char-p command)
       (let ((text (format nil "[~a] ~{~a~^ ~}" command (cdr params))))
         (irc-log-system conn "~a" text)))
      
      ;; Default - log unknown
      (t
       (irc-log-system conn "~a ~{~a~^ ~}" command params)))))

(defun irc-handle-ctcp (conn sender-nick raw-text)
  "Handle incoming CTCP request and send appropriate reply."
  ;; Strip the \x01 delimiters
  (let* ((ctcp-content (subseq raw-text 1 (1- (length raw-text))))
         (space-pos (position #\Space ctcp-content))
         (ctcp-cmd (string-upcase (if space-pos
                                      (subseq ctcp-content 0 space-pos)
                                      ctcp-content)))
         (ctcp-args (if space-pos
                        (subseq ctcp-content (1+ space-pos))
                        "")))
    (cond
      ;; ACTION - display as /me
      ((string= ctcp-cmd "ACTION")
       (let* ((app (irc-app conn))
              (buf (clatter.core.model:find-buffer app 0)))  ; server buffer for now
         (de.anvi.croatoan:submit
           (clatter.core.dispatch:deliver-message
            app buf
            (clatter.core.model:make-message :level :chat
                                             :nick (format nil "* ~a" sender-nick)
                                             :text ctcp-args)))))
      ;; VERSION - reply with client info
      ((string= ctcp-cmd "VERSION")
       (irc-send conn (clatter.core.protocol:irc-ctcp-reply
                       sender-nick "VERSION" "CLatter 0.0.1 - Common Lisp IRC Client"))
       (irc-log-system conn "CTCP VERSION from ~a" sender-nick))
      ;; PING - echo back the argument
      ((string= ctcp-cmd "PING")
       (irc-send conn (clatter.core.protocol:irc-ctcp-reply sender-nick "PING" ctcp-args))
       (irc-log-system conn "CTCP PING from ~a" sender-nick))
      ;; TIME - reply with current time
      ((string= ctcp-cmd "TIME")
       (let ((time-str (multiple-value-bind (sec min hour day month year)
                           (get-decoded-time)
                         (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
                                 year month day hour min sec))))
         (irc-send conn (clatter.core.protocol:irc-ctcp-reply sender-nick "TIME" time-str))
         (irc-log-system conn "CTCP TIME from ~a" sender-nick)))
      ;; Unknown CTCP - just log it
      (t
       (irc-log-system conn "CTCP ~a from ~a: ~a" ctcp-cmd sender-nick ctcp-args)))))

(defun irc-deliver-chat (conn target sender-nick text)
  "Deliver a PRIVMSG to the appropriate buffer."
  (let ((app (irc-app conn)))
    (de.anvi.croatoan:submit
      (let* ((buf (irc-find-or-create-buffer conn target))
             (highlight (search (irc-nick conn) text :test #'char-equal)))
        (clatter.core.dispatch:deliver-message
         app buf
         (clatter.core.model:make-message :level :chat :nick sender-nick :text text)
         :highlightp highlight)))))

(defun irc-deliver-notice (conn target sender-nick text)
  "Deliver a NOTICE to the appropriate buffer. CTCP replies go to server buffer."
  (let ((app (irc-app conn)))
    (de.anvi.croatoan:submit
      ;; Check if this is a CTCP reply (starts and ends with \x01)
      (let* ((is-ctcp (and (> (length text) 1)
                           (char= (char text 0) (code-char 1))
                           (char= (char text (1- (length text))) (code-char 1))))
             ;; Strip CTCP delimiters if present
             (display-text (if is-ctcp
                               (format nil "CTCP reply from ~a: ~a" 
                                       sender-nick 
                                       (subseq text 1 (1- (length text))))
                               text))
             ;; CTCP replies and notices to us go to server buffer
             (buf (if (or is-ctcp
                          (string= target "*") 
                          (string= target (irc-nick conn)))
                      (clatter.core.model:find-buffer app 0)
                      (irc-find-or-create-buffer conn target))))
        (clatter.core.dispatch:deliver-message
         app buf
         (clatter.core.model:make-message :level :notice :nick sender-nick :text display-text))))))

(defun irc-deliver-join (conn channel nick)
  "Deliver a JOIN notification and add nick to member list."
  (let ((app (irc-app conn)))
    (de.anvi.croatoan:submit
      (let ((buf (irc-find-buffer conn channel)))
        (when buf
          ;; Add nick to member list
          (setf (gethash nick (clatter.core.model:buffer-members buf)) t)
          (clatter.core.dispatch:deliver-message
           app buf
           (clatter.core.model:make-message :level :join :nick nick
                                            :text (format nil "~a has joined ~a" nick channel))))))))

(defun irc-deliver-part (conn channel nick message)
  "Deliver a PART notification and remove nick from member list."
  (let ((app (irc-app conn)))
    (de.anvi.croatoan:submit
      (let ((buf (irc-find-buffer conn channel)))
        (when buf
          ;; Remove nick from member list
          (remhash nick (clatter.core.model:buffer-members buf))
          (clatter.core.dispatch:deliver-message
           app buf
           (clatter.core.model:make-message :level :part :nick nick
                                            :text (format nil "~a has left ~a~@[ (~a)~]"
                                                          nick channel message))))))))

(defun irc-deliver-quit (conn nick message)
  "Deliver a QUIT notification and remove nick from all channel buffers."
  (declare (ignore message))
  (let ((app (irc-app conn)))
    (de.anvi.croatoan:submit
      ;; Remove nick from all channel buffers
      (loop for i from 1 below (length (clatter.core.model:app-buffers app))
            for buf = (aref (clatter.core.model:app-buffers app) i)
            when (gethash nick (clatter.core.model:buffer-members buf))
              do (remhash nick (clatter.core.model:buffer-members buf))))))

(defun irc-add-members (conn channel names-str)
  "Parse NAMES reply and add members to channel buffer."
  (let ((app (irc-app conn)))
    (declare (ignore app))
    (de.anvi.croatoan:submit
      (let ((buf (irc-find-buffer conn channel)))
        (when buf
          ;; Parse space-separated nicks, stripping @ + % prefixes
          (dolist (name (uiop:split-string names-str :separator " "))
            (when (> (length name) 0)
              (let ((nick (string-left-trim "@+%~&" name)))
                (when (> (length nick) 0)
                  (setf (gethash nick (clatter.core.model:buffer-members buf)) t))))))))))

(defun irc-find-buffer (conn target)
  "Find buffer for target (channel or nick)."
  (let* ((app (irc-app conn))
         (buffers (clatter.core.model:app-buffers app)))
    (loop for i from 0 below (length buffers)
          for buf = (aref buffers i)
          when (string-equal (clatter.core.model:buffer-title buf) target)
            return buf)))

(defun irc-find-or-create-buffer (conn target)
  "Find or create buffer for target."
  (or (irc-find-buffer conn target)
      (irc-create-channel-buffer conn target)))

(defun irc-create-channel-buffer (conn channel)
  "Create a new buffer for a channel."
  (let* ((app (irc-app conn))
         (kind (if (char= (char channel 0) #\#) :channel :query))
         (buf (clatter.core.model:make-buffer :id -1 :kind kind :title channel)))  ;; ID set in submit
    (de.anvi.croatoan:submit
      (let ((buffers (clatter.core.model:app-buffers app)))
        (setf (clatter.core.model:buffer-id buf) (length buffers))
        (vector-push-extend buf buffers)
        (clatter.core.model:mark-dirty app :buflist)))
    buf))

(defun irc-read-loop (conn)
  "Main read loop for IRC connection."
  (handler-case
      (loop while (and (irc-stream conn)
                       (not (eq (irc-state conn) :disconnected)))
            do (let ((line (read-line (irc-stream conn) nil nil)))
                 (if line
                     (let ((msg (clatter.core.protocol:parse-irc-line
                                 (string-right-trim '(#\Return #\Linefeed) line))))
                       (irc-handle-message conn msg))
                     ;; EOF
                     (progn
                       (irc-log-system conn "Connection closed by server")
                       (setf (irc-state conn) :disconnected)
                       (return)))))
    (error (e)
      (irc-log-error conn "Read error: ~a" e)
      (setf (irc-state conn) :disconnected))))

(defun reconnect-delay (attempts)
  "Calculate reconnect delay with exponential backoff. Returns seconds."
  (min 300 (expt 2 (min attempts 8))))  ; 1, 2, 4, 8, 16, 32, 64, 128, 256, 300 max

(defun irc-connection-loop (conn)
  "Main connection loop with auto-reconnect support."
  (loop
    (setf (irc-sasl-state conn) nil
          (irc-cap-negotiating conn) nil
          (irc-ping-sent-time conn) nil
          (irc-last-activity conn) (get-universal-time))
    (cond
      ((irc-connect conn)
       ;; Connected successfully - reset attempts
       (setf (irc-reconnect-attempts conn) 0)
       (irc-register conn)
       (irc-read-loop conn)
       (irc-disconnect conn))
      (t
       ;; Connection failed
       (incf (irc-reconnect-attempts conn))))
    ;; Check if we should reconnect
    (unless (irc-reconnect-enabled conn)
      (irc-log-system conn "Reconnection disabled, staying disconnected")
      (return))
    ;; Calculate delay and wait
    (let ((delay (reconnect-delay (irc-reconnect-attempts conn))))
      (irc-log-system conn "Reconnecting in ~a seconds..." delay)
      (sleep delay))))

(defun start-irc-connection (app network-id network-config)
  "Start an IRC connection in a background thread."
  (let ((conn (make-irc-connection app network-id network-config)))
    (setf (irc-thread conn)
          (bordeaux-threads:make-thread
           (lambda ()
             (irc-connection-loop conn))
           :name (format nil "irc-~a" (clatter.core.config:network-config-name network-config))))
    conn))

(defun irc-check-health (conn)
  "Check connection health. Call periodically from main thread.
   Sends a PING if idle too long, disconnects if PING times out."
  (when (eq (irc-state conn) :connected)
    (let* ((now (get-universal-time))
           (idle-time (- now (irc-last-activity conn)))
           (ping-timeout 120)   ; 2 minutes without activity -> send ping
           (pong-timeout 60))   ; 1 minute to respond to ping
      (cond
        ;; We sent a ping and it timed out
        ((and (irc-ping-sent-time conn)
              (> (- now (irc-ping-sent-time conn)) pong-timeout))
         (irc-log-error conn "Connection timed out (no PONG received)")
         (irc-disconnect conn))
        ;; Idle too long, send a ping
        ((and (null (irc-ping-sent-time conn))
              (> idle-time ping-timeout))
         (setf (irc-ping-sent-time conn) now)
         (irc-send conn (format nil "PING :clatter-~a" now)))))))
