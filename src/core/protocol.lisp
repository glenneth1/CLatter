(in-package #:clatter.core.protocol)

;;; IRC Protocol parsing and formatting
;;; Reference: RFC 1459, RFC 2812, IRCv3

;;; Input sanitization - prevent IRC command injection

(defun sanitize-irc-input (text)
  "Remove dangerous characters from user input.
   Removes CR, LF, and NUL to prevent IRC command injection."
  (when text
    (remove-if (lambda (ch)
                 (member (char-code ch) '(#x0D #x0A #x00)))
               text)))

(defun validate-irc-input (text &key (max-length 400))
  "Validate and sanitize user input before sending to IRC.
   Returns (values sanitized-text valid-p warning-message).
   - Removes CRLF/NUL characters
   - Truncates to max-length if needed"
  (cond
    ((or (null text) (zerop (length text)))
     (values "" nil "Empty message"))
    (t
     (let* ((sanitized (sanitize-irc-input text))
            (truncated (if (> (length sanitized) max-length)
                           (subseq sanitized 0 max-length)
                           sanitized))
            (warning (cond
                       ((not (string= text sanitized))
                        "Message contained invalid characters (removed)")
                       ((not (string= sanitized truncated))
                        (format nil "Message truncated to ~d characters" max-length))
                       (t nil))))
       (values truncated t warning)))))

;;; Channel name validation (RFC 2812)

(defun channel-prefix-p (char)
  "Check if CHAR is a valid IRC channel prefix.
   # - Normal channel
   & - Local channel (not propagated)
   + - Modeless channel
   ! - Safe channel (unique name)"
  (member char '(#\# #\& #\+ #\!) :test #'char=))

(defun channel-name-p (string)
  "Check if STRING looks like a channel name.
   Must start with channel prefix and be at least 2 chars."
  (and (stringp string)
       (> (length string) 1)
       (channel-prefix-p (char string 0))))

(defun valid-channel-name-p (name)
  "Validate channel name according to RFC 2812.
   - Starts with #&+!
   - No spaces, commas, or control-G (bell)
   - Max 50 characters (typical server limit)"
  (and (channel-name-p name)
       (<= (length name) 50)
       (notany (lambda (ch)
                 (or (char= ch #\Space)
                     (char= ch #\,)
                     (char= ch (code-char 7))))  ; ^G (bell)
               name)))

;;; IRC formatting code stripping

(defun strip-irc-formatting (text)
  "Remove IRC formatting codes: ^B (bold), ^C (color), ^O (reset), ^R (reverse), ^_ (underline)."
  (when text
    (let ((result (make-array (length text) :element-type 'character :fill-pointer 0 :adjustable t))
          (i 0)
          (len (length text)))
      (loop while (< i len) do
        (let ((ch (char text i)))
          (cond
            ;; ^B (0x02) bold, ^O (0x0F) reset, ^R (0x16) reverse, ^_ (0x1F) underline
            ((member (char-code ch) '(#x02 #x0F #x16 #x1D #x1F))
             (incf i))
            ;; ^C (0x03) color - skip color codes (up to 2 digits, comma, 2 more digits)
            ((= (char-code ch) #x03)
             (incf i)
             ;; skip foreground color (1-2 digits)
             (loop while (and (< i len) (digit-char-p (char text i)))
                   for count from 0 below 2
                   do (incf i))
             ;; skip comma and background color
             (when (and (< i len) (char= (char text i) #\,))
               (incf i)
               (loop while (and (< i len) (digit-char-p (char text i)))
                     for count from 0 below 2
                     do (incf i))))
            ;; normal character
            (t
             (vector-push-extend ch result)
             (incf i)))))
      (coerce result 'string))))

(defclass irc-message ()
  ((tags    :initarg :tags :accessor irc-message-tags :initform nil)
   (prefix  :initarg :prefix :accessor irc-message-prefix :initform nil)
   (command :initarg :command :accessor irc-message-command)
   (params  :initarg :params :accessor irc-message-params :initform nil)))

(defun make-irc-message (&key tags prefix command params)
  (make-instance 'irc-message :tags tags :prefix prefix :command command :params params))

(defun parse-prefix (prefix-str)
  "Parse nick!user@host prefix, returns (nick user host) or just (servername nil nil)."
  (when prefix-str
    (let ((bang (position #\! prefix-str))
          (at (position #\@ prefix-str)))
      (if (and bang at (< bang at))
          (list (subseq prefix-str 0 bang)
                (subseq prefix-str (1+ bang) at)
                (subseq prefix-str (1+ at)))
          (list prefix-str nil nil)))))

(defun prefix-nick (prefix)
  "Extract nick from parsed prefix."
  (first prefix))

(defun parse-irc-line (line)
  "Parse an IRC protocol line into an irc-message object.
   Returns a synthetic ERROR message if parsing fails."
  (handler-case
      (let ((pos 0)
            (len (length line))
            tags prefix command params)
        ;; Skip leading whitespace
        (loop while (and (< pos len) (char= (char line pos) #\Space)) do (incf pos))
        
        ;; Parse tags (IRCv3) - starts with @
        (when (and (< pos len) (char= (char line pos) #\@))
          (incf pos)
          (let ((end (position #\Space line :start pos)))
            (when end
              (setf tags (subseq line pos end))
              (setf pos (1+ end)))))
        
        ;; Skip whitespace
        (loop while (and (< pos len) (char= (char line pos) #\Space)) do (incf pos))
        
        ;; Parse prefix - starts with :
        (when (and (< pos len) (char= (char line pos) #\:))
          (incf pos)
          (let ((end (position #\Space line :start pos)))
            (when end
              (setf prefix (subseq line pos end))
              (setf pos (1+ end)))))
        
        ;; Skip whitespace
        (loop while (and (< pos len) (char= (char line pos) #\Space)) do (incf pos))
        
        ;; Parse command
        (let ((end (or (position #\Space line :start pos) len)))
          (setf command (string-upcase (subseq line pos end)))
          (setf pos end))
        
        ;; Validate we got a command
        (when (or (null command) (zerop (length command)))
          (error "No command in IRC line"))
        
        ;; Parse params
        (loop while (< pos len) do
          ;; Skip whitespace
          (loop while (and (< pos len) (char= (char line pos) #\Space)) do (incf pos))
          (when (< pos len)
            (if (char= (char line pos) #\:)
                ;; Trailing param (rest of line)
                (progn
                  (push (subseq line (1+ pos)) params)
                  (setf pos len))
                ;; Regular param
                (let ((end (or (position #\Space line :start pos) len)))
                  (push (subseq line pos end) params)
                  (setf pos end)))))
        
        (make-irc-message :tags tags
                          :prefix prefix
                          :command command
                          :params (nreverse params)))
    ;; Catch parsing errors - return synthetic error message
    (error (e)
      (format *error-output* "~&[PARSE-ERROR] ~a~%  Line: ~s~%" e line)
      (make-irc-message 
       :command "ERROR"
       :params (list (format nil "Parse error: ~a" e))
       :prefix "clatter-internal"))))

(defun parse-irc-tags (tags-string)
  "Parse IRCv3 tags string into an alist of (key . value) pairs.
   Tags format: key1=value1;key2=value2;key3"
  (when tags-string
    (mapcar (lambda (tag)
              (let ((eq-pos (position #\= tag)))
                (if eq-pos
                    (cons (subseq tag 0 eq-pos)
                          (subseq tag (1+ eq-pos)))
                    (cons tag nil))))
            (uiop:split-string tags-string :separator ";"))))

(defun get-server-time (tags-string)
  "Extract server-time from IRCv3 tags and convert to universal-time.
   Format: time=2024-01-08T12:34:56.789Z"
  (let ((tags (parse-irc-tags tags-string)))
    (when tags
      (let ((time-tag (cdr (assoc "time" tags :test #'string=))))
        (when time-tag
          (parse-iso8601-time time-tag))))))

(defun parse-iso8601-time (time-string)
  "Parse ISO8601 timestamp to universal-time. Returns nil on failure."
  (handler-case
      (let* ((year (parse-integer (subseq time-string 0 4)))
             (month (parse-integer (subseq time-string 5 7)))
             (day (parse-integer (subseq time-string 8 10)))
             (hour (parse-integer (subseq time-string 11 13)))
             (minute (parse-integer (subseq time-string 14 16)))
             (second (parse-integer (subseq time-string 17 19))))
        (encode-universal-time second minute hour day month year 0))
    (error () nil)))

;;; IRC line length limits (RFC 2812: 512 bytes including CRLF)

(defparameter +irc-max-line-length+ 510
  "Maximum IRC line length (512 - 2 for CRLF)")

(defparameter +irc-safe-message-length+ 400
  "Safe message length accounting for prefix/command overhead.
   Worst case: :nick!~user@longest.hostname.example.com PRIVMSG #channel :")

(defun message-overhead (command target)
  "Calculate overhead bytes for a PRIVMSG/NOTICE.
   Format: :nick!user@host COMMAND target :text"
  (+ 1              ; leading :
     30             ; nick (max typical)
     1              ; !
     10             ; user (~xxxxxxxx)
     1              ; @
     63             ; hostname (max)
     1              ; space
     (length command)
     1              ; space
     (length target)
     2))            ; space :

(defun max-message-length (command target)
  "Calculate maximum safe message length for target."
  (- +irc-max-line-length+ (message-overhead command target)))

(defun split-long-message (target text &key (command "PRIVMSG"))
  "Split TEXT into multiple messages if needed.
   Returns list of message strings that fit within IRC limits."
  (let* ((max-len (max-message-length command target))
         (len (length text)))
    (if (<= len max-len)
        (list text)
        ;; Split on word boundaries if possible
        (let ((parts nil)
              (start 0))
          (loop while (< start len) do
            (let* ((end (min (+ start max-len) len))
                   (chunk (subseq text start end)))
              ;; Try to break on space if not at end
              (when (and (< end len)
                        (not (char= (char text end) #\Space)))
                (let ((space-pos (position #\Space chunk :from-end t)))
                  (when (and space-pos (> space-pos 0))
                    (setf end (+ start space-pos)
                          chunk (subseq text start end)))))
              (push chunk parts)
              (setf start (if (and (< end len) 
                                  (char= (char text end) #\Space))
                             (1+ end)
                             end))))
          (nreverse parts)))))

(defun format-irc-line (command &rest params)
  "Format an IRC command with params into a protocol line."
  (with-output-to-string (s)
    (write-string command s)
    (when params
      (let ((last (car (last params)))
            (rest (butlast params)))
        (dolist (p rest)
          (write-char #\Space s)
          (write-string p s))
        (write-char #\Space s)
        ;; Trailing param with : if it contains spaces or is empty
        (when (or (zerop (length last))
                  (find #\Space last)
                  (and (> (length last) 0) (char= (char last 0) #\:)))
          (write-char #\: s))
        (write-string last s)))))

;;; Common IRC commands

(defun irc-nick (nick)
  (format-irc-line "NICK" nick))

(defun irc-user (username realname)
  (format-irc-line "USER" username "0" "*" realname))

(defun irc-pass (password)
  (format-irc-line "PASS" password))

(defun irc-join (channel &optional key)
  (if key
      (format-irc-line "JOIN" channel key)
      (format-irc-line "JOIN" channel)))

(defun irc-part (channel &optional message)
  (if message
      (format-irc-line "PART" channel message)
      (format-irc-line "PART" channel)))

(defun irc-privmsg (target text)
  (format-irc-line "PRIVMSG" target text))

(defun irc-notice (target text)
  (format-irc-line "NOTICE" target text))

(defun irc-quit (&optional message)
  (if message
      (format-irc-line "QUIT" message)
      (format-irc-line "QUIT")))

(defun irc-pong (server)
  (format-irc-line "PONG" server))

(defun irc-ping (server)
  (format-irc-line "PING" server))

(defun irc-cap (subcommand &rest args)
  (apply #'format-irc-line "CAP" subcommand args))

(defun irc-whois (nick)
  (format-irc-line "WHOIS" nick))

(defun irc-topic (channel &optional new-topic)
  (if new-topic
      (format-irc-line "TOPIC" channel new-topic)
      (format-irc-line "TOPIC" channel)))

(defun irc-kick (channel nick &optional reason)
  (if reason
      (format-irc-line "KICK" channel nick reason)
      (format-irc-line "KICK" channel nick)))

(defun irc-mode (target &optional mode &rest args)
  (if mode
      (apply #'format-irc-line "MODE" target mode args)
      (format-irc-line "MODE" target)))

(defun irc-away (&optional message)
  "Set or clear away status. If MESSAGE is nil or empty, clears away."
  (if (and message (> (length message) 0))
      (format-irc-line "AWAY" message)
      (format-irc-line "AWAY")))

(defun irc-invite (nick channel)
  "Invite NICK to CHANNEL."
  (format-irc-line "INVITE" nick channel))

(defun irc-names (channel)
  "Request NAMES list for CHANNEL."
  (format-irc-line "NAMES" channel))

(defun irc-monitor-add (nicks)
  "Add NICKS (comma-separated string or list) to monitor list."
  (let ((nick-str (if (listp nicks)
                      (format nil "~{~a~^,~}" nicks)
                      nicks)))
    (format nil "MONITOR + ~a" nick-str)))

(defun irc-monitor-remove (nicks)
  "Remove NICKS from monitor list."
  (let ((nick-str (if (listp nicks)
                      (format nil "~{~a~^,~}" nicks)
                      nicks)))
    (format nil "MONITOR - ~a" nick-str)))

(defun irc-monitor-clear ()
  "Clear entire monitor list."
  "MONITOR C")

(defun irc-monitor-list ()
  "Request current monitor list."
  "MONITOR L")

(defun irc-monitor-status ()
  "Request online status of monitored nicks."
  "MONITOR S")

(defun irc-ctcp-reply (target command &optional text)
  "Format a CTCP reply (sent via NOTICE)."
  (let ((ctcp-text (if text
                       (format nil "~C~A ~A~C" (code-char 1) command text (code-char 1))
                       (format nil "~C~A~C" (code-char 1) command (code-char 1)))))
    (irc-notice target ctcp-text)))

(defun irc-tagmsg (target &rest tags)
  "Format a TAGMSG with client tags. TAGS is a plist of tag names and values.
   Example: (irc-tagmsg \"#channel\" \"+typing\" \"active\")"
  (let ((tag-str (with-output-to-string (s)
                   (write-char #\@ s)
                   (loop for (name value) on tags by #'cddr
                         for first = t then nil
                         do (unless first (write-char #\; s))
                            (write-string name s)
                            (when value
                              (write-char #\= s)
                              (write-string value s))))))
    (format nil "~a TAGMSG ~a" tag-str target)))

(defun irc-typing (target state)
  "Send typing indicator to TARGET. STATE is :active, :paused, or :done."
  (irc-tagmsg target "+typing" (string-downcase (symbol-name state))))

;;; Numeric reply codes

(defparameter +rpl-welcome+ "001")
(defparameter +rpl-yourhost+ "002")
(defparameter +rpl-created+ "003")
(defparameter +rpl-myinfo+ "004")
(defparameter +rpl-isupport+ "005")
(defparameter +rpl-namreply+ "353")
(defparameter +rpl-endofnames+ "366")
(defparameter +rpl-motd+ "372")
(defparameter +rpl-motdstart+ "375")
(defparameter +rpl-endofmotd+ "376")
(defparameter +rpl-topic+ "332")
(defparameter +rpl-topicwhotime+ "333")
(defparameter +err-nicknameinuse+ "433")

;; WHOIS reply codes
(defparameter +rpl-whoisuser+ "311")
(defparameter +rpl-whoisserver+ "312")
(defparameter +rpl-whoisoperator+ "313")
(defparameter +rpl-whoisidle+ "317")
(defparameter +rpl-endofwhois+ "318")
(defparameter +rpl-whoischannels+ "319")
(defparameter +rpl-whoisaccount+ "330")
