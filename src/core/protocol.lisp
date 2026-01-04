(in-package #:clatter.core.protocol)

;;; IRC Protocol parsing and formatting
;;; Reference: RFC 1459, RFC 2812, IRCv3

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
  "Parse an IRC protocol line into an irc-message object."
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
                      :params (nreverse params))))

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

(defun irc-ctcp-reply (target command &optional text)
  "Format a CTCP reply (sent via NOTICE)."
  (let ((ctcp-text (if text
                       (format nil "~C~A ~A~C" (code-char 1) command text (code-char 1))
                       (format nil "~C~A~C" (code-char 1) command (code-char 1)))))
    (irc-notice target ctcp-text)))

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
