(in-package #:clatter.core.constants)

;;;; ============================================================
;;;; CLatter Constants
;;;; ============================================================
;;;; Centralized configuration values for easy tuning.
;;;; All magic numbers should be defined here with documentation.

;;; ============================================================
;;; Buffer & UI Constants
;;; ============================================================

(defparameter +default-scrollback-capacity+ 4000
  "Default number of messages to keep in scrollback buffer per channel.")

(defparameter +default-buflist-width+ 28
  "Default width of buffer list panel in characters.")

(defparameter +default-nicklist-width+ 20
  "Default width of nick list panel in characters.")

(defparameter +max-recent-urls+ 50
  "Maximum number of URLs to track per buffer for /url command.")

;;; ============================================================
;;; Connection & Timing Constants
;;; ============================================================

(defparameter +health-check-interval+ 30
  "Seconds between connection health checks.")

(defparameter +typing-throttle-seconds+ 3
  "Minimum seconds between sending typing notifications.")

(defparameter +ping-timeout-seconds+ 120
  "Send PING after this many seconds of idle time.")

(defparameter +pong-timeout-seconds+ 60
  "Disconnect if PONG not received within this time after PING.")

(defparameter +reconnect-min-delay+ 1
  "Initial reconnection delay in seconds (exponential backoff from here).")

(defparameter +reconnect-max-delay+ 300
  "Maximum delay between reconnection attempts (5 minutes).")

;;; ============================================================
;;; IRC Protocol Constants (RFC 2812)
;;; ============================================================

(defparameter +irc-max-line-length+ 510
  "Maximum IRC line length (512 - 2 for CRLF per RFC 2812).")

(defparameter +irc-safe-message-length+ 400
  "Safe message length accounting for prefix/command overhead.
   Worst case: :nick!~user@longest.hostname.example.com PRIVMSG #channel :")

(defparameter +irc-max-channel-length+ 50
  "Maximum channel name length (typical server limit).")

;;; ============================================================
;;; DCC Constants
;;; ============================================================

(defparameter +dcc-port-range-start+ 49152
  "Start of port range for DCC listening sockets (ephemeral/dynamic port range).")

(defparameter +dcc-port-range-end+ 65535
  "End of port range for DCC listening sockets.")

(defparameter +dcc-timeout-seconds+ 120
  "Timeout in seconds for DCC connection attempts.")

(defparameter +dcc-buffer-size+ 4096
  "Buffer size in bytes for DCC file transfers.")

;;; ============================================================
;;; IRC Numeric Reply Codes
;;; ============================================================

;; Connection registration
(defparameter +rpl-welcome+ "001")
(defparameter +rpl-yourhost+ "002")
(defparameter +rpl-created+ "003")
(defparameter +rpl-myinfo+ "004")
(defparameter +rpl-isupport+ "005")

;; Channel info
(defparameter +rpl-namreply+ "353")
(defparameter +rpl-endofnames+ "366")
(defparameter +rpl-topic+ "332")
(defparameter +rpl-topicwhotime+ "333")

;; MOTD
(defparameter +rpl-motd+ "372")
(defparameter +rpl-motdstart+ "375")
(defparameter +rpl-endofmotd+ "376")

;; WHOIS replies
(defparameter +rpl-whoisuser+ "311")
(defparameter +rpl-whoisserver+ "312")
(defparameter +rpl-whoisoperator+ "313")
(defparameter +rpl-whoisidle+ "317")
(defparameter +rpl-endofwhois+ "318")
(defparameter +rpl-whoischannels+ "319")
(defparameter +rpl-whoisaccount+ "330")

;; Errors
(defparameter +err-nicknameinuse+ "433")

;;; ============================================================
;;; External Services
;;; ============================================================

(defparameter +crafterbin-url+ "https://crafterbin.glennstack.dev"
  "URL of the CrafterBin pastebin service.")

;;; ============================================================
;;; IRCv3 Capabilities
;;; ============================================================

(defparameter +wanted-capabilities+
  '("server-time" "away-notify" "multi-prefix" "account-notify" 
    "message-tags" "draft/typing" "typing"
    "batch" "draft/chathistory" "chathistory"
    "labeled-response" "extended-join")
  "IRCv3 capabilities to request from server.")
