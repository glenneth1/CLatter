(in-package #:clatter.app)

(defvar *connections* nil "List of active IRC connections")

(defun prompt-for-nick ()
  "Simple prompt for nick at startup (before TUI)."
  (format t "~%CLatter - IRC Client~%")
  (format t "Enter your nick: ")
  (force-output)
  (string-trim '(#\Space #\Tab #\Newline #\Return) (read-line)))

(defun start ()
  (let* ((cfg (load-config))
         (networks (config-networks cfg))
         (app (make-app)))
    
    ;; If no networks configured, prompt for nick and create default Libera config
    (when (null networks)
      (let* ((nick (prompt-for-nick))
             (net-cfg (default-libera-config nick)))
        (add-network-config cfg net-cfg)
        (save-config cfg)
        (setf networks (config-networks cfg))
        (format t "~%Saved config to ~~/.config/clatter/config.lisp~%")
        (format t "Connecting to Libera.Chat as ~a...~%" nick)))
    
    ;; Start connections for all autoconnect networks
    (dolist (net-cfg networks)
      (when (clatter.core.config:network-config-autoconnect net-cfg)
        (push (start-irc-connection app 0 net-cfg) *connections*)))
    
    ;; If no autoconnect networks, connect to first one
    (when (and networks (null *connections*))
      (push (start-irc-connection app 0 (first networks)) *connections*))
    
    ;; Set current connection and config for command system
    (setf clatter.core.commands:*current-config* cfg)
    (when *connections*
      (setf *current-connection* (first *connections*))
      ;; Initialize DCC manager with app and first connection
      (clatter.net.dcc:make-dcc-manager app (first *connections*)))
    
    ;; enter UI (blocks until quit)
    (run-tui app)
    
    ;; Cleanup connections on exit
    (dolist (conn *connections*)
      (ignore-errors (clatter.net.irc:irc-disconnect conn "CLatter closing")))
    
    app))
