(in-package #:clatter.app)

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
    
    ;; Apply UI settings from config
    (setf (clatter.core.model:ui-buflist-w (clatter.core.model:app-ui app))
          (clatter.core.config:config-buflist-width cfg))
    
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
    (let ((started-connections nil))
      (dolist (net-cfg networks)
        (when (clatter.core.config:network-config-autoconnect net-cfg)
          (push (start-irc-connection app (clatter.core.config:network-config-name net-cfg) net-cfg) started-connections)))
      
      ;; If no autoconnect networks, connect to first one
      (when (and networks (null started-connections))
        (push (start-irc-connection app (clatter.core.config:network-config-name (first networks)) (first networks)) started-connections))
      
      ;; Set config for command system
      (setf clatter.core.commands:*current-config* cfg)
      
      ;; Initialize DCC manager with first connection if any
      (when started-connections
        (clatter.net.dcc:make-dcc-manager app (first started-connections)))
      
      ;; enter UI (blocks until quit)
      (run-tui app)
      
      ;; Cleanup all connections on exit
      (maphash (lambda (name conn)
                 (declare (ignore name))
                 (ignore-errors (clatter.net.irc:irc-disconnect conn "CLatter closing")))
               (clatter.core.model:app-connections app)))
    
    app))
