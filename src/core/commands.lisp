(in-package #:clatter.core.commands)

;;; Command parsing and execution
;;; Commands start with / and are parsed here

(defvar *current-connection* nil "Current active IRC connection")
(defvar *current-config* nil "Current config object for saving autojoin etc")

(defun parse-command (line)
  "Parse a command line. Returns (command . args) or nil if not a command."
  (when (and (> (length line) 0) (char= (char line 0) #\/))
    (let* ((space-pos (position #\Space line))
           (cmd (string-upcase (subseq line 1 (or space-pos (length line)))))
           (args (if space-pos
                     (string-trim '(#\Space) (subseq line (1+ space-pos)))
                     "")))
      (cons cmd args))))

(defun split-first-word (str)
  "Split string into first word and rest."
  (let ((space-pos (position #\Space str)))
    (if space-pos
        (values (subseq str 0 space-pos)
                (string-trim '(#\Space) (subseq str (1+ space-pos))))
        (values str ""))))

(defun execute-command (app conn cmd args)
  "Execute a parsed command. Returns t if handled, nil otherwise."
  (cond
    ;; /join #channel [key]
    ((string= cmd "JOIN")
     (when (> (length args) 0)
       (multiple-value-bind (channel key) (split-first-word args)
         (clatter.net.irc:irc-send conn
           (if (> (length key) 0)
               (clatter.core.protocol:irc-join channel key)
               (clatter.core.protocol:irc-join channel)))
         ;; Auto-add to autojoin list
         (add-to-autojoin app channel)))
     t)
    
    ;; /part [#channel] [message]
    ((string= cmd "PART")
     (let* ((buf (clatter.core.model:active-buffer app))
            (target (clatter.core.model:buffer-title buf)))
       (if (> (length args) 0)
           (if (char= (char args 0) #\#)
               (multiple-value-bind (channel msg) (split-first-word args)
                 (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-part channel msg)))
               (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-part target args)))
           (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-part target))))
     t)
    
    ;; /msg target message
    ((or (string= cmd "MSG") (string= cmd "PRIVMSG"))
     (when (> (length args) 0)
       (multiple-value-bind (target text) (split-first-word args)
         (when (> (length text) 0)
           (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-privmsg target text))
           ;; Echo to query buffer
           (let ((buf (clatter.net.irc::irc-find-or-create-buffer conn target)))
             (de.anvi.croatoan:submit
               (clatter.core.dispatch:deliver-message
                app buf
                (clatter.core.model:make-message :level :chat
                                                 :nick (clatter.net.irc:irc-nick conn)
                                                 :text text)))))))
     t)
    
    ;; /me action
    ((string= cmd "ME")
     (let* ((buf (clatter.core.model:active-buffer app))
            (target (clatter.core.model:buffer-title buf)))
       (when (and (> (length args) 0) (> (length target) 0))
         (let ((action-text (format nil "~CACTION ~a~C" (code-char 1) args (code-char 1))))
           (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-privmsg target action-text))
           ;; Echo locally
           (de.anvi.croatoan:submit
             (clatter.core.dispatch:deliver-message
              app buf
              (clatter.core.model:make-message :level :chat
                                               :nick (format nil "* ~a" (clatter.net.irc:irc-nick conn))
                                               :text args))))))
     t)
    
    ;; /nick newnick
    ((string= cmd "NICK")
     (when (> (length args) 0)
       (multiple-value-bind (newnick rest) (split-first-word args)
         (declare (ignore rest))
         (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-nick newnick))))
     t)
    
    ;; /quit or /q [message]
    ((or (string= cmd "QUIT") (string= cmd "Q"))
     (when conn
       (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-quit
                                       (if (> (length args) 0) args "CLatter"))))
     ;; Signal quit by setting a flag - the TUI will check this
     (setf (clatter.core.model:app-quit-requested app) t)
     t)
    
    ;; /ns command - NickServ shortcut
    ((string= cmd "NS")
     (when (> (length args) 0)
       (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-privmsg "NickServ" args)))
     t)
    
    ;; /cs command - ChanServ shortcut
    ((string= cmd "CS")
     (when (> (length args) 0)
       (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-privmsg "ChanServ" args)))
     t)
    
    ;; /query nick - open query buffer
    ((string= cmd "QUERY")
     (when (> (length args) 0)
       (multiple-value-bind (nick rest) (split-first-word args)
         (declare (ignore rest))
         (clatter.net.irc::irc-find-or-create-buffer conn nick)))
     t)
    
    ;; /raw command - send raw IRC line
    ((string= cmd "RAW")
     (when (> (length args) 0)
       (clatter.net.irc:irc-send conn args))
     t)
    
    ;; /autojoin [list|add|remove] [#channel]
    ((string= cmd "AUTOJOIN")
     (handle-autojoin-command app args)
     t)
    
    ;; /help - show help
    ((string= cmd "HELP")
     (show-help app)
     t)
    
    ;; Unknown command
    (t nil)))

(defun get-current-network-config ()
  "Get the network config for the current connection."
  (when (and *current-connection* *current-config*)
    (clatter.net.irc:irc-network-config *current-connection*)))

(defun add-to-autojoin (app channel)
  "Add a channel to the autojoin list and save config."
  (let ((net-cfg (get-current-network-config)))
    (when net-cfg
      (let ((autojoin (clatter.core.config:network-config-autojoin net-cfg)))
        (unless (member channel autojoin :test #'string-equal)
          (setf (clatter.core.config:network-config-autojoin net-cfg)
                (append autojoin (list channel)))
          (clatter.core.config:save-config *current-config*)
          (de.anvi.croatoan:submit
            (clatter.core.dispatch:deliver-message
             app (clatter.core.model:current-buffer app)
             (clatter.core.model:make-message :level :system :nick "*"
                                              :text (format nil "Added ~a to autojoin" channel)))))))))

(defun remove-from-autojoin (app channel)
  "Remove a channel from the autojoin list and save config."
  (let ((net-cfg (get-current-network-config)))
    (when net-cfg
      (let ((autojoin (clatter.core.config:network-config-autojoin net-cfg)))
        (if (member channel autojoin :test #'string-equal)
            (progn
              (setf (clatter.core.config:network-config-autojoin net-cfg)
                    (remove channel autojoin :test #'string-equal))
              (clatter.core.config:save-config *current-config*)
              (de.anvi.croatoan:submit
                (clatter.core.dispatch:deliver-message
                 app (clatter.core.model:current-buffer app)
                 (clatter.core.model:make-message :level :system :nick "*"
                                                  :text (format nil "Removed ~a from autojoin" channel)))))
            (de.anvi.croatoan:submit
              (clatter.core.dispatch:deliver-message
               app (clatter.core.model:current-buffer app)
               (clatter.core.model:make-message :level :error :nick "*"
                                                :text (format nil "~a is not in autojoin list" channel)))))))))

(defun list-autojoin (app)
  "List all channels in the autojoin list."
  (let ((net-cfg (get-current-network-config)))
    (de.anvi.croatoan:submit
      (if net-cfg
          (let ((autojoin (clatter.core.config:network-config-autojoin net-cfg)))
            (clatter.core.dispatch:deliver-message
             app (clatter.core.model:current-buffer app)
             (clatter.core.model:make-message :level :system :nick "*"
                                              :text (if autojoin
                                                        (format nil "Autojoin: ~{~a~^, ~}" autojoin)
                                                        "Autojoin list is empty"))))
          (clatter.core.dispatch:deliver-message
           app (clatter.core.model:current-buffer app)
           (clatter.core.model:make-message :level :error :nick "*"
                                            :text "No active connection"))))))

(defun handle-autojoin-command (app args)
  "Handle /autojoin command with subcommands: list, add, remove."
  (if (= (length args) 0)
      (list-autojoin app)
      (multiple-value-bind (subcmd rest) (split-first-word args)
        (let ((subcmd (string-upcase subcmd)))
          (cond
            ((string= subcmd "LIST")
             (list-autojoin app))
            ((string= subcmd "ADD")
             (if (> (length rest) 0)
                 (multiple-value-bind (channel ignored) (split-first-word rest)
                   (declare (ignore ignored))
                   (add-to-autojoin app channel))
                 (de.anvi.croatoan:submit
                   (clatter.core.dispatch:deliver-message
                    app (clatter.core.model:current-buffer app)
                    (clatter.core.model:make-message :level :error :nick "*"
                                                     :text "Usage: /autojoin add #channel")))))
            ((string= subcmd "REMOVE")
             (if (> (length rest) 0)
                 (multiple-value-bind (channel ignored) (split-first-word rest)
                   (declare (ignore ignored))
                   (remove-from-autojoin app channel))
                 (de.anvi.croatoan:submit
                   (clatter.core.dispatch:deliver-message
                    app (clatter.core.model:current-buffer app)
                    (clatter.core.model:make-message :level :error :nick "*"
                                                     :text "Usage: /autojoin remove #channel")))))
            (t
             (de.anvi.croatoan:submit
               (clatter.core.dispatch:deliver-message
                app (clatter.core.model:current-buffer app)
                (clatter.core.model:make-message :level :error :nick "*"
                                                 :text "Usage: /autojoin [list|add|remove] [#channel]")))))))))

(defun show-help (app)
  "Display help message in current buffer."
  (let ((buf (clatter.core.model:current-buffer app))
        (help-lines '("CLatter Commands:"
                      "/join #channel [key] - Join a channel (auto-adds to autojoin)"
                      "/part [#channel] [msg] - Leave channel"
                      "/autojoin - List autojoin channels"
                      "/autojoin add #channel - Add channel to autojoin"
                      "/autojoin remove #channel - Remove from autojoin"
                      "/msg target text - Send private message"
                      "/me action - Send action to current channel"
                      "/nick newnick - Change nickname"
                      "/quit [message] - Disconnect"
                      "/ns command - Send to NickServ"
                      "/cs command - Send to ChanServ"
                      "/query nick - Open query with user"
                      "/raw line - Send raw IRC command"
                      ""
                      "New to IRC? Register your nick:"
                      "  /ns register <password> <email>"
                      "Then verify via email link.")))
    (de.anvi.croatoan:submit
      (dolist (line help-lines)
        (clatter.core.dispatch:deliver-message
         app buf
         (clatter.core.model:make-message :level :system :nick "*help*" :text line))))))

(defun handle-input-line (app conn line)
  "Handle a line of input - either command or chat message."
  (let ((parsed (parse-command line)))
    (if parsed
        ;; It's a command
        (let ((cmd (car parsed))
              (args (cdr parsed)))
          (unless (execute-command app conn cmd args)
            ;; Unknown command - show error
            (de.anvi.croatoan:submit
              (clatter.core.dispatch:deliver-message
               app (clatter.core.model:current-buffer app)
               (clatter.core.model:make-message :level :error :nick "*"
                                                :text (format nil "Unknown command: /~a" cmd))))))
        ;; It's a chat message - send to active buffer's target (respects split pane)
        (let* ((buf (clatter.core.model:active-buffer app))
               (target (clatter.core.model:buffer-title buf))
               (kind (clatter.core.model:buffer-kind buf)))
          (when (and conn (member kind '(:channel :query)) (> (length target) 0))
            (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-privmsg target line))
            ;; Echo locally
            (de.anvi.croatoan:submit
              (clatter.core.dispatch:deliver-message
               app buf
               (clatter.core.model:make-message :level :chat
                                                :nick (clatter.net.irc:irc-nick conn)
                                                :text line))))))))
