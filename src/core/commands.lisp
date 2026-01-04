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
       ;; Disable auto-reconnect before quitting
       (setf (clatter.net.irc:irc-reconnect-enabled conn) nil)
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
    
    ;; /whois nick - query user info
    ((string= cmd "WHOIS")
     (when (> (length args) 0)
       (multiple-value-bind (nick rest) (split-first-word args)
         (declare (ignore rest))
         (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-whois nick))))
     t)
    
    ;; /topic [new topic] - view or set channel topic
    ((string= cmd "TOPIC")
     (let* ((buf (clatter.core.model:active-buffer app))
            (target (clatter.core.model:buffer-title buf)))
       (if (> (length args) 0)
           ;; Set topic
           (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-topic target args))
           ;; View topic
           (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-topic target))))
     t)
    
    ;; /kick nick [reason] - kick user from channel
    ((string= cmd "KICK")
     (when (> (length args) 0)
       (let* ((buf (clatter.core.model:active-buffer app))
              (channel (clatter.core.model:buffer-title buf)))
         (when (and (> (length channel) 0) (char= (char channel 0) #\#))
           (multiple-value-bind (nick reason) (split-first-word args)
             (clatter.net.irc:irc-send conn
               (if (> (length reason) 0)
                   (clatter.core.protocol:irc-kick channel nick reason)
                   (clatter.core.protocol:irc-kick channel nick)))))))
     t)
    
    ;; /ban nick - ban user from channel (+b nick!*@*)
    ((string= cmd "BAN")
     (when (> (length args) 0)
       (let* ((buf (clatter.core.model:active-buffer app))
              (channel (clatter.core.model:buffer-title buf)))
         (when (and (> (length channel) 0) (char= (char channel 0) #\#))
           (multiple-value-bind (nick rest) (split-first-word args)
             (declare (ignore rest))
             ;; Simple ban mask: nick!*@*
             (let ((banmask (format nil "~a!*@*" nick)))
               (clatter.net.irc:irc-send conn
                 (clatter.core.protocol:irc-mode channel "+b" banmask)))))))
     t)
    
    ;; /unban nick - remove ban
    ((string= cmd "UNBAN")
     (when (> (length args) 0)
       (let* ((buf (clatter.core.model:active-buffer app))
              (channel (clatter.core.model:buffer-title buf)))
         (when (and (> (length channel) 0) (char= (char channel 0) #\#))
           (multiple-value-bind (nick rest) (split-first-word args)
             (declare (ignore rest))
             (let ((banmask (format nil "~a!*@*" nick)))
               (clatter.net.irc:irc-send conn
                 (clatter.core.protocol:irc-mode channel "-b" banmask)))))))
     t)
    
    ;; /mode [modes] - view or set channel/user modes
    ((string= cmd "MODE")
     (let* ((buf (clatter.core.model:active-buffer app))
            (target (clatter.core.model:buffer-title buf)))
       (if (> (length args) 0)
           ;; If args starts with # it's a channel, otherwise apply to current target
           (if (char= (char args 0) #\#)
               (clatter.net.irc:irc-send conn (format nil "MODE ~a" args))
               (clatter.net.irc:irc-send conn (format nil "MODE ~a ~a" target args)))
           ;; View modes
           (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-mode target))))
     t)
    
    ;; /ctcp target command - send CTCP query
    ((string= cmd "CTCP")
     (when (> (length args) 0)
       (multiple-value-bind (target rest) (split-first-word args)
         (let ((ctcp-cmd (if (> (length rest) 0)
                             (string-upcase rest)
                             "VERSION")))
           ;; Send CTCP request as PRIVMSG with \x01 delimiters
           (let ((ctcp-msg (format nil "~C~A~C" (code-char 1) ctcp-cmd (code-char 1))))
             (clatter.net.irc:irc-send conn (clatter.core.protocol:irc-privmsg target ctcp-msg)))
           (de.anvi.croatoan:submit
             (clatter.core.dispatch:deliver-message
              app (clatter.core.model:current-buffer app)
              (clatter.core.model:make-message :level :system :nick "*"
                                               :text (format nil "CTCP ~a sent to ~a" ctcp-cmd target)))))))
     t)
    
    ;; /log [search pattern] - view or search logs
    ((string= cmd "LOG")
     (handle-log-command app args)
     t)
    
    ;; Unknown command
    (t nil)))

(defun get-current-network-config ()
  "Get the network config for the current connection."
  (when (and *current-connection* *current-config*)
    (clatter.net.irc:irc-network-config *current-connection*)))

(defun handle-log-command (app args)
  "Handle /log command: view recent logs or search.
   /log - show recent logs for current buffer
   /log search <pattern> - search logs for pattern
   /log list - list all logged targets
   /log export [text|json|html] [path] - export logs"
  (let* ((buf (clatter.core.model:active-buffer app))
         (target (clatter.core.model:buffer-title buf))
         (kind (clatter.core.model:buffer-kind buf))
         (network clatter.core.logging:*current-network*))
    (cond
      ;; /log export [format] [path] - export logs
      ((and (>= (length args) 6)
            (string-equal (string-upcase (subseq args 0 6)) "EXPORT"))
       (if (member kind '(:channel :query))
           (let* ((rest (string-trim " " (subseq args 6)))
                  (parts (uiop:split-string rest :separator " "))
                  (format-str (string-upcase (or (first parts) "TEXT")))
                  (format-key (cond ((string= format-str "JSON") :json)
                                    ((string= format-str "HTML") :html)
                                    (t :text)))
                  (ext (case format-key (:json "json") (:html "html") (t "txt")))
                  (default-path (merge-pathnames 
                                 (format nil "~a-export.~a" 
                                         (clatter.core.logging::sanitize-target target) ext)
                                 (user-homedir-pathname)))
                  (output-path (if (second parts)
                                   (pathname (second parts))
                                   default-path))
                  (count (clatter.core.logging:export-logs network target format-key output-path)))
             (de.anvi.croatoan:submit
               (clatter.core.dispatch:deliver-message
                app buf
                (clatter.core.model:make-message 
                 :level :system :nick "*log*"
                 :text (if count
                           (format nil "Exported ~d lines to ~a" count (namestring output-path))
                           "No logs to export")))))
           (de.anvi.croatoan:submit
             (clatter.core.dispatch:deliver-message
              app buf
              (clatter.core.model:make-message :level :error :nick "*"
                                               :text "Use /log export in a channel or query buffer")))))
      ;; /log list - show all logged targets
      ((and (> (length args) 0)
            (string-equal (string-upcase (subseq args 0 (min 4 (length args)))) "LIST"))
       (let ((targets (clatter.core.logging:list-logged-targets network)))
         (de.anvi.croatoan:submit
           (clatter.core.dispatch:deliver-message
            app buf
            (clatter.core.model:make-message :level :system :nick "*log*"
                                             :text (if targets
                                                       (format nil "Logged targets: ~{~a~^, ~}" targets)
                                                       "No logs found"))))))
      ;; /log search <pattern> - search logs
      ((and (> (length args) 6)
            (string-equal (string-upcase (subseq args 0 6)) "SEARCH"))
       (let ((pattern (string-trim " " (subseq args 6))))
         (if (and (member kind '(:channel :query)) (> (length pattern) 0))
             (let ((results (clatter.core.logging:search-logs network target pattern)))
               (de.anvi.croatoan:submit
                 (if results
                     (progn
                       (clatter.core.dispatch:deliver-message
                        app buf
                        (clatter.core.model:make-message :level :system :nick "*log*"
                                                         :text (format nil "Search results for '~a' (~d matches):"
                                                                       pattern (length results))))
                       (dolist (result results)
                         (clatter.core.dispatch:deliver-message
                          app buf
                          (clatter.core.model:make-message :level :system :nick (car result)
                                                           :text (cdr result)))))
                     (clatter.core.dispatch:deliver-message
                      app buf
                      (clatter.core.model:make-message :level :system :nick "*log*"
                                                       :text (format nil "No matches for '~a'" pattern))))))
             (de.anvi.croatoan:submit
               (clatter.core.dispatch:deliver-message
                app buf
                (clatter.core.model:make-message :level :error :nick "*"
                                                 :text "Usage: /log search <pattern> (in a channel/query buffer)"))))))
      ;; /log - show recent logs for current buffer
      ((member kind '(:channel :query))
       (let ((lines (clatter.core.logging:read-recent-logs network target 50)))
         (de.anvi.croatoan:submit
           (if lines
               (progn
                 (clatter.core.dispatch:deliver-message
                  app buf
                  (clatter.core.model:make-message :level :system :nick "*log*"
                                                   :text (format nil "--- Recent logs for ~a ---" target)))
                 (dolist (line lines)
                   (clatter.core.dispatch:deliver-message
                    app buf
                    (clatter.core.model:make-message :level :system :nick "" :text line)))
                 (clatter.core.dispatch:deliver-message
                  app buf
                  (clatter.core.model:make-message :level :system :nick "*log*"
                                                   :text "--- End of logs ---")))
               (clatter.core.dispatch:deliver-message
                app buf
                (clatter.core.model:make-message :level :system :nick "*log*"
                                                 :text (format nil "No logs found for ~a" target)))))))
      ;; Not in a channel/query buffer
      (t
       (de.anvi.croatoan:submit
         (clatter.core.dispatch:deliver-message
          app buf
          (clatter.core.model:make-message :level :error :nick "*"
                                           :text "Use /log in a channel or query buffer, or /log list")))))))

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
                      "/msg target text - Send private message"
                      "/me action - Send action to current channel"
                      "/nick newnick - Change nickname"
                      "/quit [message] - Disconnect"
                      "/query nick - Open query with user"
                      "/whois nick - Query user info"
                      "/topic [text] - View or set channel topic"
                      "/kick nick [reason] - Kick user from channel"
                      "/ban nick - Ban user (nick!*@*)"
                      "/unban nick - Remove ban"
                      "/mode [modes] - View or set modes"
                      "/ctcp nick [cmd] - Send CTCP (VERSION, PING, TIME)"
                      "/log - View recent logs for current buffer"
                      "/log search <pattern> - Search logs"
                      "/log list - List all logged targets"
                      "/log export [text|json|html] - Export logs to file"
                      "/ns command - Send to NickServ"
                      "/cs command - Send to ChanServ"
                      "/autojoin [add|remove] [#channel] - Manage autojoin"
                      "/raw line - Send raw IRC command"
                      ""
                      "Keys: Ctrl-P/N buffers | Ctrl-U/D scroll | Ctrl-W split"
                      ""
                      "New to IRC? Register: /ns register <password> <email>")))
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
