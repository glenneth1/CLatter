(defpackage #:clatter
  (:use #:cl)
  (:export #:main))

(defpackage #:clatter.core.ring
  (:use #:cl)
  (:export #:make-ring #:ring-push #:ring->list #:ring-count))

(defpackage #:clatter.core.model
  (:use #:cl)
  (:import-from #:clatter.core.ring #:make-ring #:ring-push #:ring->list #:ring-count)
  (:export
   #:app #:make-app #:app-ui #:app-buffers #:app-current-buffer-id #:app-buffer-order #:app-connections #:app-dirty-flags #:app-quit-requested
   #:mark-dirty #:dirty-p #:clear-dirty
   #:buffer #:server-buffer #:channel-buffer #:query-buffer #:dcc-buffer
   #:make-buffer #:buffer-id #:buffer-title #:buffer-kind #:buffer-network #:buffer-scrollback
   #:buffer-dcc-connection
   #:create-server-buffer
   #:buffer-unread-count #:buffer-highlight-count #:buffer-scroll-offset #:buffer-members #:buffer-typing-users #:get-typing-nicks
   #:buffer-channel-modes #:buffer-my-modes
   #:ui-win-chat2 #:ui-win-nicklist #:ui-split-mode #:ui-split-buffer-id #:ui-active-pane
   #:ui-nicklist-w #:ui-nicklist-visible
   #:message #:make-message #:message-ts #:message-level #:message-nick #:message-text #:message-highlight
   #:ui-state #:make-ui-state #:ui-input #:ui-screen
   #:ui-win-buflist #:ui-win-chat #:ui-win-status #:ui-win-input
   #:ui-term-w #:ui-term-h #:ui-buflist-w
   #:input-state #:make-input-state #:input-text #:input-cursor #:input-history #:input-history-pos
   #:find-buffer #:current-buffer #:active-buffer
   #:get-buffer-connection #:get-current-connection
   #:remove-buffer #:find-buffer-by-title
   #:app-ignore-list #:ignore-nick #:unignore-nick #:ignored-p #:list-ignored
   #:buffer-add-member #:buffer-remove-member #:buffer-has-member-p #:buffer-member-list
   #:app-buffers-list #:find-buffer-by-network #:create-buffer))

(defpackage #:clatter.core.config
  (:use #:cl)
  (:export
   #:*config-dir* #:*config-file*
   #:network-config #:make-network-config
   #:network-config-name #:network-config-server #:network-config-port
   #:network-config-tls #:network-config-nick #:network-config-username
   #:network-config-realname #:network-config-password #:network-config-nickserv-pw
   #:network-config-sasl #:network-config-client-cert #:network-config-autojoin #:network-config-autoconnect
   #:config #:make-config #:config-networks #:config-default-network #:config-time-format #:config-buflist-width
   #:load-config #:save-config #:find-network-config #:add-network-config
   #:default-libera-config #:get-network-password #:get-server-password #:lookup-authinfo))

(defpackage #:clatter.core.protocol
  (:use #:cl)
  (:export
   #:irc-message #:make-irc-message
   #:irc-message-tags #:irc-message-prefix #:irc-message-command #:irc-message-params
   #:parse-irc-line #:format-irc-line
   #:parse-irc-tags #:get-server-time #:parse-iso8601-time
   #:parse-prefix #:prefix-nick #:strip-irc-formatting
   #:irc-nick #:irc-user #:irc-pass #:irc-join #:irc-part
   #:irc-privmsg #:irc-notice #:irc-quit #:irc-pong #:irc-ping #:irc-cap
   #:irc-whois #:irc-topic #:irc-kick #:irc-mode #:irc-away #:irc-ctcp-reply
   #:irc-tagmsg #:irc-typing #:irc-invite #:irc-names
   #:irc-monitor-add #:irc-monitor-remove #:irc-monitor-clear #:irc-monitor-list #:irc-monitor-status
   #:+rpl-welcome+ #:+rpl-yourhost+ #:+rpl-created+ #:+rpl-myinfo+ #:+rpl-isupport+
   #:+rpl-namreply+ #:+rpl-endofnames+ #:+rpl-motd+ #:+rpl-motdstart+ #:+rpl-endofmotd+
   #:+rpl-topic+ #:+rpl-topicwhotime+ #:+err-nicknameinuse+
   #:+rpl-whoisuser+ #:+rpl-whoisserver+ #:+rpl-whoisoperator+
   #:+rpl-whoisidle+ #:+rpl-endofwhois+ #:+rpl-whoischannels+ #:+rpl-whoisaccount+))

(defpackage #:clatter.core.commands
  (:use #:cl)
  (:export
   #:*current-connection* #:*current-config*
   #:parse-command #:execute-command #:handle-input-line #:show-help))

(defpackage #:clatter.core.events
  (:use #:cl)
  (:export
   ;; Base class and generic function
   #:irc-event #:handle-event
   #:event-connection #:event-timestamp #:event-raw-message
   ;; Connection events
   #:connect-event #:event-server #:event-nick
   #:disconnect-event #:event-reason
   ;; Message events
   #:message-event #:event-sender #:event-target #:event-text #:event-server-time
   #:privmsg-event #:notice-event #:action-event
   ;; Channel events
   #:channel-event #:event-channel
   #:join-event #:event-account #:event-realname
   #:part-event #:event-message
   #:quit-event
   #:kick-event #:event-kicked-nick
   #:nick-event #:event-old-nick #:event-new-nick
   #:topic-event #:event-topic
   #:mode-event #:event-setter #:event-modes
   ;; Presence events
   #:away-event
   ;; CTCP events
   #:ctcp-event #:event-command #:event-args
   #:dcc-offer-event #:event-dcc-type #:event-filename #:event-ip #:event-port #:event-filesize
   ;; Server events
   #:names-event #:event-names
   #:numeric-event #:event-numeric #:event-params
   ;; Typing events
   #:typing-event #:event-state
   ;; Legacy compatibility
   #:ev #:ev-type #:ev-plist))

(defpackage #:clatter.core.logging
  (:use #:cl)
  (:export
   #:*log-base-dir* #:*logging-enabled* #:*current-network*
   #:log-message #:write-log-entry
   #:read-recent-logs #:search-logs #:list-logged-targets #:list-log-files
   #:export-logs #:export-logs-text #:export-logs-json #:export-logs-html))

(defpackage #:clatter.core.dispatch
  (:use #:cl)
  (:import-from #:clatter.core.model
                #:app #:app-ui #:app-current-buffer-id #:buffer #:buffer-id #:message
                #:mark-dirty #:current-buffer #:find-buffer
                #:buffer-scrollback #:buffer-unread-count #:buffer-highlight-count
                #:buffer-scroll-offset
                #:ui-split-mode #:ui-split-buffer-id)
  (:import-from #:clatter.core.ring #:ring-push)
  (:export #:apply-event #:deliver-message))

(defpackage #:clatter.ui.input
  (:use #:cl)
  (:import-from #:clatter.core.model
                #:app #:app-ui #:current-buffer #:mark-dirty
                #:input-state #:input-text #:input-cursor #:input-history #:input-history-pos
                #:ui-input)
  (:export
   #:input-insert-char #:input-backspace #:input-delete
   #:input-move-left #:input-move-right #:input-move-home #:input-move-end
   #:input-history-prev #:input-history-next
   #:input-submit-line #:input-set-text #:input-tab-complete))

(defpackage #:clatter.ui.render
  (:use #:cl)
  (:import-from #:clatter.core.model
                #:app #:app-ui #:app-buffers #:app-current-buffer-id #:app-buffer-order #:app-connections
                #:buffer #:buffer-title #:buffer-kind #:buffer-network #:buffer-unread-count #:buffer-highlight-count
                #:buffer-channel-modes #:buffer-my-modes #:buffer-members
                #:current-buffer #:buffer-scrollback #:buffer-scroll-offset
                #:input-text #:input-cursor
                #:dirty-p #:clear-dirty
                #:ui-win-buflist #:ui-win-chat #:ui-win-chat2 #:ui-win-nicklist #:ui-win-status #:ui-win-input #:ui-input
                #:ui-split-mode #:ui-split-buffer-id #:ui-active-pane
                #:ui-nicklist-w #:ui-nicklist-visible)
  (:import-from #:clatter.core.ring #:ring->list)
  (:export #:render-frame))

(defpackage #:clatter.ui.keymap
  (:use #:cl)
  (:import-from #:clatter.core.model
                #:app #:app-ui #:mark-dirty #:current-buffer
                #:app-buffers #:app-current-buffer-id #:buffer-scroll-offset #:dirty-p
                #:app-quit-requested #:ui-screen
                #:ui-split-mode #:ui-split-buffer-id #:ui-active-pane
                #:ui-nicklist-visible
                #:buffer-unread-count #:buffer-highlight-count
                #:buffer-kind #:buffer-title)
  (:import-from #:clatter.ui.input
                #:input-insert-char #:input-backspace #:input-delete
                #:input-move-left #:input-move-right #:input-move-home #:input-move-end
                #:input-history-prev #:input-history-next
                #:input-submit-line #:input-tab-complete)
  (:import-from #:clatter.ui.render #:render-frame)
  (:export #:install-keybindings))

(defpackage #:clatter.ui.tui
  (:use #:cl)
  (:import-from #:clatter.core.model #:app #:app-ui #:make-ui-state #:make-input-state
                #:mark-dirty #:ui-screen #:ui-win-buflist #:ui-win-chat #:ui-win-chat2 #:ui-win-nicklist
                #:ui-win-status #:ui-win-input #:ui-term-w #:ui-term-h #:ui-buflist-w
                #:ui-split-mode #:ui-split-buffer-id #:ui-active-pane
                #:ui-nicklist-w #:ui-nicklist-visible)
  (:import-from #:clatter.ui.render #:render-frame)
  (:import-from #:clatter.ui.keymap #:install-keybindings)
  (:export #:run-tui #:create-layout-windows))

(defpackage #:clatter.net.irc
  (:use #:cl)
  (:export
   #:irc-connection #:make-irc-connection
   #:irc-connect #:irc-disconnect #:irc-send
   #:irc-state #:irc-nick #:irc-network-config
   #:irc-reconnect-enabled
   #:irc-check-health
   #:irc-cap-enabled
   #:irc-send-typing
   #:irc-request-chathistory
   #:irc-send-labeled
   #:irc-app #:irc-network-id
   #:start-irc-connection))

(defpackage #:clatter.net.dcc
  (:use #:cl)
  (:export
   ;; Manager
   #:dcc-manager #:make-dcc-manager #:*dcc-manager*
   #:dcc-manager-add #:dcc-manager-remove #:dcc-manager-find
   #:dcc-manager-list #:dcc-manager-pending #:dcc-manager-connections
   ;; Connection classes
   #:dcc-connection #:dcc-chat #:dcc-send
   ;; Accessors
   #:dcc-id #:dcc-nick #:dcc-state #:dcc-direction #:dcc-buffer
   #:dcc-filename #:dcc-filesize #:dcc-bytes-transferred
   #:dcc-type-string #:dcc-status-string
   ;; Operations
   #:dcc-accept #:dcc-reject #:dcc-close
   #:dcc-chat-send
   #:find-dcc-connection-for-buffer
   ;; Initiating connections
   #:dcc-initiate-chat #:dcc-initiate-send
   ;; Handling offers
   #:dcc-handle-offer
   ;; Utilities
   #:ip-integer-to-string #:ip-string-to-integer))

(defpackage #:clatter.app
  (:use #:cl)
  (:import-from #:clatter.core.model #:make-app #:app-buffers #:make-buffer)
  (:import-from #:clatter.core.config #:load-config #:save-config #:config-networks
                #:make-network-config #:default-libera-config #:add-network-config)
  (:import-from #:clatter.core.commands #:*current-connection*)
  (:import-from #:clatter.ui.tui #:run-tui)
  (:import-from #:clatter.net.irc #:start-irc-connection)
  (:export #:start))
