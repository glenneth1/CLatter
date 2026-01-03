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
   #:app #:make-app #:app-ui #:app-buffers #:app-current-buffer-id #:app-dirty-flags #:app-quit-requested
   #:mark-dirty #:dirty-p #:clear-dirty
   #:buffer #:make-buffer #:buffer-id #:buffer-title #:buffer-kind #:buffer-scrollback
   #:buffer-unread-count #:buffer-highlight-count #:buffer-scroll-offset #:buffer-members
   #:ui-win-chat2 #:ui-split-mode #:ui-split-buffer-id #:ui-active-pane
   #:message #:make-message #:message-ts #:message-level #:message-nick #:message-text #:message-highlight
   #:ui-state #:make-ui-state #:ui-input #:ui-screen
   #:ui-win-buflist #:ui-win-chat #:ui-win-status #:ui-win-input
   #:ui-term-w #:ui-term-h #:ui-buflist-w
   #:input-state #:make-input-state #:input-text #:input-cursor #:input-history #:input-history-pos
   #:find-buffer #:current-buffer #:active-buffer))

(defpackage #:clatter.core.config
  (:use #:cl)
  (:export
   #:*config-dir* #:*config-file*
   #:network-config #:make-network-config
   #:network-config-name #:network-config-server #:network-config-port
   #:network-config-tls #:network-config-nick #:network-config-username
   #:network-config-realname #:network-config-password #:network-config-nickserv-pw
   #:network-config-autojoin #:network-config-autoconnect
   #:config #:make-config #:config-networks #:config-default-network
   #:load-config #:save-config #:find-network-config #:add-network-config
   #:default-libera-config))

(defpackage #:clatter.core.protocol
  (:use #:cl)
  (:export
   #:irc-message #:make-irc-message
   #:irc-message-tags #:irc-message-prefix #:irc-message-command #:irc-message-params
   #:parse-irc-line #:format-irc-line
   #:parse-prefix #:prefix-nick #:strip-irc-formatting
   #:irc-nick #:irc-user #:irc-pass #:irc-join #:irc-part
   #:irc-privmsg #:irc-notice #:irc-quit #:irc-pong #:irc-ping #:irc-cap
   #:+rpl-welcome+ #:+rpl-yourhost+ #:+rpl-created+ #:+rpl-myinfo+ #:+rpl-isupport+
   #:+rpl-namreply+ #:+rpl-endofnames+ #:+rpl-motd+ #:+rpl-motdstart+ #:+rpl-endofmotd+
   #:+rpl-topic+ #:+rpl-topicwhotime+ #:+err-nicknameinuse+))

(defpackage #:clatter.core.commands
  (:use #:cl)
  (:export
   #:*current-connection* #:*current-config*
   #:parse-command #:execute-command #:handle-input-line #:show-help))

(defpackage #:clatter.core.events
  (:use #:cl)
  (:export #:ev #:ev-type #:ev-plist))

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
                #:app #:app-ui #:app-buffers #:app-current-buffer-id
                #:buffer #:buffer-title #:buffer-unread-count #:buffer-highlight-count
                #:current-buffer #:buffer-scrollback #:buffer-scroll-offset
                #:input-text #:input-cursor
                #:dirty-p #:clear-dirty
                #:ui-win-buflist #:ui-win-chat #:ui-win-chat2 #:ui-win-status #:ui-win-input #:ui-input
                #:ui-split-mode #:ui-split-buffer-id #:ui-active-pane)
  (:import-from #:clatter.core.ring #:ring->list)
  (:export #:render-frame))

(defpackage #:clatter.ui.keymap
  (:use #:cl)
  (:import-from #:clatter.core.model
                #:app #:app-ui #:mark-dirty #:current-buffer
                #:app-buffers #:app-current-buffer-id #:buffer-scroll-offset #:dirty-p
                #:app-quit-requested #:ui-screen
                #:ui-split-mode #:ui-split-buffer-id #:ui-active-pane
                #:buffer-unread-count #:buffer-highlight-count)
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
                #:mark-dirty #:ui-screen #:ui-win-buflist #:ui-win-chat #:ui-win-chat2
                #:ui-win-status #:ui-win-input #:ui-term-w #:ui-term-h #:ui-buflist-w
                #:ui-split-mode #:ui-split-buffer-id #:ui-active-pane)
  (:import-from #:clatter.ui.render #:render-frame)
  (:import-from #:clatter.ui.keymap #:install-keybindings)
  (:export #:run-tui #:create-layout-windows))

(defpackage #:clatter.net.client
  (:use #:cl)
  (:import-from #:clatter.core.model #:find-buffer)
  (:import-from #:clatter.core.dispatch #:deliver-message)
  (:export #:start-demo-net-thread))

(defpackage #:clatter.net.irc
  (:use #:cl)
  (:export
   #:irc-connection #:make-irc-connection
   #:irc-connect #:irc-disconnect #:irc-send
   #:irc-state #:irc-nick #:irc-network-config
   #:start-irc-connection))

(defpackage #:clatter.app
  (:use #:cl)
  (:import-from #:clatter.core.model #:make-app #:app-buffers #:make-buffer)
  (:import-from #:clatter.core.config #:load-config #:save-config #:config-networks
                #:make-network-config #:default-libera-config #:add-network-config)
  (:import-from #:clatter.core.commands #:*current-connection*)
  (:import-from #:clatter.ui.tui #:run-tui)
  (:import-from #:clatter.net.irc #:start-irc-connection)
  (:export #:start))
