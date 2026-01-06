(asdf:defsystem #:clatter
  :description "CLatter - WeeChat-style IRC TUI in Common Lisp"
  :author "Glenn Thompson"
  :license "MIT"
  :version "0.0.1"
  :depends-on (#:croatoan #:bordeaux-threads #:alexandria #:usocket #:cl+ssl #:flexi-streams #:cl-base64 #:cl-ppcre)
  :serial t
  :components
  ((:file "src/package")
   (:file "src/core/ring")
   (:file "src/core/model")
   (:file "src/core/config")
   (:file "src/core/protocol")
   (:file "src/core/events")
   (:file "src/core/logging")
   (:file "src/core/dispatch")
   (:file "src/net/client")
   (:file "src/net/irc")
   (:file "src/net/dcc")
   (:file "src/core/commands")
   (:file "src/ui/input")
   (:file "src/ui/render")
   (:file "src/ui/keymap")
   (:file "src/ui/tui")
   (:file "src/app")
   (:file "src/main")))
