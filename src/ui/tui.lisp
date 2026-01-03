(in-package #:clatter.ui.tui)

(defun create-layout-windows (app scr)
  "Create or recreate windows based on current split mode."
  (let* ((ui (app-ui app))
         (term-w (de.anvi.croatoan:width scr))
         (term-h (de.anvi.croatoan:height scr))
         (left-w (ui-buflist-w ui))
         (gap 1)
         (right-x (+ left-w gap))
         (right-w (- term-w right-x))
         (chat-h (- term-h 2))
         (split-p (ui-split-mode ui)))
    (setf (ui-term-w ui) term-w
          (ui-term-h ui) term-h)
    ;; Destroy old windows if they exist (use ncurses delwin directly)
    (when (ui-win-buflist ui) (de.anvi.ncurses:delwin (de.anvi.croatoan:winptr (ui-win-buflist ui))))
    (when (ui-win-chat ui) (de.anvi.ncurses:delwin (de.anvi.croatoan:winptr (ui-win-chat ui))))
    (when (ui-win-chat2 ui) (de.anvi.ncurses:delwin (de.anvi.croatoan:winptr (ui-win-chat2 ui))))
    (when (ui-win-status ui) (de.anvi.ncurses:delwin (de.anvi.croatoan:winptr (ui-win-status ui))))
    (when (ui-win-input ui) (de.anvi.ncurses:delwin (de.anvi.croatoan:winptr (ui-win-input ui))))
    ;; Create buffer list window
    (setf (ui-win-buflist ui)
          (make-instance 'de.anvi.croatoan:window
                         :position (list 0 0)
                         :dimensions (list term-h left-w)
                         :border nil))
    ;; Create chat window(s) based on split mode
    (if split-p
        ;; Split mode: two chat panes side by side
        (let* ((pane-w (floor (- right-w 1) 2))  ;; 1 char gap between panes
               (pane2-x (+ right-x pane-w 1)))
          (setf (ui-win-chat ui)
                (make-instance 'de.anvi.croatoan:window
                               :position (list 0 right-x)
                               :dimensions (list chat-h pane-w)
                               :border nil))
          (setf (ui-win-chat2 ui)
                (make-instance 'de.anvi.croatoan:window
                               :position (list 0 pane2-x)
                               :dimensions (list chat-h pane-w)
                               :border nil)))
        ;; Single pane mode
        (progn
          (setf (ui-win-chat ui)
                (make-instance 'de.anvi.croatoan:window
                               :position (list 0 right-x)
                               :dimensions (list chat-h right-w)
                               :border nil))
          (setf (ui-win-chat2 ui) nil)))
    ;; Status and input windows (always full width of right side)
    (setf (ui-win-status ui)
          (make-instance 'de.anvi.croatoan:window
                         :position (list chat-h right-x)
                         :dimensions (list 1 right-w)
                         :border nil))
    (setf (ui-win-input ui)
          (make-instance 'de.anvi.croatoan:window
                         :position (list (1+ chat-h) right-x)
                         :dimensions (list 1 right-w)
                         :border nil))))

(defun run-tui (app)
  (de.anvi.croatoan:with-screen (scr
                                :input-buffering nil
                                :input-blocking nil
                                :enable-function-keys t
                                :cursor-visible t
                                :frame-rate 30)
    ;; store screen
    (setf (ui-screen (app-ui app)) scr)
    ;; Disable ncurses echo - we handle display ourselves
    (de.anvi.ncurses:noecho)

    ;; Create initial layout
    (create-layout-windows app scr)

    (install-keybindings scr app)

    (mark-dirty app :layout :chat :buflist :status :input)
    (render-frame app)

    (de.anvi.croatoan:run-event-loop scr)))
