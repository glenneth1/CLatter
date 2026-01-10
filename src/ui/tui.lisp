(in-package #:clatter.ui.tui)

(defun create-layout-windows (app scr)
  "Create or recreate windows based on current split mode and nick list visibility.
Layout: input at top (row 0), status below (row 1), chat panes below that, optional nick list on right."
  (let* ((ui (app-ui app))
         (term-w (de.anvi.croatoan:width scr))
         (term-h (de.anvi.croatoan:height scr))
         (left-w (ui-buflist-w ui))
         (gap 1)
         (right-x (+ left-w gap))
         (nicklist-visible (ui-nicklist-visible ui))
         (nicklist-w (if nicklist-visible (ui-nicklist-w ui) 0))
         (right-w (- term-w right-x nicklist-w (if nicklist-visible 1 0)))  ;; gap before nicklist
         (chat-y 2)  ;; Chat starts at row 2 (after input and status)
         (chat-h (- term-h 2))  ;; Same height as before
         (split-p (ui-split-mode ui)))
    (setf (ui-term-w ui) term-w
          (ui-term-h ui) term-h)
    ;; Destroy old windows if they exist (use ncurses delwin directly)
    (when (ui-win-buflist ui) (de.anvi.ncurses:delwin (de.anvi.croatoan:winptr (ui-win-buflist ui))))
    (when (ui-win-chat ui) (de.anvi.ncurses:delwin (de.anvi.croatoan:winptr (ui-win-chat ui))))
    (when (ui-win-chat2 ui) (de.anvi.ncurses:delwin (de.anvi.croatoan:winptr (ui-win-chat2 ui))))
    (when (ui-win-nicklist ui) (de.anvi.ncurses:delwin (de.anvi.croatoan:winptr (ui-win-nicklist ui))))
    (when (ui-win-status ui) (de.anvi.ncurses:delwin (de.anvi.croatoan:winptr (ui-win-status ui))))
    (when (ui-win-input ui) (de.anvi.ncurses:delwin (de.anvi.croatoan:winptr (ui-win-input ui))))
    ;; Clear the screen to remove artifacts from old layout
    (de.anvi.croatoan:clear scr)
    ;; Create buffer list window (full height on left)
    (setf (ui-win-buflist ui)
          (make-instance 'de.anvi.croatoan:window
                         :position (list 0 0)
                         :dimensions (list term-h left-w)
                         :border nil))
    ;; Input window at top (row 0) - spans full width including nicklist area
    (setf (ui-win-input ui)
          (make-instance 'de.anvi.croatoan:window
                         :position (list 0 right-x)
                         :dimensions (list 1 (- term-w right-x))
                         :border nil))
    ;; Status bar below input (row 1) - spans full width including nicklist area
    (setf (ui-win-status ui)
          (make-instance 'de.anvi.croatoan:window
                         :position (list 1 right-x)
                         :dimensions (list 1 (- term-w right-x))
                         :border nil))
    ;; Create nick list window if visible
    (if nicklist-visible
        (setf (ui-win-nicklist ui)
              (make-instance 'de.anvi.croatoan:window
                             :position (list chat-y (- term-w nicklist-w))
                             :dimensions (list chat-h nicklist-w)
                             :border nil))
        (setf (ui-win-nicklist ui) nil))
    ;; Create chat window(s) based on split mode (starting at row 2)
    (if split-p
        ;; Split mode: two chat panes side by side
        (let* ((pane-w (floor (- right-w 1) 2))  ;; 1 char gap between panes
               (pane2-x (+ right-x pane-w 1)))
          (setf (ui-win-chat ui)
                (make-instance 'de.anvi.croatoan:window
                               :position (list chat-y right-x)
                               :dimensions (list chat-h pane-w)
                               :border nil))
          (setf (ui-win-chat2 ui)
                (make-instance 'de.anvi.croatoan:window
                               :position (list chat-y pane2-x)
                               :dimensions (list chat-h pane-w)
                               :border nil)))
        ;; Single pane mode
        (progn
          (setf (ui-win-chat ui)
                (make-instance 'de.anvi.croatoan:window
                               :position (list chat-y right-x)
                               :dimensions (list chat-h right-w)
                               :border nil))
          (setf (ui-win-chat2 ui) nil)))))

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
