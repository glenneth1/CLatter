(in-package #:clatter.ui.keymap)

(defun %set-current-buffer (app id)
  (setf (app-current-buffer-id app)
        (mod id (length (app-buffers app))))
  ;; Clear unread/highlight counts when viewing buffer
  (let ((buf (current-buffer app)))
    (setf (clatter.core.model:buffer-unread-count buf) 0)
    (setf (clatter.core.model:buffer-highlight-count buf) 0))
  (mark-dirty app :chat :buflist :status :input))

(defun %buf-next (app)
  (%set-current-buffer app (1+ (app-current-buffer-id app))))

(defun %buf-prev (app)
  (%set-current-buffer app (1- (app-current-buffer-id app))))

(defun %scroll-up (app &optional (n 5))
  (let ((buf (current-buffer app)))
    (incf (buffer-scroll-offset buf) n)
    (mark-dirty app :chat)))

(defun %scroll-down (app &optional (n 5))
  (let ((buf (current-buffer app)))
    (setf (buffer-scroll-offset buf)
          (max 0 (- (buffer-scroll-offset buf) n)))
    (mark-dirty app :chat)))

(defun %toggle-split (app)
  "Toggle split pane mode on/off."
  (let ((ui (app-ui app)))
    (if (ui-split-mode ui)
        ;; Turn off split
        (setf (ui-split-mode ui) nil)
        ;; Turn on split - use next buffer as second pane
        (let* ((next-id (mod (1+ (app-current-buffer-id app)) (length (app-buffers app))))
               (left-buf (aref (app-buffers app) (app-current-buffer-id app)))
               (right-buf (aref (app-buffers app) next-id)))
          (setf (ui-split-mode ui) :horizontal
                (ui-split-buffer-id ui) next-id
                (ui-active-pane ui) :left)
          ;; Clear unread counts for both visible buffers
          (setf (buffer-unread-count left-buf) 0
                (buffer-highlight-count left-buf) 0
                (buffer-unread-count right-buf) 0
                (buffer-highlight-count right-buf) 0)))
    ;; Recreate windows with new layout
    (clatter.ui.tui:create-layout-windows app (ui-screen ui))
    (mark-dirty app :layout :chat :buflist :status :input)))

(defun %split-set-right-buffer (app)
  "Set the right pane to show the current buffer selection."
  (let ((ui (app-ui app)))
    (when (ui-split-mode ui)
      (let* ((buf-id (app-current-buffer-id app))
             (buf (aref (app-buffers app) buf-id)))
        (setf (ui-split-buffer-id ui) buf-id)
        ;; Clear unread counts since buffer is now visible
        (setf (buffer-unread-count buf) 0
              (buffer-highlight-count buf) 0))
      (mark-dirty app :chat :buflist))))

(defun %swap-panes (app)
  "Swap the buffers shown in left and right panes."
  (let ((ui (app-ui app)))
    (when (ui-split-mode ui)
      (let ((left-id (app-current-buffer-id app))
            (right-id (ui-split-buffer-id ui)))
        (setf (app-current-buffer-id app) right-id
              (ui-split-buffer-id ui) left-id))
      (mark-dirty app :chat :buflist :status))))

(defun %toggle-active-pane (app)
  "Toggle which pane is active for input."
  (let ((ui (app-ui app)))
    (when (ui-split-mode ui)
      (setf (ui-active-pane ui)
            (if (eq (ui-active-pane ui) :left) :right :left))
      (mark-dirty app :chat :status))))

(defun install-keybindings (screen app)
  ;; quit with F10 or Esc-q (use /quit command as primary method)
  (de.anvi.croatoan:bind screen :key-f10 #'de.anvi.croatoan:exit-event-loop)
  (de.anvi.croatoan:bind screen (coerce '(#\Esc #\q) 'string) #'de.anvi.croatoan:exit-event-loop)

  ;; idle/tick: drain submit queue + redraw + check quit
  (de.anvi.croatoan:bind screen nil
    (lambda (obj event)
      (declare (ignore obj event))
      (de.anvi.croatoan:process)
      (when (app-quit-requested app)
        (de.anvi.croatoan:exit-event-loop screen))
      (when (dirty-p app)
        (render-frame app))))

  ;; basic editing - multiple backspace bindings for different terminals
  (de.anvi.croatoan:bind screen #\Backspace (lambda (o e) (declare (ignore o e)) (input-backspace app)))
  (de.anvi.croatoan:bind screen #\Rubout (lambda (o e) (declare (ignore o e)) (input-backspace app)))  ;; Rubout = DEL
  (de.anvi.croatoan:bind screen (code-char 127) (lambda (o e) (declare (ignore o e)) (input-backspace app)))  ;; DEL
  (de.anvi.croatoan:bind screen (code-char 8) (lambda (o e) (declare (ignore o e)) (input-backspace app)))    ;; Ctrl-H / BS
  (de.anvi.croatoan:bind screen :backspace (lambda (o e) (declare (ignore o e)) (input-backspace app)))
  (de.anvi.croatoan:bind screen :key-backspace (lambda (o e) (declare (ignore o e)) (input-backspace app)))
  (de.anvi.croatoan:bind screen :dc (lambda (o e) (declare (ignore o e)) (input-delete app)))
  (de.anvi.croatoan:bind screen :key-dc (lambda (o e) (declare (ignore o e)) (input-delete app)))  ;; Delete key
  (de.anvi.croatoan:bind screen :key-left (lambda (o e) (declare (ignore o e)) (input-move-left app)))
  (de.anvi.croatoan:bind screen :key-right (lambda (o e) (declare (ignore o e)) (input-move-right app)))
  (de.anvi.croatoan:bind screen :key-home (lambda (o e) (declare (ignore o e)) (input-move-home app)))
  (de.anvi.croatoan:bind screen :key-end (lambda (o e) (declare (ignore o e)) (input-move-end app)))
  (de.anvi.croatoan:bind screen :key-up (lambda (o e) (declare (ignore o e)) (input-history-prev app)))
  (de.anvi.croatoan:bind screen :key-down (lambda (o e) (declare (ignore o e)) (input-history-next app)))
  (de.anvi.croatoan:bind screen #\Newline (lambda (o e) (declare (ignore o e)) (input-submit-line app)))
  (de.anvi.croatoan:bind screen #\Return (lambda (o e) (declare (ignore o e)) (input-submit-line app)))
  (de.anvi.croatoan:bind screen #\Tab (lambda (o e) (declare (ignore o e)) (input-tab-complete app)))

  ;; buffer nav (WeeChat-ish fallbacks)
  (de.anvi.croatoan:bind screen :key-ppage (lambda (o e) (declare (ignore o e)) (%scroll-up app 10)))
  (de.anvi.croatoan:bind screen :key-npage (lambda (o e) (declare (ignore o e)) (%scroll-down app 10)))
  (de.anvi.croatoan:bind screen :ppage (lambda (o e) (declare (ignore o e)) (%scroll-up app 10)))
  (de.anvi.croatoan:bind screen :npage (lambda (o e) (declare (ignore o e)) (%scroll-down app 10)))
  (de.anvi.croatoan:bind screen (code-char 16) (lambda (o e) (declare (ignore o e)) (%buf-prev app)))  ;; Ctrl-P
  (de.anvi.croatoan:bind screen (code-char 14) (lambda (o e) (declare (ignore o e)) (%buf-next app)))  ;; Ctrl-N
  ;; Alt+Up/Down as alternative scroll (Ctrl-U/D)
  (de.anvi.croatoan:bind screen (code-char 21) (lambda (o e) (declare (ignore o e)) (%scroll-up app 10)))   ;; Ctrl-U scroll up
  (de.anvi.croatoan:bind screen (code-char 4) (lambda (o e) (declare (ignore o e)) (%scroll-down app 10)))  ;; Ctrl-D scroll down

  ;; Split pane controls
  (de.anvi.croatoan:bind screen (code-char 23) (lambda (o e) (declare (ignore o e)) (%toggle-split app)))  ;; Ctrl-W toggle split
  (de.anvi.croatoan:bind screen (code-char 18) (lambda (o e) (declare (ignore o e)) (%split-set-right-buffer app)))  ;; Ctrl-R set right pane
  (de.anvi.croatoan:bind screen (code-char 24) (lambda (o e) (declare (ignore o e)) (%swap-panes app)))  ;; Ctrl-X swap panes
  ;; Ctrl+] to toggle active pane (where input goes)
  (de.anvi.croatoan:bind screen (code-char 29) (lambda (o e) (declare (ignore o e)) (%toggle-active-pane app)))  ;; Ctrl+] toggle active pane

  ;; raw escape sequence binds (Ghostty/xterm/various terminals)
  (de.anvi.croatoan:bind screen (coerce '(#\Esc #\[ #\A) 'string) (lambda (o e) (declare (ignore o e)) (input-history-prev app)))
  (de.anvi.croatoan:bind screen (coerce '(#\Esc #\[ #\B) 'string) (lambda (o e) (declare (ignore o e)) (input-history-next app)))
  (de.anvi.croatoan:bind screen (coerce '(#\Esc #\[ #\D) 'string) (lambda (o e) (declare (ignore o e)) (input-move-left app)))
  (de.anvi.croatoan:bind screen (coerce '(#\Esc #\[ #\C) 'string) (lambda (o e) (declare (ignore o e)) (input-move-right app)))
  ;; Page Up/Down escape sequences - multiple variants for different terminals
  (de.anvi.croatoan:bind screen (coerce '(#\Esc #\[ #\5 #\~) 'string) (lambda (o e) (declare (ignore o e)) (%scroll-up app 10)))   ;; xterm
  (de.anvi.croatoan:bind screen (coerce '(#\Esc #\[ #\6 #\~) 'string) (lambda (o e) (declare (ignore o e)) (%scroll-down app 10))) ;; xterm
  (de.anvi.croatoan:bind screen (coerce '(#\Esc #\[ #\5 #\;) 'string) (lambda (o e) (declare (ignore o e)) (%scroll-up app 10)))   ;; variant
  (de.anvi.croatoan:bind screen (coerce '(#\Esc #\[ #\6 #\;) 'string) (lambda (o e) (declare (ignore o e)) (%scroll-down app 10))) ;; variant
  (de.anvi.croatoan:bind screen (coerce '(#\Esc #\O #\5 #\~) 'string) (lambda (o e) (declare (ignore o e)) (%scroll-up app 10)))   ;; application mode
  (de.anvi.croatoan:bind screen (coerce '(#\Esc #\O #\6 #\~) 'string) (lambda (o e) (declare (ignore o e)) (%scroll-down app 10))) ;; application mode

  ;; Default handler for all other events - handles printable characters
  ;; This catches any character the terminal sends, including Shift+letter -> uppercase
  (de.anvi.croatoan:bind screen t
    (lambda (o e)
      (declare (ignore o))
      (let ((key (de.anvi.croatoan:event-key e)))
        (when (and (characterp key)
                   (graphic-char-p key))
          (input-insert-char app key))))))
