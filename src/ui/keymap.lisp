(in-package #:clatter.ui.keymap)

(defun %find-visual-position (app buffer-id)
  "Find the position of buffer-id in the visual order list."
  (let ((order (clatter.core.model:app-buffer-order app)))
    (position buffer-id order)))

(defun %get-visual-buffer-id (app visual-pos)
  "Get the buffer ID at the given visual position."
  (let ((order (clatter.core.model:app-buffer-order app)))
    (when (and order (>= visual-pos 0) (< visual-pos (length order)))
      (nth visual-pos order))))

(defun %set-current-buffer (app id)
  ;; Ensure we land on a valid (non-nil) buffer
  (let* ((buffers (app-buffers app))
         (len (length buffers))
         (valid-id (if (and (>= id 0) (< id len) (aref buffers id))
                       id
                       ;; Fallback to first buffer in visual order
                       (let ((order (clatter.core.model:app-buffer-order app)))
                         (if order (first order) 0)))))
    (setf (app-current-buffer-id app) valid-id)
    ;; Clear unread/highlight counts when viewing buffer
    (let ((buf (current-buffer app)))
      (when buf
        (setf (clatter.core.model:buffer-unread-count buf) 0)
        (setf (clatter.core.model:buffer-highlight-count buf) 0)
        ;; Auto-refresh member list for channels
        (when (eq (buffer-kind buf) :channel)
          (let ((conn (clatter.core.model:get-buffer-connection app buf)))
            (when conn
              (clatter.net.irc:irc-send conn 
                (format nil "NAMES ~a" (buffer-title buf))))))))
    (mark-dirty app :chat :buflist :status :input)))

(defun %buf-next (app)
  "Move to next buffer in visual order."
  (let* ((order (clatter.core.model:app-buffer-order app))
         (current-id (app-current-buffer-id app))
         (pos (%find-visual-position app current-id))
         (next-pos (if pos
                       (mod (1+ pos) (length order))
                       0))
         (next-id (%get-visual-buffer-id app next-pos)))
    (when next-id
      (%set-current-buffer app next-id))))

(defun %buf-prev (app)
  "Move to previous buffer in visual order."
  (let* ((order (clatter.core.model:app-buffer-order app))
         (current-id (app-current-buffer-id app))
         (pos (%find-visual-position app current-id))
         (prev-pos (if pos
                       (mod (1- pos) (length order))
                       0))
         (prev-id (%get-visual-buffer-id app prev-pos)))
    (when prev-id
      (%set-current-buffer app prev-id))))

(defun %scroll-up (app &optional (n 5))
  "Scroll up to see newer messages (decrease offset)."
  (let ((buf (current-buffer app)))
    (when buf
      (setf (buffer-scroll-offset buf)
            (max 0 (- (buffer-scroll-offset buf) n)))
      (mark-dirty app :chat))))

(defun %scroll-down (app &optional (n 5))
  "Scroll down to see older messages (increase offset)."
  (let ((buf (current-buffer app)))
    (when buf
      (incf (buffer-scroll-offset buf) n)
      (mark-dirty app :chat))))

(defun %toggle-nicklist (app)
  "Toggle nick list panel visibility."
  (let ((ui (app-ui app)))
    (setf (ui-nicklist-visible ui) (not (ui-nicklist-visible ui)))
    ;; Recreate windows with new layout
    (clatter.ui.tui:create-layout-windows app (ui-screen ui))
    (mark-dirty app :layout :chat :buflist :status :input :nicklist)))

(defun %toggle-split (app)
  "Toggle split pane mode on/off."
  (let ((ui (app-ui app)))
    (if (ui-split-mode ui)
        ;; Turn off split
        (setf (ui-split-mode ui) nil)
        ;; Turn on split - use next buffer in visual order as second pane
        (let* ((order (clatter.core.model:app-buffer-order app))
               (current-id (app-current-buffer-id app))
               (pos (%find-visual-position app current-id))
               (next-pos (if (and pos order) (mod (1+ pos) (length order)) 0))
               (next-id (if order (nth next-pos order) 0))
               (left-buf (clatter.core.model:find-buffer app current-id))
               (right-buf (clatter.core.model:find-buffer app next-id)))
          (when (and left-buf right-buf)
            (setf (ui-split-mode ui) :horizontal
                  (ui-split-buffer-id ui) next-id
                  (ui-active-pane ui) :left)
            ;; Clear unread counts for both visible buffers
            (setf (buffer-unread-count left-buf) 0
                  (buffer-highlight-count left-buf) 0
                  (buffer-unread-count right-buf) 0
                  (buffer-highlight-count right-buf) 0))))
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

  ;; Ctrl-L to force redraw/refresh (like in vim/less)
  (de.anvi.croatoan:bind screen (code-char 12)
    (lambda (o e)
      (declare (ignore o e))
      (clatter.ui.tui:create-layout-windows app (ui-screen (app-ui app)))
      (mark-dirty app :layout :chat :buflist :status :input)))

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
  ;; Emacs/readline-style cursor movement
  (de.anvi.croatoan:bind screen (code-char 1) (lambda (o e) (declare (ignore o e)) (input-move-home app)))   ;; Ctrl-A beginning
  (de.anvi.croatoan:bind screen (code-char 5) (lambda (o e) (declare (ignore o e)) (input-move-end app)))    ;; Ctrl-E end
  (de.anvi.croatoan:bind screen (code-char 2) (lambda (o e) (declare (ignore o e)) (input-move-left app)))   ;; Ctrl-B back
  (de.anvi.croatoan:bind screen (code-char 6) (lambda (o e) (declare (ignore o e)) (input-move-right app)))  ;; Ctrl-F forward
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
  ;; Ctrl-U/D as alternative scroll
  (de.anvi.croatoan:bind screen (code-char 21) (lambda (o e) (declare (ignore o e)) (%scroll-up app 10)))   ;; Ctrl-U scroll up
  (de.anvi.croatoan:bind screen (code-char 4) (lambda (o e) (declare (ignore o e)) (%scroll-down app 10)))  ;; Ctrl-D scroll down

  ;; Split pane controls
  (de.anvi.croatoan:bind screen (code-char 23) (lambda (o e) (declare (ignore o e)) (%toggle-split app)))  ;; Ctrl-W toggle split
  (de.anvi.croatoan:bind screen (code-char 18) (lambda (o e) (declare (ignore o e)) (%split-set-right-buffer app)))  ;; Ctrl-R set right pane
  (de.anvi.croatoan:bind screen (code-char 24) (lambda (o e) (declare (ignore o e)) (%swap-panes app)))  ;; Ctrl-X swap panes
  ;; Ctrl+] to toggle active pane (where input goes)
  (de.anvi.croatoan:bind screen (code-char 29) (lambda (o e) (declare (ignore o e)) (%toggle-active-pane app)))  ;; Ctrl+] toggle active pane
  ;; Ctrl-M (Enter is also Ctrl-M, so use Ctrl-O instead) to toggle nick list
  (de.anvi.croatoan:bind screen (code-char 15) (lambda (o e) (declare (ignore o e)) (%toggle-nicklist app)))  ;; Ctrl-O toggle nick list

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
