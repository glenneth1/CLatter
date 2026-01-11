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
  "Toggle split pane mode on/off (horizontal split)."
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

(defun %toggle-split-orientation (app)
  "Toggle split orientation between horizontal and vertical."
  (let ((ui (app-ui app)))
    (when (ui-split-mode ui)
      ;; Toggle between horizontal and vertical
      (setf (ui-split-mode ui)
            (if (eq (ui-split-mode ui) :horizontal) :vertical :horizontal))
      ;; Update active pane naming for vertical mode
      (when (eq (ui-split-mode ui) :vertical)
        (setf (ui-active-pane ui) 
              (if (eq (ui-active-pane ui) :left) :top :bottom)))
      (when (eq (ui-split-mode ui) :horizontal)
        (setf (ui-active-pane ui)
              (if (member (ui-active-pane ui) '(:top :left)) :left :right)))
      ;; Recreate windows with new layout
      (clatter.ui.tui:create-layout-windows app (ui-screen ui))
      (mark-dirty app :layout :chat :buflist :status :input))))

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
            (cond
              ;; Horizontal mode: toggle left/right
              ((eq (ui-split-mode ui) :horizontal)
               (if (member (ui-active-pane ui) '(:left :top)) :right :left))
              ;; Vertical mode: toggle top/bottom
              ((eq (ui-split-mode ui) :vertical)
               (if (member (ui-active-pane ui) '(:top :left)) :bottom :top))
              (t :left)))
      (mark-dirty app :chat :status))))

(defun install-keybindings (screen app)
  "Install keybindings using CLOS action dispatch.
   Actions are defined in key-actions.lisp and dispatched via execute-action."
  ;; quit with F10 or Esc-q (use /quit command as primary method)
  (de.anvi.croatoan:bind screen :key-f10 #'de.anvi.croatoan:exit-event-loop)
  (de.anvi.croatoan:bind screen (coerce '(#\Esc #\q) 'string) #'de.anvi.croatoan:exit-event-loop)

  ;; Ctrl-L to force redraw/refresh
  (de.anvi.croatoan:bind screen (code-char 12) (make-action-handler "redraw" app))

  ;; idle/tick: drain submit queue + redraw + check quit
  (de.anvi.croatoan:bind screen nil
    (lambda (obj event)
      (declare (ignore obj event))
      (de.anvi.croatoan:process)
      (when (app-quit-requested app)
        (de.anvi.croatoan:exit-event-loop screen))
      (when (dirty-p app)
        (render-frame app))))

  ;; Input editing - using CLOS actions
  (de.anvi.croatoan:bind screen #\Backspace (make-action-handler "backspace" app))
  (de.anvi.croatoan:bind screen #\Rubout (make-action-handler "backspace" app))
  (de.anvi.croatoan:bind screen (code-char 127) (make-action-handler "backspace" app))
  (de.anvi.croatoan:bind screen (code-char 8) (make-action-handler "backspace" app))
  (de.anvi.croatoan:bind screen :backspace (make-action-handler "backspace" app))
  (de.anvi.croatoan:bind screen :key-backspace (make-action-handler "backspace" app))
  (de.anvi.croatoan:bind screen :dc (make-action-handler "delete" app))
  (de.anvi.croatoan:bind screen :key-dc (make-action-handler "delete" app))
  (de.anvi.croatoan:bind screen :key-left (make-action-handler "move-left" app))
  (de.anvi.croatoan:bind screen :key-right (make-action-handler "move-right" app))
  (de.anvi.croatoan:bind screen :key-home (make-action-handler "move-home" app))
  (de.anvi.croatoan:bind screen :key-end (make-action-handler "move-end" app))
  ;; Emacs/readline-style cursor movement
  (de.anvi.croatoan:bind screen (code-char 1) (make-action-handler "move-home" app))   ;; Ctrl-A
  (de.anvi.croatoan:bind screen (code-char 5) (make-action-handler "move-end" app))    ;; Ctrl-E
  (de.anvi.croatoan:bind screen (code-char 2) (make-action-handler "move-left" app))   ;; Ctrl-B
  (de.anvi.croatoan:bind screen (code-char 6) (make-action-handler "move-right" app))  ;; Ctrl-F
  (de.anvi.croatoan:bind screen :key-up (make-action-handler "history-prev" app))
  (de.anvi.croatoan:bind screen :key-down (make-action-handler "history-next" app))
  (de.anvi.croatoan:bind screen #\Newline (make-action-handler "submit" app))
  (de.anvi.croatoan:bind screen #\Return (make-action-handler "submit" app))
  (de.anvi.croatoan:bind screen #\Tab (make-action-handler "tab-complete" app))

  ;; Buffer navigation - using CLOS actions
  (de.anvi.croatoan:bind screen :key-ppage (make-action-handler "scroll-up" app))
  (de.anvi.croatoan:bind screen :key-npage (make-action-handler "scroll-down" app))
  (de.anvi.croatoan:bind screen :ppage (make-action-handler "scroll-up" app))
  (de.anvi.croatoan:bind screen :npage (make-action-handler "scroll-down" app))
  (de.anvi.croatoan:bind screen (code-char 16) (make-action-handler "buffer-prev" app))  ;; Ctrl-P
  (de.anvi.croatoan:bind screen (code-char 14) (make-action-handler "buffer-next" app))  ;; Ctrl-N
  (de.anvi.croatoan:bind screen (code-char 21) (make-action-handler "scroll-up" app))    ;; Ctrl-U
  (de.anvi.croatoan:bind screen (code-char 4) (make-action-handler "scroll-down" app))   ;; Ctrl-D

  ;; Split pane controls - using CLOS actions
  (de.anvi.croatoan:bind screen (code-char 23) (make-action-handler "toggle-split" app))       ;; Ctrl-W
  (de.anvi.croatoan:bind screen (code-char 20) (make-action-handler "toggle-split-orientation" app)) ;; Ctrl-T
  (de.anvi.croatoan:bind screen (code-char 18) (make-action-handler "set-right-buffer" app))   ;; Ctrl-R
  (de.anvi.croatoan:bind screen (code-char 24) (make-action-handler "swap-panes" app))         ;; Ctrl-X
  (de.anvi.croatoan:bind screen (code-char 29) (make-action-handler "toggle-active-pane" app)) ;; Ctrl+]
  (de.anvi.croatoan:bind screen (code-char 15) (make-action-handler "toggle-nicklist" app))    ;; Ctrl-O

  ;; Raw escape sequence binds (Ghostty/xterm/various terminals)
  (de.anvi.croatoan:bind screen (coerce '(#\Esc #\[ #\A) 'string) (make-action-handler "history-prev" app))
  (de.anvi.croatoan:bind screen (coerce '(#\Esc #\[ #\B) 'string) (make-action-handler "history-next" app))
  (de.anvi.croatoan:bind screen (coerce '(#\Esc #\[ #\D) 'string) (make-action-handler "move-left" app))
  (de.anvi.croatoan:bind screen (coerce '(#\Esc #\[ #\C) 'string) (make-action-handler "move-right" app))
  ;; Page Up/Down escape sequences
  (de.anvi.croatoan:bind screen (coerce '(#\Esc #\[ #\5 #\~) 'string) (make-action-handler "scroll-up" app))
  (de.anvi.croatoan:bind screen (coerce '(#\Esc #\[ #\6 #\~) 'string) (make-action-handler "scroll-down" app))
  (de.anvi.croatoan:bind screen (coerce '(#\Esc #\[ #\5 #\;) 'string) (make-action-handler "scroll-up" app))
  (de.anvi.croatoan:bind screen (coerce '(#\Esc #\[ #\6 #\;) 'string) (make-action-handler "scroll-down" app))
  (de.anvi.croatoan:bind screen (coerce '(#\Esc #\O #\5 #\~) 'string) (make-action-handler "scroll-up" app))
  (de.anvi.croatoan:bind screen (coerce '(#\Esc #\O #\6 #\~) 'string) (make-action-handler "scroll-down" app))

  ;; Default handler for printable characters
  (de.anvi.croatoan:bind screen t
    (lambda (o e)
      (declare (ignore o))
      (let ((key (de.anvi.croatoan:event-key e)))
        (when (and (characterp key)
                   (graphic-char-p key))
          (input-insert-char app key))))))
