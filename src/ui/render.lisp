(in-package #:clatter.ui.render)

;; Health check timing
(defvar *last-health-check* 0 "Universal time of last connection health check")
(defparameter *health-check-interval* 30 "Seconds between health checks")

;; Nick color - now uses theme system
(defun nick-color (nick)
  "Hash a nick to a consistent color from the current theme."
  (theme-nick-color (current-theme) nick))

(defun format-time (universal-time fmt)
  "Format universal time using format string.
Supported tokens: %H (24h hour), %I (12h hour), %M (minute), %S (second), %p (AM/PM)."
  (multiple-value-bind (sec min hour) (decode-universal-time universal-time)
    (let* ((hour12 (let ((h (mod hour 12))) (if (zerop h) 12 h)))
           (ampm (if (< hour 12) "AM" "PM"))
           (result fmt))
      (setf result (cl-ppcre:regex-replace-all "%H" result (format nil "~2,'0d" hour)))
      (setf result (cl-ppcre:regex-replace-all "%I" result (format nil "~2,'0d" hour12)))
      (setf result (cl-ppcre:regex-replace-all "%M" result (format nil "~2,'0d" min)))
      (setf result (cl-ppcre:regex-replace-all "%S" result (format nil "~2,'0d" sec)))
      (setf result (cl-ppcre:regex-replace-all "%p" result ampm))
      result)))

(defun wrap-text (text width)
  "Wrap text to fit within width, returning list of lines."
  (if (<= (length text) width)
      (list text)
      (let ((lines nil)
            (start 0))
        (loop while (< start (length text))
              for end = (min (+ start width) (length text))
              for break-pos = (if (< end (length text))
                                  (or (position #\Space text :start start :end end :from-end t)
                                      end)
                                  end)
              for actual-end = (if (= break-pos start) end break-pos)
              do (push (subseq text start actual-end) lines)
                 (setf start (if (and (< actual-end (length text))
                                      (char= (char text actual-end) #\Space))
                                 (1+ actual-end)
                                 actual-end)))
        (nreverse lines))))

(defun %ui (app) (app-ui app))

(defun %draw-line (win y x text)
  (setf (de.anvi.croatoan:cursor-position-y win) y)
  (setf (de.anvi.croatoan:cursor-position-x win) x)
  (format win "~a" text))

;; Box drawing now uses theme system
(defun %draw-box (win)
  "Draw a box border around the window content area using theme characters."
  (let ((h (de.anvi.croatoan:height win))
        (w (de.anvi.croatoan:width win))
        (theme (current-theme)))
    (when (and (> h 1) (> w 1))
      ;; top border
      (setf (de.anvi.croatoan:cursor-position-y win) 0)
      (setf (de.anvi.croatoan:cursor-position-x win) 0)
      (format win "~a" (theme-box-tl theme))
      (loop repeat (- w 2) do (format win "~a" (theme-box-h theme)))
      (format win "~a" (theme-box-tr theme))
      ;; bottom border
      (setf (de.anvi.croatoan:cursor-position-y win) (1- h))
      (setf (de.anvi.croatoan:cursor-position-x win) 0)
      (format win "~a" (theme-box-bl theme))
      (loop repeat (- w 2) do (format win "~a" (theme-box-h theme)))
      (format win "~a" (theme-box-br theme))
      ;; side borders
      (loop for y from 1 below (1- h) do
        (setf (de.anvi.croatoan:cursor-position-y win) y)
        (setf (de.anvi.croatoan:cursor-position-x win) 0)
        (format win "~a" (theme-box-v theme))
        (setf (de.anvi.croatoan:cursor-position-x win) (1- w))
        (format win "~a" (theme-box-v theme))))))

(defun %clear-and-border (win &optional (title nil) (draw-border t))
  (de.anvi.croatoan:clear win)
  (when draw-border
    (%draw-box win))
  (when (and title draw-border)
    ;; Draw title in top border
    (let ((w (de.anvi.croatoan:width win))
          (tlen (length title)))
      (when (< (+ tlen 4) w)
        (setf (de.anvi.croatoan:cursor-position-y win) 0)
        (setf (de.anvi.croatoan:cursor-position-x win) 2)
        (format win "~a" title)))))

(defun level-color (level)
  "Return the color for a message level from the current theme."
  (theme-level-color (current-theme) level))

(defun render-chat-pane (win buf)
  "Render a buffer's messages into a chat window pane."
  (let* ((all-msgs (ring->list (buffer-scrollback buf)))
         ;; Apply filter if active
         (msgs (if (clatter.core.model:buffer-filter-active buf)
                   (let ((pattern (clatter.core.model:buffer-filter-pattern buf)))
                     (remove-if-not 
                      (lambda (m)
                        (search pattern (clatter.core.model:message-text m) :test #'char-equal))
                      all-msgs))
                   all-msgs))
         (h (de.anvi.croatoan:height win))
         (w (de.anvi.croatoan:width win))
         (content-h (- h 2))
         (content-w (- w 2))
         (offset (buffer-scroll-offset buf))
         (time-fmt (let ((cfg clatter.core.commands:*current-config*))
                     (if cfg (clatter.core.config:config-time-format cfg) "%H:%M"))))
    ;; Build display lines from messages, grouped by message
    ;; Each message becomes a list of its display lines (for proper reversal)
    (let ((message-groups nil))
      (loop for m in msgs
            for ts = (clatter.core.model:message-ts m)
            for time-str = (format-time ts time-fmt)
            for nick = (or (clatter.core.model:message-nick m) "*")
            for text = (clatter.core.model:message-text m)
            for level = (clatter.core.model:message-level m)
            for highlightp = (clatter.core.model:message-highlight m)
            for nick-display = (format nil "[~a] ~a: " time-str nick)
            for nick-len = (length nick-display)
            for text-width = (max 1 (- content-w nick-len))
            for wrapped = (wrap-text text text-width)
            do (let ((msg-lines nil)
                     (first-line t))
                 (dolist (line wrapped)
                   (push (list :nick (if first-line nick-display
                                         (make-string nick-len :initial-element #\Space))
                               :text line
                               :highlight highlightp
                               :nick-raw nick
                               :level level
                               :first first-line)
                         msg-lines)
                   (setf first-line nil))
                 (push (nreverse msg-lines) message-groups)))
      ;; message-groups is already newest-first (due to push)
      ;; Flatten and display from top - newest messages at top
      (let* ((display-lines (apply #'append message-groups))
             (total (length display-lines))
             (start (min offset (max 0 (- total content-h))))
             (visible (subseq display-lines start (min (+ start content-h) total)))
             (y 1))
        (dolist (dl visible)
          (when (< y (1+ content-h))
            (let* ((nick-display (getf dl :nick))
                   (text-display (getf dl :text))
                   (highlightp (getf dl :highlight))
                   (nick-raw (getf dl :nick-raw))
                   (level (getf dl :level))
                   (firstp (getf dl :first))
                   (lvl-color (level-color level)))
              (setf (de.anvi.croatoan:cursor-position-y win) y)
              (setf (de.anvi.croatoan:cursor-position-x win) 1)
              (cond
                ;; Highlighted messages: bold magenta
                (highlightp
                 (de.anvi.croatoan:add-string win nick-display
                                              :fgcolor :magenta
                                              :attributes '(:bold))
                 (de.anvi.croatoan:add-string win text-display
                                              :fgcolor :magenta
                                              :attributes '(:bold)))
                ;; Level-colored messages (join/part/system/etc)
                (lvl-color
                 (de.anvi.croatoan:add-string win nick-display :fgcolor lvl-color)
                 (de.anvi.croatoan:add-string win text-display :fgcolor lvl-color))
                ;; Regular chat messages - nick colored, text default
                (t
                 (if firstp
                     (de.anvi.croatoan:add-string win nick-display :fgcolor (nick-color nick-raw))
                     (de.anvi.croatoan:add-string win nick-display))
                 (de.anvi.croatoan:add-string win text-display))))
            (incf y)))))))

(defun maybe-check-connection-health (app)
  "Periodically check all connection health (called from render loop)."
  (let ((now (get-universal-time)))
    (when (> (- now *last-health-check*) *health-check-interval*)
      (setf *last-health-check* now)
      ;; Check health of all connections
      (maphash (lambda (name conn)
                 (declare (ignore name))
                 (when conn
                   (clatter.net.irc:irc-check-health conn)))
               (app-connections app)))))

(defun render-frame (app)
  ;; Check connection health periodically (every 30 seconds)
  (maybe-check-connection-health app)
  
  (unless (dirty-p app)
    (return-from render-frame app))

  (let* ((ui (%ui app))
         (wbuf (ui-win-buflist ui))
         (wchat (ui-win-chat ui))
         (wstatus (ui-win-status ui))
         (winput (ui-win-input ui)))

    ;; clear and draw borders
    (%clear-and-border wbuf " buffers ")
    (%clear-and-border wchat nil)  ;; no title, cleaner look
    (%clear-and-border wstatus nil nil)  ;; no border for status
    (%clear-and-border winput nil nil)   ;; no border for input

    ;; buffer list (inside border: start at y=1, x=1)
    ;; Group buffers by network - server buffers first, then their channels
    (let ((content-w (- (de.anvi.croatoan:width wbuf) 2))
          (content-h (- (de.anvi.croatoan:height wbuf) 2))
          (row 0)
          (buffers (app-buffers app))
          (current-id (app-current-buffer-id app))
          (visual-order nil))  ; Build visual order for navigation
      ;; Collect networks and their buffers
      (let ((networks (make-hash-table :test 'equal))
            (network-order nil))
        ;; First pass: group buffers by network, track order by server buffer index
        (loop for i from 0 below (length buffers)
              for buf = (aref buffers i)
              when buf
              do (let ((net (or (buffer-network buf) "unknown")))
                   (unless (gethash net networks)
                     (setf (gethash net networks) nil)  ; Initialize empty list
                     (push (cons net i) network-order))  ; Track network order by first appearance
                   (push (cons i buf) (gethash net networks))))
        ;; Sort networks by their server buffer index (first appearance)
        (setf network-order (sort network-order #'< :key #'cdr))
        ;; Render each network's buffers in order
        (dolist (net-pair network-order)
          (let* ((net-name (car net-pair))
                 (buf-list (gethash net-name networks))
                 (sorted (sort (copy-list buf-list)
                               (lambda (a b)
                                 ;; Server buffers first, then alphabetically
                                 (let ((ka (buffer-kind (cdr a)))
                                       (kb (buffer-kind (cdr b))))
                                   (cond ((eq ka :server) t)
                                         ((eq kb :server) nil)
                                         (t (string< (buffer-title (cdr a))
                                                     (buffer-title (cdr b))))))))))
            (dolist (pair sorted)
              (push (car pair) visual-order)  ; Add buffer ID to visual order
              (when (< row content-h)
                (let* ((i (car pair))
                       (buf (cdr pair))
                       (current-p (= i current-id))
                       ;; Use CLOS method for indent based on buffer type
                       (indent (if (typep buf 'clatter.core.model:server-buffer) "" "  "))
                       ;; Use CLOS method for rendering buffer list item
                       (line (render-buflist-item buf current-p indent)))
                  (%draw-line wbuf (1+ row) 1 (subseq line 0 (min (length line) content-w)))
                  (incf row)))))))
      ;; Store visual order for navigation (reversed because we pushed)
      (setf (app-buffer-order app) (nreverse visual-order)))

    ;; chat area: render pane(s)
    (let ((left-buf (current-buffer app))
          (active-pane (ui-active-pane ui)))
      (when left-buf  ;; Guard against nil buffer
        ;; In split mode, show buffer name in border with active indicator
        (when (ui-split-mode ui)
          (let ((left-title (format nil " ~a~a "
                                    (if (eq active-pane :left) ">> " "")
                                    (buffer-title left-buf))))
            (%clear-and-border wchat left-title))
          (render-chat-pane wchat left-buf))
        ;; In normal mode, no title needed
        (unless (ui-split-mode ui)
          (render-chat-pane wchat left-buf))))
    ;; Render second pane if in split mode
    (let ((wchat2 (ui-win-chat2 ui))
          (active-pane (ui-active-pane ui)))
      (when (and wchat2 (ui-split-mode ui))
        (let ((split-buf-id (ui-split-buffer-id ui)))
          (when (and split-buf-id (< split-buf-id (length (app-buffers app))))
            (let ((right-buf (aref (app-buffers app) split-buf-id)))
              (when right-buf  ;; Guard against nil buffer
                (let ((right-title (format nil " ~a~a "
                                           (if (eq active-pane :right) ">> " "")
                                           (buffer-title right-buf))))
                  (%clear-and-border wchat2 right-title)
                  (render-chat-pane wchat2 right-buf))))))))

    ;; Render nick list panel if visible
    (let ((wnicklist (ui-win-nicklist ui)))
      (when wnicklist
        (%clear-and-border wnicklist " nicks ")
        ;; Use active buffer (respects split pane selection)
        (let* ((active-pane (ui-active-pane ui))
               (buf (if (and (ui-split-mode ui) (eq active-pane :right))
                        (let ((split-id (ui-split-buffer-id ui)))
                          (when (and split-id (< split-id (length (app-buffers app))))
                            (aref (app-buffers app) split-id)))
                        (current-buffer app)))
               (content-w (- (de.anvi.croatoan:width wnicklist) 2))
               (content-h (- (de.anvi.croatoan:height wnicklist) 2))
               (row 0))
          (when (and buf (buffer-has-members-p buf))
            (let ((nick-list nil))
              (maphash (lambda (nick val)
                         (declare (ignore val))
                         (push nick nick-list))
                       (buffer-members buf))
              (setf nick-list (sort nick-list #'string-lessp))
              (dolist (nick nick-list)
                (when (< row content-h)
                  (%draw-line wnicklist (1+ row) 1 
                              (subseq nick 0 (min (length nick) content-w)))
                  (incf row))))))))

    ;; status - show info for active pane in split mode
    (let* ((split-p (ui-split-mode ui))
           (active-pane (ui-active-pane ui))
           (left-buf (current-buffer app))
           (right-buf (when (and split-p (ui-split-buffer-id ui)
                                 (< (ui-split-buffer-id ui) (length (app-buffers app))))
                        (aref (app-buffers app) (ui-split-buffer-id ui))))
           ;; Use active pane's buffer for status info
           (buf (if (and split-p (eq active-pane :right) right-buf)
                    right-buf
                    left-buf))
           (unread (if buf (buffer-unread-count buf) 0))
           (highlights (if buf (buffer-highlight-count buf) 0))
           (typing-nicks (when buf (clatter.core.model:get-typing-nicks buf)))
           (channel-modes (when buf (buffer-channel-modes buf)))
           (my-modes (when buf (buffer-my-modes buf)))
           (typing-str (when typing-nicks
                         (if (= (length typing-nicks) 1)
                             (format nil "~a is typing..." (first typing-nicks))
                             (format nil "~{~a~^, ~} are typing..." typing-nicks))))
           ;; Build title with channel modes
           (title-str (if buf
                          (if (and channel-modes (> (length channel-modes) 0))
                              (format nil "~a [~a]" (buffer-title buf) channel-modes)
                              (buffer-title buf))
                          "---"))
           ;; Get network name for status bar
           (network-name (when buf (buffer-network buf)))
           ;; Build mode indicator for status bar
           (mode-str (when (and my-modes (> (length my-modes) 0))
                       (format nil "(~a)" my-modes)))
           (shortcuts " | ^P/N buf | ^U/D scroll | ^W split | ^L redraw")
           (line (format nil " [~a]~@[ ~a~]~@[  unread:~d~]~@[  mentions:~d~]~@[  ~a~]~a"
                         (if network-name (format nil "~a/~a" network-name title-str) title-str)
                         mode-str
                         (and (> unread 0) unread)
                         (and (> highlights 0) highlights)
                         typing-str
                         shortcuts)))
      (%draw-line wstatus 0 0 (subseq line 0 (min (length line) (de.anvi.croatoan:width wstatus)))))

    ;; input - with horizontal scrolling when text exceeds window width
    (let* ((st (ui-input ui))
           (prompt "> ")
           (txt (input-text st))
           (cursor (input-cursor st))
           (win-w (de.anvi.croatoan:width winput))
           (prompt-len (length prompt))
           (visible-w (- win-w prompt-len))  ;; space available for text after prompt
           ;; Calculate scroll offset to keep cursor visible
           ;; Leave some margin (5 chars) so user can see context
           (margin 5)
           (scroll-offset (cond
                            ;; Cursor fits in visible area - no scroll
                            ((< cursor visible-w) 0)
                            ;; Scroll to keep cursor visible with margin
                            (t (- cursor (- visible-w margin)))))
           ;; Extract visible portion of text
           (visible-end (min (length txt) (+ scroll-offset visible-w)))
           (visible-txt (subseq txt scroll-offset visible-end))
           ;; Add scroll indicator if text is scrolled
           (display-prompt (if (> scroll-offset 0) "…" prompt))
           (full (concatenate 'string display-prompt visible-txt)))
      (%draw-line winput 0 0 (subseq full 0 (min (length full) win-w)))
      ;; Position cursor relative to scroll offset
      (setf (de.anvi.croatoan:cursor-position-y winput) 0)
      (setf (de.anvi.croatoan:cursor-position-x winput) 
            (+ (length display-prompt) (- cursor scroll-offset))))

    ;; batch refresh to reduce flicker
    (de.anvi.croatoan:mark-for-refresh wbuf)
    (de.anvi.croatoan:mark-for-refresh wchat)
    (let ((wchat2 (ui-win-chat2 ui)))
      (when wchat2
        (de.anvi.croatoan:mark-for-refresh wchat2)))
    (let ((wnicklist (ui-win-nicklist ui)))
      (when wnicklist
        (de.anvi.croatoan:mark-for-refresh wnicklist)))
    (de.anvi.croatoan:mark-for-refresh wstatus)
    (de.anvi.croatoan:mark-for-refresh winput)
    (de.anvi.croatoan:refresh-marked)

    ;; Final refresh of input window to position cursor there
    (de.anvi.croatoan:refresh winput)

    (clear-dirty app)
    app))
