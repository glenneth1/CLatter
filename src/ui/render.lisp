(in-package #:clatter.ui.render)

;; Nick color palette - bright colors that work on dark backgrounds
(defparameter *nick-colors*
  '(:red :green :yellow :blue :magenta :cyan
    :lime :maroon :olive :navy :purple :teal))

(defun nick-color (nick)
  "Hash a nick to a consistent color from the palette."
  (let ((hash (reduce #'+ (map 'list #'char-code nick))))
    (nth (mod hash (length *nick-colors*)) *nick-colors*)))

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

;; Unicode box-drawing characters for thin borders
(defparameter *box-h* #\─)      ;; horizontal
(defparameter *box-v* #\│)      ;; vertical
(defparameter *box-tl* #\┌)     ;; top-left
(defparameter *box-tr* #\┐)     ;; top-right
(defparameter *box-bl* #\└)     ;; bottom-left
(defparameter *box-br* #\┘)     ;; bottom-right

(defun %draw-box (win)
  "Draw a thin Unicode box border around the window content area."
  (let ((h (de.anvi.croatoan:height win))
        (w (de.anvi.croatoan:width win)))
    (when (and (> h 1) (> w 1))
      ;; top border
      (setf (de.anvi.croatoan:cursor-position-y win) 0)
      (setf (de.anvi.croatoan:cursor-position-x win) 0)
      (format win "~a" *box-tl*)
      (loop repeat (- w 2) do (format win "~a" *box-h*))
      (format win "~a" *box-tr*)
      ;; bottom border
      (setf (de.anvi.croatoan:cursor-position-y win) (1- h))
      (setf (de.anvi.croatoan:cursor-position-x win) 0)
      (format win "~a" *box-bl*)
      (loop repeat (- w 2) do (format win "~a" *box-h*))
      (format win "~a" *box-br*)
      ;; side borders
      (loop for y from 1 below (1- h) do
        (setf (de.anvi.croatoan:cursor-position-y win) y)
        (setf (de.anvi.croatoan:cursor-position-x win) 0)
        (format win "~a" *box-v*)
        (setf (de.anvi.croatoan:cursor-position-x win) (1- w))
        (format win "~a" *box-v*)))))

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

(defun render-chat-pane (win buf)
  "Render a buffer's messages into a chat window pane."
  (let* ((msgs (ring->list (buffer-scrollback buf)))
         (h (de.anvi.croatoan:height win))
         (w (de.anvi.croatoan:width win))
         (content-h (- h 2))
         (content-w (- w 2))
         (offset (buffer-scroll-offset buf)))
    ;; Build display lines from messages
    (let ((display-lines nil))
      (loop for m in msgs
            for nick = (or (clatter.core.model:message-nick m) "*")
            for text = (clatter.core.model:message-text m)
            for highlightp = (clatter.core.model:message-highlight m)
            for nick-display = (format nil "~a: " nick)
            for nick-len = (length nick-display)
            for text-width = (max 1 (- content-w nick-len))
            for wrapped = (wrap-text text text-width)
            do (let ((first-line t))
                 (dolist (line wrapped)
                   (push (list :nick (if first-line nick-display
                                         (make-string nick-len :initial-element #\Space))
                               :text line
                               :highlight highlightp
                               :nick-raw nick
                               :first first-line)
                         display-lines)
                   (setf first-line nil))))
      (setf display-lines (nreverse display-lines))
      (let* ((total (length display-lines))
             (end (max 0 (- total offset)))
             (start (max 0 (- end content-h)))
             (visible (subseq display-lines start end))
             (y 1))
        (dolist (dl visible)
          (when (< y (1+ content-h))
            (let ((nick-display (getf dl :nick))
                  (text-display (getf dl :text))
                  (highlightp (getf dl :highlight))
                  (nick-raw (getf dl :nick-raw))
                  (firstp (getf dl :first)))
              (setf (de.anvi.croatoan:cursor-position-y win) y)
              (setf (de.anvi.croatoan:cursor-position-x win) 1)
              (if highlightp
                  ;; Highlighted messages: bold magenta for visibility without harsh background
                  (de.anvi.croatoan:add-string win nick-display
                                               :fgcolor :magenta
                                               :attributes '(:bold))
                  (if firstp
                      (de.anvi.croatoan:add-string win nick-display :fgcolor (nick-color nick-raw))
                      (de.anvi.croatoan:add-string win nick-display)))
              (if highlightp
                  (de.anvi.croatoan:add-string win text-display
                                               :fgcolor :magenta
                                               :attributes '(:bold))
                  (de.anvi.croatoan:add-string win text-display)))
            (incf y)))))))

(defun render-frame (app)
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
    (let ((content-w (- (de.anvi.croatoan:width wbuf) 2))
          (content-h (- (de.anvi.croatoan:height wbuf) 2)))
      (loop for i from 0 below (min (length (app-buffers app)) content-h)
            for buf = (aref (app-buffers app) i)
            for marker = (if (= i (app-current-buffer-id app)) "▶" " ")
            for unread = (buffer-unread-count buf)
            for hi = (buffer-highlight-count buf)
            for line = (format nil "~a~a~@[ (~d)~]~@[ !~d~]"
                               marker (buffer-title buf)
                               (and (> unread 0) unread)
                               (and (> hi 0) hi))
            do (%draw-line wbuf (1+ i) 1 (subseq line 0 (min (length line) content-w)))))

    ;; chat area: render pane(s)
    (let ((left-buf (current-buffer app))
          (active-pane (ui-active-pane ui)))
      ;; In split mode, show buffer name in border with active indicator
      (when (ui-split-mode ui)
        (let ((left-title (format nil " ~a~a "
                                  (if (eq active-pane :left) "» " "")
                                  (buffer-title left-buf))))
          (%clear-and-border wchat left-title))
        (render-chat-pane wchat left-buf))
      ;; In normal mode, no title needed
      (unless (ui-split-mode ui)
        (render-chat-pane wchat left-buf)))
    ;; Render second pane if in split mode
    (let ((wchat2 (ui-win-chat2 ui))
          (active-pane (ui-active-pane ui)))
      (when (and wchat2 (ui-split-mode ui))
        (let ((split-buf-id (ui-split-buffer-id ui)))
          (when (and split-buf-id (< split-buf-id (length (app-buffers app))))
            (let* ((right-buf (aref (app-buffers app) split-buf-id))
                   (right-title (format nil " ~a~a "
                                        (if (eq active-pane :right) "» " "")
                                        (buffer-title right-buf))))
              (%clear-and-border wchat2 right-title)
              (render-chat-pane wchat2 right-buf))))))

    ;; status
    (let* ((buf (current-buffer app))
           (unread (buffer-unread-count buf))
           (highlights (buffer-highlight-count buf))
           (split-p (ui-split-mode ui))
           (right-buf (when (and split-p (ui-split-buffer-id ui)
                                 (< (ui-split-buffer-id ui) (length (app-buffers app))))
                        (aref (app-buffers app) (ui-split-buffer-id ui))))
           (line (if split-p
                     (format nil " Ctrl+] pane | Ctrl-W unsplit | Ctrl-P/N left buf | Ctrl-R right buf | Ctrl-X swap")
                     (format nil " [~a]~@[  unread:~d~]~@[  mentions:~d~]  | /q quit | Ctrl-W split | Ctrl-P/N buffers"
                             (buffer-title buf)
                             (and (> unread 0) unread)
                             (and (> highlights 0) highlights)))))
      (%draw-line wstatus 0 0 (subseq line 0 (min (length line) (de.anvi.croatoan:width wstatus)))))

    ;; input
    (let* ((st (ui-input ui))
           (prompt "> ")
           (txt (input-text st))
           (cursor (input-cursor st))
           (full (concatenate 'string prompt txt)))
      (%draw-line winput 0 0 (subseq full 0 (min (length full) (de.anvi.croatoan:width winput))))
      ;; move cursor (roughly)
      (setf (de.anvi.croatoan:cursor-position-y winput) 0)
      (setf (de.anvi.croatoan:cursor-position-x winput) (min (de.anvi.croatoan:width winput) (+ (length prompt) cursor))))

    ;; batch refresh to reduce flicker
    (de.anvi.croatoan:mark-for-refresh wbuf)
    (de.anvi.croatoan:mark-for-refresh wchat)
    (let ((wchat2 (ui-win-chat2 ui)))
      (when wchat2
        (de.anvi.croatoan:mark-for-refresh wchat2)))
    (de.anvi.croatoan:mark-for-refresh wstatus)
    (de.anvi.croatoan:mark-for-refresh winput)
    (de.anvi.croatoan:refresh-marked)

    ;; Final refresh of input window to position cursor there
    (de.anvi.croatoan:refresh winput)

    (clear-dirty app)
    app))
