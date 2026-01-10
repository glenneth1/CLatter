(in-package #:clatter.ui.input)

;; Track last typing notification time to avoid spamming
(defvar *last-typing-sent* 0)
(defparameter *typing-throttle* 3)  ; seconds between typing notifications

(defun input-set-text (app new-text &optional (cursor (length new-text)))
  (setf (input-text (ui-input (app-ui app))) new-text)
  (setf (input-cursor (ui-input (app-ui app))) cursor)
  (mark-dirty app :input)
  app)

(defun %istate (app)
  (ui-input (app-ui app)))

(defun maybe-send-typing (app)
  "Send typing notification if enough time has passed since last one."
  (let ((now (get-universal-time))
        (conn (clatter.core.model:get-current-connection app)))
    (when (and conn (> (- now *last-typing-sent*) *typing-throttle*))
      (let ((buf (clatter.core.model:active-buffer app)))
        (when (and buf (not (eq (clatter.core.model:buffer-kind buf) :server)))
          (setf *last-typing-sent* now)
          (clatter.net.irc:irc-send-typing conn (clatter.core.model:buffer-title buf) :active))))))

(defun input-insert-char (app ch)
  (let* ((st (%istate app))
         (s (input-text st))
         (i (input-cursor st)))
    (setf (input-text st) (concatenate 'string (subseq s 0 i) (string ch) (subseq s i)))
    (incf (input-cursor st))
    (mark-dirty app :input)
    ;; Send typing notification
    (maybe-send-typing app)))

(defun input-backspace (app)
  (let* ((st (%istate app))
         (s (input-text st))
         (i (input-cursor st)))
    (when (> i 0)
      (setf (input-text st) (concatenate 'string (subseq s 0 (1- i)) (subseq s i)))
      (decf (input-cursor st))
      (mark-dirty app :input))))

(defun input-delete (app)
  (let* ((st (%istate app))
         (s (input-text st))
         (i (input-cursor st)))
    (when (< i (length s))
      (setf (input-text st) (concatenate 'string (subseq s 0 i) (subseq s (1+ i))))
      (mark-dirty app :input))))

(defun input-move-left (app)
  (let ((st (%istate app)))
    (when (> (input-cursor st) 0)
      (decf (input-cursor st))
      (mark-dirty app :input))))

(defun input-move-right (app)
  (let* ((st (%istate app))
         (s (input-text st)))
    (when (< (input-cursor st) (length s))
      (incf (input-cursor st))
      (mark-dirty app :input))))

(defun input-move-home (app)
  (let ((st (%istate app)))
    (setf (input-cursor st) 0)
    (mark-dirty app :input)))

(defun input-move-end (app)
  (let* ((st (%istate app))
         (s (input-text st)))
    (setf (input-cursor st) (length s))
    (mark-dirty app :input)))

(defun input-history-prev (app)
  (let* ((st (%istate app))
         (hist (input-history st)))
    (when (> (fill-pointer hist) 0)
      (let ((pos (or (input-history-pos st) (fill-pointer hist))))
        (setf pos (max 0 (1- pos)))
        (setf (input-history-pos st) pos)
        (input-set-text app (aref hist pos))))))

(defun input-history-next (app)
  (let* ((st (%istate app))
         (hist (input-history st)))
    (when (> (fill-pointer hist) 0)
      (let ((pos (input-history-pos st)))
        (when pos
          (setf pos (min (fill-pointer hist) (1+ pos)))
          (if (= pos (fill-pointer hist))
              (progn
                (setf (input-history-pos st) nil)
                (input-set-text app ""))
              (progn
                (setf (input-history-pos st) pos)
                (input-set-text app (aref hist pos)))))))))

(defun input-submit-line (app)
  (let* ((st (%istate app))
         (line (string-trim '(#\Space #\Tab) (input-text st))))
    (when (> (length line) 0)
      ;; push into history
      (let ((hist (input-history st)))
        (vector-push-extend line hist)
        (setf (input-history-pos st) nil))
      ;; Get connection for current buffer
      (let ((conn (clatter.core.model:get-current-connection app)))
        (when conn
          (let ((buf (clatter.core.model:active-buffer app)))
            (when (and buf (not (eq (clatter.core.model:buffer-kind buf) :server)))
              (clatter.net.irc:irc-send-typing conn (clatter.core.model:buffer-title buf) :done))))
        ;; Handle via command system
        (clatter.core.commands:handle-input-line app conn line)))
    (input-set-text app "" 0)))

(defun input-tab-complete (app)
  "Tab-complete nick at cursor position."
  (let* ((st (%istate app))
         (text (input-text st))
         (cursor (input-cursor st))
         (buf (current-buffer app))
         (members (when buf (clatter.core.model:buffer-members buf))))
    (unless members (return-from input-tab-complete nil))
    ;; Find word start (go back to space or start of line)
    (let* ((word-start (or (position #\Space text :end cursor :from-end t)
                           -1))
           (word-start (1+ word-start))
           (prefix (subseq text word-start cursor)))
      (when (> (length prefix) 0)
        ;; Find matching nicks
        (let ((matches nil))
          (maphash (lambda (nick val)
                     (declare (ignore val))
                     (when (and (>= (length nick) (length prefix))
                                (string-equal prefix (subseq nick 0 (length prefix))))
                       (push nick matches)))
                   members)
          (when matches
            ;; Sort and take first match
            (setf matches (sort matches #'string-lessp))
            (let* ((completion (first matches))
                   ;; Add ": " if at start of line, " " otherwise
                   (suffix (if (= word-start 0) ": " " "))
                   (new-text (concatenate 'string
                                          (subseq text 0 word-start)
                                          completion
                                          suffix
                                          (subseq text cursor))))
              (input-set-text app new-text (+ word-start (length completion) (length suffix))))))))))
