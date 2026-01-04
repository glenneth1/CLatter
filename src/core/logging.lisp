(in-package #:clatter.core.logging)

;;; Log file management
;;; Format: ~/.local/share/clatter/logs/network/target/YYYY-MM-DD.log
;;; Line format: [YYYY-MM-DD HH:MM:SS] <nick> message

(defvar *log-base-dir* 
  (merge-pathnames ".local/share/clatter/logs/" (user-homedir-pathname))
  "Base directory for log files.")

(defvar *logging-enabled* t
  "Whether logging is enabled.")

(defvar *current-network* "libera"
  "Current network name for logging purposes.")

(defun ensure-log-directory (network target)
  "Ensure the log directory exists for network/target."
  (let ((dir (merge-pathnames 
              (make-pathname :directory (list :relative network target))
              *log-base-dir*)))
    (ensure-directories-exist dir)
    dir))

(defun log-file-path (network target &optional (time (get-universal-time)))
  "Get the log file path for a given network, target, and date."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time time)
    (declare (ignore sec min hour))
    (let ((dir (ensure-log-directory network target))
          (filename (format nil "~4,'0d-~2,'0d-~2,'0d.log" year month day)))
      (merge-pathnames filename dir))))

(defun format-log-timestamp (time)
  "Format timestamp for log entry."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time time)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month day hour min sec)))

(defun sanitize-target (target)
  "Sanitize target name for use as directory name."
  ;; Replace problematic characters, lowercase for consistency
  (string-downcase
   (substitute #\_ #\/ 
               (substitute #\_ #\\ target))))

(defun format-log-line (msg)
  "Format a message for logging."
  (let ((ts (format-log-timestamp (clatter.core.model:message-ts msg)))
        (level (clatter.core.model:message-level msg))
        (nick (clatter.core.model:message-nick msg))
        (text (clatter.core.model:message-text msg)))
    (case level
      (:chat (format nil "[~a] <~a> ~a" ts nick text))
      (:action (format nil "[~a] * ~a ~a" ts nick text))
      (:notice (format nil "[~a] -~a- ~a" ts nick text))
      (:join (format nil "[~a] --> ~a" ts text))
      (:part (format nil "[~a] <-- ~a" ts text))
      (:quit (format nil "[~a] <-- ~a has quit" ts nick))
      (:system (format nil "[~a] --- ~a" ts text))
      (:error (format nil "[~a] !!! ~a" ts text))
      (otherwise (format nil "[~a] ~a: ~a" ts nick text)))))

(defun write-log-entry (network target msg)
  "Write a message to the appropriate log file."
  (when *logging-enabled*
    (handler-case
        (let ((path (log-file-path network (sanitize-target target) 
                                   (clatter.core.model:message-ts msg))))
          (with-open-file (out path
                               :direction :output
                               :if-exists :append
                               :if-does-not-exist :create
                               :external-format :utf-8)
            (write-line (format-log-line msg) out)))
      (error (e)
        (format *error-output* "Logging error: ~a~%" e)))))

(defun log-message (buffer msg)
  "Log a message to the appropriate file based on buffer."
  (let ((target (clatter.core.model:buffer-title buffer))
        (kind (clatter.core.model:buffer-kind buffer)))
    ;; Only log channel and query buffers, not server buffer
    (when (member kind '(:channel :query))
      (write-log-entry *current-network* target msg))))

;;; Log reading functions

(defun list-log-files (network target)
  "List all log files for a network/target, sorted by date (newest first)."
  (let ((dir (merge-pathnames 
              (make-pathname :directory (list :relative network (sanitize-target target)))
              *log-base-dir*)))
    (when (probe-file dir)
      (sort (directory (merge-pathnames "*.log" dir))
            #'string> :key #'namestring))))

(defun read-log-file (path &optional (max-lines 100))
  "Read the last N lines from a log file."
  (when (probe-file path)
    (with-open-file (in path :direction :input :external-format :utf-8)
      (let ((lines nil)
            (line-count 0))
        ;; Read all lines
        (loop for line = (read-line in nil nil)
              while line
              do (push line lines)
                 (incf line-count))
        ;; Return last max-lines (they're in reverse order from push)
        (let ((result (if (> line-count max-lines)
                          (subseq lines 0 max-lines)
                          lines)))
          (nreverse result))))))

(defun read-recent-logs (network target &optional (max-lines 100))
  "Read recent log entries for a target."
  (let ((files (list-log-files network target)))
    (when files
      ;; Start with most recent file
      (let ((lines nil)
            (remaining max-lines))
        (dolist (file files)
          (when (> remaining 0)
            (let ((file-lines (read-log-file file remaining)))
              (setf lines (append lines file-lines))
              (decf remaining (length file-lines)))))
        lines))))

(defun search-logs (network target pattern &optional (max-results 50))
  "Search log files for a pattern. Returns matching lines with context."
  (let ((files (list-log-files network target))
        (results nil)
        (count 0))
    (dolist (file files)
      (when (and (< count max-results) (probe-file file))
        (with-open-file (in file :direction :input :external-format :utf-8)
          (loop for line = (read-line in nil nil)
                while (and line (< count max-results))
                when (search pattern line :test #'char-equal)
                  do (push (cons (file-namestring file) line) results)
                     (incf count)))))
    (nreverse results)))

(defun list-logged-targets (network)
  "List all targets (channels/nicks) that have logs for a network."
  (let ((dir (merge-pathnames 
              (make-pathname :directory (list :relative network))
              *log-base-dir*)))
    (when (probe-file dir)
      (mapcar (lambda (p)
                (car (last (pathname-directory p))))
              (directory (merge-pathnames "*/" dir))))))

;;; Log export functions

(defun read-all-logs (network target)
  "Read all log entries for a target, across all log files."
  (let ((files (list-log-files network target)))
    (when files
      (let ((all-lines nil))
        (dolist (file (reverse files))  ; oldest first
          (let ((lines (read-log-file file 100000)))
            (setf all-lines (append all-lines lines))))
        all-lines))))

(defun export-logs-text (network target output-path)
  "Export logs to plain text file."
  (let ((lines (read-all-logs network target)))
    (when lines
      (ensure-directories-exist output-path)
      (with-open-file (out output-path
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create
                           :external-format :utf-8)
        (dolist (line lines)
          (write-line line out)))
      (length lines))))

(defun escape-json-string (str)
  "Escape a string for JSON output."
  (with-output-to-string (out)
    (loop for char across str do
      (case char
        (#\" (write-string "\\\"" out))
        (#\\ (write-string "\\\\" out))
        (#\Newline (write-string "\\n" out))
        (#\Return (write-string "\\r" out))
        (#\Tab (write-string "\\t" out))
        (otherwise (write-char char out))))))

(defun parse-log-line (line)
  "Parse a log line back into components. Returns (timestamp type nick text)."
  ;; Format: [YYYY-MM-DD HH:MM:SS] <nick> text
  ;;     or: [YYYY-MM-DD HH:MM:SS] --> text
  ;;     or: [YYYY-MM-DD HH:MM:SS] * nick text
  (when (and (> (length line) 22) (char= (char line 0) #\[))
    (let* ((ts (subseq line 1 20))
           (rest (subseq line 22)))
      (cond
        ;; <nick> message
        ((and (> (length rest) 0) (char= (char rest 0) #\<))
         (let ((end-nick (position #\> rest)))
           (when end-nick
             (list ts "message" (subseq rest 1 end-nick) 
                   (if (> (length rest) (+ end-nick 2))
                       (subseq rest (+ end-nick 2))
                       "")))))
        ;; --> join
        ((and (>= (length rest) 4) (string= (subseq rest 0 4) "--> "))
         (list ts "join" "" (subseq rest 4)))
        ;; <-- part/quit
        ((and (>= (length rest) 4) (string= (subseq rest 0 4) "<-- "))
         (list ts "part" "" (subseq rest 4)))
        ;; * action
        ((and (>= (length rest) 2) (string= (subseq rest 0 2) "* "))
         (let* ((action-rest (subseq rest 2))
                (space-pos (position #\Space action-rest)))
           (if space-pos
               (list ts "action" (subseq action-rest 0 space-pos) 
                     (subseq action-rest (1+ space-pos)))
               (list ts "action" action-rest ""))))
        ;; --- system
        ((and (>= (length rest) 4) (string= (subseq rest 0 4) "--- "))
         (list ts "system" "" (subseq rest 4)))
        ;; -nick- notice
        ((and (> (length rest) 0) (char= (char rest 0) #\-))
         (let ((end-nick (position #\- rest :start 1)))
           (when end-nick
             (list ts "notice" (subseq rest 1 end-nick)
                   (if (> (length rest) (+ end-nick 2))
                       (subseq rest (+ end-nick 2))
                       "")))))
        ;; fallback
        (t (list ts "unknown" "" rest))))))

(defun export-logs-json (network target output-path)
  "Export logs to JSON file."
  (let ((lines (read-all-logs network target)))
    (when lines
      (ensure-directories-exist output-path)
      (with-open-file (out output-path
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create
                           :external-format :utf-8)
        (write-line "[" out)
        (let ((first t))
          (dolist (line lines)
            (let ((parsed (parse-log-line line)))
              (when parsed
                (if first
                    (setf first nil)
                    (write-line "," out))
                (format out "  {\"timestamp\": \"~a\", \"type\": \"~a\", \"nick\": \"~a\", \"text\": \"~a\"}"
                        (escape-json-string (first parsed))
                        (escape-json-string (second parsed))
                        (escape-json-string (third parsed))
                        (escape-json-string (fourth parsed)))))))
        (write-char #\Newline out)
        (write-line "]" out))
      (length lines))))

(defun export-logs-html (network target output-path)
  "Export logs to HTML file."
  (let ((lines (read-all-logs network target)))
    (when lines
      (ensure-directories-exist output-path)
      (with-open-file (out output-path
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create
                           :external-format :utf-8)
        (format out "<!DOCTYPE html>~%<html>~%<head>~%")
        (format out "<meta charset=\"utf-8\">~%")
        (format out "<title>IRC Log: ~a - ~a</title>~%" network target)
        (format out "<style>~%")
        (format out "body { font-family: monospace; background: #1a1a1a; color: #ccc; padding: 20px; }~%")
        (format out ".msg { margin: 2px 0; } .time { color: #666; } .nick { color: #6cf; font-weight: bold; }~%")
        (format out ".join { color: #6c6; } .part { color: #c66; } .action { color: #c9c; font-style: italic; }~%")
        (format out "</style>~%</head>~%<body>~%")
        (format out "<h1>~a / ~a</h1>~%" network target)
        (dolist (line lines)
          (let ((parsed (parse-log-line line)))
            (when parsed
              (let ((ts (first parsed))
                    (type (second parsed))
                    (nick (third parsed))
                    (text (fourth parsed)))
                (cond
                  ((string= type "message")
                   (format out "<div class=\"msg\"><span class=\"time\">[~a]</span> <span class=\"nick\">&lt;~a&gt;</span> ~a</div>~%"
                           ts nick text))
                  ((string= type "join")
                   (format out "<div class=\"msg join\"><span class=\"time\">[~a]</span> --&gt; ~a</div>~%"
                           ts text))
                  ((string= type "part")
                   (format out "<div class=\"msg part\"><span class=\"time\">[~a]</span> &lt;-- ~a</div>~%"
                           ts text))
                  ((string= type "action")
                   (format out "<div class=\"msg action\"><span class=\"time\">[~a]</span> * ~a ~a</div>~%"
                           ts nick text))
                  (t
                   (format out "<div class=\"msg\"><span class=\"time\">[~a]</span> ~a</div>~%"
                           ts text)))))))
        (format out "</body>~%</html>~%"))
      (length lines))))

(defun export-logs (network target format output-path)
  "Export logs to specified format (:text, :json, :html)."
  (case format
    (:text (export-logs-text network target output-path))
    (:json (export-logs-json network target output-path))
    (:html (export-logs-html network target output-path))
    (otherwise nil)))
