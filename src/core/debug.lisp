(in-package #:clatter.core.debug)

;;;; ============================================================
;;;; Debug Logging System
;;;; ============================================================
;;;; Simple, functional debug logging for troubleshooting.
;;;; Levels: 0=off, 1=error, 2=warn, 3=info, 4=debug, 5=trace

;;; Level constants
(defconstant +level-off+ 0)
(defconstant +level-error+ 1)
(defconstant +level-warn+ 2)
(defconstant +level-info+ 3)
(defconstant +level-debug+ 4)
(defconstant +level-trace+ 5)

;;; State
(defvar *debug-level* 0 "Current debug level")
(defvar *debug-stream* *error-output* "Output stream")
(defvar *debug-file-path* nil "Log file path")
(defvar *debug-file-stream* nil "Log file stream")
(defvar *debug-categories* (make-hash-table :test 'equal) "Enabled categories")
(defvar *debug-timestamps* t "Include timestamps")

(defun level-name (level)
  "Return string name for debug level."
  (case level
    (1 "ERROR") (2 "WARN") (3 "INFO") (4 "DEBUG") (5 "TRACE") (t "???")))

(defun set-debug-level (level)
  "Set the debug level (0-5)."
  (setf *debug-level*
        (etypecase level
          (integer (max 0 (min 5 level)))
          (string (or (parse-integer level :junk-allowed t) 0)))))

(defun debug-status ()
  "Return string describing current debug status."
  (let ((cats (let ((c nil)) 
                (maphash (lambda (k v) (when v (push k c))) *debug-categories*) 
                (sort c #'string<))))
    (format nil "Debug level: ~d (~a)~@[, File: ~a~]~@[, Categories: ~{~a~^, ~}~]"
            *debug-level* (level-name *debug-level*) *debug-file-path* cats)))

;;; Category management
(defun enable-debug-category (category)
  (setf (gethash category *debug-categories*) t))

(defun disable-debug-category (category)
  (setf (gethash category *debug-categories*) nil))

(defun debug-category-enabled-p (category)
  (or (zerop (hash-table-count *debug-categories*))
      (gethash category *debug-categories*)))

(defun list-debug-categories ()
  (let ((cats nil))
    (maphash (lambda (k v) (when v (push k cats))) *debug-categories*)
    (sort cats #'string<)))

(defun clear-debug-categories ()
  (clrhash *debug-categories*))

;;; File logging
(defun open-debug-file (path)
  (when *debug-file-stream*
    (ignore-errors (close *debug-file-stream*)))
  (setf *debug-file-path* path
        *debug-file-stream* (open path :direction :output 
                                       :if-exists :append 
                                       :if-does-not-exist :create)))

(defun close-debug-file ()
  (when *debug-file-stream*
    (ignore-errors (close *debug-file-stream*))
    (setf *debug-file-stream* nil *debug-file-path* nil)))

;;; Core logging
(defun format-timestamp ()
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month day hour min sec)))

(defun debug-log (level category format-string &rest args)
  "Log a debug message if level and category are enabled."
  (when (and (>= *debug-level* level)
             (debug-category-enabled-p category))
    (let ((msg (format nil "~@[~a ~][~a] [~a] ~?~%"
                       (when *debug-timestamps* (format-timestamp))
                       (level-name level) category format-string args)))
      (write-string msg *debug-stream*)
      (force-output *debug-stream*)
      (when *debug-file-stream*
        (write-string msg *debug-file-stream*)
        (force-output *debug-file-stream*)))))

;;; Convenience macros
(defmacro log-error (category format-string &rest args)
  `(debug-log +level-error+ ,category ,format-string ,@args))

(defmacro log-warn (category format-string &rest args)
  `(debug-log +level-warn+ ,category ,format-string ,@args))

(defmacro log-info (category format-string &rest args)
  `(debug-log +level-info+ ,category ,format-string ,@args))

(defmacro log-debug (category format-string &rest args)
  `(debug-log +level-debug+ ,category ,format-string ,@args))

(defmacro log-trace (category format-string &rest args)
  `(debug-log +level-trace+ ,category ,format-string ,@args))

;;; Protocol logging
(defun log-irc-raw (direction data)
  (log-trace "protocol" "~a ~a" 
             (if (eq direction :send) ">>>" "<<<")
             (string-trim '(#\Return #\Newline) data)))

(defun log-irc-event (event-type &rest details)
  (log-debug "event" "~a: ~{~a~^ ~}" event-type details))
