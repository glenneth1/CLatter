(in-package #:clatter.core.theme)

;;; Theme system for CLatter
;;; Loads theme definitions from ~/.config/clatter/themes/

(defvar *current-theme* nil "The currently loaded theme")

(defclass theme ()
  ((name :initarg :name :accessor theme-name :initform "default")
   ;; Nick colors - list of color keywords for nick rotation
   (nick-colors :initarg :nick-colors :accessor theme-nick-colors
                :initform '(:red :green :yellow :blue :magenta :cyan))
   ;; Message level colors
   (join-color :initarg :join-color :accessor theme-join-color :initform :green)
   (part-color :initarg :part-color :accessor theme-part-color :initform :yellow)
   (quit-color :initarg :quit-color :accessor theme-quit-color :initform :yellow)
   (kick-color :initarg :kick-color :accessor theme-kick-color :initform :red)
   (mode-color :initarg :mode-color :accessor theme-mode-color :initform :cyan)
   (topic-color :initarg :topic-color :accessor theme-topic-color :initform :cyan)
   (notice-color :initarg :notice-color :accessor theme-notice-color :initform :cyan)
   (system-color :initarg :system-color :accessor theme-system-color :initform :blue)
   (error-color :initarg :error-color :accessor theme-error-color :initform :red)
   ;; Highlight colors
   (highlight-fg :initarg :highlight-fg :accessor theme-highlight-fg :initform :magenta)
   (highlight-bold :initarg :highlight-bold :accessor theme-highlight-bold :initform t)
   ;; Timestamp color
   (timestamp-color :initarg :timestamp-color :accessor theme-timestamp-color :initform :white)
   ;; Status bar
   (status-fg :initarg :status-fg :accessor theme-status-fg :initform :white)
   (status-bg :initarg :status-bg :accessor theme-status-bg :initform :blue)
   ;; Buffer list
   (buflist-fg :initarg :buflist-fg :accessor theme-buflist-fg :initform :white)
   (buflist-selected-fg :initarg :buflist-selected-fg :accessor theme-buflist-selected-fg :initform :cyan)
   (buflist-unread-fg :initarg :buflist-unread-fg :accessor theme-buflist-unread-fg :initform :yellow)
   (buflist-highlight-fg :initarg :buflist-highlight-fg :accessor theme-buflist-highlight-fg :initform :magenta)
   ;; Border colors
   (border-fg :initarg :border-fg :accessor theme-border-fg :initform :blue)))

(defun make-default-theme ()
  "Create the default theme."
  (make-instance 'theme :name "default"))

(defun make-tokyo-night-theme ()
  "Create a Tokyo Night inspired theme.
   Based on https://github.com/folke/tokyonight.nvim"
  (make-instance 'theme
                 :name "tokyo-night"
                 ;; Tokyo Night accent colors - more variety
                 :nick-colors '(:magenta :cyan :green :yellow :red :white)
                 ;; Message levels - using Tokyo Night palette feel
                 :join-color :green
                 :part-color :yellow
                 :quit-color :yellow
                 :kick-color :red
                 :mode-color :magenta
                 :topic-color :cyan
                 :notice-color :cyan
                 :system-color :blue
                 :error-color :red
                 ;; Highlights - magenta/purple is key in Tokyo Night
                 :highlight-fg :magenta
                 :highlight-bold t
                 ;; Timestamp - subtle
                 :timestamp-color :blue
                 ;; Status bar - dark blue feel
                 :status-fg :cyan
                 :status-bg :blue
                 ;; Buffer list
                 :buflist-fg :white
                 :buflist-selected-fg :magenta
                 :buflist-unread-fg :cyan
                 :buflist-highlight-fg :magenta
                 ;; Borders - purple/blue for Tokyo Night feel
                 :border-fg :magenta))

(defun theme-dir ()
  "Return the themes directory path."
  (merge-pathnames "themes/" (clatter.core.config:config-dir)))

(defun ensure-theme-dir ()
  "Ensure the themes directory exists."
  (ensure-directories-exist (theme-dir)))

(defun theme-path (name)
  "Return the path to a theme file."
  (merge-pathnames (format nil "~a.lisp" name) (theme-dir)))

(defun parse-theme-plist (plist)
  "Parse a theme plist into a theme object."
  (let ((theme (make-instance 'theme)))
    (when (getf plist :name)
      (setf (theme-name theme) (getf plist :name)))
    (when (getf plist :nick-colors)
      (setf (theme-nick-colors theme) (getf plist :nick-colors)))
    (when (getf plist :join-color)
      (setf (theme-join-color theme) (getf plist :join-color)))
    (when (getf plist :part-color)
      (setf (theme-part-color theme) (getf plist :part-color)))
    (when (getf plist :quit-color)
      (setf (theme-quit-color theme) (getf plist :quit-color)))
    (when (getf plist :kick-color)
      (setf (theme-kick-color theme) (getf plist :kick-color)))
    (when (getf plist :mode-color)
      (setf (theme-mode-color theme) (getf plist :mode-color)))
    (when (getf plist :topic-color)
      (setf (theme-topic-color theme) (getf plist :topic-color)))
    (when (getf plist :notice-color)
      (setf (theme-notice-color theme) (getf plist :notice-color)))
    (when (getf plist :system-color)
      (setf (theme-system-color theme) (getf plist :system-color)))
    (when (getf plist :error-color)
      (setf (theme-error-color theme) (getf plist :error-color)))
    (when (getf plist :highlight-fg)
      (setf (theme-highlight-fg theme) (getf plist :highlight-fg)))
    (when (getf plist :highlight-bold)
      (setf (theme-highlight-bold theme) (getf plist :highlight-bold)))
    (when (getf plist :timestamp-color)
      (setf (theme-timestamp-color theme) (getf plist :timestamp-color)))
    (when (getf plist :status-fg)
      (setf (theme-status-fg theme) (getf plist :status-fg)))
    (when (getf plist :status-bg)
      (setf (theme-status-bg theme) (getf plist :status-bg)))
    (when (getf plist :buflist-fg)
      (setf (theme-buflist-fg theme) (getf plist :buflist-fg)))
    (when (getf plist :buflist-selected-fg)
      (setf (theme-buflist-selected-fg theme) (getf plist :buflist-selected-fg)))
    (when (getf plist :buflist-unread-fg)
      (setf (theme-buflist-unread-fg theme) (getf plist :buflist-unread-fg)))
    (when (getf plist :buflist-highlight-fg)
      (setf (theme-buflist-highlight-fg theme) (getf plist :buflist-highlight-fg)))
    theme))

(defun load-theme-file (path)
  "Load a theme from a file path."
  (handler-case
      (with-open-file (in path :direction :input :if-does-not-exist nil)
        (when in
          (let ((data (read in nil nil)))
            (when (and data (listp data))
              (parse-theme-plist data)))))
    (error (e)
      (format *error-output* "Error loading theme ~a: ~a~%" path e)
      nil)))

(defun load-theme (name)
  "Load a theme by name from the themes directory.
   Returns the theme object or nil if not found."
  (let ((path (theme-path name)))
    (if (probe-file path)
        (load-theme-file path)
        ;; Check for built-in themes
        (cond
          ((string-equal name "tokyo-night") (make-tokyo-night-theme))
          ((string-equal name "default") (make-default-theme))
          (t nil)))))

(defun init-theme (&optional name)
  "Initialize the theme system. Load theme by name or use default."
  (ensure-theme-dir)
  (setf *current-theme*
        (or (when name (load-theme name))
            (load-theme "tokyo-night")  ;; Default to tokyo-night
            (make-tokyo-night-theme))))

(defun current-theme ()
  "Get the current theme, initializing if needed."
  (or *current-theme*
      (progn (init-theme) *current-theme*)))

(defun save-theme (theme)
  "Save a theme to a file."
  (ensure-theme-dir)
  (let ((path (theme-path (theme-name theme))))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (format out ";;; CLatter Theme: ~a~%" (theme-name theme))
      (format out ";;; Place in ~~/.config/clatter/themes/~%~%")
      (prin1 `(:name ,(theme-name theme)
               :nick-colors ,(theme-nick-colors theme)
               :join-color ,(theme-join-color theme)
               :part-color ,(theme-part-color theme)
               :quit-color ,(theme-quit-color theme)
               :kick-color ,(theme-kick-color theme)
               :mode-color ,(theme-mode-color theme)
               :topic-color ,(theme-topic-color theme)
               :notice-color ,(theme-notice-color theme)
               :system-color ,(theme-system-color theme)
               :error-color ,(theme-error-color theme)
               :highlight-fg ,(theme-highlight-fg theme)
               :highlight-bold ,(theme-highlight-bold theme)
               :timestamp-color ,(theme-timestamp-color theme)
               :status-fg ,(theme-status-fg theme)
               :status-bg ,(theme-status-bg theme)
               :buflist-fg ,(theme-buflist-fg theme)
               :buflist-selected-fg ,(theme-buflist-selected-fg theme)
               :buflist-unread-fg ,(theme-buflist-unread-fg theme)
               :buflist-highlight-fg ,(theme-buflist-highlight-fg theme))
             out)
      (terpri out))
    path))
