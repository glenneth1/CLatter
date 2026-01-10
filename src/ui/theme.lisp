(in-package #:clatter.ui.render)

;;;; ============================================================
;;;; Theme System for CLatter UI
;;;; ============================================================
;;;; Themes define colors, styles, and visual elements.
;;;; Users can create custom themes by subclassing base-theme.

;;; Base theme class - defines all customizable visual elements
(defclass base-theme ()
  (;; Nick colors for chat messages
   (nick-colors :initarg :nick-colors
                :accessor theme-nick-colors
                :initform '(:red :green :yellow :blue :magenta :cyan
                            :lime :maroon :olive :navy :purple :teal))
   ;; Message level colors
   (join-color :initarg :join-color :accessor theme-join-color :initform :green)
   (part-color :initarg :part-color :accessor theme-part-color :initform :yellow)
   (quit-color :initarg :quit-color :accessor theme-quit-color :initform :yellow)
   (away-color :initarg :away-color :accessor theme-away-color :initform :magenta)
   (kick-color :initarg :kick-color :accessor theme-kick-color :initform :red)
   (mode-color :initarg :mode-color :accessor theme-mode-color :initform :cyan)
   (notice-color :initarg :notice-color :accessor theme-notice-color :initform :cyan)
   (system-color :initarg :system-color :accessor theme-system-color :initform :blue)
   (error-color :initarg :error-color :accessor theme-error-color :initform :red)
   (highlight-color :initarg :highlight-color :accessor theme-highlight-color :initform :magenta)
   ;; UI element colors
   (status-fg :initarg :status-fg :accessor theme-status-fg :initform nil)
   (status-bg :initarg :status-bg :accessor theme-status-bg :initform nil)
   (input-fg :initarg :input-fg :accessor theme-input-fg :initform nil)
   (input-bg :initarg :input-bg :accessor theme-input-bg :initform nil)
   (border-fg :initarg :border-fg :accessor theme-border-fg :initform nil)
   (active-buffer-fg :initarg :active-buffer-fg :accessor theme-active-buffer-fg :initform :cyan)
   (unread-fg :initarg :unread-fg :accessor theme-unread-fg :initform :yellow)
   (mention-fg :initarg :mention-fg :accessor theme-mention-fg :initform :red)
   ;; Box drawing characters
   (box-h :initarg :box-h :accessor theme-box-h :initform #\─)
   (box-v :initarg :box-v :accessor theme-box-v :initform #\│)
   (box-tl :initarg :box-tl :accessor theme-box-tl :initform #\┌)
   (box-tr :initarg :box-tr :accessor theme-box-tr :initform #\┐)
   (box-bl :initarg :box-bl :accessor theme-box-bl :initform #\└)
   (box-br :initarg :box-br :accessor theme-box-br :initform #\┘)
   ;; Prompt and markers
   (input-prompt :initarg :input-prompt :accessor theme-input-prompt :initform "> ")
   (scroll-indicator :initarg :scroll-indicator :accessor theme-scroll-indicator :initform "…")
   (active-marker :initarg :active-marker :accessor theme-active-marker :initform ">")
   (inactive-marker :initarg :inactive-marker :accessor theme-inactive-marker :initform " "))
  (:documentation "Base theme class defining all visual elements."))

;;; Generic functions for theme operations
(defgeneric theme-level-color (theme level)
  (:documentation "Get the color for a message level from the theme."))

(defmethod theme-level-color ((theme base-theme) level)
  "Return the color for a message level."
  (case level
    (:join (theme-join-color theme))
    (:part (theme-part-color theme))
    (:quit (theme-quit-color theme))
    (:away (theme-away-color theme))
    (:kick (theme-kick-color theme))
    (:mode (theme-mode-color theme))
    (:notice (theme-notice-color theme))
    (:system (theme-system-color theme))
    (:error (theme-error-color theme))
    (otherwise nil)))

(defgeneric theme-nick-color (theme nick)
  (:documentation "Get a consistent color for a nick from the theme."))

(defmethod theme-nick-color ((theme base-theme) nick)
  "Hash a nick to a consistent color from the theme's palette."
  (let* ((colors (theme-nick-colors theme))
         (hash (reduce #'+ (map 'list #'char-code nick))))
    (nth (mod hash (length colors)) colors)))

;;; Dark theme - default, optimized for dark terminals
(defclass dark-theme (base-theme)
  ()
  (:default-initargs
   :nick-colors '(:red :green :yellow :blue :magenta :cyan
                  :lime :maroon :olive :navy :purple :teal)
   :highlight-color :magenta
   :active-buffer-fg :cyan)
  (:documentation "Dark theme optimized for dark terminal backgrounds."))

;;; Light theme - for light terminal backgrounds
(defclass light-theme (base-theme)
  ()
  (:default-initargs
   :nick-colors '(:red :blue :magenta :cyan :green :yellow)
   :join-color :green
   :part-color :red
   :quit-color :red
   :system-color :blue
   :highlight-color :magenta
   :active-buffer-fg :blue)
  (:documentation "Light theme optimized for light terminal backgrounds."))

;;; Solarized dark theme
(defclass solarized-dark-theme (base-theme)
  ()
  (:default-initargs
   :nick-colors '(:blue :cyan :green :yellow :red :magenta)
   :join-color :green
   :part-color :yellow
   :quit-color :yellow
   :system-color :cyan
   :notice-color :blue
   :highlight-color :magenta
   :active-buffer-fg :cyan)
  (:documentation "Solarized dark color scheme."))

;;; Minimal theme - reduced colors for accessibility
(defclass minimal-theme (base-theme)
  ()
  (:default-initargs
   :nick-colors '(:white)
   :join-color :white
   :part-color :white
   :quit-color :white
   :away-color :white
   :kick-color :white
   :mode-color :white
   :notice-color :white
   :system-color :white
   :error-color :white
   :highlight-color :white
   :active-buffer-fg :white
   :unread-fg :white
   :mention-fg :white)
  (:documentation "Minimal theme with reduced colors for accessibility."))

;;; ASCII theme - no unicode box drawing
(defclass ascii-theme (base-theme)
  ()
  (:default-initargs
   :box-h #\-
   :box-v #\|
   :box-tl #\+
   :box-tr #\+
   :box-bl #\+
   :box-br #\+)
  (:documentation "ASCII-only theme for terminals without Unicode support."))

;;; Rounded theme - rounded corners (default)
(defclass rounded-theme (base-theme)
  ()
  (:default-initargs
   :box-h #\─
   :box-v #\│
   :box-tl #\╭
   :box-tr #\╮
   :box-bl #\╰
   :box-br #\╯)
  (:documentation "Theme with rounded corners for modern terminals."))

;;; Current theme - global variable
(defvar *current-theme* nil "The currently active theme.")

(defun current-theme ()
  "Get the current theme, creating default if needed."
  (unless *current-theme*
    (setf *current-theme* (make-instance 'rounded-theme)))
  *current-theme*)

(defun set-theme (theme-class)
  "Set the current theme by class name."
  (setf *current-theme* (make-instance theme-class)))

;;; User theme loading from ~/.config/clatter/themes/
(defparameter *user-theme-dir* 
  (merge-pathnames "themes/" clatter.core.config:*config-dir*)
  "Directory for user theme files.")

(defparameter *user-theme-file*
  (merge-pathnames "theme.lisp" *user-theme-dir*)
  "Default user theme file path.")

(defun load-user-theme ()
  "Load user theme from ~/.config/clatter/themes/theme.lisp (CLOS format).
   The file should define a theme class and call set-theme.
   Users can customize by editing this file - no rebuild needed."
  (when (probe-file *user-theme-file*)
    (handler-case
        (progn
          (load *user-theme-file*)
          t)
      (error (e)
        (format *error-output* "Error loading theme: ~a~%" e)
        nil))))

(defun ensure-theme-dir ()
  "Create the themes directory if it doesn't exist."
  (ensure-directories-exist *user-theme-dir*))

;;; Theme registry for user selection
(defvar *theme-registry* (make-hash-table :test 'equal)
  "Registry of available themes by name.")

(defun register-theme (name class)
  "Register a theme class under a name."
  (setf (gethash (string-upcase name) *theme-registry*) class))

(defun find-theme (name)
  "Find a theme class by name."
  (gethash (string-upcase name) *theme-registry*))

(defun list-themes ()
  "List all registered theme names."
  (let ((names nil))
    (maphash (lambda (k v) (declare (ignore v)) (push k names))
             *theme-registry*)
    (sort names #'string<)))

;; Register built-in themes
(register-theme "dark" 'dark-theme)
(register-theme "light" 'light-theme)
(register-theme "solarized" 'solarized-dark-theme)
(register-theme "minimal" 'minimal-theme)
(register-theme "ascii" 'ascii-theme)
(register-theme "rounded" 'rounded-theme)

;; Try to load user theme at startup
(load-user-theme)
