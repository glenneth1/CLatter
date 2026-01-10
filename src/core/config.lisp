(in-package #:clatter.core.config)

(defvar *config-dir* (merge-pathnames ".config/clatter/" (user-homedir-pathname)))
(defvar *config-file* (merge-pathnames "config.lisp" *config-dir*))
(defvar *authinfo-file* (merge-pathnames ".authinfo" (user-homedir-pathname)))
(defvar *authinfo-gpg-file* (merge-pathnames ".authinfo.gpg" (user-homedir-pathname)))

(defclass network-config ()
  ((name        :initarg :name :accessor network-config-name)
   (server      :initarg :server :accessor network-config-server :initform "irc.libera.chat")
   (port        :initarg :port :accessor network-config-port :initform 6697)
   (tls         :initarg :tls :accessor network-config-tls :initform t)
   (nick        :initarg :nick :accessor network-config-nick)
   (username    :initarg :username :accessor network-config-username :initform nil)
   (realname    :initarg :realname :accessor network-config-realname :initform "CLatter User")
   (password    :initarg :password :accessor network-config-password :initform nil)
   (nickserv-pw :initarg :nickserv-pw :accessor network-config-nickserv-pw :initform nil)
   (sasl        :initarg :sasl :accessor network-config-sasl :initform nil)  ; :plain, :external, or nil
   (client-cert :initarg :client-cert :accessor network-config-client-cert :initform nil)  ; path to .pem file
   (autojoin    :initarg :autojoin :accessor network-config-autojoin :initform nil)
   (autoconnect :initarg :autoconnect :accessor network-config-autoconnect :initform nil)))

(defclass config ()
  ((networks :initarg :networks :accessor config-networks :initform nil)
   (default-network :initarg :default-network :accessor config-default-network :initform nil)
   (time-format :initarg :time-format :accessor config-time-format :initform "%H:%M")
   (buflist-width :initarg :buflist-width :accessor config-buflist-width :initform 28)))

(defun make-network-config (&rest args)
  (apply #'make-instance 'network-config args))

(defun make-config ()
  (make-instance 'config))

(defun ensure-config-dir ()
  (ensure-directories-exist *config-dir*))

(defun network-config-to-plist (nc)
  "Convert network-config to plist for serialization."
  (list :name (network-config-name nc)
        :server (network-config-server nc)
        :port (network-config-port nc)
        :tls (network-config-tls nc)
        :nick (network-config-nick nc)
        :username (network-config-username nc)
        :realname (network-config-realname nc)
        :password (network-config-password nc)
        :nickserv-pw (network-config-nickserv-pw nc)
        :sasl (network-config-sasl nc)
        :client-cert (network-config-client-cert nc)
        :autojoin (network-config-autojoin nc)
        :autoconnect (network-config-autoconnect nc)))

(defun plist-to-network-config (plist)
  "Convert plist to network-config."
  (apply #'make-network-config plist))

(defun config-to-sexp (cfg)
  "Convert config to s-expression for saving."
  `(:clatter-config
    :version 1
    :default-network ,(config-default-network cfg)
    :time-format ,(config-time-format cfg)
    :buflist-width ,(config-buflist-width cfg)
    :networks ,(mapcar #'network-config-to-plist (config-networks cfg))))

(defun sexp-to-config (sexp)
  "Parse s-expression into config object."
  (let ((cfg (make-config)))
    (when (and (listp sexp) (eq (car sexp) :clatter-config))
      (let ((plist (cdr sexp)))
        (setf (config-default-network cfg) (getf plist :default-network))
        (when (getf plist :time-format)
          (setf (config-time-format cfg) (getf plist :time-format)))
        (when (getf plist :buflist-width)
          (setf (config-buflist-width cfg) (getf plist :buflist-width)))
        (setf (config-networks cfg)
              (mapcar #'plist-to-network-config (getf plist :networks)))))
    cfg))

(defun save-config (cfg)
  "Save config to file."
  (ensure-config-dir)
  (with-open-file (out *config-file* :direction :output :if-exists :supersede)
    (let ((*print-pretty* t)
          (*print-case* :downcase))
      (prin1 (config-to-sexp cfg) out)
      (terpri out)))
  cfg)

(defun load-config ()
  "Load config from file, or return empty config if not found."
  (if (probe-file *config-file*)
      (with-open-file (in *config-file* :direction :input)
        (sexp-to-config (read in nil nil)))
      (make-config)))

(defun find-network-config (cfg name)
  "Find network config by name."
  (find name (config-networks cfg) :key #'network-config-name :test #'string-equal))

(defun add-network-config (cfg nc)
  "Add or update network config."
  (let ((existing (find-network-config cfg (network-config-name nc))))
    (if existing
        (setf (config-networks cfg)
              (substitute nc existing (config-networks cfg)))
        (push nc (config-networks cfg))))
  cfg)

(defun default-libera-config (nick)
  "Create a default Libera.Chat network config."
  (make-network-config
   :name "libera"
   :server "irc.libera.chat"
   :port 6697
   :tls t
   :nick nick
   :realname "CLatter User"
   :autoconnect t))

;;; Authinfo support for reading passwords from ~/.authinfo or ~/.authinfo.gpg

(defun read-authinfo-content ()
  "Read authinfo content, decrypting .gpg file if needed."
  (cond
    ;; Try encrypted file first
    ((probe-file *authinfo-gpg-file*)
     (handler-case
         (with-output-to-string (out)
           (uiop:run-program (list "gpg" "--quiet" "--decrypt" 
                                   (namestring *authinfo-gpg-file*))
                             :output out
                             :error-output nil))
       (error () nil)))
    ;; Fall back to plain text file
    ((probe-file *authinfo-file*)
     (uiop:read-file-string *authinfo-file*))
    (t nil)))

(defun parse-authinfo-line (line)
  "Parse a single authinfo line into a plist.
Format: machine HOST login USER password PASS [port PORT]"
  (let ((tokens (uiop:split-string line :separator '(#\Space #\Tab)))
        (result nil))
    (loop while tokens do
      (let ((key (pop tokens))
            (val (pop tokens)))
        (when (and key val)
          (cond
            ((string-equal key "machine") (setf (getf result :machine) val))
            ((string-equal key "login") (setf (getf result :login) val))
            ((string-equal key "password") (setf (getf result :password) val))
            ((string-equal key "port") (setf (getf result :port) val))))))
    result))

(defun lookup-authinfo (machine &optional login)
  "Look up credentials from authinfo for MACHINE (and optionally LOGIN).
Returns plist with :login and :password, or nil if not found."
  (let ((content (read-authinfo-content)))
    (when content
      (dolist (line (uiop:split-string content :separator '(#\Newline)))
        (let ((entry (parse-authinfo-line line)))
          (when (and entry
                     (string-equal (getf entry :machine) machine)
                     (or (null login)
                         (string-equal (getf entry :login) login)))
            (return entry)))))))

(defun read-systemd-cred (path)
  "Decrypt a systemd-creds encrypted file and return its contents."
  (handler-case
      (string-trim '(#\Space #\Newline #\Return)
                   (with-output-to-string (out)
                     (uiop:run-program (list "systemd-creds" "decrypt" path "-")
                                       :output out
                                       :error-output nil)))
    (error () nil)))

(defun resolve-password (pw &optional server nick)
  "Resolve a password value that may be:
   - :authinfo - read from ~/.authinfo or ~/.authinfo.gpg (requires server/nick)
   - (:systemd-cred \"/path/to/file.cred\") - decrypt using systemd-creds
   - plain string - use directly
   - nil - no password"
  (cond
    ;; :authinfo - lookup from authinfo file
    ((eq pw :authinfo)
     (when (and server nick)
       (let ((entry (lookup-authinfo server nick)))
         (getf entry :password))))
    ;; (:systemd-cred "/path/to/file.cred") - decrypt using systemd-creds
    ((and (listp pw) (eq (first pw) :systemd-cred))
     (let ((cred-path (second pw)))
       (when cred-path
         (read-systemd-cred cred-path))))
    ;; Plain string password or nil
    (t pw)))

(defun get-network-password (nc)
  "Get NickServ password for network config."
  (resolve-password (network-config-nickserv-pw nc)
                    (network-config-server nc)
                    (network-config-nick nc)))

(defun get-server-password (nc)
  "Get server password for network config."
  (resolve-password (network-config-password nc)
                    (network-config-server nc)
                    (network-config-nick nc)))
