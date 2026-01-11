;;;; ============================================================
;;;; CLatter Test Suite
;;;; ============================================================
;;;; Simple test framework without external dependencies.
;;;; Run with: sbcl --load tests/test-runner.lisp --eval "(clatter.tests:run-all-tests)" --quit

(defpackage #:clatter.tests
  (:use #:cl)
  (:export #:run-all-tests #:deftest #:assert-equal #:assert-true #:assert-nil))

(in-package #:clatter.tests)

;;; Test framework
(defvar *tests* nil "List of test functions")
(defvar *test-results* nil "Results of test runs")
(defvar *current-test* nil "Currently running test name")

(defmacro deftest (name &body body)
  "Define a test function."
  `(progn
     (defun ,name ()
       (let ((*current-test* ',name))
         (handler-case
             (progn ,@body t)
           (error (e)
             (format t "~&  FAIL: ~a - ~a~%" ',name e)
             nil))))
     (pushnew ',name *tests*)))

(defun assert-equal (expected actual &optional message)
  "Assert that expected equals actual."
  (unless (equal expected actual)
    (error "~@[~a: ~]Expected ~s but got ~s" message expected actual)))

(defun assert-true (value &optional message)
  "Assert that value is true."
  (unless value
    (error "~@[~a: ~]Expected true but got ~s" message value)))

(defun assert-nil (value &optional message)
  "Assert that value is nil."
  (when value
    (error "~@[~a: ~]Expected nil but got ~s" message value)))

(defun run-all-tests ()
  "Run all defined tests and report results."
  (format t "~&~%========================================~%")
  (format t "CLatter Test Suite~%")
  (format t "========================================~%~%")
  (let ((passed 0)
        (failed 0)
        (total (length *tests*)))
    (dolist (test (reverse *tests*))
      (format t "Running ~a... " test)
      (if (funcall test)
          (progn
            (format t "PASS~%")
            (incf passed))
          (incf failed)))
    (format t "~%========================================~%")
    (format t "Results: ~d/~d passed" passed total)
    (when (> failed 0)
      (format t ", ~d FAILED" failed))
    (format t "~%========================================~%~%")
    (zerop failed)))

;;; Load CLatter for testing
(format t "Loading CLatter...~%")
(load "clatter.asd")
(ql:quickload :clatter :silent t)

;;;; ============================================================
;;;; Ring Buffer Tests
;;;; ============================================================

(deftest test-ring-make
  (let ((ring (clatter.core.ring:make-ring :capacity 5)))
    (assert-true ring "Ring should be created")
    (assert-equal 0 (clatter.core.ring:ring-count ring) "New ring should be empty")))

(deftest test-ring-push
  (let ((ring (clatter.core.ring:make-ring :capacity 5)))
    (clatter.core.ring:ring-push ring "a")
    (assert-equal 1 (clatter.core.ring:ring-count ring) "Ring should have 1 item")
    (clatter.core.ring:ring-push ring "b")
    (assert-equal 2 (clatter.core.ring:ring-count ring) "Ring should have 2 items")))

(deftest test-ring-overflow
  (let ((ring (clatter.core.ring:make-ring :capacity 3)))
    (clatter.core.ring:ring-push ring "a")
    (clatter.core.ring:ring-push ring "b")
    (clatter.core.ring:ring-push ring "c")
    (clatter.core.ring:ring-push ring "d")
    (assert-equal 3 (clatter.core.ring:ring-count ring) "Ring should cap at capacity")
    (let ((items (clatter.core.ring:ring->list ring)))
      (assert-equal '("b" "c" "d") items "Oldest item should be dropped"))))

(deftest test-ring-to-list
  (let ((ring (clatter.core.ring:make-ring :capacity 5)))
    (clatter.core.ring:ring-push ring "first")
    (clatter.core.ring:ring-push ring "second")
    (let ((items (clatter.core.ring:ring->list ring)))
      (assert-equal '("first" "second") items "Items should be in order"))))

;;;; ============================================================
;;;; URL Extraction Tests
;;;; ============================================================

(deftest test-extract-urls-simple
  (let ((urls (clatter.core.model:extract-urls "Check out https://example.com for more")))
    (assert-equal 1 (length urls) "Should find 1 URL")
    (assert-equal "https://example.com" (first urls))))

(deftest test-extract-urls-multiple
  (let ((urls (clatter.core.model:extract-urls "See http://foo.com and https://bar.org")))
    (assert-equal 2 (length urls) "Should find 2 URLs")))

(deftest test-extract-urls-none
  (let ((urls (clatter.core.model:extract-urls "No URLs here")))
    (assert-nil urls "Should find no URLs")))

(deftest test-extract-urls-with-punctuation
  (let ((urls (clatter.core.model:extract-urls "Visit https://example.com.")))
    (assert-equal "https://example.com" (first urls) "Should strip trailing period")))

(deftest test-extract-urls-nil-input
  (let ((urls (clatter.core.model:extract-urls nil)))
    (assert-nil urls "Should handle nil input")))

;;;; ============================================================
;;;; Protocol Parsing Tests
;;;; ============================================================

(deftest test-parse-simple-message
  (let ((msg (clatter.core.protocol:parse-irc-line "PING :server")))
    (assert-equal "PING" (clatter.core.protocol:irc-message-command msg))
    (assert-equal '("server") (clatter.core.protocol:irc-message-params msg))))

(deftest test-parse-privmsg
  (let ((msg (clatter.core.protocol:parse-irc-line ":nick!user@host PRIVMSG #channel :Hello world")))
    (assert-equal "PRIVMSG" (clatter.core.protocol:irc-message-command msg))
    (assert-equal "nick!user@host" (clatter.core.protocol:irc-message-prefix msg))))

(deftest test-parse-with-tags
  (let ((msg (clatter.core.protocol:parse-irc-line "@time=2024-01-01T00:00:00Z :nick PRIVMSG #ch :hi")))
    (assert-true (clatter.core.protocol:irc-message-tags msg) "Should have tags")))

;;;; ============================================================
;;;; Debug System Tests (runtime symbol lookup)
;;;; ============================================================

(deftest test-debug-level-name
  (let ((level-name-fn (fdefinition (intern "LEVEL-NAME" (find-package :clatter.core.debug)))))
    (assert-equal "ERROR" (funcall level-name-fn 1))
    (assert-equal "WARN" (funcall level-name-fn 2))
    (assert-equal "INFO" (funcall level-name-fn 3))
    (assert-equal "DEBUG" (funcall level-name-fn 4))
    (assert-equal "TRACE" (funcall level-name-fn 5))))

(deftest test-debug-set-level
  (let ((set-level-fn (fdefinition (intern "SET-DEBUG-LEVEL" (find-package :clatter.core.debug))))
        (level-var (intern "*DEBUG-LEVEL*" (find-package :clatter.core.debug))))
    (funcall set-level-fn 3)
    (assert-equal 3 (symbol-value level-var))
    (funcall set-level-fn 0)))

(deftest test-debug-categories
  (let ((pkg (find-package :clatter.core.debug)))
    (funcall (fdefinition (intern "CLEAR-DEBUG-CATEGORIES" pkg)))
    (assert-true (funcall (fdefinition (intern "DEBUG-CATEGORY-ENABLED-P" pkg)) "test") "All enabled when empty")
    (funcall (fdefinition (intern "ENABLE-DEBUG-CATEGORY" pkg)) "irc")
    (assert-true (funcall (fdefinition (intern "DEBUG-CATEGORY-ENABLED-P" pkg)) "irc"))
    (funcall (fdefinition (intern "DISABLE-DEBUG-CATEGORY" pkg)) "irc")
    (assert-nil (funcall (fdefinition (intern "DEBUG-CATEGORY-ENABLED-P" pkg)) "irc"))
    (funcall (fdefinition (intern "CLEAR-DEBUG-CATEGORIES" pkg)))))

;;;; ============================================================
;;;; Constants Tests
;;;; ============================================================

(deftest test-constants-exist
  (assert-true (> clatter.core.constants:+default-scrollback-capacity+ 0))
  (assert-true (> clatter.core.constants:+irc-max-line-length+ 0))
  (assert-equal "001" clatter.core.constants:+rpl-welcome+))

;;; Run tests if loaded directly
(run-all-tests)
