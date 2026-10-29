;;;; Tests of echo.lisp, and of the request observer in debug.lisp it hangs on, shared by every project under ~/lisp-ai as
;;;; part of the system lisp-ai-tools/tests: a project calls (lisp-ai-echo-tests:run DIRECTORY START STOP) as it calls the
;;;; debug tests: a server of the project on a socket in DIRECTORY, requests sent as bin/lisp eval sends them, and what the
;;;; echo would have written to the Sly REPL collected instead, since no Emacs is attached to a test. It returns how many
;;;; checks failed, printing a line for each.

(defpackage :lisp-ai-echo-tests
  (:use :cl)
  (:export #:run))

(in-package :lisp-ai-echo-tests)

(defvar *failures* 0)

(defmacro check (label form)
  "Count a failure, naming LABEL, unless FORM is true."
  `(unless ,form
     (incf *failures*)
     (format t "FAIL: ~a~%" ,label)))

(defun check-equal (label actual expected)
  "Count a failure naming LABEL, EXPECTED and ACTUAL unless they are EQUAL."
  (unless (equal actual expected)
    (incf *failures*)
    (format t "FAIL ~s: expected ~s, got ~s~%" label expected actual)))

(defun socket-request (path text)
  "Send TEXT to the server at PATH as bin/lisp eval does and return the whole reply."
  (lisp-ai-debug-tests::socket-request path text))

(defun exchange (when text)
  "What WRITE-EXCHANGE writes for one side of an exchange."
  (with-output-to-string (stream) (lisp-ai-echo:write-exchange stream when text)))

(defun run-write-exchange ()
  (check-equal "a request is shown after the speaker's prompt" (exchange :request "(+ 1 2)") (format nil "claude> (+ 1 2)~%"))
  (check-equal "a request keeps its lines, and the newline it was sent with is not doubled"
               (exchange :request (format nil ";; a comment~%(defun f () 1)~%"))
               (format nil "claude> ;; a comment~%(defun f () 1)~%"))
  (check-equal "a reply is shown as it is" (exchange :reply (format nil "3~%")) (format nil "3~%"))
  (check-equal "a reply that does not end its line is given one" (exchange :reply "x") (format nil "x~%"))
  (check-equal "an empty reply shows nothing" (exchange :reply "") "")
  (check-equal "a request on a line already begun starts a line of its own"
               (with-output-to-string (stream)
                 (write-string "GPS> " stream)
                 (lisp-ai-echo:write-exchange stream :request "(+ 1 2)"))
               (format nil "GPS> ~%claude> (+ 1 2)~%"))
  (check-equal "the speaker can be named"
               (let ((lisp-ai-echo:*speaker* "me")) (exchange :request "(+ 1 2)")) (format nil "me> (+ 1 2)~%")))

(defun run-through-server (path)
  "The echo installed on a running server: what reaches *WRITE* for requests sent as bin/lisp eval sends them."
  (let ((seen 'nil) (observer lisp-ai-debug:*request-observer*) (write lisp-ai-echo:*write*) (speaker lisp-ai-echo:*speaker*)
        (request lisp-ai-debug:*request*))
    (unwind-protect
         (progn
           (setf lisp-ai-echo:*write* (lambda (when text) (push (list when text) seen)))
           (lisp-ai-echo:start)
           (check "start installs the echo as the request observer" (eq lisp-ai-debug:*request-observer* 'lisp-ai-echo::echo))
           (check-equal "the request itself is answered as before" (socket-request path "(+ 1 2)") (format nil "3~%"))
           (check-equal "the request and then its reply are shown" (reverse seen)
                        (list (list :request "(+ 1 2)") (list :reply (format nil "3~%"))))
           (setf seen '())
           (socket-request path "T")
           (socket-request path "(lisp-ai-bench:pending-names)")
           (socket-request path (format nil " T~%"))
           (check-equal "the script's own probes, alive and unsaved, are not shown" seen '())
           (setf seen '())
           (socket-request path "(values)")
           (check-equal "a request with no values shows the request and an empty reply" (reverse seen)
                        (list (list :request "(values)") (list :reply "")))
           ;; a held error: the HELD report is the reply shown at once; the finished reply comes with the request that ends it
           (setf seen '())
           (let* ((reply (socket-request path "(error \"boom\")"))
                  (id (lisp-ai-debug-tests::held-id reply)))
             (check "the error is held" id)
             (check-equal "the HELD report is shown as the reply, as it was sent" (reverse seen)
                          (list (list :request "(error \"boom\")") (list :reply reply)))
             (when id
               (setf seen '())
               (let ((request (format nil "(lisp-ai-debug:debug-abandon ~d)" id)))
                 (socket-request path request)
                 (check-equal "the request that ends it is shown with the finished reply" (reverse seen)
                              (list (list :request request) (list :reply (format nil "ERROR: boom~%")))))))
           (setf seen '())
           (lisp-ai-echo:stop)
           (check "stop removes the observer" (null lisp-ai-debug:*request-observer*))
           (socket-request path "(+ 1 2)")
           (check-equal "after stop nothing is shown" seen '())
           ;; the observer protocol itself, with an observer of the test's own
           (setf seen '())
           (setf lisp-ai-debug:*request-observer*
                 (lambda (when text) (push (list when text lisp-ai-debug:*request*) seen)))
           (socket-request path "(+ 1 2)")
           (check-equal "the observer sees the request and the reply, with *request* bound to the request both times"
                        (reverse seen)
                        (list (list :request "(+ 1 2)" "(+ 1 2)") (list :reply (format nil "3~%") "(+ 1 2)")))
           (check "and *request* is, after it, what it was before (NIL, or the request these checks run in)"
            (equal lisp-ai-debug:*request* request))
           (setf lisp-ai-debug:*request-observer*
                 (lambda (when text) (declare (ignore text)) (when (eq when :request) (error "observer broke"))))
           (check-equal "an error in the observer is a line in the reply, and the request still runs"
                        (socket-request path "(+ 1 2)") (format nil ";; observer: observer broke~%3~%")))
      (setf lisp-ai-echo:*write* write
            lisp-ai-echo:*speaker* speaker
            lisp-ai-debug:*request-observer* observer))))

(defun fake-connection ()
  "A Slynk connection with no Emacs behind it, for REPL-TARGET to look into."
  (slynk::make-multithreaded-connection :socket nil :socket-io
                                        (make-two-way-stream (make-string-input-stream "") (make-broadcast-stream))))

(defun fake-listener (connection)
  "A listener made as Slynk makes one for CONNECTION, which records it as one of the connection's listeners."
  (let ((slynk::*emacs-connection* connection))
    (make-instance 'slynk::listener :out (make-string-output-stream) :in (make-string-input-stream ""))))

(defun run-repl-target ()
  "Where the echo writes, on connections made up here: the REPL held as a channel, as a listener, in the newest
   connection; and with a connection and no REPL, a line on the echo's report stream, once."
  (let* ((connection (fake-connection)) (repl (fake-listener connection)))
    (setf (slynk::connection-listeners connection) nil
          (slynk::connection-channels connection) (list repl))
    (let ((slynk::*connections* (list connection)))
      (check-equal "a REPL the connection holds only as a channel, with no listener, is written to"
       (multiple-value-list (lisp-ai-echo::repl-target)) (list (slynk::listener-out repl) connection))))
  (let* ((connection (fake-connection)) (listener (fake-listener connection)))
    (let ((slynk::*connections* (list connection)))
      (check-equal "a REPL held only as a listener is written to" (multiple-value-list (lisp-ai-echo::repl-target))
       (list (slynk::listener-out listener) connection))))
  (let* ((old (fake-connection)) (new (fake-connection)) (listener (fake-listener new)))
    (fake-listener old)
    (let ((slynk::*connections* (list new old)))
      (check-equal "the newest connection is the one written to" (multiple-value-list (lisp-ai-echo::repl-target))
       (list (slynk::listener-out listener) new))))
  (let* ((connection (fake-connection)) (report (make-string-output-stream)))
    (let ((slynk::*connections* (list connection)) (lisp-ai-echo::*unshown* 'nil) (lisp-ai-echo::*report* report))
      (check "with a connection and no REPL, the write does not fail"
       (null (lisp-ai-echo::write-to-repl :request "(+ 1 2)")))
      (check "and it says so on the report stream"
       (search "holds no REPL to write to" (get-output-stream-string report)))
      (lisp-ai-echo::write-to-repl :request "(+ 1 2)")
      (check-equal "once: the next write to that connection says nothing" (get-output-stream-string report) ""))))

(defun run (directory start stop)
  "Run the checks against a server of the calling project, started by START on a socket in DIRECTORY and ended by STOP.
   Returns the number of failures."
  (setf *failures* 0)
  (progn (run-write-exchange) (run-repl-target))
  (if slynk::*connections*
      (format t "SKIP: an Emacs is connected, so the write to no REPL was not checked~%")
      (check "with no Emacs connected, the write to the REPL does nothing and does not fail"
             (null (lisp-ai-echo::write-to-repl :request "(a line no one should see)"))))
  (let* ((path (namestring (merge-pathnames "test-echo.sock" directory)))
         (server (funcall start path)))
    (unwind-protect (run-through-server path)
      (funcall stop server)
      (when (probe-file path) (delete-file path))))
  *failures*)
