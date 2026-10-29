;;;; Tests of the script lisp (its word eval), shared by every project under ~/lisp-ai as part of the system lisp-ai-tools/tests:
;;;; a project calls (lisp-ai-script-tests:run DIRECTORY START STOP), which makes a scratch project inside DIRECTORY (the
;;;; project's run/), calls (funcall START path) for a server of the project that listens on a socket there and (funcall STOP
;;;; server) to end it, and returns how many checks failed, printing a line for each. A fake qlot (the script runs $QLOT to start
;;;; the image) stands in for the start. Three cases: a healthy server is asked and nothing is started; a server that is down,
;;;; with a stale socket left behind, is reported as down, started, and then gives its answer; a start that fails is a loud
;;;; failure, not a silent one.

(defpackage :lisp-ai-script-tests
  (:use :cl)
  (:export #:run))

(in-package :lisp-ai-script-tests)

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

(defvar *script* (namestring (asdf:system-relative-pathname "lisp-ai-tools" "lisp"))
  "The script under test; the one thing here that needs the place of the tools.")

(defun write-script (path lines)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (format out "#!/bin/sh~%~{~a~%~}" lines))
  (sb-posix:chmod path #o755))

(defun fake-qlot (path start-lines)
  "A stand-in for qlot at PATH: whatever the script asks of it, it runs START-LINES in the project's directory."
  (write-script path start-lines))

(defparameter *fake-start-and-wait*
  '(": > run/started" "echo $$ > run/lisp.pid" "sleep 60")
  "A stand-in start: note that it ran, record the pid of this process as the server's, and stay alive.")

(defun eval-command (qlot &rest forms)
  "The script's word eval with FORMS as its arguments, (* 6 7) when none are given."
  (list* "env" (format nil "QLOT=~a" qlot) "sh" *script* "scratch" "eval" (or forms (list "(* 6 7)"))))

(defun eval-command-bare (qlot)
  "The script's word eval with no form at all."
  (list "env" (format nil "QLOT=~a" qlot) "sh" *script* "scratch" "eval"))

(defun run-eval (top start stop)
  (let* ((root (concatenate 'string top "project/"))
         (qlot (concatenate 'string top "fake-qlot"))
         (path (concatenate 'string root "run/lisp.sock")))
    (flet ((tree (start-lines)
             (when (probe-file top) (uiop:delete-directory-tree (pathname top) :validate t))
             (ensure-directories-exist (concatenate 'string root "run/"))
             (fake-qlot qlot start-lines))
           (marker (name) (probe-file (concatenate 'string root "run/" name)))
           (kill-fake ()
             (let ((pidfile (concatenate 'string root "run/lisp.pid")))
               (when (probe-file pidfile)
                 (ignore-errors (sb-posix:kill (parse-integer (uiop:read-file-string pidfile) :junk-allowed t) 15))))))
      ;; 1: a healthy server is asked, and nothing is started
      (tree *fake-start-and-wait*)
      (with-open-file (out (concatenate 'string root "run/lisp.pid") :direction :output :if-exists :supersede)
        (format out "~d~%" (sb-posix:getpid)))
      (let ((server (funcall start path)))
        (unwind-protect
             (multiple-value-bind (out err code)
                 (uiop:run-program (eval-command qlot) :directory root :output :string :error-output :string
                                                        :ignore-error-status t)
               (check-equal "eval of a healthy server prints the value" out (format nil "42~%"))
               (check-equal "eval of a healthy server says nothing on stderr" err "")
               (check-equal "eval of a healthy server exits 0" code 0)
               (progn
                (check "eval of a healthy server starts nothing" (not (marker "started")))
                (multiple-value-bind (out err code)
                    (uiop/run-program:run-program (eval-command qlot "(* 6 7)" "(+ 1 2)") :directory root :output :string
                                                  :error-output :string :ignore-error-status t)
                  (check-equal "eval with two forms prints both values, in order" out (format nil "42~%3~%"))
                  (check-equal "eval with two forms says nothing on stderr" err "")
                  (check-equal "eval with two forms exits 0" code 0))
                (multiple-value-bind (out err code)
                    (uiop/run-program:run-program (eval-command-bare qlot) :directory root :output :string :error-output :string
                                                  :ignore-error-status t)
                  (check-equal "eval with no form prints nothing" out "")
                  (check-equal "eval with no form exits 2" code 2)
                  (check "eval with no form says usage" (search "usage" err)))))
          (funcall stop server)))
      ;; 2: a server that is down, with a stale socket left behind, is started, and then answers
      (tree *fake-start-and-wait*)
      (let ((stale (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
        (sb-bsd-sockets:socket-bind stale path)
        (sb-bsd-sockets:socket-close stale))
      (check "the stale socket is there before eval runs" (probe-file path))
      (let ((process (uiop:launch-program (eval-command qlot) :directory root :output :stream :error-output :stream))
            (server nil))
        (unwind-protect
             (progn
               (loop repeat 100 until (marker "started") do (sleep 0.1))
               (check "eval started the server that was down" (marker "started"))
               (setf server (funcall start path))
               (let ((code (uiop:wait-process process)))
                 (check-equal "eval after a start prints the value"
                              (uiop:slurp-input-stream 'string (uiop:process-info-output process)) (format nil "42~%"))
                 (let ((err (uiop:slurp-input-stream 'string (uiop:process-info-error-output process))))
                   (check "eval says on stderr that the server was down" (search "was down" err))
                   (check "stderr holds what the start said, stdout does not" (search "lisp-server started" err)))
                 (check-equal "eval after a start exits 0" code 0)))
          (when server (funcall stop server))
          (kill-fake)))
      ;; 3: a start that fails is a loud failure
      (tree '("echo 'stub: did not come up' >&2" "exit 1"))
      (multiple-value-bind (out err code)
          (uiop:run-program (eval-command qlot) :directory root :output :string :error-output :string
                                                 :ignore-error-status t)
        (check-equal "a failed start prints no value" out "")
        (check "a failed start exits non-zero" (and (integerp code) (plusp code)))
        (check "a failed start says why" (and (search "was down" err) (search "could not be started" err)))))))

(defun run-mcp (top start stop)
  "The script's word mcp: the door for Claude Code, an MCP server on stdin and stdout, asks a healthy server: it lists
   the one tool eval, answers a call with the image's reply, and refuses empty code as an error, sending nothing."
  (let* ((root (concatenate 'string top "project/")) (path (concatenate 'string root "run/lisp.sock")))
    (when (probe-file top) (uiop/filesystem:delete-directory-tree (pathname top) :validate t))
    (ensure-directories-exist (concatenate 'string root "run/"))
    (let ((server (funcall start path)))
      (unwind-protect
          (multiple-value-bind (out err code)
              (uiop/run-program:run-program (list "sh" *script* "scratch" "mcp") :directory root :output :string
                                            :error-output :string :ignore-error-status t :input
                                            (make-string-input-stream
                                             (format nil "~{~a~%~}"
                                                     (list
                                                      "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}"
                                                      "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}"
                                                      "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\"}"
                                                      "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\"params\":{\"name\":\"eval\",\"arguments\":{\"code\":\"(* 6 7)\"}}}"
                                                      "{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"tools/call\",\"params\":{\"name\":\"eval\",\"arguments\":{\"code\":\" \"}}}"))))
            (let ((replies
                   (mapcar #'com.inuoe.jzon:parse
                           (uiop/utility:split-string (string-right-trim '(#\Newline) out) :separator '(#\Newline)))))
              (flet ((result (id)
                       (gethash "result" (find id replies :key (lambda (r) (gethash "id" r))))))
                (check-equal "mcp exits 0 at the end of its input" code 0)
                (check-equal "mcp writes nothing on stderr" err "")
                (check-equal "mcp answers the four requests, not the notification" (length replies) 4)
                (check-equal "mcp lists one tool, eval"
                 (map 'list (lambda (tool) (gethash "name" tool)) (gethash "tools" (result 2))) '("eval"))
                (check-equal "mcp's eval gives the image's reply"
                 (gethash "text" (aref (gethash "content" (result 3)) 0)) (format nil "42~%"))
                (check-equal "mcp's eval of the image's reply is not an error" (gethash "isError" (result 3)) nil)
                (check-equal "mcp refuses empty code as an error" (gethash "isError" (result 4)) t))))
        (funcall stop server)))))

(defun run (directory start stop)
  "Run the checks with a scratch project inside DIRECTORY; START and STOP start and end a server of the calling project.
   Returns the number of failures."
  (setf *failures* 0)
  (let ((top (namestring (merge-pathnames "script-test/" (uiop:ensure-directory-pathname directory)))))
    (unwind-protect (progn (run-eval top start stop) (run-mcp top start stop))
      (when (probe-file top) (uiop:delete-directory-tree (pathname top) :validate t))))
  *failures*)
