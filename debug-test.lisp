;;;; Tests of debug.lisp, shared by every project under ~/lisp-ai as part of the system lisp-ai-tools/tests: a project
;;;; calls (lisp-ai-debug-tests:run DIRECTORY START STOP), which uses DIRECTORY for the test socket (the project's run/),
;;;; calls (funcall START path) for a running server of the project that listens there, and (funcall STOP server) to end it.
;;;; It returns how many checks failed, printing a line for each. The tests go through a real Unix socket as bin/lisp eval
;;;; uses it, so they check the project's wiring too: a held error answers at once, the commands reach the waiting thread,
;;;; a redefinition is picked up by a restarted frame, the limits hold.

(defpackage :lisp-ai-debug-tests
  (:use :cl)
  (:export #:run))

(in-package :lisp-ai-debug-tests)

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
  "Send TEXT to the server at PATH as bin/lisp eval does (the write side closed after it) and return the whole reply."
  (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (unwind-protect
         (progn
          (sb-bsd-sockets:socket-connect socket path)
          (let ((stream (sb-bsd-sockets:socket-make-stream socket :input t :output t :element-type 'character
                                                                  :external-format :utf-8 :buffering :full)))
            (write-string text stream)
            (finish-output stream)
            (sb-bsd-sockets:socket-shutdown socket :direction :output)
            (with-output-to-string (reply)
              (loop for char = (read-char stream nil) while char do (write-char char reply)))))
      (sb-bsd-sockets:socket-close socket))))

(defun held-id (reply)
  "The number in the line HELD n of a REPLY, or NIL when there is none."
  (let ((start (search "HELD " reply)))
    (and start (parse-integer reply :start (+ start 5) :junk-allowed t))))

(defmacro printed (&body body)
  "What BODY prints."
  `(with-output-to-string (*standard-output*) ,@body))

(defun frame-number (id name)
  "The number of the first frame of held session ID whose text mentions NAME."
  (car (find-if (lambda (frame) (search name (cdr frame))) (lisp-ai-debug:debug-frames id 30))))

;;; Functions that fail. Each calls the failing function inside a LIST, so its frame is not replaced by a tail call.
(defun debug-broken (x) (let ((y (* x 2))) (list :broken (/ 100 (- y 4)))))
(defun debug-outer (x) (list :outer (debug-broken x)))
(defun debug-careful (x) (list :careful (restart-case (/ 1 x) (use-value (v) v))))
(defvar *still-broken* t)
(defun debug-flaky (x) (list :flaky (if *still-broken* (error "still broken") x)))
;;; A TYPE-ERROR comes from a trap in compiled code, not from a call of ERROR as the others do.
(defvar *addend* :a)
(defun debug-typed (x) (list :typed (+ x *addend*)))

(defun run (directory start stop)
  (setf *failures* 0)
  (let* ((path (namestring (merge-pathnames "test-debug.sock" directory)))
         (server (funcall start path))
         (timeout lisp-ai-debug::*held-timeout*))
    (unwind-protect
         (progn
          (check-equal "a request that succeeds is answered as before" (socket-request path "(+ 1 2)") (format nil "3~%"))
          ;; an error is held: the reply is at once, and the thread waits
          (let* ((reply (socket-request path "(progn (format t \"before~%\") (lisp-ai-debug-tests::debug-outer 2))"))
                 (id (held-id reply)))
            (check "a held error answers at once, with what was printed first, then the error"
                   (eql 0 (search (format nil "before~%ERROR: arithmetic error DIVISION-BY-ZERO") reply)))
            (check "the reply names the session, the restarts and the frames"
                   (and id (search "restarts:" reply) (search "0 ABORT-REQUEST" reply) (search "frames:" reply)
                        (search "DEBUG-BROKEN" reply)))
            (check "an error that is not from a trap has no note about restarting" (not (search "NOTE:" reply)))
            (when id
              (let ((frame (frame-number id "DEBUG-BROKEN")))
                (check-equal "the locals of the failing function's frame"
                             (mapcar (lambda (local) (list (symbol-name (first local)) (second local)))
                                     (lisp-ai-debug:debug-locals id frame))
                             '(("X" 2) ("Y" 4)))
                (check-equal "a form evaluated in that frame sees its variables"
                             (multiple-value-list (lisp-ai-debug:debug-eval id frame '(list x (* x 100))))
                             '((2 200)))
                ;; fix the definition in the image, then run the frame again
                (setf (fdefinition 'debug-broken) (lambda (x) (let ((y (* x 2))) (list :broken (/ 100 (- y 5))))))
                (check-equal "after the fix, restarting the frame finishes the original request with the new answer"
                             (printed (lisp-ai-debug:debug-restart-frame id frame))
                             (format nil "before~%(:OUTER (:BROKEN -100))~%"))
                (check "and the session is gone once its result was read" (null (lisp-ai-debug::find-session id))))))
          ;; a restart the code establishes, taken with an argument
          (let* ((reply (socket-request path "(lisp-ai-debug-tests::debug-careful 0)"))
                 (id (held-id reply)))
            (check "the code's own restart is listed" (search "1 USE-VALUE" reply))
            (when id
              (check-equal "taking it with a value finishes the request with that value"
                           (printed (lisp-ai-debug:debug-invoke id 1 42)) (format nil "(:CAREFUL 42)~%"))))
          ;; an error that comes back after the restart is held again, under the same id
          (let* ((reply (socket-request path "(lisp-ai-debug-tests::debug-flaky 7)"))
                 (id (held-id reply)))
            (when id
              (let ((frame (frame-number id "DEBUG-FLAKY")))
                (check "restarting without a fix holds the new error under the same id"
                       (eql id (held-id (printed (lisp-ai-debug:debug-restart-frame id frame)))))
                (setf *still-broken* nil)
                (check-equal "and once fixed the request finishes"
                             (printed (lisp-ai-debug:debug-restart-frame id frame)) (format nil "(:FLAKY 7)~%")))))
          ;; a TYPE-ERROR is signalled by a trap: restarting a frame after it would end the image, so it is refused
          (let* ((reply (socket-request path "(lisp-ai-debug-tests::debug-typed 7)"))
                 (id (held-id reply)))
            (check "a type error is held, with the note that it came from a trap"
                   (and id (search "is not of type" reply) (search "NOTE: signalled by a trap" reply)))
            (when id
              (let ((frame (frame-number id "DEBUG-TYPED")))
                (check "restarting a frame of it is refused, and says why"
                       (search "would end the image" (printed (lisp-ai-debug:debug-restart-frame id frame))))
                (check "the session is still waiting after the refusal" (search "HELD " (printed (lisp-ai-debug:debug-result id 1))))
                (check "abandoning it ends the request with the ERROR: line"
                       (eql 0 (search "ERROR: The value" (printed (lisp-ai-debug:debug-abandon id)))))))
            (sleep 1)
            (check-equal "and the image still answers afterwards" (socket-request path "(+ 1 2)") (format nil "3~%")))
          ;; abandoning unwinds as before
          (let* ((reply (socket-request path "(error \"boom\")"))
                 (id (held-id reply)))
            (when id
              (check-equal "abandoning ends the request with the ERROR: line" (printed (lisp-ai-debug:debug-abandon id))
                           (format nil "ERROR: boom~%"))))
          ;; the limit: the fifth error is not held
          (let* ((replies (loop repeat (1+ lisp-ai-debug::*held-limit*) collect (socket-request path "(error \"x\")")))
                 (ids (remove nil (mapcar #'held-id replies))))
            (check-equal "up to the limit errors are held" (length ids) lisp-ai-debug::*held-limit*)
            (check-equal "one more is an ordinary ERROR: reply" (car (last replies)) (format nil "ERROR: x~%"))
            (dolist (id ids) (printed (lisp-ai-debug:debug-abandon id)))
            (check-equal "all sessions end" (hash-table-count lisp-ai-debug::*sessions*) 0))
          ;; a thread nobody answers is abandoned after the timeout
          (setf lisp-ai-debug::*held-timeout* 1)
          (let* ((reply (socket-request path "(error \"slow\")"))
                 (id (held-id reply)))
            (sleep 2)
            (when id
              (let ((text (printed (lisp-ai-debug:debug-result id 3))))
                (check "an unanswered thread is abandoned, and says so" (search "abandoned after 1 s" text))
                (check "and its reply ends with the error" (search "ERROR: slow" text)))))
          (check-equal "a session that does not exist is said so" (printed (lisp-ai-debug:debug-frames 99999))
                       (format nil ";; no session 99999~%")))
      (setf lisp-ai-debug::*held-timeout* timeout)
      (funcall stop server)
      (when (probe-file path) (delete-file path))))
  *failures*)
