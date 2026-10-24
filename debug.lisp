;;;; Debugging in place, for every project (aquarium/docs/debug-in-place.md). An error in a request from the socket does not
;;;; unwind the stack. The request's thread is kept, with its stack, inside Slynk's debugging environment, and later requests
;;;; work on it by id: frames, locals, evaluation in a frame, redefine a function and restart a frame, take a restart. Frames
;;;; belong to the thread that has them, so that thread runs each command itself and sends the answer back through a mailbox.
;;;; A project's package uses this one (so the commands are called by their bare names through bin/lisp eval), its EVAL-REQUEST
;;;; calls EVAL-WITH-DEBUGGING, and its SERVE-CONNECTION binds *REPLY-NOW* with REPLY-NOW-FUNCTION.
(defpackage :lisp-ai-debug
  (:use :cl)
  (:export #:eval-with-debugging #:reply-now-function #:*reply-now* #:*eval-observer* #:*request-observer* #:*request*
   #:condition-text #:debug-frames #:debug-locals #:debug-eval #:debug-result #:debug-restart-frame #:debug-invoke
   #:debug-abandon))
(in-package :lisp-ai-debug)

(defparameter *held-limit* 4
  "Sessions in use at once. One more error is handled as an ordinary ERROR: reply.")

(defparameter *held-timeout* 300
  "Seconds a held thread waits for a command before its request is abandoned (the same as bin/lisp eval's own wait).")

(defparameter *finished-kept* 8
  "Finished sessions whose result has not been read are kept up to this many, oldest dropped first.")

(defparameter *frames-shown* 15
  "Frames in the report of a held error.")

(defvar *reply-now* nil
  "While a request from the socket is evaluated: a function of one string that sends it as the reply at once and ends the
   connection. NIL otherwise (a direct call of a project's EVAL-REQUEST): then an error is never held.")

(defvar *session* nil
  "The session of the request being evaluated, once an error in it has been held.")

(defvar *eval-observer* nil
  "A function called around each top-level form of a request: (funcall observer :before form text) before it is
   evaluated and (funcall observer :after form text) after, TEXT being the form's span of the request as typed, comments
   above it included. The workbench (lisp-ai-bench) sets it to its RECORD-FORM. An error in the observer is printed into
   the reply as a ;; ledger: line and never held.")

(defun observe (when form text reply)
  (when *eval-observer*
    (handler-case (funcall *eval-observer* when form text)
      (error (e) (format reply ";; ledger: ~a~%" (condition-text e))))))

(defvar *request*
  nil
  "The whole text of the request being evaluated, as it was sent, while EVAL-WITH-DEBUGGING runs; NIL outside it.")

(defvar *request-observer*
  nil
  "A function called around each request: (funcall observer :request text) before its forms are evaluated, TEXT being the
   whole request as sent, and (funcall observer :reply text) with the reply as it is sent: the finished reply, or, for an
   error that is held, the HELD report that answers at once (the finished reply of such a request is read later through
   DEBUG-RESULT, whose own reply is observed in turn). The echo to the Sly REPL (echo.lisp) is one. An error in the
   observer is printed as a ;; observer: line, into the reply for :request and to *ERROR-OUTPUT* for :reply, and never
   held.")

(defun observe-request (when text reply)
  (when *request-observer*
    (handler-case (funcall *request-observer* when text)
                  (error (e)
                         (format
                          (if (eq when :request)
                              reply
                              *error-output*)
                          ";; observer: ~a~%" (condition-text e))))))

(defstruct (session (:constructor make-session (id)))
  id
  (state :held)            ; :held (waiting for a command), :running (resumed), :finished
  condition                ; the error that was last held
  restarts                 ; its restarts, ABORT-REQUEST first
  trapped                  ; true when a frame was interrupted by a trap: see TRAPPED-P
  (report "")              ; the text of the last held report
  (output "")              ; what the request printed up to the last report
  result                   ; the whole text of the request, once :finished
  (inbox (sb-concurrency:make-mailbox))
  (outbox (sb-concurrency:make-mailbox)))

(defvar *sessions* (make-hash-table))
(defvar *sessions-lock* (sb-thread:make-mutex :name "debug sessions"))
(defvar *session-counter* 0)

(defun condition-text (condition)
  "CONDITION as text. Printing a condition can itself fail (a malformed format string in its message); that must not
   escape, because an error in a connection thread would end the whole server."
  (handler-case (princ-to-string condition)
    (serious-condition ()
      (format nil "~s (its message could not be printed)" (type-of condition)))))

(defun find-session (id)
  (sb-thread:with-mutex (*sessions-lock*) (gethash id *sessions*)))

(defun open-session ()
  "A new session, or NIL when *HELD-LIMIT* are in use. Finished sessions beyond *FINISHED-KEPT* are dropped."
  (sb-thread:with-mutex (*sessions-lock*)
    (let ((in-use 0) (finished '()))
      (maphash (lambda (id session)
                 (if (eq (session-state session) :finished) (push id finished) (incf in-use)))
               *sessions*)
      (loop for id in (nthcdr *finished-kept* (sort finished #'>)) do (remhash id *sessions*))
      (when (< in-use *held-limit*)
        (let ((session (make-session (incf *session-counter*))))
          (setf (gethash (session-id session) *sessions*) session))))))

(defun forget-session (session)
  (sb-thread:with-mutex (*sessions-lock*) (remhash (session-id session) *sessions*)))

(defun frame-lines (end)
  "The frames 0 up to END of the error being debugged, as (number . text), the text cut to 200 characters."
  (let ((*print-length* 10) (*print-level* 4) (*print-pretty* nil))
    (loop for frame in (slynk-backend:compute-backtrace 0 end)
          for number from 0
          collect (cons number
                        (let ((text (with-output-to-string (stream) (slynk-backend:print-frame frame stream))))
                          (if (> (length text) 200) (concatenate 'string (subseq text 0 200) "...") text))))))

(defun trapped-p ()
  "True when a frame of the stack being debugged was interrupted by a trap: the error is a type error, an unbound variable,
   an undefined function or a wrong argument count, signalled by the machine, not by a call of ERROR. Restarting a frame
   after such an error ends the image once the request's thread finishes (SBCL 2.6.9, arm64; found by trying, with plain
   SBCL and Slynk and none of this code). Leaving by a restart, a throw, is safe."
  (loop for frame = (sb-di:top-frame) then (sb-di:frame-down frame)
        while frame
        thereis (and (typep frame 'sb-di::compiled-frame) (sb-di::compiled-frame-escaped frame))))

(defun held-report (session condition new-output)
  "The text of a held error: what the request printed since the last report, the error, the restarts and the frames."
  (with-output-to-string (report)
    (write-string new-output report)
    (format report "ERROR: ~a~%" (condition-text condition))
    (format report "HELD ~d: the thread is waiting and nothing was unwound. After ~d s without a command it is abandoned.~%"
            (session-id session) *held-timeout*)
    (when (session-trapped session)
      (format report "NOTE: signalled by a trap, so (debug-restart-frame ...) is refused: it would end the image. Fix the code, then (debug-abandon ~d) and run the request again.~%"
              (session-id session)))
    (format report "restarts:~%")
    (loop for restart in (session-restarts session) for number from 0
          do (format report "  ~d ~a  ~a~%" number (restart-name restart) (princ-to-string restart)))
    (format report "frames:~%")
    (let ((frames (frame-lines (1+ *frames-shown*))))
      (loop for (number . text) in frames for count from 0 below *frames-shown* do (format report "  ~d ~a~%" number text))
      (when (> (length frames) *frames-shown*)
        (format report "  ... more: (debug-frames ~d ~d)~%" (session-id session) (* 4 *frames-shown*))))))

(defun hold-error (condition reply)
  "The handler of EVAL-WITH-DEBUGGING for an ERROR. When the error can be held it does not return until a command resumes
   the thread (the stack is then used, or left by a restart); otherwise it returns at once and the error is handled as
   before. A first error answers the socket at once; a later error in the same request only changes the report."
  (let ((session (or *session* (and *reply-now* (open-session)))))
    (when session
      (setf *session* session)
      (let* ((all (compute-restarts condition))
             (ours (find 'abort-request all :key #'restart-name))
             (first-time (null (session-condition session))))
        (setf (session-condition session) condition
              (session-restarts session) (if ours (cons ours (remove ours all)) all))
        (slynk-backend:call-with-debugging-environment
         (lambda ()
           (setf (session-trapped session) (trapped-p))
           (let* ((new-output (get-output-stream-string reply))
                  (report (held-report session condition new-output)))
             (setf (session-output session) (concatenate 'string (session-output session) new-output)
                   (session-report session) report
                   (session-state session) :held)
             (when first-time (observe-request :reply report reply) (funcall *reply-now* report)))
           (loop
             (multiple-value-bind (command found)
                 (sb-concurrency:receive-message (session-inbox session) :timeout *held-timeout*)
               (unless found
                 (format reply "(held session ~d abandoned after ~d s without a command)~%" (session-id session) *held-timeout*)
                 (invoke-restart ours condition))
               ;; A command answers (:reply value), or resumes the thread (:resume thunk): then it says so first, because
               ;; the thunk leaves this loop. If it comes back, nothing was resumed (a frame that cannot be restarted): the
               ;; session is held again, with the reason in front of its report.
               (destructuring-bind (kind . more)
                   (handler-case (funcall command session)
                     (error (e) (list :reply (list :command-failed (condition-text e)))))
                 (ecase kind
                   (:reply (sb-concurrency:send-message (session-outbox session) (first more)))
                   (:resume (setf (session-state session) :running) (sb-concurrency:send-message (session-outbox session) :resuming)
                    (let ((value (funcall (first more))))
                      (setf (session-report session) (format nil ";; not resumed: ~a~%~a" value (session-report session))
                            (session-state session) :held)))))))))))))

(defun finish-session (session text)
  "The request has ended; TEXT is its whole reply, kept for DEBUG-RESULT."
  (setf (session-result session) text
        (session-state session) :finished))

(defun eval-with-debugging (request package)
  "Evaluate the forms in REQUEST in PACKAGE (a package or its name); the reply is what they print followed by one printed
   line per value. An error from the socket is held and answers at once; any other serious condition, an exhausted stack
   included, becomes a line starting ERROR: instead of a dead server. Returns the reply and, when an error was held, its
   session (the reply was then kept for DEBUG-RESULT). *REQUEST-OBSERVER* sees the request first and the reply as it is
   sent."
  (let ((reply (make-string-output-stream)) (*session* nil) (*request* request))
    (observe-request :request request reply)
    (handler-case
     (restart-case (handler-bind ((error (lambda (condition) (hold-error condition reply))))
                     (let ((*package* (find-package package)) (*standard-output* reply) (*error-output* reply))
                       (with-input-from-string (forms request)
                         (loop with start = 0
                               for form = (read forms nil forms)
                               until (eq form forms)
                               do
                                 (let* ((end (file-position forms))
                                        (text (subseq request start (min end (length request)))))
                                   (observe :before form text reply)
                                   (let ((values (multiple-value-list (eval form))))
                                     (observe :after form text reply)
                                     (dolist (value values) (format reply "~s~%" value)))
                                   (setf start end))))))
       (abort-request (&optional condition) :report
        "Unwind the request and reply with the error, as without debugging."
        (format reply "ERROR: ~a~%" (condition-text (or condition (session-condition *session*))))))
     (serious-condition (condition) (format reply "ERROR: ~a~%" (condition-text condition))))
    (let ((text
           (concatenate 'string
                        (if *session*
                            (session-output *session*)
                            "")
                        (get-output-stream-string reply))))
      (if *session*
          (finish-session *session* text)
          (observe-request :reply text reply))
      (values text *session*))))

(defun reply-now-function (stream connection)
  "A function for *REPLY-NOW*: it writes its string to STREAM as the reply, then closes STREAM and the socket CONNECTION."
  (lambda (text)
    (write-string text stream)
    (finish-output stream)
    (ignore-errors (close stream))
    (ignore-errors (sb-bsd-sockets:socket-close connection))))

;;; The commands: ordinary functions, called from another request by id.

(defun ask-session (session command &optional (timeout 10))
  (sb-concurrency:send-message (session-inbox session) command)
  (multiple-value-bind (reply found) (sb-concurrency:receive-message (session-outbox session) :timeout timeout)
    (if found reply (list :command-failed (format nil "no answer from held session ~d within ~d s" (session-id session) timeout)))))

(defmacro with-held-session ((session id) &body body)
  "Run BODY with SESSION bound to the session ID if it is waiting for a command; otherwise say so and return no value."
  `(let ((,session (find-session ,id)))
     (cond ((null ,session) (format t ";; no session ~d~%" ,id) (values))
           ((not (eq (session-state ,session) :held))
            (format t ";; session ~d is ~(~a~), not waiting; (debug-result ~d) waits for it~%" ,id (session-state ,session) ,id)
            (values))
           (t ,@body))))

(defun reply-or-complain (reply)
  "The payload of an (:ok ...) reply, or say why not and return no value."
  (if (eq (first reply) :ok)
      (values-list (rest reply))
      (progn (format t ";; ~a~%" (second reply)) (values))))

(defun debug-frames (id &optional (end 30))
  "The frames 0 up to END of held session ID, as (number . text)."
  (with-held-session (session id)
    (reply-or-complain (ask-session session (lambda (session) (declare (ignore session)) (list :reply (list :ok (frame-lines end))))))))

(defun debug-locals (id frame)
  "The local variables of FRAME in held session ID, as (name value). What the compiler kept depends on the debug 3 policy
   that each project's .asd sets."
  (with-held-session (session id)
    (reply-or-complain
     (ask-session session
                  (lambda (session) (declare (ignore session))
                    (list :reply (list :ok (loop for local in (slynk-backend:frame-locals frame)
                                                 collect (list (getf local :name) (getf local :value))))))))))

(defun debug-eval (id frame form)
  "Evaluate FORM in FRAME of held session ID, so it can use the frame's variables. Returns the values."
  (with-held-session (session id)
    (reply-or-complain
     (ask-session session
                  (lambda (session) (declare (ignore session))
                    (list :reply (cons :ok (multiple-value-list (slynk-backend:eval-in-frame form frame)))))))))

(defun debug-result (id &optional (wait 20))
  "Wait up to WAIT seconds for session ID to stop running; then print its report (held again) or the whole reply of the
   request (finished). The reply of a finished request is read once."
  (let ((session (find-session id)))
    (if (null session)
        (format t ";; no session ~d~%" id)
        (progn
          (loop repeat (ceiling (* wait 20)) while (eq (session-state session) :running) do (sleep 0.05))
          (ecase (session-state session)
            (:running (format t ";; session ~d is still running after ~d s; ask again with (debug-result ~d)~%" id wait id))
            (:held (write-string (session-report session)))
            (:finished (write-string (session-result session)) (forget-session session)))))
    (values)))

(defun resume (id thunk-maker)
  "Tell held session ID to leave the debugger by the function THUNK-MAKER makes (given the session), then wait for what comes."
  (with-held-session (session id)
    (let ((answer (ask-session session (lambda (session) (list :resume (funcall thunk-maker session))))))
      (if (eq answer :resuming)
          (debug-result id)
          (progn (format t ";; ~a~%" (second answer)) (values))))))

(defun debug-restart-frame (id frame)
  "Run FRAME of held session ID again with its original arguments: after the function was redefined, the new one runs."
  (resume id (lambda (session)
               (when (session-trapped session)
                 (error "session ~d: the error was signalled by a trap, and restarting a frame would end the image; fix the code, take (debug-abandon ~d) and run the request again"
                        (session-id session) (session-id session)))
               (lambda () (slynk-backend:restart-frame frame)))))

(defun debug-invoke (id number &rest arguments)
  "Take restart NUMBER (as numbered in the report) of held session ID, with ARGUMENTS if it takes any."
  (resume id (lambda (session)
               (let ((restart (nth number (session-restarts session)))
                     (condition (session-condition session)))
                 (unless restart (error "session ~d has no restart ~d" (session-id session) number))
                 (lambda ()
                   (if (eq (restart-name restart) 'abort-request)
                       (invoke-restart restart condition)
                       (apply #'invoke-restart restart arguments)))))))

(defun debug-abandon (id)
  "Unwind the request of held session ID, as if it had not been held: its reply ends with the ERROR: line."
  (debug-invoke id 0))
