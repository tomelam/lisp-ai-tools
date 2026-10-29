;;;; The conversation between Claude Code and the image, shown in the Sly REPL as it happens: each request that comes
;;;; through bin/lisp eval, as typed, after a prompt with the speaker's name, then the reply as it is sent back. Whoever is
;;;; attached with Sly sees both sides in the REPL, while the source file in the other window moves only when a definition
;;;; is saved. The hook is LISP-AI-DEBUG:*REQUEST-OBSERVER*. The two probes the script lisp sends on its own with every
;;;; call (is the image alive, what is unsaved) are not shown. A project turns it on with (lisp-ai-echo:start) in its
;;;; bin/serve.lisp.
(defpackage :lisp-ai-echo
  (:use :cl)
  (:export #:start #:stop #:write-exchange #:*speaker* #:*silent-requests* #:*write*))
(in-package :lisp-ai-echo)

(defparameter *speaker* "claude"
  "The name shown as a prompt before each request: claude> (form).")

(defparameter *silent-requests* '("T" "(lisp-ai-bench:pending-names)")
  "Requests not shown: the ones the script lisp sends for its own bookkeeping with every call, to see that the image is
   alive and what it holds unsaved.")

(defun silent-p (request)
  "True when REQUEST, the whole text of a request, is one of *SILENT-REQUESTS*, whitespace around it aside."
  (and request
       (member (string-trim '(#\Space #\Newline #\Tab #\Return) request) *silent-requests* :test #'string=)))

(defun write-exchange (stream when text)
  "Write one side of an exchange to STREAM, on lines of its own: for :REQUEST, TEXT (the request as typed) after the
   prompt *SPEAKER*>; for :REPLY, TEXT as it is, ended with a newline when it lacks one. A reply that is empty (a request
   that printed nothing and returned no value) writes nothing."
  (ecase when
    (:request
     (fresh-line stream)
     (format stream "~a> ~a~%" *speaker* (string-right-trim '(#\Space #\Newline #\Tab #\Return) text)))
    (:reply
     (unless (zerop (length text))
       (fresh-line stream)
       (write-string text stream)
       (fresh-line stream)))))

(defun repl-target ()
  "The Sly REPL to write to: in the newest Emacs connection, the output stream of its first REPL channel (a channel that
   is a listener, as Slynk's REPLs are), else of its first listener. Second value, that connection, also when it has no
   REPL to write to; NIL and NIL when no Emacs is connected. The channel comes first because on 2026-10-29 the aquarium
   demo's test saw a connection whose REPL answered while it had no listener, and the echo, which looked only for the
   listener, wrote nothing and said nothing."
  (let* ((connection (first slynk::*connections*))
         (listener
          (and connection
               (or
                (find-if (lambda (channel) (typep channel 'slynk::listener)) (slynk::connection-channels connection))
                (slynk::default-listener connection)))))
    (values (and listener (slynk::listener-out listener)) connection)))

(defvar *unshown*
  'nil
  "The Emacs connections already reported as having no REPL to write to, so that each is reported once.")

(defvar *report*
  sb-sys:*stderr*
  "Where the echo says that it cannot show requests: the image's own standard error, which bin/lisp start sends to
   run/lisp-server.log, not the reply, since programs read replies (2026-10-29: a ;; observer: line in the reply of
   (pending) stopped the aquarium demo from starting). The tests bind it to a stream of their own.")

(defun write-to-repl (when text)
  "Write one side of an exchange to the Sly REPL, inside its connection: that is what sends the text on to Emacs from a
   thread that is not Slynk's own (outside it the stream takes the text and nothing arrives). Nothing when no Emacs is
   connected. When one is connected but holds no REPL to write to, say so on *REPORT*, once for each connection, and
   write nothing. Slynk was seen, after Emacs reconnected at once, to list only the old connection, emptied, while the
   REPL ran on a new one it did not list (2026-10-29)."
  (multiple-value-bind (stream connection)
      (repl-target)
    (cond
     (stream
      (slynk::with-connection (connection)
        (write-exchange stream when text)
        (finish-output stream)))
     ((and connection (not (member connection *unshown*))) (push connection *unshown*)
      (format *report*
              "~&;; echo: the Sly connection ~a holds no REPL to write to, so requests are not shown in Emacs~%"
              connection)
      (finish-output *report*)))))

(defvar *write* 'write-to-repl
  "The function called with (WHEN TEXT) for each side of an exchange that is shown. WRITE-TO-REPL writes to the Sly REPL;
   the tests set it to one of their own, since no Emacs is attached to a test.")

(defun echo (when text)
  "The request observer: show the request and its reply through *WRITE*, unless the request is one of the silent ones."
  (unless (silent-p lisp-ai-debug:*request*)
    (funcall *write* when text)))

(defun start (&key (speaker *speaker*))
  "Show every request and its reply in the Sly REPL from now on, the requests after the prompt SPEAKER>. Returns nothing."
  (setf *speaker* speaker
        lisp-ai-debug:*request-observer* 'echo)
  (values))

(defun stop ()
  "Show nothing from now on. Returns nothing."
  (setf lisp-ai-debug:*request-observer* nil)
  (values))
