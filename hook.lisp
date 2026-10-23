;;;; The hook: what Claude Code's PreToolUse hook prints, for Bash and for the Edit, MultiEdit and Write tools. The guard
;;;; (tools/guard.lisp) judges the call; the profiler (tools/profile.lisp) logs the judgement while profiling is on, and while
;;;; its shadow is active lets a command through that the guard would deny. Neither of the two knows the other: this is where
;;;; they meet.
(defpackage :lisp-ai-hook
  (:use :cl)
  (:export #:hook-response #:respond #:hook-payload-p #:socket-reply #:socket-marker))
(in-package :lisp-ai-hook)

(defun shadow-allows-p (command project)
  "True when the profile's shadow is active for PROJECT and covers COMMAND: the guard then logs a denial but lets the call
   through, so the deny list judges it too. The shadow covers COMMAND if it is in rules/profile-corpus.sexp, or if the limit
   is off. An error here means no shadow: the call is denied."
  (handler-case
   (and (lisp-ai-profile:shadow-active-p project)
        (or (not (lisp-ai-profile:limit project)) (lisp-ai-profile:corpus-command-p project command)))
   (error nil nil)))

(defun profile-guard-call (hook-json forbidden refused shadowed project)
  "Log the guard's judgement of the call in HOOK-JSON while PROJECT's profile is on. A failure to log is logged if it can be,
   and never changes the verdict."
  (handler-case
   (multiple-value-bind (id command)
       (lisp-ai-guard:hook-bash-call hook-json)
     (when (and command (lisp-ai-profile:profiling project))
       (lisp-ai-profile:record-guard project :id id :command command :verdict
                                     (if refused
                                         :deny
                                         :allow)
                                     :programs (lisp-ai-guard:forbidden-uses command forbidden) :shadowed shadowed)))
   (error (e) (ignore-errors (lisp-ai-profile:record-error project (princ-to-string e))))))

(defun hook-response (hook-json forbidden project)
  "What the hook prints for HOOK-JSON: the empty string to allow, a deny decision to refuse. While PROJECT's profile is on
   (tools/profile.lisp) the judgement is logged; while its shadow is active, a call the guard would deny is allowed instead,
   and logged as shadowed, if its command is in rules/profile-corpus.sexp or the limit is off."
  (let* ((refusal (lisp-ai-guard:hook-verdict hook-json forbidden (lisp-ai-guard:lisp-files project)))
         (command (nth-value 1 (lisp-ai-guard:hook-bash-call hook-json)))
         (shadowed (and refusal command (shadow-allows-p command project))))
    (profile-guard-call hook-json forbidden refusal shadowed project)
    (if (and refusal (not shadowed))
        (lisp-ai-guard:deny-json refusal)
        "")))

(defun respond (project hook-json)
  "HOOK-RESPONSE for HOOK-JSON with the forbidden programs read from PROJECT's rules/forbidden-programs.sexp."
  (hook-response hook-json (lisp-ai-guard:forbidden-programs project) project))

;;; The always-on image answers hook payloads on its socket, which tools/guard asks first. A reply that is empty or has no
;;; marker is not an answer (a dropped connection, a server that read the payload as Lisp), so tools/guard then takes the
;;; slow path instead of reading silence as permission.
(defparameter *socket-marker* "OK:" "What a reply from the socket starts with when the hook judged the payload.")

(defun socket-marker () *socket-marker*)

(defun hook-payload-p (request)
  "True when the text REQUEST is a Claude Code hook payload (JSON, which starts with a brace) and not Lisp."
  (let ((start (position-if-not (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return))) request)))
    (and start (char= (char request start) #\{))))

(defun socket-reply (request forbidden project)
  "The reply to a hook payload REQUEST on the socket: the marker, then HOOK-RESPONSE."
  (concatenate 'string *socket-marker* (hook-response request forbidden project)))
