(defsystem "lisp-ai-tools"
  :description "The tools every project under ~/lisp-ai/ shares: form-edit changes a Lisp file as forms."
  :depends-on ("eclector")
  :components ((:file "form-edit")))

;;; Debugging in place needs the build policy debug 3, which each project's .asd sets with RESTRICT-COMPILER-POLICY before its
;;; first defsystem (a project's files compile before this file is found, so the policy cannot be set here). tools/debug-test.lisp
;;; fails when it is missing: it reads a let variable in a frame.
(defsystem "lisp-ai-tools/debug"
  :description "Debugging in place: an error in a request from the socket is held, with its stack, and worked on by id."
  :depends-on ("sb-bsd-sockets" "sb-concurrency" "slynk")
  :components ((:file "debug")))

;;; The profile of the guard and the deny list (tools/profile.lisp, docs/guard-and-deny.md). It needs only JSON, so the guard's
;;; small system can use it.
(defsystem "lisp-ai-tools/profile" :description
 "Logs what the guard and the deny list decide, and whether the command ran, for each project." :depends-on
 ("com.inuoe.jzon" "uiop") :components ((:file "profile")))
