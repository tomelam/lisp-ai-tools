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
 ("lisp-ai-tools/project" "com.inuoe.jzon" "uiop") :components ((:file "profile")))

;;; Where a project is and how its data files are read (tools/project.lisp). The guard and the profile use it.
(defsystem "lisp-ai-tools/project" :description "A project's directory and the data files in its rules/ and data/."
 :depends-on ("uiop") :components ((:file "project")))

;;; Which text-processing programs a shell command runs, and the deny list made from the same rules (tools/guard.lisp).
;;; It knows nothing of the profiler; lisp-ai-tools/hook joins the two.
(defsystem "lisp-ai-tools/guard" :description
 "Which text-processing programs does a shell command run? The permission rules that deny them." :depends-on
 ("lisp-ai-tools/project" "com.inuoe.jzon" "uiop") :components ((:file "guard")))

;;; What Claude Code's PreToolUse hook for Bash prints (tools/hook.lisp): the guard's verdict, logged and shadowed by the profile.
(defsystem "lisp-ai-tools/hook" :description
 "The hook response: the guard's verdict, with the profile's logging and shadow." :depends-on
 ("lisp-ai-tools/guard" "lisp-ai-tools/profile") :components ((:file "hook")))
