(defsystem "lisp-ai-tools"
  :description
  "The tools every project shares: form-edit changes a Lisp file as forms."
  :depends-on ("eclector")
  :components ((:file "form-edit")))

;;; Debugging in place needs the build policy debug 3, which each
;;; project's .asd sets with RESTRICT-COMPILER-POLICY before its first
;;; defsystem (a project's files compile before this file is found, so
;;; the policy cannot be set here). tools/debug-test.lisp fails when it
;;; is missing: it reads a let variable in a frame.
(defsystem "lisp-ai-tools/debug"
  :description
  "Debugging in place: a failed request is held, with its stack."
  :depends-on ("sb-bsd-sockets" "sb-concurrency" "slynk")
  :components ((:file "debug")))

;;; The profile of the guard and the deny list (tools/profile.lisp,
;;; docs/guard-and-deny.md). It needs only JSON, so the guard's small
;;; system can use it.
(defsystem "lisp-ai-tools/profile"
  :description
  "Logs what the guard and deny list decide, and whether the call ran."
  :depends-on ("lisp-ai-tools/project" "com.inuoe.jzon" "uiop")
  :components ((:file "profile")))

;;; Where a project is and how its data files are read
;;; (tools/project.lisp). The guard and the profile use it.
(defsystem "lisp-ai-tools/project"
  :description
  "A project's directory and the data files in its rules/ and data/."
  :depends-on ("uiop")
  :components ((:file "project")))

;;; Which text-processing programs a shell command runs, and the deny
;;; list made from the same rules (tools/guard.lisp). It knows nothing
;;; of the profiler; lisp-ai-tools/hook joins the two.
(defsystem "lisp-ai-tools/guard"
  :description
  "The text-processing programs a shell command runs; the deny rules."
  :depends-on ("lisp-ai-tools/project" "com.inuoe.jzon" "uiop")
  :components ((:file "guard")))

;;; What Claude Code's PreToolUse hook for Bash prints
;;; (tools/hook.lisp): the guard's verdict, logged and shadowed by the
;;; profile.
(defsystem "lisp-ai-tools/hook"
  :description
  "The hook response: the guard's verdict, logged and shadowed."
  :depends-on ("lisp-ai-tools/guard" "lisp-ai-tools/profile")
  :components ((:file "hook")))

;;; The tests of all the above, as one system, so that a project runs
;;; them by naming it (no path to a file): each file has a package with
;;; RUN, and a project calls (form-edit-tests:run DIRECTORY) and so on
;;; from its own tests (docs: CLAUDE.md of the tree).
(defsystem "lisp-ai-tools/tests"
  :description
  "The tests of the shared tools; a project calls their RUN functions."
  :depends-on ("lisp-ai-tools" "lisp-ai-tools/debug" "lisp-ai-tools/hook"
               "lisp-ai-tools/profile" "sb-posix" "sb-bsd-sockets" "uiop")
  :components ((:file "form-edit-test") (:file "debug-test")
               (:file "profile-test") (:file "guard-test")
               (:file "lisp-script-test")))
