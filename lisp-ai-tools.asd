(defsystem "lisp-ai-tools"
  :description
  "The tools every project shares: form-edit changes a Lisp file as forms."
  :depends-on ("eclector")
  :components ((:file "form-edit")))

;;; Where a project is and how its data files are read
;;; (tools/project.lisp). The gate and the workbench use it.
(defsystem "lisp-ai-tools/project"
  :description
  "A project's directory and the data files in its rules/ and data/."
  :depends-on ("uiop")
  :components ((:file "project")))

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

;;; The workbench (tools/bench.lisp, docs/workbench.md): the ledger of
;;; the definitions made in the image through bin/lisp eval, and the
;;; commands that save them to files as typed.
(defsystem "lisp-ai-tools/bench"
  :description
  "The workbench: definitions made in the image, saved to files later."
  :depends-on ("lisp-ai-tools" "lisp-ai-tools/project" "lisp-ai-tools/debug"
               "sb-introspect" "uiop")
  :components ((:file "bench")))

;;; The echo (tools/echo.lisp): every request through bin/lisp eval
;;; and its reply, shown in the Sly REPL as they happen, so that
;;; whoever is attached with Sly watches the conversation between
;;; Claude Code and the image. A project turns it on in bin/serve.lisp.
(defsystem "lisp-ai-tools/echo" :description "The echo: requests and replies shown in the Sly REPL as they happen."
 :depends-on ("lisp-ai-tools/debug" "slynk") :components ((:file "echo")))

;;; The gate (tools/guard.lisp, docs/workbench.md): the one PreToolUse
;;; hook, for Bash and for Edit, Write and MultiEdit. A Lisp file is
;;; changed only through the workbench.
(defsystem "lisp-ai-tools/guard"
  :description
  "The gate: refuse changing Lisp files by any way but the workbench."
  :depends-on ("lisp-ai-tools/project" "com.inuoe.jzon" "uiop")
  :components ((:file "guard")))

;;; The tests of all the above, as one system, so that a project runs
;;; them by naming it (no path to a file): each file has a package with
;;; RUN, and a project calls (form-edit-tests:run DIRECTORY) and so on
;;; from its own tests (docs: CLAUDE.md of the tree).
(defsystem "lisp-ai-tools/tests"
  :description
  "The tests of the shared tools; a project calls their RUN functions."
  :depends-on ("lisp-ai-tools" "lisp-ai-tools/debug" "lisp-ai-tools/bench" "lisp-ai-tools/guard" "lisp-ai-tools/echo" "sb-posix"
               "sb-bsd-sockets" "uiop")
  :components ((:file "form-edit-test") (:file "debug-test") (:file "bench-test") (:file "guard-test") (:file "lisp-script-test")
               (:file "echo-test")))
