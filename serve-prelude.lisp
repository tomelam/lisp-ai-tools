;;;; tools/serve-prelude.lisp -- the first half of every project's bin/serve.lisp, which tools/lisp-server loads in the
;;;; always-on image. Loaded from there with (load (merge-pathnames "../../tools/serve-prelude.lisp" *load-truename*)).
;;;; The project's name is the name of its directory (the working directory of the image), which is also the name of its
;;;; .asd and of its package. This loads the system, makes the project's package the one Sly's REPLs start in, loads the
;;;; editor system <name>/form-edit and the debugger lisp-ai-tools/debug (tools/debug.lisp), and makes the package SCRATCH,
;;;; which uses the debugger's package as the project's package does. What is particular to the project follows in its
;;;; bin/serve.lisp.

(let* ((name (car (last (pathname-directory (uiop:getcwd)))))
       (package (string-upcase name)))
  (asdf:load-system name)
  ;; Sly's REPLs take the global value of *package*, so they start in the project's package and need no in-package.
  (setf (sb-ext:symbol-global-value '*package*) (find-package package))
  ;; The editor stays loaded for the life of the image, so changing a file costs no start-up. SCRATCH is where forms are
  ;; tried before they go to a file; form-edit refuses a form with a symbol of it.
  (asdf/operate:load-system (concatenate 'string name "/form-edit"))
  (progn
   (asdf/operate:load-system "lisp-ai-tools/debug")
   (make-package :scratch :use (list :cl (find-package package) :lisp-ai-debug))))
