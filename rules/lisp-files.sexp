;;; What counts as Lisp for the guard (tools/guard.lisp, lisp-ai-guard:lisp-files). :code-suffixes: the Edit, MultiEdit and Write
;;; tools may not change such a file (Write may create one): Lisp code is changed as forms, through the image.
;;; :code-suffixes with :data-suffixes, and a path through one of :directories: a text-processing program (rules/forbidden-programs.sexp)
;;; may not read or change them. On any other file those programs are allowed.

(:code-suffixes ("lisp" "asd") :data-suffixes ("sexp") :directories ("src" "tests" "tools"))
