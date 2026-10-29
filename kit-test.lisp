;;;; Tests of kit.lisp, shared by every project under ~/lisp-ai as part of the system lisp-ai-tools/tests: a project calls
;;;; (lisp-ai-kit-tests:run DIRECTORY), which makes a scratch project inside DIRECTORY (the project's run/), points the
;;;; workbench at it for the duration, removes it afterwards and returns how many checks failed, printing a line for each.
;;;; Definitions are made as bin/lisp eval makes them, through lisp-ai-debug:eval-with-debugging, so the ledger records
;;;; them the way it does in a running image.

(defpackage :lisp-ai-kit-tests
  (:use :cl)
  (:export #:run))

(in-package :lisp-ai-kit-tests)

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

(defun write-text (path text)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
    (write-string text out)))

(defun refusal (thunk)
  "The text of the error THUNK signals, or NIL."
  (handler-case (progn (funcall thunk) nil)
    (error (e) (princ-to-string e))))

(defun printed (thunk)
  "What THUNK prints on *standard-output*, and its first value."
  (let (value)
    (values (with-output-to-string (*standard-output*) (setf value (funcall thunk))) value)))

(defun evaluate (text)
  "Make the definitions in TEXT as a request through bin/lisp eval does, in CL-USER."
  (lisp-ai-debug:eval-with-debugging text :cl-user))

(defun sym (name) (intern name :cl-user))

(defparameter *names* '("KIT-TEST-F" "KIT-TEST-G"))

(defun executable-p (path)
  (logtest (sb-posix:stat-mode (sb-posix:stat (namestring path))) sb-posix:s-ixusr))

(defun run-verify ()
  "VERIFY prints each failing check and the line a reader looks for, and is true only when every check passed."
  (multiple-value-bind (text value) (printed (lambda () (eval '(lisp-ai-kit:verify (= 1 1) (stringp "a")))))
    (check-equal "all passing: the line" text (format nil "all checks passed~%"))
    (check-equal "all passing: true" value t))
  (multiple-value-bind (text value) (printed (lambda () (eval '(lisp-ai-kit:verify (= 1 1) (= 1 2)))))
    (check "a failing check is printed" (search "FAIL: (= 1 2)" text))
    (check "and the count" (search "1 check(s) failed" text))
    (check "and not the line" (not (search "all checks passed" text)))
    (check-equal "one failing: false" value nil)))

(defun run-commands (root)
  "WRITE-COMMAND writes an executable script relative to the project that changes to the project's directory and asks the
   image for the function; RUN-COMMAND runs a script with arguments and returns its output, or signals with its standard
   error."
  (let ((script (lisp-ai-kit:write-command "bin/kit-report" (sym "KIT-TEST-F"))))
    (check-equal "the script is where it was asked for" (truename script) (truename (merge-pathnames "bin/kit-report" root)))
    (check "it is executable" (executable-p script))
    (let ((text (uiop:read-file-string script)))
      (check "it changes to the project's directory, one level up" (search "cd \"$(dirname \"$0\")/..\"" text))
      (check "it names the function with its package" (search "COMMON-LISP-USER::KIT-TEST-F" text))
      (check "it asks the image through bin/lisp eval" (search "bin/lisp eval" text))))
  (let ((text (uiop:read-file-string (lisp-ai-kit:write-command "kit-top" (sym "KIT-TEST-F")))))
    (check "a script at the project's top changes to its own directory" (search "cd \"$(dirname \"$0\")/.\"" text)))
  (check "an absolute path is refused"
         (refusal (lambda () (lisp-ai-kit:write-command "/tmp/kit-report" (sym "KIT-TEST-F")))))
  (check "a function that is not a symbol is refused"
         (refusal (lambda () (lisp-ai-kit:write-command "bin/kit-x" (lambda () 1)))))
  (let ((echo (merge-pathnames "bin/kit-echo" root))
        (fail (merge-pathnames "bin/kit-fail" root)))
    (write-text echo (format nil "#!/bin/sh~%echo \"$@\"~%"))
    (write-text fail (format nil "#!/bin/sh~%echo boom >&2~%exit 3~%"))
    (dolist (p (list echo fail))
      (sb-posix:chmod (namestring p) #o755))
    (check-equal "run-command returns what the script printed" (lisp-ai-kit:run-command "bin/kit-echo" "a" "b c")
                 (format nil "a b c~%"))
    (let ((message (refusal (lambda () (lisp-ai-kit:run-command "bin/kit-fail")))))
      (check "a failing script is an error" message)
      (check "that names its status and carries its standard error"
             (and message (search "status 3" message) (search "boom" message))))))

(defun run-source (root)
  "FILE-DEFINITION finds a definition's text by reading the file's forms; SOURCE prints definitions from the ledger or from
   their files, and the forms of a file of src/, and says what it cannot find."
  (let ((file (merge-pathnames "src/k.lisp" root)))
    (write-text file (format nil "(in-package :cl-user)~%~%;; about g~%(defun kit-test-g (x)~%  \"adds one\"~%  (+ x 1))~%"))
    (multiple-value-bind (text package) (lisp-ai-kit:file-definition (sym "KIT-TEST-G") file)
      (check-equal "file-definition: the form's text, without the comment above it" text
                   (format nil "(defun kit-test-g (x)~%  \"adds one\"~%  (+ x 1))"))
      (check-equal "and the package of the file's in-package" package (find-package :cl-user)))
    (check-equal "a name the file does not define gives NIL" (lisp-ai-kit:file-definition (sym "KIT-TEST-NONE") file) nil)
    (load file)
    (let ((text (printed (lambda () (lisp-ai-kit:source "k")))))
      (check "source of a file prints its definitions" (search "(defun kit-test-g (x)" text))
      (check "with the docstring" (search "\"adds one\"" text))
      (check "and without the comment" (not (search ";; about g" text))))
    (check "source of a file that is not there says so"
           (search ";; there is no file src/nope.lisp" (printed (lambda () (lisp-ai-kit:source "nope")))))
    (evaluate "(defun kit-test-f (x) \"doubles\" (* x 2))")
    (let ((text (printed (lambda () (lisp-ai-kit:source (sym "KIT-TEST-F"))))))
      (check "source of a definition made in the image prints it from the ledger" (search "(defun kit-test-f (x)" text)))
    (check "source of a name with no definition says so"
           (search ";; the image has no definition of"
                   (printed (lambda () (lisp-ai-kit:source (sym "KIT-TEST-NONE"))))))))

(defun run-revise ()
  "REVISE replaces the one subform that reads as OLD, evaluates the new definition and records it; %OLD in NEW wraps the
   old subform; an OLD that is not exactly one subform, or text that is not one form, is refused and nothing changes."
  (let ((f (sym "KIT-TEST-F")))
    (let ((text (lisp-ai-kit:revise f "(*   x 2)" "(* x 3)")))
      (check "the new text holds the new part" (search "(* x 3)" text))
      (check "and keeps the docstring" (search "\"doubles\"" text)))
    (check-equal "the image runs the new definition" (funcall f 2) 6)
    (check "the ledger's newest text for it is the new one"
           (search "(* x 3)" (lisp-ai-bench::entry-text (car (last (lisp-ai-bench::entries-named f :state nil))))))
    (lisp-ai-kit:revise f "(* x 3)" "(+ 1 %old)")
    (check-equal "%old wraps the old part" (funcall f 2) 7)
    (check "an OLD that is not a subform is refused" (refusal (lambda () (lisp-ai-kit:revise f "(* x 9)" "(* x 4)"))))
    (check "text that is not one whole form is refused"
           (refusal (lambda () (lisp-ai-kit:revise f "(* x 3)" "(* x 4))"))))
    (check-equal "and nothing changed" (funcall f 2) 7)
    (lisp-ai-kit:revise (sym "KIT-TEST-G") "(+ x 1)" "(+ x 10)")
    (check-equal "a definition known only from its file is revised too" (funcall (sym "KIT-TEST-G") 1) 11)))

(defun run (directory)
  "Run every check with a scratch project inside DIRECTORY; returns the number of failures."
  (setf *failures* 0)
  (let* ((root (merge-pathnames "kit-test-project/" (uiop:ensure-directory-pathname directory)))
         (old-project lisp-ai-bench:*project*)
         (old-ledger (lisp-ai-bench:ledger)))
    (when (probe-file root) (uiop:delete-directory-tree root :validate t))
    (ensure-directories-exist (merge-pathnames "run/" root))
    (unwind-protect
         (progn
           (setf lisp-ai-bench:*project* root)
           (run-verify)
           (run-commands root)
           (run-source root)
           (handler-bind ((sb-kernel:redefinition-warning #'muffle-warning))
             (run-revise)))
      (lisp-ai-bench:forget-all)
      (setf lisp-ai-bench:*project* old-project)
      (setf lisp-ai-bench::*ledger* old-ledger)
      (dolist (name *names*)
        (let ((symbol (sym name)))
          (when (fboundp symbol) (fmakunbound symbol))))
      (when (probe-file root) (uiop:delete-directory-tree root :validate t))))
  *failures*)
