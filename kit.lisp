;;;; The kit, for every project: the small commands a session in the image keeps reaching for, first made for the
;;;; hackathon's Lisp workspace (2026-10-26 to 10-28) and kept there in its src/package.lisp; here for every project. A
;;;; project's package uses this one, so they are called by their bare names:
;;;;   (verify form ...)                     checks, each failing one printed, then "all checks passed" or how many failed
;;;;   (write-command path 'function)        a shell script, path relative to the project, that the image answers
;;;;   (run-command path args...)            run such a script, its standard output as a string
;;;;   (source 'name ...) (source "file")    definitions as the image holds them, printed as forms
;;;;   (revise 'name "old form" "new form")  change one subform of a definition, matched as a form, and evaluate it
;;;; The image's door for Claude Code as an MCP server is the script mcp-door beside this file (bin/lisp mcp).
(defpackage :lisp-ai-kit
  (:use :cl)
  (:export #:verify #:write-command #:run-command #:source #:revise #:file-definition #:definition-text))
(in-package :lisp-ai-kit)

(defun project-file (path)
  "PATH, relative to the project this image serves, as an absolute pathname."
  (merge-pathnames path (lisp-ai-bench::project-directory)))

;;; One check idiom: a check is a function whose body is one VERIFY. The line it prints is what a reader of the reply
;;; looks for (the hackathon's scorecard reads its first appearance as the moment the program first passed).
(defmacro verify (&rest forms)
  "Run each FORM as a check: print every one that fails, then the line all checks passed or how many failed. True when
   all passed."
  `(let ((failed 0))
     ,@(loop for form in forms
             collect `(unless ,form (incf failed) (format t "FAIL: ~s~%" ',form)))
     (if (zerop failed)
         (format t "all checks passed~%")
         (format t "~d check(s) failed~%" failed))
     (zerop failed)))

;;; A command a project offers is a shell script that asks the image. WRITE-COMMAND writes it from the image and
;;; RUN-COMMAND runs it from the image, so a program, its command and the check that runs the command can go in one
;;; request: no turn goes to quoting a request inside a shell script by hand.
(defun write-command (path function)
  "Write the shell script PATH, relative to this project (bin/report, say), that prints what FUNCTION, a symbol, returns
   when called with the script's arguments as strings, once and nothing else. When the function signals, the script
   prints the condition on standard error and exits with status 1, and no debugger session is held: nobody at a command
   line can work in one. The script changes to this project's directory first, so a relative argument is read from
   there. Make it executable and return its full name. Each run is one request to the image through bin/lisp eval,
   about 50 ms."
  (let ((relative (pathname path)))
    (assert (member (first (pathname-directory relative)) '(nil :relative)) (path)
            "WRITE-COMMAND takes a path relative to this project, like bin/report, not ~s" path)
    (assert (symbolp function) (function) "WRITE-COMMAND takes the function's name, 'report, not ~s" function)
    (let ((script (project-file relative))
          (up (format nil "~{~a~^/~}" (make-list (length (rest (pathname-directory relative))) :initial-element "..")))
          (name (let ((*package* (find-package :keyword)))
                  (prin1-to-string function))))
      (ensure-directories-exist script)
      (with-open-file (out script :direction :output :if-exists :supersede)
        (format out "#!/bin/sh
# Written by lisp-ai-kit:write-command: prints what ~a returns for this script's arguments; when it signals, the
# condition goes to standard error and the status is 1.
cd \"$(dirname \"$0\")/~a\" || exit 1
request=\"(handler-case (progn (princ (~a\"
for argument in \"$@\"; do request=\"$request \\\"$argument\\\"\"; done
reply=$(bin/lisp eval \"$request)) (terpri) (values)) (serious-condition (c) (format t \\\"write-command: ~~a~~%\\\" c) (values)))\") || exit 1
case $reply in \"write-command: \"*) printf '%s\\n' \"$reply\" >&2; exit 1 ;; esac
printf '%s\\n' \"$reply\"
"
                name (if (string= up "") "." up) name))
      (sb-posix:chmod (namestring script)
                      (logior sb-posix:s-irusr sb-posix:s-iwusr sb-posix:s-ixusr sb-posix:s-irgrp sb-posix:s-ixgrp
                              sb-posix:s-iroth sb-posix:s-ixoth))
      (namestring script))))

(defun run-command (path &rest arguments)
  "Run the script PATH, relative to this project, with ARGUMENTS (strings), and return what it printed on standard
   output, as a string. Signal an error that carries its standard error when it exits with a status other than 0."
  (multiple-value-bind (output error-output status)
      (uiop:run-program (cons (namestring (project-file path)) arguments)
                        :output :string :error-output :string :ignore-error-status t)
    (unless (zerop status) (error "~a exited with status ~d: ~a" path status error-output))
    output))

;;; Reading a definition's source from what the image knows: the ledger's text for one made in the image, else its form
;;; in the file SBCL recorded for it, found by reading the file's forms (a definition saved from the image is recorded at
;;; offset 0, its place kept elsewhere: 2026-10-27).
(defun file-definition (name file)
  "The text of the one top-level form of FILE that defines NAME, found by reading the file's forms, not at the offset the
   image records. Second value, the package of the file's last in-package before it, the package to read the text in.
   NIL when no form or more than one is named NAME."
  (let ((text (uiop:read-file-string file))
        (package (find-package :cl-user)))
    (form-edit::with-scratch-package (scratch)
      (let* ((nodes (form-edit::read-nodes text scratch))
             (named (form-edit::forms-matching nodes (symbol-name name) scratch)))
        (when (= (length named) 1)
          (loop for node in nodes
                for e = (form-edit::node-expression node)
                until (eq node (first named))
                when (and (consp e) (symbolp (car e)) (string= (symbol-name (car e)) "IN-PACKAGE"))
                  do (setf package (find-package (second e))))
          (values (subseq text (form-edit::node-start (first named)) (form-edit::node-end (first named))) package))))))

(defun definition-text (name)
  "The source text of the definition NAME: that of its newest entry in the ledger (saved or not), else its form in the
   file the image records for it. Second value, the package to read it in. An error when the image knows neither."
  (let ((entry (car (last (lisp-ai-bench::entries-named name :state nil)))))
    (if entry
        (values (lisp-ai-bench::entry-text entry) (lisp-ai-bench::entry-package entry))
        (let ((file (lisp-ai-bench:where name)))
          (multiple-value-bind (text package) (and (pathnamep file) (file-definition name file))
            (if text
                (values text package)
                (error "The image has no source text for ~s: define it whole" name)))))))

(defun source (&rest names)
  "Print the definitions the image holds, as forms: for a symbol, its definitions; for a string, every form of the
   project's file of that name under src/ (\"aquarium\" is src/aquarium.lisp), in the file's order. A definition made
   or changed in the image and not yet saved is printed from the form the ledger keeps; any other is read by the Lisp
   reader from the file SBCL recorded for it. The pretty-printer prints them in lower case, so docstrings are shown and
   comments, which are not part of a form, are not. A name the image has no definition for is said so, not signalled.
   Returns no value."
  (let ((*print-case* :downcase)
        (*print-pretty* t)
        (*print-right-margin* 110)
        (*print-pprint-dispatch* (copy-pprint-dispatch)))
    (set-pprint-dispatch '(eql #\Space) (lambda (stream c) (declare (ignore c)) (write-string "#\\Space" stream)))
    (labels ((show (form)
               (format t "~s~%~%" form))
             (unsaved (kind key)
               (find-if (lambda (e) (and (eq (lisp-ai-bench::entry-kind e) kind) (equal (lisp-ai-bench::entry-key e) key)))
                        (lisp-ai-bench::entries)))
             (file-forms (name)
               (let ((file (project-file (format nil "src/~a.lisp" name)))
                     (*package* *package*))
                 (if (probe-file file)
                     (with-open-file (in file)
                       (loop for form = (read in nil in)
                             until (eq form in)
                             do (if (and (consp form) (eq (car form) 'in-package))
                                    (setf *package* (find-package (second form)))
                                    (let* ((kind (lisp-ai-bench::definer-of form))
                                           (entry (and kind (unsaved kind (nth-value 1 (lisp-ai-bench::entry-name-and-key
                                                                                         kind form))))))
                                      (show (if entry (lisp-ai-bench::entry-form entry) form))))))
                     (format t ";; there is no file src/~a.lisp~%~%" name))))
             (definitions (name)
               (let ((entries (lisp-ai-bench::entries-named name)))
                 (if entries
                     (dolist (e entries)
                       (let ((*package* (or (lisp-ai-bench::entry-package e) *package*)))
                         (show (lisp-ai-bench::entry-form e))))
                     (let ((file (lisp-ai-bench:where name)))
                       (multiple-value-bind (text package) (and (pathnamep file) (file-definition name file))
                         (if text
                             (let ((*package* package))
                               (show (read-from-string text)))
                             (format t ";; the image has no definition of ~s~%~%" name))))))))
      (dolist (name names)
        (if (stringp name)
            (file-forms name)
            (definitions name)))))
  (values))

;;; A change to part of a definition, sent as that part only, matched as a form, never as text: in the hackathon's
;;; repair rounds of 2026-10-27 the Lisp team sent whole functions again where Python's changed a few lines.
(defun revise (name old new)
  "Change one part of the definition NAME (a symbol) and evaluate the changed definition: in its source text, the one
   subform that reads as OLD is replaced by NEW. OLD and NEW are text, each one whole form. OLD is matched as a form,
   not as text, so its layout does not matter and it cannot match a second place: an error, and nothing changed, when
   it is not exactly one subform of the definition. In NEW the symbol %OLD stands for the old subform, so
   (revise 'f \"(incf total x)\" \"(when (plusp x) %old)\") wraps it. The new definition is recorded in the ledger like
   any other, so a save keeps it. Returns its new text."
  (multiple-value-bind (text package) (definition-text name)
    (let ((package (or package *package*)))
      (flet ((one-form (text what)
               (let ((*package* package))
                 (multiple-value-bind (form end)
                     (handler-case (read-from-string text)
                       (error ()
                         (error "REVISE: ~a is not one whole form: ~s. Give the whole form, from its ( to its matching ), ~
                                 and to wrap it write %OLD inside NEW." what text)))
                   (unless (every (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return))) (subseq text end))
                     (error "REVISE: ~a holds more than one form: ~s. Change one form per call." what text))
                   form))))
        (let* ((old-form (one-form old "OLD"))
               (plain (one-form new "NEW"))
               (new-form (subst-if old-form (lambda (x) (and (symbolp x) (string= (symbol-name x) "%OLD"))) plain))
               (header (format nil "(in-package ~s)" (package-name package))))
          (uiop:with-temporary-file (:pathname file :type "lisp" :keep nil)
            (with-open-file (out file :direction :output :if-exists :supersede)
              (format out "~a~%~%~a~%" header text))
            (let ((form-edit::*register-locations* nil))
              (form-edit::edit-subform (namestring file) (symbol-name name) old
                                       (if (equal new-form plain) new new-form)))
            (let* ((edited (uiop:read-file-string file))
                   (new-text (string-trim '(#\Newline) (subseq edited (length header))))
                   (form (let ((*package* package))
                           (read-from-string new-text))))
              (let ((*package* package))
                (lisp-ai-bench:record-form :before form new-text)
                (eval form)
                (lisp-ai-bench:record-form :after form new-text))
              new-text)))))))
