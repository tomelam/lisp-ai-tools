;;;; Tests of form-edit.lisp, shared by every project under ~/lisp-ai: a project's harness loads this file and calls
;;;; (form-edit-tests:run DIRECTORY), which uses DIRECTORY for scratch files (the project's run/) and returns how many
;;;; checks failed, printing a line for each. form-edit changes a Lisp file as forms: only the forms it is told to change are
;;;; rewritten, and everything else, comments included, stays byte for byte.

(defpackage :form-edit-tests
  (:use :cl)
  (:export #:run))

(defvar cl-user::*form-edit-tests-here* (directory-namestring *load-truename*))

(load (merge-pathnames "form-edit.lisp" cl-user::*form-edit-tests-here*))

(in-package :form-edit-tests)

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

(defun text-of (path)
  (with-open-file (in path :external-format :utf-8)
    (let ((text (make-string (file-length in))))
      (subseq text 0 (read-sequence text in)))))

(defun write-text (path text)
  (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
    (write-string text out)))

(defun refused-p (thunk)
  "True when THUNK signals an error: the edit was refused."
  (handler-case (progn (funcall thunk) nil)
    (error () t)))

(defparameter *edit-sample*
  ";;; old header
(in-package :cl-user)

;; a comment between forms
(defun keep-a () 1)

(defun change-me () 1)   ; trailing comment
(defun keep-b (x)
  \"kept as typed\"
  (list    x   2))
")

(defun run-edit (directory)
  (let ((path (merge-pathnames "form-edit-test-edit.lisp" directory)))
    (unwind-protect
        (progn
         (write-text path *edit-sample*)
         (form-edit::edit path :header ";;; new header
" :replace '(("CHANGE-ME" . "(defun change-me () 99)")) :insert-before '(("KEEP-B" . "(defun added () 3)")) :append
                          "(defun last-one () 4)")
         (let ((text (text-of path)))
           (check "the header is replaced" (eql 0 (search ";;; new header" text)))
           (check "the old header is gone" (not (search "old header" text)))
           (check "a comment between forms is kept" (search ";; a comment between forms" text))
           (check "a trailing comment is kept" (search "; trailing comment" text))
           (check "an untouched form is kept byte for byte, spacing and all"
                  (search "(defun keep-b (x)
  \"kept as typed\"
  (list    x   2))" text))
           (check "the replaced form is new"
                  (and (search "(defun change-me () 99)" text) (not (search "(defun change-me () 1)" text))))
           (check "forms are in order"
                  (let ((positions (mapcar (lambda (needle) (search needle text))
                                           '("keep-a" "change-me" "added" "keep-b" "last-one"))))
                    (and (every #'identity positions) (apply #'< positions))))
           (check-equal "it still reads as six forms"
                        (with-open-file (in path)
                          (let ((*read-eval* nil))
                            (loop for form = (read in nil in) until (eq form in) count t)))
                        6)))
      (when (probe-file path) (delete-file path)))))

(defun run-strings (directory)
  "A form named by a string, as (defsystem \"x\" ...) is, can be replaced like one named by a symbol."
  (let ((path (merge-pathnames "form-edit-test-strings.lisp" directory)))
    (unwind-protect
        (progn
         (write-text path "(defsystem \"a\" :depends-on (\"x\"))

(defsystem \"b\" :depends-on (\"x\"))
")
         (form-edit::edit path :replace '(("b" . "(defsystem \"b\" :depends-on (\"y\"))")))
         (let ((text (text-of path)))
           (check "the form named b is replaced" (search "(\"y\")" text))
           (check "and the form named a is untouched" (search "(defsystem \"a\" :depends-on (\"x\"))" text))))
      (when (probe-file path) (delete-file path)))))

(defparameter *subform-sample*
  "(in-package :cl-user)

;; before
(defun keep-a () 1)

(defun change-me (x)
  ;; a comment inside the form (with parens)
  (list x
        \"old\"   ; a trailing comment
        #| a block comment ( |#
        (+ 1 2)))

(defun keep-b (y) #+sbcl (one) #-sbcl (two) y)
")

(defun run-subform (directory)
  "edit-subform changes one subform of a form and leaves the comments inside that form, and every other form, as typed."
  (let ((path (merge-pathnames "form-edit-test-subform.lisp" directory)))
    (flet ((edit-subform (name old new) (form-edit::edit-subform path name old new)))
      (unwind-protect
          (progn
           ;; a string and a list, inside a form with comments
           (write-text path *subform-sample*)
           (edit-subform "CHANGE-ME" "\"old\"" "\"new\"")
           (edit-subform "CHANGE-ME" "(+ 1 2)" "(* 3 4)")
           (let ((text (text-of path)))
             (check "the string is replaced" (and (search "\"new\"" text) (not (search "\"old\"" text))))
             (check "and the list" (and (search "(* 3 4)" text) (not (search "(+ 1 2)" text))))
             (check "a line comment inside the form is kept" (search ";; a comment inside the form (with parens)" text))
             (check "a trailing comment is kept" (search "; a trailing comment" text))
             (check "a block comment is kept" (search "#| a block comment ( |#" text))
             (check "a form before is kept byte for byte" (search ";; before
(defun keep-a () 1)
" text))
             (check "a form with #+ and #- after is kept byte for byte"
                    (search "(defun keep-b (y) #+sbcl (one) #-sbcl (two) y)" text)))
           ;; refusals leave the file as it was
           (write-text path *subform-sample*)
           (check "two subforms read as x: refused" (refused-p (lambda () (edit-subform "CHANGE-ME" "x" "z"))))
           (check "no subform reads as that: refused"
                  (refused-p (lambda () (edit-subform "CHANGE-ME" "(nothing here)" "(a)"))))
           (check "no form of that name: refused" (refused-p (lambda () (edit-subform "NO-SUCH" "\"old\"" "\"new\""))))
           (check "a new text of two forms: refused"
                  (refused-p (lambda () (edit-subform "CHANGE-ME" "\"old\"" "(a) (b)"))))
           (check "a subform under #+: refused" (refused-p (lambda () (edit-subform "KEEP-B" "(one)" "(1)"))))
           (check-equal "and after all the refusals the file is as it was" (text-of path) *subform-sample*)
           ;; the printed form fits where it is put
           (check-equal "continuation lines are indented to the column of the subform"
                        (form-edit::indented (format nil "a~%b") 4) (format nil "a~%    b")))
        (when (probe-file path) (delete-file path))))))

(defun run-atom (directory)
  "A symbol or a number that is the only one of its kind in a form can be replaced, comments around it kept."
  (let ((path (merge-pathnames "form-edit-test-atom.lisp" directory)))
    (unwind-protect
        (progn
         (write-text path (format nil "(defun f (x)~%  ;; keep (me)~%  (list x 42 (quote y)))~%"))
         (form-edit::edit-subform path "F" "42" "43")
         (form-edit::edit-subform path "F" "list" "vector")
         (check-equal "a number and a symbol are replaced, the comment and the rest kept" (text-of path)
                      (format nil "(defun f (x)~%  ;; keep (me)~%  (vector x 43 (quote y)))~%")))
      (when (probe-file path) (delete-file path)))))

(defun run (directory)
  "Run every check of form-edit, with DIRECTORY (a pathname ending in a slash) for scratch files. Returns the number of
   failures."
  (setf *failures* 0)
  (let ((directory (pathname directory)))
    (run-edit directory)
    (run-strings directory)
    (run-subform directory)
    (run-atom directory))
  *failures*)
