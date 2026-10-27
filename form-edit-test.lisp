;;;; Tests of form-edit.lisp, shared by every project under ~/lisp-ai as part of the system lisp-ai-tools/tests: a
;;;; project calls (form-edit-tests:run DIRECTORY), which uses DIRECTORY for scratch files (the project's run/) and returns
;;;; how many checks failed, printing a line for each. form-edit changes a Lisp file as forms: only the forms it is told to
;;;; change are rewritten, and everything else, comments included, stays byte for byte.

(defpackage :form-edit-tests
  (:use :cl)
  (:export #:run))

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

(defun run-tail (directory)
  "A list that is also the tail of a longer list in the form can be replaced: the read-back compares the forms subform by
   subform, so the tail (x) of (g x) is not taken for the lambda list (x). Before 2026-10-27 it was, and the hackathon's
   Lisp team saw (revise 'f \"(readings)\" \"(readings &optional keep)\") refused."
  (let ((path (merge-pathnames "form-edit-test-tail.lisp" directory)))
    (unwind-protect
        (progn
         (write-text path (format nil "(defun f (x)~%  (g x))~%"))
         (form-edit::edit-subform path "F" "(x)" "(x &optional (keep (constantly t)))")
         (check-equal "a lambda list that is also the tail of (g x) is replaced, and only it" (text-of path)
          (format nil "(defun f (x &optional (keep (constantly t)))~%  (g x))~%"))
         (write-text path (format nil "(defun f (x)~%  (g x))~%"))
         (check "a list that occurs as a subform and also as a tail is still one subform, not refused as two"
          (not (refused-p (lambda () (form-edit::edit-subform path "F" "(x)" "(y)"))))))
      (when (probe-file path) (delete-file path)))))

(defparameter *delete-sample*
  ";;; header
(in-package :cl-user)

;; about a
(defun a () 1)

(defun b () 2)   ; trailing b

;; about c
(defun c () 3)
")

(defun run-delete (directory)
  "edit :delete takes a form and the comment after it on its line; the comments above stay and no double gap is left."
  (let ((path (merge-pathnames "form-edit-test-delete.lisp" directory)))
    (unwind-protect
        (progn
         (write-text path *delete-sample*)
         (form-edit::edit path :delete '("B"))
         (let ((text (text-of path)))
           (check "the form is gone" (not (search "defun b" text)))
           (check "and the comment after it on its line" (not (search "trailing b" text)))
           (check "the comment above the next form stays" (search ";; about c" text))
           (check "forms around it are kept byte for byte"
            (search ";; about a
(defun a () 1)
"
                    text))
           (check "no double blank line is left" (not (search (format nil "~%~%~%") text)))
           (check-equal "the text is the sample without that form" text ";;; header
(in-package :cl-user)

;; about a
(defun a () 1)

;; about c
(defun c () 3)
"))
         (write-text path *delete-sample*)
         (form-edit::edit path :delete '("A") :replace '(("C" . "(defun c () 33)")) :insert-before
                          '(("B" . "(defun d () 4)")))
         (check-equal "delete, replace and insert together" (text-of path) ";;; header
(in-package :cl-user)

;; about a

(defun d () 4)

(defun b () 2)   ; trailing b

;; about c
(defun c () 33)
")
         (write-text path *delete-sample*)
         (check "no form of that name: refused" (refused-p (lambda () (form-edit::edit path :delete '("NO-SUCH")))))
         (check "a replace of no form: refused"
          (refused-p (lambda () (form-edit::edit path :replace '(("NO-SUCH" . "(a)"))))))
         (check "an insert before no form: refused"
          (refused-p (lambda () (form-edit::edit path :insert-before '(("NO-SUCH" . "(a)"))))))
         (check "a form both replaced and deleted: refused"
          (refused-p (lambda () (form-edit::edit path :delete '("A") :replace '(("A" . "(b)"))))))
         (write-text path (format nil "(defun a () 1)~%(defun a () 2)~%"))
         (check "two forms of that name: refused" (refused-p (lambda () (form-edit::edit path :delete '("A")))))
         (write-text path (format nil "#+sbcl (defun a () 1)~%(defun b () 2)~%"))
         (check "a form under #+: refused" (refused-p (lambda () (form-edit::edit path :delete '("A")))))
         (check-equal "and the file is as it was" (text-of path)
          (format nil "#+sbcl (defun a () 1)~%(defun b () 2)~%")))
      (when (probe-file path) (delete-file path)))))

(defun run-delete-subform (directory)
  "delete-subform cuts one subform out of a form; its line goes when it is alone on it, comments around it stay."
  (let ((path (merge-pathnames "form-edit-test-delete-subform.lisp" directory)))
    (unwind-protect
        (progn
         (write-text path "(defun f (x)
  ;; keep me
  (list x
        1
        2
        ;; before three
        3))

(defun g () (h a b c))
")
         (form-edit::delete-subform path "F" "2")
         (form-edit::delete-subform path "G" "b")
         (check-equal "a subform alone on its line takes the line; one in a line takes its blanks" (text-of path)
          "(defun f (x)
  ;; keep me
  (list x
        1
        ;; before three
        3))

(defun g () (h a c))
")
         (form-edit::delete-subform path "G" "a")
         (check "the first argument after the operator goes, the rest stay"
          (search "(defun g () (h c))" (text-of path)))
         (check "the other form is untouched"
          (search "(defun f (x)
  ;; keep me"
                  (text-of path)))
         (check "a subform that is not there: refused"
          (refused-p (lambda () (form-edit::delete-subform path "F" "(nothing)"))))
         (check "a subform that is not unique: refused"
          (refused-p (lambda () (form-edit::delete-subform path "F" "x")))))
      (when (probe-file path) (delete-file path)))))

(defun run-foreign-packages (directory)
  "A file may name packages the image does not have; they are read as stand-ins that keep : and ::, and are gone after."
  (let ((path (merge-pathnames "form-edit-test-foreign.lisp" directory)))
    (unwind-protect
        (progn
         (write-text path "(defun f ()
  ;; keep me
  (nopkg:thing nopkg::hidden 1))
")
         (form-edit::edit-subform path "F" "1" "2")
         (check-equal "a subform is replaced next to symbols of a package the image lacks" (text-of path) "(defun f ()
  ;; keep me
  (nopkg:thing nopkg::hidden 2))
")
         (form-edit::edit-subform path "F" "(nopkg:thing nopkg::hidden 2)"
                                  "(other:call other::inner :key `(a ,b ,@c))")
         (check "new text with foreign symbols and a backquote is printed with its colons"
          (search "(other:call other::inner :key `(a ,b ,@c))" (text-of path)))
         (form-edit::edit path :replace '(("F" . "(defun f () (nopkg:thing nopkg::hidden))")))
         (check-equal "a form is replaced as a whole too" (text-of path) "(defun f () (nopkg:thing nopkg::hidden))
")
         (check "and no stand-in package is left behind" (notany #'find-package '("NOPKG" "OTHER"))))
      (when (probe-file path) (delete-file path)))))

(defparameter *anchor-sample*
  "(in-package :cl-user)

;; first
(setf *x* 1)

(handler-case (a) (error () nil))

(setf *y* 2)
")

(defun run-anchors (directory)
  "A form with no name is addressed as (:form TEXT): the one top-level form that reads as TEXT."
  (let ((path (merge-pathnames "form-edit-test-anchors.lisp" directory)))
    (unwind-protect
        (progn
         (write-text path *anchor-sample*)
         (form-edit::edit path :replace '(((:form "(setf *x* 1)") . "(setf *x* 10)")))
         (check "a form with no name is replaced, the comment above it kept"
          (search ";; first
(setf *x* 10)

(handler-case"
                  (text-of path)))
         (write-text path *anchor-sample*)
         (form-edit::edit path :delete '((:form "(handler-case (a) (error () nil))")))
         (check-equal "a form with no name is deleted, no double gap left" (text-of path) "(in-package :cl-user)

;; first
(setf *x* 1)

(setf *y* 2)
")
         (write-text path *anchor-sample*)
         (form-edit::edit path :insert-before '(((:form "(setf *y* 2)") . "(setf *z* 3)")))
         (check "a form is inserted before a form with no name"
          (search "(error () nil))

(setf *z* 3)

(setf *y* 2)"
                  (text-of path)))
         (write-text path *anchor-sample*)
         (form-edit::edit-subform path '(:form "(handler-case (a) (error () nil))") "(a)" "(b)")
         (check "a subform is edited in a form with no name"
          (search "(handler-case (b) (error () nil))" (text-of path)))
         (write-text path (format nil "(setf *x* 1)~%(setf *x* 1)~%"))
         (check "an anchor that matches twice: refused"
          (refused-p (lambda () (form-edit::edit path :delete '((:form "(setf *x* 1)"))))))
         (check "an anchor that matches nothing: refused"
          (refused-p (lambda () (form-edit::edit path :delete '((:form "(setf *q* 9)"))))))
         (check-equal "and the file is as it was" (text-of path) (format nil "(setf *x* 1)~%(setf *x* 1)~%")))
      (when (probe-file path) (delete-file path)))))

(defun run-comments (directory)
  "Comments between the forms of new text are kept as typed; a comment inside a form is refused, never silently lost."
  (let ((path (merge-pathnames "form-edit-test-comments.lisp" directory))
        (sample (format nil "(defun a () 1)~%~%(defun b () 2)~%")))
    (unwind-protect
        (progn
         (write-text path sample)
         (form-edit::edit path :insert-before
                          '(("B" . ";; about d
(defun d () 4)"))
                          :append "(defun e () 5) ; note")
         (check-equal "a comment above a new form, and one after the last" (text-of path) "(defun a () 1)

;; about d
(defun d () 4)

(defun b () 2)

(defun e () 5)  ; note
")
         (write-text path sample)
         (form-edit::edit path :replace
                          '(("A" . ";; new a
(defun a () 11) ; eleven")))
         (check-equal "a replaced form brings its comments" (text-of path) ";; new a
(defun a () 11)  ; eleven

(defun b () 2)
")
         (write-text path sample)
         (check "a comment inside a new form: refused, not lost"
          (refused-p
           (lambda ()
             (form-edit::edit path :replace
                              '(("A" . "(defun a ()
  ;; inner
  1)"))))))
         (check "a comment in a new subform: refused"
          (refused-p
           (lambda ()
             (form-edit::edit-subform path "A" "1" "(list 1 ; c
 2)"))))
         (check "a comment before a new subform: refused"
          (refused-p
           (lambda ()
             (form-edit::edit-subform path "A" "1" ";; lead
2"))))
         (check-equal "and after the refusals the file is as it was" (text-of path) sample))
      (when (probe-file path) (delete-file path)))))

(defun run-indentation (directory)
  "A form printed over several lines is indented to where it is put, but a newline inside a string is left alone."
  (let ((path (merge-pathnames "form-edit-test-indentation.lisp" directory)))
    (unwind-protect
        (progn
         (check-equal "newlines between tokens get the column; newlines inside a string do not"
          (form-edit::indented (format nil "(a \"x~%y\"~%b ~c~%c)" #\ ) 4)
          (format nil "(a \"x~%y\"~%    b ~c~%    c)" #\ ))
         (check-equal "a quote or a newline written as a character is not a string"
          (form-edit::indented (format nil "(f #\\\" ~%g #\\~%~%h)") 2) (format nil "(f #\\\" ~%  g #\\~%~%  h)"))
         (write-text path (format nil "(defun f ()~%  \"line one~%   line two\"~%  1)~%"))
         (form-edit::edit-subform path "F" "\"line one
   line two\""
                                  "\"line one
   line two, edited\"")
         (check-equal "a docstring that spans lines can be edited, its second line as typed" (text-of path)
          (format nil "(defun f ()~%  \"line one~%   line two, edited\"~%  1)~%")))
      (when (probe-file path) (delete-file path)))))

(defun run-insert-after (directory)
  "edit :insert-after puts new text after a form, and after the comment that follows it on its line."
  (let ((path (merge-pathnames "form-edit-test-insert-after.lisp" directory))
        (sample
         ";;; where a is
(defun a () 1) ; about a

;;; where b is
(defun b () 2)
"))
    (unwind-protect
        (progn
         (write-text path sample)
         (form-edit::edit path :insert-after
                          '(("A" . ";; new, after a
(defun c () 3)")))
         (check-equal "text lands after the form and its trailing comment, and the next form keeps its own comment"
          (text-of path) ";;; where a is
(defun a () 1) ; about a

;; new, after a
(defun c () 3)

;;; where b is
(defun b () 2)
")
         (write-text path sample)
         (form-edit::edit path :insert-after '(((:form "(defun b () 2)") . "(defun d () 4)")))
         (check "text lands after the last form too"
          (search "(defun b () 2)

(defun d () 4)
"
                  (text-of path)))
         (check "insert-after a form that is not there: refused"
          (refused-p (lambda () (form-edit::edit path :insert-after '(("NO-SUCH" . "(x)")))))))
      (when (probe-file path) (delete-file path)))))

(defun run-loop-layout (directory)
  "An extended LOOP is written with one clause per line, an action staying on its condition's line; a plain LOOP and a loop
   the edit does not touch are as they were."
  (let ((path (merge-pathnames "form-edit-test-loop.lisp" directory))
        (sample
         "(defun keep () (loop for x in l do (f x) finally (return 1)))
(defun f () 0)
"))
    (unwind-protect
        (progn
         (write-text path sample)
         (form-edit::edit path :replace
                          '(("F"
                             . "(defun f (l) (loop with total = 0 for x in l when (plusp x) collect x into plus and sum x into total else do (g x) finally (return (values plus total))))")))
         (check-equal "an edited loop: a clause to a line, a conditional's actions on its line" (text-of path)
          "(defun keep () (loop for x in l do (f x) finally (return 1)))
(defun f (l)
  (loop with total = 0
        for x in l
        when (plusp x) collect x into plus and sum x into total else do (g x)
        finally (return (values plus total))))
")
         (write-text path sample)
         (form-edit::edit path :replace
                          '(("F"
                             . "(defun f (l) (loop for count from 0 below 3 when (oddp count) collect count into count-list finally do (print count)))")))
         (check-equal "a variable spelled like a loop word is not taken for one" (text-of path)
          "(defun keep () (loop for x in l do (f x) finally (return 1)))
(defun f (l)
  (loop for count from 0 below 3
        when (oddp count) collect count into count-list
        finally do (print count)))
")
         (write-text path sample)
         (form-edit::edit path :replace '(("F" . "(defun f () (loop (print 1) (print 2)))")))
         (check-equal "a plain loop is printed as before" (text-of path)
          "(defun keep () (loop for x in l do (f x) finally (return 1)))
(defun f ()
  (loop (print 1)
        (print 2)))
"))
      (when (probe-file path) (delete-file path)))))

(defun run-form-sources (directory)
  "edit and edit-subform take a form where they take text: it is printed in the package of the file. A symbol of the SCRATCH
   package, or of a package the image lacks, is refused."
  (let ((path (merge-pathnames "form-edit-test-forms.lisp" directory))
        (home (make-package "FORM-EDIT-SAMPLE-HOME" :use '(:cl)))
        (scratch (or (find-package "SCRATCH") (make-package "SCRATCH" :use '(:cl))))
        (made-scratch (not (find-package "SCRATCH")))
        (sample
         ";; the package
(in-package :form-edit-sample-home)
;; keep me
(defun f () 0)
(defun g (x) (list x 1))
"))
    (unwind-protect
        (flet ((in-home (text)
                 (let ((*package* home))
                   (read-from-string text))))
          (write-text path sample)
          (form-edit::edit path :replace
                           (list
                            (cons "F"
                                  (in-home
                                   "(defun f (l) \"Doc.\" (cl-user::h (loop for x in l when (plusp x) collect x finally (return 1))))"))))
          (check-equal "a form is written in the file's package: its own symbols plain, another package's qualified"
           (text-of path) ";; the package
(in-package :form-edit-sample-home)
;; keep me
(defun f (l)
  \"Doc.\"
  (common-lisp-user::h
   (loop for x in l
         when (plusp x) collect x
         finally (return 1))))
(defun g (x) (list x 1))
")
          (write-text path sample)
          (form-edit::edit-subform path "G" (in-home "(list x 1)") (in-home "(list x 2)"))
          (check-equal "edit-subform with forms" (text-of path) ";; the package
(in-package :form-edit-sample-home)
;; keep me
(defun f () 0)
(defun g (x) (list x 2))
")
          (write-text path sample)
          (check "a symbol of the SCRATCH package is refused"
           (refused-p
            (lambda ()
              (form-edit::edit path :replace
                               (list (cons "F" (list 'defun (intern "F" home) nil (intern "S" scratch))))))))
          (check-equal "and the refusal wrote nothing" (text-of path) sample)
          (write-text path ";; no such package
(in-package :form-edit-package-nobody-has)
(defun f () 0)
")
          (check "a form for a file in a package the image lacks is refused"
           (refused-p (lambda () (form-edit::edit path :replace (list (cons "F" (list 'defun 'f nil 1)))))))
          (check "while text for it is still fine"
           (not (refused-p (lambda () (form-edit::edit path :replace '(("F" . "(defun f () 1)"))))))))
      (when (probe-file path) (delete-file path))
      (delete-package home)
      (when made-scratch (delete-package scratch)))))

(defun in-cl-user (text)
  "The form TEXT reads as, read in CL-USER."
  (let ((*package* (find-package :cl-user)))
    (read-from-string text)))

(defun recorded-file (symbol)
  "The file the image records for the function SYMBOL, or NIL."
  (let ((source (first (sb-introspect:find-definition-sources-by-name symbol :function))))
    (and source (sb-introspect:definition-source-pathname source))))

(defun recorded-offset (symbol)
  "The character offset M-. would be told for the function SYMBOL, or NIL."
  (let* ((location (second (first (slynk-backend:find-definitions symbol))))
         (offset (and (consp location) (find :offset (cdr location) :key (lambda (x) (and (consp x) (car x)))))))
    (and offset (second offset))))

(defun run-locations (directory)
  "After an edit the image knows where the file keeps a definition it already has, so M-. finds it. The definitions
   are first made in the image from forms, as a request through bin/lisp eval makes them, so they have no file. A definition
   the image does not have is not defined by the edit; a variable is not evaluated again; the switch turns it all off."
  (require :sb-introspect)
  (unless (find-package "SLYNK-BACKEND")
    (incf *failures*)
    (format t "FAIL: Slynk is not loaded, so the location checks did not run~%")
    (return-from run-locations))
  (let* ((path (merge-pathnames "form-edit-test-locations.lisp" directory))
         (a (intern "LOCATION-TEST-A" :cl-user))
         (b (intern "LOCATION-TEST-B" :cl-user))
         (c (intern "LOCATION-TEST-C" :cl-user))
         (new (intern "LOCATION-TEST-NEW" :cl-user))
         (p (intern "*LOCATION-TEST-P*" :cl-user)))
    (unwind-protect
        (progn
         (write-text path "(in-package :cl-user)

(defun location-test-a () 1)

(defun location-test-b () 2)

(defun location-test-c (x)
  (list x 1))

(defparameter *location-test-p* 1)
")
         (dolist
             (text
              '("(defun location-test-a () 1)" "(defun location-test-b () 2)" "(defun location-test-c (x) (list x 1))"
                "(defparameter *location-test-p* 1)"))
           (eval (in-cl-user text)))
         (check "before an edit the image records no file for the definition" (null (recorded-file b)))
         (multiple-value-bind (written report)
             (form-edit::edit path :replace '(("LOCATION-TEST-B" . "(defun location-test-b () 3)")))
           (check-equal "edit returns the path" written path)
           (check-equal "the replaced definition is reported registered" (getf report :registered) (list b)))
         (check-equal "the image records the file" (and (recorded-file b) (namestring (recorded-file b)))
          (namestring (truename path)))
         (check-equal "and the character offset M-. is told is where the form starts in the file" (recorded-offset b)
          (search "(defun location-test-b" (text-of path)))
         (check-equal "the image has the new text of the definition too" (funcall b) 3)
         (check "a definition the edit did not write is left alone" (null (recorded-file a)))
         (multiple-value-bind (written report)
             (form-edit::edit-subform path "LOCATION-TEST-C" "(list x 1)" "(list x 2)")
           (declare (ignore written))
           (check-equal "edit-subform reports the changed form registered" (getf report :registered) (list c)))
         (check-equal "and it knows the new place after the file changed size" (recorded-offset c)
          (search "(defun location-test-c" (text-of path)))
         (let ((report (nth-value 1 (form-edit::edit path :append "(defun location-test-new () 9)"))))
           (check-equal "a new definition is skipped, with the reason" (getf report :skipped)
            (list (list new "not defined in the image")))
           (check "and it is not defined in the image" (not (fboundp new))))
         (setf (symbol-value p) 42)
         (let ((report
                (nth-value 1
                           (form-edit::edit path :replace
                                            '(("*LOCATION-TEST-P*" . "(defparameter *location-test-p* 2)"))))))
           (check-equal "a defparameter is not registered" (getf report :registered) 'nil))
         (check-equal "and its value is not reset" (symbol-value p) 42)
         (let ((report
                (let ((form-edit::*register-locations* nil))
                  (nth-value 1
                             (form-edit::edit path :replace '(("LOCATION-TEST-A" . "(defun location-test-a () 1)")))))))
           (check-equal "turned off, nothing is registered" (getf report :registered) 'nil)
           (check "and the report says why" (stringp (getf report :note)))
           (check "so the definition still has no file" (null (recorded-file a)))))
      (when (probe-file path) (delete-file path))
      (dolist (symbol (list a b c new)) (when (fboundp symbol) (fmakunbound symbol)))
      (when (boundp p) (makunbound p)))))

(defun run-written-forms (directory)
  "edit checks that the forms it wrote read back as the forms meant, not only that the other forms are unchanged. The
   check is broken once on purpose: the printer is made to write a 2 for every 1, which leaves the number of forms and
   every untouched form as they were, so only this check can see it."
  (let ((path (merge-pathnames "form-edit-test-written.lisp" directory))
        (sample
         "(in-package :cl-user)

(defun a () 0)

(defun b () 0)
"))
    (flet ((damaged (thunk)
             (sb-int:encapsulate 'form-edit::printed 'damage
                                 (lambda (function &rest arguments) (substitute #\2 #\1 (apply function arguments))))
             (unwind-protect (refused-p thunk) (sb-int:unencapsulate 'form-edit::printed 'damage))))
      (unwind-protect
          (progn
           (write-text path sample)
           (check "a damaged replacement of text is refused"
            (damaged (lambda () (form-edit::edit path :replace '(("A" . "(defun a () 1)"))))))
           (check "a damaged replacement given as a form is refused"
            (damaged (lambda () (form-edit::edit path :replace '(("A" defun a nil 1))))))
           (check "a damaged insertion is refused"
            (damaged (lambda () (form-edit::edit path :insert-after '(("B" . "(defun c () 1)"))))))
           (check "a damaged appended form is refused"
            (damaged (lambda () (form-edit::edit path :append "(defun d () 1)"))))
           (check "one damaged form among several is refused"
            (damaged
             (lambda ()
               (form-edit::edit path :replace '(("A" . "(defun a () 0)")) :insert-after '(("B" . "(defun c () 1)"))))))
           (check-equal "every refusal left the file as it was" (text-of path) sample)
           (form-edit::edit path :replace '(("A" . "(defun a () 1)")))
           (check "the same edit, undamaged, is written" (search "(defun a () 1)" (text-of path))))
        (when (probe-file path) (delete-file path))))))

(defun run-read-back (directory)
  "After a write, edit and edit-subform read the file back from the disk and compare it with the text they meant to
   write. The check is broken once on purpose: write-file is made to drop the last character."
  (let ((path (merge-pathnames "form-edit-test-read-back.lisp" directory))
        (sample
         "(defun a () 0)

(defun b () 0)
"))
    (flet ((short-write (thunk)
             (write-text path sample)
             (sb-int:encapsulate 'form-edit::write-file 'short
                                 (lambda (function path text)
                                   (funcall function path (subseq text 0 (1- (length text))))))
             (unwind-protect (refused-p thunk) (sb-int:unencapsulate 'form-edit::write-file 'short))))
      (unwind-protect
          (progn
           (write-text path sample)
           (check "edit refuses when the file does not read back as written"
            (short-write (lambda () (form-edit::edit path :replace '(("A" . "(defun a () 1)"))))))
           (check "edit-subform refuses when the file does not read back as written"
            (short-write (lambda () (form-edit::edit-subform path "A" "0" "1"))))
           (check "delete-subform refuses when the file does not read back as written"
            (short-write (lambda () (form-edit::delete-subform path "A" "0"))))
           (write-text path sample)
           (form-edit::edit path :replace '(("A" . "(defun a () 1)")))
           (form-edit::edit-subform path "B" "0" "2")
           (check "undamaged writes are not refused"
            (and (search "(defun a () 1)" (text-of path)) (search "(defun b () 2)" (text-of path)))))
        (when (probe-file path) (delete-file path))))))

(defun run (directory)
  "Run every check of form-edit, with DIRECTORY (a pathname ending in a slash) for scratch files. Returns the number of failures."
  (setf *failures* 0)
  (let ((directory (pathname directory)))
    (run-edit directory)
    (run-strings directory)
    (run-subform directory)
    (run-atom directory)
    (run-tail directory)
    (run-delete directory)
    (run-delete-subform directory)
    (run-foreign-packages directory)
    (run-anchors directory)
    (run-comments directory)
    (run-indentation directory)
    (run-insert-after directory)
    (run-loop-layout directory)
    (run-form-sources directory)
    (run-locations directory)
    (run-written-forms directory)
    (run-read-back directory))
  *failures*)
