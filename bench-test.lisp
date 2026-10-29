;;;; Tests of bench.lisp, the workbench, shared by every project under ~/lisp-ai as part of the system lisp-ai-tools/tests:
;;;; a project calls (lisp-ai-bench-tests:run DIRECTORY), which makes a scratch project inside DIRECTORY (the project's
;;;; run/), points the workbench at it for the duration, removes it afterwards and returns how many checks failed,
;;;; printing a line for each. The definitions are made as bin/lisp eval makes them, through
;;;; lisp-ai-debug:eval-with-debugging, so the ledger records them the way it does in a running image.

(defpackage :lisp-ai-bench-tests
  (:use :cl)
  (:export #:run))

(in-package :lisp-ai-bench-tests)

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
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
    (write-string text out)))

(defun refused-p (thunk)
  "True when THUNK signals an error: the save was refused."
  (handler-case (progn (funcall thunk) nil)
    (error () t)))

(defun refusal (thunk)
  "The text of the error THUNK signals, or NIL."
  (handler-case (progn (funcall thunk) nil)
    (error (e) (princ-to-string e))))

(defun evaluate (text)
  "Make the definitions in TEXT as a request through bin/lisp eval does, in CL-USER."
  (lisp-ai-debug:eval-with-debugging text :cl-user))

(defun sym (name) (intern name :cl-user))

(defparameter *names*
  '("BENCH-TEST-F" "BENCH-TEST-G" "BENCH-TEST-H" "BENCH-TEST-Z" "BENCH-TEST-Y" "BENCH-TEST-M" "*BENCH-TEST-V*"
    "BENCH-TEST-P" "BENCH-TEST-Q"))

(defun forms-of (path)
  (let ((*package* (find-package :cl-user)) (*read-eval* nil))
    (with-open-file (in path) (loop for form = (read in nil in) until (eq form in) collect form))))

(defun run-ledger (root)
  "A definition made through eval is recorded, with its text as typed; saving it writes that text into the file, comments
   and docstring included; the mirror in run/pending.sexp follows; where finds the file afterwards."
  (let ((source (merge-pathnames "src/t.lisp" root))
        (mirror (merge-pathnames "run/pending.sexp" root)))
    (write-text source (format nil "(in-package :cl-user)~%~%(defun bench-test-other () 0)~%"))
    (lisp-ai-bench:forget-all)
    (check-equal "nothing is pending at first" (lisp-ai-bench:pending) '())
    (evaluate ";; about f
(defun bench-test-f (x)
  \"doubles\"
  ;; twice
  (* x 2))")
    (check-equal "the definition is pending, with no file known" (lisp-ai-bench:pending)
                 (list (list :defun (sym "BENCH-TEST-F") nil)))
    (check-equal "and the image has it" (funcall (sym "BENCH-TEST-F") 4) 8)
    (check "the mirror names it" (search "BENCH-TEST-F" (text-of mirror)))
    (check-equal "where says unsaved" (lisp-ai-bench:where (sym "BENCH-TEST-F")) :unsaved)
    (check "saving with no file known is refused, and the message says :file"
           (search ":file" (refusal (lambda () (lisp-ai-bench:save (sym "BENCH-TEST-F"))))))
    (check "saving into a file that does not exist is refused"
           (refused-p (lambda () (lisp-ai-bench:save (sym "BENCH-TEST-F") :file "src/missing.lisp"))))
    (lisp-ai-bench:save (sym "BENCH-TEST-F") :file "src/t.lisp")
    (let ((text (text-of source)))
      (check "the comment above the form is in the file" (search ";; about f" text))
      (check "the comment inside the form is in the file" (search "  ;; twice" text))
      (check "the docstring is in the file" (search "\"doubles\"" text))
      (check "the other form is untouched" (search "(defun bench-test-other () 0)" text))
      (check "the new form comes after it" (< (search "bench-test-other" text) (search "bench-test-f" text))))
    (check-equal "nothing is pending after the save" (lisp-ai-bench:pending) '())
    (check "the mirror is empty too" (not (search "BENCH-TEST-F" (text-of mirror))))
    (check-equal "where gives the file" (ignore-errors (truename (lisp-ai-bench:where (sym "BENCH-TEST-F")))) (truename source))
    (check-equal "the file reads as two definitions" (length (forms-of source)) 3)))

(defun run-replace (root)
  "A definition the file already has is replaced in place. Comments inside the old one that the new text lacks, and a
   docstring the new form lacks, are a refusal unless meant. Comment lines above the old form are kept when the new text
   brings none, and replaced when it does."
  (let ((source (merge-pathnames "src/t.lisp" root)))
    (evaluate "(defun bench-test-f (x) \"doubles\" (* x 2))")
    (let ((text (refusal (lambda () (lisp-ai-bench:save (sym "BENCH-TEST-F"))))))
      (check "dropping the inner comment is refused" text)
      (check "and the refusal names the comment" (and text (search ";; twice" text))))
    (check "the file is untouched by the refusal" (search "  ;; twice" (text-of source)))
    (lisp-ai-bench:save (sym "BENCH-TEST-F") :drop-comments t)
    (let ((text (text-of source)))
      (check "with :drop-comments the comment is gone" (not (search ";; twice" text)))
      (check "the comment above the form is kept, since the new text brought none" (search ";; about f" text))
      (check "the new definition is in" (search "(defun bench-test-f (x) \"doubles\" (* x 2))" text))
      (check-equal "and there is still one f" (count (sym "BENCH-TEST-F") (forms-of source) :key (lambda (f) (and (consp f) (second f)))) 1))
    (check-equal "the known file is remembered for the next time" (lisp-ai-bench:pending) '())
    (evaluate ";; about f, third version
(defun bench-test-f (x) \"doubles\" (* x 2))")
    (lisp-ai-bench:save (sym "BENCH-TEST-F"))
    (let ((text (text-of source)))
      (check "new comment lines above replace the old ones" (and (search ";; about f, third version" text)
                                                                 (not (search (format nil ";; about f~%") text)))))
    (evaluate "(defun bench-test-f (x) (* x 2))")
    (let ((text (refusal (lambda () (lisp-ai-bench:save (sym "BENCH-TEST-F"))))))
      (check "dropping the docstring is refused" text)
      (check "and the refusal quotes it" (and text (search "doubles" text))))
    (lisp-ai-bench:save (sym "BENCH-TEST-F") :drop-docstring t)
    (check "with :drop-docstring it is written" (not (search "doubles" (text-of source))))))

(defun run-anchors (root)
  "A new definition goes after or before a named form, or at the end; before puts it above that form's own comments."
  (let ((source (merge-pathnames "src/t.lisp" root)))
    (evaluate "(defun bench-test-g () 1)")
    (lisp-ai-bench:save (sym "BENCH-TEST-G") :file "src/t.lisp" :after "BENCH-TEST-OTHER")
    (evaluate ";; h first
(defun bench-test-h () 2)")
    (lisp-ai-bench:save (sym "BENCH-TEST-H") :file "src/t.lisp" :before "BENCH-TEST-F")
    (let ((text (text-of source)))
      (check "g follows other" (< (search "bench-test-other" text) (search "bench-test-g" text) (search "bench-test-h" text)))
      (check "h comes before f's comment lines" (< (search ";; h first" text) (search ";; about f, third" text) (search "(defun bench-test-f" text))))
    (check-equal "the file reads as five definitions" (length (forms-of source)) 5)
    (check "an anchor that is not there is refused"
           (refused-p (lambda () (evaluate "(defun bench-test-y () 3)")
                        (lisp-ai-bench:save (sym "BENCH-TEST-Y") :file "src/t.lisp" :after "NO-SUCH"))))
    (lisp-ai-bench:drop (sym "BENCH-TEST-Y"))))

(defun run-kinds (root)
  "Variables, generic functions and methods: each kind is recorded, methods by their specializers, and save-all writes them
   in the order made; a redefined method replaces only its own."
  (let ((source (merge-pathnames "src/t.lisp" root)))
    (evaluate "(defvar *bench-test-v* 1 \"a variable\")
(defgeneric bench-test-m (x))
(defmethod bench-test-m ((x integer)) :integer)
(defmethod bench-test-m ((x string)) :string)")
    (check-equal "four definitions are pending" (mapcar #'first (lisp-ai-bench:pending)) '(:defvar :defgeneric :defmethod :defmethod))
    (check-equal "save-all writes them all into the given file: two names, the methods with their generic function"
                 (length (lisp-ai-bench:save-all :file "src/t.lisp")) 2)
    (check-equal "nothing is left" (lisp-ai-bench:pending) '())
    (let ((text (text-of source)))
      (check "the variable's docstring is there" (search "\"a variable\"" text))
      (check "both methods are there" (and (search "((x integer))" text) (search "((x string))" text))))
    (evaluate "(defmethod bench-test-m ((x integer)) :whole-number)")
    (check-equal "the method is pending under its name" (mapcar #'second (lisp-ai-bench:pending)) (list (sym "BENCH-TEST-M")))
    (lisp-ai-bench:save (sym "BENCH-TEST-M"))
    (let ((text (text-of source)))
      (check "only the integer method changed" (and (search ":whole-number" text) (not (search ":integer)" text))
                                                     (search ":string" text)))
      (check-equal "the file still reads as nine forms" (length (forms-of source)) 9))))

(defun run-progn (root)
  "Definitions sent inside one top-level progn are recorded one by one, each with its own text, the comment lines above
   it included and the progn's indentation removed; save-all writes them as if typed on their own. A progn whose forms
   cannot be matched to its text is refused, and nothing is recorded."
  (let ((source (merge-pathnames "src/t.lisp" root)))
    (evaluate "(progn
  ;; p first
  (defun bench-test-p () 1)
  (defun bench-test-q ()
    (bench-test-p))
  (bench-test-q))")
    (check-equal "both definitions are pending, in order" (mapcar #'second (lisp-ai-bench:pending))
     (list (sym "BENCH-TEST-P") (sym "BENCH-TEST-Q")))
    (check-equal "each has its own text: p with the comment above it, both at the margin, q keeping its inner indentation"
     (mapcar #'lisp-ai-bench::entry-text (lisp-ai-bench::entries))
     (list (format nil ";; p first~%(defun bench-test-p () 1)") (format nil "(defun bench-test-q ()~%  (bench-test-p))")))
    (check-equal "save-all writes both" (length (lisp-ai-bench:save-all :file "src/t.lisp")) 2)
    (let ((text (text-of source)))
      (check "p is in the file with its comment, at the margin"
       (search (format nil "~%;; p first~%(defun bench-test-p () 1)") text))
      (check "q is in the file" (search (format nil "~%(defun bench-test-q ()~%  (bench-test-p))") text)))
    (let ((message
           (refusal
            (lambda ()
              (lisp-ai-bench:record-form :after '(progn (defun bench-test-r () 1) (defun bench-test-s () 2))
                                         "(progn (defun bench-test-r () 1))")))))
      (check "a progn whose text does not match its forms is refused"
       (and message (search "could not be matched" message))))
    (check-equal "and nothing is recorded by it" (lisp-ai-bench:pending) 'nil)))

(defun run-drop-and-scratch (root)
  "drop forgets an experiment, with or without removing it from the image; a definition read in another package than the
   file's is refused."
  (declare (ignore root))
  (evaluate "(defun bench-test-z () 9)")
  (check-equal "z is pending" (mapcar #'second (lisp-ai-bench:pending)) (list (sym "BENCH-TEST-Z")))
  (lisp-ai-bench:drop (sym "BENCH-TEST-Z"))
  (check-equal "dropped, nothing is pending" (lisp-ai-bench:pending) '())
  (check "but the image still has it" (fboundp (sym "BENCH-TEST-Z")))
  (evaluate "(defun bench-test-z () 10)")
  (lisp-ai-bench:drop (sym "BENCH-TEST-Z") :unbind t)
  (check "dropped with :unbind, the image lost it" (not (fboundp (sym "BENCH-TEST-Z"))))
  (check "dropping what is not pending is refused" (refused-p (lambda () (lisp-ai-bench:drop (sym "BENCH-TEST-Z")))))
  (let ((package (make-package "BENCH-TEST-OTHER-PACKAGE" :use '(:cl))))
    (unwind-protect
         (progn
           (lisp-ai-debug:eval-with-debugging "(defun bench-test-foreign () 1)" package)
           (check "a definition read in another package than the file's is refused"
                  (refused-p (lambda () (lisp-ai-bench:save "BENCH-TEST-FOREIGN" :file "src/t.lisp"))))
           (lisp-ai-bench:drop "BENCH-TEST-FOREIGN"))
      (delete-package package))))

(defun run-loose (root)
  "loose-definitions lists a function the image holds and no file holds: one made inside a let, or one dropped from the
   ledger while the image keeps it; not one the ledger lists, nor one loaded from a file. pending-names names the
   ledger's, then the loose."
  (let ((package (make-package "BENCH-TEST-LOOSE" :use '(:cl))) (file (merge-pathnames "run/loose.lisp" root)))
    (unwind-protect
        (let ((lisp-ai-bench:*project* :bench-test-loose) (lisp-ai-bench::*ledger* 'nil))
          (lisp-ai-debug:eval-with-debugging "(defun listed () 1)" package)
          (lisp-ai-debug:eval-with-debugging "(let ((x 2)) (defun in-let () x))" package)
          (write-text file (format nil "(in-package :bench-test-loose)~%~%(defun from-file () 3)~%"))
          (let ((*compile-verbose* nil) (*compile-print* nil) (*load-verbose* nil))
            (load (compile-file file)))
          (check-equal "made inside a let, it is loose; listed by the ledger or loaded from a file, it is not"
           (mapcar #'symbol-name (lisp-ai-bench:loose-definitions)) '("IN-LET"))
          (check-equal "pending-names names the ledger's, then the loose" (lisp-ai-bench:pending-names)
           '("LISTED" "IN-LET"))
          (lisp-ai-bench:drop (find-symbol "LISTED" package))
          (check-equal "dropped from the ledger and kept in the image, it is loose"
           (mapcar #'symbol-name (lisp-ai-bench:loose-definitions)) '("IN-LET" "LISTED")))
      (delete-package package))))

(defun run-files (root)
  "new-file makes a source file with a header and an in-package, once; add-component puts a (:file ...) into the .asd after
   a named file or at the end, as forms, and refuses a duplicate."
  (let ((asd (merge-pathnames "benchproj.asd" root)))
    (let ((path (lisp-ai-bench:new-file "src/n.lisp" :package :cl-user)))
      (check "the new file exists" (probe-file path))
      (check "and starts with a header and an in-package"
             (let ((text (text-of path))) (and (eql 0 (search ";;;; src/n.lisp" text)) (search "(in-package :cl-user)" text))))
      (check "making it again is refused" (refused-p (lambda () (lisp-ai-bench:new-file "src/n.lisp")))))
    (write-text asd "(defsystem \"benchproj\"
  :description \"a scratch system\"   ; kept
  :components ((:module \"src\" :components ((:file \"a\") (:file \"c\")))))
")
    (lisp-ai-bench:add-component "b" :after "a")
    (lisp-ai-bench:add-component "d")
    (let* ((form (first (forms-of asd)))
           (module (first (getf (cddr form) :components)))
           (files (mapcar #'second (getf (cddr module) :components))))
      (check-equal "the files are in order" files '("a" "b" "c" "d"))
      (check "the comment in the .asd is kept" (search "; kept" (text-of asd))))
    (check "a duplicate is refused" (refused-p (lambda () (lisp-ai-bench:add-component "b"))))
    (check "a missing anchor is refused" (refused-p (lambda () (lisp-ai-bench:add-component "e" :after "zz"))))))

(defun run (directory)
  "Run every check with a scratch project inside DIRECTORY; returns the number of failures."
  (setf *failures* 0)
  (let* ((root (merge-pathnames "bench-test-project/" (uiop:ensure-directory-pathname directory)))
         (old-project lisp-ai-bench:*project*)
         (old-ledger (lisp-ai-bench:ledger)))
    (when (probe-file root) (uiop:delete-directory-tree root :validate t))
    (ensure-directories-exist (merge-pathnames "run/" root))
    (unwind-protect
         (progn
          (setf lisp-ai-bench:*project* root)
          (run-ledger root)
          (run-replace root)
          (run-anchors root)
          (run-kinds root)
          (run-progn root)
          (run-drop-and-scratch root)
          (run-loose root)
          (run-files root))
      (lisp-ai-bench:forget-all)
      (setf lisp-ai-bench:*project* old-project)
      (setf lisp-ai-bench::*ledger* old-ledger)
      (dolist (name *names*)
        (let ((symbol (sym name)))
          (when (fboundp symbol) (fmakunbound symbol))
          (when (boundp symbol) (makunbound symbol))))
      (when (fboundp (sym "BENCH-TEST-OTHER")) (fmakunbound (sym "BENCH-TEST-OTHER")))
      (when (probe-file root) (uiop:delete-directory-tree root :validate t))))
  *failures*)
