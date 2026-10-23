;;;; Change a Lisp file as forms without touching the rest of its text: find the span of each top-level form, replace
;;;; the span of a form named in REPLACE, insert forms before a named one, add forms at the end, and replace the header
;;;; comment. New forms are written by the printer (never by hand); everything else stays byte for byte, comments
;;;; included.

(defpackage :form-edit (:use :cl))
(in-package :form-edit)

(defvar *auto-packages*
  'nil
  "While text is read: the packages made for names of packages the image does not have, deleted afterwards.")

(defmacro with-scratch-package ((var) &body body)
  "Run BODY with VAR a fresh package that uses CL, to read text in. Afterwards VAR, and every package made for a package
   name the image lacks, is deleted, so reading a file never leaves a package behind."
  `(let ((,var (make-package (gensym "SCRATCH") :use '(:cl))) (*auto-packages* 'nil))
     (unwind-protect (progn ,@body) (mapc #'delete-package *auto-packages*) (delete-package ,var))))

(defun file-text (path)
  (with-open-file (s path :external-format :utf-8)
    (let ((text (make-string (file-length s))))
      (subseq text 0 (read-sequence text s)))))

(defvar *comment-ranges* 'nil "While text is read: the (start . end) of each comment, newest first.")

(defclass span-client (eclector.parse-result:parse-result-client) nil
          (:documentation
           "An Eclector client that records, for every expression read, where it starts and ends in the text."))

(defstruct node
  "One expression read from the text: its value, where its text starts and ends, and the nodes inside it."
  expression
  start
  end
  children)

(defmethod eclector.parse-result:make-expression-result ((client span-client) result children source)
  (make-node :expression result :start (car source) :end (cdr source) :children (remove nil children)))

(defmethod eclector.parse-result:make-skipped-input-result ((client span-client) stream reason children source)
  "Comments and forms the features exclude are not nodes; their text stays where it is. Comments are noted by position."
  (when (or (eq reason :block-comment) (and (consp reason) (eq (car reason) :line-comment)))
    (push source *comment-ranges*))
  nil)

(defmethod eclector.reader:interpret-symbol ((client span-client) input-stream package-indicator symbol-name internp)
  "A symbol of a package the image does not have is read into a stand-in package of that name, which exports it unless the
   text wrote two colons. The stand-in is deleted when the edit is done."
  (if (and (stringp package-indicator)
           (let ((package (find-package package-indicator)))
             (or (null package) (member package *auto-packages*))))
      (let* ((package
              (or (find-package package-indicator)
                  (car (push (make-package package-indicator :use 'nil) *auto-packages*))))
             (symbol (intern symbol-name package)))
        (unless internp (export symbol package))
        symbol)
      (call-next-method)))

(defmethod eclector.reader:wrap-in-quasiquote ((client span-client) form) (list 'sb-int:quasiquote form))

(defmethod eclector.reader:wrap-in-unquote ((client span-client) form)
  (sb-impl::%default-comma-constructor :expr form :kind 0))

(defmethod eclector.reader:wrap-in-unquote-splicing ((client span-client) form)
  "Eclector has one splicing comma; a ,. in the text is read, and so printed, as ,@."
  (sb-impl::%default-comma-constructor :expr form :kind 2))

(defun read-nodes (text scratch)
  "The top-level nodes of TEXT, read in SCRATCH, each with the nodes inside it; second value, the (start . end) of each
   comment in TEXT, in order. A form that a #+ or #- excludes is not seen."
  (let ((*read-eval* nil) (*package* scratch) (client (make-instance 'span-client)) (*comment-ranges* 'nil))
    (with-input-from-string (in text)
      (let ((nodes
             (loop for node = (eclector.parse-result:read client in nil :eof)
                   until (eq node :eof)
                   collect node)))
        (values nodes (reverse *comment-ranges*))))))

(defun same-form-p (a b)
  "Whether the forms A and B, read from text, are the same form: EQUALP, except that strings and characters are compared
   exactly and two uninterned symbols with one name are the same (each reading of #:x makes a new one)."
  (cond
   ((and (symbolp a) (symbolp b))
    (or (eq a b) (and (null (symbol-package a)) (null (symbol-package b)) (string= (symbol-name a) (symbol-name b)))))
   ((and (consp a) (consp b)) (and (same-form-p (car a) (car b)) (same-form-p (cdr a) (cdr b))))
   ((and (stringp a) (stringp b)) (string= a b)) ((and (characterp a) (characterp b)) (char= a b))
   ((and (sb-int:comma-p a) (sb-int:comma-p b))
    (and (eql (sb-int:comma-kind a) (sb-int:comma-kind b)) (same-form-p (sb-int:comma-expr a) (sb-int:comma-expr b))))
   (t (equalp a b))))

(defun sub-nodes (node expression)
  "The nodes inside NODE (NODE itself excluded, at any depth) whose value is EQUAL to EXPRESSION."
  (loop for child in (node-children node)
        when (same-form-p (node-expression child) expression)
        collect child
        append (sub-nodes child expression)))

(defun only-node (text scratch what)
  "The one node TEXT reads as; an error naming WHAT if it reads as none or several."
  (let ((nodes (read-nodes text scratch)))
    (unless (= (length nodes) 1) (error "~a must be one form; it reads as ~d: ~s" what (length nodes) text))
    (first nodes)))

(defun form-spans (text scratch)
  "A list of (start end form) for the top-level forms of TEXT, read in SCRATCH."
  (mapcar (lambda (node) (list (node-start node) (node-end node) (node-expression node))) (read-nodes text scratch)))

(defun form-name (form)
  "The name a form is known by: the symbol or string after its operator, as text. NIL for a form with none."
  (and (consp form) (consp (cdr form))
       (let ((name (second form)))
         (cond ((symbolp name) (symbol-name name)) ((stringp name) name)))))

(defun forms-matching (nodes key scratch)
  "The top-level NODES that KEY names. A string names a form by the symbol or string after its operator; (:form TEXT)
   names the form that reads as TEXT, for a form with no such name, like a (setf ...) or a (let ...)."
  (if (and (consp key) (eq (first key) :form))
      (let ((expression (node-expression (only-node (second key) scratch "The anchor"))))
        (remove-if-not (lambda (node) (same-form-p (node-expression node) expression)) nodes))
      (remove-if-not (lambda (node) (equal (form-name (node-expression node)) key)) nodes)))

(defparameter *loop-clause-starters*
  '("WITH" "FOR" "AS" "REPEAT" "WHILE" "UNTIL" "ALWAYS" "NEVER" "THEREIS" "WHEN" "IF" "UNLESS" "INITIALLY" "FINALLY"
    "NAMED")
  "The words that begin a clause of an extended LOOP, on a line of their own.")

(defparameter *loop-actions*
  '("DO" "DOING" "COLLECT" "COLLECTING" "APPEND" "APPENDING" "NCONC" "NCONCING" "SUM" "SUMMING" "COUNT" "COUNTING"
    "MAXIMIZE" "MAXIMIZING" "MINIMIZE" "MINIMIZING" "RETURN")
  "The words that begin an action. After WHEN, IF or UNLESS an action stays on the line of its condition.")

(defun loop-word-p (item words) (and item (symbolp item) (member (symbol-name item) words :test #'string=) t))

(defun extended-loop-p (form)
  "True when FORM is (LOOP symbol ...): a LOOP with keywords, not a plain one that repeats its forms."
  (and (consp form) (consp (cdr form)) (cadr form) (symbolp (cadr form)) (null (cdr (last form)))))

(defun loop-clauses (items)
  "ITEMS, the part of an extended LOOP after the word LOOP, as a list of clauses, each a list. A symbol that follows a word
   taking an argument (FOR, COLLECT, WHEN, INTO ...) is that argument, even when it is spelled like a loop word."
  (let ((clauses 'nil) (clause 'nil) (keyword nil) (conditional nil))
    (dolist (item items)
      (let* ((word
              (and item (symbolp item) (not (loop-word-p keyword *loop-clause-starters*))
                   (not (loop-word-p keyword *loop-actions*)) (not (loop-word-p keyword '("INTO")))))
             (action (and word (loop-word-p item *loop-actions*)))
             (starts
              (and word
                   (or (loop-word-p item *loop-clause-starters*)
                       (and action (not conditional) (not (loop-word-p keyword '("AND" "ELSE"))))))))
        (when (and starts clause) (push (nreverse clause) clauses) (setf clause 'nil))
        (cond ((and word (loop-word-p item '("WHEN" "IF" "UNLESS"))) (setf conditional t))
              (action (setf conditional nil)))
        (push item clause)
        (setf keyword (and word item))))
    (when clause (push (nreverse clause) clauses))
    (nreverse clauses)))

(defun print-loop (stream form)
  "Print an extended LOOP with each clause on its own line, under the first one."
  (pprint-logical-block (stream nil :prefix "(" :suffix ")")
    (write-string "loop " stream)
    (pprint-indent :current 0 stream)
    (loop for (clause . more) on (loop-clauses (rest form))
          do (pprint-logical-block (stream nil)
               (pprint-indent :block 2 stream)
               (loop for (item . rest) on clause
                     do (write item :stream stream) (when rest
                                                      (write-char #\  stream)
                                                      (pprint-newline :fill stream)))) (when more
                                                                                         (pprint-newline :mandatory
                                                                                                         stream)))))

(defparameter *layout-table*
  (let ((table (copy-pprint-dispatch nil)))
    (set-pprint-dispatch '(and (cons (eql loop)) (satisfies extended-loop-p)) #'print-loop 1 table)
    table)
  "The printer's dispatch table for the forms form-edit writes: the standard one, with its own layout for LOOP.")

(defun printed (source scratch)
  "SOURCE, text of one or more forms and the comments between them, printed by the printer: the forms are laid out by the
   printer and separated by blank lines, the comments are kept as typed (one after a form on its line stays there). A
   comment inside a form is an error, because the printer cannot keep it."
  (multiple-value-bind (nodes comments)
      (read-nodes source scratch)
    (dolist (comment comments)
      (when (some (lambda (node) (and (< (node-start node) (car comment)) (< (car comment) (node-end node)))) nodes)
        (error "A comment inside a form cannot be printed, and would be lost: ~s"
               (subseq source (car comment) (cdr comment)))))
    (let ((*package* scratch))
      (with-output-to-string (out)
        (let ((position 0) (previous-end nil) (first t))
          (flet ((comment-text (comment)
                   (string-right-trim '(#\  #\Tab #\Newline) (subseq source (car comment) (cdr comment))))
                 (on-line-of-previous-p (comment)
                   (and previous-end (not (find #\Newline source :start previous-end :end (car comment)))))
                 (comments-between (from to)
                   (remove-if-not (lambda (comment) (and (>= (car comment) from) (<= (cdr comment) to))) comments)))
            (dolist (node nodes)
              (let* ((before (comments-between position (node-start node)))
                     (trailing (remove-if-not #'on-line-of-previous-p before))
                     (leading (remove-if (lambda (comment) (member comment trailing)) before)))
                (dolist (comment trailing) (format out "  ~a" (comment-text comment)))
                (unless first (format out "~%~%"))
                (dolist (comment leading) (format out "~a~%" (comment-text comment)))
                (write (node-expression node) :stream out :pretty t :case :downcase :right-margin 120 :pprint-dispatch *layout-table*)
                (setf position (node-end node)
                      previous-end (node-end node)
                      first nil)))
            (let* ((after (comments-between position (length source)))
                   (trailing (remove-if-not #'on-line-of-previous-p after))
                   (leading (remove-if (lambda (comment) (member comment trailing)) after)))
              (dolist (comment trailing) (format out "  ~a" (comment-text comment)))
              (when leading (format out "~%~%~{~a~^~%~}" (mapcar #'comment-text leading))))))))))

(defun write-file (path text)
  (with-open-file (s path :direction :output :if-exists :supersede :external-format :utf-8) (write-string text s)))

(defun conditional-start-p (text start)
  "True when the form whose text starts at START is governed by #+ or #-."
  (and (< (1+ start) (length text)) (char= (char text start) #\#) (find (char text (1+ start)) "+-") t))

(defun blank-p (char) (member char '(#\  #\Tab)))

(defun deletion-end (text start end)
  "Where the deletion of the form at START..END ends: past the blanks and the comment after it on its line, past the
   newline, and past one more newline when a blank line comes before the form too, so no double gap is left."
  (let ((end (or (position-if-not #'blank-p text :start end) (length text))))
    (when (and (< end (length text)) (char= (char text end) #\;))
      (setf end (or (position #\Newline text :start end) (length text))))
    (when (and (< end (length text)) (char= (char text end) #\Newline))
      (incf end)
      (when
          (and (>= start 2) (char= (char text (- start 1)) #\Newline) (char= (char text (- start 2)) #\Newline)
               (< end (length text)) (char= (char text end) #\Newline))
        (incf end)))
    end))

(defun after-trailing-comment (text end)
  "END, moved to the end of the line when a comment follows it there, so an insertion after a form lands after its own
   trailing comment."
  (let ((next (or (position-if-not #'blank-p text :start end) (length text))))
    (if (and (< next (length text)) (char= (char text next) #\;))
        (or (position #\Newline text :start next) (length text))
        end)))

(defun file-package (nodes)
  "The package that the first (in-package ...) among NODES names, as the image has it; CL-USER for a file with none. An
   error when the image lacks the package."
  (let* ((node
          (find-if
           (lambda (node)
             (let ((x (node-expression node)))
               (and (consp x) (symbolp (car x)) (string= (symbol-name (car x)) "IN-PACKAGE") (consp (cdr x)))))
           nodes))
         (name (and node (string (second (node-expression node))))))
    (cond ((null name) (find-package "CL-USER")) ((find-package name))
          (t
           (error "The file is in package ~a, which this image lacks: define it first, or give the new code as text"
                  name)))))

(defun scratch-symbol-in (form)
  "A symbol of a package named SCRATCH inside FORM, else NIL. Such a symbol would be written as scratch::name."
  (cond ((symbolp form) (and form (symbol-package form) (string= (package-name (symbol-package form)) "SCRATCH") form))
        ((consp form) (or (scratch-symbol-in (car form)) (scratch-symbol-in (cdr form))))))

(defun source-text (source nodes)
  "SOURCE as text. A string is text already and is returned as it is. Anything else is a form, printed as the printer lays
   it out, in the package of the file whose top-level forms are NODES, so its symbols are written as that file would write
   them. NIL stays NIL. A form with a symbol from the SCRATCH package is refused: it would be written as scratch::name."
  (cond ((or (null source) (stringp source)) source)
        (t
         (let ((symbol (scratch-symbol-in source)))
           (when symbol
             (error "The form has the symbol ~s of the SCRATCH package; intern it in the file's package" symbol))
           (let ((*package* (file-package nodes)))
             (write-to-string source :pretty t :escape t :readably nil :circle nil :case :downcase :right-margin 120
                              :pprint-dispatch *layout-table*))))))

(defun path-source-text (source path)
  "SOURCE as text, as source-text makes it, for a form to be written into the file PATH."
  (if (or (null source) (stringp source))
      source
      (with-scratch-package (scratch) (source-text source (read-nodes (file-text path) scratch)))))

(defun alist-source-text (alist nodes)
  "ALIST, of (key . source) pairs, with each source made text by source-text."
  (mapcar (lambda (pair) (cons (car pair) (source-text (cdr pair) nodes))) alist))

(defvar *register-locations*
  t
  "When true, an edit that writes a function-defining top-level form also tells the image where the file keeps it, so that M-. finds it. Bind it to NIL to turn that off.")

(defparameter *definition-heads*
  '("DEFUN" "DEFMACRO" "DEFGENERIC" "DEFMETHOD")
  "The operators of the top-level forms whose source location is registered after an edit.")

(defun changed-nodes (untouched nodes)
  "The members of NODES, the top-level nodes of the text just written, that are not one of the UNTOUCHED expressions (taken in order, as edit checks them): the forms the edit wrote."
  (let ((remaining nodes) (matched 'nil))
    (dolist (expression untouched)
      (let ((tail (member expression remaining :key #'node-expression :test #'same-form-p)))
        (when tail (push (first tail) matched) (setf remaining (cdr tail)))))
    (remove-if (lambda (node) (member node matched)) nodes)))

(defun real-name (name scratch package)
  "NAME, a symbol or (setf symbol) read in SCRATCH, as the symbol of PACKAGE it stands for."
  (cond ((and (symbolp name) name (eq (symbol-package name) scratch)) (intern (symbol-name name) package))
        ((and (consp name) (consp (cdr name))) (list (first name) (real-name (second name) scratch package))) (t name)))

(defun register-locations (path text nodes changed scratch)
  "Tell the image where the file PATH keeps the definitions among the CHANGED nodes. TEXT is the whole file just written and NODES are its top-level nodes. Each defun, defmacro, defgeneric and defmethod that the image already has is compiled again from its text in the file, with the file name and the position, as Sly's C-c C-c does; then the image knows the file's copy and M-. finds it. A definition the image does not have is not defined: it is reported as skipped. Returns a plist: :registered (names), :skipped ((name reason) ...), :failed ((name compiler output) ...), and :note when nothing could be tried."
  (let ((compile
         (let ((package (find-package "SLYNK-BACKEND")))
           (and package
                (let ((symbol (find-symbol "SLYNK-COMPILE-STRING" package)))
                  (and symbol (fboundp symbol) symbol)))))
        (package (ignore-errors (file-package nodes))))
    (cond ((not *register-locations*) (list :registered 'nil :note "turned off by *register-locations*"))
          ((null compile)
           (list :registered 'nil :note
                 "Slynk is not loaded in this image, so it does not learn where the file keeps its definitions"))
          ((null package) (list :registered 'nil :note "the package of the file is not in this image"))
          (t
           (let ((registered 'nil) (skipped 'nil) (failed 'nil))
             (dolist (node changed)
               (let ((form (node-expression node)))
                 (when
                     (and (consp form) (symbolp (car form)) (consp (cdr form))
                          (member (symbol-name (car form)) *definition-heads* :test #'string=))
                   (let ((name (real-name (second form) scratch package)))
                     (if (not (fboundp name))
                         (push (list name "not defined in the image") skipped)
                         (let ((output (make-string-output-stream)) (ok nil))
                           (let ((*package* package)
                                 (*standard-output* output)
                                 (*error-output* output)
                                 (*compile-verbose* nil)
                                 (*compile-print* nil))
                             (handler-bind ((sb-kernel:redefinition-warning #'muffle-warning))
                               (setf ok
                                       (funcall compile (subseq text (node-start node) (node-end node)) :buffer
                                                (file-namestring path) :position (node-start node) :filename
                                                (namestring (truename path))))))
                           (if ok
                               (push name registered)
                               (push (list name (get-output-stream-string output)) failed))))))))
             (list :registered (nreverse registered) :skipped (nreverse skipped) :failed (nreverse failed)))))))

(defun image-form-p (intended read-back scratch package)
  "Whether READ-BACK, a form read from the file's text in SCRATCH, is the form INTENDED, which was given as a form (its
   symbols are the image's) or read from the source text (its symbols are in SCRATCH too). Like same-form-p, but a symbol
   that the file writes without a package prefix and that reads into SCRATCH is the one the image's PACKAGE, the package
   of the file, finds by that name: the symbol meant, and not another with the same name. Numbers must be EQL."
  (cond
   ((and (symbolp intended) (symbolp read-back))
    (or (eq intended read-back)
        (and (null (symbol-package intended)) (null (symbol-package read-back))
             (string= (symbol-name intended) (symbol-name read-back)))
        (and package (eq (symbol-package read-back) scratch)
             (eq intended (find-symbol (symbol-name read-back) package)))))
   ((and (consp intended) (consp read-back))
    (and (image-form-p (car intended) (car read-back) scratch package)
         (image-form-p (cdr intended) (cdr read-back) scratch package)))
   ((and (stringp intended) (stringp read-back)) (string= intended read-back))
   ((and (characterp intended) (characterp read-back)) (char= intended read-back))
   ((and (sb-int:comma-p intended) (sb-int:comma-p read-back))
    (and (eql (sb-int:comma-kind intended) (sb-int:comma-kind read-back))
         (image-form-p (sb-int:comma-expr intended) (sb-int:comma-expr read-back) scratch package)))
   ((numberp intended) (eql intended read-back)) (t (equalp intended read-back))))

(defun intended-forms (sources scratch)
  "The forms that the SOURCES of an edit mean: a source that is text means the forms it reads as, in SCRATCH; any other
   source is a form and means itself. NIL sources are left out."
  (loop for source in sources
        when (stringp source) append (mapcar #'node-expression (read-nodes source scratch)) else
        when source collect source))

(defun check-written-forms (intended nodes scratch package)
  "An error unless the top-level NODES, the forms an edit wrote into the file, read as the INTENDED forms: each node as
   one of them, each of them once, in any order. This is the file's copy compared with what the image meant."
  (let ((remaining (copy-list intended)))
    (dolist (node nodes)
      (let ((match
             (find (node-expression node) remaining :test
                   (lambda (written meant) (image-form-p meant written scratch package)))))
        (unless match
          (let ((*print-length* 8) (*print-level* 4) (*package* scratch))
            (error "The file would hold a form that is not the one meant: ~s. Nothing written"
                   (node-expression node))))
        (setf remaining (remove match remaining :count 1 :test #'eq))))
    (when remaining
      (let ((*print-length* 8) (*print-level* 4) (*package* scratch))
        (error "The file would lack ~d of the forms meant, the first being ~s. Nothing written" (length remaining)
               (first remaining))))))

(defun verify-written (path text)
  "An error unless the file PATH now holds exactly TEXT, read back from the disk: the check of the artifact itself, after
   the write. A file that is not what was meant is left for the caller to see; nothing is repaired."
  (let ((now (file-text path)))
    (unless (string= now text)
      (error
       "The file ~a was written, but reading it back does not give the text meant (~d characters, ~d meant): look at it before anything else"
       path (length now) (length text)))))

(defun edit (path &key replace delete insert-before insert-after append header)
  "Change the top-level forms of the file PATH. REPLACE, INSERT-BEFORE and INSERT-AFTER are alists of (key . source-text), DELETE is a list of keys, APPEND and HEADER are text. A key is a form's name (the symbol or string after its operator, in upper case) or, for a form with no name, (:form TEXT): the form that reads as TEXT. Every other byte stays as it was. New source text may carry comments between its forms, kept as typed; a comment inside a form is refused. Deleting a form takes the comment after it on its line with it, not the comments above it. An error, and no write, when a key is not exactly one form, when a form to replace or delete is governed by #+ or #-, or when the result does not read back as the other forms, unchanged and in order, followed by exactly the forms meant: the new forms in the file are compared with the forms the sources mean (a source that is text means the forms it reads as, a source that is a form means that form, its symbols the image's), so a printer that wrote something else is caught before the write. A source, and the new code of a subform, may also be a form, not text: any value but a string is printed, in the package of the file's in-package, so it can be built in the image (a form with a symbol of the SCRATCH package is refused). Comments in the new code need text. Returns PATH and, as a second value, what REGISTER-LOCATIONS did: the written definitions that the image already has are given their place in the file, so that M-. finds them."
  (with-scratch-package (scratch)
   (let* ((intended
           (intended-forms
            (append (mapcar #'cdr replace) (mapcar #'cdr insert-before) (mapcar #'cdr insert-after) (list append))
            scratch))
          (text (file-text path))
          (nodes (read-nodes text scratch))
          (replace (alist-source-text replace nodes))
          (insert-before (alist-source-text insert-before nodes))
          (insert-after (alist-source-text insert-after nodes))
          (append (source-text append nodes))
          (operations 'nil)
          (removed 'nil)
          (added 0))
     (flet ((form-named (name what)
              (let ((found (forms-matching nodes name scratch)))
                (unless (= (length found) 1)
                  (error "~d top-level forms are named ~s in ~a (to ~a); expected 1" (length found) name path what))
                (first found)))
            (forms-in (source)
              (length (read-nodes source scratch)))
            (remove-form (node name)
              (when (conditional-start-p text (node-start node))
                (error "The form ~s is under a #+ or #-: ask before changing it" name))
              (when (member node removed) (error "The form ~s is both replaced and deleted, or named twice" name))
              (push node removed)))
       (when header
         (push
          (list 0
                (if nodes
                    (node-start (first nodes))
                    (length text))
                header)
          operations))
       (progn
        (loop for (name . source) in insert-before
              do
                (let ((node (form-named name "insert before")))
                  (incf added (forms-in source))
                  (push (list (node-start node) (node-start node) (format nil "~a~%~%" (printed source scratch)))
                        operations)))
        (loop for (name . source) in insert-after
              do
                (let ((node (form-named name "insert after")))
                  (incf added (forms-in source))
                  (let ((at (after-trailing-comment text (node-end node))))
                    (push (list at at (format nil "~%~%~a" (printed source scratch))) operations)))))
       (loop for (name . source) in replace
             do
               (let ((node (form-named name "replace")))
                 (remove-form node name)
                 (incf added (forms-in source))
                 (push (list (node-start node) (node-end node) (printed source scratch)) operations)))
       (dolist (name delete)
         (let ((node (form-named name "delete")))
           (remove-form node name)
           (push (list (node-start node) (deletion-end text (node-start node) (node-end node)) "") operations)))
       (let ((out (make-string-output-stream)) (position 0))
         (dolist (operation (stable-sort (reverse operations) #'< :key #'first))
           (destructuring-bind
               (start end string)
               operation
             (when (< start position) (error "Two edits overlap at character ~d: nothing written" start))
             (write-string text out :start position :end start)
             (write-string string out)
             (setf position end)))
         (write-string text out :start position)
         (when append (incf added (forms-in append)) (format out "~%~a~%" (printed append scratch)))
         (let* ((result (get-output-stream-string out))
                (result-nodes (read-nodes result scratch))
                (after (mapcar #'node-expression result-nodes))
                (untouched
                 (loop for node in nodes
                       unless (member node removed) collect (node-expression node))))
           (unless (= (length after) (+ (length untouched) added))
             (error "The edit would leave ~d forms, not the ~d meant: nothing written" (length after)
                    (+ (length untouched) added)))
           (dolist (expression untouched)
             (let ((tail (member expression after :test #'same-form-p)))
               (unless tail (error "The edit would change a form it was not asked to: nothing written"))
               (setf after (cdr tail))))
           (progn
            (check-written-forms intended (changed-nodes untouched result-nodes) scratch
             (ignore-errors (file-package nodes)))
            (progn (write-file path result) (verify-written path result)))
           (values path
                   (register-locations path result result-nodes (changed-nodes untouched result-nodes) scratch))))))))

(defun new-file (path header source)
  "Write a file made of HEADER (comment text) and the forms of SOURCE, printed."
  (with-scratch-package (scratch)
   (with-open-file (s path :direction :output :if-exists :error :external-format :utf-8)
     (write-string header s)
     (format s "~%~a~%" (printed source scratch)))
   path))

(defun column-of (text position)
  "How many characters lie between the start of the line and POSITION."
  (let ((newline (position #\Newline text :end position :from-end t)))
    (- position
       (if newline
           (1+ newline)
           0))))

(defun indented (string column)
  "STRING with COLUMN spaces after each of its newlines that lies between tokens, so a form printed on its own lines
   fits where it is put. A newline inside a string, a |symbol| or after # is part of what is written and is left as it is."
  (with-output-to-string (out)
    (let ((i 0) (n (length string)) (inside nil))
      (loop while (< i n)
            do (let ((char (char string i)))
                 (cond
                  ((and (null inside) (char= char #\#) (< (+ i 2) n) (char= (char string (1+ i)) #\\))
                   (write-string string out :start i :end (+ i 3)) (incf i 3))
                  ((and inside (char= char #\\) (< (1+ i) n)) (write-string string out :start i :end (+ i 2))
                   (incf i 2))
                  (t (write-char char out)
                   (cond ((and (null inside) (member char '(#\" #\|))) (setf inside char))
                         ((eql char inside) (setf inside nil))
                         ((and (null inside) (char= char #\Newline)) (dotimes (k column) (write-char #\  out))))
                   (incf i))))))))

(defun subform-deletion (text start end)
  "The span to cut when deleting the subform at START..END: its whole line when it is alone on it; otherwise it and the
   blanks before it, or after it when it follows an open parenthesis or starts the line."
  (let* ((line-start
          (let ((newline (position #\Newline text :end start :from-end t)))
            (if newline
                (1+ newline)
                0)))
         (line-end (or (position #\Newline text :start end) (length text))))
    (cond
     ((and (every #'blank-p (subseq text line-start start)) (every #'blank-p (subseq text end line-end))
           (< line-end (length text)))
      (values line-start (1+ line-end)))
     ((and (> start 0) (blank-p (char text (1- start))))
      (values (1+ (or (position-if-not #'blank-p text :end start :from-end t) -1)) end))
     (t (values start (or (position-if-not #'blank-p text :start end) (length text)))))))

(defun without (expression old)
  "EXPRESSION with every element EQUAL to OLD removed from the lists inside it."
  (cond ((atom expression) expression) ((same-form-p (car expression) old) (without (cdr expression) old))
        (t (cons (without (car expression) old) (without (cdr expression) old)))))

(defun change-subform (path name old new)
  "The work of edit-subform and delete-subform: put the text NEW in place of the subform OLD of the form NAME in the
   file PATH, or, when NEW is NIL, cut that subform out. Returns PATH and, as a second value, what REGISTER-LOCATIONS did
   for the changed top-level form."
  (with-scratch-package (scratch)
   (let* ((text (file-text path))
          (nodes (read-nodes text scratch))
          (tops (forms-matching nodes name scratch))
          (old-expression (node-expression (only-node old scratch "OLD")))
          (new-expression
           (and new
                (progn
                 (when (nth-value 1 (read-nodes new scratch))
                   (error "NEW contains a comment, which cannot be put inside a form: ~s" new))
                 (node-expression (only-node new scratch "NEW"))))))
     (unless (= (length tops) 1) (error "~d top-level forms are named ~s in ~a; expected 1" (length tops) name path))
     (let* ((top (first tops)) (matches (sub-nodes top old-expression)))
       (unless (= (length matches) 1)
         (error "~d subforms of ~s read as ~s; expected 1 (name an enclosing list to tell them apart)" (length matches)
                name old))
       (let ((match (first matches)))
         (when (conditional-start-p text (node-start top))
           (error "The form ~s is under a #+ or #-: ask before changing it" name))
         (when (conditional-start-p text (node-start match))
           (error "The subform ~s is under a #+ or #-: ask before changing it" old))
         (multiple-value-bind (from to)
             (if new
                 (values (node-start match) (node-end match))
                 (subform-deletion text (node-start match) (node-end match)))
           (let* ((replacement
                   (if new
                       (indented (printed new scratch) (column-of text (node-start match)))
                       ""))
                  (result (concatenate 'string (subseq text 0 from) replacement (subseq text to)))
                  (after (read-nodes result scratch))
                  (expected
                   (if new
                       (subst new-expression old-expression (node-expression top) :test #'same-form-p)
                       (without (node-expression top) old-expression))))
             (unless (= (length after) (length nodes))
               (error "The edit would change the number of forms, from ~d to ~d: nothing written" (length nodes)
                      (length after)))
             (loop for before in nodes
                   for now in after
                   do
                     (unless
                         (if (eq before top)
                             (same-form-p (node-expression now) expected)
                             (same-form-p (node-expression now) (node-expression before)))
                       (error "The edit would not read back as the one change meant: nothing written")))
             (progn (write-file path result) (verify-written path result))
             (values path (register-locations path result after (list (nth (position top nodes) after)) scratch)))))))))

(defun edit-subform (path name old new)
  "In the one top-level form of the file PATH that is named NAME, replace the one subform that reads as OLD with NEW. OLD
   and NEW are text. The text of every other form, and the comments inside the form, stay byte for byte. An error, and
   no write, when NAME is not exactly one form, when OLD is not exactly one subform of it, when either is a form that
   #+ or #- governs, or when the new text does not read as the old form with that one substitution. NAME may also be (:form TEXT), and NEW may not contain a comment. OLD and NEW may be forms too, as for edit."
  (unless new (error "NEW is missing; use delete-subform to remove a subform"))
  (change-subform path name (path-source-text old path) (path-source-text new path)))

(defun delete-subform (path name old)
  "In the one top-level form of the file PATH that is named NAME, cut out the one subform that reads as OLD (text). Its
   whole line goes when it is alone on it. Refused, and nothing written, in the cases edit-subform refuses."
  (change-subform path name (path-source-text old path) nil))
