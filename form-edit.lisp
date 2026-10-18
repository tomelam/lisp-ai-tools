;;;; Change a Lisp file as forms without touching the rest of its text: find the span of each top-level form, replace
;;;; the span of a form named in REPLACE, insert forms before a named one, add forms at the end, and replace the header
;;;; comment. New forms are written by the printer (never by hand); everything else stays byte for byte, comments
;;;; included.
(asdf/operate:load-system "eclector")

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
  "Comments and forms the features exclude are not nodes; their text stays where it is."
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
  "The top-level nodes of TEXT, read in SCRATCH, each with the nodes inside it. A form that a #+ or #- excludes is not
   seen."
  (let ((*read-eval* nil) (*package* scratch) (client (make-instance 'span-client)))
    (with-input-from-string (in text)
      (loop for node = (eclector.parse-result:read client in nil :eof)
            until (eq node :eof)
            collect node))))

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

(defun printed (source scratch)
  "SOURCE, text of one or more forms, printed by the printer, separated by blank lines."
  (let ((*package* scratch))
    (with-output-to-string (out)
      (loop for node in (read-nodes source scratch)
            for first = t then nil
            do (unless first
                 (format out "~%~%")) (write (node-expression node) :stream out :pretty t :case :downcase :right-margin
                                             120)))))

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

(defun edit (path &key replace delete insert-before append header)
  "Change the top-level forms of the file PATH. REPLACE and INSERT-BEFORE are alists of (name . source-text), DELETE is
   a list of names, APPEND and HEADER are text. Every other byte stays as it was. Deleting a form takes the comment
   after it on its line with it, not the comments above it. An error, and no write, when a name is not exactly one
   form, when a form to replace or delete is governed by #+ or #-, or when the result does not read back as the other
   forms, unchanged and in order."
  (with-scratch-package (scratch)
   (let* ((text (file-text path)) (nodes (read-nodes text scratch)) (operations 'nil) (removed 'nil) (added 0))
     (flet ((form-named (name what)
              (let ((found (remove-if-not (lambda (node) (equal (form-name (node-expression node)) name)) nodes)))
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
       (loop for (name . source) in insert-before
             do (let ((node (form-named name "insert before")))
                  (incf added (forms-in source))
                  (push (list (node-start node) (node-start node) (format nil "~a~%~%" (printed source scratch)))
                        operations)))
       (loop for (name . source) in replace
             do (let ((node (form-named name "replace")))
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
                (after (mapcar #'node-expression (read-nodes result scratch)))
                (untouched
                 (loop for node in nodes
                       unless (member node removed)
                       collect (node-expression node))))
           (unless (= (length after) (+ (length untouched) added))
             (error "The edit would leave ~d forms, not the ~d meant: nothing written" (length after)
                    (+ (length untouched) added)))
           (dolist (expression untouched)
             (let ((tail (member expression after :test #'same-form-p)))
               (unless tail (error "The edit would change a form it was not asked to: nothing written"))
               (setf after (cdr tail))))
           (write-file path result)
           path))))))

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
  "STRING with COLUMN spaces after each of its newlines, so a form printed on its own lines fits where it is put."
  (with-output-to-string (out)
    (loop for char across string
          do (write-char char out) (when (char= char #\Newline) (dotimes (i column) (write-char #\  out))))))

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
   file PATH, or, when NEW is NIL, cut that subform out."
  (with-scratch-package (scratch)
   (let* ((text (file-text path))
          (nodes (read-nodes text scratch))
          (tops (remove-if-not (lambda (node) (equal (form-name (node-expression node)) name)) nodes))
          (old-expression (node-expression (only-node old scratch "OLD")))
          (new-expression (and new (node-expression (only-node new scratch "NEW")))))
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
                   do (unless
                          (if (eq before top)
                              (same-form-p (node-expression now) expected)
                              (same-form-p (node-expression now) (node-expression before)))
                        (error "The edit would not read back as the one change meant: nothing written")))
             (write-file path result)
             path)))))))

(defun edit-subform (path name old new)
  "In the one top-level form of the file PATH that is named NAME, replace the one subform that reads as OLD with NEW. OLD
   and NEW are text. The text of every other form, and the comments inside the form, stay byte for byte. An error, and
   no write, when NAME is not exactly one form, when OLD is not exactly one subform of it, when either is a form that
   #+ or #- governs, or when the new text does not read as the old form with that one substitution."
  (unless (stringp new) (error "NEW must be text; use delete-subform to remove a subform"))
  (change-subform path name old new))

(defun delete-subform (path name old)
  "In the one top-level form of the file PATH that is named NAME, cut out the one subform that reads as OLD (text). Its
   whole line goes when it is alone on it. Refused, and nothing written, in the cases edit-subform refuses."
  (change-subform path name old nil))
