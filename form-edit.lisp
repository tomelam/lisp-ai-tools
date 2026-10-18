;;;; Change a Lisp file as forms without touching the rest of its text: find the span of each top-level form, replace
;;;; the span of a form named in REPLACE, insert forms before a named one, add forms at the end, and replace the header
;;;; comment. New forms are written by the printer (never by hand); everything else stays byte for byte, comments
;;;; included.
(asdf/operate:load-system "eclector")

(defpackage :form-edit (:use :cl))
(in-package :form-edit)

(defun file-text (path)
  (with-open-file (s path :external-format :utf-8)
    (let ((text (make-string (file-length s))))
      (subseq text 0 (read-sequence text s)))))

(defun skip-blank (text pos)
  "POS moved past whitespace and whole comment lines."
  (loop
    (cond ((>= pos (length text)) (return pos))
          ((member (char text pos) '(#\Space #\Newline #\Tab)) (incf pos))
          ((char= (char text pos) #\;) (setf pos (or (position #\Newline text :start pos) (length text))))
          (t (return pos)))))

(defun form-spans (text scratch)
  "A list of (start end form) for the top-level forms of TEXT, read in SCRATCH."
  (let ((*read-eval* nil) (*package* scratch) (pos 0) (spans '()))
    (loop
      (setf pos (skip-blank text pos))
      (when (>= pos (length text)) (return (nreverse spans)))
      (multiple-value-bind (form end) (read-from-string text t nil :start pos :preserve-whitespace t)
        (push (list pos end form) spans)
        (setf pos end)))))

(defun form-name (form)
  "The name a form is known by: the symbol or string after its operator, as text. NIL for a form with none."
  (and (consp form) (consp (cdr form))
       (let ((name (second form)))
         (cond ((symbolp name) (symbol-name name)) ((stringp name) name)))))

(defun printed (source scratch)
  "SOURCE, text of one or more forms, printed by the printer, separated by blank lines."
  (let ((*package* scratch) (*read-eval* nil))
    (with-output-to-string (out)
      (with-input-from-string (in source)
        (loop for first = t then nil
              for form = (read in nil :eof)
              until (eq form :eof)
              do (unless first (format out "~%~%"))
                 (write form :stream out :pretty t :case :downcase :right-margin 120))))))

(defun edit (path &key replace insert-before append header)
  "REPLACE and INSERT-BEFORE are alists of (name . source-text); APPEND and HEADER are text."
  (let* ((scratch (make-package (gensym "SCRATCH") :use '(:cl)))
         (text (file-text path))
         (spans (form-spans text scratch))
         (out (make-string-output-stream))
         (pos 0))
    (when header
      (let ((first-start (first (first spans))))
        (write-string header out)
        (setf pos first-start)))
    (dolist (span spans)
      (destructuring-bind (start end form) span
        (let ((name (form-name form)))
          (write-string text out :start pos :end start)
          (dolist (entry insert-before)
            (when (equal (car entry) name)
              (write-string (printed (cdr entry) scratch) out)
              (format out "~%~%")))
          (let ((r (assoc name replace :test #'equal)))
            (if r
                (write-string (printed (cdr r) scratch) out)
                (write-string text out :start start :end end)))
          (setf pos end))))
    (write-string text out :start pos)
    (when append
      (format out "~%~a~%" (printed append scratch)))
    (let ((new (get-output-stream-string out)))
      (with-open-file (s path :direction :output :if-exists :supersede :external-format :utf-8)
        (write-string new s)))
    (delete-package scratch)
    path))

(defun new-file (path header source)
  "Write a file made of HEADER (comment text) and the forms of SOURCE, printed."
  (let ((scratch (make-package (gensym "SCRATCH") :use '(:cl))))
    (with-open-file (s path :direction :output :if-exists :error :external-format :utf-8)
      (write-string header s)
      (format s "~%~a~%" (printed source scratch)))
    (delete-package scratch)
    path))

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

(defun read-nodes (text scratch)
  "The top-level nodes of TEXT, read in SCRATCH, each with the nodes inside it. A form that a #+ or #- excludes is not
   seen."
  (let ((*read-eval* nil) (*package* scratch) (client (make-instance 'span-client)))
    (with-input-from-string (in text)
      (loop for node = (eclector.parse-result:read client in nil :eof)
            until (eq node :eof)
            collect node))))

(defun sub-nodes (node expression)
  "The nodes inside NODE (NODE itself excluded, at any depth) whose value is EQUAL to EXPRESSION."
  (loop for child in (node-children node)
        when (equal (node-expression child) expression)
        collect child
        append (sub-nodes child expression)))

(defun only-node (text scratch what)
  "The one node TEXT reads as; an error naming WHAT if it reads as none or several."
  (let ((nodes (read-nodes text scratch)))
    (unless (= (length nodes) 1) (error "~a must be one form; it reads as ~d: ~s" what (length nodes) text))
    (first nodes)))

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

(defun edit-subform (path name old new)
  "In the one top-level form of the file PATH that is named NAME, replace the one subform that reads as OLD with NEW. OLD
   and NEW are text. The text of every other form, and the comments inside the form, stay byte for byte. An error, and
   no write, when NAME is not exactly one form, when OLD is not exactly one subform of it, when either is a form that
   #+ or #- governs, or when the new text does not read as the old form with that one substitution."
  (let* ((scratch (make-package (gensym "SCRATCH") :use '(:cl))) (text (file-text path)))
    (unwind-protect
        (let* ((nodes (read-nodes text scratch))
               (tops (remove-if-not (lambda (node) (equal (form-name (node-expression node)) name)) nodes))
               (old-expression (node-expression (only-node old scratch "OLD")))
               (new-expression (node-expression (only-node new scratch "NEW"))))
          (unless (= (length tops) 1)
            (error "~d top-level forms are named ~s in ~a; expected 1" (length tops) name path))
          (let* ((top (first tops)) (matches (sub-nodes top old-expression)))
            (unless (= (length matches) 1)
              (error "~d subforms of ~s read as ~s; expected 1 (name an enclosing list to tell them apart)"
                     (length matches) name old))
            (let ((match (first matches)))
              (when (and (char= (char text (node-start top)) #\#) (find (char text (1+ (node-start top))) "+-"))
                (error "The form ~s is under a #+ or #-: ask before changing it" name))
              (when (and (char= (char text (node-start match)) #\#) (find (char text (1+ (node-start match))) "+-"))
                (error "The subform ~s is under a #+ or #-: ask before changing it" old))
              (let* ((replacement (indented (printed new scratch) (column-of text (node-start match))))
                     (result
                      (concatenate 'string (subseq text 0 (node-start match)) replacement
                                   (subseq text (node-end match))))
                     (after (read-nodes result scratch)))
                (unless (= (length after) (length nodes))
                  (error "The edit would change the number of forms, from ~d to ~d: nothing written" (length nodes)
                         (length after)))
                (loop for before in nodes
                      for now in after
                      do (unless
                             (if (eq before top)
                                 (equal (node-expression now)
                                        (subst new-expression old-expression (node-expression top) :test #'equal))
                                 (equalp (node-expression now) (node-expression before)))
                           (error "The edit would not read back as the one change meant: nothing written")))
                (with-open-file (s path :direction :output :if-exists :supersede :external-format :utf-8)
                  (write-string result s))
                path))))
      (delete-package scratch))))
