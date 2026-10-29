;;;; The workbench, for every project (docs/workbench.md of the tree): Lisp is changed in the image and saved from it. The
;;;; image keeps a ledger of the definitions made through bin/lisp eval (the eval loop of lisp-ai-debug calls
;;;; RECORD-FORM before and after each top-level form; the definitions inside a top-level progn are recorded one by one),
;;;; with the text of each as it was typed, comments included, since READ drops comments. The commands, called by their
;;;; bare names because a project's package uses this one:
;;;;   (pending)                     the unsaved definitions
;;;;   (save name &key file after before drop-comments drop-docstring)   write one to its file, as typed
;;;;   (save-all &key file)          every unsaved one, in the order made
;;;;   (drop name &key unbind)       forget an experiment
;;;;   (where name)                  the file and offset the image records for a definition
;;;;   (run-tests)                   the project's own suite, inside this image, against what the image holds
;;;;   (new-file path &key package)  a new source file with a header and an in-package
;;;;   (add-component name &key after module system)   a new (:file ...) in the project's .asd
;;;;   (loose-definitions)           the functions the image holds that no file holds and the ledger does not list
;;;; The ledger is mirrored to run/pending.sexp after every change, and bin/lisp stop refuses while it is not empty or a
;;;; function is loose.
(defpackage :lisp-ai-bench
  (:use :cl)
  (:export #:configure
           #:*project*
           #:record-form
           #:pending
           #:pending-names
           #:loose-definitions
           #:save
           #:save-all
           #:drop
           #:where
           #:run-tests
           #:new-file
           #:add-component
           #:ledger
           #:forget-all))
(in-package :lisp-ai-bench)

(defvar *project* nil
  "The project this image serves: a pathname, a string with a slash, or the name of its ASDF system (:aquarium), as
   lisp-ai-project reads it. Set by CONFIGURE from the project's bin/serve.lisp. Without it the package of each request
   names the project, as a system of the same name.")

(defstruct entry
  kind        ; the definer, as a keyword: :defun :defmacro :defvar :defmethod ...
  name        ; the symbol (or (setf symbol)) the definition is known by
  key         ; what makes it one of a kind: the name, or for a method (name qualifiers specializers)
  form        ; the form as read
  text        ; the text as typed, leading comment lines included
  package     ; the package the form was read in
  file        ; the file the image recorded for the name before the form ran, or NIL
  (state :unsaved)
  time)

(defvar *ledger* '() "The entries, oldest first.")
(defvar *ledger-lock* (sb-thread:make-mutex :name "bench ledger"))
(defvar *before* (make-hash-table :test 'equal)
  "The file known for each definition about to run, from the :before call of RECORD-FORM, by key.")

(defparameter *definers*
  '(defun defmacro defgeneric defmethod defvar defparameter defconstant defclass defstruct deftype define-condition
    define-modify-macro define-symbol-macro define-setf-expander defsetf declaim)
  "The operators whose top-level forms the ledger records.")

(defun project ()
  (or *project* (intern (string-upcase (package-name *package*)) :keyword)))

(defun project-directory ()
  (lisp-ai-project:project-directory (project)))

(defun configure (&key project)
  "Say which project this image serves. Reports, as a fact, unsaved definitions that a previous image of this project left
   in run/pending.sexp; they are gone with it."
  (setf *project* project)
  (let ((left (previous-pending)))
    (when left
      (format t "~&;; bench: the previous image left ~d unsaved definition~:p in ~a: ~{~a~^, ~}~%"
              (length left) (ledger-file) (mapcar (lambda (e) (getf e :name)) left))))
  (persist)
  (project))

;;; What a form defines

(defun definer-of (form)
  "The definer keyword of FORM when it is a top-level definition, else NIL."
  (and (consp form) (symbolp (car form))
       (let ((definer (find (symbol-name (car form)) *definers* :key #'symbol-name :test #'string=)))
         (and definer (intern (symbol-name definer) :keyword)))))

(defun required-specializers (lambda-list)
  "The specializers of the required parameters of a method's LAMBDA-LIST."
  (loop for parameter in lambda-list
        until (member parameter lambda-list-keywords)
        collect (if (consp parameter) (second parameter) t)))

(defun method-parts (form)
  "(name qualifiers specializers) of a DEFMETHOD form."
  (let* ((name (second form))
         (rest (cddr form))
         (qualifiers (loop while (and rest (not (listp (first rest)))) collect (pop rest))))
    (list name qualifiers (required-specializers (first rest)))))

(defun entry-name-and-key (kind form)
  "(values name key) for a definition FORM of KIND."
  (case kind
    (:defmethod (let ((parts (method-parts form))) (values (first parts) parts)))
    (:defstruct (let ((name (if (consp (second form)) (first (second form)) (second form)))) (values name name)))
    (:declaim (values (prin1-to-string form) (prin1-to-string form)))
    (t (values (second form) (second form)))))

(defun introspect-types (kind)
  "The sb-introspect definition types a definition of KIND may be recorded under."
  (case kind
    (:defun '(:function)) (:defmacro '(:macro)) (:defgeneric '(:generic-function)) (:defmethod '(:generic-function))
    ((:defvar :defparameter :defconstant) '(:variable)) (:defclass '(:class)) (:defstruct '(:structure))
    (:deftype '(:type)) (:define-condition '(:condition)) (:define-symbol-macro '(:symbol-macro))
    (:defsetf '(:setf-expander)) (:define-setf-expander '(:setf-expander)) (t '())))

(defun recorded-source (name kind)
  "(values pathname offset) that the image records for the definition NAME of KIND, or NIL."
  (when (or (symbolp name) (and (consp name) (eq (first name) 'setf)))
    (dolist (type (introspect-types kind))
      (dolist (source (ignore-errors (sb-introspect:find-definition-sources-by-name name type)))
        (let ((path (sb-introspect:definition-source-pathname source)))
          (when path
            (return-from recorded-source
              (values path (sb-introspect:definition-source-character-offset source)))))))))

;;; Recording

(defun record-form (when form text)
  "Called by the eval loop of lisp-ai-debug with WHEN :before and :after each top-level form of a request. Before, the file
   the image records for the definition is noted (the form's own run would wipe it); after, the definition is entered in
   the ledger with that file and the TEXT as typed. Forms that define nothing are ignored. A top-level PROGN is looked
   into: each definition inside it is recorded as its own, with its own span of the text (PROGN-PARTS); no other wrapper
   is."
  (if (and (consp form) (eq (car form) 'progn))
      (loop for (subform . subtext) in (progn-parts form text)
            do (record-form when subform subtext))
      (let ((kind (definer-of form)))
        (when kind
          (multiple-value-bind (name key)
              (entry-name-and-key kind form)
            (ecase when
              (:before (setf (gethash (list kind key) *before*) (or (recorded-source name kind) :none)))
              (:after
               (let* ((known (gethash (list kind key) *before*))
                      (file (cond ((pathnamep known) known) ((eq known :none) nil) (t (recorded-source name kind))))
                      (entry
                       (make-entry :kind kind :name name :key key :form form :text (trim-text text) :package *package*
                        :file file :time (get-universal-time))))
                 (remhash (list kind key) *before*)
                 (sb-thread:with-mutex (*ledger-lock*)
                   (setf *ledger*
                           (append
                            (remove-if (lambda (e) (and (eq (entry-kind e) kind) (equal (entry-key e) key))) *ledger*)
                            (list entry))))
                 (persist)))))))))

(defun trim-text (text)
  "TEXT without blank lines before its first line or after its last, as it will be written."
  (string-trim '(#\Newline #\Space #\Tab #\Return) text))

(defparameter *indentation* (list (code-char 32) (code-char 9)) "A space and a tab: what indentation is made of.")

(defun line-start (text position)
  "POSITION moved back to the start of its line when only spaces and tabs come before it on that line, else POSITION."
  (let ((start position))
    (loop while (and (plusp start) (member (char text (1- start)) *indentation*))
          do (decf start))
    (if (or (zerop start) (char= (char text (1- start)) #\Newline))
        start
        position)))

(defun dedent (text)
  "TEXT with the indentation its lines share removed: the smallest count of leading spaces and tabs among its lines that
   are not blank, taken off every line."
  (let* ((lines (uiop/utility:split-string text :separator (string #\Newline)))
         (indents
          (loop for line in lines
                for indent = (or (position-if-not (lambda (c) (member c *indentation*)) line) (length line))
                when (< indent (length line)) collect indent))
         (common
          (if indents
              (reduce #'min indents)
              0)))
    (format nil "~{~a~^~%~}" (mapcar (lambda (line) (subseq line (min common (length line)))) lines))))

(defun progn-parts (form text)
  "The forms inside the top-level PROGN FORM, each with its own span of TEXT, the comment lines above it included and the
   indentation they share removed, as a list of (form . text). An error, and nothing recorded, when the spans read from
   TEXT do not match the forms one for one."
  (form-edit::with-scratch-package (scratch)
    (let* ((nodes (form-edit::read-nodes text scratch))
           (children (and (= (length nodes) 1) (rest (form-edit::node-children (first nodes))))))
      (unless (= (length children) (length (rest form)))
        (error
         "bench: the ~d form~:p inside this progn could not be matched to its text (~d span~:p read): define them as top-level forms, not inside progn"
         (length (rest form)) (length children)))
      (loop for subform in (rest form)
            for child in children
            collect
              (cons subform
                    (dedent
                     (subseq text (leading-comment-start text (line-start text (form-edit::node-start child)))
                             (form-edit::node-end child))))))))

;;; The ledger, read

(defun entries (&key (state :unsaved))
  (sb-thread:with-mutex (*ledger-lock*)
    (remove-if-not (lambda (e) (or (null state) (eq (entry-state e) state))) *ledger*)))

(defun ledger () "Every entry, saved ones too." (entries :state nil))

(defun name-string (name)
  (cond ((stringp name) name)
        ((consp name) (format nil "(SETF ~a)" (symbol-name (second name))))
        (t (symbol-name name))))

(defun entries-named (name &key (state :unsaved))
  "The entries whose name is NAME (a symbol, a string, or (setf symbol)), newest last."
  (let ((wanted (name-string name)))
    (remove-if-not (lambda (e) (string= (name-string (entry-name e)) wanted)) (entries :state state))))

(defun pending ()
  "The unsaved definitions, oldest first, each as (kind name file-or-nil)."
  (mapcar (lambda (e) (list (entry-kind e) (entry-name e) (and (entry-file e) (enough-namestring (entry-file e) (project-directory)))))
          (entries)))

(defun project-packages ()
  "The packages whose functions LOOSE-DEFINITIONS examines: the project's own and its tests' (<PROJECT>/TESTS), those
   of them that exist. None when the project is named by a directory, as in the tests."
  (let ((project (project)))
    (when (symbolp project)
      (remove nil (mapcar #'find-package (list (symbol-name project) (format nil "~a/TESTS" (symbol-name project))))))))

(defun loose-definitions (&optional (packages (project-packages)))
  "The functions, macros and generic functions of PACKAGES (by default the project's own and its tests') that the image
   holds, that no file holds and that the ledger does not list: made inside a LET, an EVAL-WHEN or a function, or
   dropped from the ledger without :UNBIND. A restart loses them, and SAVE cannot write them, since the ledger has no
   text for them: define each again at top level through bin/lisp eval, then save it. A function counts as held by a
   file when SBCL records a file for it or for one of its methods. A (SETF NAME) function is not examined. The symbols,
   sorted by name."
  (let ((listed (mapcar #'entry-name (entries :state nil))) (found 'nil))
    (dolist (package packages)
      (do-symbols (symbol package)
        (when
            (and (eq (symbol-package symbol) package) (fboundp symbol) (not (special-operator-p symbol))
                 (not (member symbol listed))
                 (notany #'sb-introspect:definition-source-pathname
                         (loop for type in '(:function :macro :generic-function :method)
                               append (sb-introspect:find-definition-sources-by-name symbol type))))
          (pushnew symbol found))))
    (sort found #'string< :key #'symbol-name)))

(defun pending-names ()
  "The names of the definitions the image holds and no file holds, as strings: the unsaved ones of the ledger, then the
   loose ones (LOOSE-DEFINITIONS). What bin/lisp stop asks before stopping the image, and what bin/lisp status shows."
  (append (mapcar (lambda (e) (name-string (entry-name e))) (entries)) (mapcar #'name-string (loose-definitions))))

(defun drop (name &key unbind)
  "Forget the unsaved definitions named NAME. With UNBIND, also remove them from the image (fmakunbound, makunbound)."
  (let ((dropped (entries-named name)))
    (unless dropped (error "bench: nothing unsaved is named ~a" name))
    (sb-thread:with-mutex (*ledger-lock*) (setf *ledger* (set-difference *ledger* dropped)))
    (when unbind
      (dolist (e dropped)
        (case (entry-kind e)
          ((:defun :defmacro :defgeneric) (when (fboundp (entry-name e)) (fmakunbound (entry-name e))))
          ((:defvar :defparameter) (when (boundp (entry-name e)) (makunbound (entry-name e)))))))
    (persist)
    (mapcar #'entry-kind dropped)))

(defun forget-all ()
  "Empty the ledger (the tests use it). The definitions stay in the image."
  (sb-thread:with-mutex (*ledger-lock*) (setf *ledger* '()))
  (persist)
  nil)

(defun where (name &optional kind)
  "Where the image keeps the definition NAME: (values :unsaved file-or-nil) while it waits in the ledger, (values file
   offset) when a file is recorded, :unknown otherwise. KIND (a definer keyword) narrows the search."
  (let ((unsaved (entries-named name)))
    (if unsaved
        (values :unsaved (entry-file (car (last unsaved))))
        (let ((kinds (if kind (list kind) '(:defun :defmacro :defgeneric :defvar :defclass :defstruct :deftype :define-condition))))
          (dolist (k kinds :unknown)
            (multiple-value-bind (path offset) (recorded-source (if (stringp name) (read-from-string name) name) k)
              (when path (return (values path offset k)))))))))

;;; The mirror in run/pending.sexp

(defun ledger-file ()
  (merge-pathnames "run/pending.sexp" (project-directory)))

(defun persist ()
  "Write the unsaved entries to run/pending.sexp, so that they can be read without the image and a crash does not lose the
   list. Without a project nothing is written."
  (ignore-errors
   (when *project*
     (let ((file (ledger-file)))
       (ensure-directories-exist file)
       (with-open-file (out file :direction :output :if-exists :supersede :external-format :utf-8)
         (let ((*package* (find-package :cl)) (*print-readably* nil))
           (format out ";;; unsaved definitions of the image, written by lisp-ai-bench after every change~%")
           (dolist (e (entries))
             (prin1 (list :kind (entry-kind e) :name (name-string (entry-name e))
                          :package (package-name (entry-package e))
                          :file (and (entry-file e) (namestring (entry-file e))) :time (entry-time e)
                          :text (entry-text e))
                    out)
             (terpri out))))))))

(defun previous-pending ()
  "The entries a previous image left in run/pending.sexp, as plists; none when the file is missing or empty."
  (let ((file (ignore-errors (ledger-file))))
    (when (and file (probe-file file))
      (with-open-file (in file :external-format :utf-8)
        (let ((*read-eval* nil) (*package* (find-package :keyword)))
          (loop for form = (read in nil in) until (eq form in) collect form))))))

;;; Saving: the text as typed is spliced into the file, over the old definition or at an anchor, and the result is checked
;;; as form-edit checks its own writes.

(defun resolve (file)
  (merge-pathnames file (project-directory)))

(defun comment-line-p (line)
  (let ((start (position-if-not (lambda (c) (member c '(#\Space #\Tab))) line)))
    (and start (char= (char line start) #\;))))

(defun leading-comment-start (text node-start)
  "The position where the comment lines immediately above the form at NODE-START begin (no blank line between them and the
   form), or NODE-START when there are none."
  (let ((position node-start))
    (loop
      (let* ((line-start (if (and (plusp position) (char= (char text (1- position)) #\Newline))
                             (or (and (> position 1) (position #\Newline text :end (1- position) :from-end t)) -1)
                             (return position)))
             (line (subseq text (1+ line-start) (1- position))))
        (if (comment-line-p line)
            (setf position (1+ line-start))
            (return position))))))

(defun text-comments (text scratch)
  "The comments inside TEXT (one definition as typed), as trimmed strings: those inside the form and those above it."
  (multiple-value-bind (nodes comments) (form-edit::read-nodes text scratch)
    (declare (ignore nodes))
    (mapcar (lambda (range) (string-trim '(#\Space #\Tab #\Newline) (subseq text (car range) (cdr range)))) comments)))

(defun docstring-of (form kind)
  "The documentation string of a definition FORM of KIND, or NIL."
  (case kind
    ((:defun :defmacro :deftype :define-modify-macro)
     (let ((body (cdddr form))) (and (stringp (first body)) (rest body) (first body))))
    ((:defvar :defparameter :defconstant) (let ((doc (fourth form))) (and (stringp doc) doc)))
    (:defgeneric (second (find :documentation (cdddr form) :key (lambda (o) (and (consp o) (first o))))))
    ((:defclass :define-condition)
     (second (find :documentation (cddddr form) :key (lambda (o) (and (consp o) (first o))))))
    (:defstruct (let ((doc (third form))) (and (stringp doc) doc)))
    (t nil)))

(defun form-key-matches-p (expression entry scratch)
  "True when EXPRESSION, a top-level form read from the file in SCRATCH, is the definition ENTRY stands for: same definer,
   same name, and for a method the same qualifiers and specializers (compared by name, since the packages differ)."
  (declare (ignore scratch))
  (let ((kind (definer-of expression)))
    (and (eq kind (entry-kind entry))
         (flet ((same-name (a b) (string= (name-string a) (name-string b))))
           (if (eq kind :defmethod)
               (destructuring-bind (name qualifiers specializers) (method-parts expression)
                 (destructuring-bind (ename equalifiers especializers) (entry-key entry)
                   (and (same-name name ename)
                        (equal (mapcar #'name-string qualifiers) (mapcar #'name-string equalifiers))
                        (equal (mapcar #'prin1-to-string specializers) (mapcar #'prin1-to-string especializers)))))
               (multiple-value-bind (name) (entry-name-and-key kind expression)
                 (same-name name (entry-name entry))))))))

(defun splice (path text start end new-text nodes replaced scratch)
  "The file PATH, whose text is TEXT with top-level NODES, with NEW-TEXT in place of the characters START..END (REPLACED
   is the node that span held, or NIL). Checked before the write as form-edit checks its own: the other forms read back
   unchanged and in order, the new forms read back as the forms NEW-TEXT means; then the file is read back and the
   definitions it now holds are registered with the image, so M-. finds them. Returns the register report."
  (let* ((result (concatenate 'string (subseq text 0 start) new-text (subseq text end)))
         (result-nodes (form-edit::read-nodes result scratch))
         (intended (mapcar #'form-edit::node-expression (form-edit::read-nodes new-text scratch)))
         (untouched (loop for node in nodes unless (eq node replaced) collect (form-edit::node-expression node)))
         (after (mapcar #'form-edit::node-expression result-nodes)))
    (unless (= (length after) (+ (length untouched) (length intended)))
      (error "bench: the save would leave ~d forms in ~a, not the ~d meant: nothing written" (length after) path
             (+ (length untouched) (length intended))))
    (dolist (expression untouched)
      (let ((tail (member expression after :test #'form-edit::same-form-p)))
        (unless tail (error "bench: the save would change a form it was not asked to in ~a: nothing written" path))
        (setf after (cdr tail))))
    (form-edit::check-written-forms intended (form-edit::changed-nodes untouched result-nodes) scratch
                                    (ignore-errors (form-edit::file-package nodes)))
    (form-edit::write-file path result)
    (form-edit::verify-written path result)
    (form-edit::register-locations path result result-nodes (form-edit::changed-nodes untouched result-nodes) scratch)))

(defun save (name &key file after before drop-comments drop-docstring)
  "Write the unsaved definition NAME into FILE (default: the file the image recorded for it; none known is an error that
   says to give :file), as it was typed, comments and docstring included. A definition the file already has is replaced in
   place (its own comment lines above it too, when the new text brings some); a new one goes after the form AFTER, before
   the form BEFORE, or at the end. Refused, and nothing written: when the file's definition has comments inside it that
   the new text lacks, or a docstring the new form lacks (DROP-COMMENTS and DROP-DOCSTRING say it is meant); when the
   definition was made in the SCRATCH package; when FILE does not exist (new-file first). Several unsaved definitions of
   one name (methods) are all saved. Returns the file and the register report of the last one."
  (let ((entries (entries-named name)) (path nil) (report nil))
    (unless entries (error "bench: nothing unsaved is named ~a; (pending) lists what there is" name))
    (dolist (entry entries)
      (when (string= (package-name (entry-package entry)) "SCRATCH")
        (error "bench: ~a was defined in the SCRATCH package; define it in the project's package (bin/lisp eval reads there) and save that" name))
      (setf path (resolve (or file (entry-file entry)
                              (error "bench: no file is known for ~a (it is new to the image): say :file \"src/x.lisp\"" name))))
      (unless (probe-file path)
        (error "bench: ~a does not exist; (new-file ~s) makes it" path (enough-namestring path (project-directory))))
      (form-edit::with-scratch-package (scratch)
        (let* ((text (form-edit::file-text path))
               (nodes (form-edit::read-nodes text scratch))
               (old (find-if (lambda (node) (form-key-matches-p (form-edit::node-expression node) entry scratch)) nodes))
               (new-text (entry-text entry))
               (new-comments (text-comments new-text scratch)))
          (let ((file-package (handler-case (form-edit::file-package nodes) (error (e) (error "bench: ~a" e)))))
            (unless (eq file-package (entry-package entry))
              (error "bench: ~a was read in package ~a but ~a is in package ~a: define it in that package (an (in-package ...) form first in the same request) and save that"
                     name (package-name (entry-package entry)) (enough-namestring path (project-directory))
                     (package-name file-package))))
          (cond
            (old
             (let* ((old-text (subseq text (form-edit::node-start old) (form-edit::node-end old)))
                    (old-comments (text-comments old-text scratch))
                    (missing (remove-if (lambda (c) (member c new-comments :test #'string=)) old-comments))
                    (old-doc (docstring-of (form-edit::node-expression old) (entry-kind entry)))
                    (new-doc (docstring-of (entry-form entry) (entry-kind entry))))
               (when (and missing (not drop-comments))
                 (error "bench: the definition of ~a in ~a has ~d comment~:p inside it that the new text lacks:~{~%  ~a~}~%Carry them over, or say :drop-comments t"
                        name (enough-namestring path (project-directory)) (length missing) missing))
               (when (and old-doc (not new-doc) (not drop-docstring))
                 (error "bench: the definition of ~a in ~a has a docstring that the new form lacks: ~s~%Carry it over, or say :drop-docstring t"
                        name (enough-namestring path (project-directory)) old-doc))
               (let ((start (if (comment-line-p (first (uiop:split-string new-text :separator '(#\Newline))))
                                (leading-comment-start text (form-edit::node-start old))
                                (form-edit::node-start old))))
                 (setf report (splice path text start (form-edit::node-end old) new-text nodes old scratch)))))
            (t
             (flet ((anchor (key what)
                      (let ((found (form-edit::forms-matching nodes (string-upcase (name-string key)) scratch)))
                        (unless (= (length found) 1)
                          (error "bench: ~d forms are named ~a in ~a (to insert ~a); expected 1" (length found) key path what))
                        (first found))))
               (cond
                 (after (let ((at (form-edit::after-trailing-comment text (form-edit::node-end (anchor after "after")))))
                          (setf report (splice path text at at (format nil "~%~%~a" new-text) nodes nil scratch))))
                 (before (let ((at (leading-comment-start text (form-edit::node-start (anchor before "before")))))
                           (setf report (splice path text at at (format nil "~a~%~%" new-text) nodes nil scratch))))
                 (t (let ((at (length text)))
                      (setf report
                            (splice path text at at
                                    (format nil "~:[~%~;~]~%~a~%" (or (zerop at) (char= (char text (1- at)) #\Newline)) new-text)
                                    nodes nil scratch))))))))))
      (setf (entry-state entry) :saved
            (entry-file entry) path)
      (persist))
    (values path report)))

(defun save-all (&key file)
  "Save every unsaved definition, oldest first, each to its known file, or to FILE when none is known. Stops at the first
   refusal, saying which were saved. Returns the names saved."
  (let ((saved '()))
    (handler-bind ((error (lambda (e)
                            (when saved (format t "~&;; saved before the refusal: ~{~a~^, ~}~%" (reverse saved)))
                            (error e))))
      (loop for entry = (first (entries)) while entry
            do (save (entry-name entry) :file (and (null (entry-file entry)) file))
               (push (entry-name entry) saved)))
    (nreverse saved)))

;;; New files and the .asd

(defun new-file (file &key package header)
  "Make the source file FILE (relative to the project) with a header comment and an in-package form for PACKAGE (default:
   the project's package, named like the project). Refused when the file exists."
  (let ((path (resolve file))
        (package (or package (project))))
    (when (probe-file path) (error "bench: ~a exists already" path))
    (ensure-directories-exist path)
    (form-edit::new-file path
                         (or header (format nil ";;;; ~a~%" (enough-namestring path (project-directory))))
                         (format nil "(in-package :~(~a~))" (if (symbolp package) (symbol-name package) package)))
    path))

(defun system-file (&optional system)
  "The .asd of the project: that of the system SYSTEM when given, else the one .asd file in the project's directory."
  (if system
      (or (asdf:system-source-file system) (error "bench: no .asd for the system ~a" system))
      (let ((files (directory (merge-pathnames "*.asd" (project-directory)))))
        (unless (= (length files) 1)
          (error "bench: ~d .asd files in ~a; say :system" (length files) (project-directory)))
        (first files))))

(defun add-component (name &key after (module "src") system)
  "Add (:file NAME) to the :components of the module MODULE (default \"src\") of the system SYSTEM (default: the one .asd of
   the project, its first defsystem) in the project's .asd, after the file AFTER or at the end. Refused when the file is
   there already. The .asd is edited as forms (form-edit::edit-subform), so the rest of it stays byte for byte."
  (let* ((path (system-file system)))
    (form-edit::with-scratch-package (scratch)
      (let* ((text (form-edit::file-text path))
             (nodes (form-edit::read-nodes text scratch))
             (system-node (if system
                              (first (form-edit::forms-matching nodes (string-downcase (string system)) scratch))
                              (find-if (lambda (n) (let ((x (form-edit::node-expression n)))
                                                     (and (consp x) (string= (symbol-name (car x)) "DEFSYSTEM"))))
                                       nodes)))
             (system-name (and system-node (form-edit::form-name (form-edit::node-expression system-node)))))
        (unless system-node (error "bench: no defsystem ~a in ~a" system path))
        (let* ((module-node
                (find-if (lambda (n) (let ((x (form-edit::node-expression n)))
                                       (and (consp x) (keywordp (car x)) (string= (symbol-name (car x)) "MODULE")
                                            (equal (second x) module))))
                         (form-edit::sub-nodes system-node (form-edit::node-expression system-node))))
               (module-node (or module-node
                                (find-if (lambda (n) (let ((x (form-edit::node-expression n)))
                                                       (and (consp x) (keywordp (car x)) (string= (symbol-name (car x)) "MODULE")
                                                            (equal (second x) module))))
                                         (all-sub-nodes system-node))))
               (module-form (and module-node (form-edit::node-expression module-node)))
               (components (and module-form (getf (cddr module-form) :components))))
          (unless module-node (error "bench: no (:module ~s ...) in the system ~a of ~a" module system-name path))
          (when (find-if (lambda (c) (and (consp c) (equal (second c) name))) components)
            (error "bench: the module ~s already has (:file ~s)" module name))
          (let* ((new (list (intern "FILE" :keyword) name))
                 (new-components
                  (if after
                      (let ((at (position-if (lambda (c) (and (consp c) (equal (second c) after))) components)))
                        (unless at (error "bench: no (:file ~s) in the module ~s to insert after" after module))
                        (append (subseq components 0 (1+ at)) (list new) (subseq components (1+ at))))
                      (append components (list new))))
                 (new-module (let ((copy (copy-list module-form)))
                               (setf (getf (cddr copy) :components) new-components)
                               copy))
                 (old-text (subseq text (form-edit::node-start module-node) (form-edit::node-end module-node))))
            (form-edit::edit-subform path system-name old-text (let ((*package* scratch) (*print-case* :downcase))
                                                                  (prin1-to-string new-module)))
            path))))))

(defun all-sub-nodes (node)
  "Every node inside NODE, at any depth."
  (loop for child in (form-edit::node-children node)
        collect child
        append (all-sub-nodes child)))

;;; The project's tests, inside this image

(defun run-tests (&key (system (project)))
  "Run the project's own test suite inside this image, against what the image holds: (asdf:load-system \"SYSTEM/tests\")
   loads the test files and any source file that changed on disk since it was loaded (unchanged files are not reloaded, so
   unsaved definitions survive; the files that were compiled are listed), then the test package's RUN is called with
   uiop:quit made to return instead of ending the image. Returns the number of failures and the files compiled. bin/lisp
   test, a fresh SBCL from the files, stays the arbiter after a save."
  (let* ((name (string-downcase (string system)))
         (tests (format nil "~a/tests" name))
         (compiled '())
         (output (make-string-output-stream))
         (ledger (sb-thread:with-mutex (*ledger-lock*) (copy-list *ledger*)))
         ;; The tests are run as a plain call, not as a request: an error a test makes on purpose is unwound as the
         ;; test expects, not held. The ledger is put back afterwards, so nothing the tests define stays in it.
         (lisp-ai-debug:*reply-now* nil))
    (unwind-protect
         (progn
           (let ((*standard-output* (make-broadcast-stream *standard-output* output))
                 (*compile-verbose* t) (*compile-print* nil) (*load-verbose* nil))
             (asdf:load-system tests))
           (dolist (line (uiop:split-string (get-output-stream-string output) :separator '(#\Newline)))
             (let ((at (search "compiling file \"" line)))
               (when at (push (subseq line (+ at 16) (position #\" line :start (+ at 16))) compiled))))
           (let* ((package (or (find-package (string-upcase tests)) (error "bench: no package ~a after loading it" tests)))
                  (run (or (find-symbol "RUN" package) (error "bench: no RUN in ~a" tests)))
                  (failures-symbol (find-symbol "*FAILURES*" package))
                  (quit 'uiop/image:quit))
             (sb-int:encapsulate quit 'bench (lambda (function &optional (code 0) &rest more)
                                               (declare (ignore function more))
                                               (throw 'bench-quit code)))
             (unwind-protect (catch 'bench-quit (funcall run))
               (sb-int:unencapsulate quit 'bench))
             (let ((failures (if (and failures-symbol (boundp failures-symbol)) (symbol-value failures-symbol) :unknown))
                   (compiled (reverse compiled)))
               (when compiled (format t "~&;; bench: reloaded from disk: ~{~a~^, ~}~%" compiled))
               (values failures compiled))))
      (sb-thread:with-mutex (*ledger-lock*) (setf *ledger* ledger))
      (persist))))

;;; Wire the ledger into the eval loop at load.
(setf lisp-ai-debug:*eval-observer* 'record-form)
