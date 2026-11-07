;;;; What the shared tools need to know about a project: where its directory is, and how to read the data files in its
;;;; rules/ and data/. A project is a pathname, a string with a slash, or the name of its ASDF system (:code-as-data).
(defpackage :lisp-ai-project
  (:use :cl)
  (:export #:system-directory #:project-directory #:read-data #:read-rules #:load-for-image #:*load-failures*))
(in-package :lisp-ai-project)

(defun system-directory (name)
  (or (asdf:system-source-directory name) (error "project: no ASDF system ~a" name)))

(defun project-directory (project)
  "PROJECT as a directory pathname: a pathname, a string holding a slash, or the name of an ASDF system."
  (etypecase project
    (pathname (uiop:ensure-directory-pathname project))
    (string (if (find #\/ project) (uiop:ensure-directory-pathname project) (system-directory project)))
    (symbol (system-directory (string-downcase (symbol-name project))))))

(defun read-data (project relative)
  "The one form in the file RELATIVE of PROJECT, or NIL when there is no such file."
  (let ((file (merge-pathnames relative (project-directory project))) (*read-eval* nil))
    (when (probe-file file)
      (with-open-file (in file :external-format :utf-8) (read in nil nil)))))

(defun read-rules (project name)
  "The one form in rules/NAME.sexp of PROJECT. A missing file is an error: a rule that is not there must not read as an
   empty rule."
  (let ((file (merge-pathnames (format nil "rules/~a.sexp" name) (project-directory project))) (*read-eval* nil))
    (with-open-file (in file :external-format :utf-8) (read in))))

;;; The always-on image loads its project with LOAD-FOR-IMAGE, not with ASDF:LOAD-SYSTEM: a file that fails must not keep
;;; the image down, since with no image no file can be changed (the gate refuses Lisp edited as text). In the hackathon's
;;; rerun chain 2 (2026-10-30) a saved (defvar *mr-before* (meter-report ...)), listed before the file that defines
;;; METER-REPORT, would have stopped every later start. bin/lisp test still loads strictly, so such a file is reported.
(defvar *load-failures*
  'nil
  "What LOAD-FOR-IMAGE could not load when this image started, as a list of (file phase what): FILE relative to the
   project when it is the project's, PHASE :compile (nothing of the file is in the image) or :load (the forms before the
   failing one are), WHAT the condition's text. Empty when every file loaded.")

(defun component-of-file (system file)
  "The source file component whose source or compiled file is FILE, among the components of the ASDF system SYSTEM and
   of every other system defined in the same .asd (a project's server system loads its main system as a dependency), or
   NIL."
  (let ((wanted (or (ignore-errors (namestring (truename file))) (ignore-errors (namestring file))))
        (asd (asdf/system:system-source-file (asdf/system:find-system system))))
    (labels ((walk (c)
               (if (typep c 'asdf/component:parent-component)
                   (some #'walk (asdf/component:component-children c))
                   (and (typep c 'asdf/lisp-action:cl-source-file)
                        (member wanted
                                (cons (namestring (asdf/component:component-pathname c))
                                      (mapcar #'namestring (asdf/action:output-files 'asdf/lisp-action:compile-op c)))
                                :test #'string=)
                        c))))
      (and wanted
           (some #'walk
                 (cons (asdf/system:find-system system)
                       (loop for name in (asdf/system-registry:registered-systems)
                             for other = (asdf/system:find-system name nil)
                             when (and other asd (equal (asdf/system:system-source-file other) asd)) collect
                               other)))))))

(defun load-for-image (system)
  "Load the ASDF system SYSTEM as the always-on image must, so that it comes up even when a file of it does not load. A
   file whose compiling warns is loaded as compiled, its warnings printed. A file that signals an error while it is
   compiled is not loaded: its stale compiled file is deleted, so the image cannot run what the file held before (ASDF
   signals that failure after COMPILE-FILE has returned, naming the component in the condition). A file
   that signals an error while it is loaded is left at the form that failed, and the rest of the system is loaded; then
   it is loaded once more, whole, since a form in it may need a file loaded after it, and a line says when that worked.
   Each file that still did not load is printed on a line that begins LOAD FAILED and kept in *LOAD-FAILURES*. Returns
   *LOAD-FAILURES*."
  (let ((first-failures 'nil) (directory (asdf/system:system-source-directory system)))
    (flet ((file-name (file)
             (let ((name (namestring file)))
               (if (and directory (uiop/pathname:subpathp file directory))
                   (enough-namestring file directory)
                   name))))
      (handler-bind ((error
                      (lambda (condition)
                        (let ((accept (find-restart 'asdf/action:accept condition)))
                          (when accept
                            (let* ((context
                                    (and (typep condition 'uiop/lisp-build:compile-condition)
                                         (labels ((component-in (x)
                                                    (cond ((typep x 'asdf/component:component) x)
                                                          ((consp x)
                                                           (or (component-in (car x)) (component-in (cdr x)))))))
                                           (component-in
                                            (uiop/lisp-build::compile-condition-context-arguments condition)))))
                                   (compiling (or context *compile-file-truename*))
                                   (file
                                    (and (not context)
                                         (or *compile-file-truename*
                                             (and (typep condition 'file-error) (file-error-pathname condition))
                                             *load-truename*)))
                                   (component (or context (and file (component-of-file system file)))))
                              (when (and compiling component)
                                (dolist (output (asdf/action:output-files 'asdf/lisp-action:compile-op component))
                                  (when (probe-file output) (delete-file output))))
                              (push
                               (list component
                                     (if component
                                         (asdf/component:component-pathname component)
                                         file)
                                     (if compiling
                                         :compile
                                         :load)
                                     (princ-to-string condition))
                               first-failures)
                              (invoke-restart accept)))))))
        (let ((uiop/lisp-build:*compile-file-failure-behaviour* :warn))
          (asdf/operate:load-system system)))
      (setf *load-failures* 'nil)
      (loop for (component file phase what) in (reverse first-failures)
            unless (find file *load-failures* :key #'first :test #'equal) do
              (let ((fasl
                     (and component (eq phase :load)
                          (first (asdf/action:output-files 'asdf/lisp-action:compile-op component)))))
                (if (and fasl (probe-file fasl)
                         (handler-case (progn (load fasl) t) (error (e) (setf what (princ-to-string e)) nil)))
                    (format t
                            "~&;; loaded on the second try, after the files that follow it: ~a (first try: ~a). A form in it needs a later file: move it, or the file~%"
                            (file-name file) what)
                    (push (list file phase what) *load-failures*))))
      (setf *load-failures*
              (loop for (file phase what) in (reverse *load-failures*)
                    collect (list (file-name file) phase what)))
      (loop for (file phase what) in *load-failures*
            do
              (format t
                      "~&;; LOAD FAILED: ~a: ~a. ~:[What follows the failing form in it is not in this image~;It did not compile (the compiler's messages above say why), so nothing of it is in this image~].~%"
                      file what (eq phase :compile)))
      *load-failures*)))
