;;;; What the shared tools need to know about a project: where its directory is, and how to read the data files in its
;;;; rules/ and data/. A project is a pathname, a string with a slash, or the name of its ASDF system (:code-as-data).
(defpackage :lisp-ai-project
  (:use :cl)
  (:export #:system-directory #:project-directory #:read-data #:read-rules))
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
