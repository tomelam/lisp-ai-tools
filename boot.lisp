;;;; Loaded by a project's bin/serve.lisp when the image starts: (load "../tools/boot.lisp") from the project, then
;;;; (tools-boot:load-tools :project). Puts the tools every project shares into the image for as long as it lives, so using
;;;; them costs no start-up: form-edit, and the package SCRATCH for trying code before it goes into a file.
(defpackage :tools-boot
  (:use :cl)
  (:export #:load-tools))

(in-package :tools-boot)

(defvar *here* (directory-namestring *load-truename*) "The directory of this file, where the tools are.")

(defun load-tools (project)
  "Load form-edit and make the package SCRATCH, using PROJECT's package. PROJECT is the keyword that names the project's
   ASDF system and package (:code-as-data). Safe to call again."
  (asdf/operate:load-system (format nil "~(~a~)/form-edit" project))
  (load (merge-pathnames "form-edit.lisp" *here*))
  (or (find-package :scratch) (make-package :scratch :use (list :cl project))))
