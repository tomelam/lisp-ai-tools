;;;; SBCL user init file for the bin/ scripts of every project under ~/lisp-ai (--userinit).
;;;;
;;;; Qlot loads its own, newer UIOP over the one SBCL ships, and Slynk redefines a method, so every start of an image used
;;;; to print about 480 lines of "WARNING: redefining UIOP/...". Nobody acts on them. This mutes a redefinition warning
;;;; when the function redefined belongs to a library (UIOP, ASDF, Slynk, Eclector) and no other: a function of the
;;;; project defined twice, or a project function redefined by mistake, is still reported. Nothing else is muted.
;;;;
;;;; ~/.sbclrc does not exist on this machine, so this file takes the place of nothing.

(defpackage :quiet-init
  (:use :cl))

(in-package :quiet-init)

(defparameter *library-package-prefixes* '("UIOP" "ASDF" "SLYNK" "ECLECTOR")
  "A package is a library's when its name is one of these or begins with one followed by a slash, a hyphen or a dot.")

(defun library-package-p (package)
  (and package
       (let ((name (package-name package)))
         (some (lambda (prefix)
                 (and (>= (length name) (length prefix))
                      (string= prefix name :end2 (length prefix))
                      (or (= (length name) (length prefix)) (char= (char name (length prefix)) #\/)
                          (member (char name (length prefix)) '(#\- #\.)))))
               *library-package-prefixes*))))

(defun symbol-library-p (symbol)
  (and (symbolp symbol) (library-package-p (symbol-package symbol))))

(defun library-redefinition-p (condition)
  "True when CONDITION is a redefinition warning about a function, method or class of a library: the name is a library's,
   or, for a method, so is the class one of its arguments is specialized on. Never signals."
  (ignore-errors
   (let ((name (sb-kernel::redefinition-warning-name condition)))
     (or
      (cond ((symbolp name) (symbol-library-p name))
            ((and (consp name) (symbolp (second name))) (symbol-library-p (second name))))
      (and (typep condition 'sb-kernel:redefinition-with-defmethod)
           (some (lambda (specializer) (and (typep specializer 'class) (symbol-library-p (class-name specializer))))
                 (sb-kernel::redefinition-with-defmethod-specializers condition)))))))

(setf sb-ext:*muffled-warnings*
        '(or sb-kernel:uninteresting-redefinition
             (and sb-kernel:redefinition-warning (satisfies library-redefinition-p))))

;;;; SBCL opens /dev/tty for *terminal-io* when it can, so *debug-io* and *query-io* reach the window the process was
;;;; started from, past every redirection of stdout and stderr, held in a line buffer until a newline or the exit. Slynk's
;;;; system definition writes "SLYNK's ASDF loader finished." there with no newline: it sat on the hackathon's terminal
;;;; at each image stop and each bin/lisp test (2026-10-26). Here the terminal is stdin and stderr, so such a line goes
;;;; where the warnings go: the server's log, test.stderr.
(setf *terminal-io* (make-two-way-stream sb-sys:*stdin* sb-sys:*stderr*))
