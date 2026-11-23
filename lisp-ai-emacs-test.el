;;; lisp-ai-emacs-test.el --- shared checks of lisp-ai.el and lisp-ai-demo.el  -*- lexical-binding: t; -*-

;; Loaded by a project's tests/demo-test.el, which is run by hand in batch Emacs with the project's image up:
;;   /opt/homebrew/bin/Emacs --batch -Q -l tests/demo-test.el
;; The project's test loads its demo.el (which loads lisp-ai.el and lisp-ai-demo.el from the tools commit it pins), then
;; this file from `lisp-ai-directory', calls the checks below with what is particular to the project, adds its own, and
;; ends with `lisp-ai-test-finish'. Each check prints one line, PASS or FAIL; the exit status is 1 when any failed.

(require 'lisp-ai)

(defvar lisp-ai-test-failures 0 "How many checks have failed so far.")

(defun lisp-ai-test-report (label ok)
  "Print LABEL as PASS when OK is true, else as FAIL and count it. Returns OK."
  (unless ok (setq lisp-ai-test-failures (1+ lisp-ai-test-failures)))
  (princ (format "%s: %s\n" (if ok "PASS" "FAIL") label))
  ok)

(defun lisp-ai-test-wait-until (test &optional seconds)
  "Run TEST every tenth of a second, up to SECONDS (15 if nil); its last value."
  (let ((tries 0) (limit (* 10 (or seconds 15))))
    (while (and (< tries limit) (not (funcall test))) (accept-process-output nil 0.1) (setq tries (1+ tries))))
  (funcall test))

(defun lisp-ai-test-finish ()
  "Print the tally and leave Emacs with 0 when every check passed, 1 when any failed."
  (princ (if (zerop lisp-ai-test-failures) "all checks passed\n"
           (format "%d check(s) failed\n" lisp-ai-test-failures)))
  (kill-emacs (if (zerop lisp-ai-test-failures) 0 1)))

(defun lisp-ai-test-text (root file)
  "The contents of FILE, relative to ROOT."
  (with-temp-buffer (insert-file-contents (expand-file-name file root)) (buffer-string)))

(defun lisp-ai-test-lisp (root request)
  "The trimmed output of REQUEST sent to the image of ROOT through bin/lisp eval."
  (string-trim (cdr (lisp-ai-eval-now root request))))

;;; Layer 1 without the image

(defun lisp-ai-test-project (root)
  "Check finding the project, its port and its Sly, and refusing a wrong Sly. ROOT is the project under test."
  ;; software/ holds links into Qlot's cache, so the names are compared as given, not as true names
  (lisp-ai-test-report "lisp-ai.el is loaded from the commit the project's Qlot install pins"
                       (with-temp-buffer
                         (insert-file-contents (expand-file-name ".qlot/dists/lisp-ai-tools/distinfo.txt" root))
                         (and (re-search-forward "^version: git-\\([0-9a-f]+\\)$" nil t)
                              (string= (directory-file-name lisp-ai-directory)
                                       (expand-file-name (format ".qlot/dists/lisp-ai-tools/software/lisp-ai-tools-ref-%s"
                                                                 (match-string 1))
                                                         root)))))
  (lisp-ai-test-report "the project is found from a directory inside it"
                       (string= (file-truename (lisp-ai-project-root (expand-file-name "src/" root)))
                                (file-truename root)))
  (lisp-ai-test-report "a directory under no project is refused"
                       (condition-case nil (progn (lisp-ai-project-root "/") nil) (error t)))
  (lisp-ai-test-report "the port comes from rules/slynk.sexp"
                       (with-temp-buffer
                         (insert-file-contents (expand-file-name "rules/slynk.sexp" root))
                         (and (re-search-forward ":port \\([0-9]+\\)" nil t)
                              (eql (lisp-ai-port root) (string-to-number (match-string 1))))))
  (lisp-ai-test-report "the Sly in use is the one the project's Qlot install pins"
                       (string-prefix-p (file-truename (lisp-ai-sly-directory root))
                                        (file-truename (symbol-file 'sly-connect 'defun))))
  (let ((fake (make-temp-file "lisp-ai-test" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".qlot/dists/quicklisp/software/" fake) t)
          (lisp-ai-test-report "a project with no Sly is refused"
                               (condition-case nil (progn (lisp-ai-sly-directory fake) nil) (error t)))
          (make-directory (expand-file-name ".qlot/dists/quicklisp/software/sly-20260101-git" fake))
          (make-directory (expand-file-name ".qlot/dists/quicklisp/software/sly-20270101-git" fake))
          (lisp-ai-test-report "a project with two Slys is refused"
                               (condition-case nil (progn (lisp-ai-sly-directory fake) nil) (error t))))
      (delete-directory fake t)))
  (lisp-ai-test-report "loading the project's Sly again is allowed" (ignore-errors (lisp-ai-load-sly root)))
  (lisp-ai-test-report "a Sly loaded from elsewhere is refused"
                       (cl-letf (((symbol-function 'symbol-file) (lambda (&rest _) "/elsewhere/sly/sly.el")))
                         (condition-case nil (progn (lisp-ai-load-sly root) nil) (error t)))))

(defun lisp-ai-test-watch ()
  "Check the changed region and the watch of a file: its redraw, its flash, ON-REVERT and polling."
  (lisp-ai-test-report "no change, no region" (null (lisp-ai-changed-region "abc" "abc")))
  (lisp-ai-test-report "a middle change is found"
                       (equal (lisp-ai-changed-region "hello world" "hello brave world") '(7 . 13)))
  (lisp-ai-test-report "a deletion is one character" (equal (lisp-ai-changed-region "abcdef" "abdef") '(3 . 4)))
  (lisp-ai-test-report "an addition at the end" (equal (lisp-ai-changed-region "abc" "abcdef") '(4 . 7)))
  ;; the real hooks: visit a file, change it behind Emacs's back, revert, see what was flashed
  (let* ((file (make-temp-file "lisp-ai-watch" nil ".lisp" "(defun f ()\n  1)\n(defun g ()\n  2)\n"))
         (buffer (lisp-ai-watch "/" file :flash t))
         (flashed nil))
    (unwind-protect
        (progn
          (write-region "(defun f ()\n  1)\n(defun g ()\n  99)\n" nil file nil 'silent)
          (cl-letf (((symbol-function 'pulse-momentary-highlight-region)
                     (lambda (start end &rest _) (setq flashed (cons start end)))))
            (with-current-buffer buffer (revert-buffer t t t)))
          (with-current-buffer buffer
            (lisp-ai-test-report "the buffer shows the new text"
                                 (string= (buffer-string) "(defun f ()\n  1)\n(defun g ()\n  99)\n"))
            (lisp-ai-test-report "and the flash covers exactly what changed"
                                 (and flashed (string= (buffer-substring (car flashed) (cdr flashed)) "99"))))
          (lisp-ai-test-report "auto-revert-mode is on in the watched buffer" (buffer-local-value 'auto-revert-mode buffer))
          (lisp-ai-test-report "and it polls" (null (buffer-local-value 'auto-revert-use-notify buffer))))
      (kill-buffer buffer)
      (delete-file file)))
  (let* ((file (make-temp-file "lisp-ai-watch" nil ".txt" "one\n"))
         (ran nil)
         (buffer (lisp-ai-watch "/" file :on-revert (lambda () (setq ran (current-buffer))))))
    (unwind-protect
        (progn
          (write-region "two\n" nil file nil 'silent)
          (cl-letf (((symbol-function 'pulse-momentary-highlight-region) (lambda (&rest _) (setq ran 'flashed))))
            (with-current-buffer buffer (revert-buffer t t t)))
          (lisp-ai-test-report "ON-REVERT runs in the buffer after a redraw, and without FLASH nothing flashes"
                               (eq ran buffer)))
      (kill-buffer buffer)
      (delete-file file))))

;;; With the image

(defun lisp-ai-test-eval (root)
  "Check `lisp-ai-eval-now' and `lisp-ai-eval-async' against the image of ROOT, a held error among them."
  (lisp-ai-test-report "lisp-ai-eval-now gives the exit status and the reply"
                       (equal (lisp-ai-eval-now root "(+ 1 2)") '(0 . "3\n")))
  (let ((result :waiting))
    (lisp-ai-eval-async root '("(+ 1 2)" "(+ 3 4)") (lambda (failure) (setq result failure)))
    (lisp-ai-test-report "lisp-ai-eval-async sends every request and calls DONE with nil"
                         (progn (lisp-ai-test-wait-until (lambda () (not (eq result :waiting))) 30)
                                (null result))))
  (lisp-ai-eval-now root "(setf (get :lisp-ai-test :sent) nil)")
  (let ((result :waiting))
    (lisp-ai-eval-async root '("(car 'fish)" "(setf (get :lisp-ai-test :sent) t)")
                        (lambda (failure) (setq result failure)))
    (lisp-ai-test-wait-until (lambda () (not (eq result :waiting))) 30)
    (lisp-ai-test-report "a held error is handed to DONE" (and (stringp result) (string-match-p "^HELD [0-9]+" result)))
    (lisp-ai-test-report "and abandoned, so the image holds no session"
                         (string= (lisp-ai-test-lisp root "(hash-table-count lisp-ai-debug::*sessions*)") "0"))
    (lisp-ai-test-report "and the requests after it are not sent"
                         (string= (lisp-ai-test-lisp root "(get :lisp-ai-test :sent)") "NIL"))))

(defun lisp-ai-test-repl (package)
  "Check the connected REPL: one connection, no debugger claimed, the prompt PACKAGE>, RET under paredit, the image's
package PACKAGE, and the window following the end of the REPL."
  (lisp-ai-test-report "one connection" (= 1 (length sly-net-processes)))
  (with-current-buffer "*sly-mrepl for sbcl*"
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (princ (format "REPL buffer:\n%s\n" text))
      (lisp-ai-test-report "the REPL does not claim a debugger" (not (string-match-p "Debugger entered" text)))
      (lisp-ai-test-report (format "and its prompt is %s>" package)
                           (string-match-p (concat (regexp-quote package) "> *\\'") text))))
  ;; typing at the prompt and pressing the RET key, with paredit-mode on: paredit-RET used to swallow it
  (with-current-buffer "*sly-mrepl for sbcl*"
    (lisp-ai-test-report "paredit-mode is on in the REPL, as in init.el" (bound-and-true-p paredit-mode))
    (lisp-ai-test-report "and RET still belongs to Sly" (eq (key-binding (kbd "RET")) 'sly-mrepl-return))
    (goto-char (point-max))
    (insert "(+ 40 2)")
    (call-interactively (key-binding (kbd "RET")))
    (lisp-ai-test-report "an expression typed at the prompt is evaluated"
                         (lisp-ai-test-wait-until
                          (lambda () (string-match-p "^42 " (buffer-substring-no-properties (point-min) (point-max)))) 8)))
  (lisp-ai-test-report (format "the image answers: its package is %s" package)
                       (equal (sly-eval '(cl:package-name cl:*package*)) package))
  ;; the window follows the end of the REPL: the echo is inserted before the prompt
  (with-current-buffer "*sly-mrepl for sbcl*"
    (let ((window lisp-ai--repl-window))
      (dotimes (i 100) (sly-mrepl--insert-output (format "claude> (line %d)\n" i)))
      (redisplay t)
      (let ((last (line-number-at-pos (point-max)))
            (start (line-number-at-pos (window-start window)))
            (height (window-body-height window)))
        (lisp-ai-test-report "after a hundred lines of output the REPL window's start has moved down" (> start 1))
        (lisp-ai-test-report "so that the last line is within the window"
                             (and (<= (- last start) height) (<= start last)))))))

(defun lisp-ai-test-reconnect (root package)
  "Restart the image of ROOT and check that `lisp-ai-reconnect' connects again. Only in a clone: it restarts the image."
  (let ((default-directory root))
    (lisp-ai-test-report "the image stops" (zerop (call-process "sh" nil nil nil "bin/lisp" "stop")))
    (lisp-ai-test-report "and Sly loses its connection" (lisp-ai-test-wait-until (lambda () (not (sly-connected-p))) 10))
    (lisp-ai-test-report "the image starts again" (zerop (call-process "sh" nil nil nil "bin/lisp" "start")))
    (lisp-ai-reconnect)
    ;; not sly-connected-p, which is true as soon as the new process exists: until the new connection is set up, Sly's
    ;; current connection is still the closed one, and a request sent then fails with "Current connection ... is closed"
    (lisp-ai-test-report "lisp-ai-reconnect connects Sly to it"
                         (lisp-ai-test-wait-until (lambda () (let ((connection (ignore-errors (sly-connection))))
                                                               (and connection (eq (process-status connection) 'open))))))
    (lisp-ai-test-report "with one connection" (= 1 (length sly-net-processes)))
    (lisp-ai-test-report (format "and the new image answers: its package is %s" package)
                         (equal (sly-eval '(cl:package-name cl:*package*)) package))
    (lisp-ai-test-report "the REPL is back in the window it was shown in"
                         (lisp-ai-test-wait-until
                          (lambda () (and (window-live-p lisp-ai--repl-window)
                                          (string-prefix-p "*sly-mrepl" (buffer-name (window-buffer lisp-ai--repl-window)))))
                          10))))

(defun lisp-ai-test-in-clone-p (root real)
  "True when ROOT is not the real tree REAL: the checks that rewrite files or restart the image run only then."
  (not (string= (file-truename root) (file-truename real))))

(provide 'lisp-ai-emacs-test)
;;; lisp-ai-emacs-test.el ends here
