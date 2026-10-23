;;;; Tests of profile.lisp and profile-hook, shared by every project under ~/lisp-ai as part of the system
;;;; lisp-ai-tools/tests: a project calls (lisp-ai-profile-tests:run DIRECTORY), which makes a scratch project inside DIRECTORY
;;;; (the project's run/), removes it afterwards and returns how many checks failed, printing a line for each.

(defpackage :lisp-ai-profile-tests
  (:use :cl)
  (:export #:run))

(in-package :lisp-ai-profile-tests)

(defvar *failures* 0)

(defvar *here*
  (directory-namestring (asdf/system:system-relative-pathname "lisp-ai-tools" "profile-hook"))
  "The directory of the tools, where the script profile-hook is; the one test that runs a script needs it.")

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

(defun write-file (path text)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
    (write-string text out)))

(defun log-lines (&rest lines)
  (format nil "~{~a~%~}" lines))

(defun guard-line (time id command verdict shadowed)
  (format nil "~d~cguard~c{\"id\":~s,\"command\":~s,\"verdict\":~s,\"programs\":[],\"shadowed\":~a}"
          time #\Tab #\Tab id command verdict (if shadowed "true" "false")))

(defun post-line (time kind id)
  (format nil "~d~c~a~c{\"hook_event_name\":\"Post\",\"tool_use_id\":~s}" time #\Tab kind #\Tab id))

(defun pre-line (time id command)
  (format nil "~d~cpre~c{\"tool_use_id\":~s,\"tool_name\":\"Bash\",\"tool_input\":{\"command\":~s}}"
          time #\Tab #\Tab id command))

(defun entry-for (entries id)
  (find id entries :key (lambda (entry) (getf entry :id)) :test #'string=))

(defun run-switches (project)
  (check-equal "nothing is on at first" (lisp-ai-profile:status project)
               '(:profiling nil :shadow nil :limit t :shadow-active nil))
  (check "turning profiling on says so" (lisp-ai-profile:profiling project :on))
  (lisp-ai-profile:shadowing project :on)
  (check "shadow and profiling together are active" (lisp-ai-profile:shadow-active-p project))
  (lisp-ai-profile:profiling project :off)
  (check "shadow without profiling is not active" (not (lisp-ai-profile:shadow-active-p project)))
  (check "but the shadow switch stays set" (lisp-ai-profile:shadowing project))
  (check "the limit is on by default" (lisp-ai-profile:limit project))
  (check "lifting the limit says so" (not (lisp-ai-profile:limit project :off)))
  (check "and it stays lifted" (not (lisp-ai-profile:limit project)))
  (check "restoring the limit says so" (lisp-ai-profile:limit project :on))
  (lisp-ai-profile:shadowing project :off)
  (check-equal "all off again" (lisp-ai-profile:status project) '(:profiling nil :shadow nil :limit t :shadow-active nil)))

(defun run-corpus-and-hits (project)
  (check-equal "the corpus is read" (lisp-ai-profile:corpus project) '("cat /dev/null" "sed -n 1p /dev/null"))
  (check "a corpus command matches exactly" (lisp-ai-profile:corpus-command-p project "cat /dev/null"))
  (check "a near miss does not" (not (lisp-ai-profile:corpus-command-p project "cat /dev/null ")))
  (check "a command that is not in it does not" (not (lisp-ai-profile:corpus-command-p project "cat /etc/hosts")))
  (check-equal "the forbidden programs are read" (lisp-ai-profile:forbidden-programs project) '("cat" "sed" "grep"))
  (let ((forbidden '("cat" "sed" "grep")))
    (check-equal "a word counts anywhere" (lisp-ai-profile:loose-hits "echo sed" forbidden) '("sed"))
    (check-equal "a full path counts" (lisp-ai-profile:loose-hits "/usr/bin/sed -n p f | grep x" forbidden) '("sed" "grep"))
    (check-equal "a longer word does not" (lisp-ai-profile:loose-hits "echo seded concatenate" forbidden) nil)
    (check-equal "a repeat is listed once" (lisp-ai-profile:loose-hits "cat a; cat b" forbidden) '("cat"))
    (check-equal "no program, no hit" (lisp-ai-profile:loose-hits "git status" forbidden) nil)))

(defun run-entries (project)
  (let ((log (merge-pathnames "run/profile/events.log" (lisp-ai-profile:project-directory project))))
    (write-file log
                (log-lines (guard-line 1000 "g1" "cat /dev/null" "deny" nil)
                           (guard-line 1001 "g2" "sed -n 1p /dev/null" "deny" t)
                           (post-line 1002 "post" "g2")
                           (guard-line 1003 "g3" "cat /dev/null" "deny" t)
                           (guard-line 1004 "g4" "echo hi" "allow" nil)
                           (post-line 1005 "post" "g4")
                           (guard-line 1006 "g5" "grep x f" "allow" nil)
                           (post-line 1007 "failure" "g5")
                           (pre-line 1008 "p1" "cat f")
                           (guard-line 1990 "g6" "ls" "allow" nil)
                           "garbage"))
    (multiple-value-bind (events bad) (lisp-ai-profile:read-events project)
      (check-equal "eleven lines, ten of them readable" (list (length events) bad) '(10 1)))
    (let ((entries (lisp-ai-profile:entries project :now 2000)))
      (flet ((cell (id) (let ((entry (entry-for entries id))) (list (getf entry :guard) (getf entry :deny)))))
        (check-equal "six guard records and a pre record make seven calls" (length entries) 7)
        (check-equal "stopped at the guard: the deny list never judged it" (cell "g1") '(:denied :unknown))
        (check-equal "shadowed and ran: the deny list missed it" (cell "g2") '(:would-deny :missed))
        (check-equal "shadowed and did not run: the deny list caught it" (cell "g3") '(:would-deny :caught))
        (check-equal "allowed and ran" (cell "g4") '(:allowed :missed))
        (check-equal "a failure counts as having run" (cell "g5") '(:allowed :missed))
        (check-equal "no guard, did not run: the deny list caught it" (cell "p1") '(:none :caught))
        (check-equal "too recent to say" (cell "g6") '(:allowed :pending))
        (check-equal "the loose hits ride along" (getf (entry-for entries "g5") :hits) '("grep"))))
    (let ((summary (lisp-ai-profile:summary project :now 2000)))
      (check-equal "the unreadable line is counted" (getf summary :unreadable-lines) 1)
      (check-equal "guard-only catches" (getf summary :guard-only-catches) '(("sed -n 1p /dev/null" . 1)))
      (check-equal "both caught" (getf summary :both-caught) '(("cat /dev/null" . 1)))
      (check-equal "deny-only catches" (getf summary :deny-only-catches) '(("cat f" . 1)))
      (check-equal "candidates both missed" (getf summary :candidates-both-missed) '(("grep x f" . 1)))
      (check-equal "every corpus command has been seen" (getf summary :corpus-not-yet-run) nil))
    (let ((path (lisp-ai-profile:write-summary project :date "2026-01-02")))
      (check "the summary is written under data/, named by its date"
             (search "data/profile-2026-01-02.sexp" (namestring path)))
      (check-equal "and reads back as the summary"
                   (let ((*read-eval* nil)) (with-open-file (in path) (read in)))
                   (lisp-ai-profile:summary project)))))

(defun run-hook (project)
  (let* ((dir (namestring (lisp-ai-profile:project-directory project)))
         (script (concatenate 'string *here* "profile-hook"))
         (log (merge-pathnames "run/profile/events.log" (lisp-ai-profile:project-directory project)))
         (payload "{\"hook_event_name\":\"PostToolUse\",\"tool_use_id\":\"h1\",\"tool_name\":\"Bash\"}"))
    (when (probe-file log) (delete-file log))
    (lisp-ai-profile:profiling project :off)
    (multiple-value-bind (out err code)
        (with-input-from-string (in payload)
          (uiop:run-program (list "sh" script dir "post") :input in :output :string :error-output :string
                                                          :ignore-error-status t))
      (check-equal "with profiling off the hook prints nothing and exits 0" (list out err code) (list "" "" 0)))
    (check "and logs nothing" (not (probe-file log)))
    (lisp-ai-profile:profiling project :on)
    (multiple-value-bind (out err code)
        (with-input-from-string (in payload)
          (uiop:run-program (list "sh" script dir "post") :input in :output :string :error-output :string
                                                          :ignore-error-status t))
      (check-equal "with profiling on it prints nothing and exits 0" (list out err code) (list "" "" 0)))
    (multiple-value-bind (events bad) (lisp-ai-profile:read-events project)
      (check-equal "and logs one readable event" (list (length events) bad) '(1 0))
      (check-equal "of kind post, with the payload intact"
                   (list (second (first events)) (gethash "tool_use_id" (third (first events)))) '("post" "h1")))
    (lisp-ai-profile:record-guard project :id "h1" :command "ls" :verdict :allow :programs '() :shadowed nil)
    (check-equal "the guard's record and the hook's join into a call that ran"
                 (let ((entry (entry-for (lisp-ai-profile:entries project) "h1"))) (list (getf entry :guard) (getf entry :ran)))
                 '(:allowed t))
    (lisp-ai-profile:profiling project :off)))

(defun run (directory)
  "Run the checks with a scratch project inside DIRECTORY; returns the number of failures."
  (setf *failures* 0)
  (let* ((root (merge-pathnames "profile-test-project/" (uiop:ensure-directory-pathname directory))) (project root))
    (when (probe-file root) (uiop:delete-directory-tree root :validate t))
    (write-file (merge-pathnames "rules/forbidden-programs.sexp" root) (format nil "(\"cat\" \"sed\" \"grep\")~%"))
    (write-file (merge-pathnames "rules/profile-corpus.sexp" root)
                (format nil "(\"cat /dev/null\" \"sed -n 1p /dev/null\")~%"))
    (unwind-protect
         (progn (run-switches project) (run-corpus-and-hits project) (run-entries project) (run-hook project))
      (when (probe-file root) (uiop:delete-directory-tree root :validate t))))
  *failures*)
