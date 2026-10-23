;;;; Tests of guard.lisp, hook.lisp and tools/guard, shared by every project under ~/lisp-ai as part of the system
;;;; lisp-ai-tools/tests: a project calls (lisp-ai-guard-tests:run DIRECTORY PROJECT), which makes a scratch project inside
;;;; DIRECTORY (the project's run/) for the checks that need one, removes it afterwards, checks the real PROJECT (its bin/guard
;;;; and its .claude/settings.json), and returns how many checks failed, printing a line for each.

(defpackage :lisp-ai-guard-tests
  (:use :cl)
  (:export #:run))

(in-package :lisp-ai-guard-tests)

(defvar *failures* 0)

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

(defparameter *forbidden* '("cat" "head" "sed" "grep" "awk" "wc" "python3"))

(defun uses (command) (lisp-ai-guard:forbidden-uses command *forbidden*))

(defun hook-json (tool command)
  (format nil "{\"tool_name\":~s,\"tool_input\":{\"command\":~s}}" tool command))

(defun run-guard ()
  ;; each row: a command, and the forbidden programs it runs
  (dolist (row '(("cat file" ("cat"))
                 ("git diff | head -5" ("head"))
                 ("ls && sed -n 1p f" ("sed"))
                 ("FOO=1 grep x f" ("grep"))
                 ("sudo awk 1 f" ("awk"))
                 ("/usr/bin/sed p f" ("sed"))
                 ("echo $(cat f)" ("cat"))
                 ("cd x && ls | wc -l" ("wc"))
                 ("python3 -c 'print(1)'" ("python3"))
                 ("xargs -I {} sed s/a/b/ {}" ("sed"))
                 ("timeout 60 grep x f" ("grep"))
                 ;; none of these run a forbidden program
                 ("echo sed" nil)
                 ("echo \"a | sed b\"" nil)
                 ("echo a\\|sed" nil)
                 ("sbcl --eval '(print \"grep\")'" nil)
                 ("timeout 60 bin/test" nil)
                 ("git status" nil)
                 ("sbcl <<'E'
(head 1)
head
E
git status" nil)))
    (destructuring-bind (command expected) row
      (check-equal command (uses command) expected)))
  ;; the hook verdict: NIL lets the call through, a string refuses it and names the program
  (let ((refusal (lisp-ai-guard:hook-verdict (hook-json "Bash" "cat f") *forbidden*)))
    (check "a forbidden command is refused with a message" (stringp refusal))
    (check "and the message names the program" (search "cat" refusal)))
  (check "an allowed command goes through" (null (lisp-ai-guard:hook-verdict (hook-json "Bash" "git status") *forbidden*)))
  (check "a call of another tool goes through" (null (lisp-ai-guard:hook-verdict (hook-json "Read" "cat f") *forbidden*)))
  ;; a hook payload that cannot be read is refused loudly, never waved through
  (check "a payload that is not JSON is refused" (stringp (lisp-ai-guard:hook-verdict "not json" *forbidden*))))

(defun run-guard-wrappers ()
  "Wrappers with options that take a value, shells given a script, eval and find -exec: the program inside is still
   the program that runs. Each row is a command and the forbidden programs it runs; rows ending in nil run none."
  (dolist
      (row
       '(("sudo -u tom sed p f" ("sed")) ("nice -n 5 grep x f" ("grep")) ("nice -5 grep x f" ("grep"))
         ("env FOO=1 sed p f" ("sed")) ("env -i FOO=1 sed p f" ("sed")) ("env -u X sed p f" ("sed"))
         ("time -p grep x f" ("grep")) ("timeout -k 5 60 grep x f" ("grep")) ("sh -c 'sed p f'" ("sed"))
         ("bash -lc 'cat f'" ("cat")) ("bash -c 'echo a | head -1'" ("head")) ("sh -c 'sh -c \"grep x f\"'" ("grep"))
         ("eval 'sed p f'" ("sed")) ("find . -name x -exec sed p {} +" ("sed"))
         ("find . -exec sh -c 'grep x {}' \\;" ("grep")) ("sh -c 'echo sed'" nil) ("bash script.sh" nil)
         ("find . -name x" nil) ("find . -exec ls {} \\;" nil) ("sudo -u tom ls" nil) ("nice -n 5 ls" nil)
         ("env FOO=1 ls" nil) ("timeout -k 5 60 bin/test" nil)))
    (destructuring-bind (command expected) row (check-equal command (uses command) expected))))

(defun run-guard-structure ()
  "Compound-command keywords (if, then, do, while, {, !) and substitutions inside double quotes: the program inside still
   runs. Each row is a command and the forbidden programs it runs; rows ending in nil run none."
  (dolist
      (row
       '(("for f in a b; do cat $f; done" ("cat")) ("if true; then cat f; fi" ("cat"))
         ("while true; do sed p f; done" ("sed")) ("{ cat f; }" ("cat")) ("! grep x f" ("grep"))
         ("if grep x f; then echo y; fi" ("grep")) ("echo \"$(cat f)\"" ("cat")) ("echo \"a `cat f` b\"" ("cat"))
         ("echo \"$(cat f | head -1)\"" ("cat" "head")) ("echo '$(cat f)'" nil) ("echo \"$(echo hi)\"" nil)
         ("echo \"cat\"" nil) ("while read l; do echo $l; done" nil) ("for f in a b; do echo $f; done" nil)
         ("echo \"$(git status)\"" nil)))
    (destructuring-bind (command expected) row (check-equal command (uses command) expected)))
  (check "a substitution nested in a substitution inside double quotes is found"
         (member "sed" (uses "echo \"x $(echo \"$(sed p f)\")\"") :test #'string=)))

(defun deny-list (settings)
  "The permissions.deny list of SETTINGS (a parsed settings object), as a list."
  (coerce (gethash "deny" (gethash "permissions" settings)) 'list))

(defun run-permissions ()
  "The permission rules come from the forbidden programs, each denied by its name and by its full path. They replace the old
   deny list and leave the rest of the settings alone."
  (check-equal "each program is denied by name, then by full path" (lisp-ai-guard:permission-rules '("sed" "grep"))
               '("Bash(sed *)" "Bash(/bin/sed *)" "Bash(/usr/bin/sed *)" "Bash(/usr/local/bin/sed *)"
                 "Bash(/opt/homebrew/bin/sed *)" "Bash(grep *)" "Bash(/bin/grep *)" "Bash(/usr/bin/grep *)"
                 "Bash(/usr/local/bin/grep *)" "Bash(/opt/homebrew/bin/grep *)"))
  (let ((after
         (com.inuoe.jzon:parse
          (lisp-ai-guard:settings-with-deny
           "{\"hooks\":{\"x\":1},\"permissions\":{\"allow\":[\"Read\"],\"deny\":[\"stale\"]}}" '("sed")))))
    (check-equal "the deny list is replaced, not added to" (deny-list after) (lisp-ai-guard:permission-rules '("sed")))
    (check-equal "allow is kept" (coerce (gethash "allow" (gethash "permissions" after)) 'list) '("Read"))
    (check-equal "hooks are kept" (gethash "x" (gethash "hooks" after)) 1))
  (check-equal "settings with no permissions get a deny list"
               (deny-list (com.inuoe.jzon:parse (lisp-ai-guard:settings-with-deny "{}" '("sed"))))
               (lisp-ai-guard:permission-rules '("sed"))))

(defun hook-json-with-id (tool command id)
  (format nil "{\"tool_name\":~s,\"tool_use_id\":~s,\"tool_input\":{\"command\":~s}}" tool id command))

(defun run-hook (root)
  "The hook (tools/hook.lisp), on the scratch project ROOT: while profiling is on it logs what the guard decided; while the
   shadow is active it lets a command of rules/profile-corpus.sexp through (every command, with the limit off) and logs it
   as shadowed; a failure to log never changes its verdict."
  (flet ((respond (command id)
           (lisp-ai-hook:hook-response (hook-json-with-id "Bash" command id) *forbidden* root))
         (denied-p (reply)
           (and (search "\"permissionDecision\":\"deny\"" reply) t))
         (guards ()
           (mapcar (lambda (entry) (getf entry :guard)) (lisp-ai-profile:entries root))))
    (check "profiling off: a forbidden command is denied" (denied-p (respond "cat /dev/null" "t1")))
    (check "and nothing is logged" (not (probe-file (merge-pathnames "run/profile/events.log" root))))
    (lisp-ai-profile:profiling root :on)
    (check "profiling on: still denied" (denied-p (respond "cat /dev/null" "t2")))
    (check-equal "an allowed command is allowed" (respond "echo hi" "t3") "")
    (lisp-ai-profile:shadowing root :on)
    (check-equal "shadow on, limit on: a corpus command is let through" (respond "cat /dev/null" "t4") "")
    (check "but another forbidden command is still denied" (denied-p (respond "cat /etc/hosts" "t5")))
    (lisp-ai-profile:limit root :off)
    (check-equal "limit off: any command is let through" (respond "cat /etc/hosts" "t6") "")
    (lisp-ai-profile:limit root :on)
    (lisp-ai-profile:shadowing root :off)
    (check "shadow off: the corpus command is denied again" (denied-p (respond "cat /dev/null" "t7")))
    (check-equal "what the guard logged, in order" (guards) '(:denied :allowed :would-deny :denied :would-deny :denied))
    (check-equal "a call of another tool is let through"
                 (lisp-ai-hook:hook-response "{\"tool_name\":\"Read\",\"tool_input\":{}}" *forbidden* root) "")
    (check-equal "and is not logged" (length (guards)) 6)
    (check-equal "respond reads the forbidden programs from the project's rules"
                 (denied-p (lisp-ai-hook:respond root (hook-json-with-id "Bash" "head /dev/null" "t9"))) t)
    (let ((log (merge-pathnames "run/profile/events.log" root)))
      (delete-file log)
      (ensure-directories-exist (merge-pathnames "x" (merge-pathnames "run/profile/events.log/" root)))
      (check "logging fails when events.log cannot be written, yet the verdict stands"
             (denied-p (respond "cat /dev/null" "t8")))
      (check "and the failure is counted" (eql 1 (lisp-ai-profile:error-count root))))))

(defun guard-hook-wired-p (settings)
  "True when SETTINGS (a parsed settings object) has a PreToolUse hook with the matcher Bash whose command ends in
   /bin/guard."
  (let* ((hooks (gethash "hooks" settings)) (entries (and hooks (gethash "PreToolUse" hooks))))
    (and (some (lambda (entry)
                 (and (equal (gethash "matcher" entry) "Bash")
                      (some (lambda (hook)
                              (let ((command (gethash "command" hook)))
                                (and (stringp command) (uiop:string-suffix-p command "/bin/guard"))))
                            (coerce (gethash "hooks" entry) 'list))))
               (coerce entries 'list))
         t)))

(defun answer-once (path reply)
  "Listen on the Unix socket PATH and answer the first connection with REPLY, after reading its request to the end, as the
   always-on image does. Returns the thread."
  (when (probe-file path) (delete-file path))
  (let ((listener (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (sb-bsd-sockets:socket-bind listener path)
    (sb-bsd-sockets:socket-listen listener 1)
    (sb-thread:make-thread
     (lambda ()
       (unwind-protect
            (let* ((connection (sb-bsd-sockets:socket-accept listener))
                   (stream (sb-bsd-sockets:socket-make-stream connection :input t :output t :element-type 'character
                                                                         :external-format :utf-8 :buffering :full)))
              (loop while (read-char stream nil))
              (write-string reply stream)
              (finish-output stream)
              (close stream))
         (sb-bsd-sockets:socket-close listener))))))

(defun guard-output (guard socket)
  "What GUARD prints for a refused command when the socket SOCKET is the only place it can ask."
  (with-input-from-string (payload (hook-json "Bash" "cat f"))
    (uiop:run-program (list "env" (format nil "LISP_AI_SOCKET=~a" socket) guard)
                      :input payload :output :string :ignore-error-status t)))

(defun run-socket-replies (guard)
  "tools/guard trusts the image's reply only when it starts with the marker: an empty reply (a dropped connection) and an
   error text are not answers, so the guard judges the command itself; a reply with the marker is the answer."
  (let ((path (format nil "/tmp/lisp-ai-guard-fake-~d.sock" (sb-posix:getpid))))
    (unwind-protect
         (progn
           (answer-once path "")
           (check "an empty reply is not trusted: the command is refused anyway"
                  (search "\"permissionDecision\":\"deny\"" (guard-output guard path)))
           (answer-once path "ERROR: boom")
           (check "an error text is not trusted: the command is refused anyway"
                  (search "\"permissionDecision\":\"deny\"" (guard-output guard path)))
           (answer-once path (lisp-ai-hook:socket-marker))
           (check-equal "a reply with the marker is the answer, here to allow" (guard-output guard path) ""))
      (when (probe-file path) (delete-file path)))))

(defun run-project (project)
  "The checks on the real PROJECT: its bin/guard falls back to a fresh guard when the socket file is stale, and its
   .claude/settings.json is in step with rules/forbidden-programs.sexp and still wires the guard hook."
  (let ((stale (format nil "/tmp/lisp-ai-guard-stale-~d.sock" (sb-posix:getpid)))
        (guard (namestring (merge-pathnames "bin/guard" (uiop:ensure-directory-pathname project)))))
    ;; bin/guard with a stale socket file (the server died and left it) must fall back to a fresh guard, never wave through
    (when (probe-file stale) (delete-file stale))
    (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
      (sb-bsd-sockets:socket-bind socket stale)
      (sb-bsd-sockets:socket-close socket))
    (unwind-protect
         (progn
           (check "the stale socket file exists" (probe-file stale))
           (check "bin/guard still refuses with a stale socket"
                  (search "\"permissionDecision\":\"deny\""
                          (with-input-from-string (payload (hook-json "Bash" "cat f"))
                            (uiop:run-program (list "env" (format nil "LISP_AI_SOCKET=~a" stale) guard)
                                              :input payload :output :string :ignore-error-status t)))))
      (when (probe-file stale) (delete-file stale)))
    (run-socket-replies guard))
  (let ((text (uiop:read-file-string (merge-pathnames ".claude/settings.json" (uiop:ensure-directory-pathname project)))))
    (let ((have (deny-list (com.inuoe.jzon:parse text)))
          (want (lisp-ai-guard:permission-rules (lisp-ai-guard:forbidden-programs project))))
      (check-equal "the project settings lack no deny rule of the forbidden programs"
                   (set-difference want have :test #'string=) nil)
      (check-equal "and have no deny rule that is not one of theirs" (set-difference have want :test #'string=) nil))
    (check "the project settings wire bin/guard as a PreToolUse hook on Bash"
           (guard-hook-wired-p (com.inuoe.jzon:parse text)))))

(defun run (directory project)
  "Run the checks with a scratch project inside DIRECTORY, and the checks on the real PROJECT (a directory); returns the
   number of failures."
  (setf *failures* 0)
  (let ((root (merge-pathnames "guard-test-project/" (uiop:ensure-directory-pathname directory))))
    (when (probe-file root) (uiop:delete-directory-tree root :validate t))
    (write-file (merge-pathnames "rules/profile-corpus.sexp" root) (format nil "(\"cat /dev/null\")~%"))
    (write-file (merge-pathnames "rules/forbidden-programs.sexp" root) (format nil "(\"cat\" \"head\")~%"))
    (unwind-protect
         (progn (run-guard) (run-guard-wrappers) (run-guard-structure) (run-permissions) (run-hook root))
      (when (probe-file root) (uiop:delete-directory-tree root :validate t))))
  (run-project project)
  *failures*)
