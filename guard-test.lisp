;;;; Tests of guard.lisp (the gate) and of the word guard of the script lisp, shared by every project under ~/lisp-ai as
;;;; part of the system lisp-ai-tools/tests: a project calls (lisp-ai-guard-tests:run DIRECTORY PROJECT), which makes a
;;;; scratch project inside DIRECTORY (the project's run/) for the checks that need one, removes it afterwards, checks the
;;;; real PROJECT (its bin/lisp guard, its .claude/settings.json and its rules), and returns how many checks failed,
;;;; printing a line for each.

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

(defparameter *lisp-files*
  '(:code-suffixes ("lisp" "asd") :data-suffixes ("sexp") :directories ("src" "tests" "tools")))

(defun uses (command) (lisp-ai-guard:forbidden-uses command *forbidden*))

(defun verdict (json) (lisp-ai-guard:hook-verdict json *forbidden* *lisp-files*))

(defun hook-json (tool command)
  (format nil "{\"tool_name\":~s,\"tool_input\":{\"command\":~s}}" tool command))

(defun file-edit-json (tool path)
  (format nil "{\"tool_name\":~s,\"tool_input\":{\"file_path\":~s}}" tool path))

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
  (let ((refusal (verdict (hook-json "Bash" "cat f.lisp"))))
    (check "a forbidden command on Lisp is refused with a message" (stringp refusal))
    (check "and the message names the program" (search "cat" refusal)))
  (check "an allowed command goes through" (null (verdict (hook-json "Bash" "git status"))))
  (check "a call of another tool goes through" (null (verdict (hook-json "Read" "cat f.lisp"))))
  ;; a hook payload that cannot be read is refused loudly, never waved through
  (check "a payload that is not JSON is refused" (stringp (verdict "not json"))))

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

(defun run-lisp-files ()
  "A forbidden program is refused only when the command also names Lisp: a file with a code or data suffix, or a path
   through one of the Lisp directories. On any other file it is allowed. Each row is a command and whether it is refused."
  (dolist
      (row
       '(("sed -i s/a/b/ src/x.lisp" t) ("cat x.asd" t) ("cat data/t.sexp" t) ("grep -r foo src" t)
         ("grep foo ./tests/a" t) ("git diff -- a.lisp | head -5" t) ("sh -c 'sed p x.lisp'" t)
         ("python3 -c 'print(open(\"x.lisp\").read())'" t) ("wc -l run/test.stderr" nil) ("jq . report.json" nil)
         ("git status | head" nil) ("sort names.txt" nil) ("cat notes.md" nil) ("grep foo docs/a.md" nil)
         ("diff a.txt b.txt" nil) ("echo x.lisp" nil) ("ls src tests" nil) ("cat tests-output.txt" nil)
         ("sed p lisp" nil)))
    (destructuring-bind (command refused) row
      (check-equal command (and (verdict (hook-json "Bash" command)) t) refused)))
  (check "the refusal names the Lisp file" (search "x.lisp" (verdict (hook-json "Bash" "cat x.lisp"))))
  (check "and says other files are allowed" (search "not Lisp" (verdict (hook-json "Bash" "cat x.lisp")))))

(defun run-writers ()
  "A shell command that would write a Lisp code file is refused whatever program runs it: a redirection into one, tee,
   cp, mv, install, ln, rsync, dd. Writing other files, and reading Lisp files with these programs, is allowed. Each row
   is a command and the files it would write."
  (dolist
      (row
       '(("echo x > src/a.lisp" ("src/a.lisp")) ("echo x >> a.lisp" ("a.lisp")) ("echo x >src/a.lisp" ("src/a.lisp"))
         ("printf '(x)' 1> x.asd" ("x.asd")) ("echo a | tee src/a.lisp" ("src/a.lisp"))
         ("cp /tmp/x.lisp src/a.lisp" ("src/a.lisp")) ("cp /tmp/x.lisp src/" ("src/x.lisp"))
         ("mv a.lisp src/b.lisp" ("src/b.lisp")) ("install -m 644 x.lisp src/x.lisp" ("src/x.lisp"))
         ("ln -s /tmp/x.lisp src/x.lisp" ("src/x.lisp")) ("dd if=/dev/zero of=src/a.lisp" ("src/a.lisp"))
         ("ls && echo x > tests/t.lisp" ("tests/t.lisp")) ("sh -c 'echo x > a.lisp'" nil)
         ("echo x > run/notes.txt" nil) ("echo x 2>&1" nil) ("cat src/a.lisp > /tmp/copy.txt" nil)
         ("cp src/a.lisp /tmp/" nil) ("cp a.txt src/" nil) ("echo a | tee run/out.log" nil) ("git mv a.lisp b.lisp" nil)))
    (destructuring-bind (command expected) row
      (check-equal command (lisp-ai-guard:written-lisp-files command *lisp-files*) expected)))
  (let ((refusal (verdict (hook-json "Bash" "echo '(defun f ())' > src/f.lisp"))))
    (check "a shell write of a Lisp file is refused" (stringp refusal))
    (check "and the message points to the workbench" (and refusal (search "(save '" refusal))))
  (check "a shell write of another file is allowed" (null (verdict (hook-json "Bash" "echo x > run/out.txt")))))

(defun run-file-edit (directory)
  "The Edit, MultiEdit and Write tools may not change a Lisp file (suffix lisp or asd) that exists: it is changed in the
   image and saved from it. Write may create one that does not exist yet. Files that are not code, and other tools, go
   through. A call with no file_path is refused, not guessed at."
  (let ((existing (namestring (merge-pathnames "guard-test-existing.lisp" directory)))
        (absent (namestring (merge-pathnames "guard-test-absent.lisp" directory)))
        (notes (namestring (merge-pathnames "guard-test-notes.md" directory))))
    (write-file existing (format nil "(x)~%"))
    (write-file notes (format nil "text~%"))
    (when (probe-file absent) (delete-file absent))
    (unwind-protect
        (progn
         (check "Edit of a Lisp file is refused" (stringp (verdict (file-edit-json "Edit" existing))))
         (check "and the message points to the workbench"
                (search "(save '" (verdict (file-edit-json "Edit" existing))))
         (check "MultiEdit of a Lisp file is refused" (stringp (verdict (file-edit-json "MultiEdit" existing))))
         (check "Edit of an .asd file is refused" (stringp (verdict (file-edit-json "Edit" "/x/y/z.asd"))))
         (check "Write over an existing Lisp file is refused" (stringp (verdict (file-edit-json "Write" existing))))
         (check "Write of a Lisp file that does not exist yet is allowed"
                (null (verdict (file-edit-json "Write" absent))))
         (check "Edit of a data file (.sexp) is allowed" (null (verdict (file-edit-json "Edit" "/x/data/t.sexp"))))
         (check "Edit of a markdown file is allowed" (null (verdict (file-edit-json "Edit" notes))))
         (check "Write over an existing markdown file is allowed" (null (verdict (file-edit-json "Write" notes))))
         (check "Read of a Lisp file is allowed" (null (verdict (file-edit-json "Read" existing))))
         (check "a call with no file_path is refused"
                (stringp (verdict "{\"tool_name\":\"Edit\",\"tool_input\":{\"old_string\":\"a\"}}")))
         (check "a Write whose file_path is not a string is refused"
                (stringp (verdict "{\"tool_name\":\"Write\",\"tool_input\":{\"file_path\":7}}"))))
      (dolist (file (list existing notes absent)) (when (probe-file file) (delete-file file))))))

(defun gate-log-lines (root)
  (let ((file (merge-pathnames "run/gate.log" root)))
    (if (probe-file file)
        (with-open-file (in file :external-format :utf-8)
          (loop for line = (read-line in nil) while line collect line))
        '())))

(defun run-hook (root)
  "The hook response on the scratch project ROOT: a deny decision for a refused call, the empty string for an allowed
   one; respond reads both rules files of the project; each denial is one line in run/gate.log and an allowed call none."
  (flet ((respond (json) (lisp-ai-guard:hook-response json *forbidden* *lisp-files* root))
         (denied-p (reply) (and (search "\"permissionDecision\":\"deny\"" reply) t)))
    (check "a forbidden command on Lisp is denied" (denied-p (respond (hook-json "Bash" "cat /dev/null.lisp"))))
    (check-equal "an allowed command is allowed" (respond (hook-json "Bash" "echo hi")) "")
    (check-equal "a call of another tool is let through"
                 (respond "{\"tool_name\":\"Read\",\"tool_input\":{}}") "")
    (check "an Edit of a Lisp file is denied"
           (denied-p (respond (file-edit-json "Edit" (namestring (merge-pathnames "src/a.lisp" root))))))
    (check-equal "respond reads the forbidden programs and the Lisp files from the project's rules"
                 (denied-p (lisp-ai-guard:respond root (hook-json "Bash" "head /dev/null.lisp"))) t)
    (check-equal "respond allows head on a file that is not Lisp"
                 (lisp-ai-guard:respond root (hook-json "Bash" "head /dev/null")) "")
    (let ((lines (gate-log-lines root)))
      (check-equal "three denials, three lines in the gate log" (length lines) 3)
      (check "a line names the tool and the command" (and lines (search "Bash" (first lines)) (search "cat /dev/null.lisp" (first lines))))
      (check "the Edit denial names the file" (and (third lines) (search "Edit" (second lines)))))
    (let ((lisp-ai-guard:*gate-log* nil))
      (check "with the log off a denial is still a denial" (denied-p (respond (hook-json "Bash" "cat x.lisp"))))
      (check-equal "and nothing more is written" (length (gate-log-lines root)) 3))))

(defun json-with-gate-dir (directory json)
  "JSON with the field lisp_ai_gate_dir naming DIRECTORY added in front, as the script adds it before asking an image."
  (format nil "{\"lisp_ai_gate_dir\":~s,~a" (string-right-trim "/" (namestring directory))
          (subseq json (1+ (position #\{ json)))))

(defun run-gate-directory (root)
  "A hook payload that names a directory in lisp_ai_gate_dir is judged by that directory's rules and logged in its
   run/gate.log, not in the project's: a session started outside a project asks a running image, and the denial belongs
   to the caller. Without the field the image's own project applies, as before. A directory that does not exist is an
   error, not a fallback."
  (let ((other (merge-pathnames "other/" root))
        (before (length (gate-log-lines root))))
    (write-file (merge-pathnames "rules/forbidden-programs.sexp" other) (format nil "(\"wc\")~%"))
    (write-file (merge-pathnames "rules/lisp-files.sexp" other) (format nil "~s~%" *lisp-files*))
    (flet ((reply (json) (lisp-ai-guard:socket-reply json *forbidden* root))
           (denied-p (reply) (and (search "\"permissionDecision\":\"deny\"" reply) t)))
      (check-equal "without the field the image's own rules apply: cat on Lisp is denied"
                   (denied-p (reply (hook-json "Bash" "cat x.lisp"))) t)
      (check-equal "with the field the named directory's rules apply, and cat is not in them"
                   (reply (json-with-gate-dir other (hook-json "Bash" "cat x.lisp"))) (lisp-ai-guard:socket-marker))
      (check-equal "and wc is" (denied-p (reply (json-with-gate-dir other (hook-json "Bash" "wc x.lisp")))) t)
      (check-equal "the denial is logged in the named directory" (length (gate-log-lines other)) 1)
      (check-equal "and the project's log grew only by the denial without the field"
                   (- (length (gate-log-lines root)) before) 1)
      (check "a directory that does not exist is an error"
             (handler-case (progn (reply (json-with-gate-dir (merge-pathnames "nowhere/" root) (hook-json "Bash" "ls")))
                                  nil)
               (error () t))))))

(defun guard-hook-wired-p (settings matcher)
  "True when SETTINGS (a parsed settings object) has a PreToolUse hook with the matcher MATCHER whose command ends in
   /bin/lisp guard."
  (let* ((hooks (gethash "hooks" settings)) (entries (and hooks (gethash "PreToolUse" hooks))))
    (and (some (lambda (entry)
                 (and (equal (gethash "matcher" entry) matcher)
                      (some (lambda (hook)
                              (let ((command (gethash "command" hook)))
                                (and (stringp command) (uiop/utility:string-suffix-p command "/bin/lisp guard"))))
                            (coerce (gethash "hooks" entry) 'list))))
               (coerce entries 'list))
         t)))

(defun profile-hook-p (settings)
  "True when any hook command in SETTINGS still calls bin/lisp profile."
  (let ((hooks (gethash "hooks" settings)) (found nil))
    (when hooks
      (maphash (lambda (event entries)
                 (declare (ignore event))
                 (loop for entry across entries
                       do (loop for hook across (gethash "hooks" entry)
                                when (search "lisp profile" (or (gethash "command" hook) ""))
                                do (setf found t))))
               hooks))
    found))

(defvar *last-request* nil "The request the last ANSWER-ONCE listener read, so a test can see what the script sent.")

(defun answer-once (path reply)
  "Listen on the Unix socket PATH and answer the first connection with REPLY, after reading its request to the end, as the
   always-on image does; the request is kept in *LAST-REQUEST*. Returns the thread."
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
              (setf *last-request*
                    (with-output-to-string (text)
                      (loop for char = (read-char stream nil) while char do (write-char char text))))
              (write-string reply stream)
              (finish-output stream)
              (close stream))
         (sb-bsd-sockets:socket-close listener))))))

(defun guard-output (script socket &optional (payload (hook-json "Bash" "cat f.lisp")))
  "What the guard word of SCRIPT (the project's bin/lisp) prints for PAYLOAD when the socket SOCKET is the only place it
   can ask."
  (with-input-from-string (in payload)
    (uiop/run-program:run-program (list "env" (format nil "LISP_AI_SOCKET=~a" socket) script "guard") :input in
                                  :output :string :ignore-error-status t)))

(defun run-socket-replies (guard)
  "bin/lisp guard trusts the image's reply only when it starts with the marker: an empty reply (a dropped connection) and
   an error text are not answers, so the guard judges the command itself; a reply with the marker is the answer. What it
   sends is the payload with the field lisp_ai_gate_dir naming the project's directory, whose rules and gate log apply,
   in front of the payload's own fields."
  (let ((path (format nil "/tmp/lisp-ai-guard-fake-~d.sock" (sb-posix:getpid))))
    (unwind-protect
         (progn
           (answer-once path "")
           (check "an empty reply is not trusted: the command is refused anyway"
                  (search "\"permissionDecision\":\"deny\"" (guard-output guard path)))
           (answer-once path "ERROR: boom")
           (check "an error text is not trusted: the command is refused anyway"
                  (search "\"permissionDecision\":\"deny\"" (guard-output guard path)))
           (answer-once path (lisp-ai-guard:socket-marker))
           (check-equal "a reply with the marker is the answer, here to allow" (guard-output guard path) "")
           (check "the script names the directory whose rules and log apply, in lisp_ai_gate_dir"
                  (and (stringp *last-request*)
                       (search (format nil "{\"lisp_ai_gate_dir\":~s,"
                                       (string-right-trim "/" (namestring (uiop:getcwd))))
                               *last-request*)))
           (check "and the payload's own fields follow unchanged"
                  (and (stringp *last-request*)
                       (search (subseq (hook-json "Bash" "cat f.lisp") 1) *last-request*))))
      (when (probe-file path) (delete-file path)))))

(defun run-project (project)
  "The checks on the real PROJECT: its bin/lisp guard falls back to a fresh guard when the socket file is stale and judges
   an Edit payload too; its .claude/settings.json wires the guard hook for Bash and for Edit, Write and MultiEdit, has no
   deny list and no profile hook; its rules say what Lisp is."
  (let ((stale (format nil "/tmp/lisp-ai-guard-stale-~d.sock" (sb-posix:getpid)))
        (guard (namestring (merge-pathnames "bin/lisp" (uiop/pathname:ensure-directory-pathname project)))))
    ;; bin/lisp guard with a stale socket file (the server died and left it) must fall back to a fresh guard, never wave through
    (when (probe-file stale) (delete-file stale))
    (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
      (sb-bsd-sockets:socket-bind socket stale)
      (sb-bsd-sockets:socket-close socket))
    (unwind-protect
         (progn
          (check "the stale socket file exists" (probe-file stale))
          (check "bin/lisp guard still refuses with a stale socket"
           (search "\"permissionDecision\":\"deny\"" (guard-output guard stale)))
          (check "and refuses an Edit of a Lisp file the same way"
           (search "\"permissionDecision\":\"deny\""
                   (guard-output guard stale
                    (file-edit-json "Edit"
                     (namestring (merge-pathnames "src/x.lisp" (uiop/pathname:ensure-directory-pathname project)))))))
          (check "and allows a command that names no Lisp: the fallback judges, it does not deny everything"
           (string= "" (guard-output guard stale (hook-json "Bash" "wc -l qlfile")))))
      (when (probe-file stale) (delete-file stale)))
    (run-socket-replies guard))
  (let ((settings (com.inuoe.jzon:parse
                   (uiop:read-file-string (merge-pathnames ".claude/settings.json" (uiop:ensure-directory-pathname project))))))
    (check "the project settings wire bin/lisp guard as a PreToolUse hook on Bash" (guard-hook-wired-p settings "Bash"))
    (check "and on the tools that change a file by its path" (guard-hook-wired-p settings "Edit|Write|MultiEdit"))
    (check "and carry no permissions.deny list: the gate decides by what a command names"
           (null (gethash "deny" (or (gethash "permissions" settings) (make-hash-table)))))
    (check "and no profile hook" (not (profile-hook-p settings)))
    (check "the project's rules/lisp-files.sexp says what Lisp is"
           (member "lisp" (getf (lisp-ai-guard:lisp-files project) :code-suffixes) :test #'equal))))

(defun run (directory project)
  "Run the checks with a scratch project inside DIRECTORY, and the checks on the real PROJECT (a directory); returns the
   number of failures."
  (setf *failures* 0)
  (let ((root (merge-pathnames "guard-test-project/" (uiop:ensure-directory-pathname directory))))
    (when (probe-file root) (uiop:delete-directory-tree root :validate t))
    (write-file (merge-pathnames "rules/forbidden-programs.sexp" root) (format nil "(\"cat\" \"head\")~%"))
    (write-file (merge-pathnames "rules/lisp-files.sexp" root) (format nil "~s~%" *lisp-files*))
    (write-file (merge-pathnames "src/a.lisp" root) (format nil "(x)~%"))
    (unwind-protect
         (progn (run-guard) (run-guard-wrappers) (run-guard-structure) (run-lisp-files) (run-writers)
                (run-file-edit directory) (run-hook root) (run-gate-directory root))
      (when (probe-file root) (uiop:delete-directory-tree root :validate t))))
  (run-project project)
  *failures*)
