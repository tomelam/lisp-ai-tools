;;;; The guard: which text-processing programs does a shell command run, and the permission rules that deny them.
;;;; Pure functions over strings, except FORBIDDEN-PROGRAMS and SYNC-DENY-RULES, which read and write a project's files.
;;;; It knows nothing of the profiler: tools/hook.lisp puts the two together. tools/guard is the only place that touches
;;;; stdin and exit codes.
(defpackage :lisp-ai-guard
  (:use :cl)
  (:export #:forbidden-uses
           #:command-programs
           #:hook-verdict
           #:hook-bash-call
           #:refusal-message
           #:deny-json
           #:forbidden-programs
           #:permission-rules
           #:settings-with-deny
           #:sync-deny-rules
           #:*program-directories*
           #:lisp-files
           #:lisp-names
           #:file-edit-refusal))

(in-package :lisp-ai-guard)

(defparameter *wrappers* '("sudo" "env" "time" "nohup" "command" "exec" "nice" "builtin")
  "Programs that only run another program, named by the words after them.")

(defparameter *option-taking-flags* '("-I" "-n" "-P" "-L" "-d" "-E" "-s")
  "xargs flags whose value is the next word.")

(defun heredoc-tag (line)
  "The terminator named by a here-document marker (<<TAG, <<-TAG, <<'TAG') on LINE, or NIL."
  (let ((marker (search "<<" line)))
    (when (and marker
               (not (and (< (+ marker 2) (length line)) (char= (char line (+ marker 2)) #\<))))
      (let* ((rest (string-left-trim "- " (subseq line (+ marker 2))))
             (end (position-if (lambda (c) (find c '(#\Space #\Tab #\; #\| #\&))) rest))
             (tag (string-trim "'\"" (subseq rest 0 end))))
        (and (plusp (length tag)) tag)))))

(defun strip-heredoc-bodies (command)
  "COMMAND without the lines of its here-documents: they are data fed to a program, not commands."
  (with-output-to-string (out)
    (let ((tag nil))
      (dolist (line (uiop:split-string command :separator '(#\Newline)))
        (cond (tag (when (string= (string-trim '(#\Tab) line) tag) (setf tag nil)))
              (t (write-line line out)
                 (setf tag (heredoc-tag line))))))))

(defun split-simple-commands (command)
  "COMMAND as a list of simple commands, each a list of words with quoting removed.
   Separators: | ; & newline ( ) and the backtick. Not analysed: a command substitution inside double quotes."
  (let ((segments '()) (words '()) (word (make-string-output-stream))
        (in-word nil) (quote-char nil) (i 0)
        (command (strip-heredoc-bodies command)))
    (flet ((end-word ()
             (when in-word (push (get-output-stream-string word) words) (setf in-word nil)))
           (end-segment ()
             (when in-word (push (get-output-stream-string word) words) (setf in-word nil))
             (when words (push (nreverse words) segments) (setf words '()))))
      (loop while (< i (length command))
            do (let ((c (char command i)))
                 (cond
                   ((eql quote-char #\') (if (char= c #\') (setf quote-char nil) (write-char c word)))
                   ((eql quote-char #\")
                    (cond ((char= c #\") (setf quote-char nil))
                          ((and (char= c #\\) (< (1+ i) (length command)))
                           (incf i)
                           (write-char (char command i) word))
                          (t (write-char c word))))
                   ((char= c #\\)
                    (setf in-word t)
                    (when (< (1+ i) (length command))
                      (incf i)
                      (write-char (char command i) word)))
                   ((char= c #\') (setf quote-char #\' in-word t))
                   ((char= c #\") (setf quote-char #\" in-word t))
                   ((find c '(#\Space #\Tab)) (end-word))
                   ((find c '(#\| #\; #\& #\Newline #\( #\) #\`)) (end-segment))
                   (t (setf in-word t) (write-char c word))))
               (incf i))
      (end-segment))
    (nreverse segments)))

(defun assignment-p (word)
  "True for NAME=value, a variable set just for the command that follows."
  (let ((eq (position #\= word)))
    (and eq (plusp eq) (every (lambda (c) (or (alphanumericp c) (char= c #\_))) (subseq word 0 eq)))))

(defun base-name (word)
  (subseq word (1+ (or (position #\/ word :from-end t) -1))))

(defun drop-options (words &key (value-flags '()))
  "WORDS without its leading options; a flag in VALUE-FLAGS also takes the next word."
  (loop while (and words (plusp (length (first words))) (char= (char (first words) 0) #\-))
        do (let ((flag (pop words)))
             (when (and words (member flag value-flags :test #'string=)) (pop words))))
  words)

(defparameter *wrapper-value-flags*
  '(("sudo" "-u" "-g" "-h" "-p" "-C" "-D" "-R" "-T" "-U") ("nice" "-n") ("env" "-u" "-C") ("time" "-f" "-o"))
  "Per wrapper, the options whose value is the next word.")

(defparameter *timeout-value-flags* '("-s" "-k") "timeout options whose value is the next word.")

(defparameter *shells* '("sh" "bash" "zsh" "dash" "ksh") "Programs that run the script given to them by -c.")

(defun drop-assignments (words)
  "WORDS without its leading NAME=value words."
  (loop while (and words (assignment-p (first words)))
        do (pop words))
  words)

(defparameter *shell-keywords*
  '("if" "then" "elif" "else" "do" "while" "until" "{" "!")
  "Words that begin a command inside a compound command (if ...; then CMD; fi, while ...; do CMD; done, { CMD; }): the
   program that runs is the word after them.")

(defun unwrap (words)
  "The program a simple command (a list of words) runs, and the words after it, looking through assignments, shell keywords
   such as then and do, and wrappers such as sudo, env, nice, timeout and xargs, with their options and the values of
   their options."
  (loop (setf words (drop-assignments words))
        (let ((base (and words (base-name (first words)))))
          (cond ((null base) (return (values nil nil))) ((member base *shell-keywords* :test #'string=) (pop words))
                ((member base *wrappers* :test #'string=) (pop words)
                 (setf words
                         (drop-options words :value-flags (rest (assoc base *wrapper-value-flags* :test #'string=)))))
                ((string= base "timeout") (pop words)
                 (setf words (drop-options words :value-flags *timeout-value-flags*)) (pop words))
                ((string= base "xargs") (pop words)
                 (setf words (drop-options words :value-flags *option-taking-flags*)))
                (t (return (values base (rest words))))))))

(defun shell-command-string (words)
  "The script a shell is given by -c (or -lc, -ec ...) in WORDS, the words after the shell's name; NIL if none."
  (loop while (and words (plusp (length (first words))) (char= (char (first words) 0) #\-))
        do (let ((flag (pop words)))
             (cond ((string= flag "--") (return nil))
                   ((and (> (length flag) 1) (char/= (char flag 1) #\-) (find #\c flag)) (return (first words)))
                   ((member flag '("-o" "-O") :test #'string=) (pop words))))))

(defun find-exec-commands (words)
  "The commands find runs for -exec, -execdir, -ok and -okdir in WORDS (the words after find): each a list of words."
  (let ((commands nil))
    (loop while words
          do (when (member (pop words) '("-exec" "-execdir" "-ok" "-okdir") :test #'string=)
               (let ((end (position-if (lambda (w) (member w '(";" "+") :test #'string=)) words)))
                 (push (subseq words 0 end) commands)
                 (setf words (and end (nthcdr (1+ end) words))))))
    (nreverse commands)))

(defun segment-programs (words)
  "Every program a simple command runs: its own, and those in a script it gives to a shell or eval, and those find
   runs with -exec."
  (multiple-value-bind (base rest)
      (unwrap words)
    (when base
      (cons base
            (cond
             ((member base *shells* :test #'string=)
              (let ((script (shell-command-string rest)))
                (and script (command-programs script))))
             ((string= base "eval") (command-programs (format nil "~{~a~^ ~}" rest)))
             ((string= base "find")
              (loop for command in (find-exec-commands rest)
                    append (segment-programs command))))))))

(defun double-quoted-substitutions (command)
  "The texts of the $(...) and backtick substitutions that sit inside double quotes in COMMAND, outermost first. The splitter
   does not look inside double quotes, so COMMAND-PROGRAMS reads these separately. Text in single quotes and here-document
   bodies is not searched."
  (let ((text (strip-heredoc-bodies command)) (found 'nil) (state nil) (i 0))
    (flet ((closing-paren (open)
             (let ((depth 0))
               (loop for j from open below (length text)
                     do (case (char text j) (#\( (incf depth)) (#\) (decf depth) (when (zerop depth) (return j))))))))
      (loop while (< i (length text))
            do
              (let ((c (char text i)))
                (cond ((eql state #\') (when (char= c #\') (setf state nil))) ((char= c #\\) (incf i))
                      ((and (eql state #\") (char= c #\")) (setf state nil))
                      ((and (null state) (member c '(#\' #\"))) (setf state c))
                      ((and (eql state #\") (char= c #\$) (< (1+ i) (length text)) (char= (char text (1+ i)) #\())
                       (let ((end (closing-paren (1+ i))))
                         (push (subseq text (+ i 2) end) found)
                         (setf i (or end (length text)))))
                      ((and (eql state #\") (char= c #\`))
                       (let ((end (position #\` text :start (1+ i))))
                         (push (subseq text (1+ i) end) found)
                         (setf i (or end (length text)))))))
              (incf i)))
    (nreverse found)))

(defun command-programs (command)
  "The programs of COMMAND (a shell command string): those of its simple commands in order of appearance, then those inside
   $(...) and backticks within double quotes."
  (append
   (loop for words in (split-simple-commands command)
         append (segment-programs words))
   (loop for inner in (double-quoted-substitutions command)
         append (command-programs inner))))

(defun segment-program (words) "The program a simple command runs: the first value of UNWRAP." (values (unwrap words)))

(defun forbidden-uses (command forbidden)
  "The programs of COMMAND (a shell command string) that are in the list FORBIDDEN, in order of appearance."
  (remove-if-not (lambda (program) (member program forbidden :test #'string=)) (command-programs command)))

(defun lisp-files (project)
  "What counts as Lisp for the guard: the one form in rules/lisp-files.sexp of PROJECT, a plist (:code-suffixes (\"lisp\" \"asd\") :data-suffixes (\"sexp\") :directories (\"src\" \"tests\" \"tools\")). Code is what the Edit and Write tools may not change; code and data are what a text-processing program may not read or change. A missing file is an error, never an empty rule."
  (lisp-ai-project:read-rules project "lisp-files"))

(defun command-tokens (command)
  "The words of COMMAND, a shell command string, split at blanks, quotes and shell punctuation, here-document bodies left out. Scripts given to sh -c, and file names after a redirection, come out as words too."
  (remove ""
          (uiop/utility:split-string (strip-heredoc-bodies command) :separator
                                     '(#\  #\Tab #\Newline #\; #\| #\& #\( #\) #\< #\> #\` #\' #\" #\=))
          :test #'string=))

(defun path-type (path)
  "The suffix after the last dot of the last component of PATH, or NIL when there is none or the name only starts with a dot."
  (let* ((name (subseq path (1+ (or (position #\/ path :from-end t) -1)))) (dot (position #\. name :from-end t)))
    (and dot (plusp dot) (subseq name (1+ dot)))))

(defun token-names-lisp-p (token lisp-files)
  "True when TOKEN, a word of a command, names Lisp: a file with a code or data suffix of LISP-FILES, or a path with one of its directories as a component."
  (let ((type (path-type token))
        (components (remove "" (uiop/utility:split-string token :separator "/") :test #'string=)))
    (and
     (or
      (and type
           (member type (append (getf lisp-files :code-suffixes) (getf lisp-files :data-suffixes)) :test #'string=))
      (intersection components (getf lisp-files :directories) :test #'string=))
     t)))

(defun lisp-names (command lisp-files)
  "The words of COMMAND that name Lisp (token-names-lisp-p), in order, each once."
  (remove-duplicates (remove-if-not (lambda (token) (token-names-lisp-p token lisp-files)) (command-tokens command))
                     :test #'string= :from-end t))

(defparameter *file-edit-tools*
  '("Edit" "MultiEdit" "Write")
  "The Claude Code tools that change a file by its path (tool_input.file_path).")

(defun file-edit-refusal (tool path lisp-files)
  "NIL if the call of TOOL on PATH may proceed, else the refusal message. Edit and MultiEdit may not change a file with a code suffix of LISP-FILES, Write may not replace one; Write may create one that does not exist. A call with no path cannot be judged and is refused."
  (cond ((not (member tool *file-edit-tools* :test #'string=)) nil)
        ((not (stringp path))
         (format nil "bin/lisp guard: the ~a call has no file_path, so it cannot be judged: refused." tool))
        ((not (member (path-type path) (getf lisp-files :code-suffixes) :test #'equal)) nil)
        ((and (string= tool "Write") (not (probe-file path))) nil)
        (t
         (format nil
                 "Blocked: ~a on the Lisp file ~a. Lisp code is changed as forms, through the image: bin/lisp eval '(form-edit::edit ...)', form-edit::edit-subform or form-edit::delete-subform. The Write tool may only create a Lisp file that does not exist yet."
                 tool path))))

(defun refusal-message (uses names forbidden)
  (let ((distinct (remove-duplicates uses :test #'string= :from-end t)))
    (format nil
            "Blocked: ~{~a~^, ~} ~:[is a text-processing program~;are text-processing programs~], used here on Lisp (~{~a~^, ~}). Read, search and change Lisp as forms: the REPL, bin/lisp eval, form-edit::edit, and let the Lisp printer format them. On files that are not Lisp these programs are allowed. Forbidden on Lisp: ~{~a~^ ~}."
            distinct (rest distinct) names forbidden)))

(defun deny-json (reason)
  "The Claude Code PreToolUse decision that refuses a tool call, with REASON, as a JSON string."
  (let ((decision (make-hash-table :test 'equal))
        (details (make-hash-table :test 'equal)))
    (setf (gethash "hookEventName" details) "PreToolUse"
          (gethash "permissionDecision" details) "deny"
          (gethash "permissionDecisionReason" details) reason
          (gethash "hookSpecificOutput" decision) details)
    (com.inuoe.jzon:stringify decision)))

(defun hook-bash-call (hook-json)
  "(values id command) of the Claude Code hook payload HOOK-JSON when it is a call of the Bash tool, else NIL."
  (let ((payload (handler-case (com.inuoe.jzon:parse hook-json) (error nil nil))))
    (when (and (hash-table-p payload) (equal (gethash "tool_name" payload) "Bash"))
      (let* ((input (gethash "tool_input" payload)) (command (and (hash-table-p input) (gethash "command" input))))
        (when (stringp command) (values (gethash "tool_use_id" payload) command))))))

(defun hook-verdict (hook-json forbidden lisp-files)
  "NIL if the Claude Code hook payload HOOK-JSON may proceed, else the refusal message. A Bash call is refused when it runs a program of FORBIDDEN and also names Lisp (LISP-FILES); a call of Edit, MultiEdit or Write is judged by file-edit-refusal. Other tools are not judged. A payload that cannot be read is refused."
  (let ((payload (handler-case (com.inuoe.jzon:parse hook-json) (error nil nil))))
    (if (not (hash-table-p payload))
        "bin/lisp guard could not read the hook payload as JSON, so it refuses the call."
        (let* ((tool (gethash "tool_name" payload))
               (input
                (let ((input (gethash "tool_input" payload)))
                  (and (hash-table-p input) input))))
          (cond
           ((equal tool "Bash")
            (let* ((command (and input (gethash "command" input)))
                   (uses (and (stringp command) (forbidden-uses command forbidden)))
                   (names (and uses (lisp-names command lisp-files))))
              (when (and uses names) (refusal-message uses names forbidden))))
           ((member tool *file-edit-tools* :test #'equal)
            (file-edit-refusal tool (and input (gethash "file_path" input)) lisp-files)))))))

(defun forbidden-programs (project)
  "The programs named in rules/forbidden-programs.sexp of PROJECT: the one list the guard and the permission rules both
   come from. A missing file is an error, never an empty list."
  (lisp-ai-project:read-rules project "forbidden-programs"))

(defparameter *program-directories*
  '("/bin" "/usr/bin" "/usr/local/bin" "/opt/homebrew/bin")
  "Where a forbidden program may be started from by its full path. Claude Code's deny rule for the plain name does not match
   /usr/bin/sed, so each program is also denied under each of these directories. Not every place a program can live.")

(defun permission-rules (forbidden)
  "The Claude Code permission rules that deny each program in FORBIDDEN in Bash: by its name, then by its full path in each
   of *PROGRAM-DIRECTORIES*."
  (loop for program in forbidden
        append
          (cons (format nil "Bash(~a *)" program)
                (mapcar (lambda (directory) (format nil "Bash(~a/~a *)" directory program)) *program-directories*))))

(defun settings-with-deny (settings-json forbidden)
  "SETTINGS-JSON, the text of a Claude Code settings file, with permissions.deny set to exactly the rules for FORBIDDEN.
   Everything else in it is kept. Returns JSON text."
  (let* ((settings (com.inuoe.jzon:parse settings-json))
         (permissions (or (gethash "permissions" settings) (make-hash-table :test 'equal))))
    (setf (gethash "deny" permissions) (coerce (permission-rules forbidden) 'vector)
          (gethash "permissions" settings) permissions)
    (com.inuoe.jzon:stringify settings :pretty t)))

(defun sync-deny-rules (project)
  "Rewrite PROJECT's .claude/settings.json so its permissions.deny lists the rules for its
   rules/forbidden-programs.sexp. The deny list is generated: change the rules file and run this, never edit the list by
   hand. Returns the rules."
  (let* ((path (merge-pathnames ".claude/settings.json" (lisp-ai-project:project-directory project)))
         (forbidden (forbidden-programs project))
         (new (settings-with-deny (uiop/stream:read-file-string path) forbidden)))
    (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string new out)
      (terpri out))
    (permission-rules forbidden)))
