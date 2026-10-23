;;;; The gate: what a Claude Code tool call does to Lisp. One PreToolUse hook, for Bash and for the Edit, MultiEdit and
;;;; Write tools (bin/lisp guard). Lisp is changed in the image and saved from it (lisp-ai-bench), so the gate refuses
;;;; every other way of changing a Lisp file: Edit and MultiEdit of one, Write over one that exists, a shell command that
;;;; writes one (a redirection, tee, cp, mv, install, ln, rsync, dd), and a text-processing program (rules/
;;;; forbidden-programs.sexp) used on Lisp (a file with a Lisp suffix, or a path through a Lisp directory:
;;;; rules/lisp-files.sexp). Everything else is allowed: wc on a log, jq on JSON, grep on markdown. Each denial is one
;;;; line in the project's run/gate.log; nothing else is recorded and there are no switches.
;;;; Pure functions over strings, except FORBIDDEN-PROGRAMS and LISP-FILES (a project's rules), FILE-EDIT-REFUSAL (does a
;;;; file exist) and LOG-DENIAL. The script lisp (word guard) is the only place that touches stdin and exit codes.
(defpackage :lisp-ai-guard
  (:use :cl)
  (:export #:forbidden-uses
           #:command-programs
           #:lisp-names
           #:written-lisp-files
           #:file-edit-refusal
           #:hook-verdict
           #:hook-response
           #:respond
           #:deny-json
           #:forbidden-programs
           #:lisp-files
           #:hook-payload-p
           #:socket-reply
           #:socket-marker
           #:*gate-log*))

(in-package :lisp-ai-guard)

;;; Which programs a shell command runs

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

(defun forbidden-uses (command forbidden)
  "The programs of COMMAND (a shell command string) that are in the list FORBIDDEN, in order of appearance."
  (remove-if-not (lambda (program) (member program forbidden :test #'string=)) (command-programs command)))

;;; What counts as Lisp, and whether a command names or writes it

(defun lisp-files (project)
  "What counts as Lisp for the gate: the one form in rules/lisp-files.sexp of PROJECT, a plist
   (:code-suffixes (\"lisp\" \"asd\") :data-suffixes (\"sexp\") :directories (\"src\" \"tests\" \"tools\")). Code is what
   no tool but the workbench may change; code and data are what a text-processing program may not read or change. A
   missing file is an error, never an empty rule."
  (lisp-ai-project:read-rules project "lisp-files"))

(defun command-tokens (command)
  "The words of COMMAND, a shell command string, split at blanks, quotes and shell punctuation, here-document bodies left
   out. Scripts given to sh -c, and file names after a redirection, come out as words too."
  (remove ""
          (uiop:split-string (strip-heredoc-bodies command)
                             :separator '(#\Space #\Tab #\Newline #\; #\| #\& #\( #\) #\< #\> #\` #\' #\" #\=))
          :test #'string=))

(defun path-type (path)
  "The suffix after the last dot of the last component of PATH, or NIL when there is none or the name only starts with a dot."
  (let* ((name (subseq path (1+ (or (position #\/ path :from-end t) -1)))) (dot (position #\. name :from-end t)))
    (and dot (plusp dot) (subseq name (1+ dot)))))

(defun code-file-p (token lisp-files)
  "True when TOKEN names a file with a code suffix of LISP-FILES (a .lisp or .asd)."
  (and (member (path-type token) (getf lisp-files :code-suffixes) :test #'equal) t))

(defun path-components (token)
  (remove "" (uiop:split-string token :separator "/") :test #'string=))

(defun in-lisp-directory-p (token lisp-files)
  (and (intersection (path-components token) (getf lisp-files :directories) :test #'string=) t))

(defun token-names-lisp-p (token lisp-files)
  "True when TOKEN, a word of a command, names Lisp: a file with a code or data suffix of LISP-FILES, or a path with one
   of its directories as a component."
  (let ((type (path-type token)))
    (or (and type
             (member type (append (getf lisp-files :code-suffixes) (getf lisp-files :data-suffixes)) :test #'string=)
             t)
        (in-lisp-directory-p token lisp-files))))

(defun lisp-names (command lisp-files)
  "The words of COMMAND that name Lisp (token-names-lisp-p), in order, each once."
  (remove-duplicates (remove-if-not (lambda (token) (token-names-lisp-p token lisp-files)) (command-tokens command))
                     :test #'string= :from-end t))

(defparameter *file-writers* '("cp" "mv" "install" "ln" "rsync")
  "Programs whose last word names the file or directory they write.")

(defun option-p (word) (and (plusp (length word)) (char= (char word 0) #\-)))

(defun redirection-targets (words)
  "The files that the redirections among WORDS write: the rest of a word that starts with > (after an optional stream
   number), or the word after a bare > or >>. 2>&1 and the like name no file."
  (let ((targets '()))
    (loop while words
          do (let* ((word (pop words))
                    (start (position-if-not #'digit-char-p word)))
               (when (and start (char= (char word start) #\>))
                 (let ((rest (string-left-trim ">" (subseq word start))))
                   (cond ((and (plusp (length rest)) (char/= (char rest 0) #\&)) (push rest targets))
                         ((and (zerop (length rest)) words (plusp (length (first words)))
                               (char/= (char (first words) 0) #\&))
                          (push (pop words) targets)))))))
    (nreverse targets)))

(defun writer-targets (words lisp-files)
  "The Lisp code files a simple command writes with a program: tee (every file named), dd (of=), or cp, mv, install, ln
   and rsync (the last word, or, when that is a Lisp directory and a source is a Lisp file, the file that lands in it)."
  (multiple-value-bind (base rest) (unwrap words)
    (let ((files (remove-if #'option-p rest)))
      (cond ((null base) '())
            ((string= base "tee") (remove-if-not (lambda (w) (code-file-p w lisp-files)) files))
            ((string= base "dd")
             (loop for w in rest
                   when (and (> (length w) 3) (string= "of=" w :end2 3) (code-file-p (subseq w 3) lisp-files))
                   collect (subseq w 3)))
            ((member base *file-writers* :test #'string=)
             (let ((destination (car (last files))) (sources (butlast files)))
               (cond ((null destination) '())
                     ((code-file-p destination lisp-files) (list destination))
                     ((and (in-lisp-directory-p destination lisp-files)
                           (some (lambda (s) (code-file-p s lisp-files)) sources))
                      (list (format nil "~a/~a" (string-right-trim "/" destination)
                                    (base-name (find-if (lambda (s) (code-file-p s lisp-files)) sources)))))
                     (t '()))))
            (t '())))))

(defun written-lisp-files (command lisp-files)
  "The Lisp code files that COMMAND would write from the shell: by a redirection, or by tee, cp, mv, install, ln, rsync
   or dd."
  (remove-duplicates
   (loop for words in (split-simple-commands command)
         append (remove-if-not (lambda (target) (code-file-p target lisp-files)) (redirection-targets words))
         append (writer-targets words lisp-files))
   :test #'string= :from-end t))

;;; The verdicts

(defparameter *file-edit-tools* '("Edit" "MultiEdit" "Write")
  "The Claude Code tools that change a file by its path (tool_input.file_path).")

(defun workbench-advice (file)
  (format nil "Lisp is changed in the image and saved from it: define it with bin/lisp eval '(defun ...)', test it there, then (save 'name :file ~s)." file))

(defun file-edit-refusal (tool path lisp-files)
  "NIL if the call of TOOL on PATH may proceed, else the refusal message. Edit and MultiEdit may not change a file with a
   code suffix of LISP-FILES, Write may not replace one; Write may create one that does not exist. A call with no path
   cannot be judged and is refused."
  (cond ((not (member tool *file-edit-tools* :test #'string=)) nil)
        ((not (stringp path))
         (format nil "bin/lisp guard: the ~a call has no file_path, so it cannot be judged: refused." tool))
        ((not (code-file-p path lisp-files)) nil)
        ((and (string= tool "Write") (not (probe-file path))) nil)
        (t (format nil "Blocked: ~a on the Lisp file ~a. ~a The Write tool may only create a Lisp file that does not exist yet."
                   tool path (workbench-advice path)))))

(defun refusal-message (uses names forbidden)
  (let ((distinct (remove-duplicates uses :test #'string= :from-end t)))
    (format nil "Blocked: ~{~a~^, ~} ~:[is a text-processing program~;are text-processing programs~], used here on Lisp (~{~a~^, ~}). Lisp is read and changed in the image: bin/lisp eval, describe, who-calls, form-edit. On files that are not Lisp these programs are allowed. Forbidden on Lisp: ~{~a~^ ~}."
            distinct (rest distinct) names forbidden)))

(defun writer-message (files)
  (format nil "Blocked: this command would write the Lisp file~p ~{~a~^, ~} from the shell. ~a"
          (length files) files (workbench-advice (first files))))

(defun hook-verdict (hook-json forbidden lisp-files)
  "NIL if the Claude Code hook payload HOOK-JSON may proceed, else the refusal message. A Bash call is refused when it
   would write a Lisp code file, or runs a program of FORBIDDEN and also names Lisp (LISP-FILES); a call of Edit,
   MultiEdit or Write is judged by FILE-EDIT-REFUSAL. Other tools are not judged. A payload that cannot be read is refused."
  (let ((payload (handler-case (com.inuoe.jzon:parse hook-json) (error () nil))))
    (if (not (hash-table-p payload))
        "bin/lisp guard could not read the hook payload as JSON, so it refuses the call."
        (let* ((tool (gethash "tool_name" payload))
               (input (let ((input (gethash "tool_input" payload))) (and (hash-table-p input) input))))
          (cond ((equal tool "Bash")
                 (let ((command (and input (gethash "command" input))))
                   (when (stringp command)
                     (let ((written (written-lisp-files command lisp-files)))
                       (if written
                           (writer-message written)
                           (let* ((uses (forbidden-uses command forbidden))
                                  (names (and uses (lisp-names command lisp-files))))
                             (when (and uses names) (refusal-message uses names forbidden))))))))
                ((member tool *file-edit-tools* :test #'equal)
                 (file-edit-refusal tool (and input (gethash "file_path" input)) lisp-files)))))))

(defun deny-json (reason)
  "The Claude Code PreToolUse decision that refuses a tool call, with REASON, as a JSON string."
  (let ((decision (make-hash-table :test 'equal))
        (details (make-hash-table :test 'equal)))
    (setf (gethash "hookEventName" details) "PreToolUse"
          (gethash "permissionDecision" details) "deny"
          (gethash "permissionDecisionReason" details) reason
          (gethash "hookSpecificOutput" decision) details)
    (com.inuoe.jzon:stringify decision)))

(defun forbidden-programs (project)
  "The programs named in rules/forbidden-programs.sexp of PROJECT. A missing file is an error, never an empty list."
  (lisp-ai-project:read-rules project "forbidden-programs"))

;;; The gate log: one line per denial, in the project's run/gate.log

(defvar *gate-log* "run/gate.log"
  "Where a denial is written, relative to the project's directory. NIL writes nothing.")

(defun log-denial (project hook-json reason)
  "Append one line to PROJECT's gate log: the time, the tool, the command or file, and REASON. A failure to log never
   changes the verdict."
  (when (and project *gate-log*)
    (ignore-errors
     (let* ((payload (com.inuoe.jzon:parse hook-json))
            (input (gethash "tool_input" payload))
            (what (and (hash-table-p input) (or (gethash "command" input) (gethash "file_path" input))))
            (file (merge-pathnames *gate-log* (lisp-ai-project:project-directory project))))
       (ensure-directories-exist file)
       (with-open-file (out file :direction :output :if-exists :append :if-does-not-exist :create
                                 :external-format :utf-8)
         (multiple-value-bind (s m h day month year) (get-decoded-time)
           (format out "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d~c~a~c~s~c~a~%" year month day h m s #\Tab
                   (gethash "tool_name" payload) #\Tab (if (stringp what) (substitute #\Space #\Newline what) "")
                   #\Tab reason)))))))

(defun hook-response (hook-json forbidden lisp-files &optional project)
  "What the hook prints for HOOK-JSON: the empty string to allow, a deny decision to refuse. A denial is logged in
   PROJECT's gate log when PROJECT is given."
  (let ((refusal (hook-verdict hook-json forbidden lisp-files)))
    (cond (refusal (log-denial project hook-json refusal) (deny-json refusal))
          (t ""))))

(defun respond (project hook-json)
  "HOOK-RESPONSE for HOOK-JSON with the forbidden programs and the Lisp files read from PROJECT's rules."
  (hook-response hook-json (forbidden-programs project) (lisp-files project) project))

;;; The always-on image answers hook payloads on its socket, which bin/lisp guard asks first. A reply that is empty or has
;;; no marker is not an answer (a dropped connection, a server that read the payload as Lisp), so the script then takes
;;; the slow path instead of reading silence as permission.

(defparameter *socket-marker* "OK:" "What a reply from the socket starts with when the gate judged the payload.")

(defun socket-marker () *socket-marker*)

(defun hook-payload-p (request)
  "True when the text REQUEST is a Claude Code hook payload (JSON, which starts with a brace) and not Lisp."
  (let ((start (position-if-not (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return))) request)))
    (and start (char= (char request start) #\{))))

(defun socket-reply (request forbidden project)
  "The reply to a hook payload REQUEST on the socket: the marker, then HOOK-RESPONSE with PROJECT's rules. FORBIDDEN is
   the list of programs the server was started with."
  (concatenate 'string *socket-marker* (hook-response request forbidden (lisp-files project) project)))
