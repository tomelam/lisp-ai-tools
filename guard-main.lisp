;;;; Loaded by tools/guard after lisp-ai-tools/hook, in the project's directory. Reads the hook payload on stdin and answers on
;;;; stdout. Allow: print nothing. Refuse: print the Claude Code PreToolUse "deny" decision with the reason.
(write-string (lisp-ai-hook:respond (uiop:getcwd) (uiop:slurp-input-stream :string *standard-input*)))
