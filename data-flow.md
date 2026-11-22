# Data flow: a request, its reply, and where values become text

These diagrams show how a piece of Lisp travels from Claude Code (or Tom) into a project's always-on image and how the
reply travels back. Each arrow is labelled with sample data: what actually passes along it. They answer two
questions: which code owns each step, and where values are printed, that is, turned into text.

The samples were taken on 2026-11-22 from the `omnifocus` image at tools `1c8a365`. Every reply shown (`3`,
`#1=(1 2 . #1#)`, the `ERROR:` and `HELD 1` text, `OK:`) is a real reply, shortened only where `…` says so. The JSON
around them is built from the code of `mcp-door` and `lisp`, not captured; the `id` 7 and the path `/path/to/project`
are made up. The project's side is the same in every project (its `src/server.lisp` files are near-copies), with the
project's name in place of `omnifocus`.

## How to read them

```
┌──────────────┐
│ Claude Code  │   a box with corners: something outside the image's code (a person, a program, a library)
└──────────────┘
( 4 eval-with-debugging )   a step that changes or passes on data, with who owns it beside it:
                            tools = this repo; project = the project's own code
═══ ledger ═══              data that is kept
      │ "(+ 1 2)"           an arrow; the text beside it is the data passing along it
      ▼
```

In the samples, `⏎` is a newline. A Lisp value is written as Lisp writes it (`3`, `(+ 1 2)`); a Lisp string has
double quotes (`"(+ 1 2)"`); bytes on a socket or a pipe have no quotes.

## A. A request and its values

Claude Code's tool `mcp__lisp__eval`. The shell path, `bin/lisp eval`, joins at step 2.

```
┌──────────────┐
│ Claude Code  │
└──────┬───────┘
       │ {"jsonrpc":"2.0","id":7,"method":"tools/call",
       │  "params":{"name":"eval","arguments":{"code":"(+ 1 2)"}}}        one line of JSON on stdin
       ▼
( 1 mcp-door )                         tools · mcp-door (Python); started by bin/lisp mcp
       │ (+ 1 2)                       the code's bytes, then end of input, on run/lisp.sock
       │
       │      ┌────────────────────────────────┐
       │      │ shell: bin/lisp eval '(+ 1 2)' │   the other way in: tools · lisp, which runs
       │      └──────────────┬─────────────────┘   printf '%s' "$request" | nc -U run/lisp.sock
       │                     │ (+ 1 2)
       ▼                     ▼
( 2 serve-connection )                 project · src/server.lisp; one thread per connection
       │ "(+ 1 2)"                     a Lisp string: read-all reads to end of input
       ▼
( 3 handle-request )                   project · src/server.lisp
       │ "(+ 1 2)"                     not a hook payload: (lisp-ai-guard:hook-payload-p "(+ 1 2)") ⇒ NIL
       ▼
( 4 eval-with-debugging )              tools · debug.lisp
       │   read      "(+ 1 2)"  ⇒  (+ 1 2)       a list of three objects: the symbol +, 1 and 2
       │   evaluate  (+ 1 2)    ⇒  3             a number, not text
       │   print     3          ⇒  "3⏎"          with *print-circle* on: values become text HERE
       │
       │ "3⏎"                          the reply, returned to step 3, then 2
       ▼
( 2 serve-connection )                 writes the text to the socket and closes the connection
       │ 3⏎
       ├─────────────────────────────► or, when the shell connected: nc prints  3
       ▼
( 1 mcp-door )
       │ {"jsonrpc":"2.0","id":7,"result":{"content":[{"type":"text","text":"3\n"}],"isError":false}}
       ▼
┌──────────────┐
│ Claude Code  │   shows:  3
└──────────────┘
```

A request may return more than one value, and step 4 prints one line for each. The request

```lisp
(let ((l (list 1 2))) (setf (cdr (last l)) l) (values (+ 1 2) l))
```

returns 3 and a circular list, and step 4 sends back `3⏎#1=(1 2 . #1#)⏎`. Before tools `d710471`, step 4 printed
with `*print-circle*` off, the printing of the circular list never ended, and the image died of heap exhaustion.

### What step 4 also hands on

Two observers see each request as step 4 works. Neither prints a value: they pass on text that already exists.

```
( 4 eval-with-debugging )
       │                                         │
       │ the request "(+ 1 2)", then the         │ each form and its text as typed, before and
       │ reply "3⏎": two strings                 │ after it runs: (defun fish-glyph ...), "(defun fish-glyph ...)"
       ▼                                         ▼
( echo )   tools · echo.lisp              ( record-form )   tools · bench.lisp
       │                                         │ a definition only; (+ 1 2) records nothing
       │ claude> (+ 1 2)⏎3⏎                      ▼
       ▼                                  ═══ ledger, mirrored to run/pending.sexp ═══
┌─────────────────────────┐
│ Tom's Sly REPL (Emacs)  │
└─────────────────────────┘
```

## B. A request that signals an error

The thread that evaluates the request does not unwind. It answers at once and then waits for commands, each of which
comes as a request of its own.

```
┌──────────────┐
│ Claude Code  │
└──────┬───────┘
       │ (car 'fish)                   through steps 1 to 3, as in A
       ▼
( 4 eval-with-debugging )              tools · debug.lisp, thread "omnifocus connection"
       │   evaluate (car 'fish)  ⇒  a TYPE-ERROR condition: FISH is not a LIST
       │   hold-error: print the message, the restarts and the frames, and send them NOW
       │   (through *reply-now*, which step 2 bound); then the thread waits, up to 300 s
       │
       │ ERROR: The value⏎  FISH⏎is not of type⏎  LIST⏎when binding LIST⏎
       │ HELD 1: the thread is waiting and nothing was unwound. …⏎
       │ restarts:⏎  0 ABORT-REQUEST  Unwind the request and reply with the error, …⏎ …
       │ frames:⏎  0 (CAR FISH) [external]⏎  1 … ⏎  3 (EVAL-WITH-DEBUGGING "(car 'fish)" :OMNIFOCUS)⏎ …
       ▼
┌──────────────┐
│ Claude Code  │
└──────┬───────┘
       │ (debug-abandon 1)             a new request, a new connection, a new thread
       ▼
( 4 eval-with-debugging )              tools · debug.lisp, the new thread
       │ abandon                       tells session 1's waiting thread to unwind
       ▼
( 4, session 1's thread )              unwinds; its final reply goes back as the reply to (debug-abandon 1)
       │ ERROR: The value⏎  FISH⏎is not of type⏎  LIST⏎when binding LIST⏎
       ▼
┌──────────────┐
│ Claude Code  │
└──────────────┘
```

`(debug-locals 1 0)` and `(debug-eval 1 0 'form)` go the same way: their values come back as the values of a request,
so step 4 prints them with `*print-circle*` on.

## C. The gate: a hook payload through the same socket

Before Claude Code runs a Bash, Edit, Write or MultiEdit tool call, it asks the gate. The question goes over the same
socket, and step 3 sends it elsewhere.

```
┌──────────────┐
│ Claude Code  │   about to run:  ls run
└──────┬───────┘
       │ {"session_id":"…","cwd":"/path/to/project","hook_event_name":"PreToolUse",
       │  "tool_name":"Bash","tool_input":{"command":"ls run"}}               JSON on stdin
       ▼
( bin/lisp guard )                     tools · lisp, the word guard
       │ {"lisp_ai_gate_dir":"/path/to/project","session_id":"…", …}         one field put in front
       ▼
( 2 serve-connection ) → ( 3 handle-request )        project · src/server.lisp
       │ hook-payload-p ⇒ T            so it is not evaluated as Lisp
       ▼
( socket-reply )                       tools · guard.lisp: judges by the rules files
       │ OK:                           allow: the marker and nothing after it
       │ OK:{"hookSpecificOutput":{"hookEventName":"PreToolUse",
       │     "permissionDecision":"deny","permissionDecisionReason":"…"}}     deny, and a line in run/gate.log
       ▼
( bin/lisp guard )                     drops the marker OK: and prints the rest: nothing to allow, the JSON to deny
       │ (nothing)
       ▼
┌──────────────┐
│ Claude Code  │   runs  ls run
└──────────────┘
```

A reply that does not start with `OK:` (no image, a dropped connection, an image that read the payload as Lisp) is no
answer: `bin/lisp guard` then judges in a fresh SBCL with `lisp-ai-guard:respond`, about 2 s, and denies if that fails.

## D. Tom's Sly REPL: a different path

What Tom types at his Sly REPL does not touch any step above. Sly in Emacs talks to Slynk in the image over TCP, on
127.0.0.1 at the port in the project's `rules/slynk.sexp`. Slynk, from Quicklisp, evaluates and prints with its own
settings.

```
┌─────────────────────────┐
│ Tom's Sly REPL (Emacs)  │
└──────┬──────────────────┘
       │ (car 'fish), inside a message of Sly's own protocol, over TCP
       ▼
( Slynk )                              Quicklisp's code, not this repo's
       │ the value, printed by Slynk's settings; its error goes to Sly's own debugger, not to HELD
       ▼
┌─────────────────────────┐
│ Tom's Sly REPL (Emacs)  │
└─────────────────────────┘
```

A definition typed there is not in the ledger (`bin/lisp eval` is the only way in that records). The echo of A writes
into the same REPL buffer, after `claude>`.

## Where a value becomes text

| Step | Owner | What is printed | `*print-circle*` |
| --- | --- | --- | --- |
| 4, the values of each form | tools, `debug.lisp` | every value a request returns, including those of `debug-locals` and `debug-eval` | on (since `d710471`) |
| 4, an error's message | tools, `debug.lisp`, `condition-text` | the condition, and any value inside it | on (since `d710471`) |
| 4, a held error's restarts | tools, `debug.lisp`, `held-report` | each restart's description | on (since `d710471`); no test with a circular value yet |
| 4, a held error's frames | tools, `debug.lisp` with Slynk's backtrace | each frame with its arguments | labels already shown before `d710471`; also cut at 10 items and depth 4 |
| 4, what the request's own code prints | the request (`(print l)`, `(format t "~s" l)`) | whatever that code prints | **off**: a circular value printed there still kills the image (known from where the binding sits, not tested) |
| the echo | tools, `echo.lisp` | nothing: it copies two strings that already exist | does not apply |
| `form-edit` writing a form into a file | tools, `form-edit.lisp` | the new form | does not print labels: a shared list was written as `((1 2) (1 2))` with `*print-circle*` on around the call (checked 2026-11-22) |
| Slynk answering Tom's REPL | Slynk (Quicklisp) | the value of what Tom typed | off in Slynk's print settings (`slynk:*slynk-pprint-bindings*`); a circular value there is not tested |

Very large values that do not loop are printed in full at every step of the table: nothing limits their length.
