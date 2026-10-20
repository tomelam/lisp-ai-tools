**Title:** arm64: SB-DEBUG:UNWIND-TO-FRAME-AND-CALL after a trap-signalled error (TYPE-ERROR) kills the process when the thread finishes

**Summary**

In a non-main thread, a handler-bind handler for a TYPE-ERROR calls `sb-debug:unwind-to-frame-and-call` to restart the failing function's frame. The restart works and returns the right value. When the thread then finishes, the original TYPE-ERROR is signalled again, with garbage arguments, and is unhandled, so the process quits. An error raised by a call of `ERROR` instead of a trap does not do this. The same code on x86-64 does not do this.

**Environment**

macOS (Darwin 25.6.0), Apple silicon. SBCL 2.6.9 (Homebrew) and 2.4.0 (official arm64 binary) crash. SBCL 2.2.9 (official x86-64 binary, under Rosetta, same machine) does not.

**Reproduction** (SBCL only; no Slynk, no ASDF)

```lisp
;;; CASE=type  sbcl --non-interactive --no-userinit --load repro.lisp   -> crashes on arm64
;;; CASE=plain sbcl --non-interactive --no-userinit --load repro.lisp   -> control: prints ALIVE
(sb-ext:restrict-compiler-policy 'debug 3)     ; the frame needs a debug tag
(defvar *fixed* nil)
(defvar *addend* :a)
(defun f-type (x) (list :f (+ x *addend*)))                       ; TYPE-ERROR from a trap
(defun f-plain (x) (list :f (if *fixed* x (error "plain"))))      ; ERROR called

(defun frame-of (name)
  (let ((hint sb-debug:*stack-top-hint*))
    (loop for frame = (if (typep hint 'sb-di:frame) hint (sb-di:top-frame)) then (sb-di:frame-down frame)
          while frame
          when (eq (sb-di:debug-fun-name (sb-di:frame-debug-fun frame)) name) return frame)))

(defun run (fn)
  (handler-bind ((error (lambda (c)
                          (declare (ignore c))
                          (setf *fixed* t *addend* 1)             ; "fix the code"
                          (sb-debug:unwind-to-frame-and-call
                           (frame-of fn) (lambda () (declare (optimize (debug 0))) (funcall fn 5))))))
    (funcall fn 5)))

(let ((case (sb-ext:posix-getenv "CASE")))
  (sb-thread:make-thread
   (lambda () (format t "CASE ~a result: ~s~%" case (run (if (equal case "type") 'f-type 'f-plain))) (finish-output)))
  (sleep 6)                                       ; the thread has finished by now
  (format t "ALIVE after 6 s~%")
  (finish-output)
  (sb-ext:exit :code 0 :abort t))
```

**Expected:** `CASE type result: (:F 6)` then `ALIVE after 6 s`, as with CASE=plain.

**Actual (CASE=type, arm64):**

```
CASE type result: (:F 6)
Unhandled TYPE-ERROR in thread #<SB-THREAD:THREAD tid=3075 FINISHED values: NIL {...}>:
  The value :A is not of type NUMBER
Backtrace for: ...
0: (SB-KERNEL:TWO-ARG-+ 274905691546 1)
unhandled condition in --disable-debugger mode, quitting
```

Under a long-running image the same failure has also shown "Memory fault" and "maximum interrupt nesting depth (8) exceeded". The failure appears after the restarted call has returned and the thread has finished, with or without a GC in between. Calling `unwind-to-frame-and-call` from the main thread did not show it in the few cases I tried.

**Results**

| SBCL | platform | CASE=plain | CASE=type |
|---|---|---|---|
| 2.6.9 Homebrew | arm64 macOS | ok | crash |
| 2.4.0 official binary | arm64 macOS | ok | crash |
| 2.2.9 official binary | x86-64 macOS (Rosetta) | ok | ok |

Errors that leave a frame marked "escaped" (`sb-di::compiled-frame-escaped` true), like the TYPE-ERROR: unbound variable, undefined function, wrong argument count (I checked the flag only; I did not run the restart for these). Errors that leave none: `ERROR` calls, division by zero (the restart was fine for division by zero). Leaving by a plain `throw` instead of `unwind-to-frame-and-call` is fine.

**Not tried:** Linux arm64; x86-64 2.6.9; arm64 releases between 2.1.5 and 2.3.x (2.1.x runtimes will not start on this macOS: `mmap: Operation not permitted`); a self-built 2.6.9 on this machine. I searched Launchpad (all statuses) for "restart-frame" and "unwind-to-frame" and found no matching report. NEWS says 2.1.5 changed the arm64 implementation of RESTART-FRAME; I have not tested whether that is related.

**Context:** Slynk's and Sly's own `restart-frame` command call `sb-debug:unwind-to-frame-and-call` in the same way, and Sly's debugger (`r` on a frame, via a real Sly connection) shows the same crash. Tom Elam (tomelam@gmail.com) is building a debugging layer for Claude Code (an AI coding assistant) on top of Slynk's backend, so that it can fix a function and restart the failing frame in a running image; he found this bug along the way.
