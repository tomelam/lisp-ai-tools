#!/bin/sh
# lisp SYSTEM WORD [ARGS...] -- the one way into a project's Lisp, for Claude Code and for Tom. A project's bin/lisp changes to
# the project's directory (the current directory is the project) and runs this script, which Qlot installed inside the project
# (.qlot/dists/lisp-ai-tools/software/), with the name of the project's ASDF system. Nothing here derives a name or a path
# from a directory name; shared files are taken from the directory of this script.
#   eval '(form)'     evaluate in the project's always-on image, started first when it is down; print the values
#   start|stop|status the always-on image (loads bin/serve.lisp, answers on run/lisp.sock)
#   test              load the system fresh and run ASDF test-op on SYSTEM/tests
#   guard             Claude Code PreToolUse hook for Bash, Edit, Write and MultiEdit: the gate (lisp-ai-tools/guard)
#   qlot ARGS...      Qlot with the setup file that pins cl+ssl to openssl@3 (needed to install or update, not to run)
# Everything else is the project's own: bin/serve.lisp. Qlot is $HOME/.qlot/bin/qlot, or $QLOT.
system=$1
word=$2
if [ -z "$system" ] || [ -z "$word" ]; then echo "usage: bin/lisp WORD [ARGS...]" >&2; exit 2; fi
shift 2
tools=$(cd "$(dirname "$0")" && pwd)
qlot=${QLOT:-$HOME/.qlot/bin/qlot}
socket=run/lisp.sock
pidfile=run/lisp.pid
mkdir -p run

# alive = the pid exists and the socket answers a real request (macOS nc has no -z for Unix sockets)
running() { [ -f "$pidfile" ] && kill -0 "$(cat "$pidfile")" 2>/dev/null && [ "$(printf T | nc -w 2 -U "$socket" 2>/dev/null)" = "T" ]; }

# --userinit mutes redefinition warnings about libraries only (UIOP, ASDF, Slynk, Eclector): quiet-init.lisp
start_server() {
  if running; then echo "lisp-server already running (pid $(cat "$pidfile"))"; return 0; fi
  [ -f "$tools/quiet-init.lisp" ] || { echo "lisp: $tools/quiet-init.lisp is missing" >&2; return 1; }
  rm -f run/*.sock "$pidfile"
  nohup "$qlot" exec sbcl --noinform --userinit "$tools/quiet-init.lisp" --non-interactive \
    --eval "(asdf:load-asd (truename \"$system.asd\"))" \
    --load bin/serve.lisp > run/lisp-server.log 2>&1 &
  child=$!
  waited=0
  while ! running; do
    # a server that has already exited will never come up: say so now, not after the 60 s
    if ! kill -0 "$child" 2>/dev/null; then echo "lisp-server exited before it came up; see run/lisp-server.log" >&2; return 1; fi
    sleep 0.5
    waited=$((waited + 1))
    if [ "$waited" -ge 120 ]; then echo "lisp-server did not come up in 60 s; see run/lisp-server.log" >&2; return 1; fi
  done
  echo "lisp-server started (pid $(cat "$pidfile"))"
  # Slynk failing to start does not stop the server (it is useful without Sly), but it must not be silent
  case "$(cat run/lisp-server.log)" in
    *"Slynk NOT started"*)
      echo "WARNING: Slynk did not start, so Sly cannot connect: see run/lisp-server.log (the port in rules/slynk.sexp is probably taken)" >&2
      ;;
  esac
}

status_server() {
  if running; then echo "running (pid $(cat "$pidfile"))"; else echo "not running"; return 1; fi
}

stop_server() {
  if [ -f "$pidfile" ]; then
    pid=$(cat "$pidfile")
    kill "$pid" 2>/dev/null
    # wait for it to go: a start straight after a stop must not find the old image still holding the Slynk port.
    # SBCL gives its threads up to 60 s to stop after the signal, so a live image can take about that long.
    waited=0
    while kill -0 "$pid" 2>/dev/null; do
      sleep 0.2
      waited=$((waited + 1))
      if [ "$waited" -ge 350 ]; then echo "lisp-server (pid $pid) did not exit within 70 s" >&2; return 1; fi
    done
  fi
  rm -f run/*.sock "$pidfile"
  echo "lisp-server stopped"
}

case $word in
  eval)
    # A server that is down (a stale socket counts as down) is started first, and stderr says so.
    if ! state=$(status_server 2>&1); then
      echo "lisp-server was down ($state); starting it" >&2
      start_server >&2 || { echo "lisp-server could not be started; see run/lisp-server.log" >&2; exit 1; }
    fi
    printf '%s' "$1" | nc -w 300 -U "$socket" || { echo "no answer from lisp-server on $socket" >&2; exit 1; }
    ;;
  start) start_server ;;
  stop) stop_server ;;
  status) status_server ;;
  test)
    # The loader's redefinition warnings go to run/test.stderr; a load or test error is printed on stdout.
    # The compiler's own progress lines are off, so stdout holds only what needs reading.
    exec "$qlot" exec sbcl --noinform --userinit "$tools/quiet-init.lisp" --non-interactive \
      --eval "(asdf:load-asd (truename \"$system.asd\"))" \
      --eval "(let ((*compile-verbose* nil) (*compile-print* nil) (*load-verbose* nil))
                (handler-case (progn (asdf:load-system \"$system\" :force t) (asdf:test-system \"$system/tests\"))
                  (error (e) (format t \"TEST RUN FAILED: ~a~%\" e) (uiop:quit 2))))" \
      2>run/test.stderr
    ;;
  guard)
    # Reads the hook JSON on stdin. Prints a "deny" decision on stdout, or nothing to allow.
    # Fast path: ask the always-on image. Slow path, a fresh SBCL, when it is not running, so enforcement never depends on
    # the server. If the guard itself cannot run it denies: a guard that fails open looks exactly like one that works.
    socket=${LISP_AI_SOCKET:-run/lisp.sock}
    if [ -S "$socket" ]; then
      # no exec: a stale socket file (the server died) makes nc fail with no output, and no output means "allow".
      # The server's reply counts only when it starts with OK: (lisp-ai-hook:socket-marker); the rest is the hook's answer.
      # Anything else (nc cannot reach the server, a dropped connection, a server that does not know hook payloads and
      # read it as Lisp) takes the slow path.
      input=$(cat)
      if reply=$(printf '%s' "$input" | nc -w 10 -U "$socket"); then
        case $reply in
          OK:*) printf '%s' "${reply#OK:}"; exit 0 ;;
        esac
      fi
      printf '%s' "$input" > run/guard-input.json
      exec < run/guard-input.json
    fi
    "$qlot" exec sbcl --noinform --non-interactive \
      --eval "(let ((*standard-output* *error-output*))
                (asdf:load-asd (truename \"$system.asd\"))
                (asdf:load-system \"lisp-ai-tools/guard\"))" \
      --load "$tools/guard-main.lisp" 2>run/guard.stderr
    status=$?
    if [ "$status" -ne 0 ]; then
      echo '{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"deny","permissionDecisionReason":"bin/lisp guard failed to run (exit status non-zero); see run/guard.stderr"}}'
    fi
    exit 0
    ;;
  qlot)
    QLOT_SETUP_FILE="$tools/qlot-setup.lisp" exec "$qlot" "$@"
    ;;
  *)
    echo "usage: bin/lisp eval|start|stop|status|test|guard|qlot ..." >&2
    exit 2
    ;;
esac
