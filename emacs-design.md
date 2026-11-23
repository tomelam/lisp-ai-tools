# The Emacs half of the tools: a design

Status: written and approved 2026-11-23, and built the same day: `lisp-ai.el`, `lisp-ai-demo.el`,
`lisp-ai-emacs-test.el`, with `aquarium` and `paip-gps` on them (their branches `emacs-layer`). Built as designed, with
these choices the design left open: a watched screen is `lisp-ai-demo` without `:caption-lines`, so both projects use
`M-x lisp-ai-demo`; the layout is given to `lisp-ai-demo-configure`, since an `M-x` command takes no arguments; the
caption buffer is `*lisp-ai demo*`.

## Why

The projects' Emacs code is copied, not shared. `aquarium/demo.el` (326 lines) and `paip-gps/demo.el` (156 lines) hold
the same functions under two prefixes, and the copies have drifted:

- Only `paip-gps` has `gps-demo--follow`, which keeps the end of the Sly REPL in view so that the echo does not land
  out of sight. `aquarium` never got that fix.
- Only `aquarium` has a reconnect command (`aquarium-demo-reconnect`).
- Both load the newest Sly found anywhere in Qlot's cache (`~/.cache/qlot/releases/.../quicklisp/*/distinfo/sly/*`),
  not the Sly of the date the project's `qlfile` pins. That is harmless while every project pins 2026-01-01 and wrong as
  soon as one moves.

Other projects need the same pieces: `omnifocus`'s release 2 includes an approval in Emacs, and `code-as-data` has none
yet. A tools repo that ships the Lisp half and not the Emacs half leaves each project to copy it again.

## What it is

Two Emacs Lisp files in this repo, reaching a project the way the Lisp code does: through the project's Qlot install,
pinned by its `qlfile.lock`.

### Layer 1: `lisp-ai.el`, for any project

Each function is shown as `C-h f` (`describe-function`) shows it: shaped like a call, with the parameters in capitals.
Parameters after `&optional` may be left out; those after `&key` may be left out and are given by name (`:flash t`).
A function with `&key` is defined with `cl-defun` (from `cl-lib`), since plain `defun` does not accept `&key`.

| Function | Does | Comes from |
| --- | --- | --- |
| `(lisp-ai-project-root &optional DIR)` | The project: the nearest directory upwards from DIR (default: the current buffer's) that holds `bin/lisp`. Never derived from a directory's name. | new |
| `(lisp-ai-load-sly ROOT)` | Load the Emacs half of the Sly the project pins: `ROOT/.qlot/dists/quicklisp/software/sly-*`, which Qlot links to that date's checkout. An error when there is none or more than one, and when another Sly is already loaded in this Emacs (as now). | both `--sly-directory`, fixed |
| `(lisp-ai-port ROOT)` | The port in `ROOT/rules/slynk.sexp`. | both `-port` |
| `(lisp-ai-connect ROOT &optional WINDOW)` | Close a leftover connection, connect Sly to `127.0.0.1` at that port, show the REPL in WINDOW. Also the reconnect after an image restart. | both setups, `aquarium-demo-reconnect` |
| `(lisp-ai-watch ROOT FILE &key FLASH ON-REVERT)` | Visit FILE, redraw it within half a second of a change on disk (polling: the files are replaced by a rename); with FLASH, bring the changed part into view and flash it; ON-REVERT, a function, for anything else (the tank's "stay at the top"). | both `--watch`, `--changed-region`, `--flash` |
| `(lisp-ai-eval-now ROOT REQUEST)` | Send REQUEST through `bin/lisp eval` and wait: `(EXIT-STATUS . OUTPUT)`. | `aquarium-demo--lisp-now` |
| `(lisp-ai-eval-async ROOT REQUESTS DONE)` | Send REQUESTS one after another without blocking Emacs; call DONE with nil, or with the failing reply. A reply with `HELD n` is abandoned first (`(debug-abandon n)`), so the image is not left waiting. | `aquarium-demo--send` |

`lisp-ai-connect` also puts two fixes in place in the REPL buffer (from `--give-return-back-to-sly` and
`gps-demo--follow`): RET sends the input to the image even when paredit is on, and the end of the REPL stays in view as
the image writes to it.

Requests go through `bin/lisp eval`, not through Sly (decided 2026-11-23): that path already gives held errors, the
ledger and the echo, and a request sent over Sly would need its own thread to hold an error. `data-flow.md` gains a
diagram of it.

### Layer 2: `lisp-ai-demo.el`, a stepped demo

`aquarium`'s stepping engine, made general: a caption window, SPACE for the next step, `q` to end, steps read from
`demo/steps.sexp` (`:intro`, `:end`, `:steps` of `(:do :send :look)`, as now). What is particular to `aquarium` moves
into its steps file as data:

| New key in `demo/steps.sexp` | `aquarium`'s value (from today's code) |
| --- | --- |
| `:file` | `"src/aquarium.lisp"`: the file the demo rewrites; the demo refuses to start while it has uncommitted changes or the image holds unsaved definitions, and `q` half way puts it back with `git restore` |
| `:start` | `("(reset-demo)")` |
| `:quit` | the `(progn (forget-all) (load "src/aquarium.lisp") ...)` request of `aquarium-demo-quit` |

The layout stays in the project's `demo.el`, as arguments: the source file, an optional file shown above the REPL with
its height (the tank, 14 lines), and the caption window's height. A project's `demo.el` becomes about 20 lines: find
the tools in the Qlot install, load the two files, call `lisp-ai-demo` with the layout.

### How a project's `demo.el` finds them

`ROOT/.qlot/dists/lisp-ai-tools/distinfo.txt` names the current commit on its `version:` line (`git-<commit>`), and the
files are in `ROOT/.qlot/dists/lisp-ai-tools/software/lisp-ai-tools-ref-<commit>/`. The distinfo is needed because a
Qlot update leaves the old releases in `software/` (18 of them in `aquarium` today). These few lines stay in each
`demo.el`, since they run before `lisp-ai.el` is loaded. `bin/demo` does not change.

## Testing

- A shared test file, `lisp-ai-emacs-test.el`, run by hand with batch Emacs as the `demo-test.el` files are now:
  `Emacs --batch -Q -l tests/demo-test.el`, each project's file calling the shared checks.
- From clean clones in `~/tmp/e2e/`, with fresh SBCLs, Slynk ports 4206 and up, and a real Sly connection; the clones
  are deleted afterwards; Tom's Emacs and the running images are not touched.
- Each new check broken once and watched failing. The two demos are played through whole in the clones, as
  `aquarium`'s test already does.
- The `aquarium` result to compare against: today's 49 PASS on a fresh image (and the known step-6 failure on a second
  run, a smaller item of its own).

## Order of work

1. Tools branch `emacs-layer`: the two files, the shared tests, the README, diagram E in `data-flow.md`.
2. `aquarium` branch: pin the new tools, `demo.el` cut down to its layout, the new keys in `demo/steps.sexp`.
3. `paip-gps` branch: the same; its demo stays a screen to watch (Tom, 2026-11-23): the source, and the REPL with the
   echo, with no captions and no steps, so what happens is done by someone else, a Claude session through `bin/lisp
   eval` or the programmer at the REPL. A set of scripted steps for it is an open item, not part of this work.
4. Tested from clean clones of both; merged one at a time, asking before each merge.

## Not in it

- Requests over Sly (above).
- A command that sends the form at point: Sly's own `C-c C-c` does that, through Slynk, without the ledger.
- `omnifocus`'s approval in Emacs: release 2 will build on layer 1.
- The gate: `.el` files are not Lisp by `rules/lisp-files.sexp` (suffixes `lisp` and `asd`), so they are edited as
  text, as the `demo.el` files are now. Emacs Lisp runs in Emacs, not in the image, so the workbench cannot save it.

## Tom's answers (2026-11-23)

1. `paip-gps` keeps its demo as a screen to watch for now; scripted steps like `aquarium`'s (captions, SPACE and `q`,
   its own `demo/steps.sexp`) are to come later, as an open item.
2. The names stand: the prefix `lisp-ai-` for the functions, and `M-x lisp-ai-demo` and `M-x lisp-ai-reconnect` as the
   commands, in place of `M-x aquarium-demo` and `M-x gps-demo`.
