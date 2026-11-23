# The Emacs half of the tools: a design

Status: written 2026-11-23, waiting for approval. Nothing here is built.

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

| Function | Does | Comes from |
| --- | --- | --- |
| `lisp-ai-project-root (&optional dir)` | The project: the nearest directory upwards that holds `bin/lisp`. Never derived from a directory's name. | new |
| `lisp-ai-load-sly (root)` | Load the Emacs half of the Sly the project pins: `ROOT/.qlot/dists/quicklisp/software/sly-*`, which Qlot links to that date's checkout. An error when there is none or more than one, and when another Sly is already loaded in this Emacs (as now). | both `--sly-directory`, fixed |
| `lisp-ai-port (root)` | The port in `ROOT/rules/slynk.sexp`. | both `-port` |
| `lisp-ai-connect (root &optional window)` | Close a leftover connection, connect Sly to `127.0.0.1` at that port, show the REPL in WINDOW. Also the reconnect after an image restart. | both setups, `aquarium-demo-reconnect` |
| (installed by `lisp-ai-connect`) | In the REPL: RET sends input even under paredit; the end of the REPL stays in view. | `--give-return-back-to-sly`, `gps-demo--follow` |
| `lisp-ai-watch (root file &key flash on-revert)` | Visit FILE, redraw it within half a second of a change on disk (polling: the files are replaced by a rename); with FLASH, bring the changed part into view and flash it; ON-REVERT for anything else (the tank's "stay at the top"). | both `--watch`, `--changed-region`, `--flash` |
| `lisp-ai-eval-now (root request)` | Send REQUEST through `bin/lisp eval` and wait: `(EXIT-STATUS . OUTPUT)`. | `aquarium-demo--lisp-now` |
| `lisp-ai-eval-async (root requests done)` | Send REQUESTS one after another without blocking Emacs; call DONE with nil, or with the failing reply. A reply with `HELD n` is abandoned first (`(debug-abandon n)`), so the image is not left waiting. | `aquarium-demo--send` |

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
3. `paip-gps` branch: the same; its demo stays screen-only (no steps), as today, unless Tom wants steps.
4. Tested from clean clones of both; merged one at a time, asking before each merge.

## Not in it

- Requests over Sly (above).
- A command that sends the form at point: Sly's own `C-c C-c` does that, through Slynk, without the ledger.
- `omnifocus`'s approval in Emacs: release 2 will build on layer 1.
- The gate: `.el` files are not Lisp by `rules/lisp-files.sexp` (suffixes `lisp` and `asd`), so they are edited as
  text, as the `demo.el` files are now. Emacs Lisp runs in Emacs, not in the image, so the workbench cannot save it.

## Questions for Tom

1. `paip-gps`: keep its demo screen-only, or give it steps?
2. Names: the prefix `lisp-ai-` for the functions, and `M-x lisp-ai-demo` and `M-x lisp-ai-reconnect` as the commands,
   in place of `M-x aquarium-demo` and `M-x gps-demo`?
