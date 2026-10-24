# lisp-ai-tools

The shared tooling that lets [Claude Code](https://claude.com/claude-code) work on a Common Lisp project the way a Lisp
programmer does: **inside a running SBCL image, not by editing text.** A project keeps one always-on image. Claude and
the human both talk to it. A definition is made in the image, tested there, and only then written to its source file,
as forms, with its comments and docstring intact. The source file is a record of the image, written out on purpose.

Built for the projects under Tom Elam's `~/lisp-ai/` tree (SBCL 2.6.9, macOS arm64, Sly/Slynk, Qlot). Everything here
is shared by every project; a project installs it with Qlot and uses it as it is.

## The idea in one picture

```
  Claude Code
       │  bin/lisp eval '(defun fish-glyph ...)'
       ▼
  ┌────┴───────────────────────────────────────┐
  │ the always-on image (SBCL, run/lisp.sock)  │
  │                                            │
  │ holds the definition, and a ledger entry:  │
  │ kind, name, package, the text as typed     │
  └────┬───────────────────────────────────────┘
       │  (pending)  (run-tests)  (where 'name)
       ▼
  (save 'fish-glyph)
       │  the text as typed, spliced into src/aquarium.lisp
       ▼
  bin/lisp test
       │  a fresh SBCL loads the files: the arbiter
       ▼
  commit
```

Two pieces make this the easy path rather than a rule to remember:

- **The workbench** records every definition made through `bin/lisp eval` and saves it to a file on request.
- **The gate** refuses every other way of changing a Lisp file, and leaves everything that is not Lisp alone.

## The systems

All in `lisp-ai-tools.asd`. A project names the ones it uses in its own `.asd`.

| System | Package | File | What it does |
| --- | --- | --- | --- |
| `lisp-ai-tools` | `form-edit` | `form-edit.lisp` | Changes a Lisp file **as forms**: replace, insert, append or delete a top-level form by name, or one subform inside a form; every other byte stays as it was. Before writing it checks that the new forms read back as the forms meant; after writing it reads the file back. It recompiles the written `defun`s from the file with their position, so `M-.` finds them. Uses [Eclector](https://github.com/s-expressionists/Eclector) for subform spans. |
| `lisp-ai-tools/project` | `lisp-ai-project` | `project.lisp` | Where a project's `rules/` and `data/` are, and how its `.sexp` data files are read. |
| `lisp-ai-tools/debug` | `lisp-ai-debug` | `debug.lisp` | **Debugging in place.** An error in a `bin/lisp eval` request is not unwound: the reply comes at once as `HELD n` with the restarts and the frames, and the failing thread waits. Commands: `debug-locals`, `debug-eval`, `debug-restart-frame`, `debug-invoke`, `debug-abandon`. Also the eval loop, with the observer the workbench records through. |
| `lisp-ai-tools/bench` | `lisp-ai-bench` | `bench.lisp` | **The workbench.** The ledger of definitions made in the image, with the text as typed; `pending`, `save`, `save-all`, `drop`, `where`, `run-tests`, `new-file`, `add-component`. |
| `lisp-ai-tools/guard` | `lisp-ai-guard` | `guard.lisp` | **The gate.** The one Claude Code PreToolUse hook, for `Bash` and for `Edit`, `Write` and `MultiEdit`. The image answers hook payloads on its socket with the marker `OK:`. |
| `lisp-ai-tools/tests` | one package per file | `*-test.lisp` | The tests of all the above. A project runs them from its own suite by calling each package's `run`. |

Beside them: `lisp`, the one shell script a project's `bin/lisp` runs; `quiet-init.lisp`, SBCL's `--userinit` that
mutes redefinition warnings about the libraries only; `qlot-setup.lisp`, Qlot's setup file pinning `cl+ssl` to
openssl@3 on this Mac; `rules/`, the two rules files the gate reads, which a project copies; `qlfile`, so that the
repo can run its own gate (below); and `sbcl-restart-frame-bug.md`, a report of an SBCL arm64 fault found on the way.

## How a change flows

1. **Define it in the image.** `bin/lisp eval '(defun fish-glyph (f) ...)'`, read in the project's package. Comments
   typed above and inside the form travel with it: the ledger keeps the request text, not only the form.
2. **Look.** `(pending)` lists what the image holds that no file holds, as `(kind name file)`. `(where 'name)` says
   which file and offset the image records for a name. `bin/lisp status` shows the unsaved count.
3. **Test it there.** `(run-tests)` loads the project's test system inside the image, without reloading source files
   unchanged on disk, so unsaved definitions survive, and runs the suite against what the image holds. It returns the
   failure count. `trace`, `describe`, `inspect`, xref and the `HELD` commands apply as usual.
4. **Save it.** `(save 'fish-glyph)` writes the text as typed into the file the image knows for it; `(save 'new-fn
   :file "src/x.lisp" :after "OTHER-FN")` places a new one; `(save-all)` saves everything pending, oldest first. A save
   that would drop a comment or a docstring the file's definition already has is refused and names it;
   `:drop-comments t` or `:drop-docstring t` says it is meant. New files: `(new-file "src/y.lisp")` and
   `(add-component "y" :after "x")`, which edits the `.asd` as forms.
5. **Prove it from the files.** `bin/lisp test` loads the files into a fresh SBCL and runs the suite. Then commit.

`bin/lisp stop` refuses while anything is unsaved and prints the names; `stop --discard` throws them away on purpose.
The ledger is mirrored to `run/pending.sexp` after every change, so a crash still leaves the list of what was lost.

What the workbench refuses, and why: a save into a file that does not exist (`new-file` first); of a definition read in
another package than the file's `in-package`; of one made in the `scratch` package; an anchor that is not exactly one
form of the file. Known limits: a comment after a form's closing parenthesis on the same line is not captured; a
definition typed at the Sly REPL is not in the ledger, only requests through `bin/lisp eval` are.

## What the gate refuses

What counts as Lisp is data in each project's `rules/lisp-files.sexp`:

```lisp
(:code-suffixes ("lisp" "asd") :data-suffixes ("sexp") :directories ("src" "tests" "tools"))
```

| Refused | Allowed |
| --- | --- |
| Edit or MultiEdit of a `.lisp` or `.asd` file | Edit of prose, JSON, settings, `rules/*.sexp` |
| Write over a Lisp file that exists | Write of a new Lisp file (`new-file` is the better way) |
| `>`, `>>`, `tee`, `cp`, `mv`, `install`, `ln`, `rsync`, `dd` into a Lisp file, whatever runs them | The same into anything else |
| A program of `rules/forbidden-programs.sexp` (`cat`, `sed`, `grep`, `awk`, `wc`, `python`, ...) **on Lisp** | The same programs on a log, JSON, markdown |
| A hook payload it cannot read, or an Edit with no `file_path` | |

The gate looks through `sudo`, `env`, `nice`, `timeout`, `xargs`, `sh -c`, `eval`, `find -exec`, shell keywords and
`$(...)` inside double quotes. Every denial is one line in the project's `run/gate.log`: time, tool, what was refused,
why. The messages say what to do instead. There are no deny lists, no switches, and nothing else is recorded. The gate
catches habit, not intent: a Lisp form can still run a program, so the rule has to hold without it.

## `bin/lisp`

A project's `bin/lisp` is eight lines: it changes to the project's directory and runs the script `lisp` from this repo,
as Qlot installed it inside the project, with the name of the project's ASDF system. Nothing derives a name or a path
from a directory name, so a project clones anywhere.

| Word | Does |
| --- | --- |
| `eval '(form)'` | Evaluate in the always-on image, in the project's package; print the values. Starts the image if it is down. One form per call. |
| `start` / `stop` / `status` | The always-on image: loads the project's `bin/serve.lisp`, answers on `run/lisp.sock`, serves Slynk. `stop` refuses while anything is unsaved and waits until the process has exited. |
| `test` | Load the system fresh in a new SBCL and run `test-op` on `<name>/tests`. The arbiter. The loader's warnings go to `run/test.stderr`. |
| `guard` | The hook. Reads the hook JSON on stdin, asks the image first (about 30 ms), judges in a fresh SBCL when the image does not answer (about 2 s), denies if that fails. |
| `qlot ARGS...` | Qlot with `qlot-setup.lisp`. Needed to install or update, not to run. |

## Using it in a project

A project gets the tools through Qlot, pinned to a commit:

```
# qlfile
ql :all 2026-01-01
git lisp-ai-tools https://github.com/tomelam/lisp-ai-tools.git :branch master
```

`qlot install` puts the repo inside the project at `.qlot/dists/lisp-ai-tools/software/lisp-ai-tools-ref-<commit>/`
with the libraries its systems name, and `qlfile.lock` pins the commit. A change here reaches a project only after
`bin/lisp qlot update lisp-ai-tools` there and a commit of the new lock file. The first install in a fresh clone, before
`bin/lisp` exists:

```sh
QLOT_SETUP_FILE=path/to/lisp-ai-tools/qlot-setup.lisp ~/.qlot/bin/qlot install
```

What the project supplies:

- Its `.asd` depends on `lisp-ai-tools`, `lisp-ai-tools/debug`, `lisp-ai-tools/bench` and `lisp-ai-tools/guard`, and
  has `(sb-ext:restrict-compiler-policy 'debug 3)` before the first `defsystem` (without it a `let` variable cannot be
  read in a held frame).
- Its package uses `lisp-ai-debug` and `lisp-ai-bench`, so the commands are bare names through `bin/lisp eval`.
- Its socket server calls `(lisp-ai-debug:eval-with-debugging request :<name>)`, binds `lisp-ai-debug:*reply-now*`, and
  answers a hook payload (`lisp-ai-guard:hook-payload-p`) with `lisp-ai-guard:socket-reply`.
- Its `bin/serve.lisp` loads the system, sets `*package*`, makes the package `scratch`, calls
  `(lisp-ai-bench:configure :project :<name>)` and starts the server.
- `rules/forbidden-programs.sexp` and `rules/lisp-files.sexp`, copied from `rules/` here.
- `.claude/settings.json` wires `bin/lisp guard` as a PreToolUse hook on `Bash` and on `Edit|Write|MultiEdit`, and
  `bin/lisp start` on `SessionStart`; no `permissions.deny`.
- `tests/shared-test.lisp` calls `(form-edit-tests:run DIRECTORY)`, `(lisp-ai-bench-tests:run DIRECTORY)`,
  `(lisp-ai-guard-tests:run DIRECTORY PROJECT)`, `(lisp-ai-debug-tests:run DIRECTORY START STOP)` and
  `(lisp-ai-script-tests:run DIRECTORY START STOP)`. The shared tests fail when any of the above is missing.

Two projects use it today: `aquarium`, a demo tank of fish drawn as text once a second and changed while it runs, and
`code-as-data`. Their `CLAUDE.md` files and the tree's `docs/workbench.md` hold the full method.

## Testing

Every refusal was broken once and watched failing before it was trusted: writer rule off, 14 failures; gate log off, 4;
Lisp-names rule off, 6; Edit/Write rule off, 7; all intact, 0. The gate was also watched firing in a real `claude -p`
session: an Edit of a source file refused, a Write of a new file allowed, `wc -l qlfile` allowed, `cat src/package.lisp`
refused, `echo x > src/zzz.lisp` refused. The workbench was run end to end in a clone's image: a definition typed with a
comment above it, a comment inside it and a docstring reached its file byte for byte, and a second version that dropped
the inner comment was refused by name.

Tests run from clean clones of a project, never against a running image someone is attached to. `bin/lisp test` in a
project runs this repo's tests among its own.

## The repo gates itself

A Claude Code session started in a directory that holds this repo but is not a project (the `~/lisp-ai/` tree) has no
image of its own, so its `.claude/settings.json` runs the gate from here: `cd tools && sh lisp lisp-ai-tools guard`,
after one `sh lisp lisp-ai-tools qlot install` in this directory (`qlfile` names the Quicklisp dist; `.qlot/` and `run/`
are ignored). The judgement uses the rules in `rules/` and logs denials in `run/gate.log` here. With `LISP_AI_SOCKET`
set to a running project image's socket (the tree's hook picks the first it finds) the image answers in about 40 ms:
the script puts the field `lisp_ai_gate_dir`, this directory, in front of the payload, and `socket-reply` judges by that
directory's rules and logs there, not in the image's own project. When no image answers, a fresh SBCL judges, about 2 s.

## History

The repo was extracted from the `~/lisp-ai/` tree on 2026-10-23 with `git filter-repo`. On 2026-10-24 the workbench and
the gate replaced an older guard, a 140-rule `permissions.deny` list and a `profile` package: those judged which
programs a command ran rather than what it acted on, so `wc` on a log was refused as readily as `sed` on source, and
nothing covered the Edit and Write tools or recorded what the image held that no file held.

## Requirements

SBCL (developed on 2.6.9, arm64 macOS), Qlot, Quicklisp dist 2026-01-01 or later for Slynk, Eclector and
`com.inuoe.jzon`. Emacs with Sly at the same commit as Slynk for the editor link, optional.
