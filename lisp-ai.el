;;; lisp-ai.el --- the Emacs half of lisp-ai-tools, for any project  -*- lexical-binding: t; -*-

;; Layer 1 of the Emacs half (emacs-design.md): find the project, load the Sly it pins, connect Sly to its image, watch
;; a file that the image rewrites, and send requests to the image through bin/lisp eval, the door Claude Code uses.
;; A project reaches this file through its Qlot install, as it reaches the Lisp half: its demo.el reads
;; .qlot/dists/lisp-ai-tools/distinfo.txt for the pinned commit and loads this file from that commit's directory.
;; Nothing here changes ~/.emacs.d/init.el, and none of it outlasts the Emacs session.

(require 'cl-lib)
(require 'pulse)
(require 'subr-x)

(declare-function sly-connect "sly")
(declare-function sly-connected-p "sly")
(declare-function sly-disconnect-all "sly")
(declare-function sly-mrepl-return "sly-mrepl")
(defvar paredit-mode-map)

(defconst lisp-ai-directory (file-name-directory (or load-file-name buffer-file-name))
  "The directory this file was loaded from: the tools commit the project pins.")

;;; The project

(defun lisp-ai-project-root (&optional dir)
  "The project: the nearest directory upwards from DIR (default: the current buffer's) that holds bin/lisp.
Never derived from a directory's name. An error when there is none."
  (let* ((start (or dir default-directory))
         (found (locate-dominating-file start "bin/lisp")))
    (unless found (error "No project above %s: no directory there holds bin/lisp" start))
    (file-name-as-directory (expand-file-name found))))

(defun lisp-ai-sly-directory (root)
  "The Sly checkout the project ROOT pins: ROOT/.qlot/dists/quicklisp/software/sly-*, which Qlot links to the checkout
of the date the project's qlfile names. An error when there is none or more than one."
  (let ((found (file-expand-wildcards (expand-file-name ".qlot/dists/quicklisp/software/sly-*" root))))
    (cond ((null found)
           (error "No Sly in %s.qlot/dists/quicklisp/software/: run qlot install in the project first" root))
          ((cdr found)
           (error "More than one Sly in %s.qlot/dists/quicklisp/software/: %s" root
                  (mapconcat #'file-name-nondirectory found ", ")))
          (t (file-name-as-directory (car found))))))

(defun lisp-ai-load-sly (root)
  "Load the Emacs half of the Sly the project ROOT pins (`lisp-ai-sly-directory'): the checkout the image's Slynk comes
from. ELPA's Sly differs from it in the prompt message, and with it every prompt says \"Debugger entered on <n>\".
An error when another Sly is already loaded in this Emacs: start a new Emacs and load the project's demo.el first."
  (let* ((directory (lisp-ai-sly-directory root))
         (loaded (and (featurep 'sly) (symbol-file 'sly-connect 'defun))))
    (when (and loaded
               (not (string-prefix-p (file-truename directory) (file-truename loaded))))
      (error "Another Sly (%s) is already loaded in this Emacs. Start a new Emacs, then load demo.el first"
             (file-name-directory loaded)))
    (add-to-list 'load-path directory)
    (add-to-list 'load-path (expand-file-name "contrib" directory))
    (require 'sly)
    directory))

(defun lisp-ai-port (root)
  "The port in ROOT/rules/slynk.sexp: that file is the one place the port is written."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "rules/slynk.sexp" root))
    (or (plist-get (read (current-buffer)) :port)
        (error "%srules/slynk.sexp gives no :port" root))))

;;; Watching a file the image rewrites

(defun lisp-ai-changed-region (old new)
  "The part of the string NEW that differs from the string OLD, as (START . END) buffer positions; nil if none.
Characters the two have in common at the start and at the end do not count. A pure deletion gives one character."
  (unless (string= old new)
    (let* ((limit (min (length old) (length new)))
           (prefix (let ((i 0))
                     (while (and (< i limit) (eq (aref old i) (aref new i))) (setq i (1+ i)))
                     i))
           (suffix (let ((i 0) (room (- limit prefix)))
                     (while (and (< i room) (eq (aref old (- (length old) 1 i)) (aref new (- (length new) 1 i))))
                       (setq i (1+ i)))
                     i))
           (start (1+ prefix))
           (end (1+ (- (length new) suffix))))
      (cons start (max end (min (1+ start) (1+ (length new))))))))

(defvar-local lisp-ai--before nil "The text this buffer held just before Emacs reverted it.")

(defun lisp-ai--remember ()
  (setq lisp-ai--before (buffer-string)))

(defun lisp-ai--flash ()
  "After a revert: bring the changed part into view and flash it."
  (when lisp-ai--before
    (let ((region (lisp-ai-changed-region lisp-ai--before (buffer-string)))
          (window (get-buffer-window (current-buffer))))
      (setq lisp-ai--before nil)
      (when region
        (when window
          (set-window-point window (car region))
          (with-selected-window window (recenter)))
        (let ((pulse-iterations 40) (pulse-delay 0.06))
          (pulse-momentary-highlight-region (car region) (cdr region) 'highlight))))))

(cl-defun lisp-ai-watch (root file &key flash on-revert)
  "Visit FILE (relative to ROOT, or absolute) and redraw it within half a second of a change on disk. Returns the buffer.
With FLASH, bring the changed part into view and flash it. ON-REVERT, a function of no arguments, runs in the buffer
after each redraw, for anything else (the aquarium's tank stays at the top)."
  (setq auto-revert-interval 0.5
        auto-revert-verbose nil)
  (let* ((path (expand-file-name file root))
         ;; a buffer already visiting the file is reused: visiting it again would ask "changed on disk, reread?" every
         ;; time the file has changed since the last poll, which for the aquarium's tank is every second
         (buffer (or (find-buffer-visiting path) (find-file-noselect path))))
    (with-current-buffer buffer
      (unless (verify-visited-file-modtime buffer) (revert-buffer t t t))
      ;; polling, not file notification: the files are replaced by a rename, which a notification watch loses
      (setq-local auto-revert-use-notify nil)
      (auto-revert-mode 1)
      (when flash
        (add-hook 'before-revert-hook #'lisp-ai--remember nil t)
        (add-hook 'after-revert-hook #'lisp-ai--flash nil t))
      (when on-revert
        (add-hook 'after-revert-hook on-revert nil t)))
    buffer))

;;; The Sly REPL

(defun lisp-ai--give-return-back-to-sly ()
  "In the Sly REPL, make RET send the input to the image even when paredit-mode is on.
init.el turns paredit-mode on in REPLs, and paredit binds RET to `paredit-RET', which only inserts a newline: the
expression is typed and nothing is ever evaluated. This changes the key in the REPL buffer only."
  (when (boundp 'paredit-mode-map)
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map paredit-mode-map)
      (define-key map (kbd "RET") #'sly-mrepl-return)
      (setq-local minor-mode-overriding-map-alist
                  (cons (cons 'paredit-mode map)
                        (assq-delete-all 'paredit-mode minor-mode-overriding-map-alist))))))

(defun lisp-ai--follow (&rest _)
  "Keep the end of the REPL in view, in every window that shows it.
What the image writes is inserted before the prompt. When the prompt is the window's first line (as it is right after
connecting), that is above the window's top, and Emacs does not scroll for text inserted above the top: the echo lands
in the buffer and out of sight. So after every insertion the last line is put at the bottom of the window, as a
terminal does; while the buffer is shorter than the window it shows from its first line.
The start is computed with `vertical-motion' and set directly. Selecting the window and calling `recenter' here, inside
Sly's network filter, crashed Emacs 31.1 (a segfault in redisplay's window-size hook) when a demo was started with
`-f', as bin/demo starts it (2026-10-25)."
  (when (derived-mode-p 'sly-mrepl-mode)
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (let ((start (save-excursion
                     (goto-char (point-max))
                     (vertical-motion (- 1 (window-body-height window)) window)
                     (point))))
        (set-window-start window start)))))

(defvar lisp-ai--root nil "The project the last `lisp-ai-connect' connected to.")
(defvar lisp-ai--repl-window nil "The window the Sly REPL is shown in, or nil for Sly's own choice.")

(defun lisp-ai--display-repl (buffer _alist)
  "A `display-buffer' action: show BUFFER, the Sly REPL, in the window `lisp-ai-connect' was given."
  (when (window-live-p lisp-ai--repl-window)
    (set-window-buffer lisp-ai--repl-window buffer)
    lisp-ai--repl-window))

(defun lisp-ai--install-repl-fixes ()
  "Put in place, once, the two fixes of the REPL buffer and the rule that shows it in the demo's window."
  ;; last in the hook, so it runs after the hook that turns paredit-mode on
  (add-hook 'sly-mrepl-mode-hook #'lisp-ai--give-return-back-to-sly t)
  ;; `sly-mrepl--insert-output' writes what the image sends (the echo among it); `sly-mrepl--insert' writes Sly's own
  ;; notes, the results and the prompt
  (advice-add 'sly-mrepl--insert-output :after #'lisp-ai--follow)
  (advice-add 'sly-mrepl--insert :after #'lisp-ai--follow)
  (add-to-list 'display-buffer-alist '("\\*sly-mrepl" (lisp-ai--display-repl))))

(defun lisp-ai-connect (root &optional window)
  "Close a leftover Sly connection, connect Sly to 127.0.0.1 at the port of the project ROOT (`lisp-ai-port'), and show
the REPL in WINDOW (default: wherever Sly puts it). Also the reconnect after an image restart.
In the REPL buffer RET sends the input to the image even when paredit-mode is on, and the end stays in view as the
image writes to it."
  (lisp-ai--install-repl-fixes)
  (setq lisp-ai--root root
        lisp-ai--repl-window window)
  (when (sly-connected-p) (sly-disconnect-all))
  (when (window-live-p window) (select-window window))
  (sly-connect "127.0.0.1" (lisp-ai-port root)))

(defun lisp-ai-reconnect ()
  "Connect Sly to the image again, at the port of the project last connected: a restart of the image drops the
connection. A connection left over is closed first, and the REPL comes back in the window it was shown in."
  (interactive)
  (unless lisp-ai--root (error "Not connected yet: nothing to reconnect to"))
  (lisp-ai-connect lisp-ai--root lisp-ai--repl-window))

;;; Requests through bin/lisp eval

(defun lisp-ai-eval-now (root request)
  "Send REQUEST, a string of Lisp, to the image of the project ROOT through bin/lisp eval and wait.
Returns (EXIT-STATUS . OUTPUT)."
  (with-temp-buffer
    (let* ((default-directory root)
           (status (call-process (expand-file-name "bin/lisp" root) nil t nil "eval" request)))
      (cons status (buffer-string)))))

(defun lisp-ai-failed-p (status output)
  "True when a reply of bin/lisp eval, its exit STATUS and OUTPUT, says the request failed or is held."
  (or (not (eql status 0)) (string-match-p "^\\(ERROR:\\|HELD [0-9]+\\)" output)))

(defun lisp-ai-eval-async (root requests done)
  "Send REQUESTS, strings of Lisp, to the image of the project ROOT one after another through bin/lisp eval, without
blocking Emacs; then call DONE with nil, or with the failing request's output when one failed. A held error is
abandoned first, by (debug-abandon n), so the image is not left waiting; the rest of REQUESTS is not sent."
  (if (null requests)
      (funcall done nil)
    (let* ((default-directory root)
           (output (generate-new-buffer " *lisp-ai request*")))
      (make-process
       :name "lisp-ai-request" :buffer output :noquery t
       :command (list (expand-file-name "bin/lisp" root) "eval" (car requests))
       :sentinel
       (lambda (process _event)
         (unless (process-live-p process)
           (let ((text (with-current-buffer output (buffer-string)))
                 (status (process-exit-status process)))
             (kill-buffer output)
             (if (lisp-ai-failed-p status text)
                 (progn
                   (when (string-match "^HELD \\([0-9]+\\)" text)
                     (lisp-ai-eval-now root (format "(debug-abandon %s)" (match-string 1 text))))
                   (funcall done text))
               (lisp-ai-eval-async root (cdr requests) done)))))))))

(provide 'lisp-ai)
;;; lisp-ai.el ends here
