;;; lisp-ai-demo.el --- a demo screen, stepped or watched  -*- lexical-binding: t; -*-

;; Layer 2 of the Emacs half (emacs-design.md), on top of lisp-ai.el. A project's demo.el calls `lisp-ai-demo-configure'
;; with its layout; `M-x lisp-ai-demo' (bin/demo runs it with -f) then sets up the screen: the source on the left, and on
;; the right a Sly REPL connected to the image, with an optional file above it (the aquarium's tank).
;;
;; With a caption window (:caption-lines), the demo is stepped: the steps are data in the project's demo/steps.sexp, a
;; plist of :file (the source the demo rewrites), :start and :quit (requests that put the demo at its start, and back
;; after a stop half way), :intro, :end and :steps, each step a plist of :do, :send (requests) and :look. SPACE in the
;; caption window sends the next step's requests through bin/lisp eval, the door Claude Code uses, so the echo shows each
;; request and reply in the REPL after claude>; q ends the demo and puts :file back as committed.
;; Without one, the screen is only watched: what happens is done by someone else, a Claude Code session through
;; bin/lisp eval or the programmer at the REPL.

(require 'lisp-ai)

(defvar lisp-ai-demo-start-on-setup t
  "When true, `lisp-ai-demo' starts a stepped demo once the screen is set up. A project's tests/demo-test.el sets it to
nil to check the screen in the real tree, where starting would rewrite the demo's :file.")

(defvar lisp-ai-demo--layout nil "The plist `lisp-ai-demo-configure' was given, with :root.")
(defvar lisp-ai-demo--script nil "The plist read from demo/steps.sexp.")
(defvar lisp-ai-demo--next nil "The index of the step SPACE does next, or nil before the start and after the end.")
(defvar lisp-ai-demo--busy nil "True while a step's requests are on their way.")
(defvar lisp-ai-demo--caption-window nil "The window the captions are shown in, or nil when the screen is watched.")

(defconst lisp-ai-demo-caption-buffer "*lisp-ai demo*" "The name of the buffer the captions are shown in.")

(cl-defun lisp-ai-demo-configure (root &key source above-repl (above-repl-lines 14) caption-lines message)
  "Give `lisp-ai-demo' the layout of the project ROOT's demo.
SOURCE: the file shown on the left, relative to ROOT; it flashes where it changes on disk.
ABOVE-REPL: a file shown above the REPL on the right, ABOVE-REPL-LINES high, redrawn as it changes and kept at its top.
CAPTION-LINES: the height of the caption window under the source; given, the demo is stepped by ROOT/demo/steps.sexp;
nil, the screen is only watched.
MESSAGE: shown in the echo area once a watched screen is set up."
  (unless source (error "lisp-ai-demo-configure: no :source"))
  (setq lisp-ai-demo--layout (list :root root :source source :above-repl above-repl
                                   :above-repl-lines above-repl-lines :caption-lines caption-lines
                                   :message message)))

(defun lisp-ai-demo--get (key) (plist-get lisp-ai-demo--layout key))
(defun lisp-ai-demo--root () (lisp-ai-demo--get :root))
(defun lisp-ai-demo--steps () (plist-get lisp-ai-demo--script :steps))

(defun lisp-ai-demo--read-script ()
  "The plist of ROOT/demo/steps.sexp. An error when it lacks :file or :steps."
  (let ((script (with-temp-buffer
                  (insert-file-contents (expand-file-name "demo/steps.sexp" (lisp-ai-demo--root)))
                  (read (current-buffer)))))
    (dolist (key '(:file :steps))
      (unless (plist-get script key) (error "demo/steps.sexp gives no %s" key)))
    script))

;;; The caption window

(defvar lisp-ai-demo-caption-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") #'lisp-ai-demo-next)
    (define-key map (kbd "q") #'lisp-ai-demo-quit)
    map))

(define-derived-mode lisp-ai-demo-caption-mode special-mode "Demo"
  "The captions of a stepped demo. SPACE: the next step. q: end the demo, the file put back as committed."
  (visual-line-mode 1)
  (setq-local cursor-type nil))

(defun lisp-ai-demo--caption (&rest paragraphs)
  "Show PARAGRAPHS (strings; nil ones are left out) in the caption window, and select it, so that SPACE reaches it."
  (let ((buffer (get-buffer-create lisp-ai-demo-caption-buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'lisp-ai-demo-caption-mode) (lisp-ai-demo-caption-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (mapconcat #'identity (delq nil paragraphs) "\n\n"))
        (goto-char (point-min))))
    (when (window-live-p lisp-ai-demo--caption-window)
      (set-window-buffer lisp-ai-demo--caption-window buffer)
      (select-window lisp-ai-demo--caption-window))
    buffer))

(defun lisp-ai-demo--keys ()
  (if lisp-ai-demo--next
      (format "[SPACE: step %d of %d]   [q: end the demo]" (1+ lisp-ai-demo--next) (length (lisp-ai-demo--steps)))
    "[SPACE: run the demo again]   [q: end the demo]"))

(defun lisp-ai-demo--back-to-caption ()
  "Once the REPL is up, give the keyboard back to the caption window, so that SPACE goes on with the demo."
  (when (window-live-p lisp-ai-demo--caption-window)
    (run-at-time 0.5 nil (lambda () (when (window-live-p lisp-ai-demo--caption-window)
                                      (select-window lisp-ai-demo--caption-window))))))

;;; Stepping

(defun lisp-ai-demo--file-committed-p ()
  "True when the demo's :file is as committed."
  (let ((default-directory (lisp-ai-demo--root)))
    (string= "" (shell-command-to-string
                 (concat "git status --porcelain -- " (shell-quote-argument (plist-get lisp-ai-demo--script :file)))))))

(defun lisp-ai-demo--refusal ()
  "Why the demo must not start, or nil: it rewrites its :file and forgets the ledger, so it starts only when the file is
as committed and the image holds nothing unsaved."
  (let ((file (plist-get lisp-ai-demo--script :file)))
    (cond ((not (lisp-ai-demo--file-committed-p))
           (format "%s has changes that are not committed, and the demo would write over them. Commit them, or put the file back with  git restore %s , then run bin/demo again."
                   file file))
          ((let ((reply (lisp-ai-eval-now (lisp-ai-demo--root) "(pending)")))
             (unless (and (eql (car reply) 0) (string= (string-trim (cdr reply)) "NIL"))
               (format "The running program holds definitions that are in no file yet: %s\nThe demo would throw them away. Save them, or drop them, then run bin/demo again."
                       (string-trim (cdr reply)))))))))

(defun lisp-ai-demo-start ()
  "Start the stepped demo over: send its :start requests, then show :intro and the first step."
  (interactive)
  (setq lisp-ai-demo--script (lisp-ai-demo--read-script)
        lisp-ai-demo--next nil)
  (let ((refusal (lisp-ai-demo--refusal)))
    (if refusal
        (lisp-ai-demo--caption "The demo cannot start." refusal)
      (setq lisp-ai-demo--busy t)
      (lisp-ai-demo--caption "Starting: the source is being put back to how the program started out...")
      (lisp-ai-eval-async
       (lisp-ai-demo--root) (plist-get lisp-ai-demo--script :start)
       (lambda (failure)
         (setq lisp-ai-demo--busy nil)
         (if failure
             (lisp-ai-demo--caption "The demo could not start: the program answered" failure)
           (setq lisp-ai-demo--next 0)
           (lisp-ai-demo--caption (plist-get lisp-ai-demo--script :intro)
                                  (concat "Next: " (plist-get (nth 0 (lisp-ai-demo--steps)) :do))
                                  (lisp-ai-demo--keys))))))))

(defun lisp-ai-demo-next ()
  "Do the next step of the demo; after the last, start it over."
  (interactive)
  (cond (lisp-ai-demo--busy (message "One moment: the step is still on its way."))
        ((null lisp-ai-demo--next) (lisp-ai-demo-start))
        (t
         (let* ((index lisp-ai-demo--next)
                (step (nth index (lisp-ai-demo--steps))))
           (setq lisp-ai-demo--busy t)
           (lisp-ai-demo--caption (concat "Now: " (plist-get step :do)))
           (lisp-ai-eval-async
            (lisp-ai-demo--root) (plist-get step :send)
            (lambda (failure)
              (setq lisp-ai-demo--busy nil)
              (if failure
                  (lisp-ai-demo--caption (format "Step %d did not work: the program answered" (1+ index)) failure
                                         "q ends the demo and puts the file back.")
                (let ((following (nth (1+ index) (lisp-ai-demo--steps))))
                  (setq lisp-ai-demo--next (and following (1+ index)))
                  (lisp-ai-demo--caption (plist-get step :look)
                                         (if following
                                             (concat "Next: " (plist-get following :do))
                                           (plist-get lisp-ai-demo--script :end))
                                         (lisp-ai-demo--keys))))))))))

(defun lisp-ai-demo-quit ()
  "End the demo. If it was stopped half way, put its :file back as committed and send its :quit requests, which take
the file back into the image and forget what the demo left unsaved."
  (interactive)
  (if lisp-ai-demo--busy
      (message "One moment: the step is still on its way.")
    (unless lisp-ai-demo--script (setq lisp-ai-demo--script (lisp-ai-demo--read-script)))
    (let ((default-directory (lisp-ai-demo--root))
          (half-way (or lisp-ai-demo--next (not (lisp-ai-demo--file-committed-p)))))
      (setq lisp-ai-demo--next nil)
      (if (not half-way)
          (lisp-ai-demo--caption "The demo has ended. The source file is as it was committed." (lisp-ai-demo--keys))
        (call-process "git" nil nil nil "restore" "--" (plist-get lisp-ai-demo--script :file))
        (setq lisp-ai-demo--busy t)
        (lisp-ai-eval-async
         (lisp-ai-demo--root) (plist-get lisp-ai-demo--script :quit)
         (lambda (failure)
           (setq lisp-ai-demo--busy nil)
           (lisp-ai-demo--caption (if failure "The demo has ended, but the program did not take the file back:"
                                    "The demo has ended. The source file is back as it was committed, and so is the running program.")
                                  failure (lisp-ai-demo--keys))))))))

;;; The screen

(defun lisp-ai-demo ()
  "Set up the demo's screen (`lisp-ai-demo-configure' gave the layout): the source on the left, above the captions when
the demo is stepped; on the right a Sly REPL connected to the image, under the :above-repl file when there is one.
A stepped demo then starts; SPACE in the caption window goes on."
  (interactive)
  (unless lisp-ai-demo--layout (error "No demo configured: load the project's demo.el"))
  (let* ((root (lisp-ai-demo--root))
         (caption-lines (lisp-ai-demo--get :caption-lines))
         (source (lisp-ai-watch root (lisp-ai-demo--get :source) :flash t))
         (above (and (lisp-ai-demo--get :above-repl)
                     (lisp-ai-watch root (lisp-ai-demo--get :above-repl)
                                    :on-revert (lambda () (goto-char (point-min)))))))
    (delete-other-windows)
    (switch-to-buffer source)
    (let* ((source-window (selected-window))
           (right-window (split-window-right))
           (caption-window (and caption-lines (split-window source-window (- caption-lines) 'below)))
           (repl-window (if above
                            (progn (select-window right-window)
                                   (split-window-below (lisp-ai-demo--get :above-repl-lines)))
                          right-window)))
      (when above (set-window-buffer right-window above))
      (setq lisp-ai-demo--caption-window caption-window)
      (when caption-window
        (add-hook 'sly-mrepl-mode-hook #'lisp-ai-demo--back-to-caption t))
      (lisp-ai-connect root repl-window))
    (cond ((null caption-lines)
           (when (lisp-ai-demo--get :message) (message "%s" (lisp-ai-demo--get :message))))
          (lisp-ai-demo-start-on-setup (lisp-ai-demo-start))
          (t (lisp-ai-demo--caption "The screen is set up; SPACE starts the demo.")))))

(provide 'lisp-ai-demo)
;;; lisp-ai-demo.el ends here
