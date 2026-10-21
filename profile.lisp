;;;; The profile of the guard and the deny list, for every project (docs/guard-and-deny.md says what the two layers are).
;;;; While profiling is on, each Bash command Claude Code runs in a project is logged: the guard's verdict (where the project
;;;; has a guard) and whether the command ran. A command the guard allowed and that did not run was stopped by the deny
;;;; list (or declined at a prompt); one that ran got past it. Switches are files in the project's run/profile/, so a hook
;;;; reads them at each call and nothing has to be restarted:
;;;;   on        profiling is on: events are logged to run/profile/events.log
;;;;   shadow    the guard logs what it would deny but allows, so the deny list judges the command too
;;;;   no-limit  the shadow applies to every command; without this file it applies only to commands listed in the project's
;;;;             rules/profile-corpus.sexp (harmless ones), and every other command is still enforced
;;;; A project is a pathname, a string with a slash, or the name of its ASDF system (:code-as-data). Only dated summaries
;;;; (write-summary, into the project's data/) are meant to be committed; the log is not.
(defpackage :lisp-ai-profile
  (:use :cl)
  (:export #:project-directory #:profile-directory #:profiling #:shadowing #:limit #:shadow-active-p #:status
           #:corpus #:corpus-command-p #:forbidden-programs #:loose-hits #:record-guard #:record-error #:error-count #:read-events #:entries
           #:summary #:write-summary))
(in-package :lisp-ai-profile)

(defun system-directory (name)
  (or (asdf:system-source-directory name) (error "profile: no ASDF system ~a" name)))

(defun project-directory (project)
  "PROJECT as a directory pathname: a pathname, a string holding a slash, or the name of an ASDF system."
  (etypecase project
    (pathname (uiop:ensure-directory-pathname project))
    (string (if (find #\/ project) (uiop:ensure-directory-pathname project) (system-directory project)))
    (symbol (system-directory (string-downcase (symbol-name project))))))

(defun profile-directory (project)
  (merge-pathnames "run/profile/" (project-directory project)))

(defun switch (project name &optional (state nil state-p))
  "Whether the switch file NAME exists in PROJECT's run/profile/. With STATE, :on makes the file and :off removes it first."
  (let ((file (merge-pathnames name (profile-directory project))))
    (when state-p
      (ecase state
        (:on (ensure-directories-exist file)
         (with-open-file (out file :direction :output :if-exists :supersede :if-does-not-exist :create)
           (declare (ignorable out))))
        (:off (when (probe-file file) (delete-file file)))))
    (and (probe-file file) t)))

(defun profiling (project &optional (state nil state-p))
  "Whether profiling is on; with STATE (:on or :off), turn it so first."
  (if state-p (switch project "on" state) (switch project "on")))

(defun shadowing (project &optional (state nil state-p))
  "Whether the shadow switch is set; with STATE (:on or :off), set it first. It has an effect only while profiling is on."
  (if state-p (switch project "shadow" state) (switch project "shadow")))

(defun limit (project &optional (state nil state-p))
  "Whether the shadow is limited to the commands of rules/profile-corpus.sexp (the default). :off lifts the limit, :on
   restores it."
  (when state-p (switch project "no-limit" (ecase state (:on :off) (:off :on))))
  (not (switch project "no-limit")))

(defun shadow-active-p (project)
  (and (profiling project) (shadowing project)))

(defun status (project)
  (list :profiling (profiling project) :shadow (shadowing project) :limit (limit project)
        :shadow-active (shadow-active-p project)))

(defun read-data (project relative)
  "The one form in the file RELATIVE of PROJECT, or NIL when there is no such file."
  (let ((file (merge-pathnames relative (project-directory project))) (*read-eval* nil))
    (when (probe-file file)
      (with-open-file (in file :external-format :utf-8) (read in nil nil)))))

(defun corpus (project)
  "The commands in PROJECT's rules/profile-corpus.sexp: harmless ones (/dev/null arguments, no writes), each a form of
   command that the guard or the deny list should catch."
  (read-data project "rules/profile-corpus.sexp"))

(defun corpus-command-p (project command)
  (and (member command (corpus project) :test #'string=) t))

(defun forbidden-programs (project)
  (read-data project "rules/forbidden-programs.sexp"))

(defun token-list (command)
  (let ((tokens '()) (start nil))
    (flet ((end (i) (when start (push (subseq command start i) tokens) (setf start nil))))
      (loop for i from 0 below (length command)
            for c = (char command i)
            do (if (or (alphanumericp c) (find c "_-")) (unless start (setf start i)) (end i)))
      (end (length command)))
    (nreverse tokens)))

(defun loose-hits (command forbidden)
  "The programs of FORBIDDEN that appear anywhere in COMMAND as a word: an over-approximation, since `echo sed` counts. A
   command that ran and has a hit is a candidate for something both layers missed, to be looked at by hand."
  (remove-duplicates (remove-if-not (lambda (token) (member token forbidden :test #'string=)) (token-list command))
                     :test #'string= :from-end t))

(defun unix-time () (- (get-universal-time) 2208988800))

(defun events-file (project) (merge-pathnames "events.log" (profile-directory project)))

(defun log-line (project kind text)
  (let ((file (events-file project)))
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output :if-exists :append :if-does-not-exist :create :external-format :utf-8)
      (format out "~d~c~a~c~a~%" (unix-time) #\Tab kind #\Tab text))))

(defun object (&rest pairs)
  (let ((table (make-hash-table :test 'equal)))
    (loop for (key value) on pairs by #'cddr do (setf (gethash key table) value))
    table))

(defun record-guard (project &key id command verdict programs shadowed)
  "Log the guard's judgement of one call: VERDICT is :allow or :deny, PROGRAMS the forbidden programs it found, SHADOWED true
   when it would have denied but let the call through."
  (log-line project "guard"
            (com.inuoe.jzon:stringify
             (object "id" id "command" command "verdict" (string-downcase (symbol-name verdict))
                     "programs" (coerce programs 'vector) "shadowed" (if shadowed t nil)))))

(defun errors-file (project) (merge-pathnames "errors.log" (profile-directory project)))

(defun record-error (project text)
  "Log that something in the profiling itself failed, in a file of its own so that a broken events.log cannot hide it. It
   never changes a verdict; the summary counts these lines."
  (let ((file (errors-file project)))
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output :if-exists :append :if-does-not-exist :create :external-format :utf-8)
      (format out "~d~c~a~%" (unix-time) #\Tab (substitute #\Space #\Newline text)))))

(defun error-count (project)
  (let ((file (errors-file project)))
    (if (probe-file file)
        (with-open-file (in file :external-format :utf-8) (loop for line = (read-line in nil) while line count t))
        0)))

(defun split-line (line)
  "(time kind payload) for a line of the log, or NIL when it is not one."
  (let* ((first (position #\Tab line)) (second (and first (position #\Tab line :start (1+ first)))))
    (when second
      (let ((time (parse-integer line :end first :junk-allowed t)))
        (when time (list time (subseq line (1+ first) second) (subseq line (1+ second))))))))

(defun read-events (project)
  "The log as (time kind payload) with PAYLOAD parsed (a hash table), and as second value the number of lines that could not
   be read. A project with no log has none."
  (let ((file (events-file project)) (events '()) (bad 0))
    (when (probe-file file)
      (with-open-file (in file :external-format :utf-8)
        (loop for line = (read-line in nil) while line
              do (let ((parts (split-line line)))
                   (let ((payload (and parts (handler-case (com.inuoe.jzon:parse (third parts)) (error () nil)))))
                     (if (hash-table-p payload)
                         (push (list (first parts) (second parts) payload) events)
                         (incf bad)))))))
    (values (nreverse events) bad)))

(defun entries (project &key (now (unix-time)) (pending-after 120))
  "One plist per call that was logged before it ran: :id :time :command :guard :ran :deny :hits.
   :guard is :denied (the call stopped at the guard), :would-deny (shadowed), :allowed, or :none (a project with no guard).
   :deny is :unknown (the guard stopped it, so the deny list never judged it), :missed (it ran), :caught (it did not run
   and more than PENDING-AFTER seconds have passed: stopped by the deny list, or declined at a prompt) or :pending."
  (let ((table (make-hash-table :test 'equal)) (order '()) (ran (make-hash-table :test 'equal))
        (forbidden (forbidden-programs project)))
    (dolist (event (read-events project))
      (destructuring-bind (time kind payload) event
        (flet ((id () (or (gethash "id" payload) (gethash "tool_use_id" payload))))
          (cond ((string= kind "guard")
                 (let ((id (id)))
                   (unless (gethash id table) (push id order))
                   (setf (gethash id table)
                         (list :id id :time time :command (gethash "command" payload)
                               :guard (cond ((equal (gethash "shadowed" payload) t) :would-deny)
                                            ((equal (gethash "verdict" payload) "deny") :denied)
                                            (t :allowed))))))
                ((string= kind "pre")
                 (let ((id (id)))
                   (unless (gethash id table)
                     (push id order)
                     (setf (gethash id table)
                           (list :id id :time time
                                 :command (let ((input (gethash "tool_input" payload)))
                                            (and (hash-table-p input) (gethash "command" input)))
                                 :guard :none)))))
                ((member kind '("post" "failure") :test #'string=) (setf (gethash (id) ran) t))))))
    (loop for id in (nreverse order)
          collect (let* ((entry (gethash id table)) (guard (getf entry :guard)) (ran-p (gethash id ran))
                         (command (or (getf entry :command) "")))
                    (list* :ran (and ran-p t)
                           :deny (cond ((eq guard :denied) :unknown)
                                       (ran-p :missed)
                                       ((> (- now (getf entry :time)) pending-after) :caught)
                                       (t :pending))
                           :hits (loose-hits command forbidden)
                           entry)))))

(defun shorten (command) (if (> (length command) 200) (subseq command 0 200) command))

(defun distinct-counts (commands)
  "((command . count) ...) of the distinct COMMANDS, first seen first."
  (let ((counts '()))
    (dolist (command commands)
      (let ((cell (assoc command counts :test #'string=)))
        (if cell (incf (cdr cell)) (push (cons command 1) counts))))
    (nreverse counts)))

(defun summary (project &key (now (unix-time)))
  "What the log says so far: how many calls fell in each cell of guard-verdict by deny-verdict, and the distinct commands
   in the cells that matter. Commands are cut to 200 characters."
  (multiple-value-bind (events bad) (read-events project)
    (let* ((entries (entries project :now now))
           (cells (make-hash-table :test 'equal))
           (seen (mapcar (lambda (entry) (getf entry :command)) entries)))
      (dolist (entry entries) (incf (gethash (list (getf entry :guard) (getf entry :deny)) cells 0)))
      (flet ((commands (test) (distinct-counts (loop for entry in entries
                                                     when (funcall test entry) collect (shorten (getf entry :command))))))
        (list :project (car (last (pathname-directory (project-directory project))))
              :events (length events) :unreadable-lines bad :logging-errors (error-count project)
              :calls (length entries)
              :first (and events (first (first events))) :last (and events (first (car (last events))))
              :cells (sort (loop for key being the hash-keys of cells using (hash-value count) collect (list key count))
                           #'string< :key (lambda (row) (prin1-to-string (first row))))
              :guard-only-catches (commands (lambda (e) (and (eq (getf e :guard) :would-deny) (eq (getf e :deny) :missed))))
              :deny-only-catches (commands (lambda (e) (and (member (getf e :guard) '(:allowed :none))
                                                            (eq (getf e :deny) :caught) (getf e :hits))))
              :both-caught (commands (lambda (e) (and (eq (getf e :guard) :would-deny) (eq (getf e :deny) :caught))))
              :candidates-both-missed (commands (lambda (e) (and (member (getf e :guard) '(:allowed :none))
                                                                 (eq (getf e :deny) :missed) (getf e :hits))))
              :corpus-not-yet-run (remove-if (lambda (command) (member command seen :test #'string=)) (corpus project)))))))

(defun write-summary (project &key (date (multiple-value-bind (s m h day month year) (get-decoded-time)
                                           (declare (ignore s m h))
                                           (format nil "~d-~2,'0d-~2,'0d" year month day))))
  "Write (summary PROJECT) to data/profile-DATE.sexp in the project, replacing that day's earlier summary, and return the
   path. This is the file to commit; the log is not."
  (let ((path (merge-pathnames (format nil "data/profile-~a.sexp" date) (project-directory project))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
      (let ((*package* (find-package :keyword)) (*print-pretty* t) (*print-right-margin* 120))
        (prin1 (summary project) out)
        (terpri out)))
    path))
