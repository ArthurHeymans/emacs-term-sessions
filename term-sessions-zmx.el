;;; term-sessions-zmx.el --- zmx backend for term-sessions -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; zmx control operations and user-facing backend commands.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'term-sessions-core)

(defcustom term-sessions-zmx-program "zmx"
  "Program name or absolute path for zmx."
  :group 'term-sessions
  :type 'string)

(defcustom term-sessions-zmx-dir nil
  "Optional value for the ZMX_DIR environment variable.
This variable is connection-local aware."
  :group 'term-sessions
  :type '(choice (const :tag "Unset" nil) string))

(defcustom term-sessions-zmx-session-prefix nil
  "Optional value for the ZMX_SESSION_PREFIX environment variable.
This variable is connection-local aware."
  :group 'term-sessions
  :type '(choice (const :tag "Unset" nil) string))

(defcustom term-sessions-zmx-enrich-process-info t
  "When non-nil, enrich `zmx list' rows with live procfs cwd/command info.
This is best-effort and currently works on Linux hosts with `/proc' and `ps'."
  :group 'term-sessions
  :type 'boolean)

(defmacro term-sessions-zmx--with-environment (&rest body)
  "Run BODY with connection-local zmx variables and environment."
  (declare (indent 0) (debug t))
  `(with-connection-local-variables
     (let ((process-environment (copy-sequence process-environment)))
       (when term-sessions-zmx-dir
         (setenv "ZMX_DIR" term-sessions-zmx-dir))
       (when term-sessions-zmx-session-prefix
         (setenv "ZMX_SESSION_PREFIX" term-sessions-zmx-session-prefix))
       ,@body)))

(defun term-sessions--ensure-zmx ()
  "Signal an error unless zmx is clearly unavailable.
For remote `default-directory' values, defer to `process-file' and the
remote file handler so the remote PATH and connection-local settings apply."
  (term-sessions-zmx--with-environment
    (unless (or (file-remote-p default-directory)
                (executable-find term-sessions-zmx-program))
      (user-error "Cannot find zmx executable `%s'" term-sessions-zmx-program))))

(defun term-sessions--call (program &rest args)
  "Run PROGRAM with ARGS and return stdout as a string.
Signals `user-error' on a non-zero exit status.  The current
`default-directory' is respected, including TRAMP directories where
`process-file' supports them."
  (with-temp-buffer
    (let ((status (apply #'process-file program nil t nil args)))
      (unless (eq status 0)
        (user-error "%s %s failed: %s"
                    program (string-join args " ")
                    (string-trim (buffer-string))))
      (buffer-string))))

(defun term-sessions--call-with-stdin (program stdin &rest args)
  "Run PROGRAM with STDIN and ARGS, returning stdout as a string."
  (let ((file (make-temp-file (expand-file-name "term-sessions-stdin-"
                                                (if (file-remote-p default-directory)
                                                    default-directory
                                                  temporary-file-directory)))))
    (unwind-protect
        (progn
          (with-temp-file file (insert stdin))
          (with-temp-buffer
            (let ((status (apply #'process-file program file t nil args)))
              (unless (eq status 0)
                (user-error "%s %s failed: %s"
                            program (string-join args " ")
                            (string-trim (buffer-string))))
              (buffer-string))))
      (ignore-errors (delete-file file)))))

(defun term-sessions--zmx (&rest args)
  "Run zmx with ARGS and return stdout."
  (term-sessions-zmx--with-environment
    (term-sessions--ensure-zmx)
    (apply #'term-sessions--call term-sessions-zmx-program args)))

(defun term-sessions--zmx-with-stdin (stdin &rest args)
  "Run zmx with STDIN and ARGS and return stdout."
  (term-sessions-zmx--with-environment
    (term-sessions--ensure-zmx)
    (apply #'term-sessions--call-with-stdin term-sessions-zmx-program stdin args)))

(defun term-sessions--start-zmx-process (name buffer &rest args)
  "Start asynchronous zmx process NAME in BUFFER with ARGS."
  (term-sessions-zmx--with-environment
    (term-sessions--ensure-zmx)
    (apply #'start-file-process name buffer term-sessions-zmx-program args)))

(defun term-sessions--zmx-list-names ()
  "Return a list of active zmx session names."
  (let ((output (condition-case nil
                    (term-sessions--zmx "list" "--short")
                  (error (term-sessions--zmx "list")))))
    (seq-filter (lambda (line) (not (string-empty-p line)))
                (mapcar #'string-trim (split-string output "\n" t)))))

(defun term-sessions--parse-key-value-fields (line)
  "Parse tab-separated key=value fields from LINE into a plist."
  (let (plist)
    (dolist (field (split-string (string-trim line) "\t" t))
      (when (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" field)
        (let ((key (intern (concat ":" (match-string 1 field))))
              (value (match-string 2 field)))
          (setq plist (plist-put plist key value)))))
    plist))

(defun term-sessions--zmx-version-info ()
  "Return parsed `zmx version' output as a plist."
  (let (plist)
    (dolist (line (split-string (term-sessions--zmx "version") "\n" t))
      (when (string-match "\\`\\([^[:space:]]+\\)[[:space:]]+\\(.*\\)\\'" line)
        (setq plist (plist-put plist
                               (intern (concat ":" (match-string 1 line)))
                               (match-string 2 line)))))
    plist))

(defun term-sessions--remote-absolute-file (path)
  "Return PATH qualified with the current TRAMP prefix when remote."
  (if-let ((remote (file-remote-p default-directory)))
      (concat remote path)
    path))

(defun term-sessions--zmx-log-dir ()
  "Return zmx log directory, or nil if unavailable."
  (or term-sessions-zmx-dir
      (condition-case nil
          (plist-get (term-sessions--zmx-version-info) :log_dir)
        (error nil))))

(defun term-sessions--zmx-log-mtime (name)
  "Return modification time for zmx session NAME log, or nil."
  (when-let ((log-dir (term-sessions--zmx-log-dir)))
    (let* ((log-file (expand-file-name (concat name ".log") log-dir))
           (qualified (term-sessions--remote-absolute-file log-file)))
      (when-let ((attrs (ignore-errors (file-attributes qualified))))
        (file-attribute-modification-time attrs)))))

(defun term-sessions--process-output-string (program &rest args)
  "Run PROGRAM with ARGS and return trimmed stdout, or nil on failure."
  (ignore-errors
    (with-temp-buffer
      (let ((status (apply #'process-file program nil t nil args)))
        (when (eq status 0)
          (string-trim (buffer-string)))))))

(defun term-sessions--zmx-process-cwd (pid)
  "Return current working directory for process PID, or nil."
  (let ((cwd (term-sessions--process-output-string
              "readlink" "-f" (format "/proc/%s/cwd" pid))))
    (unless (string-empty-p (or cwd ""))
      cwd)))

(defun term-sessions--zmx-process-args (pid)
  "Return command line for process PID, or nil."
  (let ((args (term-sessions--process-output-string
               "ps" "-o" "args=" "-p" (format "%s" pid))))
    (unless (string-empty-p (or args ""))
      args)))

(defun term-sessions--zmx-process-tpgid (pid)
  "Return foreground process group id for process PID, or nil."
  (when-let ((output (term-sessions--process-output-string
                      "ps" "-o" "tpgid=" "-p" (format "%s" pid))))
    (let ((tpgid (string-to-number output)))
      (when (> tpgid 0)
        tpgid))))

(defun term-sessions--zmx-current-command (pid)
  "Return best-effort current foreground command for zmx session PID."
  (let ((tpgid (term-sessions--zmx-process-tpgid pid)))
    (or (and tpgid (term-sessions--zmx-process-args tpgid))
        (term-sessions--zmx-process-args pid))))

(defun term-sessions--zmx-enrich-session-process-info (session)
  "Add live cwd/command fields to SESSION when available."
  (if (not term-sessions-zmx-enrich-process-info)
      session
    (if-let ((pid (plist-get session :pid)))
        (let ((cwd (term-sessions--zmx-process-cwd pid))
              (cmd (term-sessions--zmx-current-command pid)))
          (when cwd
            (setq session (plist-put session :cwd cwd)))
          (when cmd
            (setq session (plist-put session :current-cmd cmd)))
          session)
      session)))

(defun term-sessions--zmx-list-sessions ()
  "Return detailed active zmx sessions as plists.
Fields include at least :name, and may include :pid, :clients, :created,
:start_dir, :cmd, and :updated-time."
  (let ((output (condition-case nil
                    (term-sessions--zmx "list")
                  (error ""))))
    (delq nil
          (mapcar
           (lambda (line)
             (unless (or (string-empty-p (string-trim line))
                         (string-prefix-p "no sessions found" line))
               (let ((entry (term-sessions--parse-key-value-fields line)))
                 (when-let ((name (plist-get entry :name)))
                   (plist-put entry :updated-time
                              (term-sessions--zmx-log-mtime name))
                   (term-sessions--zmx-enrich-session-process-info entry)))))
           (split-string output "\n" t)))))

(defun term-sessions--active-p (name)
  "Return non-nil when zmx session NAME is active."
  (member name (term-sessions--zmx-list-names)))

(defun term-sessions--read-name (&optional prompt require-existing)
  "Read a session name with PROMPT.
When REQUIRE-EXISTING is non-nil, complete against active sessions."
  (let ((prompt (or prompt "Session: ")))
    (if require-existing
        (completing-read prompt (term-sessions--zmx-list-names)
                         nil t nil 'term-sessions-name-history)
      (read-string prompt nil 'term-sessions-name-history))))

(defun term-sessions--attach-args (name command)
  "Return zmx attach arguments for NAME and optional COMMAND."
  (append (list "attach" name)
          (when (and command (not (string-empty-p command)))
            (split-string-and-unquote command))))

(defun term-sessions--login-shell-setup-command ()
  "Return shell fragment that initializes SHELL from the passwd database."
  "shell=$(getent passwd \"$(id -un)\" | cut -d: -f7 2>/dev/null); [ -n \"$shell\" ] && SHELL=$shell; export SHELL")

(defun term-sessions--attach-command (name &optional command prefer-login-shell)
  "Return a shell command that attaches to zmx session NAME.
When COMMAND is non-nil, it is passed to zmx for session creation if needed.
When PREFER-LOGIN-SHELL is non-nil and COMMAND is nil, initialize SHELL from
passwd and ask zmx to create missing sessions with that login shell."
  (term-sessions-zmx--with-environment
    (if (and prefer-login-shell (not (term-sessions--string-or-nil command)))
        (concat (term-sessions--login-shell-setup-command)
                " && "
                (shell-quote-argument term-sessions-zmx-program)
                " attach "
                (shell-quote-argument name)
                " \"$SHELL\" -l")
      (term-sessions--command-string term-sessions-zmx-program
                                     (term-sessions--attach-args name command)))))

;;;###autoload
(defun term-sessions-kill (name &optional force)
  "Kill zmx session NAME.
With prefix argument FORCE, pass --force to zmx."
  (interactive (list (term-sessions--read-name "Kill session: " t)
                     current-prefix-arg))
  (when (yes-or-no-p (format "Kill zmx session `%s'? " name))
    (apply #'term-sessions--zmx (append (list "kill" name)
                                        (when force (list "--force"))))
    (message "Killed zmx session %s" name)))

;;;###autoload
(defun term-sessions-send (name text)
  "Send raw TEXT to zmx session NAME.
No newline or carriage return is appended."
  (interactive
   (list (term-sessions--read-name "Send to session: " t)
         (read-string "Raw input: ")))
  (term-sessions--zmx-with-stdin text "send" name))

;;;###autoload
(defun term-sessions-send-command (name command)
  "Send COMMAND plus carriage return to zmx session NAME."
  (interactive
   (list (term-sessions--read-name "Send command to session: " t)
         (read-string "Command: " nil 'term-sessions-command-history)))
  (term-sessions-send name (concat command "\r")))

;;;###autoload
(defun term-sessions-run (name command &optional detached)
  "Run COMMAND in zmx session NAME.
With prefix argument DETACHED, pass -d and return immediately."
  (interactive
   (list (term-sessions--read-name "Run in session: " t)
         (read-string "Command: " nil 'term-sessions-command-history)
         current-prefix-arg))
  (let ((args (append (list "run" name)
                      (when detached (list "-d"))
                      (split-string-and-unquote command))))
    (message "%s" (string-trim (apply #'term-sessions--zmx args)))))

;;;###autoload
(defun term-sessions-run-async (name command)
  "Run COMMAND in zmx session NAME asynchronously.
This starts `zmx run NAME -d COMMAND...' and returns immediately.  Use
`term-sessions-wait-async' or `term-sessions-wait' to observe completion."
  (interactive
   (list (term-sessions--read-name "Run async in session: " t)
         (read-string "Command: " nil 'term-sessions-command-history)))
  (let* ((buffer (get-buffer-create (term-sessions--buffer-name name "run")))
         (args (append (list "run" name "-d")
                       (split-string-and-unquote command)))
         (proc (apply #'term-sessions--start-zmx-process
                      (format "term-session-run:%s" name) buffer args)))
    (set-process-sentinel
     proc
     (lambda (process event)
       (when (memq (process-status process) '(exit signal))
         (message "%s %s" (process-name process) (string-trim event)))))
    (message "Started async zmx run in %s" name)
    proc))

;;;###autoload
(defun term-sessions-wait (name)
  "Wait for tracked zmx tasks in session NAME to finish."
  (interactive (list (term-sessions--read-name "Wait for session: " t)))
  (message "%s" (string-trim (term-sessions--zmx "wait" name))))

;;;###autoload
(defun term-sessions-wait-async (name)
  "Wait for tracked zmx tasks in session NAME asynchronously."
  (interactive (list (term-sessions--read-name "Wait async for session: " t)))
  (let* ((buffer (get-buffer-create (term-sessions--buffer-name name "wait")))
         (proc (term-sessions--start-zmx-process
                (format "term-session-wait:%s" name) buffer "wait" name)))
    (set-process-sentinel
     proc
     (lambda (process event)
       (when (memq (process-status process) '(exit signal))
         (message "%s %s" (process-name process) (string-trim event)))))
    (pop-to-buffer buffer)
    proc))

;;;###autoload
(defun term-sessions-history (name &optional lines vt html)
  "Show zmx history for session NAME.
LINES limits displayed lines.  With one prefix argument use VT output;
with two prefix arguments use HTML output."
  (interactive
   (list (term-sessions--read-name "History for session: " t)
         term-sessions-history-lines
         (equal current-prefix-arg '(4))
         (equal current-prefix-arg '(16))))
  (let* ((format-arg (cond (html "--html") (vt "--vt")))
         (args (append (list "history" name) (when format-arg (list format-arg))))
         (output (apply #'term-sessions--zmx args))
         (display (if (and lines (> lines 0) (not (or vt html)))
                      (string-join (last (split-string output "\n") lines) "\n")
                    output))
         (buffer (get-buffer-create (term-sessions--buffer-name name "history"))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert display)
        (goto-char (point-min))
        (view-mode 1)
        (term-sessions--mark-buffer name)))
    (pop-to-buffer buffer)))

;;;###autoload
(defun term-sessions-tail (name)
  "Follow zmx output for session NAME in a comint buffer."
  (interactive (list (term-sessions--read-name "Tail session: " t)))
  (term-sessions--ensure-zmx)
  (let* ((buffer-name (term-sessions--buffer-name name "tail"))
         (buffer (get-buffer-create buffer-name)))
    (when (get-buffer-process buffer)
      (delete-process (get-buffer-process buffer)))
    (term-sessions-zmx--with-environment
      (start-file-process buffer-name buffer term-sessions-zmx-program "tail" name))
    (with-current-buffer buffer
      (term-sessions--mark-buffer name)
      (view-mode 1))
    (pop-to-buffer buffer)))

(provide 'term-sessions-zmx)
;;; term-sessions-zmx.el ends here
