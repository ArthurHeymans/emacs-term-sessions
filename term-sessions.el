;;; term-sessions.el --- Persistent terminal sessions via zmx -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Arthur
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: terminals, processes, tools
;; URL: https://github.com/arthur/term-sessions

;;; Commentary:

;; A small greenfield Emacs frontend for persistent terminal sessions.
;; The first backend is zmx: https://github.com/neurosnap/zmx
;;
;; Sessions are owned by zmx, not by Emacs.  Emacs shells out for control
;; operations and opens interactive attaches through a selected terminal
;; frontend such as vterm, eat, term, or shell.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'term)
(require 'url-util)

(declare-function comint-send-string "comint" (proc string))
(declare-function eat "ext:eat" (&optional program name))
(declare-function org-link-set-parameters "ol" (type &rest parameters))
(declare-function org-link-store-props "ol" (&rest plist))
(declare-function vterm "ext:vterm" (&optional buffer-name))
(defvar vterm-shell)
(defvar vterm-buffer-name)

(defgroup term-sessions nil
  "Persistent terminal sessions owned by an external backend."
  :group 'terminals
  :prefix "term-sessions-")

(defcustom term-sessions-backend 'zmx
  "Backend used by `term-sessions'.
Currently only `zmx' is implemented."
  :type '(choice (const :tag "zmx" zmx)))

(defcustom term-sessions-zmx-program "zmx"
  "Program name or absolute path for zmx."
  :type 'string)

(defcustom term-sessions-preferred-frontend 'term
  "Preferred terminal frontend for interactive session attaches."
  :type '(choice (const :tag "vterm" vterm)
                 (const :tag "eat" eat)
                 (const :tag "ghostel" ghostel)
                 (const :tag "term" term)
                 (const :tag "shell" shell)))

(defcustom term-sessions-ghostel-open-function nil
  "Function used to open an attach command with ghostel.
The function is called with two arguments: BUFFER-NAME and COMMAND.
This is intentionally pluggable because ghostel APIs are still evolving."
  :type '(choice (const :tag "Disabled" nil) function))

(defcustom term-sessions-history-lines 200
  "Default number of history lines to show from zmx history commands."
  :type 'integer)

(defvar term-sessions-name-history nil)
(defvar term-sessions-command-history nil)

(defvar-local term-sessions-current-name nil)
(defvar-local term-sessions-current-backend nil)

(defun term-sessions--ensure-zmx ()
  "Signal an error unless zmx is clearly unavailable.
For remote `default-directory' values, defer to `process-file' and the
remote file handler so the remote PATH and connection-local settings apply."
  (unless (or (file-remote-p default-directory)
              (executable-find term-sessions-zmx-program))
    (user-error "Cannot find zmx executable `%s'" term-sessions-zmx-program)))

(defun term-sessions--command-string (program args)
  "Return shell command string for PROGRAM and ARGS."
  (mapconcat #'shell-quote-argument (cons program args) " "))

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
  (let ((file (make-temp-file "term-sessions-stdin-")))
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
  (term-sessions--ensure-zmx)
  (apply #'term-sessions--call term-sessions-zmx-program args))

(defun term-sessions--zmx-with-stdin (stdin &rest args)
  "Run zmx with STDIN and ARGS and return stdout."
  (term-sessions--ensure-zmx)
  (apply #'term-sessions--call-with-stdin term-sessions-zmx-program stdin args))

(defun term-sessions--zmx-list-names ()
  "Return a list of active zmx session names."
  (let ((output (condition-case nil
                    (term-sessions--zmx "list" "--short")
                  (error (term-sessions--zmx "list")))))
    (seq-filter (lambda (line) (not (string-empty-p line)))
                (mapcar #'string-trim (split-string output "\n" t)))))

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

(defun term-sessions--attach-command (name &optional command)
  "Return a shell command that attaches to zmx session NAME.
When COMMAND is non-nil, it is passed to zmx for session creation if needed."
  (term-sessions--command-string term-sessions-zmx-program
                                 (term-sessions--attach-args name command)))

(defun term-sessions--buffer-name (name &optional suffix)
  "Return a buffer name for session NAME and optional SUFFIX."
  (format "*term-session:%s%s*" name (if suffix (format ":%s" suffix) "")))

(defun term-sessions--term-buffer-base-name (name)
  "Return a bare buffer base name for `make-term' session NAME.
`make-term' adds the surrounding stars itself."
  (format "term-session:%s" name))

(defun term-sessions--ensure-local-interactive-attach ()
  "Refuse interactive attach from remote `default-directory' for now.
Control commands use `process-file' and may run through TRAMP, but the current
frontend adapters would run shell command strings locally or otherwise have
unvalidated remote PTY semantics."
  (when (file-remote-p default-directory)
    (user-error "Interactive zmx attach from remote default-directory is not supported yet; use zmx control commands or attach from a local buffer")))

(defun term-sessions--mark-buffer (name)
  "Record NAME/backend metadata in the current buffer."
  (setq-local term-sessions-current-name name)
  (setq-local term-sessions-current-backend term-sessions-backend))

(defun term-sessions--open-vterm (name command buffer-name)
  "Open COMMAND in vterm BUFFER-NAME for session NAME."
  (unless (require 'vterm nil t)
    (user-error "vterm is not available"))
  (let ((vterm-shell command)
        (vterm-buffer-name buffer-name))
    (vterm buffer-name)
    (term-sessions--mark-buffer name)))

(defun term-sessions--open-eat (name command buffer-name)
  "Open COMMAND in eat BUFFER-NAME for session NAME."
  (unless (require 'eat nil t)
    (user-error "eat is not available"))
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (setq default-directory default-directory))
    (eat command buffer-name)
    (term-sessions--mark-buffer name)))

(defun term-sessions--open-term (name command _buffer-name)
  "Open COMMAND in built-in term buffer for session NAME."
  (let ((buffer (apply #'make-term (term-sessions--term-buffer-base-name name)
                       shell-file-name nil
                       (list shell-command-switch command))))
    (pop-to-buffer buffer)
    (term-mode)
    (term-char-mode)
    (term-sessions--mark-buffer name)))

(defun term-sessions--open-shell (name command buffer-name)
  "Open COMMAND in an ordinary shell BUFFER-NAME for session NAME."
  (let ((buffer (shell buffer-name)))
    (with-current-buffer buffer
      (comint-send-string buffer (concat command "\n"))
      (term-sessions--mark-buffer name))))

(defun term-sessions--open-ghostel (name command buffer-name)
  "Open COMMAND in ghostel BUFFER-NAME for session NAME."
  (unless term-sessions-ghostel-open-function
    (user-error "Set `term-sessions-ghostel-open-function' before using ghostel"))
  (funcall term-sessions-ghostel-open-function buffer-name command)
  (term-sessions--mark-buffer name))

(defun term-sessions--active-p (name)
  "Return non-nil when zmx session NAME is active."
  (member name (term-sessions--zmx-list-names)))

(defun term-sessions-open-with-frontend (name &optional command frontend allow-create)
  "Open zmx session NAME with optional creation COMMAND in FRONTEND.
When ALLOW-CREATE is nil, require NAME to already exist according to zmx."
  (interactive
   (list (term-sessions--read-name "Open session: " t)
         nil
         (intern (completing-read "Frontend: " '("vterm" "eat" "ghostel" "term" "shell")
                                  nil t nil nil
                                  (symbol-name term-sessions-preferred-frontend)))
         nil))
  (term-sessions--ensure-zmx)
  (term-sessions--ensure-local-interactive-attach)
  (unless (or allow-create (term-sessions--active-p name))
    (user-error "No active zmx session named `%s'; use `term-sessions-start' to create it" name))
  (let* ((frontend (or frontend term-sessions-preferred-frontend))
         (attach-command (term-sessions--attach-command name command))
         (buffer-name (term-sessions--buffer-name name)))
    (pcase frontend
      ('vterm (term-sessions--open-vterm name attach-command buffer-name))
      ('eat (term-sessions--open-eat name attach-command buffer-name))
      ('ghostel (term-sessions--open-ghostel name attach-command buffer-name))
      ('term (term-sessions--open-term name attach-command buffer-name))
      ('shell (term-sessions--open-shell name attach-command buffer-name))
      (_ (user-error "Unknown frontend: %S" frontend)))))

;;;###autoload
(defun term-sessions-start (name &optional command)
  "Create or open persistent zmx session NAME.
With prefix argument, prompt for COMMAND to run when the session is created.
Without COMMAND, zmx starts a login shell."
  (interactive
   (list (term-sessions--read-name "Start/open session: " nil)
         (when current-prefix-arg
           (read-string "Command for new session: " nil 'term-sessions-command-history))))
  (term-sessions-open-with-frontend name command term-sessions-preferred-frontend t))

;;;###autoload
(defun term-sessions-open (name)
  "Open existing zmx session NAME.
This command does not create sessions; use `term-sessions-start' for
create-or-attach behavior."
  (interactive (list (term-sessions--read-name "Open session: " t)))
  (term-sessions-open-with-frontend name nil term-sessions-preferred-frontend nil))

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
(defun term-sessions-wait (name)
  "Wait for tracked zmx tasks in session NAME to finish."
  (interactive (list (term-sessions--read-name "Wait for session: " t)))
  (message "%s" (string-trim (term-sessions--zmx "wait" name))))

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
    (start-file-process buffer-name buffer term-sessions-zmx-program "tail" name)
    (with-current-buffer buffer
      (term-sessions--mark-buffer name)
      (view-mode 1))
    (pop-to-buffer buffer)))

(defvar term-sessions-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'term-sessions-list-open)
    (define-key map (kbd "g") #'revert-buffer)
    (define-key map (kbd "k") #'term-sessions-list-kill)
    (define-key map (kbd "h") #'term-sessions-list-history)
    (define-key map (kbd "t") #'term-sessions-list-tail)
    map)
  "Keymap for `term-sessions-list-mode'.")

(define-derived-mode term-sessions-list-mode tabulated-list-mode "Term-Sessions"
  "Major mode for listing persistent terminal sessions."
  (setq tabulated-list-format [ ("Name" 32 t) ("Backend" 10 t) ])
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'term-sessions-list-refresh nil t)
  (tabulated-list-init-header))

(defun term-sessions-list-refresh ()
  "Refresh `term-sessions-list-mode' rows."
  (setq tabulated-list-entries
        (mapcar (lambda (name)
                  (list name (vector name (symbol-name term-sessions-backend))))
                (term-sessions--zmx-list-names))))

;;;###autoload
(defun term-sessions-list ()
  "List active zmx sessions."
  (interactive)
  (let ((buffer (get-buffer-create "*term-sessions*")))
    (with-current-buffer buffer
      (term-sessions-list-mode)
      (term-sessions-list-refresh)
      (tabulated-list-print t))
    (pop-to-buffer buffer)))

(defun term-sessions-list--name-at-point ()
  "Return session name at point in `term-sessions-list-mode'."
  (or (tabulated-list-get-id)
      (user-error "No session on this line")))

(defun term-sessions-list-open ()
  "Open session at point."
  (interactive)
  (term-sessions-open (term-sessions-list--name-at-point)))

(defun term-sessions-list-kill ()
  "Kill session at point."
  (interactive)
  (term-sessions-kill (term-sessions-list--name-at-point))
  (revert-buffer))

(defun term-sessions-list-history ()
  "Show history for session at point."
  (interactive)
  (term-sessions-history (term-sessions-list--name-at-point)))

(defun term-sessions-list-tail ()
  "Tail session at point."
  (interactive)
  (term-sessions-tail (term-sessions-list--name-at-point)))

(defun term-sessions--org-link (backend name)
  "Build an Org link for BACKEND session NAME."
  (format "term-session:%s:local:%s" backend (url-hexify-string name)))

;;;###autoload
(defun term-sessions-store-org-link (&optional name)
  "Store an Org link to persistent session NAME.
When called from a session buffer, default to that session."
  (interactive)
  (when (file-remote-p default-directory)
    (user-error "Storing remote term-session Org links is not supported in this MVP"))
  (let* ((name (or name term-sessions-current-name
                   (term-sessions--read-name "Store link for session: " t)))
         (backend (or term-sessions-current-backend term-sessions-backend))
         (link (term-sessions--org-link backend name))
         (description (format "terminal session:%s" name)))
    (if (fboundp 'org-link-store-props)
        (org-link-store-props :type "term-session"
                              :link link
                              :description description)
      (kill-new (format "[[%s][%s]]" link description)))
    (message "Stored %s" link)
    link))

(defun term-sessions--org-path-components (path)
  "Parse Org term-session link PATH.
Return a list (BACKEND LOCATION NAME)."
  (pcase-let ((`(,backend ,location . ,rest) (split-string path ":")))
    (unless (and backend location rest)
      (user-error "Invalid term-session link: %s" path))
    (list backend location (url-unhex-string (string-join rest ":")))))

(defun term-sessions--open-org-path (path _arg)
  "Open Org term-session link PATH.
Open only active sessions automatically.  If the linked session is missing,
offer to recreate it with `term-sessions-start'."
  (pcase-let ((`(,backend ,location ,name) (term-sessions--org-path-components path)))
    (unless (string= backend "zmx")
      (user-error "Unsupported term-session backend: %s" backend))
    (unless (string= location "local")
      (user-error "Unsupported term-session location: %s" location))
    (if (term-sessions--active-p name)
        (term-sessions-open name)
      (when (yes-or-no-p (format "Session `%s' is not active; recreate it? " name))
        (term-sessions-start name)))))

(with-eval-after-load 'org
  (org-link-set-parameters "term-session"
                           :follow #'term-sessions--open-org-path
                           :store #'term-sessions-store-org-link))

(provide 'term-sessions)
;;; term-sessions.el ends here
