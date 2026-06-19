;;; term-sessions-frontends.el --- Frontends for term-sessions -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Terminal frontend adapters and open commands.

;;; Code:

(require 'term)
(require 'term-sessions-core)
(require 'term-sessions-zmx)
(require 'term-sessions-tramp)

(declare-function comint-send-string "comint" (proc string))
(declare-function eat "ext:eat" (&optional program name))
(declare-function eat-semi-char-mode "eat" ())
(declare-function eat-make "eat" (name program &optional startfile &rest switches))
(declare-function ghostel-semi-char-mode "ghostel" ())
(declare-function ghostel-exec "ghostel" (buffer program &optional args))
(declare-function term-emulate-terminal "term" (proc string))
(declare-function term-generate-db-directory "term" ())
(declare-function term-sentinel "term" (proc msg))
(declare-function vterm "ext:vterm" (&optional buffer-name))
(declare-function term-sessions-list--session-rows "term-sessions-list" ())
(defvar term-height)
(defvar term-protocol-version)
(defvar term-ptyp)
(defvar term-set-terminal-size)
(defvar term-term-name)
(defvar term-termcap-format)
(defvar term-width)
(defvar vterm-shell)
(defvar vterm-buffer-name)
(defvar vterm-tramp-shells)
(defvar ghostel--process)
(defvar ghostel-buffer-name-function)
(defvar ghostel-set-title-function)

(defcustom term-sessions-ghostel-open-function #'term-sessions--ghostel-open-command
  "Function used to open an attach command with ghostel.
The function is called with two arguments: BUFFER-NAME and COMMAND.
This is intentionally pluggable because ghostel APIs are still evolving."
  :group 'term-sessions
  :type '(choice (const :tag "Disabled" nil) function))

(defun term-sessions--ghostel-open-command (buffer-name command)
  "Open shell COMMAND in a Ghostel BUFFER-NAME."
  (unless (require 'ghostel nil t)
    (user-error "ghostel is not available"))
  (let ((directory default-directory)
        (buffer (get-buffer-create buffer-name)))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (setq default-directory directory)
      (if (and (boundp 'ghostel--process)
               (process-live-p ghostel--process))
          (ghostel-semi-char-mode)
        (ghostel-exec buffer "/bin/sh" (list "-lc" command))
        (rename-buffer buffer-name t)
        ;; Semi-char mode sends ordinary text to the terminal while preserving
        ;; Ghostel/Emacs escape keybindings.  Char mode captures almost every
        ;; key, which makes normal Emacs navigation/control feel broken.
        (ghostel-semi-char-mode)))
    buffer))

(defun term-sessions--short-directory-name (directory)
  "Return compact display name for DIRECTORY."
  (let* ((localname (or (file-remote-p directory 'localname) directory))
         (short (abbreviate-file-name (directory-file-name localname))))
    (if (string-match "\\`/home/[^/]+\\(/.*\\)?\\'" short)
        (concat "~" (or (match-string 1 short) ""))
      short)))

(defun term-sessions--buffer-name-for-spec (spec)
  "Return interactive terminal buffer name for SPEC."
  (let* ((name (term-sessions-spec-name spec))
         (cwd (term-sessions-spec-cwd spec))
         (location (term-sessions-spec-location spec))
         (where (if (term-sessions-location-remote-p location)
                    (format "[%s] " (term-sessions--location-remote-label location))
                  ""))
         (dir (term-sessions--short-directory-name cwd)))
    (format "*term-session:%s: %s%s*" name where dir)))

(defun term-sessions--buffer-name-for-title (name title)
  "Return term-session buffer name for NAME using Ghostel TITLE."
  (format "*term-session:%s: %s*" name (string-trim (or title ""))))

(defun term-sessions--ghostel-buffer-name-for-title (name fallback-name title)
  "Return Ghostel buffer name for session NAME, FALLBACK-NAME, and TITLE."
  (let ((title (string-trim (or title ""))))
    (if (string-empty-p title)
        fallback-name
      (term-sessions--buffer-name-for-title name title))))

(defun term-sessions--install-ghostel-title-tracking (name fallback-name)
  "Keep Ghostel buffer title tracking while prefixing with session NAME."
  (if (boundp 'ghostel-buffer-name-function)
      ;; Newer Ghostel calls this function for both title changes and OSC 7
      ;; directory reports.  On directory-only updates the title can be nil, so
      ;; return the fallback buffer name instead of calling `string-trim' on nil.
      (setq-local ghostel-buffer-name-function
                  (lambda (title)
                    (term-sessions--ghostel-buffer-name-for-title
                     name fallback-name title)))
    ;; Older Ghostel exposed `ghostel-set-title-function' as an effectful hook.
    (setq-local ghostel-set-title-function
                (lambda (title)
                  (rename-buffer
                   (term-sessions--ghostel-buffer-name-for-title
                    name fallback-name title)
                   t)))))

(defun term-sessions--open-vterm (name command buffer-name &optional spec)
  "Open COMMAND in vterm BUFFER-NAME for session NAME."
  (unless (require 'vterm nil t)
    (user-error "vterm is not available"))
  (let* ((remote-method (file-remote-p default-directory 'method))
         ;; vterm inserts `vterm-shell' after an `exec' in a shell wrapper.
         ;; Use a real shell command there so compound attach setup fragments
         ;; (for example setting SHELL from passwd) are interpreted correctly.
         (vterm-command (term-sessions--command-string
                         (if remote-method "/bin/sh" shell-file-name)
                         (list shell-command-switch command)))
         (vterm-shell vterm-command)
         (vterm-buffer-name buffer-name)
         (vterm-tramp-shells (if remote-method
                                 (append (list (list remote-method vterm-command)
                                               (list t vterm-command))
                                         vterm-tramp-shells)
                               vterm-tramp-shells)))
    (vterm buffer-name)
    (term-sessions--mark-buffer name spec)))

(defun term-sessions--open-eat (name command buffer-name &optional spec)
  "Open COMMAND in eat BUFFER-NAME for session NAME."
  (unless (require 'eat nil t)
    (user-error "eat is not available"))
  (let* ((directory default-directory)
         (base-name (term-sessions--terminal-buffer-base-name name buffer-name))
         (target-buffer (get-buffer-create (format "*%s*" base-name))))
    (with-current-buffer target-buffer
      (setq default-directory directory))
    (let ((buffer (eat-make base-name "/usr/bin/env" nil "sh" "-c" command)))
      (pop-to-buffer buffer)
      (with-current-buffer buffer
        (eat-semi-char-mode)
        (term-sessions--mark-buffer name spec)))))

(defun term-sessions--terminal-buffer-base-name (name buffer-name)
  "Return a terminal base name for NAME from BUFFER-NAME.
Terminal constructors such as `make-term' and `eat-make' add their own
surrounding stars, so BUFFER-NAME is stripped when present."
  (if (term-sessions--string-or-nil buffer-name)
      (string-trim buffer-name "\\*" "\\*")
    (term-sessions--term-buffer-base-name name)))

(defun term-sessions--open-term (name command buffer-name &optional spec)
  "Open COMMAND in built-in term BUFFER-NAME for session NAME."
  (let ((buffer (apply #'make-term (term-sessions--terminal-buffer-base-name
                                    name buffer-name)
                       shell-file-name nil
                       (list shell-command-switch command))))
    (pop-to-buffer buffer)
    (term-mode)
    (term-char-mode)
    (term-sessions--mark-buffer name spec)))

(defun term-sessions--open-term-process (name program args buffer-name &optional spec)
  "Open PROGRAM with ARGS in a built-in term buffer for session NAME.
This starts PROGRAM with `start-file-process' and term-mode plumbing rather
than wrapping the attach in a shell command, so a remote `default-directory'
can be handled by TRAMP or tramp-rpc process file handlers."
  (let* ((base-name (term-sessions--terminal-buffer-base-name name buffer-name))
         (buffer-name (format "*%s*" base-name))
         (directory default-directory)
         (buffer (get-buffer-create buffer-name)))
    (unless (term-check-proc buffer)
      (with-current-buffer buffer
        (setq default-directory directory)
        (term-mode)
        (when-let ((proc (get-buffer-process buffer)))
          (delete-process proc))
        (let* ((process-environment
                (nconc
                 (list
                  (format "TERM=%s" term-term-name)
                  (format "TERMINFO=%s" (term-generate-db-directory))
                  (format term-termcap-format "TERMCAP="
                          term-term-name term-height term-width)
                  (format "INSIDE_EMACS=%s,term:%s"
                          emacs-version term-protocol-version))
                 (when term-set-terminal-size
                   (list (format "LINES=%d" term-height)
                         (format "COLUMNS=%d" term-width)))
                 process-environment))
               (process-connection-type t)
               (inhibit-eol-conversion t)
               (coding-system-for-read 'binary)
               (proc (apply #'start-file-process
                            base-name
                            buffer program args)))
          (setq-local term-ptyp process-connection-type)
          (goto-char (point-max))
          (set-marker (process-mark proc) (point))
          (set-process-filter proc #'term-emulate-terminal)
          (set-process-sentinel proc #'term-sentinel))))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (term-char-mode)
      (term-sessions--mark-buffer name spec))
    buffer))

(defun term-sessions--open-tramp-process (name command frontend buffer-name &optional spec)
  "Attach to NAME through TRAMP using FRONTEND.
COMMAND is the optional zmx creation command for missing sessions."
  (term-sessions-zmx--with-environment
    (let ((attach-command (term-sessions--attach-command name command t)))
      (pcase frontend
        ('eat (term-sessions--open-eat name attach-command buffer-name spec))
        ('term (term-sessions--open-term-process name "/bin/sh" (list "-lc" attach-command)
                                                 buffer-name spec))
        ('ghostel (term-sessions--open-ghostel name attach-command buffer-name spec))
        ('vterm (term-sessions--open-vterm name attach-command buffer-name spec))
        ('shell (term-sessions--open-shell name attach-command buffer-name spec))
        (_ (user-error "Frontend `%s' cannot attach through TRAMP process APIs" frontend))))))

(defun term-sessions--open-shell (name command buffer-name &optional spec)
  "Open COMMAND in an ordinary shell BUFFER-NAME for session NAME."
  (let ((buffer (shell buffer-name)))
    (with-current-buffer buffer
      (comint-send-string buffer (concat command "\n"))
      (term-sessions--mark-buffer name spec))))

(defun term-sessions--open-command-frontend (name command frontend buffer-name &optional spec)
  "Open COMMAND for session NAME using command-string FRONTEND."
  (pcase frontend
    ('vterm (term-sessions--open-vterm name command buffer-name spec))
    ('eat (term-sessions--open-eat name command buffer-name spec))
    ('ghostel (term-sessions--open-ghostel name command buffer-name spec))
    ('term (term-sessions--open-term name command buffer-name spec))
    ('shell (term-sessions--open-shell name command buffer-name spec))
    (_ (user-error "Unknown frontend: %S" frontend))))

(defun term-sessions--open-ghostel (name command buffer-name &optional spec)
  "Open COMMAND in ghostel BUFFER-NAME for session NAME."
  (unless term-sessions-ghostel-open-function
    (user-error "Set `term-sessions-ghostel-open-function' before using ghostel"))
  (let ((buffer (or (funcall term-sessions-ghostel-open-function buffer-name command)
                    (current-buffer))))
    (with-current-buffer buffer
      (term-sessions--mark-buffer name spec)
      (term-sessions--install-ghostel-title-tracking name buffer-name)
      (rename-buffer buffer-name t))))

(defun term-sessions--read-session-entry (&optional prompt require-existing)
  "Read a session entry, including already-open TRAMP remotes.
When REQUIRE-EXISTING is non-nil, require the selected session to exist.
Otherwise, a non-matching name creates a new entry in `default-directory', like
`find-file' does for files."
  (if (fboundp 'term-sessions-list--session-rows)
      (progn
        (clrhash term-sessions--completion-entry-table)
        (let* ((entries (mapcar #'car (term-sessions-list--session-rows)))
               (candidates
                (mapcar (lambda (entry)
                          (term-sessions--register-completion-entry
                           (format "%s @ %s %s"
                                   (plist-get entry :name)
                                   (plist-get entry :where)
                                   (abbreviate-file-name
                                    (or (plist-get entry :cwd) "")))
                           entry))
                        entries))
               (table (lambda (string predicate action)
                        (if (eq action 'metadata)
                            `(metadata
                              (category . term-session)
                              (annotation-function . term-sessions--completion-annotate))
                          (complete-with-action action candidates string predicate))))
               (selected (completing-read (or prompt "Session: ") table nil require-existing
                                          nil 'term-sessions-name-history)))
          (or (term-sessions--completion-entry selected)
              (and (not require-existing)
                   (list :name (substring-no-properties selected)
                         :directory default-directory))
              (user-error "No term session selected"))))
    (list :name (term-sessions--read-name prompt require-existing)
          :directory default-directory)))

(defun term-sessions--read-existing-session-entry (&optional prompt)
  "Read an existing session entry, including already-open TRAMP remotes."
  (term-sessions--read-session-entry prompt t))

(defun term-sessions--pop-existing-session-buffer (name directory &optional backend)
  "Pop to an existing term-session buffer for NAME at DIRECTORY.
Return the buffer when one was found, otherwise nil."
  (when-let ((buffer (term-sessions--session-buffer name directory backend)))
    (pop-to-buffer buffer)
    buffer))

;;;###autoload
(defun term-sessions-open-with-frontend (name &optional command frontend allow-create)
  "Open zmx session NAME with optional creation COMMAND in FRONTEND.
NAME may also be a session entry plist.  When ALLOW-CREATE is nil, require the
session to already exist according to zmx in the entry/current directory."
  (interactive
   (list (term-sessions--read-existing-session-entry "Open session: ")
         nil
         (intern (completing-read "Frontend: " '("vterm" "eat" "ghostel" "term" "shell")
                                  nil t nil nil
                                  (symbol-name term-sessions-preferred-frontend)))
         nil))
  (let* ((entry (and (consp name) name))
         (default-directory (if entry
                                (term-sessions--entry-directory entry)
                              default-directory))
         (name (term-sessions--entry-name name)))
    (or (term-sessions--pop-existing-session-buffer
         name default-directory term-sessions-backend)
        (progn
          (term-sessions--ensure-zmx)
          (let* ((frontend (or frontend term-sessions-preferred-frontend))
                 (spec (term-sessions-spec-current name command frontend))
                 (transport (term-sessions--ensure-interactive-attach-supported
                             (term-sessions-spec-location spec) frontend)))
            (unless (or allow-create (term-sessions--active-p name))
              (user-error "No active zmx session named `%s'" name))
            (let* ((buffer-name (term-sessions--buffer-name-for-spec spec))
                   (attach-command (unless (eq transport 'tramp-process)
                                     (term-sessions--interactive-attach-command name command))))
              (pcase transport
                ('tramp-process
                 (term-sessions--open-tramp-process
                  name command frontend buffer-name spec))
                ('local
                 (term-sessions--open-command-frontend
                  name attach-command frontend buffer-name spec)))))))))

;;;###autoload
(defun term-sessions-open (name &optional command)
  "Open persistent zmx session NAME, creating it when missing.
NAME may also be a session entry plist with a `:directory'.  With prefix
argument, prompt for COMMAND to run when the session is created.  Without
COMMAND, zmx starts a login shell for new sessions."
  (interactive
   (list (term-sessions--read-session-entry "Open session: " nil)
         (when current-prefix-arg
           (read-string "Command for new session: " nil 'term-sessions-command-history))))
  (term-sessions-open-with-frontend name command term-sessions-preferred-frontend t))

(provide 'term-sessions-frontends)
;;; term-sessions-frontends.el ends here
