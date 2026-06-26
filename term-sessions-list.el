;;; term-sessions-list.el --- Session list UI for term-sessions -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tabulated list UI for persistent terminal sessions.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'hl-line)
(require 'imenu)
(require 'tramp)
(require 'term-sessions-core)
(require 'term-sessions-zmx)
(require 'term-sessions-frontends)
(require 'term-sessions-org)

(defcustom term-sessions-list-include-open-remotes t
  "When non-nil, `term-sessions-list' includes all open TRAMP remotes.
The list always includes the local zmx server.  Open remotes are discovered via
`tramp-list-connections', so this does not scan your network; it only queries
TRAMP connections Emacs already knows about."
  :group 'term-sessions
  :type 'boolean)

(defcustom term-sessions-list-failed-remote-retry-delay 300
  "Seconds before `term-sessions-list' retries a failed remote query.
When nil, failed remotes are retried on every refresh.  When 0, a failed remote
is skipped until `term-sessions-list-clear-failed-remotes' is called."
  :group 'term-sessions
  :type '(choice (const :tag "Retry every refresh" nil)
                 (const :tag "Skip until cleared" 0)
                 (integer :tag "Retry delay in seconds")))

(defcustom term-sessions-list-remote-query-timeout 10
  "Seconds to wait for an asynchronous remote list query.
When nil, remote `zmx list' processes are allowed to run until they finish.
Timed-out remotes are remembered in the failed-remote cache so a broken TRAMP
connection does not keep distracting later list refreshes."
  :group 'term-sessions
  :type '(choice (const :tag "No timeout" nil)
                 (number :tag "Seconds")))

(defvar term-sessions-list--failed-remotes (make-hash-table :test #'equal)
  "Remote directories that recently failed during `term-sessions-list' refresh.")

(defvar-local term-sessions-list--marked-entries nil
  "Entries marked in the current `term-sessions-list-mode' buffer.")

(defvar-local term-sessions-list--narrow-criteria nil
  "Composable narrowing criteria active in the current list buffer.")

(defvar-local term-sessions-list--all-entries nil
  "Unfiltered tabulated entries for the current list refresh.")

(defvar-local term-sessions-list--pending-remote-queries nil
  "Remote `zmx list' processes pending for the current list buffer.")

(defvar-local term-sessions-list--refresh-generation 0
  "Monotonic refresh id used to ignore stale asynchronous remote queries.")

(defvar term-sessions-list-mode-map
  (let ((map (make-sparse-keymap))
        (narrow-map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'term-sessions-list-open)
    (define-key map (kbd "o") #'term-sessions-list-open)
    (define-key map (kbd "g") #'revert-buffer)
    (define-key map (kbd "R") #'term-sessions-list-clear-failed-remotes)
    (define-key map (kbd "k") #'term-sessions-list-kill)
    (define-key map (kbd "h") #'term-sessions-list-history)
    (define-key map (kbd "s") #'term-sessions-list-send-command)
    (define-key map (kbd "y") #'term-sessions-list-store-org-link)
    (define-key map (kbd "m") #'term-sessions-list-mark)
    (define-key map (kbd "u") #'term-sessions-list-unmark)
    (define-key map (kbd "U") #'term-sessions-list-unmark-all)
    (define-key map (kbd "t") #'term-sessions-list-toggle-mark)
    (define-key map (kbd "T") #'term-sessions-list-toggle-all-marks)
    (define-key map (kbd "%") #'term-sessions-list-mark-regexp)
    (define-key map (kbd "-") #'term-sessions-list-widen)
    (define-key map (kbd "<backspace>") #'term-sessions-list-remove-narrow-criterion)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "/") narrow-map)
    (define-key narrow-map (kbd "n") #'term-sessions-list-narrow-name)
    (define-key narrow-map (kbd "h") #'term-sessions-list-narrow-host)
    (define-key narrow-map (kbd "l") #'term-sessions-list-narrow-local)
    (define-key narrow-map (kbd "r") #'term-sessions-list-narrow-remote)
    (define-key narrow-map (kbd "w") #'term-sessions-list-narrow-cwd-or-project)
    (define-key narrow-map (kbd "c") #'term-sessions-list-narrow-command)
    (define-key narrow-map (kbd "a") #'term-sessions-list-narrow-attached)
    (define-key narrow-map (kbd "d") #'term-sessions-list-narrow-detached)
    (define-key narrow-map (kbd "u") #'term-sessions-list-narrow-recently-updated)
    map)
  "Keymap for `term-sessions-list-mode'.")

(define-derived-mode term-sessions-list-mode tabulated-list-mode "Term-Sessions"
  "Major mode for listing persistent terminal sessions."
  (setq tabulated-list-padding 2)
  (term-sessions-list--update-format)
  (setq imenu-create-index-function #'term-sessions-list-imenu-index)
  (setq-local revert-buffer-function #'term-sessions-list-revert)
  (setq-local eldoc-echo-area-use-multiline-p t)
  (setq-local eldoc-idle-delay 0)
  (setq-local mode-line-position '((:eval (term-sessions-list--mode-line-indicator))))
  (hl-line-mode 1)
  (add-hook 'eldoc-documentation-functions #'term-sessions-list-eldoc nil t)
  (add-hook 'tabulated-list-revert-hook #'term-sessions-list-refresh nil t))

(defun term-sessions-list--column-widths (&optional total-width)
  "Return responsive column widths for TOTAL-WIDTH or the selected window."
  (let* ((padding 2)
         (window-width (or total-width (window-body-width) 100))
         (budget (max 80 (- window-width (* padding 7))))
         (fixed-width (+ 16 16 7))
         (variable-budget (max 44 (- budget fixed-width)))
         (base '((name 12 2 32)
                 (where 8 2 28)
                 (project 6 1 24)
                 (cwd 10 4 nil)
                 (command 8 5 nil))))
    (append (term-sessions--scaled-column-widths base variable-budget)
            '((created . 16)
              (updated . 16)
              (clients . 7)))))

(defun term-sessions-list--format (&optional total-width)
  "Return a `tabulated-list-format' scaled for TOTAL-WIDTH."
  (let ((widths (term-sessions-list--column-widths total-width)))
    (vector (list "Name" (term-sessions--column-width widths 'name) t)
            (list "Where" (term-sessions--column-width widths 'where) t)
            (list "Project" (term-sessions--column-width widths 'project) t)
            (list "Created" (term-sessions--column-width widths 'created) t)
            (list "Updated" (term-sessions--column-width widths 'updated) t)
            (list "Clients" (term-sessions--column-width widths 'clients) t)
            (list "Cwd" (term-sessions--column-width widths 'cwd) t)
            (list "Command" (term-sessions--column-width widths 'command) t))))

(defun term-sessions-list--update-format ()
  "Update `tabulated-list-format' for the current window width."
  (setq tabulated-list-format (term-sessions-list--format))
  (tabulated-list-init-header))

(defun term-sessions-list--numeric-string-p (string)
  "Return non-nil when STRING is a numeric timestamp string."
  (string-match-p "\\`[[:space:]]*[+-]?[0-9]+\\(?:\\.[0-9]+\\)?[[:space:]]*\\'"
                  string))

(defun term-sessions-list--time-string (time-or-seconds)
  "Return compact display string for TIME-OR-SECONDS."
  (cond
   ((null time-or-seconds) "")
   ((stringp time-or-seconds)
    (if (term-sessions-list--numeric-string-p time-or-seconds)
        (format-time-string "%Y-%m-%d %H:%M" (string-to-number time-or-seconds))
      time-or-seconds))
   (t (format-time-string "%Y-%m-%d %H:%M" time-or-seconds))))

(defun term-sessions-list--location-label (directory)
  "Return human label for DIRECTORY."
  (let ((location (term-sessions--location directory)))
    (if (term-sessions-location-remote-p location)
        (term-sessions--location-remote-label location)
      "local")))

(defun term-sessions-list--project-label (cwd)
  "Return a compact project label for CWD."
  (or (and cwd
           (not (file-remote-p cwd))
           (ignore-errors (term-sessions--project-name cwd)))
      (and cwd
           (not (string-empty-p cwd))
           (file-name-nondirectory (directory-file-name cwd)))
      ""))

(defun term-sessions-list--clients-number (clients)
  "Return numeric client count from CLIENTS string or number."
  (cond
   ((numberp clients) clients)
   ((stringp clients) (string-to-number clients))
   (t 0)))

(defun term-sessions-list--updated-seconds (entry)
  "Return update timestamp for ENTRY as float seconds, or nil."
  (let ((time (plist-get entry :updated-raw)))
    (cond
     ((null time) nil)
     ((numberp time) (float time))
     ((stringp time)
      (when (term-sessions-list--numeric-string-p time)
        (string-to-number time)))
     (t (float-time time)))))

(defun term-sessions-list--parse-duration (duration)
  "Parse DURATION such as 30m, 2h, or 3 days into seconds."
  (when (stringp duration)
    (cond
     ((string-match "\\`[[:space:]]*\\([0-9]+\\)[[:space:]]*s\\(?:ec\\(?:ond\\)?s?\\)?[[:space:]]*\\'" duration)
      (string-to-number (match-string 1 duration)))
     ((string-match "\\`[[:space:]]*\\([0-9]+\\)[[:space:]]*m\\(?:in\\(?:ute\\)?s?\\)?[[:space:]]*\\'" duration)
      (* 60 (string-to-number (match-string 1 duration))))
     ((string-match "\\`[[:space:]]*\\([0-9]+\\)[[:space:]]*h\\(?:ours?\\)?[[:space:]]*\\'" duration)
      (* 3600 (string-to-number (match-string 1 duration))))
     ((string-match "\\`[[:space:]]*\\([0-9]+\\)[[:space:]]*d\\(?:ays?\\)?[[:space:]]*\\'" duration)
      (* 86400 (string-to-number (match-string 1 duration))))
     ((string-match "\\`[[:space:]]*\\([0-9]+\\)[[:space:]]*weeks?[[:space:]]*\\'" duration)
      (* 604800 (string-to-number (match-string 1 duration)))))))

(defun term-sessions-list--entry-label (entry)
  "Return a readable label for ENTRY."
  (format "%s @ %s"
          (plist-get entry :name)
          (plist-get entry :where)))

(defun term-sessions-list--mode-line-indicator ()
  "Return mode-line text for active narrowing criteria."
  (if term-sessions-list--narrow-criteria
      (string-join (mapcar #'car term-sessions-list--narrow-criteria) " > ")
    ""))

(defun term-sessions-list-imenu-index ()
  "Create an Imenu index for `term-sessions-list-mode'."
  (let (index)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let ((entry (tabulated-list-get-id)))
          (push (cons (term-sessions-list--entry-label entry) (point)) index))
        (forward-line 1)))
    (nreverse index)))

(defun term-sessions-list-eldoc (_callback)
  "Show session details in Eldoc for the session at point."
  (when-let ((entry (tabulated-list-get-id)))
    (string-join
     (delq nil
           (list (format "%s  clients:%s"
                         (term-sessions-list--entry-label entry)
                         (or (plist-get entry :clients) ""))
                 (when-let ((cwd (plist-get entry :cwd)))
                   (format "cwd: %s" cwd))
                 (when-let ((command (plist-get entry :command)))
                   (unless (string-empty-p command)
                     (format "cmd: %s" command)))))
     "\n")))

(defun term-sessions-list--directory-for-tramp-vec (vec)
  "Return a TRAMP root directory for connection VEC, preserving hops."
  (let* ((method (tramp-file-name-method vec))
         (user (tramp-file-name-user vec))
         (host (tramp-file-name-host vec))
         (port (tramp-file-name-port vec))
         (hop (tramp-file-name-hop vec)))
    (when (and method host)
      (format "/%s%s:%s%s%s:/"
              (or hop "")
              method
              (if user (concat user "@") "")
              host
              (if port (concat "#" port) "")))))

(defun term-sessions-list--remote-key-for-tramp-vec (vec)
  "Return backend identity key for TRAMP connection VEC.
zmx sessions are owned by the final remote user/host, not by the TRAMP method
used to reach it.  `/sshx:host:' and `/rpc:host:' therefore refer to the same
session server and should not be listed twice."
  (when-let ((host (tramp-file-name-host vec)))
    (list (or (tramp-file-name-user vec) (user-login-name))
          (substring-no-properties host)
          (tramp-file-name-port vec))))

(defun term-sessions-list--tramp-vec-score (vec)
  "Return preference score for TRAMP connection VEC; lower is better."
  (+ (if (tramp-file-name-hop vec) 10 0)
     (pcase (tramp-file-name-method vec)
       ("rpc" 0)
       ("sshx" 1)
       ("ssh" 2)
       ("scp" 3)
       (_ 9))))

(defun term-sessions-list--local-directory ()
  "Return a stable local directory for local zmx queries."
  (expand-file-name "~/"))

(defun term-sessions-list--open-remote-directories ()
  "Return TRAMP root directories for currently open connections."
  (when (and term-sessions-list-include-open-remotes
             (fboundp 'tramp-list-connections))
    (let ((best-by-remote (make-hash-table :test #'equal))
          directories)
      (dolist (vec (tramp-list-connections))
        (when-let* ((directory (ignore-errors
                                 (substring-no-properties
                                  (term-sessions-list--directory-for-tramp-vec vec))))
                    (key (ignore-errors
                           (term-sessions-list--remote-key-for-tramp-vec vec))))
          (let* ((score (term-sessions-list--tramp-vec-score vec))
                 (existing (gethash key best-by-remote)))
            (when (or (null existing) (< score (car existing)))
              (puthash key (cons score directory) best-by-remote)))))
      (maphash (lambda (_key value)
                 (push (cdr value) directories))
               best-by-remote)
      (nreverse directories))))

(defun term-sessions-list--failed-remote-p (directory)
  "Return non-nil when remote DIRECTORY should be skipped this refresh."
  (when-let* ((remote (file-remote-p directory))
              (entry (gethash remote term-sessions-list--failed-remotes)))
    (let ((retry-delay term-sessions-list-failed-remote-retry-delay))
      (cond
       ((null retry-delay)
        (remhash remote term-sessions-list--failed-remotes)
        nil)
       ((zerop retry-delay)
        t)
       ((< (float-time (time-subtract (current-time) (car entry))) retry-delay)
        t)
       (t
        (remhash remote term-sessions-list--failed-remotes)
        nil)))))

(defun term-sessions-list--record-remote-failure (directory error)
  "Remember that remote DIRECTORY failed with ERROR."
  (when-let ((remote (file-remote-p directory)))
    (puthash remote (cons (current-time) error) term-sessions-list--failed-remotes)))

(defun term-sessions-list--remote-connection-state (directory)
  "Return DIRECTORY's cheap TRAMP connection state.
Possible values are `live', `dead', `absent', `unknown', or nil for local
directories.  This consults existing TRAMP connection state only; it must not
start or reconnect providers during list refresh."
  (when (file-remote-p directory)
    (if-let ((vec (ignore-errors (tramp-dissect-file-name directory))))
        (if (fboundp 'tramp-get-connection-process)
            (let ((process (ignore-errors (tramp-get-connection-process vec))))
              (cond
               ((processp process)
                (if (process-live-p process) 'live 'dead))
               ((null process) 'absent)
               (t 'unknown)))
          'unknown)
      'unknown)))

(defun term-sessions-list--remote-known-offline-p (directory)
  "Return non-nil when DIRECTORY's TRAMP connection process is known dead."
  (eq (term-sessions-list--remote-connection-state directory) 'dead))

(defun term-sessions-list--skip-known-offline-remote-p (directory)
  "Record and return non-nil when DIRECTORY should be skipped as offline."
  (when (term-sessions-list--remote-known-offline-p directory)
    (term-sessions-list--record-remote-failure
     directory "TRAMP connection process is not live")
    (message "term-sessions: skipping offline TRAMP remote %s (R retries)"
             directory)
    t))

(defun term-sessions-list--skip-unavailable-async-remote-p (directory)
  "Record and return non-nil when DIRECTORY should not be queried async.
Asynchronous list refreshes are intentionally conservative: a remote from
`tramp-list-connections' without a live connection process would make TRAMP
reconnect in the UI path, which is exactly what we are trying to avoid."
  (when (memq (term-sessions-list--remote-connection-state directory)
              '(dead absent))
    (term-sessions-list--record-remote-failure
     directory "TRAMP connection process is not live")
    (message "term-sessions: skipping offline TRAMP remote %s (R retries)"
             directory)
    t))

(defun term-sessions-list--clear-remote-failure (directory)
  "Forget any cached failure for remote DIRECTORY."
  (when-let ((remote (file-remote-p directory)))
    (remhash remote term-sessions-list--failed-remotes)))

(defun term-sessions-list--session-buffer-directories ()
  "Return remote directories for currently open term-session buffers."
  (delq nil
        (mapcar (lambda (buffer)
                  (with-current-buffer buffer
                    (when (and term-sessions-current-name
                               (file-remote-p default-directory))
                      default-directory)))
                (buffer-list))))

(defun term-sessions-list--directory-key (directory)
  "Return backend identity key for DIRECTORY."
  (term-sessions--directory-key directory))

(defun term-sessions-list--delete-duplicate-directories (directories)
  "Return DIRECTORIES with duplicate backend identities removed."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (directory directories)
      (let ((key (term-sessions-list--directory-key directory)))
        (unless (gethash key seen)
          (puthash key t seen)
          (push directory result))))
    (nreverse result)))

;;;###autoload
(defun term-sessions-list-clear-failed-remotes ()
  "Forget failed remote queries and refresh the session list.
Use this after fixing a TRAMP connection, or when you want to force retrying
remotes before `term-sessions-list-failed-remote-retry-delay' has elapsed."
  (interactive)
  (clrhash term-sessions-list--failed-remotes)
  (message "term-sessions: cleared failed remote cache")
  (when (derived-mode-p 'term-sessions-list-mode)
    (revert-buffer)))

(defun term-sessions-list--query-directory (directory)
  "Return session rows for zmx at DIRECTORY."
  (let ((default-directory directory))
    (term-sessions-list--rows-for-sessions
     (cond
      ((term-sessions-list--failed-remote-p directory)
       nil)
      ((term-sessions-list--skip-known-offline-remote-p directory)
       nil)
      (t
       (condition-case err
           (term-sessions--zmx-list-sessions)
         (error
          (term-sessions-list--record-remote-failure directory err)
          (message "term-sessions: cannot list %s: %s" directory err)
          nil))))
     directory)))

(defun term-sessions-list--parse-zmx-list-output (output)
  "Parse zmx list OUTPUT into session plists without extra remote probes."
  (delq nil
        (mapcar
         (lambda (line)
           (unless (or (string-empty-p (string-trim line))
                       (string-prefix-p "no sessions found" line))
             (let ((entry (term-sessions--parse-key-value-fields line)))
               (when (plist-get entry :name)
                 entry))))
         (split-string output "\n" t))))

(defun term-sessions-list--rows-for-sessions (sessions directory)
  "Return tabulated rows for SESSIONS from DIRECTORY."
  (let (rows)
    (dolist (session sessions)
      (let* ((entry (term-sessions--zmx-session-entry session directory))
             (name (plist-get entry :name))
             (where (term-sessions-list--location-label directory))
             (created (term-sessions-list--time-string (plist-get session :created)))
             (updated-raw (plist-get entry :updated-time))
             (updated (term-sessions-list--time-string updated-raw))
             (clients (plist-get entry :clients))
             (cwd (plist-get entry :cwd))
             (cmd (plist-get entry :command))
             (project (term-sessions-list--project-label cwd))
             (id (append entry
                         (list :where where
                               :project project
                               :created created
                               :updated updated
                               :updated-raw updated-raw))))
        (push (list id (vector name where project created updated clients cwd cmd)) rows)))
    (nreverse rows)))

(defun term-sessions-list--rows-without-directory (rows directory)
  "Return ROWS without entries owned by DIRECTORY's backend."
  (let ((key (term-sessions--directory-key directory)))
    (seq-remove (lambda (row)
                  (equal (term-sessions--directory-key
                          (term-sessions--entry-directory (car row)))
                         key))
                rows)))

(defun term-sessions-list--remote-query-done (process)
  "Clean up PROCESS bookkeeping for an asynchronous remote query."
  (when-let ((timer (process-get process 'term-sessions-list-timer)))
    (cancel-timer timer))
  (when-let ((list-buffer (process-get process 'term-sessions-list-buffer)))
    (when (buffer-live-p list-buffer)
      (with-current-buffer list-buffer
        (setq term-sessions-list--pending-remote-queries
              (delq process term-sessions-list--pending-remote-queries))))))

(defun term-sessions-list--remote-query-install (buffer generation directory rows)
  "Install asynchronous ROWS for DIRECTORY into BUFFER if still current."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (derived-mode-p 'term-sessions-list-mode)
                 (= generation term-sessions-list--refresh-generation))
        (setq term-sessions-list--all-entries
              (append (term-sessions-list--rows-without-directory
                       term-sessions-list--all-entries directory)
                      rows))
        (term-sessions-list--reprint)))))

(defun term-sessions-list--remote-query-timeout (process)
  "Abort remote query PROCESS after `term-sessions-list-remote-query-timeout'."
  (when (process-live-p process)
    (process-put process 'term-sessions-list-timeout t)
    (delete-process process)))

(defun term-sessions-list--remote-query-sentinel (process event)
  "Handle completion of an asynchronous remote list PROCESS with EVENT."
  (when (memq (process-status process) '(exit signal))
    (let* ((cancelled-p (process-get process 'term-sessions-list-cancelled))
           (directory (process-get process 'term-sessions-list-directory))
           (list-buffer (process-get process 'term-sessions-list-buffer))
           (generation (process-get process 'term-sessions-list-generation))
           (timeout-p (process-get process 'term-sessions-list-timeout))
           (output-buffer (process-buffer process))
           (output (if (buffer-live-p output-buffer)
                       (with-current-buffer output-buffer (buffer-string))
                     "")))
      (term-sessions-list--remote-query-done process)
      (unwind-protect
          (unless cancelled-p
            (if (and (eq (process-status process) 'exit)
                     (eq (process-exit-status process) 0)
                     (not timeout-p))
                (progn
                  (term-sessions-list--clear-remote-failure directory)
                  (term-sessions-list--remote-query-install
                   list-buffer generation directory
                   (term-sessions-list--rows-for-sessions
                    (term-sessions-list--parse-zmx-list-output output)
                    directory)))
              (let ((reason (if timeout-p
                                (format "timed out after %ss"
                                        term-sessions-list-remote-query-timeout)
                              (string-trim (concat output " " event)))))
                (term-sessions-list--record-remote-failure directory reason)
                (message "term-sessions: cannot list %s: %s (R retries)"
                         directory reason))))
        (when (buffer-live-p output-buffer)
          (kill-buffer output-buffer))))))

(defun term-sessions-list--start-remote-query (directory buffer generation)
  "Start an asynchronous zmx list query for remote DIRECTORY."
  (cond
   ((term-sessions-list--failed-remote-p directory)
    nil)
   ((term-sessions-list--skip-unavailable-async-remote-p directory)
    nil)
   (t
    (let ((default-directory directory)
          (output-buffer (generate-new-buffer
                          (format " *term-sessions-list:%s*" directory))))
      (condition-case err
          (let (process)
            (term-sessions-zmx--with-environment
              (term-sessions--ensure-zmx)
              (setq process
                    (start-file-process
                     (format "term-sessions-list:%s" directory)
                     output-buffer term-sessions-zmx-program "list")))
            (process-put process 'term-sessions-list-buffer buffer)
            (process-put process 'term-sessions-list-directory directory)
            (process-put process 'term-sessions-list-generation generation)
            (when term-sessions-list-remote-query-timeout
              (process-put process 'term-sessions-list-timer
                           (run-at-time term-sessions-list-remote-query-timeout nil
                                        #'term-sessions-list--remote-query-timeout
                                        process)))
            (set-process-sentinel process
                                  #'term-sessions-list--remote-query-sentinel)
            (push process term-sessions-list--pending-remote-queries)
            (when (memq (process-status process) '(exit signal))
              (term-sessions-list--remote-query-sentinel process "finished\n"))
            process)
        (error
         (term-sessions-list--record-remote-failure directory err)
         (message "term-sessions: cannot start remote query for %s: %s"
                  directory err)
         (when (buffer-live-p output-buffer)
           (kill-buffer output-buffer))
         nil))))))

(defun term-sessions-list--cancel-pending-remote-queries ()
  "Cancel asynchronous remote queries owned by the current list buffer."
  (dolist (process term-sessions-list--pending-remote-queries)
    (process-put process 'term-sessions-list-cancelled t)
    (when (process-live-p process)
      (delete-process process))
    (when-let ((timer (process-get process 'term-sessions-list-timer)))
      (cancel-timer timer))
    (when-let ((buffer (process-buffer process)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))
  (setq term-sessions-list--pending-remote-queries nil))

(defun term-sessions-list--filtered-entries (entries)
  "Return ENTRIES after applying `term-sessions-list--narrow-criteria'."
  (let ((result entries))
    (dolist (criterion term-sessions-list--narrow-criteria result)
      (setq result (funcall (cdr criterion) result)))))

(defun term-sessions-list--entry-key (entry)
  "Return stable identity key for ENTRY."
  (list (term-sessions--entry-name entry)
        (term-sessions--directory-key (term-sessions--entry-directory entry))))

(defun term-sessions-list--same-entry-p (a b)
  "Return non-nil when entries A and B identify the same session."
  (equal (term-sessions-list--entry-key a)
         (term-sessions-list--entry-key b)))

(defun term-sessions-list--mark-entry (entry)
  "Mark ENTRY unless an equivalent entry is already marked."
  (cl-pushnew entry term-sessions-list--marked-entries
              :test #'term-sessions-list--same-entry-p))

(defun term-sessions-list--unmark-entry (entry)
  "Unmark any entry equivalent to ENTRY."
  (setq term-sessions-list--marked-entries
        (seq-remove (lambda (marked)
                      (term-sessions-list--same-entry-p entry marked))
                    term-sessions-list--marked-entries)))

(defun term-sessions-list--unmark-entries (entries)
  "Unmark any entry equivalent to an element of ENTRIES."
  (setq term-sessions-list--marked-entries
        (seq-remove (lambda (marked)
                      (seq-some (lambda (entry)
                                  (term-sessions-list--same-entry-p entry marked))
                                entries))
                    term-sessions-list--marked-entries)))

(defun term-sessions-list--entry-marked-p (entry)
  "Return non-nil when ENTRY is marked."
  (seq-find (lambda (marked)
              (term-sessions-list--same-entry-p entry marked))
            term-sessions-list--marked-entries))

(defun term-sessions-list--restore-marks ()
  "Mark entries that are selected in the visible tabulated list."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when-let ((entry (tabulated-list-get-id)))
        (when (term-sessions-list--entry-marked-p entry)
          (tabulated-list-put-tag "*")))
      (forward-line 1))))

(defun term-sessions-list-revert (&rest _ignore)
  "Refresh and reprint the current term session list."
  (interactive)
  (term-sessions-list--update-format)
  (term-sessions-list-refresh)
  (tabulated-list-print t)
  (term-sessions-list--restore-marks))

(defun term-sessions-list--session-rows ()
  "Return session rows across local and already-open TRAMP remotes."
  (let* ((session-directories (term-sessions-list--session-buffer-directories))
         (directories (term-sessions-list--delete-duplicate-directories
                       (append (list (term-sessions-list--local-directory))
                               session-directories
                               (term-sessions-list--open-remote-directories)))))
    ;; If the user has successfully opened a term-session on a remote, retry
    ;; that remote even if an earlier list refresh cached a TRAMP failure.
    (mapc #'term-sessions-list--clear-remote-failure session-directories)
    (apply #'append
           (mapcar #'term-sessions-list--query-directory directories))))

(defun term-sessions-list-refresh ()
  "Refresh `term-sessions-list-mode' rows.
Local sessions are queried synchronously.  Remote TRAMP sessions are queried in
the background so stale or offline providers do not block the list UI."
  (cl-incf term-sessions-list--refresh-generation)
  (term-sessions-list--cancel-pending-remote-queries)
  (let* ((generation term-sessions-list--refresh-generation)
         (session-directories (term-sessions-list--session-buffer-directories))
         (directories (term-sessions-list--delete-duplicate-directories
                       (append (list (term-sessions-list--local-directory))
                               session-directories
                               (term-sessions-list--open-remote-directories))))
         (local-directories (seq-remove #'file-remote-p directories))
         (remote-directories (seq-filter #'file-remote-p directories)))
    ;; If the user has successfully opened a term-session on a remote, retry
    ;; that remote even if an earlier list refresh cached a TRAMP failure.
    (mapc #'term-sessions-list--clear-remote-failure session-directories)
    (setq term-sessions-list--all-entries
          (apply #'append
                 (mapcar #'term-sessions-list--query-directory local-directories)))
    (let ((started 0))
      (dolist (directory remote-directories)
        (when (term-sessions-list--start-remote-query directory (current-buffer) generation)
          (cl-incf started)))
      (when (> started 0)
        (message "term-sessions: querying %d TRAMP remote%s in background"
                 started
                 (if (= started 1) "" "s")))))
  (setq tabulated-list-entries
        (term-sessions-list--filtered-entries term-sessions-list--all-entries)))

;;;###autoload
(defun term-sessions-list ()
  "List active zmx sessions across local and currently open TRAMP remotes."
  (interactive)
  (let ((buffer (get-buffer-create "*term-sessions*")))
    (with-current-buffer buffer
      (setq default-directory (term-sessions-list--local-directory))
      (term-sessions-list-mode)
      (term-sessions-list--update-format)
      (term-sessions-list-refresh)
      (tabulated-list-print t)
      (term-sessions-list--restore-marks))
    (pop-to-buffer buffer)))

(defun term-sessions-list--entry-at-point ()
  "Return term session entry at point in `term-sessions-list-mode'."
  (or (tabulated-list-get-id)
      (user-error "No session on this line")))

(defun term-sessions-list--selected-entries ()
  "Return marked entries, or the entry at point when nothing is marked."
  (or term-sessions-list--marked-entries
      (list (term-sessions-list--entry-at-point))))

(defun term-sessions-list--map-selected (function)
  "Call FUNCTION for each selected entry with `default-directory' bound."
  (dolist (entry (term-sessions-list--selected-entries))
    (let ((default-directory (term-sessions--entry-directory entry)))
      (funcall function entry))))

(defun term-sessions-list--reprint ()
  "Reprint current entries and mark selected rows."
  (term-sessions-list--update-format)
  (setq tabulated-list-entries
        (term-sessions-list--filtered-entries term-sessions-list--all-entries))
  (tabulated-list-print t)
  (term-sessions-list--restore-marks))

(defun term-sessions-list-open ()
  "Open session at point, reusing an existing session buffer when present."
  (interactive)
  (term-sessions-open (term-sessions-list--entry-at-point)))

(defun term-sessions-list-kill ()
  "Kill selected sessions."
  (interactive)
  (let ((entries (term-sessions-list--selected-entries)))
    (when (yes-or-no-p (if (> (length entries) 1)
                           (format "Kill %d term sessions? " (length entries))
                         (format "Kill zmx session `%s'? "
                                 (plist-get (car entries) :name))))
      (dolist (entry entries)
        (let ((default-directory (term-sessions--entry-directory entry)))
          (term-sessions--zmx "kill" (term-sessions--entry-name entry))))
      (setq term-sessions-list--marked-entries nil)
      (revert-buffer))))

(defun term-sessions-list-history ()
  "Show history for selected sessions."
  (interactive)
  (term-sessions-list--map-selected
   (lambda (entry)
     (term-sessions-history (term-sessions--entry-name entry)))))

(defun term-sessions-list-send-command (command)
  "Send COMMAND to selected sessions."
  (interactive (list (read-string "Command: " nil 'term-sessions-command-history)))
  (term-sessions-list--map-selected
   (lambda (entry)
     (term-sessions-send-command (term-sessions--entry-name entry) command))))

(defun term-sessions-list-store-org-link ()
  "Store or copy Org links for selected sessions."
  (interactive)
  (let ((links
         (mapcar #'term-sessions--org-link-for-entry
                 (term-sessions-list--selected-entries))))
    (kill-new (string-join links "\n"))
    (when (= (length links) 1)
      (let* ((entry (car (term-sessions-list--selected-entries)))
             (default-directory (term-sessions--entry-cwd-directory entry)))
        (term-sessions-store-org-link (term-sessions--entry-name entry))))
    (message "Copied %d term-session Org link%s"
             (length links) (if (= (length links) 1) "" "s"))))

(defun term-sessions-list-mark ()
  "Mark the session at point and move to the next line."
  (interactive)
  (let ((entry (term-sessions-list--entry-at-point)))
    (term-sessions-list--mark-entry entry)
    (tabulated-list-put-tag "*")
    (forward-line 1)))

(defun term-sessions-list-unmark ()
  "Unmark the session at point and move to the next line."
  (interactive)
  (let ((entry (term-sessions-list--entry-at-point)))
    (term-sessions-list--unmark-entry entry)
    (tabulated-list-put-tag " ")
    (forward-line 1)))

(defun term-sessions-list-unmark-all ()
  "Unmark all sessions."
  (interactive)
  (setq term-sessions-list--marked-entries nil)
  (term-sessions-list--reprint))

(defun term-sessions-list-toggle-mark ()
  "Toggle the mark on the session at point."
  (interactive)
  (if (term-sessions-list--entry-marked-p (term-sessions-list--entry-at-point))
      (term-sessions-list-unmark)
    (term-sessions-list-mark)))

(defun term-sessions-list-toggle-all-marks ()
  "Mark all visible sessions, or unmark them when all are already marked."
  (interactive)
  (let ((visible (mapcar #'car tabulated-list-entries)))
    (if (seq-every-p #'term-sessions-list--entry-marked-p visible)
        (term-sessions-list--unmark-entries visible)
      (dolist (entry visible)
        (term-sessions-list--mark-entry entry)))
    (term-sessions-list--reprint)))

(defun term-sessions-list-mark-regexp (regexp)
  "Mark visible sessions whose name or command matches REGEXP.
With prefix argument, unmark matching sessions instead."
  (interactive (list (read-regexp (if current-prefix-arg
                                      "Unmark matching name/command: "
                                    "Mark matching name/command: "))))
  (dolist (entry (mapcar #'car tabulated-list-entries))
    (when (or (string-match-p regexp (or (plist-get entry :name) ""))
              (string-match-p regexp (or (plist-get entry :command) "")))
      (if current-prefix-arg
          (term-sessions-list--unmark-entry entry)
        (term-sessions-list--mark-entry entry))))
  (term-sessions-list--reprint))

(defun term-sessions-list-narrow-sessions (label predicate)
  "Add narrowing criterion LABEL using PREDICATE over entries."
  (push (cons label
              (lambda (rows)
                (seq-filter (lambda (row) (funcall predicate (car row))) rows)))
        term-sessions-list--narrow-criteria)
  (term-sessions-list--reprint))

(defun term-sessions-list-remove-narrow-criterion ()
  "Remove the most recently added narrowing criterion."
  (interactive)
  (if term-sessions-list--narrow-criteria
      (progn
        (setq term-sessions-list--narrow-criteria
              (cdr term-sessions-list--narrow-criteria))
        (term-sessions-list--reprint))
    (message "No narrowing criteria")))

(defun term-sessions-list-widen ()
  "Remove all narrowing criteria."
  (interactive)
  (setq term-sessions-list--narrow-criteria nil)
  (term-sessions-list--reprint))

(defun term-sessions-list-narrow-name (regexp)
  "Narrow to sessions whose name matches REGEXP."
  (interactive (list (read-regexp "Name matches: ")))
  (term-sessions-list-narrow-sessions
   (format "Name: %s" regexp)
   (lambda (entry) (string-match-p regexp (or (plist-get entry :name) "")))))

(defun term-sessions-list-narrow-command (regexp)
  "Narrow to sessions whose foreground/start command matches REGEXP."
  (interactive (list (read-regexp "Command matches: ")))
  (term-sessions-list-narrow-sessions
   (format "Command: %s" regexp)
   (lambda (entry) (string-match-p regexp (or (plist-get entry :command) "")))))

(defun term-sessions-list-narrow-cwd-or-project (regexp)
  "Narrow to sessions whose cwd or project matches REGEXP."
  (interactive (list (read-regexp "Cwd/project matches: ")))
  (term-sessions-list-narrow-sessions
   (format "Cwd/project: %s" regexp)
   (lambda (entry)
     (or (string-match-p regexp (or (plist-get entry :cwd) ""))
         (string-match-p regexp (or (plist-get entry :project) ""))))))

(defun term-sessions-list-narrow-host (host)
  "Narrow to sessions for HOST."
  (interactive
   (list (completing-read
          "Host: "
          (delete-dups (mapcar (lambda (row) (plist-get (car row) :where))
                               tabulated-list-entries))
          nil t)))
  (term-sessions-list-narrow-sessions
   (format "Host: %s" host)
   (lambda (entry) (string= host (plist-get entry :where)))))

(defun term-sessions-list-narrow-local ()
  "Narrow to local sessions."
  (interactive)
  (term-sessions-list-narrow-sessions
   "Local"
   (lambda (entry) (not (file-remote-p (term-sessions--entry-directory entry))))))

(defun term-sessions-list-narrow-remote ()
  "Narrow to remote sessions."
  (interactive)
  (term-sessions-list-narrow-sessions
   "Remote"
   (lambda (entry) (file-remote-p (term-sessions--entry-directory entry)))))

(defun term-sessions-list-narrow-attached ()
  "Narrow to sessions with one or more attached clients."
  (interactive)
  (term-sessions-list-narrow-sessions
   "Attached"
   (lambda (entry) (> (term-sessions-list--clients-number (plist-get entry :clients)) 0))))

(defun term-sessions-list-narrow-detached ()
  "Narrow to sessions with no attached clients."
  (interactive)
  (term-sessions-list-narrow-sessions
   "No clients"
   (lambda (entry) (= (term-sessions-list--clients-number (plist-get entry :clients)) 0))))

(defun term-sessions-list-narrow-recently-updated (duration)
  "Narrow to sessions updated within DURATION."
  (interactive (list (read-string "Updated within (for example 30m, 2h, 3 days): " "1h")))
  (let ((seconds (term-sessions-list--parse-duration duration)))
    (unless seconds
      (user-error "Cannot parse duration: %s" duration))
    (term-sessions-list-narrow-sessions
     (format "Updated < %s" duration)
     (lambda (entry)
       (when-let ((updated (term-sessions-list--updated-seconds entry)))
         (< (- (float-time) updated) seconds))))))

(provide 'term-sessions-list)
;;; term-sessions-list.el ends here
