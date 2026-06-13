;;; term-sessions-list.el --- Session list UI for term-sessions -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tabulated list UI for persistent terminal sessions.

;;; Code:

(require 'tabulated-list)
(require 'tramp)
(require 'term-sessions-core)
(require 'term-sessions-zmx)
(require 'term-sessions-frontends)

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

(defvar term-sessions-list--failed-remotes (make-hash-table :test #'equal)
  "Remote directories that recently failed during `term-sessions-list' refresh.")

(defvar term-sessions-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'term-sessions-list-open)
    (define-key map (kbd "g") #'revert-buffer)
    (define-key map (kbd "R") #'term-sessions-list-clear-failed-remotes)
    (define-key map (kbd "k") #'term-sessions-list-kill)
    (define-key map (kbd "h") #'term-sessions-list-history)
    (define-key map (kbd "t") #'term-sessions-list-tail)
    map)
  "Keymap for `term-sessions-list-mode'.")

(define-derived-mode term-sessions-list-mode tabulated-list-mode "Term-Sessions"
  "Major mode for listing persistent terminal sessions."
  (setq tabulated-list-format [("Name" 28 t)
                               ("Where" 24 t)
                               ("Created" 17 t)
                               ("Updated" 17 t)
                               ("Clients" 7 t)
                               ("Cwd" 34 t)
                               ("Command" 40 t)])
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'term-sessions-list-refresh nil t)
  (tabulated-list-init-header))

(defun term-sessions-list--time-string (time-or-seconds)
  "Return compact display string for TIME-OR-SECONDS."
  (cond
   ((null time-or-seconds) "")
   ((stringp time-or-seconds)
    (if-let ((seconds (ignore-errors (string-to-number time-or-seconds))))
        (format-time-string "%Y-%m-%d %H:%M" seconds)
      time-or-seconds))
   (t (format-time-string "%Y-%m-%d %H:%M" time-or-seconds))))

(defun term-sessions-list--location-label (directory)
  "Return human label for DIRECTORY."
  (let ((location (term-sessions--location directory)))
    (if (term-sessions-location-remote-p location)
        (term-sessions--location-remote-label location)
      "local")))

(defun term-sessions-list--directory-for-tramp-vec (vec)
  "Return a TRAMP root directory for connection VEC, preserving hops."
  (let* ((method (tramp-file-name-method vec))
         (user (tramp-file-name-user vec))
         (host (tramp-file-name-host vec))
         (port (tramp-file-name-port vec))
         (hop (tramp-file-name-hop vec)))
    (format "/%s%s:%s%s%s:/"
            (or hop "")
            method
            (if user (concat user "@") "")
            host
            (if port (concat "#" port) ""))))

(defun term-sessions-list--remote-key-for-tramp-vec (vec)
  "Return backend identity key for TRAMP connection VEC.
zmx sessions are owned by the final remote user/host, not by the TRAMP method
used to reach it.  `/sshx:host:' and `/rpc:host:' therefore refer to the same
session server and should not be listed twice."
  (list (or (tramp-file-name-user vec) (user-login-name))
        (substring-no-properties (tramp-file-name-host vec))
        (tramp-file-name-port vec)))

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
        (when-let ((directory (ignore-errors
                                (substring-no-properties
                                 (term-sessions-list--directory-for-tramp-vec vec)))))
          (let* ((key (term-sessions-list--remote-key-for-tramp-vec vec))
                 (score (term-sessions-list--tramp-vec-score vec))
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
  "Return backend identity key for DIRECTORY.
Remote zmx sessions are keyed by TRAMP prefix rather than localname, because
`/rpc:host:/' and `/rpc:host:/some/cwd' query the same zmx server."
  (or (file-remote-p directory)
      (expand-file-name directory)))

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
  (let ((default-directory directory)
        rows)
    (dolist (session (if (term-sessions-list--failed-remote-p directory)
                         nil
                       (condition-case err
                           (term-sessions--zmx-list-sessions)
                         (error
                          (term-sessions-list--record-remote-failure directory err)
                          (message "term-sessions: cannot list %s: %s" directory err)
                          nil))))
      (let* ((name (plist-get session :name))
             (id (list :name name :directory directory))
             (where (term-sessions-list--location-label directory))
             (created (term-sessions-list--time-string (plist-get session :created)))
             (updated (term-sessions-list--time-string (plist-get session :updated-time)))
             (clients (or (plist-get session :clients) ""))
             (cwd (or (plist-get session :cwd)
                      (plist-get session :start_dir)
                      ""))
             (cmd (or (plist-get session :current-cmd)
                      (plist-get session :cmd)
                      "")))
        (push (list id (vector name where created updated clients cwd cmd)) rows)))
    (nreverse rows)))

(defun term-sessions-list-refresh ()
  "Refresh `term-sessions-list-mode' rows."
  (let* ((session-directories (term-sessions-list--session-buffer-directories))
         (directories (append (list (term-sessions-list--local-directory))
                              session-directories
                              (term-sessions-list--open-remote-directories))))
    ;; If the user has successfully opened a term-session on a remote, retry
    ;; that remote even if an earlier list refresh cached a TRAMP failure.
    (mapc #'term-sessions-list--clear-remote-failure session-directories)
    (setq tabulated-list-entries
          (apply #'append
                 (mapcar #'term-sessions-list--query-directory
                         (term-sessions-list--delete-duplicate-directories
                          directories))))))

;;;###autoload
(defun term-sessions-list ()
  "List active zmx sessions across local and currently open TRAMP remotes."
  (interactive)
  (let ((buffer (get-buffer-create "*term-sessions*")))
    (with-current-buffer buffer
      (setq default-directory (term-sessions-list--local-directory))
      (term-sessions-list-mode)
      (term-sessions-list-refresh)
      (tabulated-list-print t))
    (pop-to-buffer buffer)))

(defun term-sessions-list--entry-at-point ()
  "Return term session entry at point in `term-sessions-list-mode'."
  (or (tabulated-list-get-id)
      (user-error "No session on this line")))

(defun term-sessions-list--name-at-point ()
  "Return session name at point in `term-sessions-list-mode'."
  (let ((entry (term-sessions-list--entry-at-point)))
    (if (stringp entry) entry (plist-get entry :name))))

(defun term-sessions-list--directory-at-point ()
  "Return session directory at point in `term-sessions-list-mode'."
  (let ((entry (term-sessions-list--entry-at-point)))
    (if (stringp entry) default-directory (plist-get entry :directory))))

(defun term-sessions-list-open ()
  "Open session at point."
  (interactive)
  (let ((default-directory (term-sessions-list--directory-at-point)))
    (term-sessions-open (term-sessions-list--name-at-point))))

(defun term-sessions-list-kill ()
  "Kill session at point."
  (interactive)
  (let ((default-directory (term-sessions-list--directory-at-point)))
    (term-sessions-kill (term-sessions-list--name-at-point)))
  (revert-buffer))

(defun term-sessions-list-history ()
  "Show history for session at point."
  (interactive)
  (let ((default-directory (term-sessions-list--directory-at-point)))
    (term-sessions-history (term-sessions-list--name-at-point))))

(defun term-sessions-list-tail ()
  "Tail session at point."
  (interactive)
  (let ((default-directory (term-sessions-list--directory-at-point)))
    (term-sessions-tail (term-sessions-list--name-at-point))))

(provide 'term-sessions-list)
;;; term-sessions-list.el ends here
