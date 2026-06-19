;;; term-sessions-actions.el --- Action maps for term-sessions -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Completion and Embark actions for the `term-session' category.

;;; Code:

(require 'term-sessions-core)
(require 'term-sessions-zmx)
(require 'term-sessions-tramp)
(require 'term-sessions-frontends)
(require 'term-sessions-org)

(declare-function compile "compile" (command &optional comint))
(declare-function consult-line "consult")
(declare-function dired "dired" (dirname &optional switches))
(declare-function project-compile "project" ())
(declare-function project-current "project" (&optional prompt))
(declare-function project-root "project" (project))
(defvar compile-command)

(defvar term-sessions-action-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'term-sessions-action-open)
    (define-key map (kbd "k") #'term-sessions-action-kill)
    (define-key map (kbd "h") #'term-sessions-action-history)
    (define-key map (kbd "H") #'term-sessions-action-history-full)
    (define-key map (kbd "v") #'term-sessions-action-history-vt)
    (define-key map (kbd "x") #'term-sessions-action-history-html)
    (define-key map (kbd "Y") #'term-sessions-action-copy-history)
    (define-key map (kbd "?") #'term-sessions-action-search-history)
    (define-key map (kbd "w") #'term-sessions-action-copy-name)
    (define-key map (kbd "a") #'term-sessions-action-copy-attach-command)
    (define-key map (kbd "y") #'term-sessions-action-store-org-link)
    (define-key map (kbd "s") #'term-sessions-action-send-command)
    (define-key map (kbd "D") #'term-sessions-action-dired-cwd)
    (define-key map (kbd "P") #'term-sessions-action-dired-project)
    (define-key map (kbd "M-!") #'term-sessions-action-compile-cwd)
    (define-key map (kbd "M-p") #'term-sessions-action-project-compile)
    (define-key map (kbd "!") #'term-sessions-action-run-command)
    (define-key map (kbd "&") #'term-sessions-action-run-async)
    (define-key map (kbd "W") #'term-sessions-action-wait)
    (define-key map (kbd "M-W") #'term-sessions-action-wait-async)
    map)
  "Action map for term-session completion candidates.")

(defun term-sessions-action--entry (candidate)
  "Return session entry for completion CANDIDATE."
  (or (term-sessions--completion-entry candidate)
      (user-error "No term-session candidate: %s" candidate)))

(defun term-sessions-action--call (candidate function)
  "Call FUNCTION with entry for CANDIDATE in that entry's directory."
  (let* ((entry (term-sessions-action--entry candidate))
         (default-directory (term-sessions--entry-directory entry)))
    (funcall function entry)))

(defun term-sessions-action--entry-cwd-directory (entry)
  "Return an Emacs directory name for ENTRY's backend cwd."
  (let* ((backend-directory (term-sessions--entry-directory entry))
         (cwd (term-sessions--string-or-nil (plist-get entry :cwd)))
         (remote-prefix (file-remote-p backend-directory)))
    (file-name-as-directory
     (cond
      ((null cwd) backend-directory)
      ((and remote-prefix (file-name-absolute-p cwd))
       (concat remote-prefix cwd))
      ((and remote-prefix (not (file-remote-p cwd)))
       (concat remote-prefix cwd))
      (t cwd)))))

;;;###autoload
(defun term-sessions-action-open (candidate)
  "Open term session CANDIDATE."
  (interactive (list (term-sessions--read-name "Open session: " t)))
  (term-sessions-action--call
   candidate
   (lambda (entry)
     (term-sessions-open entry))))

;;;###autoload
(defun term-sessions-action-kill (candidate)
  "Kill term session CANDIDATE."
  (interactive (list (term-sessions--read-name "Kill session: " t)))
  (term-sessions-action--call
   candidate
   (lambda (entry)
     (term-sessions-kill (term-sessions--entry-name entry)))))

;;;###autoload
(defun term-sessions-action-history (candidate)
  "Show history for term session CANDIDATE."
  (interactive (list (term-sessions--read-name "History for session: " t)))
  (term-sessions-action--call
   candidate
   (lambda (entry)
     (term-sessions-history (term-sessions--entry-name entry)))))

;;;###autoload
(defun term-sessions-action-history-full (candidate)
  "Show full history for term session CANDIDATE."
  (interactive (list (term-sessions--read-name "Full history for session: " t)))
  (term-sessions-action--call
   candidate
   (lambda (entry)
     (term-sessions-history (term-sessions--entry-name entry) nil))))

;;;###autoload
(defun term-sessions-action-history-vt (candidate)
  "Show VT-formatted history for term session CANDIDATE."
  (interactive (list (term-sessions--read-name "VT history for session: " t)))
  (term-sessions-action--call
   candidate
   (lambda (entry)
     (term-sessions-history (term-sessions--entry-name entry) nil t nil))))

;;;###autoload
(defun term-sessions-action-history-html (candidate)
  "Show HTML history for term session CANDIDATE."
  (interactive (list (term-sessions--read-name "HTML history for session: " t)))
  (term-sessions-action--call
   candidate
   (lambda (entry)
     (term-sessions-history (term-sessions--entry-name entry) nil nil t))))

;;;###autoload
(defun term-sessions-action-copy-history (candidate)
  "Copy full zmx history for term session CANDIDATE."
  (interactive (list (term-sessions--read-name "Copy history for session: " t)))
  (term-sessions-action--call
   candidate
   (lambda (entry)
     (let ((history (term-sessions--zmx "history" (term-sessions--entry-name entry))))
       (kill-new history)
       (message "Copied history for session: %s" (term-sessions--entry-name entry))))))

;;;###autoload
(defun term-sessions-action-search-history (candidate)
  "Search tail history for term session CANDIDATE with Consult when available."
  (interactive (list (term-sessions--read-name "Search history for session: " t)))
  (term-sessions-action--call
   candidate
   (lambda (entry)
     (term-sessions-history (term-sessions--entry-name entry))
     (if (require 'consult nil t)
         (call-interactively #'consult-line)
       (call-interactively #'isearch-forward)))))

;;;###autoload
(defun term-sessions-action-dired-cwd (candidate)
  "Open term session CANDIDATE's current directory in Dired."
  (interactive (list (term-sessions--read-name "Dired session cwd: " t)))
  (term-sessions-action--call
   candidate
   (lambda (entry)
     (dired (term-sessions-action--entry-cwd-directory entry)))))

;;;###autoload
(defun term-sessions-action-dired-project (candidate)
  "Open term session CANDIDATE's project root in Dired."
  (interactive (list (term-sessions--read-name "Dired session project: " t)))
  (term-sessions-action--call
   candidate
   (lambda (entry)
     (let* ((directory (term-sessions-action--entry-cwd-directory entry))
            (default-directory directory)
            (project (and (require 'project nil t)
                          (project-current nil))))
       (unless project
         (user-error "No project for session cwd: %s" directory))
       (dired (project-root project))))))

;;;###autoload
(defun term-sessions-action-compile-cwd (candidate command)
  "Run Compile COMMAND in term session CANDIDATE's current directory."
  (interactive
   (list (term-sessions--read-name "Compile in session cwd: " t)
         (read-shell-command "Compile command: " compile-command)))
  (term-sessions-action--call
   candidate
   (lambda (entry)
     (let ((default-directory (term-sessions-action--entry-cwd-directory entry)))
       (compile command)))))

;;;###autoload
(defun term-sessions-action-project-compile (candidate)
  "Run `project-compile' from term session CANDIDATE's current directory."
  (interactive (list (term-sessions--read-name "Project compile in session cwd: " t)))
  (term-sessions-action--call
   candidate
   (lambda (entry)
     (let ((default-directory (term-sessions-action--entry-cwd-directory entry)))
       (unless (require 'project nil t)
         (user-error "Install project.el to use project compile"))
       (call-interactively #'project-compile)))))

;;;###autoload
(defun term-sessions-action-run-command (candidate command)
  "Run COMMAND in term session CANDIDATE and wait for completion."
  (interactive
   (list (term-sessions--read-name "Run in session: " t)
         (read-string "Command: " nil 'term-sessions-command-history)))
  (term-sessions-action--call
   candidate
   (lambda (entry)
     (term-sessions-run (term-sessions--entry-name entry) command nil))))

;;;###autoload
(defun term-sessions-action-run-async (candidate command)
  "Run COMMAND asynchronously in term session CANDIDATE."
  (interactive
   (list (term-sessions--read-name "Run async in session: " t)
         (read-string "Command: " nil 'term-sessions-command-history)))
  (term-sessions-action--call
   candidate
   (lambda (entry)
     (term-sessions-run-async (term-sessions--entry-name entry) command))))

;;;###autoload
(defun term-sessions-action-wait (candidate)
  "Wait for tracked tasks in term session CANDIDATE."
  (interactive (list (term-sessions--read-name "Wait for session: " t)))
  (term-sessions-action--call
   candidate
   (lambda (entry)
     (term-sessions-wait (term-sessions--entry-name entry)))))

;;;###autoload
(defun term-sessions-action-wait-async (candidate)
  "Wait asynchronously for tracked tasks in term session CANDIDATE."
  (interactive (list (term-sessions--read-name "Wait async for session: " t)))
  (term-sessions-action--call
   candidate
   (lambda (entry)
     (term-sessions-wait-async (term-sessions--entry-name entry)))))

;;;###autoload
(defun term-sessions-action-copy-name (candidate)
  "Copy term session CANDIDATE's name."
  (interactive (list (term-sessions--read-name "Copy name for session: " t)))
  (let ((name (term-sessions--entry-name (term-sessions-action--entry candidate))))
    (kill-new name)
    (message "Copied session name: %s" name)))

;;;###autoload
(defun term-sessions-action-copy-attach-command (candidate)
  "Copy a local attach command for term session CANDIDATE."
  (interactive (list (term-sessions--read-name "Copy attach command for session: " t)))
  (term-sessions-action--call
   candidate
   (lambda (entry)
     (when (file-remote-p default-directory)
       (user-error "Remote term sessions attach through TRAMP; no local command is available to copy"))
     (let ((command (term-sessions--interactive-attach-command
                     (term-sessions--entry-name entry))))
       (kill-new command)
       (message "Copied attach command")))))

;;;###autoload
(defun term-sessions-action-store-org-link (candidate)
  "Store an Org link to term session CANDIDATE."
  (interactive (list (term-sessions--read-name "Store link for session: " t)))
  (term-sessions-action--call
   candidate
   (lambda (entry)
     (term-sessions-store-org-link (term-sessions--entry-name entry)))))

;;;###autoload
(defun term-sessions-action-send-command (candidate command)
  "Send COMMAND to term session CANDIDATE."
  (interactive
   (list (term-sessions--read-name "Send command to session: " t)
         (read-string "Command: " nil 'term-sessions-command-history)))
  (term-sessions-action--call
   candidate
   (lambda (entry)
     (term-sessions-send-command (term-sessions--entry-name entry) command))))

(with-eval-after-load 'embark
  (defvar embark-keymap-alist)
  (add-to-list 'embark-keymap-alist '(term-session . term-sessions-action-map)))

(provide 'term-sessions-actions)
;;; term-sessions-actions.el ends here
