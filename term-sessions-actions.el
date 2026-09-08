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
(declare-function org-element-context "org-element")
(declare-function org-element-property "org-element" (property element))
(declare-function project-root "project" (project))
(declare-function tabulated-list-get-id "tabulated-list")
(defvar compile-command)

(defvar term-sessions-org-link-action-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'term-sessions-action-open-org-link)
    (define-key map (kbd "w") #'term-sessions-action-copy-org-link-target)
    map)
  "Action map for term-session Org link targets.")

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
    (define-key map (kbd "M-d") #'term-sessions-action-copy-cwd)
    (define-key map (kbd "M-c") #'term-sessions-action-copy-command)
    (define-key map (kbd "M-w") #'term-sessions-action-copy-where)
    (define-key map (kbd "M-s") #'term-sessions-action-copy-spec-link)
    (define-key map (kbd "y") #'term-sessions-action-store-org-link)
    (define-key map (kbd "O") #'term-sessions-action-copy-org-link)
    (define-key map (kbd "i") #'term-sessions-action-insert-org-link)
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

(defun term-sessions-action--entry-spec-link (entry)
  "Return raw Org link target for term session ENTRY."
  (let* ((default-directory (term-sessions-action--entry-cwd-directory entry))
         (name (term-sessions--entry-name entry)))
    (term-sessions--spec-org-link
     (term-sessions-spec-current name nil term-sessions-preferred-frontend))))

(defun term-sessions-action--entry-org-link (entry)
  "Return bracketed Org link for term session ENTRY."
  (let* ((default-directory (term-sessions-action--entry-cwd-directory entry))
         (name (term-sessions--entry-name entry)))
    (term-sessions--org-link-for-spec
     (term-sessions-spec-current name nil term-sessions-preferred-frontend))))

(defun term-sessions-action--read-target-session-entry (prompt)
  "Read an existing session entry for target action using PROMPT."
  (if (fboundp 'term-sessions--read-existing-session-entry)
      (term-sessions--read-existing-session-entry prompt)
    (list :name (term-sessions--read-name prompt t)
          :directory default-directory)))

(defun term-sessions-action--with-target-session (prompt function)
  "Read a target session with PROMPT and call FUNCTION with its entry."
  (let* ((entry (term-sessions-action--read-target-session-entry prompt))
         (default-directory (term-sessions--entry-directory entry)))
    (funcall function entry)))

(defun term-sessions-action--current-buffer-entry ()
  "Return a session entry for the current term-session buffer."
  (when term-sessions-current-name
    (list :name term-sessions-current-name
          :directory default-directory
          :cwd (or (and term-sessions-current-spec
                        (term-sessions-spec-cwd term-sessions-current-spec))
                   default-directory)
          :command (and term-sessions-current-spec
                        (term-sessions-spec-command term-sessions-current-spec)))))

(defun term-sessions-action--entry-cwd-directory (entry)
  "Return an Emacs directory name for ENTRY's backend cwd."
  (term-sessions--entry-cwd-directory entry))

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
(defun term-sessions-action-copy-cwd (candidate)
  "Copy term session CANDIDATE's current directory."
  (interactive (list (term-sessions--read-name "Copy cwd for session: " t)))
  (term-sessions-action--call
   candidate
   (lambda (entry)
     (let ((directory (term-sessions-action--entry-cwd-directory entry)))
       (kill-new directory)
       (message "Copied session cwd: %s" directory)))))

;;;###autoload
(defun term-sessions-action-copy-command (candidate)
  "Copy term session CANDIDATE's current command."
  (interactive (list (term-sessions--read-name "Copy command for session: " t)))
  (let* ((entry (term-sessions-action--entry candidate))
         (command (or (plist-get entry :command) "")))
    (kill-new command)
    (message "Copied session command")))

;;;###autoload
(defun term-sessions-action-copy-where (candidate)
  "Copy term session CANDIDATE's host/location label."
  (interactive (list (term-sessions--read-name "Copy location for session: " t)))
  (let* ((entry (term-sessions-action--entry candidate))
         (where (or (plist-get entry :where)
                    (if (file-remote-p (term-sessions--entry-directory entry))
                        (file-remote-p (term-sessions--entry-directory entry))
                      "local"))))
    (kill-new where)
    (message "Copied session location: %s" where)))

;;;###autoload
(defun term-sessions-action-copy-spec-link (candidate)
  "Copy a raw term-session spec link for CANDIDATE."
  (interactive (list (term-sessions--read-name "Copy spec link for session: " t)))
  (term-sessions-action--call
   candidate
   (lambda (entry)
     (let ((link (term-sessions-action--entry-spec-link entry)))
       (kill-new link)
       (message "Copied session spec link: %s" (term-sessions--entry-name entry))))))

;;;###autoload
(defun term-sessions-action-store-org-link (candidate)
  "Store an Org link to term session CANDIDATE."
  (interactive (list (term-sessions--read-name "Store link for session: " t)))
  (term-sessions-action--call
   candidate
   (lambda (entry)
     (term-sessions-store-org-link (term-sessions--entry-name entry)))))

;;;###autoload
(defun term-sessions-action-copy-org-link (candidate)
  "Copy a bracketed Org link for term session CANDIDATE."
  (interactive (list (term-sessions--read-name "Copy Org link for session: " t)))
  (term-sessions-action--call
   candidate
   (lambda (entry)
     (let ((link (term-sessions-action--entry-org-link entry)))
       (kill-new link)
       (message "Copied Org link for session: %s" (term-sessions--entry-name entry))))))

;;;###autoload
(defun term-sessions-action-insert-org-link (candidate)
  "Insert a bracketed Org link for term session CANDIDATE at point."
  (interactive (list (term-sessions--read-name "Insert Org link for session: " t)))
  (term-sessions-action--call
   candidate
   (lambda (entry)
     (insert (term-sessions-action--entry-org-link entry)))))

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

;;;###autoload
(defun term-sessions-action-send-text-to-session (text)
  "Send raw TEXT to a selected term session."
  (interactive
   (list (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Text to send: "))))
  (term-sessions-action--with-target-session
   "Send text to session: "
   (lambda (entry)
     (term-sessions-send (term-sessions--entry-name entry) text))))

;;;###autoload
(defun term-sessions-action-send-command-text-to-session (text)
  "Send TEXT as a shell command to a selected term session."
  (interactive (list (read-string "Command: ")))
  (term-sessions-action--with-target-session
   "Send command to session: "
   (lambda (entry)
     (term-sessions-send-command (term-sessions--entry-name entry) text))))

;;;###autoload
(defun term-sessions-action-send-file-path-to-session (file)
  "Send shell-quoted FILE path to a selected term session."
  (interactive "fFile path to send: ")
  (term-sessions-action-send-text-to-session (shell-quote-argument file)))

;;;###autoload
(defun term-sessions-action-run-file-in-session (file)
  "Run shell-quoted FILE path in a selected term session."
  (interactive "fFile to run: ")
  (term-sessions-action--with-target-session
   "Run file in session: "
   (lambda (entry)
     (term-sessions-run (term-sessions--entry-name entry)
                        (shell-quote-argument file)
                        nil))))

;;;###autoload
(defun term-sessions-action-open-org-link (link)
  "Open term-session Org LINK."
  (interactive "sTerm-session link: ")
  (let ((path (if (string-prefix-p "term-session:" link)
                  (substring link (length "term-session:"))
                link)))
    (term-sessions--open-org-path path nil)))

;;;###autoload
(defun term-sessions-action-copy-org-link-target (link)
  "Copy term-session Org LINK target."
  (interactive "sTerm-session link: ")
  (kill-new link)
  (message "Copied term-session link"))

(defun term-sessions-action--org-element-link-target ()
  "Return an Embark target for an Org term-session link at point."
  (when (and (fboundp 'org-element-context)
             (fboundp 'org-element-property))
    (let ((context (org-element-context)))
      (when (and (eq (car-safe context) 'link)
                 (equal (org-element-property :type context) "term-session"))
        (let ((begin (org-element-property :begin context))
              (end (org-element-property :end context))
              (path (org-element-property :path context)))
          `(term-session-link ,(concat "term-session:" path) ,begin . ,end))))))

(defun term-sessions-action--raw-link-target ()
  "Return an Embark target for a raw term-session link at point."
  (let ((line-beginning (line-beginning-position))
        (line-end (line-end-position))
        start end)
    (save-excursion
      (when (search-backward "term-session:spec:" line-beginning t)
        (setq start (point))
        (goto-char (match-end 0))
        (skip-chars-forward "[:alnum:]%._~+=&@/:#-" line-end)
        (setq end (point))))
    (when (and start (<= start (point)) (<= (point) end))
      `(term-session-link ,(buffer-substring-no-properties start end) ,start . ,end))))

(defun term-sessions-action-org-link-target ()
  "Return an Embark target for a term-session Org link at point."
  (or (term-sessions-action--org-element-link-target)
      (term-sessions-action--raw-link-target)))

(defun term-sessions-action-list-row-target ()
  "Return an Embark target for the `term-sessions-list-mode' row at point."
  (when (and (derived-mode-p 'term-sessions-list-mode)
             (fboundp 'tabulated-list-get-id))
    (when-let* ((entry (tabulated-list-get-id)))
      (let ((candidate (term-sessions--register-completion-entry
                        (format "%s @ %s %s"
                                (term-sessions--entry-name entry)
                                (or (plist-get entry :where) "")
                                (abbreviate-file-name
                                 (or (plist-get entry :cwd) "")))
                        entry)))
        `(term-session . ,candidate)))))

(defun term-sessions-action-current-buffer-target ()
  "Return an Embark target for the term session owned by the current buffer."
  (when-let* ((entry (term-sessions-action--current-buffer-entry)))
    (let ((candidate (term-sessions--register-completion-entry
                      (term-sessions--entry-name entry) entry)))
      `(term-session . ,candidate))))

(defvar embark-expression-map)
(defvar embark-file-map)
(defvar embark-identifier-map)
(defvar embark-keymap-alist)
(defvar embark-region-map)
(defvar embark-target-finders)

(defconst term-sessions--embark-bindings
  '((embark-region-map "S" term-sessions-action-send-text-to-session)
    (embark-region-map "C" term-sessions-action-send-command-text-to-session)
    (embark-file-map "S" term-sessions-action-send-file-path-to-session)
    (embark-file-map "R" term-sessions-action-run-file-in-session)
    (embark-expression-map "S" term-sessions-action-send-command-text-to-session)
    (embark-identifier-map "S" term-sessions-action-send-command-text-to-session))
  "Embark keybindings managed by setup and teardown.
Each entry is a (KEYMAP KEY COMMAND) triple.")

(defconst term-sessions--embark-keymap-entries
  '((term-session . term-sessions-action-map)
    (term-session-link . term-sessions-org-link-action-map))
  "Entries added to `embark-keymap-alist' by setup.")

(defconst term-sessions--embark-target-finders
  '((term-sessions-action-org-link-target)
    (term-sessions-action-list-row-target append)
    (term-sessions-action-current-buffer-target append))
  "Finders added to `embark-target-finders' by setup.
Each entry is (FINDER . APPEND-P), mirroring `add-to-list' arguments.")

(defvar term-sessions--embark-installed nil
  "Non-nil after `term-sessions-embark-setup' has installed its state.")

(defvar term-sessions--embark-prior-bindings nil
  "Embark bindings shadowed by setup, as (KEYMAP KEY DEFINITION) triples.")

(defvar term-sessions--embark-added-alist-entries nil
  "Integration entries installed by setup, as (ALIST-SYMBOL . ENTRY) pairs.
Only these entries are removed again on teardown.")

;;;###autoload
(defun term-sessions-embark-setup ()
  "Wire term-sessions actions into Embark.
Install keybindings, keymap entries and target finders.  Safe to
call multiple times.  Undo with `term-sessions-embark-teardown'.
When Embark is already loaded this takes effect immediately;
otherwise call this once Embark is available."
  (interactive)
  (require 'embark)
  (unless term-sessions--embark-installed
    (setq term-sessions--embark-prior-bindings
          (mapcar (lambda (spec)
                    (list (nth 0 spec) (nth 1 spec)
                          (lookup-key (symbol-value (nth 0 spec))
                                      (kbd (nth 1 spec)))))
                  term-sessions--embark-bindings))
    (setq term-sessions--embark-added-alist-entries nil)
    (dolist (spec term-sessions--embark-bindings)
      (define-key (symbol-value (nth 0 spec))
                  (kbd (nth 1 spec)) (nth 2 spec)))
    (dolist (entry term-sessions--embark-keymap-entries)
      (unless (member entry embark-keymap-alist)
        (push entry embark-keymap-alist)
        (push (cons 'embark-keymap-alist entry)
              term-sessions--embark-added-alist-entries)))
    (dolist (spec term-sessions--embark-target-finders)
      (unless (member (car spec) embark-target-finders)
        (add-to-list 'embark-target-finders (car spec) (cdr spec))
        (push (cons 'embark-target-finders (car spec))
              term-sessions--embark-added-alist-entries)))
    (setq term-sessions--embark-installed t)))

(defun term-sessions-embark-teardown ()
  "Remove term-sessions actions from Embark.
Restore the keybindings shadowed by setup; entries added to Embark
alists after setup are left alone."
  (when (and term-sessions--embark-installed (featurep 'embark))
    (dolist (saved term-sessions--embark-prior-bindings)
      (define-key (symbol-value (nth 0 saved))
                  (kbd (nth 1 saved)) (nth 2 saved)))
    (dolist (added term-sessions--embark-added-alist-entries)
      (set (car added) (delete (cdr added) (symbol-value (car added))))))
  (setq term-sessions--embark-prior-bindings nil
        term-sessions--embark-added-alist-entries nil
        term-sessions--embark-installed nil))

(defun term-sessions-actions-unload-function ()
  "Remove Embark integration before unloading this feature."
  (term-sessions-embark-teardown))

;; Install eagerly when Embark arrived first; otherwise call
;; `term-sessions-embark-setup' once Embark is loaded.
(when (featurep 'embark)
  (term-sessions-embark-setup))

(provide 'term-sessions-actions)
;;; term-sessions-actions.el ends here
