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

(defvar term-sessions-action-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'term-sessions-action-open)
    (define-key map (kbd "k") #'term-sessions-action-kill)
    (define-key map (kbd "h") #'term-sessions-action-history)
    (define-key map (kbd "w") #'term-sessions-action-copy-name)
    (define-key map (kbd "a") #'term-sessions-action-copy-attach-command)
    (define-key map (kbd "y") #'term-sessions-action-store-org-link)
    (define-key map (kbd "s") #'term-sessions-action-send-command)
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

;;;###autoload
(defun term-sessions-action-open (candidate)
  "Open term session CANDIDATE."
  (interactive (list (term-sessions--read-name "Open session: " t)))
  (term-sessions-action--call
   candidate
   (lambda (entry)
     (let* ((name (term-sessions--entry-name entry))
            (directory (term-sessions--entry-directory entry)))
       (if-let ((buffer (term-sessions--session-buffer name directory term-sessions-backend)))
           (pop-to-buffer buffer)
         (term-sessions-open name))))))

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
(defun term-sessions-action-copy-name (candidate)
  "Copy term session CANDIDATE's name."
  (interactive (list (term-sessions--read-name "Copy name for session: " t)))
  (let ((name (term-sessions--entry-name (term-sessions-action--entry candidate))))
    (kill-new name)
    (message "Copied session name: %s" name)))

;;;###autoload
(defun term-sessions-action-copy-attach-command (candidate)
  "Copy an attach command for term session CANDIDATE."
  (interactive (list (term-sessions--read-name "Copy attach command for session: " t)))
  (term-sessions-action--call
   candidate
   (lambda (entry)
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
