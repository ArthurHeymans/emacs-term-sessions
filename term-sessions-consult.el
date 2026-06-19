;;; term-sessions-consult.el --- Consult integration for term-sessions -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Consult multi-source session picker for term-sessions.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'term-sessions-core)
(require 'term-sessions-zmx)
(require 'term-sessions-tramp)
(require 'term-sessions-frontends)
(require 'term-sessions-list)

(declare-function consult--multi "consult")
(declare-function project-root "project" (project))

(defcustom term-sessions-consult-sources
  '(term-sessions-consult--source-session
    term-sessions-consult--source-local-session
    term-sessions-consult--source-remote-session
    term-sessions-consult--source-current-host-session
    term-sessions-consult--source-current-project-session
    term-sessions-consult--source-attached-session
    term-sessions-consult--source-detached-session)
  "Sources used by `term-sessions-consult-session'."
  :group 'term-sessions
  :type '(repeat symbol))

(defvar term-sessions-consult--entry-table (make-hash-table :test #'equal)
  "Session entries keyed by Consult candidate string.")

(defun term-sessions-consult--column-widths (&optional total-width)
  "Return responsive Consult candidate column widths for TOTAL-WIDTH."
  (let* ((separators 6)
         (window-width (or total-width (max 80 (- (frame-width) 20))))
         (budget (max 66 (- window-width separators)))
         (base '((name 12 1 28)
                 (where 10 1 24)
                 (cwd 18 4 nil)
                 (command 20 5 nil))))
    (term-sessions--scaled-column-widths base budget)))

(defun term-sessions-consult--display (entry)
  "Return aligned display candidate for ENTRY.
When two entries truncate to the same display string, append a small visible
counter suffix so actions and annotations still resolve to the intended entry."
  (let* ((widths (term-sessions-consult--column-widths))
         (name (term-sessions--fit-column
                (plist-get entry :name)
                (term-sessions--column-width widths 'name)))
         (where (term-sessions--fit-column
                 (plist-get entry :where)
                 (term-sessions--column-width widths 'where)))
         (cwd (term-sessions--fit-column
               (abbreviate-file-name (or (plist-get entry :cwd) ""))
               (term-sessions--column-width widths 'cwd)))
         (command (term-sessions--fit-column
                   (or (plist-get entry :command) "")
                   (term-sessions--column-width widths 'command)))
         (base (format "%s  %s  %s  %s" name where cwd command))
         (candidate base)
         (counter 2))
    (while (gethash candidate term-sessions-consult--entry-table)
      (setq candidate (format "%s  #%d" base counter)
            counter (1+ counter)))
    (puthash candidate entry term-sessions-consult--entry-table)
    (term-sessions--register-completion-entry candidate entry)))

(defun term-sessions-consult--entry (candidate)
  "Return session entry for CANDIDATE."
  (or (gethash (substring-no-properties candidate) term-sessions-consult--entry-table)
      (term-sessions--completion-entry candidate)))

(defun term-sessions-consult--entries ()
  "Return session entries across local and already-open TRAMP remotes."
  (clrhash term-sessions-consult--entry-table)
  (mapcar #'car (term-sessions-list--session-rows)))

(defun term-sessions-consult--items (&optional predicate)
  "Return Consult item strings filtered by PREDICATE."
  (mapcar #'term-sessions-consult--display
          (seq-filter (or predicate #'always)
                      (term-sessions-consult--entries))))

(defun term-sessions-consult--annotate (candidate)
  "Annotate CANDIDATE with compact client/project/update metadata.
The candidate itself carries the high-value scan columns: name, host, cwd, and
running command."
  (when-let ((entry (term-sessions-consult--entry candidate)))
    (let* ((client-value (term-sessions--string-or-nil (plist-get entry :clients)))
           (clients (if client-value (format "c:%s" client-value) ""))
           (project-value (term-sessions--string-or-nil (plist-get entry :project)))
           (project (term-sessions--fit-column
                     (if project-value (format "[%s]" project-value) "")
                     18))
           (updated (if-let ((updated (term-sessions--string-or-nil
                                       (plist-get entry :updated))))
                        (format "updated:%s" updated)
                      "")))
      (if (and (string-empty-p clients)
               (string-empty-p (string-trim project))
               (string-empty-p updated))
          ""
        (format "  %s  %s  %s" clients project updated)))))

(defun term-sessions-consult--open (candidate)
  "Open the session named by CANDIDATE."
  (term-sessions-open (term-sessions-consult--entry candidate)))

(defun term-sessions-consult--open-new (name)
  "Create and open a new term session NAME in `default-directory'."
  (let ((name (string-trim (substring-no-properties name))))
    (when (string-empty-p name)
      (user-error "No term session name"))
    (term-sessions-open name)))

(defun term-sessions-consult--local-p (entry)
  "Return non-nil when ENTRY is local."
  (not (file-remote-p (term-sessions--entry-directory entry))))

(defun term-sessions-consult--remote-p (entry)
  "Return non-nil when ENTRY is remote."
  (file-remote-p (term-sessions--entry-directory entry)))

(defun term-sessions-consult--backend-key (&optional directory)
  "Return backend host identity key for DIRECTORY."
  (let ((location (term-sessions--location (or directory default-directory))))
    (if (term-sessions-location-remote-p location)
        (list (or (term-sessions-location-user location) (user-login-name))
              (term-sessions-location-host location)
              (term-sessions-location-port location))
      'local)))

(defun term-sessions-consult--current-host-p (entry)
  "Return non-nil when ENTRY belongs to the current backend host."
  (equal (term-sessions-consult--backend-key (term-sessions--entry-directory entry))
         (term-sessions-consult--backend-key default-directory)))

(defun term-sessions-consult--current-project-p (entry)
  "Return non-nil when ENTRY is in the current project or cwd subtree."
  (let* ((entry-cwd (or (plist-get entry :cwd) ""))
         (directory (or (file-remote-p default-directory 'localname)
                        default-directory))
         (root (or (and (not (file-remote-p default-directory))
                        (ignore-errors
                          (when-let* ((project (and (require 'project nil t)
                                                    (project-current nil default-directory)))
                                      (project-root (project-root project)))
                            project-root)))
                   directory)))
    (and (term-sessions-consult--current-host-p entry)
         (not (string-empty-p entry-cwd))
         (string-prefix-p (file-name-as-directory (expand-file-name root))
                          (file-name-as-directory (expand-file-name entry-cwd))))))

(defun term-sessions-consult--attached-p (entry)
  "Return non-nil when ENTRY has clients."
  (> (term-sessions-list--clients-number (plist-get entry :clients)) 0))

(defun term-sessions-consult--make-source (name predicate &rest properties)
  "Return a Consult source named NAME filtered by PREDICATE.
PROPERTIES are source-specific plist entries, such as `:narrow' and `:hidden'."
  (append
   (list :name name)
   properties
   (list :category 'term-session
         :annotate 'term-sessions-consult--annotate
         :action 'term-sessions-consult--open
         :items (lambda ()
                  (if predicate
                      (term-sessions-consult--items predicate)
                    (term-sessions-consult--items))))))

(defconst term-sessions-consult--source-session
  (term-sessions-consult--make-source "All term sessions" nil)
  "All term sessions as a Consult source.")

(defconst term-sessions-consult--source-local-session
  (term-sessions-consult--make-source
   "Local term sessions" #'term-sessions-consult--local-p
   :narrow '(?l . "Local")
   :hidden t)
  "Local term sessions as a Consult source.")

(defconst term-sessions-consult--source-remote-session
  (term-sessions-consult--make-source
   "Remote term sessions" #'term-sessions-consult--remote-p
   :narrow '(?r . "Remote")
   :hidden t)
  "Remote term sessions as a Consult source.")

(defconst term-sessions-consult--source-current-host-session
  (term-sessions-consult--make-source
   "Current host term sessions" #'term-sessions-consult--current-host-p
   :narrow '(?h . "Current host")
   :hidden t)
  "Current backend host term sessions as a Consult source.")

(defconst term-sessions-consult--source-current-project-session
  (term-sessions-consult--make-source
   "Current project/cwd term sessions" #'term-sessions-consult--current-project-p
   :narrow '(?p . "Project/cwd")
   :hidden t)
  "Current project or cwd term sessions as a Consult source.")

(defconst term-sessions-consult--source-attached-session
  (term-sessions-consult--make-source
   "Term sessions with clients" #'term-sessions-consult--attached-p
   :narrow '(?a . "With clients")
   :hidden t)
  "Term sessions with attached clients as a Consult source.")

(defconst term-sessions-consult--source-detached-session
  (term-sessions-consult--make-source
   "Term sessions without clients"
   (lambda (entry) (not (term-sessions-consult--attached-p entry)))
   :narrow '(?d . "No clients")
   :hidden t)
  "Term sessions without attached clients as a Consult source.")

;;;###autoload
(defun term-sessions-consult-session ()
  "Select and open a term session with Consult multi-source narrowing.
If the selected name does not match an existing session, create and open it in
`default-directory'."
  (interactive)
  (unless (require 'consult nil t)
    (user-error "Install Consult to use `term-sessions-consult-session'"))
  (let ((selected (consult--multi term-sessions-consult-sources
                                  :prompt "Term session: "
                                  :require-match nil
                                  :sort nil)))
    (unless (plist-get (cdr selected) :match)
      (term-sessions-consult--open-new (car selected)))))

(provide 'term-sessions-consult)
;;; term-sessions-consult.el ends here
