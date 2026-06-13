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

(defun term-sessions-consult--distribute-extra-width (columns extra)
  "Add EXTRA display cells to COLUMNS.
COLUMNS is a list of (KEY WIDTH WEIGHT MAX-WIDTH).  Return an alist of
KEY-to-WIDTH values."
  (let ((columns (mapcar #'copy-sequence columns))
        changed)
    (while (> extra 0)
      (setq changed nil)
      (dolist (column columns)
        (pcase-let ((`(,_key ,width ,weight ,max-width) column))
          (dotimes (_ weight)
            (when (and (> extra 0)
                       (or (null max-width) (< width max-width)))
              (setcar (cdr column) (1+ width))
              (setq width (1+ width)
                    extra (1- extra)
                    changed t)))))
      (unless changed
        (setq extra 0)))
    (mapcar (lambda (column) (cons (car column) (cadr column))) columns)))

(defun term-sessions-consult--column-widths (&optional total-width)
  "Return responsive Consult candidate column widths for TOTAL-WIDTH."
  (let* ((separators 6)
         (window-width (or total-width (max 80 (- (frame-width) 20))))
         (budget (max 66 (- window-width separators)))
         (base '((name 12 1 28)
                 (where 10 1 24)
                 (cwd 18 4 nil)
                 (command 20 5 nil)))
         (base-total (apply #'+ (mapcar #'cadr base))))
    (term-sessions-consult--distribute-extra-width
     base (max 0 (- budget base-total)))))

(defun term-sessions-consult--width (widths key)
  "Return WIDTHS value for KEY."
  (or (alist-get key widths) 10))

(defun term-sessions-consult--display (entry)
  "Return aligned display candidate for ENTRY.
When two entries truncate to the same display string, append a small visible
counter suffix so actions and annotations still resolve to the intended entry."
  (let* ((widths (term-sessions-consult--column-widths))
         (name (term-sessions--fit-column
                (plist-get entry :name)
                (term-sessions-consult--width widths 'name)))
         (where (term-sessions--fit-column
                 (plist-get entry :where)
                 (term-sessions-consult--width widths 'where)))
         (cwd (term-sessions--fit-column
               (abbreviate-file-name (or (plist-get entry :cwd) ""))
               (term-sessions-consult--width widths 'cwd)))
         (command (term-sessions--fit-column
                   (or (plist-get entry :command) "")
                   (term-sessions-consult--width widths 'command)))
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
  (let* ((session-directories (term-sessions-list--session-buffer-directories))
         (directories (term-sessions-list--delete-duplicate-directories
                       (append (list (term-sessions-list--local-directory))
                               session-directories
                               (term-sessions-list--open-remote-directories)))))
    (mapc #'term-sessions-list--clear-remote-failure session-directories)
    (mapcar #'car
            (apply #'append
                   (mapcar #'term-sessions-list--query-directory directories)))))

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
    (let* ((clients (format "c:%s" (or (plist-get entry :clients) "")))
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
  (let* ((entry (term-sessions-consult--entry candidate))
         (name (term-sessions--entry-name entry))
         (directory (term-sessions--entry-directory entry)))
    (if-let ((buffer (term-sessions--session-buffer name directory term-sessions-backend)))
        (pop-to-buffer buffer)
      (let ((default-directory directory))
        (term-sessions-open name)))))

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

(defconst term-sessions-consult--source-session
  `(:name "All term sessions"
    :category term-session
    :annotate term-sessions-consult--annotate
    :action term-sessions-consult--open
    :items ,(lambda () (term-sessions-consult--items)))
  "All term sessions as a Consult source.")

(defconst term-sessions-consult--source-local-session
  `(:name "Local term sessions"
    :narrow (?l . "Local")
    :hidden t
    :category term-session
    :annotate term-sessions-consult--annotate
    :action term-sessions-consult--open
    :items ,(lambda () (term-sessions-consult--items #'term-sessions-consult--local-p)))
  "Local term sessions as a Consult source.")

(defconst term-sessions-consult--source-remote-session
  `(:name "Remote term sessions"
    :narrow (?r . "Remote")
    :hidden t
    :category term-session
    :annotate term-sessions-consult--annotate
    :action term-sessions-consult--open
    :items ,(lambda () (term-sessions-consult--items #'term-sessions-consult--remote-p)))
  "Remote term sessions as a Consult source.")

(defconst term-sessions-consult--source-current-host-session
  `(:name "Current host term sessions"
    :narrow (?h . "Current host")
    :hidden t
    :category term-session
    :annotate term-sessions-consult--annotate
    :action term-sessions-consult--open
    :items ,(lambda () (term-sessions-consult--items #'term-sessions-consult--current-host-p)))
  "Current backend host term sessions as a Consult source.")

(defconst term-sessions-consult--source-current-project-session
  `(:name "Current project/cwd term sessions"
    :narrow (?p . "Project/cwd")
    :hidden t
    :category term-session
    :annotate term-sessions-consult--annotate
    :action term-sessions-consult--open
    :items ,(lambda () (term-sessions-consult--items #'term-sessions-consult--current-project-p)))
  "Current project or cwd term sessions as a Consult source.")

(defconst term-sessions-consult--source-attached-session
  `(:name "Term sessions with clients"
    :narrow (?a . "With clients")
    :hidden t
    :category term-session
    :annotate term-sessions-consult--annotate
    :action term-sessions-consult--open
    :items ,(lambda () (term-sessions-consult--items #'term-sessions-consult--attached-p)))
  "Term sessions with attached clients as a Consult source.")

(defconst term-sessions-consult--source-detached-session
  `(:name "Term sessions without clients"
    :narrow (?d . "No clients")
    :hidden t
    :category term-session
    :annotate term-sessions-consult--annotate
    :action term-sessions-consult--open
    :items ,(lambda ()
              (term-sessions-consult--items
               (lambda (entry) (not (term-sessions-consult--attached-p entry))))))
  "Term sessions without attached clients as a Consult source.")

;;;###autoload
(defun term-sessions-consult-session ()
  "Select and open a term session with Consult multi-source narrowing."
  (interactive)
  (unless (require 'consult nil t)
    (user-error "Install Consult to use `term-sessions-consult-session'"))
  (consult--multi term-sessions-consult-sources
                  :prompt "Term session: "
                  :require-match t
                  :sort nil))

(provide 'term-sessions-consult)
;;; term-sessions-consult.el ends here
