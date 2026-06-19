;;; term-sessions-core.el --- Core data for persistent terminal sessions -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Internal data types and small helpers shared by term-sessions modules.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup term-sessions nil
  "Persistent terminal sessions owned by an external backend."
  :group 'terminals
  :prefix "term-sessions-")

(defcustom term-sessions-backend 'zmx
  "Backend used by `term-sessions'.
Currently only `zmx' is implemented."
  :group 'term-sessions
  :type '(choice (const :tag "zmx" zmx)))

(defcustom term-sessions-preferred-frontend 'term
  "Preferred terminal frontend for interactive session attaches."
  :group 'term-sessions
  :type '(choice (const :tag "vterm" vterm)
                 (const :tag "eat" eat)
                 (const :tag "ghostel" ghostel)
                 (const :tag "term" term)
                 (const :tag "shell" shell)))

(defcustom term-sessions-history-lines 200
  "Default number of history lines to show from zmx history commands."
  :group 'term-sessions
  :type 'integer)

(defvar term-sessions-name-history nil
  "History of session names read by term-sessions commands.")

(defvar term-sessions-command-history nil
  "History of shell commands read by term-sessions prompts.")

(defvar term-sessions--completion-entry-table (make-hash-table :test #'equal)
  "Recently offered completion candidates keyed by display string.")

(defvar-local term-sessions-current-name nil
  "Name of the term session associated with the current buffer.")

(defvar-local term-sessions-current-backend nil
  "Backend symbol for the term session associated with the current buffer.")

(defvar-local term-sessions-current-spec nil
  "`term-sessions-spec' object for the session associated with the current buffer.")

(cl-defstruct (term-sessions-location
               (:constructor term-sessions-location-create)
               (:copier nil))
  "Parsed location for a terminal session operation."
  remote-p
  directory
  prefix
  method
  user
  host
  port
  hop
  localname)

(cl-defstruct (term-sessions-spec
               (:constructor term-sessions-spec-create)
               (:copier nil))
  "Persistable description of a terminal session."
  name
  backend
  location
  cwd
  command
  frontend
  project
  tags
  created-at
  recreate-policy)

(cl-defstruct (term-sessions-session
               (:constructor term-sessions-session-create)
               (:copier nil))
  "Runtime view of a terminal session."
  name
  backend
  location
  spec
  active-p)

(defun term-sessions--command-string (program args)
  "Return shell command string for PROGRAM and ARGS."
  (mapconcat #'shell-quote-argument (cons program args) " "))

(defun term-sessions--buffer-name (name &optional suffix)
  "Return a buffer name for session NAME and optional SUFFIX."
  (format "*term-session:%s%s*" name (if suffix (format ":%s" suffix) "")))

(defun term-sessions--term-buffer-base-name (name)
  "Return a bare buffer base name for `make-term' session NAME.
`make-term' adds the surrounding stars itself."
  (format "term-session:%s" name))

(defun term-sessions--mark-buffer (name &optional spec)
  "Record NAME/backend/SPEC metadata in the current buffer."
  (setq-local term-sessions-current-name name)
  (setq-local term-sessions-current-backend term-sessions-backend)
  (setq-local term-sessions-current-spec spec))

(defun term-sessions--string-or-nil (value)
  "Return VALUE unless it is nil or the empty string."
  (when (and value (not (string-empty-p value)))
    value))

(defun term-sessions--fit-column (value width &optional ellipsis)
  "Return VALUE truncated or padded to display WIDTH.
ELLIPSIS defaults to a single-character ellipsis.  Text properties are not
preserved; callers should add faces/properties after fitting if needed."
  (let* ((ellipsis (or ellipsis "…"))
         (string (format "%s" (or value "")))
         (truncated (truncate-string-to-width string width nil nil ellipsis))
         (padding (- width (string-width truncated))))
    (concat truncated (make-string (max 0 padding) ? ))))

(defun term-sessions--distribute-extra-width (columns extra)
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

(defun term-sessions--scaled-column-widths (columns budget)
  "Return column widths for COLUMNS scaled to BUDGET cells.
COLUMNS is a list of (KEY WIDTH WEIGHT MAX-WIDTH), as accepted by
`term-sessions--distribute-extra-width'."
  (let ((base-total (apply #'+ (mapcar #'cadr columns))))
    (term-sessions--distribute-extra-width
     columns (max 0 (- budget base-total)))))

(defun term-sessions--column-width (widths key &optional fallback)
  "Return WIDTHS value for KEY, or FALLBACK.
FALLBACK defaults to 10."
  (or (alist-get key widths) fallback 10))

(defun term-sessions--directory-key (directory)
  "Return backend identity key for DIRECTORY.
Remote zmx sessions are keyed by TRAMP prefix rather than localname, because
`/rpc:host:/' and `/rpc:host:/some/cwd' query the same zmx server.  Local zmx
sessions are likewise keyed to the local backend rather than to one cwd."
  (or (file-remote-p directory)
      'local))

(defun term-sessions--session-buffer (name directory &optional backend)
  "Return an existing term-sessions BACKEND buffer for NAME at DIRECTORY."
  (let ((directory-key (term-sessions--directory-key directory))
        (backend (or backend term-sessions-backend))
        found)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (null found)
                   (equal term-sessions-current-name name)
                   (eq term-sessions-current-backend backend)
                   (equal (term-sessions--directory-key default-directory)
                          directory-key))
          (setq found buffer))))
    found))

(defun term-sessions--entry-name (entry)
  "Return session name from ENTRY."
  (cond
   ((stringp entry) entry)
   ((consp entry) (plist-get entry :name))
   (t nil)))

(defun term-sessions--entry-directory (entry)
  "Return backend directory from ENTRY, defaulting to `default-directory'."
  (or (and (consp entry) (plist-get entry :directory))
      default-directory))

(defun term-sessions--register-completion-entry (candidate entry)
  "Remember that CANDIDATE names ENTRY for completion actions."
  (puthash (substring-no-properties candidate)
           entry
           term-sessions--completion-entry-table)
  candidate)

(defun term-sessions--completion-entry (candidate)
  "Return session entry associated with CANDIDATE.
Falls back to treating CANDIDATE as a session name in `default-directory'."
  (or (and (stringp candidate)
           (gethash (substring-no-properties candidate)
                    term-sessions--completion-entry-table))
      (and (stringp candidate)
           (list :name (substring-no-properties candidate)
                 :directory default-directory))))

(provide 'term-sessions-core)
;;; term-sessions-core.el ends here
