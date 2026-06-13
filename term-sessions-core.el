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

(defvar term-sessions-name-history nil)
(defvar term-sessions-command-history nil)

(defvar-local term-sessions-current-name nil)
(defvar-local term-sessions-current-backend nil)
(defvar-local term-sessions-current-spec nil)

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

(defun term-sessions--directory-key (directory)
  "Return backend identity key for DIRECTORY.
Remote zmx sessions are keyed by TRAMP prefix rather than localname, because
`/rpc:host:/' and `/rpc:host:/some/cwd' query the same zmx server.  Local zmx
sessions are likewise keyed to the local backend rather than to one cwd."
  (or (file-remote-p directory)
      'local))

(defun term-sessions--session-buffer (name directory &optional backend)
  "Return an existing term-sessions buffer for NAME at DIRECTORY, or nil."
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

(provide 'term-sessions-core)
;;; term-sessions-core.el ends here
