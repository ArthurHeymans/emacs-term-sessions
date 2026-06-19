;;; term-sessions-tramp.el --- TRAMP integration for term-sessions -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Location parsing and remote attach transport selection.

;;; Code:

(require 'subr-x)
(require 'tramp)
(require 'term-sessions-core)
(require 'term-sessions-zmx)

(declare-function project-root "project" (project))

(defcustom term-sessions-attach-transport 'auto
  "Transport used for interactive zmx attaches.
The value `auto' prefers local execution for local directories, TRAMP process
APIs for remote directories when the selected frontend supports them.

The value `tramp-rpc' is an alias for TRAMP process attach that additionally
requires the current TRAMP method to be `rpc'."
  :group 'term-sessions
  :type '(choice (const :tag "Auto" auto)
                 (const :tag "Local process" local)
                 (const :tag "TRAMP process" tramp-process)
                 (const :tag "TRAMP RPC process" tramp-rpc)))

(defcustom term-sessions-tramp-process-frontends '(term eat ghostel vterm shell)
  "Frontends that can attach through TRAMP process APIs.
These frontends start the attach command while `default-directory' is remote,
so TRAMP or tramp-rpc owns the transport."
  :group 'term-sessions
  :type '(repeat symbol))

(defvar term-sessions-current-time-function #'current-time
  "Function used to timestamp newly-created session specs.")

(defun term-sessions--split-host-port (host)
  "Return (HOST PORT) parsed from TRAMP HOST.
This is a fallback for methods that are not known to `tramp-dissect-file-name',
where `file-remote-p' reports HOST as \"host#port\"."
  (if (and host (string-match-p "#[0-9]+\\'" host))
      (let ((pos (string-match "#[0-9]+\\'" host)))
        (list (substring host 0 pos) (substring host (1+ pos))))
    (list host nil)))

(defun term-sessions--location (&optional directory)
  "Return a `term-sessions-location' for DIRECTORY.
The full TRAMP identity is preserved where Emacs exposes it: method, user,
host, port, hop, prefix, and localname.  Unknown TRAMP methods are still parsed
with public `file-remote-p' accessors so tests and configurations can describe
methods such as /rpc: before their file handler is loaded."
  (let* ((directory (or directory default-directory))
         (prefix (file-remote-p directory)))
    (if (not prefix)
        (term-sessions-location-create
         :remote-p nil
         :directory (file-name-as-directory (expand-file-name directory))
         :localname (file-name-as-directory (expand-file-name directory)))
      (let* ((method (file-remote-p directory 'method))
             (user (file-remote-p directory 'user))
             (raw-host (file-remote-p directory 'host))
             (hop (file-remote-p directory 'hop))
             (localname (or (file-remote-p directory 'localname) "~"))
             (host-port (term-sessions--split-host-port raw-host))
             (host (car host-port))
             (port (cadr host-port)))
        (when (assoc method tramp-methods)
          (condition-case nil
              (let ((vec (tramp-dissect-file-name directory)))
                (setq method (tramp-file-name-method vec)
                      user (tramp-file-name-user vec)
                      host (tramp-file-name-host vec)
                      port (tramp-file-name-port vec)
                      hop (tramp-file-name-hop vec)
                      localname (tramp-file-name-localname vec)))
            (error nil)))
        (term-sessions-location-create
         :remote-p t
         :directory directory
         :prefix prefix
         :method method
         :user user
         :host host
         :port port
         :hop hop
         :localname (if (or (null localname) (string-empty-p localname))
                        "~"
                      localname))))))

(defun term-sessions--location-target (location)
  "Return user@host target string for LOCATION."
  (let ((user (term-sessions-location-user location))
        (host (term-sessions-location-host location)))
    (if (and user (not (string-empty-p user)))
        (format "%s@%s" user host)
      host)))

(defun term-sessions--location-remote-label (location)
  "Return a display label preserving LOCATION's full TRAMP route."
  (let* ((method (term-sessions-location-method location))
         (target (term-sessions--location-target location))
         (port (term-sessions-location-port location))
         (hop (term-sessions-location-hop location))
         (final (format "%s:%s%s" method target (if port (concat "#" port) ""))))
    (concat (or hop "") final)))

(defun term-sessions--remote-info (&optional directory)
  "Return remote plist for DIRECTORY, or nil for local directories.
The returned plist preserves method, user, host, port, hop, target,
localname, prefix, and directory."
  (let ((location (term-sessions--location directory)))
    (when (term-sessions-location-remote-p location)
      (list :method (term-sessions-location-method location)
            :user (term-sessions-location-user location)
            :host (term-sessions-location-host location)
            :port (term-sessions-location-port location)
            :hop (term-sessions-location-hop location)
            :target (term-sessions--location-target location)
            :localname (term-sessions-location-localname location)
            :prefix (term-sessions-location-prefix location)
            :directory (term-sessions-location-directory location)))))

(defun term-sessions--frontend-supports-tramp-process-p (frontend)
  "Return non-nil if FRONTEND can run an attach through TRAMP process APIs."
  (memq frontend term-sessions-tramp-process-frontends))

(defun term-sessions--resolve-attach-transport (&optional location frontend transport)
  "Resolve TRANSPORT for LOCATION and FRONTEND.
Return one of `local' or `tramp-process'."
  (let* ((location (or location (term-sessions--location)))
         (frontend (or frontend term-sessions-preferred-frontend))
         (transport (or transport term-sessions-attach-transport)))
    (pcase transport
      ('auto
       (cond
        ((not (term-sessions-location-remote-p location)) 'local)
        ((term-sessions--frontend-supports-tramp-process-p frontend) 'tramp-process)
        (t
         (user-error "Frontend `%s' cannot attach through TRAMP process APIs" frontend))))
      ('local
       (when (term-sessions-location-remote-p location)
         (user-error "Local attach transport cannot be used for remote directory `%s'"
                     (term-sessions-location-directory location)))
       'local)
      ('tramp-rpc
       (unless (and (term-sessions-location-remote-p location)
                    (string= (term-sessions-location-method location) "rpc"))
         (user-error "TRAMP RPC attach transport requires an /rpc: directory"))
       (unless (term-sessions--frontend-supports-tramp-process-p frontend)
         (user-error "Frontend `%s' cannot attach through TRAMP process APIs" frontend))
       'tramp-process)
      ('tramp-process
       (unless (term-sessions-location-remote-p location)
         (user-error "TRAMP process attach transport requires a remote directory"))
       (unless (term-sessions--frontend-supports-tramp-process-p frontend)
         (user-error "Frontend `%s' cannot attach through TRAMP process APIs" frontend))
       'tramp-process)
      (_ (user-error "Unknown attach transport: %S" transport)))))

(defun term-sessions--interactive-attach-command (name &optional command)
  "Return local shell command for an interactive attach to NAME and COMMAND."
  (when (file-remote-p default-directory)
    (user-error "Remote interactive attaches must use TRAMP process APIs"))
  (term-sessions--attach-command name command))

(defun term-sessions--ensure-interactive-attach-supported (&optional location frontend transport)
  "Signal unless interactive attach is supported for LOCATION and FRONTEND.
TRANSPORT defaults to `term-sessions-attach-transport'.  Local sessions are
supported directly; remote sessions use configured transport resolution."
  (term-sessions--resolve-attach-transport location frontend transport))

(defun term-sessions--project-name (&optional directory)
  "Return a project name for DIRECTORY, or nil."
  (let ((directory (or directory default-directory)))
    ;; Avoid opening TRAMP connections while merely constructing link/spec
    ;; metadata.  Remote project detection can be added later with caching.
    (unless (file-remote-p directory)
      (when (require 'project nil t)
        (when-let* ((project (project-current nil directory))
                    (root (project-root project)))
          (file-name-nondirectory (directory-file-name root)))))))

(defun term-sessions-spec-current (name &optional command frontend tags recreate-policy)
  "Return a `term-sessions-spec' for NAME in `default-directory'.
COMMAND, FRONTEND, TAGS, and RECREATE-POLICY populate the spec metadata."
  (let* ((location (term-sessions--location default-directory))
         (cwd default-directory)
         (frontend (or frontend term-sessions-preferred-frontend)))
    (term-sessions-spec-create
     :name name
     :backend term-sessions-backend
     :location location
     :cwd cwd
     :command (term-sessions--string-or-nil command)
     :frontend frontend
     :project (term-sessions--project-name cwd)
     :tags tags
     :created-at (format-time-string "%FT%T%z" (funcall term-sessions-current-time-function))
     :recreate-policy (or recreate-policy 'ask))))

(provide 'term-sessions-tramp)
;;; term-sessions-tramp.el ends here
