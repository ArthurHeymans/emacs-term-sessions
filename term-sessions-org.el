;;; term-sessions-org.el --- Org links for term-sessions -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Org link storage and following for persistent terminal sessions.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'url-util)
(require 'term-sessions-core)
(require 'term-sessions-zmx)
(require 'term-sessions-tramp)
(require 'term-sessions-frontends)

(declare-function org-link-set-parameters "ol" (type &rest parameters))
(declare-function org-link-store-props "ol" (&rest plist))

(defun term-sessions--org-encode-alist (alist)
  "Encode ALIST as a query string."
  (string-join
   (mapcar (lambda (pair)
             (format "%s=%s"
                     (url-hexify-string (symbol-name (car pair)))
                     (url-hexify-string (format "%s" (cdr pair)))))
           (seq-filter #'cdr alist))
   "&"))

(defun term-sessions--org-decode-query (query)
  "Decode QUERY into a plist with keyword keys."
  (let (plist)
    (dolist (part (split-string query "&" t))
      (pcase-let ((`(,key ,value) (split-string part "=")))
        (when key
          (setq plist (plist-put plist
                                 (intern (concat ":" (url-unhex-string key)))
                                 (url-unhex-string (or value "")))))))
    plist))

(defun term-sessions--spec-org-link (spec)
  "Build an Org link for SPEC."
  (let* ((location (term-sessions-spec-location spec))
         (query (term-sessions--org-encode-alist
                 `((backend . ,(term-sessions-spec-backend spec))
                   (name . ,(term-sessions-spec-name spec))
                   (directory . ,(term-sessions-spec-cwd spec))
                   (cwd . ,(term-sessions-spec-cwd spec))
                   (command . ,(term-sessions-spec-command spec))
                   (frontend . ,(term-sessions-spec-frontend spec))
                   (project . ,(term-sessions-spec-project spec))
                   (created-at . ,(term-sessions-spec-created-at spec))
                   (recreate-policy . ,(term-sessions-spec-recreate-policy spec))
                   (remote . ,(and (term-sessions-location-remote-p location) "t"))
                   (method . ,(term-sessions-location-method location))
                   (user . ,(term-sessions-location-user location))
                   (host . ,(term-sessions-location-host location))
                   (port . ,(term-sessions-location-port location))
                   (hop . ,(term-sessions-location-hop location))
                   (localname . ,(term-sessions-location-localname location))))))
    (format "term-session:spec:%s" query)))

(defun term-sessions--org-link (backend name)
  "Build an Org link for BACKEND session NAME in `default-directory'."
  (let ((term-sessions-backend backend))
    (term-sessions--spec-org-link (term-sessions-spec-current name nil term-sessions-preferred-frontend))))

(defun term-sessions--org-link-description (name &optional spec)
  "Return default Org link description for session NAME and optional SPEC.
Keep the session name first so inserted links remain recognizable even when
additional location context is included."
  (if-let* ((spec spec)
            (cwd (term-sessions-spec-cwd spec))
            (location (term-sessions-spec-location spec)))
      (let ((dir (term-sessions--short-directory-name cwd)))
        (if (term-sessions-location-remote-p location)
            (format "%s @ %s:%s"
                    name (term-sessions--location-remote-label location) dir)
          (format "%s @ %s" name dir)))
    name))

;;;###autoload
(defun term-sessions-store-org-link (&optional name)
  "Store an Org link to persistent session NAME.
When called from a session buffer, default to that session."
  (interactive)
  ;; Org store functions can be called with Org's prefix/context argument.
  ;; Treat only string NAME values as an explicit session name; otherwise use
  ;; the current term-sessions buffer metadata or prompt.  Without this,
  ;; `C-u 1 C-c l' can accidentally store a session named "1".
  (let* ((name (or (and (stringp name) name)
                   term-sessions-current-name
                   (term-sessions--read-name "Store link for session: " t)))
         (backend (or term-sessions-current-backend term-sessions-backend))
         (spec (or term-sessions-current-spec
                   (let ((term-sessions-backend backend))
                     (term-sessions-spec-current name nil term-sessions-preferred-frontend))))
         (link (term-sessions--spec-org-link spec))
         (description (term-sessions--org-link-description name spec)))
    (if (fboundp 'org-link-store-props)
        (org-link-store-props :type "term-session"
                              :link link
                              :description description)
      (kill-new (format "[[%s][%s]]" link description)))
    (message "Stored %s" link)
    link))

(defun term-sessions--org-path-components (path)
  "Parse Org term-session link PATH.
Return a plist with at least :backend, :location, :target, :name, :directory,
:cwd, :command, :frontend, :project, and :created-at.  Legacy local/ssh links
are still accepted."
  (if (string-prefix-p "spec:" path)
      (let ((plist (term-sessions--org-decode-query (substring path 5))))
        (plist-put plist :location "spec")
        (unless (plist-get plist :backend)
          (user-error "Invalid term-session spec link: %s" path))
        (unless (plist-get plist :name)
          (user-error "Invalid term-session spec link: %s" path))
        plist)
    (pcase-let ((`(,backend ,location . ,rest) (split-string path ":")))
      (unless (and backend location rest)
        (user-error "Invalid term-session link: %s" path))
      (pcase location
        ("local"
         (list :backend backend
               :location location
               :target nil
               :name (url-unhex-string (string-join rest ":"))))
        ("ssh"
         (unless (cdr rest)
           (user-error "Invalid SSH term-session link: %s" path))
         (let ((target (url-unhex-string (car rest)))
               (name (url-unhex-string (string-join (cdr rest) ":"))))
           (list :backend backend
                 :location location
                 :target target
                 :directory (format "/ssh:%s:~/" target)
                 :cwd (format "/ssh:%s:~/" target)
                 :name name)))
        (_
         (user-error "Unsupported term-session location: %s" location))))))

(defun term-sessions--org-default-directory (components)
  "Return `default-directory' for Org link COMPONENTS."
  (or (plist-get components :cwd)
      (plist-get components :directory)
      (pcase (plist-get components :location)
        ("local" default-directory)
        ("ssh" (format "/ssh:%s:~/" (plist-get components :target)))
        (_ (user-error "Unsupported term-session location: %s"
                       (plist-get components :location))))))

(defun term-sessions--org-symbol (components key fallback)
  "Return COMPONENTS KEY as a symbol, or FALLBACK."
  (if-let ((value (plist-get components key)))
      (intern value)
    fallback))

(defun term-sessions--open-org-path (path _arg)
  "Open Org term-session link PATH.
Open only active sessions automatically.  If the linked session is missing,
offer to recreate it with the stored command and cwd."
  (let* ((components (term-sessions--org-path-components path))
         (backend (plist-get components :backend))
         (name (plist-get components :name))
         (command (term-sessions--string-or-nil (plist-get components :command)))
         (frontend (term-sessions--org-symbol components :frontend term-sessions-preferred-frontend))
         (default-directory (term-sessions--org-default-directory components)))
    (unless (string= backend "zmx")
      (user-error "Unsupported term-session backend: %s" backend))
    (if (term-sessions--active-p name)
        (term-sessions-open-with-frontend name nil frontend nil)
      (when (yes-or-no-p (format "Session `%s' is not active; recreate it? " name))
        (term-sessions-open-with-frontend name command frontend t)))))

(with-eval-after-load 'org
  (org-link-set-parameters "term-session"
                           :follow #'term-sessions--open-org-path
                           :store #'term-sessions-store-org-link))

(provide 'term-sessions-org)
;;; term-sessions-org.el ends here
