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
(declare-function org-babel-pick-name "ob-core" (names params))
(declare-function org-babel-reassemble-table "ob-core" (table colnames rownames))
(declare-function comint-send-string "comint" (process string))
(declare-function ghostel--send-string "ghostel" (string))
(declare-function ghostel-semi-char-mode "ghostel" ())
(declare-function term-send-raw-string "term" (string))
(defvar ghostel--process nil)
(defvar ghostel--input-mode nil)

(defcustom term-sessions-org-babel-default-session-name "org-babel"
  "Default zmx session name for Org Babel blocks.
This is used when a shell block has `:term-session t' and no usable
`:session' header value.  To choose a specific zmx session per block, use
`:term-session NAME'."
  :group 'term-sessions
  :type 'string)

(defcustom term-sessions-org-babel-result-format "[[%s][%s]]"
  "Format string inserted as the result of a term-session Babel block.
It is called with the Org link and session name as arguments."
  :group 'term-sessions
  :type 'string)

(defcustom term-sessions-org-babel-send-after-create-delay 1.0
  "Seconds to wait before sending a Babel block to a newly created session.
The delay gives the visible terminal frontend and user shell a moment to
initialize before the block is sent."
  :group 'term-sessions
  :type 'number)

(defcustom term-sessions-org-babel-use-bracketed-paste nil
  "When non-nil, wrap Org Babel blocks in bracketed paste markers.
This can make some shells treat a block as one paste, but it is disabled by
default because sending bracketed-paste escape sequences through zmx can make
some terminals/shells appear frozen while they wait for escape sequence
resolution."
  :group 'term-sessions
  :type 'boolean)

(defcustom term-sessions-org-babel-use-zmx-send-when-no-buffer nil
  "When non-nil, fall back to `zmx send' if no Emacs terminal buffer exists.
The default is nil because a short-lived `zmx send' client can temporarily take
leadership away from the visible attach client, which makes subsequent manual
terminal input appear delayed.  With the default nil, an active session without
an Emacs buffer is opened first and the block is sent through that buffer."
  :group 'term-sessions
  :type 'boolean)

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

(defun term-sessions--org-link-for-spec (spec)
  "Return a bracketed Org link for term session SPEC."
  (let ((name (term-sessions-spec-name spec)))
    (format "[[%s][%s]]"
            (term-sessions--spec-org-link spec)
            (term-sessions--org-link-description name spec))))

(defun term-sessions--org-link-for-entry (entry &optional frontend)
  "Return a bracketed Org link for session ENTRY.
FRONTEND defaults to `term-sessions-preferred-frontend'."
  (let* ((default-directory (term-sessions--entry-directory entry))
         (name (term-sessions--entry-name entry))
         (spec (term-sessions-spec-current
                name nil (or frontend term-sessions-preferred-frontend))))
    (term-sessions--org-link-for-spec spec)))

;;;###autoload
(defun term-sessions-store-org-link (&optional name-or-interactive)
  "Store an Org link to a persistent session.
When called from a session buffer, default to that session.  When called
interactively as a command, prompt for a session if needed.  When Org calls this
as a link store function outside a term-sessions buffer, return nil so Org can
fall back to the normal link for the current context.  NAME-OR-INTERACTIVE is a
string session name for direct Lisp callers, or Org's INTERACTIVE? callback
argument otherwise."
  (interactive)
  ;; Org store functions are called for every `org-store-link' and
  ;; `org-capture' annotation.  They must decline contexts they do not own;
  ;; otherwise Org sees an extra candidate link and prompts for a selection in
  ;; unrelated buffers.  Treat only string values as explicit session names;
  ;; Org passes a boolean INTERACTIVE? argument here.
  (let ((name (or (and (stringp name-or-interactive) name-or-interactive)
                  term-sessions-current-name)))
    (when (or name (called-interactively-p 'interactive))
      (let* ((name (or name (term-sessions--read-name "Store link for session: " t)))
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
        link))))

(defun term-sessions--org-path-components (path)
  "Parse Org term-session link PATH.
Return a plist with at least :backend, :location, :name, :cwd, :command,
:frontend, :project, and :created-at."
  (unless (string-prefix-p "spec:" path)
    (user-error "Invalid term-session spec link: %s" path))
  (let ((plist (term-sessions--org-decode-query (substring path 5))))
    (plist-put plist :location "spec")
    (unless (plist-get plist :backend)
      (user-error "Invalid term-session spec link: %s" path))
    (unless (plist-get plist :name)
      (user-error "Invalid term-session spec link: %s" path))
    plist))

(defun term-sessions--org-default-directory (components)
  "Return `default-directory' for Org link COMPONENTS."
  (or (plist-get components :cwd)
      (plist-get components :directory)
      default-directory))

(defun term-sessions--org-symbol (components key fallback)
  "Return COMPONENTS KEY as a symbol, or FALLBACK."
  (if-let ((value (plist-get components key)))
      (intern value)
    fallback))

(defun term-sessions--org-babel-false-value-p (value)
  "Return non-nil when Babel header VALUE means false."
  (member (downcase (format "%s" value)) '("" "nil" "no" "false")))

(defun term-sessions--org-babel-default-value-p (value)
  "Return non-nil when Babel header VALUE asks for the default name."
  (member (downcase (format "%s" value)) '("t" "yes" "true")))

(defun term-sessions--org-babel-session-name (params)
  "Return zmx session name requested by Babel PARAMS, or nil.
The `:term-session' header enables this integration.  If its value is a
specific string, that string is used as the zmx session name.  If its value
is t/yes/true, use the Org `:session' name when present and not `none';
otherwise use `term-sessions-org-babel-default-session-name'."
  (when-let ((value (alist-get :term-session params)))
    (unless (term-sessions--org-babel-false-value-p value)
      (if (term-sessions--org-babel-default-value-p value)
          (let ((session (alist-get :session params)))
            (if (or (not session)
                    (term-sessions--org-babel-false-value-p session)
                    (string= (downcase (format "%s" session)) "none"))
                term-sessions-org-babel-default-session-name
              (format "%s" session)))
        (format "%s" value)))))

(defun term-sessions--org-babel-input (body)
  "Return BODY as terminal input for an interactive shell."
  (let ((text (string-trim-right body)))
    (if term-sessions-org-babel-use-bracketed-paste
        ;; The final carriage return executes the pasted block.
        (concat "\e[200~" text "\e[201~\r")
      (concat text "\r"))))

(defun term-sessions--org-babel-link-result (name)
  "Return an Org link result for zmx session NAME."
  (format term-sessions-org-babel-result-format
          (term-sessions--spec-org-link
           (term-sessions-spec-current name nil term-sessions-preferred-frontend))
          name))

(defun term-sessions--org-babel-login-shell ()
  "Return the user's login shell for creating Org Babel sessions."
  (or (getenv "SHELL")
      (when (not (file-remote-p default-directory))
        (ignore-errors
          (with-temp-buffer
            (when (eq 0 (process-file "getent" nil t nil "passwd" (user-login-name)))
              (let* ((line (string-trim (buffer-string)))
                     (fields (split-string line ":")))
                (when (>= (length fields) 7)
                  (nth 6 fields)))))))
      shell-file-name))

(defun term-sessions--org-babel-ensure-session (name)
  "Ensure zmx session NAME exists.
Return `active' when NAME was already active.  If it was missing, open it
through the normal visible frontend and return `created'."
  (if (term-sessions--active-p name)
      'active
    (let ((process-environment (copy-sequence process-environment)))
      (when-let ((shell (term-sessions--org-babel-login-shell)))
        (setenv "SHELL" shell))
      (term-sessions-open-with-frontend
       name nil term-sessions-preferred-frontend t))
    'created))

(defun term-sessions--org-babel-buffer-process (buffer)
  "Return BUFFER's live terminal process, or nil."
  (with-current-buffer buffer
    (let ((process (or (get-buffer-process (current-buffer))
                       (and (boundp 'ghostel--process) ghostel--process))))
      (when (and process (process-live-p process))
        process))))

(defun term-sessions--org-babel-live-buffer (name)
  "Return NAME's existing session buffer when it has a live process."
  (when-let ((buffer (term-sessions--session-buffer
                      name default-directory term-sessions-backend)))
    (when (term-sessions--org-babel-buffer-process buffer)
      buffer)))

(defun term-sessions--org-babel-process-send-string (process input)
  "Send INPUT to PROCESS using the current buffer's terminal frontend API."
  (cond
   ((and (derived-mode-p 'ghostel-mode)
         (fboundp 'ghostel--send-string))
    ;; Ghostel keeps redraw/input latency bookkeeping around its own send
    ;; function.  Bypassing it with `process-send-string' can leave subsequent
    ;; user input looking delayed until the normal redraw timer catches up.  If
    ;; the user left the buffer in copy/emacs mode, return to the normal input
    ;; mode before injecting text so live redraws are not frozen.
    (when (and (boundp 'ghostel--input-mode)
               (memq ghostel--input-mode '(copy emacs))
               (fboundp 'ghostel-semi-char-mode))
      (ghostel-semi-char-mode))
    (ghostel--send-string input))
   ((and (derived-mode-p 'term-mode)
         (fboundp 'term-send-raw-string))
    (term-send-raw-string input))
   ((derived-mode-p 'comint-mode)
    (comint-send-string process input))
   (t
    (process-send-string process input))))

(defun term-sessions--org-babel-send-to-buffer (buffer body)
  "Send BODY through existing terminal BUFFER.
Return non-nil when BUFFER had a live process and the text was sent."
  (when-let ((process (term-sessions--org-babel-buffer-process buffer)))
    (with-current-buffer buffer
      (term-sessions--org-babel-process-send-string
       process (term-sessions--org-babel-input body)))
    t))

(defun term-sessions--org-babel-send-via-zmx-or-error (name body)
  "Send BODY to zmx session NAME with `zmx send', or signal if disabled.
Return `zmx' when sent."
  (if term-sessions-org-babel-use-zmx-send-when-no-buffer
      (progn
        (term-sessions--zmx-with-stdin (term-sessions--org-babel-input body)
                                       "send" name)
        'zmx)
    (user-error "No live Emacs terminal buffer for term session `%s'" name)))

(defun term-sessions--org-babel-send-now (name body)
  "Send BODY to active zmx session NAME now.
Prefer an already-open Emacs terminal buffer.  This avoids creating a
short-lived `zmx send' client, which can temporarily steal zmx leadership from
the visible terminal.  Return `buffer' or `zmx' to describe the send path."
  (if-let ((buffer (term-sessions--org-babel-live-buffer name)))
      (if (term-sessions--org-babel-send-to-buffer buffer body)
          'buffer
        (term-sessions--org-babel-send-via-zmx-or-error name body))
    (term-sessions--org-babel-send-via-zmx-or-error name body)))

(defun term-sessions--org-babel-send-later (name body)
  "Send BODY to zmx session NAME after creation delay."
  (run-at-time
   term-sessions-org-babel-send-after-create-delay nil
   (lambda (session text directory)
     (let ((default-directory directory))
       (condition-case err
           (let ((method (term-sessions--org-babel-send-now session text)))
             (message "Sent Org Babel block to new term session `%s' via %s"
                      session method))
         (error
          (message "Failed to send Org Babel block to `%s': %s"
                   session (error-message-string err))))))
   name body default-directory))

(defun term-sessions--org-babel-open-active-session (name)
  "Open active zmx session NAME in an Emacs terminal buffer."
  (term-sessions-open-with-frontend
   name nil term-sessions-preferred-frontend nil))

(defun term-sessions--org-babel-send (name body)
  "Send Babel BODY to zmx session NAME and return an Org link."
  (pcase (term-sessions--org-babel-ensure-session name)
    ('active
     (if (or (term-sessions--org-babel-live-buffer name)
             term-sessions-org-babel-use-zmx-send-when-no-buffer)
         (let ((method (term-sessions--org-babel-send-now name body)))
           (message "Sent Org Babel block to term session `%s' via %s" name method))
       (term-sessions--org-babel-open-active-session name)
       (term-sessions--org-babel-send-later name body)
       (message "Opened term session `%s'; scheduled Org Babel block send" name)))
    ('created
     (term-sessions--org-babel-send-later name body)
     (message "Created term session `%s'; scheduled Org Babel block send" name)))
  (term-sessions--org-babel-link-result name))

(defun term-sessions--org-babel-reassemble-result (result params)
  "Return Org Babel RESULT reassembled according to PARAMS."
  (org-babel-reassemble-table
   result
   (org-babel-pick-name
    (cdr (assq :colname-names params)) (cdr (assq :colnames params)))
   (org-babel-pick-name
    (cdr (assq :rowname-names params)) (cdr (assq :rownames params)))))

;;;###autoload
(defun term-sessions-org-babel-shell (org-babel-execute-shell-fun body params)
  "Send ob-shell BODY to a zmx shell when `:term-session' is present.
If the session is missing, create it as an interactive user shell first.
Then send the block text through the Emacs terminal buffer and return a
clickable `term-session:' Org link."
  (if-let ((name (term-sessions--org-babel-session-name params)))
      (term-sessions--org-babel-reassemble-result
       (term-sessions--org-babel-send name body)
       params)
    (funcall org-babel-execute-shell-fun body params)))

;;;###autoload
(defun term-sessions-org-babel-sh (org-babel-sh-evaluate-fun &rest args)
  "Fallback around advice for `org-babel-sh-evaluate'.
If `:term-session' reaches this lower-level function, send BODY to the terminal
session and return a `term-session:' Org link."
  (pcase-let ((`(,_session ,body ,params ,_stdin ,_cmdline) args))
    (if-let ((name (term-sessions--org-babel-session-name params)))
        (term-sessions--org-babel-send name body)
      (apply org-babel-sh-evaluate-fun args))))

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
        (if-let ((buffer (term-sessions--session-buffer name default-directory 'zmx)))
            (pop-to-buffer buffer)
          (term-sessions-open-with-frontend name nil frontend nil))
      (when (yes-or-no-p (format "Session `%s' is not active; recreate it? " name))
        (term-sessions-open-with-frontend name command frontend t)))))

(with-eval-after-load 'org
  (org-link-set-parameters "term-session"
                           :follow #'term-sessions--open-org-path
                           :store #'term-sessions-store-org-link))

(with-eval-after-load 'ob-shell
  (advice-add 'org-babel-execute:shell :around #'term-sessions-org-babel-shell)
  (advice-add 'org-babel-sh-evaluate :around #'term-sessions-org-babel-sh))

(provide 'term-sessions-org)
;;; term-sessions-org.el ends here
