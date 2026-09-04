;;; term-sessions-tests.el --- Tests for term-sessions -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'term-sessions)

(defvar ghostel-buffer-name-function)

(ert-deftest term-sessions-test-attach-command-quotes-args ()
  (let ((term-sessions-zmx-program "zmx"))
    (should (equal (term-sessions--attach-command "dev" nil)
                   "zmx attach dev"))
    (should (equal (term-sessions--attach-command "dev box" "echo hello")
                   "zmx attach dev\\ box echo hello"))))

(ert-deftest term-sessions-test-zmx-list-names-parses-short-output ()
  (cl-letf (((symbol-function 'term-sessions--zmx)
             (lambda (&rest args)
               (should (equal args '("list" "--short")))
               " dev \n\nbuild\n")))
    (should (equal (term-sessions--zmx-list-names) '("dev" "build")))))

(ert-deftest term-sessions-test-zmx-list-names-fallback-parses-details ()
  (cl-letf (((symbol-function 'term-sessions--zmx)
             (lambda (&rest args)
               (if (equal args '("list" "--short"))
                   (error "unknown flag: --short")
                 "name=dev\tpid=1\tclients=0\nname=build\tpid=2\tclients=2\n"))))
    (should (equal (term-sessions--zmx-list-names) '("dev" "build")))))

(ert-deftest term-sessions-test-stdin-temp-file-prefix-uses-remote-temp-dir ()
  (let ((default-directory "/ssh:user@example:/read-only/project/")
        (temporary-file-directory "/tmp/"))
    (should (equal (term-sessions--stdin-temp-file-prefix)
                   "/ssh:user@example:/tmp/term-sessions-stdin-"))))

(ert-deftest term-sessions-test-ensure-zmx-probes-remote-host ()
  (clrhash term-sessions--remote-zmx-availability)
  (let ((default-directory "/ssh:user@example:/tmp/")
        probes)
    (cl-letf (((symbol-function 'process-file)
               (lambda (_program _infile _dest _display &rest args)
                 (push args probes)
                 0)))
      (term-sessions--ensure-zmx)
      (term-sessions--ensure-zmx)
      (should (equal (car probes) '("-c" "command -v zmx")))
      ;; The remote probe is cached per remote and program.
      (should (= (length probes) 1)))))

(ert-deftest term-sessions-test-ensure-zmx-errors-for-missing-remote-zmx ()
  (clrhash term-sessions--remote-zmx-availability)
  (let ((default-directory "/ssh:user@example:/tmp/"))
    (cl-letf (((symbol-function 'process-file) (lambda (&rest _args) 1)))
      (should-error (term-sessions--ensure-zmx) :type 'user-error))))

(ert-deftest term-sessions-test-zmx-list-sessions-parses-details ()
  (let ((term-sessions-zmx-enrich-process-info nil))
    (cl-letf (((symbol-function 'term-sessions--zmx)
               (lambda (&rest args)
                 (should (equal args '("list")))
                 "  name=dev\tpid=123\tclients=2\tcreated=1781290004\tstart_dir=/repo\tcmd=/bin/bash -l\n"))
              ;; Avoid the real log-dir probe, which calls
              ;; `term-sessions--zmx' with ("version") before the "list"
              ;; call; older ERT counts the resulting `should' failure even
              ;; though `term-sessions--zmx-log-dir' catches it.
              ((symbol-function 'term-sessions--zmx-log-dir)
               (lambda () nil))
              ((symbol-function 'term-sessions--zmx-log-mtime)
               (lambda (_name &optional _log-dir)
                 0)))
      (should (equal (term-sessions--zmx-list-sessions)
                     '((:name "dev" :pid "123" :clients "2" :created "1781290004"
                       :start_dir "/repo" :cmd "/bin/bash -l" :updated-time 0)))))))

(ert-deftest term-sessions-test-zmx-list-sessions-propagates-errors ()
  (cl-letf (((symbol-function 'term-sessions--zmx)
             (lambda (&rest _args) (error "TRAMP failed"))))
    (should-error (term-sessions--zmx-list-sessions) :type 'error)))

(ert-deftest term-sessions-test-zmx-list-sessions-resolves-log-dir-once ()
  (let ((log-dir-calls 0))
    (cl-letf (((symbol-function 'term-sessions--zmx)
               (lambda (&rest _args)
                 "name=a\tclients=0\nname=b\tclients=1\n"))
              ((symbol-function 'term-sessions--zmx-log-dir)
               (lambda ()
                 (cl-incf log-dir-calls)
                 "/tmp/zmx-logs"))
              ((symbol-function 'file-attributes)
               (lambda (&rest _args) nil))
              (term-sessions-zmx-enrich-process-info nil))
      (should (= (length (term-sessions--zmx-list-sessions)) 2))
      (should (= log-dir-calls 1)))))

(ert-deftest term-sessions-test-zmx-log-file-preserves-remote-tilde ()
  (let ((default-directory "/ssh:remote-user@example:/repo/"))
    (should (equal (term-sessions--zmx-log-file-name "dev" "~/.zmx")
                   "/ssh:remote-user@example:~/.zmx/dev.log"))))

(ert-deftest term-sessions-test-zmx-log-file-qualifies-remote-absolute-path ()
  (let ((default-directory "/ssh:remote-user@example:/repo/"))
    (should (equal (term-sessions--zmx-log-file-name "dev" "/var/log/zmx")
                   "/ssh:remote-user@example:/var/log/zmx/dev.log"))))

(ert-deftest term-sessions-test-zmx-list-sessions-adds-live-cwd-and-command ()
  (cl-letf (((symbol-function 'term-sessions--zmx)
             (lambda (&rest args)
               (should (equal args '("list")))
               "name=dev\tpid=123\tclients=0\tstart_dir=/repo\tcmd=/bin/bash -l\n"))
            ;; See term-sessions-test-zmx-list-sessions-parses-details.
            ((symbol-function 'term-sessions--zmx-log-dir)
             (lambda () nil))
            ((symbol-function 'term-sessions--zmx-log-mtime)
             (lambda (_name &optional _log-dir) nil))
            ((symbol-function 'term-sessions--zmx-process-cwd)
             (lambda (pid)
               (should (equal pid "123"))
               "/repo/subdir"))
            ((symbol-function 'term-sessions--zmx-current-command)
             (lambda (pid)
               (should (equal pid "123"))
               "nvim src/main.c")))
    (should (equal (term-sessions--zmx-list-sessions)
                   '((:name "dev" :pid "123" :clients "0" :start_dir "/repo"
                     :cmd "/bin/bash -l" :updated-time nil :cwd "/repo/subdir"
                     :current-cmd "nvim src/main.c"))))))

(ert-deftest term-sessions-test-active-p-uses-list ()
  (cl-letf (((symbol-function 'term-sessions--zmx-list-names)
             (lambda () '("dev" "build"))))
    (should (term-sessions--active-p "dev"))
    (should-not (term-sessions--active-p "missing"))))

(ert-deftest term-sessions-test-zmx-session-entry-normalizes-fields ()
  (should (equal (term-sessions--zmx-session-entry
                  '(:name "dev" :start_dir "/repo" :cwd "/repo/sub"
                    :cmd "bash" :current-cmd "nvim" :updated-time 10)
                  "/tmp/")
                 '(:name "dev" :directory "/tmp/" :session
                   (:name "dev" :start_dir "/repo" :cwd "/repo/sub"
                    :cmd "bash" :current-cmd "nvim" :updated-time 10)
                   :cwd "/repo/sub" :command "nvim" :clients ""
                   :updated-time 10))))

(ert-deftest term-sessions-test-org-link-roundtrip-special-name ()
  (let* ((term-sessions-backend 'zmx)
         (term-sessions-preferred-frontend 'term)
         (term-sessions-current-time-function (lambda () 0))
         (spec (term-sessions-spec-current "dev:box" nil term-sessions-preferred-frontend))
         (link (term-sessions--spec-org-link spec))
         (components (term-sessions--org-path-components
                      (substring link (length "term-session:")))))
    (should (string-prefix-p "term-session:spec:" link))
    (should (equal (plist-get components :backend) "zmx"))
    (should (equal (plist-get components :name) "dev:box"))
    (should (equal (plist-get components :frontend) "term"))
    (should (plist-get components :cwd))))

(ert-deftest term-sessions-test-remote-org-link-roundtrip ()
  (let* ((default-directory "/ssh:user@example:/tmp")
         (term-sessions-backend 'zmx)
         (term-sessions-current-time-function (lambda () 0))
         (spec (term-sessions-spec-current "dev:box" nil term-sessions-preferred-frontend))
         (link (term-sessions--spec-org-link spec))
         (components (term-sessions--org-path-components
                      (substring link (length "term-session:")))))
    (should (string-prefix-p "term-session:spec:" link))
    (should (equal (plist-get components :backend) "zmx"))
    (should (equal (plist-get components :name) "dev:box"))
    (should (equal (plist-get components :cwd) "/ssh:user@example:/tmp"))
    (should (equal (plist-get components :method) "ssh"))
    (should (equal (plist-get components :user) "user"))
    (should (equal (plist-get components :host) "example"))
    (should (equal (plist-get components :localname) "/tmp"))))

(ert-deftest term-sessions-test-org-link-for-entry-builds-link-on-entry-directory ()
  (let ((term-sessions-current-time-function (lambda () 0)))
    (let ((link (term-sessions--org-link-for-entry
                 (list :name "dev" :directory "/ssh:user@example:/tmp/project/"))))
      (should (string-match-p "\\`\\[\\[term-session:spec:" link))
      (should (string-match-p "name=dev" link))
      (should (string-match-p "%2Fssh%3Auser%40example%3A%2Ftmp%2Fproject%2F" link)))))

(ert-deftest term-sessions-test-store-org-link-description-starts-with-session-name ()
  (let ((default-directory "/ssh:user@example:/tmp/project")
        (term-sessions-current-time-function (lambda () 0))
        stored)
    (cl-letf (((symbol-function 'org-link-store-props)
               (lambda (&rest plist) (setq stored plist))))
      (term-sessions-store-org-link "dev")
      (should (string-prefix-p "dev" (plist-get stored :description)))
      (should (string-match-p "ssh:user@example" (plist-get stored :description)))
      (should (string-match-p "/tmp/project" (plist-get stored :description))))))

(ert-deftest term-sessions-test-store-org-link-explicit-name-beats-buffer-spec ()
  ;; An explicit session name must not be stored with another session's
  ;; buffer spec.
  (let ((default-directory "/tmp/project")
        (term-sessions-current-time-function (lambda () 0))
        (term-sessions-current-name "dev")
        (term-sessions-current-spec
         (term-sessions-spec-create :name "dev" :backend 'zmx
                                    :cwd "/tmp/project/"))
        stored)
    (cl-letf (((symbol-function 'org-link-store-props)
               (lambda (&rest plist) (setq stored plist))))
      (term-sessions-store-org-link "other")
      (should (string-match-p "name=other" (plist-get stored :link)))
      (should (string-prefix-p "other" (plist-get stored :description))))))

(ert-deftest term-sessions-test-store-org-link-ignores-numeric-org-arg ()
  (let ((default-directory "/home/arthur/")
        (term-sessions-current-name "hello")
        (term-sessions-current-time-function (lambda () 0))
        stored)
    (cl-letf (((symbol-function 'org-link-store-props)
               (lambda (&rest plist) (setq stored plist))))
      (term-sessions-store-org-link 1)
      (should (string-prefix-p "hello" (plist-get stored :description)))
      (should (string-match-p "name=hello" (plist-get stored :link))))))

(ert-deftest term-sessions-test-store-org-link-declines-unrelated-org-context ()
  (let (stored)
    (cl-letf (((symbol-function 'term-sessions--read-name)
               (lambda (&rest _args) (error "Should not prompt")))
              ((symbol-function 'org-link-store-props)
               (lambda (&rest plist) (setq stored plist))))
      (should-not (term-sessions-store-org-link nil))
      (should-not (term-sessions-store-org-link t))
      (should-not stored))))

(ert-deftest term-sessions-test-session-spec-captures-current-location ()
  (let ((default-directory "/ssh:t480-arthur:/tmp/project")
        (term-sessions-current-time-function (lambda () 0)))
    (let* ((spec (term-sessions-spec-current "dev" "echo hi" 'term '("pi") 'ask))
           (location (term-sessions-spec-location spec)))
      (should (equal (term-sessions-spec-name spec) "dev"))
      (should (eq (term-sessions-spec-backend spec) 'zmx))
      (should (equal (term-sessions-spec-cwd spec) "/ssh:t480-arthur:/tmp/project"))
      (should (equal (term-sessions-spec-command spec) "echo hi"))
      (should (eq (term-sessions-spec-frontend spec) 'term))
      (should (equal (term-sessions-spec-tags spec) '("pi")))
      (should (eq (term-sessions-spec-recreate-policy spec) 'ask))
      (should (term-sessions-location-remote-p location))
      (should (equal (term-sessions-location-method location) "ssh"))
      (should (equal (term-sessions-location-host location) "t480-arthur")))))

(ert-deftest term-sessions-test-location-parses-ssh-port ()
  (let* ((location (term-sessions--location "/ssh:user@example#2222:/tmp/project"))
         (info (term-sessions--remote-info "/ssh:user@example#2222:/tmp/project")))
    (should (term-sessions-location-remote-p location))
    (should (equal (term-sessions-location-method location) "ssh"))
    (should (equal (term-sessions-location-user location) "user"))
    (should (equal (term-sessions-location-host location) "example"))
    (should (equal (term-sessions-location-port location) "2222"))
    (should (equal (term-sessions-location-localname location) "/tmp/project"))
    (should (equal (plist-get info :target) "user@example"))
    (should (equal (plist-get info :port) "2222"))))

(ert-deftest term-sessions-test-location-parses-unknown-rpc-method ()
  (let ((location (term-sessions--location "/rpc:user@example:/tmp/project")))
    (should (term-sessions-location-remote-p location))
    (should (equal (term-sessions-location-method location) "rpc"))
    (should (equal (term-sessions-location-user location) "user"))
    (should (equal (term-sessions-location-host location) "example"))
    (should (equal (term-sessions-location-localname location) "/tmp/project"))))

(ert-deftest term-sessions-test-location-preserves-multi-hop ()
  (let ((location (term-sessions--location
                   "/ssh:jump|ssh:user@example#2222:/tmp/project")))
    (should (term-sessions-location-remote-p location))
    (should (equal (term-sessions-location-method location) "ssh"))
    (should (equal (term-sessions-location-user location) "user"))
    (should (equal (term-sessions-location-host location) "example"))
    (should (equal (term-sessions-location-port location) "2222"))
    (should (equal (term-sessions-location-hop location) "ssh:jump|"))
    (should (equal (term-sessions-location-localname location) "/tmp/project"))
    (should (equal (term-sessions--location-remote-label location)
                   "ssh:jump|ssh:user@example#2222"))))

(ert-deftest term-sessions-test-org-path-opens-active-session ()
  (let ((opened nil)
        (started nil))
    (cl-letf (((symbol-function 'term-sessions--active-p) (lambda (name) (equal name "dev")))
              ((symbol-function 'term-sessions-open-with-frontend)
               (lambda (name command frontend allow-create)
                 (setq opened (list name command frontend allow-create)))))
      (term-sessions--open-org-path "spec:backend=zmx&name=dev&cwd=%2Ftmp%2F&frontend=term" nil)
      (should (equal opened '("dev" nil term nil)))
      (should-not started))))

(ert-deftest term-sessions-test-org-path-reuses-existing-buffer ()
  (let ((buffer (get-buffer-create " *term-sessions-test-org-link*"))
        popped
        opened)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq default-directory "/tmp/")
            (setq-local term-sessions-current-name "dev")
            (setq-local term-sessions-current-backend 'zmx)
            (setq-local term-sessions-current-terminal-p t))
          (cl-letf (((symbol-function 'term-sessions--active-p)
                     (lambda (name) (equal name "dev")))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (buf &rest _args) (setq popped buf)))
                    ((symbol-function 'term-sessions-open-with-frontend)
                     (lambda (&rest args) (setq opened args))))
            (term-sessions--open-org-path
             "spec:backend=zmx&name=dev&cwd=%2Ftmp%2F&frontend=term" nil)
            (should (eq popped buffer))
            (should-not opened)))
      (kill-buffer buffer))))

(ert-deftest term-sessions-test-org-path-opens-ssh-with-remote-directory ()
  (let (active-directory opened-directory opened)
    (cl-letf (((symbol-function 'term-sessions--active-p)
               (lambda (name)
                 (setq active-directory default-directory)
                 (equal name "dev")))
              ((symbol-function 'term-sessions-open-with-frontend)
               (lambda (name command frontend allow-create)
                 (setq opened (list name command frontend allow-create))
                 (setq opened-directory default-directory))))
      (term-sessions--open-org-path "spec:backend=zmx&name=dev&cwd=%2Fssh%3Auser%40example%3A~%2F&frontend=term" nil)
      (should (equal opened '("dev" nil term nil)))
      (should (equal active-directory "/ssh:user@example:~/"))
      (should (equal opened-directory "/ssh:user@example:~/")))))

(ert-deftest term-sessions-test-org-path-offers-start-for-missing-session ()
  (let ((opened nil)
        (started nil))
    (cl-letf (((symbol-function 'term-sessions--active-p) (lambda (_name) nil))
              ((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
              ((symbol-function 'term-sessions-open-with-frontend)
               (lambda (name command frontend allow-create)
                 (setq started (list name command frontend allow-create)))))
      (term-sessions--open-org-path "spec:backend=zmx&name=missing&cwd=%2Ftmp%2F&frontend=term" nil)
      (should-not opened)
      (should (equal started '("missing" nil term t))))))

(ert-deftest term-sessions-test-org-query-ignores-unknown-keys-without-interning ()
  (let ((unknown "term-sessions-test-never-intern-this-key"))
    (should-not (intern-soft (concat ":" unknown)))
    (should (equal (term-sessions--org-decode-query
                    (format "name=dev&%s=value" unknown))
                   '(:name "dev")))
    (should-not (intern-soft (concat ":" unknown)))))

(ert-deftest term-sessions-test-org-query-keeps-values-containing-equals ()
  (should (equal (term-sessions--org-decode-query "command=make VAR=1")
                 '(:command "make VAR=1"))))

(ert-deftest term-sessions-test-org-rejects-unknown-frontend-without-interning ()
  (let ((unknown "term-sessions-test-never-intern-this-frontend"))
    (should-not (intern-soft unknown))
    (should-error
     (term-sessions--org-frontend (list :frontend unknown) 'term)
     :type 'user-error)
    (should-not (intern-soft unknown))))

(ert-deftest term-sessions-test-org-babel-session-name-from-header ()
  (let ((term-sessions-org-babel-default-session-name "org-default"))
    (should (equal (term-sessions--org-babel-session-name
                    '((:term-session . "dev")))
                   "dev"))
    (should (equal (term-sessions--org-babel-session-name
                    '((:term-session . "t") (:session . "build")))
                   "build"))
    (should (equal (term-sessions--org-babel-session-name
                    '((:term-session . "t") (:session . "none")))
                   "org-default"))
    (should-not (term-sessions--org-babel-session-name
                 '((:term-session . "no") (:session . "build"))))))

(ert-deftest term-sessions-test-org-babel-sh-sends-to-existing-session ()
  (let ((term-sessions-org-babel-use-zmx-send-when-no-buffer t)
        (term-sessions-current-time-function (lambda () 0))
        sent args)
    (cl-letf (((symbol-function 'term-sessions--active-p)
               (lambda (name) (equal name "dev")))
              ((symbol-function 'term-sessions--zmx-with-stdin)
               (lambda (stdin &rest zmx-args)
                 (setq sent stdin
                       args zmx-args)
                 "")))
      (should (equal (term-sessions-org-babel-sh
                      (lambda (&rest _args) "original")
                      nil " echo hi\n" '((:term-session . "dev")) nil nil)
                     (term-sessions--org-babel-link-result "dev")))
      (should (equal sent " echo hi\r"))
      (should (equal args '("send" "dev"))))))

(ert-deftest term-sessions-test-org-babel-shell-skips-normal-executor ()
  (let ((term-sessions-org-babel-use-zmx-send-when-no-buffer t)
        sent args original-called)
    (cl-letf (((symbol-function 'term-sessions--active-p)
               (lambda (name) (equal name "dev")))
              ((symbol-function 'term-sessions--zmx-with-stdin)
               (lambda (stdin &rest zmx-args)
                 (setq sent stdin
                       args zmx-args)
                 ""))
              ((symbol-function 'org-babel-reassemble-table)
               (lambda (table _colnames _rownames) table))
              ((symbol-function 'org-babel-pick-name)
               (lambda (_names _params) nil)))
      (should (equal (term-sessions-org-babel-shell
                      (lambda (&rest _args)
                        (setq original-called t)
                        "original")
                      "echo hi"
                      '((:term-session . "t") (:session . "dev")))
                     (term-sessions--org-babel-link-result "dev")))
      (should-not original-called)
      (should (equal sent "echo hi\r"))
      (should (equal args '("send" "dev"))))))

(ert-deftest term-sessions-test-org-babel-inserts-clickable-raw-link-result ()
  (require 'org)
  (require 'ob-shell)
  (let ((org-confirm-babel-evaluate nil)
        (term-sessions-org-babel-use-zmx-send-when-no-buffer t)
        (term-sessions-current-time-function (lambda () 0))
        (default-directory "/tmp/")
        sent)
    (with-temp-buffer
      (org-mode)
      (insert "#+begin_src sh :term-session dev\necho hi\n#+end_src\n")
      (goto-char (point-min))
      (cl-letf (((symbol-function 'term-sessions--active-p)
                 (lambda (name) (equal name "dev")))
                ((symbol-function 'term-sessions--zmx-with-stdin)
                 (lambda (stdin &rest _zmx-args)
                   (setq sent stdin)
                   "")))
        (org-babel-execute-src-block))
      (let ((contents (buffer-string)))
        (should (equal sent "echo hi\r"))
        (should (string-match-p "#\\+RESULTS:\n\\[\\[term-session:spec:" contents))
        (should-not (string-match-p "#\\+RESULTS:\n: \\[\\[term-session:spec:"
                                    contents))))))

(ert-deftest term-sessions-test-org-babel-raw-advice-skips-non-shell-blocks ()
  ;; A :term-session header on a non-shell block must not force raw
  ;; results; only shell blocks are delivered to a terminal.
  (require 'org)
  (let (received)
    (cl-letf (((symbol-function 'term-sessions--org-babel-session-name)
               (lambda (_params) "dev")))
      (with-temp-buffer
        (org-mode)
        (insert "#+begin_src python :term-session dev\nprint(1)\n#+end_src\n")
        (goto-char (point-min))
        (let ((info (org-babel-get-src-block-info)))
          (term-sessions-org-babel-execute-src-block
           (lambda (&rest args) (setq received args))
           nil info '((:term-session . "dev"))))
        (should received)
        (should (equal (nth 2 received) '((:term-session . "dev"))))))))

(ert-deftest term-sessions-test-org-babel-raw-advice-does-not-add-trailing-nil ()
  (require 'org)
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_src sh :term-session dev\necho hi\n#+end_src\n")
    (goto-char (point-min))
    (let (called-args)
      (term-sessions-org-babel-execute-src-block
       (lambda (&optional arg info params)
         (setq called-args (list arg info params))
         'ok))
      (should (equal (length called-args) 3))
      (should (member "raw" (split-string (cdr (assq :results (nth 2 called-args)))))))))

(ert-deftest term-sessions-test-org-babel-prefers-existing-buffer-process ()
  (let* ((buffer (get-buffer-create " *term-sessions-test-org-babel*"))
         (process (start-process "term-sessions-test-org-babel" buffer "cat")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq default-directory "/tmp/")
            (setq-local term-sessions-current-name "dev")
            (setq-local term-sessions-current-backend 'zmx)
            (setq-local term-sessions-current-terminal-p t))
          (cl-letf (((symbol-function 'term-sessions--zmx-with-stdin)
                     (lambda (&rest _args) (error "Should not call zmx send"))))
            (should (eq (term-sessions--org-babel-send-now "dev" "echo hi")
                        'buffer))))
      (when (process-live-p process)
        (delete-process process))
      (kill-buffer buffer))))

(ert-deftest term-sessions-test-org-babel-opens-active-session-without-buffer ()
  (let ((term-sessions-preferred-frontend 'term)
        (term-sessions-org-babel-use-zmx-send-when-no-buffer nil)
        opened scheduled sent)
    (cl-letf (((symbol-function 'term-sessions--active-p)
               (lambda (name) (equal name "dev")))
              ((symbol-function 'term-sessions-open-with-frontend)
               (lambda (&rest args) (setq opened args)))
              ((symbol-function 'term-sessions--org-babel-send-later)
               (lambda (&rest args) (setq scheduled args)))
              ((symbol-function 'term-sessions--zmx-with-stdin)
               (lambda (&rest args) (setq sent args))))
      (should (equal (term-sessions--org-babel-send "dev" "echo hi")
                     (term-sessions--org-babel-link-result "dev")))
      (should (equal opened '("dev" nil term nil)))
      (should (equal scheduled '("dev" "echo hi")))
      (should-not sent))))

(ert-deftest term-sessions-test-org-babel-opens-active-remote-session-with-remote-directory ()
  (let ((default-directory "/ssh:user@example:/tmp/project/")
        (term-sessions-preferred-frontend 'term)
        (term-sessions-org-babel-use-zmx-send-when-no-buffer nil)
        opened-directory
        scheduled-directory
        opened
        scheduled
        sent)
    (cl-letf (((symbol-function 'term-sessions--active-p)
               (lambda (name) (equal name "dev")))
              ((symbol-function 'term-sessions-open-with-frontend)
               (lambda (&rest args)
                 (setq opened args)
                 (setq opened-directory default-directory)))
              ((symbol-function 'term-sessions--org-babel-send-later)
               (lambda (&rest args)
                 (setq scheduled args)
                 (setq scheduled-directory default-directory)))
              ((symbol-function 'term-sessions--zmx-with-stdin)
               (lambda (&rest args) (setq sent args))))
      (term-sessions--org-babel-send "dev" "echo hi")
      (should (equal opened '("dev" nil term nil)))
      (should (equal opened-directory "/ssh:user@example:/tmp/project/"))
      (should (equal scheduled '("dev" "echo hi")))
      (should (equal scheduled-directory "/ssh:user@example:/tmp/project/"))
      (should-not sent))))

(ert-deftest term-sessions-test-org-babel-reuses-remote-session-buffer-by-prefix ()
  (let* ((buffer (get-buffer-create " *term-sessions-test-org-babel-remote*"))
         (process (start-process "term-sessions-test-org-babel-remote" buffer "cat"))
         sent)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq default-directory "/ssh:user@example:/tmp/project/")
            (setq-local term-sessions-current-name "dev")
            (setq-local term-sessions-current-backend 'zmx)
            (setq-local term-sessions-current-terminal-p t))
          (cl-letf (((symbol-function 'process-send-string)
                     (lambda (proc string)
                       (setq sent (list proc string)))))
            (let ((default-directory "/ssh:user@example:/tmp/other-project/"))
              (should (eq (term-sessions--org-babel-send-now "dev" "echo remote")
                          'buffer)))
            (should (eq (car sent) process))
            (should (equal (cadr sent) "echo remote\r"))))
      (when (process-live-p process)
        (delete-process process))
      (kill-buffer buffer))))

(ert-deftest term-sessions-test-org-babel-sh-falls-through-without-header ()
  (let (called)
    (should (equal (term-sessions-org-babel-sh
                    (lambda (&rest args)
                      (setq called args)
                      "original")
                    'session "echo hi" '((:session . "dev")) nil nil)
                   "original"))
    (should (equal called '(session "echo hi" ((:session . "dev")) nil nil)))))

(ert-deftest term-sessions-test-open-requires-existing-session ()
  (cl-letf (((symbol-function 'term-sessions--ensure-zmx) #'ignore)
            ((symbol-function 'term-sessions--active-p) (lambda (_name) nil)))
    (should-error (term-sessions-open-with-frontend "missing" nil 'term nil)
                  :type 'user-error)))

(ert-deftest term-sessions-test-open-entry-uses-entry-directory ()
  (let ((entry (list :name "dev" :directory "/ssh:host:/repo/"))
        opened-directory opened-name)
    (cl-letf (((symbol-function 'term-sessions-open-with-frontend)
               (lambda (name _command _frontend _allow-create)
                 (setq opened-name (term-sessions--entry-name name)
                       opened-directory (term-sessions--entry-directory name)))))
      (term-sessions-open entry)
      (should (equal opened-name "dev"))
      (should (equal opened-directory "/ssh:host:/repo/")))))

(ert-deftest term-sessions-test-entry-cwd-directory-qualifies-remote-cwd ()
  (should (equal (term-sessions--entry-cwd-directory
                  (list :name "dev" :directory "/ssh:user@example:/" :cwd "/repo/sub"))
                 "/ssh:user@example:/repo/sub/"))
  (should (equal (term-sessions--entry-cwd-directory
                  (list :name "dev" :directory "/tmp/backend/" :cwd "/repo/sub"))
                 "/repo/sub/")))

(ert-deftest term-sessions-test-open-entry-prefers-entry-cwd-for-buffer-directory ()
  (let ((entry (list :name "dev" :directory "/ssh:user@example:/" :cwd "/repo/sub"))
        seen-directory opened)
    (cl-letf (((symbol-function 'term-sessions--ensure-zmx) #'ignore)
              ((symbol-function 'term-sessions--ensure-interactive-attach-supported)
               (lambda (&rest _args) 'tramp-process))
              ((symbol-function 'term-sessions--active-p)
               (lambda (_name)
                 (setq seen-directory default-directory)
                 t))
              ((symbol-function 'term-sessions--open-tramp-process)
               (lambda (&rest args) (setq opened args))))
      (term-sessions-open-with-frontend entry nil 'ghostel nil)
      (should (equal seen-directory "/ssh:user@example:/repo/sub/"))
      (should (equal (term-sessions-spec-cwd (nth 4 opened))
                     "/ssh:user@example:/repo/sub/")))))

(ert-deftest term-sessions-test-open-entry-prefers-local-cwd-for-buffer-directory ()
  (let ((entry (list :name "dev" :directory "/tmp/backend/" :cwd "/tmp/project"))
        opened-directory)
    (cl-letf (((symbol-function 'term-sessions--ensure-zmx) #'ignore)
              ((symbol-function 'term-sessions--ensure-interactive-attach-supported)
               (lambda (&rest _args) 'local))
              ((symbol-function 'term-sessions--active-p) (lambda (_name) t))
              ((symbol-function 'term-sessions--interactive-attach-command)
               (lambda (&rest _args) "zmx attach dev"))
              ((symbol-function 'term-sessions--open-command-frontend)
               (lambda (&rest _args) (setq opened-directory default-directory))))
      (term-sessions-open-with-frontend entry nil 'term nil)
      (should (equal opened-directory "/tmp/project/")))))

(ert-deftest term-sessions-test-open-returns-frontend-buffer ()
  (let ((buffer (generate-new-buffer " *term-sessions-test-open-return*"))
        opened)
    (unwind-protect
        (cl-letf (((symbol-function 'term-sessions-open-with-frontend)
                   (lambda (name command frontend allow-create)
                     (setq opened (list name command frontend allow-create))
                     buffer)))
          (should (eq (term-sessions-open "new" "echo hi") buffer))
          (should (equal opened (list "new" "echo hi" term-sessions-preferred-frontend t))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest term-sessions-test-open-vterm-returns-selected-buffer ()
  (let ((buffer (generate-new-buffer " *term-sessions-test-vterm-return*")))
    (unwind-protect
        (cl-progv '(vterm-tramp-shells vterm-shell vterm-buffer-name)
            '(nil nil nil)
          (cl-letf (((symbol-function 'require)
                     (lambda (feature &optional _filename _noerror)
                       (eq feature 'vterm)))
                    ((symbol-function 'vterm)
                     (lambda (&rest _args)
                       (set-buffer buffer)
                       "incidental return value")))
            (should (eq (term-sessions--open-vterm
                         "dev" "zmx attach dev" "*term-session:dev*")
                        buffer))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest term-sessions-test-open-with-frontend-reuses-existing-buffer ()
  (let ((buffer (generate-new-buffer " *term-sessions-test-open*"))
        ensured opened)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq default-directory "/tmp/project/"
                  term-sessions-current-name "dev"
                  term-sessions-current-backend 'zmx
                  term-sessions-current-terminal-p t))
          (cl-letf (((symbol-function 'term-sessions--ensure-zmx)
                     (lambda () (setq ensured t)))
                    ((symbol-function 'term-sessions--open-command-frontend)
                     (lambda (&rest args) (setq opened args))))
            (should (eq (term-sessions-open-with-frontend "dev" nil 'term t)
                        buffer))
            (should (eq (current-buffer) buffer))
            (should-not ensured)
            (should-not opened)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest term-sessions-test-open-with-frontend-returns-fresh-ghostel-buffer ()
  (let* ((buffer (generate-new-buffer " *term-sessions-test-fresh-ghostel*"))
         (default-directory "/tmp/")
         (term-sessions-ghostel-open-function (lambda (&rest _args) buffer)))
    (unwind-protect
        (cl-letf (((symbol-function 'term-sessions--ensure-zmx) #'ignore)
                  ((symbol-function 'term-sessions--ensure-interactive-attach-supported)
                   (lambda (&rest _args) 'local))
                  ((symbol-function 'term-sessions--active-p) (lambda (_name) t))
                  ((symbol-function 'term-sessions--interactive-attach-command)
                   (lambda (&rest _args) "zmx attach dev")))
          (should (eq (term-sessions-open-with-frontend "dev" nil 'ghostel nil)
                      buffer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest term-sessions-test-open-with-frontend-entry-binds-entry-directory ()
  (let ((entry (list :name "dev" :directory "/ssh:host:/repo/"))
        seen-directory opened)
    (cl-letf (((symbol-function 'term-sessions--ensure-zmx) #'ignore)
              ((symbol-function 'term-sessions--ensure-interactive-attach-supported)
               (lambda (&rest _args) 'tramp-process))
              ((symbol-function 'term-sessions--active-p)
               (lambda (name)
                 (setq seen-directory default-directory)
                 (equal name "dev")))
              ((symbol-function 'term-sessions--open-tramp-process)
               (lambda (&rest args) (setq opened args))))
      (term-sessions-open-with-frontend entry nil 'term nil)
      (should (equal seen-directory "/ssh:host:/repo/"))
      (should (equal (car opened) "dev")))))

(ert-deftest term-sessions-test-read-session-entry-selects-local-session ()
  (clrhash term-sessions--completion-entry-table)
  (let ((source-entry (list :name "dev"
                            :directory "/tmp/"
                            :where "local"
                            :cwd "/tmp/project"
                            :session '(:pid "123")
                            :clients "2"))
        prompt collection required)
    (cl-letf (((symbol-function 'term-sessions-list--session-rows)
               (lambda ()
                 (list (list source-entry []))))
              ((symbol-function 'completing-read)
               (lambda (p c _predicate require-match &rest _args)
                 (setq prompt p collection c required require-match)
                 "dev @ local /tmp/project")))
      (let ((entry (term-sessions-read-session-entry "Send to session: ")))
        (should (equal prompt "Send to session: "))
        (should-not required)
        (should (equal (all-completions "dev" collection)
                       '("dev @ local /tmp/project")))
        (should (equal entry
                       '(:name "dev" :directory "/tmp/" :where "local"
                         :cwd "/tmp/project" :session (:pid "123") :clients "2"
                         :existing t)))
        (should-not (eq entry source-entry))))))

(ert-deftest term-sessions-test-read-session-entry-selects-open-remote ()
  (clrhash term-sessions--completion-entry-table)
  (let ((default-directory "/tmp/current/")
        prompt collection required)
    (cl-letf (((symbol-function 'term-sessions-list--session-rows)
               (lambda ()
                 (list (list (list :name "local"
                                   :directory "/home/me/"
                                   :where "local"
                                   :cwd "/home/me")
                             [])
                       (list (list :name "remote"
                                   :directory "/ssh:host:/"
                                   :where "ssh:host"
                                   :cwd "/repo")
                             []))))
              ((symbol-function 'completing-read)
               (lambda (p c _predicate require-match &rest _args)
                 (setq prompt p collection c required require-match)
                 "remote @ ssh:host /repo")))
      (let ((entry (term-sessions-read-session-entry "Open session: ")))
        (should (equal prompt "Open session: "))
        (should-not required)
        (should (equal (all-completions "remote" collection)
                       '("remote @ ssh:host /repo")))
        (should (equal (plist-get entry :name) "remote"))
        (should (equal (plist-get entry :directory) "/ssh:host:/"))
        (should (plist-get entry :existing))))))

(ert-deftest term-sessions-test-read-existing-session-entry-requires-match ()
  (clrhash term-sessions--completion-entry-table)
  (let ((selected '("dev @ local /tmp/project" "missing"))
        required)
    (cl-letf (((symbol-function 'term-sessions-list--session-rows)
               (lambda ()
                 (list (list (list :name "dev"
                                   :directory "/tmp/"
                                   :where "local"
                                   :cwd "/tmp/project")
                             []))))
              ((symbol-function 'completing-read)
               (lambda (_prompt _collection _predicate require-match &rest _args)
                 (setq required require-match)
                 (pop selected))))
      (should (plist-get (term-sessions-read-existing-session-entry) :existing))
      (should-error (term-sessions-read-existing-session-entry) :type 'user-error)
      (should required))))

(ert-deftest term-sessions-test-read-session-entry-rejects-empty-name ()
  (cl-letf (((symbol-function 'term-sessions-list--session-rows)
             (lambda () nil))
            ((symbol-function 'completing-read)
             (lambda (&rest _args) "")))
    (should-error (term-sessions-read-session-entry) :type 'user-error)))

(ert-deftest term-sessions-test-read-session-entry-preserves-cancellation ()
  (let (result)
    (cl-letf (((symbol-function 'term-sessions-list--session-rows)
               (lambda () nil))
              ((symbol-function 'completing-read)
               (lambda (&rest _args)
                 (signal 'quit nil))))
      (setq result
            (condition-case nil
                (term-sessions-read-session-entry)
              (quit 'cancelled))))
    (should (eq result 'cancelled))))

(ert-deftest term-sessions-test-read-session-entry-allows-new-name ()
  (clrhash term-sessions--completion-entry-table)
  (let ((default-directory "/tmp/project/")
        required)
    (cl-letf (((symbol-function 'term-sessions-list--session-rows)
               (lambda ()
                 (list (list (list :name "dev"
                                   :directory "/home/me/"
                                   :where "local"
                                   :cwd "/home/me")
                             []))))
              ((symbol-function 'completing-read)
               (lambda (_prompt _collection _predicate require-match &rest _args)
                 (setq required require-match
                       default-directory "/tmp/changed/")
                 "new-session")))
      (let ((entry (term-sessions-read-session-entry "Open session: ")))
        (should-not required)
        (should (equal (plist-get entry :name) "new-session"))
        (should (equal (plist-get entry :directory) "/tmp/project/"))
        (should-not (plist-get entry :existing))))))

(ert-deftest term-sessions-test-open-interactively-uses-public-session-reader ()
  (let ((entry (list :name "dev" :directory "/tmp/" :existing t))
        (buffer (generate-new-buffer " *term-sessions-test-interactive-open*"))
        prompt opened)
    (unwind-protect
        (cl-letf (((symbol-function 'term-sessions-read-session-entry)
                   (lambda (value)
                     (setq prompt value)
                     entry))
                  ((symbol-function 'term-sessions-open-with-frontend)
                   (lambda (&rest args)
                     (setq opened args)
                     buffer)))
          (should (eq (call-interactively #'term-sessions-open) buffer))
          (should (equal prompt "Open session: "))
          (should (equal opened (list entry nil term-sessions-preferred-frontend t))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest term-sessions-test-remote-open-dispatches-tramp-for-command-frontend ()
  (let ((default-directory "/ssh:user@example:/tmp/project")
        (term-sessions-zmx-program "zmx")
        opened)
    (cl-letf (((symbol-function 'term-sessions--ensure-zmx) #'ignore)
              ((symbol-function 'term-sessions--active-p) (lambda (_name) t))
              ((symbol-function 'term-sessions--open-shell)
               (lambda (name command buffer-name &optional _spec)
                 (setq opened (list name command buffer-name default-directory)))))
      (term-sessions-open-with-frontend "dev" nil 'shell nil)
      (should (equal (nth 0 opened) "dev"))
      (should (string-match-p "getent passwd" (nth 1 opened)))
      (should (string-match-p "zmx attach dev \\\"\\$SHELL\\\" -l" (nth 1 opened)))
      (should (equal (nth 2 opened) "*term-session:dev: [ssh:user@example] /tmp/project*"))
      (should (file-remote-p (nth 3 opened))))))

(ert-deftest term-sessions-test-remote-open-dispatches-tramp-process-for-term ()
  (let ((default-directory "/ssh:user@example#2222:/tmp/project")
        (term-sessions-zmx-program "zmx")
        opened)
    (cl-letf (((symbol-function 'term-sessions--ensure-zmx) #'ignore)
              ((symbol-function 'term-sessions--active-p) (lambda (_name) t))
              ((symbol-function 'term-sessions--open-tramp-process)
               (lambda (name command frontend buffer-name &optional _spec)
                 (setq opened (list name command frontend buffer-name default-directory)))))
      (term-sessions-open-with-frontend "dev" nil 'term nil)
      (should (equal opened
                     '("dev" nil term "*term-session:dev: [ssh:user@example#2222] /tmp/project*"
                       "/ssh:user@example#2222:/tmp/project"))))))

(ert-deftest term-sessions-test-auto-propagates-tramp-attach-error ()
  (let ((default-directory "/ssh:user@example:/tmp/project")
        (term-sessions-zmx-program "zmx")
        (term-sessions-attach-transport 'auto)
        (term-sessions-tramp-process-frontends '(shell)))
    (cl-letf (((symbol-function 'term-sessions--ensure-zmx) #'ignore)
              ((symbol-function 'term-sessions--active-p) (lambda (_name) t))
              ((symbol-function 'term-sessions--open-tramp-process)
               (lambda (&rest _args) (error "TRAMP failed"))))
      (should-error (term-sessions-open-with-frontend "dev" nil 'shell nil)
                    :type 'error))))

(ert-deftest term-sessions-test-rpc-resolves-to-tramp-process ()
  (let ((default-directory "/rpc:user@example:/tmp/project"))
    (should (eq (term-sessions--ensure-interactive-attach-supported nil 'term)
                'tramp-process))
    (should (eq (term-sessions--ensure-interactive-attach-supported nil 'shell)
                'tramp-process))))

(ert-deftest term-sessions-test-ghostel-title-tracking-allows-nil-title ()
  (should (equal (term-sessions--buffer-name-for-title "dev" nil)
                 "*term-session:dev: *"))
  (with-temp-buffer
    ;; Simulate newer Ghostel, where this variable is bound and callbacks return
    ;; the desired buffer name.  Ghostel calls it with nil on OSC 7-only updates.
    (setq-local ghostel-buffer-name-function #'ignore)
    (term-sessions--install-ghostel-title-tracking "dev" "*term-session:dev: fallback*")
    (should (functionp ghostel-buffer-name-function))
    (should (equal (funcall ghostel-buffer-name-function nil)
                   "*term-session:dev: fallback*"))
    (should (equal (funcall ghostel-buffer-name-function "remote title")
                   "*term-session:dev: remote title*"))))

(ert-deftest term-sessions-test-ghostel-open-seeds-safe-name-function-before-exec ()
  (let ((buffer-name "*term-session:dev: [ssh:host] /repo*")
        (had-ghostel-buffer-name-function (boundp 'ghostel-buffer-name-function))
        (old-ghostel-buffer-name-function (and (boundp 'ghostel-buffer-name-function)
                                               ghostel-buffer-name-function))
        seen
        buffer)
    (unwind-protect
        (progn
          (set 'ghostel-buffer-name-function nil)
          (cl-letf (((symbol-function 'require)
                     (lambda (feature &optional _filename _noerror)
                       (eq feature 'ghostel)))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (buf &rest _args)
                       (setq buffer buf)
                       (set-buffer buf)))
                    ((symbol-function 'ghostel-mode)
                     (lambda () (setq major-mode 'ghostel-mode)))
                    ((symbol-function 'ghostel-exec)
                     (lambda (buf _program _args)
                       ;; Simulate Ghostel processing an initial OSC 7-only
                       ;; remote directory update during process startup.
                       (with-current-buffer buf
                         (setq seen (and (functionp ghostel-buffer-name-function)
                                         (funcall ghostel-buffer-name-function nil))))))
                    ((symbol-function 'ghostel-semi-char-mode) #'ignore))
            (setq buffer (term-sessions--ghostel-open-command buffer-name "zmx attach dev"))
            (should (equal seen buffer-name))))
      (if had-ghostel-buffer-name-function
          (set 'ghostel-buffer-name-function old-ghostel-buffer-name-function)
        (makunbound 'ghostel-buffer-name-function))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest term-sessions-test-remote-interactive-attach-supports-ssh-tramp-path ()
  (let ((default-directory "/ssh:example:/tmp"))
    (should (eq (term-sessions--ensure-interactive-attach-supported nil 'term)
                'tramp-process))
    (should (eq (term-sessions--ensure-interactive-attach-supported nil 'shell)
                'tramp-process))))

(ert-deftest term-sessions-test-remote-interactive-attach-refuses-local-transport ()
  (let ((default-directory "/ssh:example:/tmp"))
    (should-error (term-sessions--ensure-interactive-attach-supported nil 'shell 'local)
                  :type 'user-error)))

(ert-deftest term-sessions-test-interactive-attach-command-refuses-remote-directory ()
  (let ((default-directory "/sudo:example:/tmp"))
    (should-error (term-sessions--interactive-attach-command "dev")
                  :type 'user-error)))

(ert-deftest term-sessions-test-term-buffer-base-name-has-no-stars ()
  (should (equal (term-sessions--term-buffer-base-name "dev")
                 "term-session:dev")))

(ert-deftest term-sessions-test-terminal-buffer-base-name-preserves-location ()
  (should (equal (term-sessions--terminal-buffer-base-name
                  "dev" "*term-session:dev: [ssh:host] /repo*")
                 "term-session:dev: [ssh:host] /repo")))

(ert-deftest term-sessions-test-attach-shell-is-remote-safe ()
  ;; Frontends spawn the attach shell on the host owning `default-directory',
  ;; so a local `shell-file-name' must not be handed to a remote attach.
  (let ((shell-file-name "/run/current-system/sw/bin/zsh"))
    (should (equal "/run/current-system/sw/bin/zsh"
                   (term-sessions--attach-shell "/home/user/project/")))
    (should (equal "/bin/sh"
                   (term-sessions--attach-shell "/ssh:user@example:/tmp/")))
    (should (equal "/bin/sh"
                   (term-sessions--attach-shell
                    "/ssh:jump|ssh:user@example#2222:/tmp/")))))

(ert-deftest term-sessions-test-open-term-process-initializes-stty ()
  ;; Mirror term.el: the attach must run through an stty init wrapper so
  ;; remote terminals get sane rows/columns without a pty resize ioctl.
  (let ((buffer (get-buffer-create "*term-session:dev*"))
        captured)
    (unwind-protect
        (progn
          ;; term.el only sets these buffer-locals in `term-mode'; provide
          ;; them the way a real term buffer would have.
          (with-current-buffer buffer
            (setq-local term-height 24)
            (setq-local term-width 80)
            (setq-local term-term-name "eterm-color")
            (setq-local term-termcap-format "%s")
            (setq-local term-protocol-version "2.1")
            (setq-local term-set-terminal-size nil)
            (setq default-directory "/tmp/"))
          (cl-letf (((symbol-function 'term-check-proc) #'ignore)
                    ((symbol-function 'term-mode) #'ignore)
                    ((symbol-function 'term-char-mode) #'ignore)
                    ((symbol-function 'pop-to-buffer) #'ignore)
                    ((symbol-function 'set-process-sentinel) #'ignore)
                    ((symbol-function 'start-file-process)
                     (lambda (_name _buf &rest args)
                       (setq captured args)
                       (start-process "term-sessions-test-stty" nil "true"))))
            (term-sessions--open-term-process
             "dev" "/bin/zmx" '("attach" "dev") "*term-session:dev*")
            (should (equal (nth 0 captured) "/bin/sh"))
            (should (equal (nth 1 captured) "-c"))
            (should (string-prefix-p "stty" (nth 2 captured)))
            (should (equal (nth 3 captured) ".."))
            (should (equal (nth 4 captured) "/bin/zmx"))
            (should (equal (nthcdr 5 captured) '("attach" "dev")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest term-sessions-test-open-term-uses-buffer-name-base ()
  (let ((buffer (generate-new-buffer " *term-sessions-test-term-open*"))
        make-term-name)
    (unwind-protect
        (cl-letf (((symbol-function 'make-term)
                   (lambda (name &rest _args)
                     (setq make-term-name name)
                     buffer))
                  ((symbol-function 'pop-to-buffer) #'ignore)
                  ((symbol-function 'term-mode) #'ignore)
                  ((symbol-function 'term-char-mode) #'ignore))
          (term-sessions--open-term
           "dev" "zmx attach dev" "*term-session:dev: [ssh:host] /repo*")
          (should (equal make-term-name "term-session:dev: [ssh:host] /repo")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest term-sessions-test-list-skips-recently-failed-remotes ()
  (let ((term-sessions-list--failed-remotes (make-hash-table :test #'equal))
        (term-sessions-list-failed-remote-retry-delay 300)
        (calls 0))
    (cl-letf (((symbol-function 'term-sessions--zmx-list-sessions)
               (lambda ()
                 (cl-incf calls)
                 (error "TRAMP failed"))))
      (should-not (term-sessions-list--query-directory "/ssh:example:/"))
      (should (= calls 1))
      (should-not (term-sessions-list--query-directory "/ssh:example:/"))
      (should (= calls 1)))))

(ert-deftest term-sessions-test-list-remote-query-sentinel-runs-once ()
  (let* ((output-buffer (generate-new-buffer " *term-sessions-test-output*"))
         (list-buffer (generate-new-buffer " *term-sessions-test-list*"))
         (process (start-process "term-sessions-test-finished"
                                 output-buffer shell-file-name
                                 shell-command-switch "exit 0"))
         (installs 0)
         (done 0))
    (unwind-protect
        (progn
          (with-current-buffer output-buffer (insert "name=dev\n"))
          (process-put process 'term-sessions-list-buffer list-buffer)
          (process-put process 'term-sessions-list-directory "/ssh:example:/")
          (process-put process 'term-sessions-list-generation 1)
          (while (process-live-p process)
            (accept-process-output process 0.01))
          (cl-letf (((symbol-function 'term-sessions-list--remote-query-done)
                     (lambda (_process) (cl-incf done)))
                    ((symbol-function 'term-sessions-list--clear-remote-failure)
                     #'ignore)
                    ((symbol-function 'term-sessions-list--remote-query-install)
                     (lambda (&rest _args) (cl-incf installs)))
                    ((symbol-function 'term-sessions-list--rows-for-sessions)
                     (lambda (&rest _args) nil)))
            (term-sessions-list--remote-query-sentinel process "finished\n")
            (term-sessions-list--remote-query-sentinel process "finished\n")
            (should (= done 1))
            (should (= installs 1))))
      (when (process-live-p process) (delete-process process))
      (when (buffer-live-p output-buffer) (kill-buffer output-buffer))
      (when (buffer-live-p list-buffer) (kill-buffer list-buffer)))))

(ert-deftest term-sessions-test-list-async-skips-remotes-without-live-connection ()
  (let ((term-sessions-list--failed-remotes (make-hash-table :test #'equal))
        started)
    (cl-letf (((symbol-function 'term-sessions-list--remote-connection-state)
               (lambda (_directory) 'absent))
              ((symbol-function 'start-file-process)
               (lambda (&rest _args)
                 (setq started t))))
      (should-not (term-sessions-list--start-remote-query
                   "/ssh:example:/" (current-buffer) 1))
      (should-not started)
      (should (gethash "/ssh:example:" term-sessions-list--failed-remotes)))))

(ert-deftest term-sessions-test-list-retries-open-session-remotes ()
  (let ((term-sessions-list--failed-remotes (make-hash-table :test #'equal))
        (term-sessions-list-include-open-remotes nil)
        queried)
    (puthash "/ssh:example:" (cons (current-time) "old failure")
             term-sessions-list--failed-remotes)
    (with-temp-buffer
      (setq default-directory "/ssh:example:/tmp/"
            term-sessions-current-name "dev")
      (cl-letf (((symbol-function 'term-sessions-list--query-directory)
                 (lambda (directory)
                   (push directory queried)
                   nil))
                ((symbol-function 'term-sessions-list--start-remote-query)
                 (lambda (directory _buffer _generation)
                   (push directory queried))))
        (with-temp-buffer
          (term-sessions-list-mode)
          (term-sessions-list-refresh))))
    (should (member "/ssh:example:/tmp/" queried))
    (should-not (gethash "/ssh:example:" term-sessions-list--failed-remotes))))

(ert-deftest term-sessions-test-list-deduplicates-remote-root-and-cwd ()
  (should (equal (term-sessions-list--delete-duplicate-directories
                  '("/home/arthur/" "/ssh:example:/tmp/" "/ssh:example:/"))
                 '("/home/arthur/" "/ssh:example:/tmp/"))))

(ert-deftest term-sessions-test-directory-key-ignores-tramp-method ()
  (should (equal (term-sessions--directory-key "/ssh:user@example:/tmp/")
                 (term-sessions--directory-key "/rpc:user@example:/repo/"))))

(ert-deftest term-sessions-test-list-remote-project-label-skips-local-discovery ()
  (let (called)
    (cl-letf (((symbol-function 'term-sessions--project-name)
               (lambda (_cwd)
                 (setq called t)
                 "wrong-local-project")))
      (should (equal (term-sessions-list--project-label
                      "/home/user/project" "/ssh:user@example:/")
                     "project"))
      (should-not called))))

(ert-deftest term-sessions-test-list-skips-malformed-tramp-connections ()
  (let ((term-sessions-list-include-open-remotes t)
        (bad '(tramp-file-name "ssh" nil nil nil nil "/" nil))
        (good '(tramp-file-name "ssh" nil nil "host" nil "/" nil)))
    (cl-letf (((symbol-function 'tramp-list-connections)
               (lambda () (list bad good))))
      (should (equal (term-sessions-list--open-remote-directories)
                     '("/ssh:host:/"))))))

(ert-deftest term-sessions-test-list-session-rows-queries-known-directories ()
  ;; Earlier tests may have cached remote failures in the shared cache.
  (clrhash term-sessions-list--failed-remotes)
  (let (queried remote-queried cleared)
    (cl-letf (((symbol-function 'term-sessions-list--local-directory)
               (lambda () "/home/me/"))
              ((symbol-function 'term-sessions-list--session-buffer-directories)
               (lambda () '("/ssh:host:/repo/")))
              ((symbol-function 'term-sessions-list--open-remote-directories)
               (lambda () '("/ssh:host:/" "/ssh:other:/")))
              ((symbol-function 'term-sessions-list--clear-remote-failure)
               (lambda (directory) (push directory cleared)))
              ((symbol-function 'term-sessions-list--query-directory)
               (lambda (directory)
                 (push directory queried)
                 (list (list (list :name directory :directory directory) []))))
              ((symbol-function 'term-sessions-list--query-remote-directory)
               (lambda (directory)
                 (push directory remote-queried)
                 (list (list (list :name directory :directory directory) [])))))
      (should (equal (mapcar (lambda (row) (plist-get (car row) :directory))
                             (term-sessions-list--session-rows))
                     '("/home/me/" "/ssh:host:/repo/" "/ssh:other:/")))
      (should (equal queried '("/home/me/")))
      ;; "/ssh:host:/" and "/ssh:host:/repo/" share a backend identity.
      (should (equal (nreverse remote-queried)
                     '("/ssh:host:/repo/" "/ssh:other:/")))
      (should (equal cleared '("/ssh:host:/repo/"))))))

(ert-deftest term-sessions-test-list-bounded-remote-query-parses-output ()
  (let ((term-sessions-list-remote-query-timeout 5))
    (clrhash term-sessions--remote-zmx-availability)
    (cl-letf (((symbol-function 'process-file) (lambda (&rest _args) 0))
              ((symbol-function 'start-file-process)
               (lambda (_name buffer &rest _args)
                 (start-process "term-sessions-test-list" buffer
                                "echo" "name=dev\tclients=0"))))
      (should (equal (mapcar (lambda (row) (plist-get (car row) :name))
                             (term-sessions-list--bounded-remote-rows
                              "/ssh:host:/" 5))
                     '("dev"))))))

(ert-deftest term-sessions-test-list-bounded-remote-query-times-out ()
  (let ((term-sessions-list-remote-query-timeout 5)
        failures)
    (clrhash term-sessions--remote-zmx-availability)
    (cl-letf (((symbol-function 'process-file) (lambda (&rest _args) 0))
              ((symbol-function 'start-file-process)
               (lambda (_name buffer &rest _args)
                 (start-process "term-sessions-test-list" buffer "sleep" "5")))
              ((symbol-function 'term-sessions-list--record-remote-failure)
               (lambda (_directory reason) (push reason failures))))
      (should (null (term-sessions-list--bounded-remote-rows
                     "/ssh:slow:/" 0)))
      (should (string-prefix-p "timed out" (car failures))))))

(ert-deftest term-sessions-test-finds-existing-local-session-buffer ()
  (let ((term-sessions-backend 'zmx))
    (with-temp-buffer
      (setq default-directory "/tmp/"
            term-sessions-current-name "dev"
            term-sessions-current-backend 'zmx
            term-sessions-current-terminal-p t)
      (should (eq (term-sessions--session-buffer "dev" "/home/arthur/" 'zmx)
                  (current-buffer))))))

(ert-deftest term-sessions-test-finds-existing-remote-session-buffer-by-prefix ()
  (let ((term-sessions-backend 'zmx))
    (with-temp-buffer
      (setq default-directory "/rpc:example:/tmp/project/"
            term-sessions-current-name "dev"
            term-sessions-current-backend 'zmx
            term-sessions-current-terminal-p t)
      (should (eq (term-sessions--session-buffer "dev" "/rpc:example:/" 'zmx)
                  (current-buffer))))))

(ert-deftest term-sessions-test-session-buffer-ignores-non-terminal-buffers ()
  ;; History and other ancillary buffers carry the session name but must
  ;; never be reused when opening a session.
  (let ((term-sessions-backend 'zmx))
    (with-temp-buffer
      (setq default-directory "/tmp/"
            term-sessions-current-name "dev"
            term-sessions-current-backend 'zmx
            term-sessions-current-terminal-p nil)
      (should (null (term-sessions--session-buffer "dev" "/home/arthur/" 'zmx))))))

(ert-deftest term-sessions-test-list-parse-duration-accepts-weeks ()
  (should (= (term-sessions-list--parse-duration "3w") 1814400))
  (should (= (term-sessions-list--parse-duration "3 weeks") 1814400))
  (should (= (term-sessions-list--parse-duration "2h") 7200))
  (should-not (term-sessions-list--parse-duration "soon")))

(ert-deftest term-sessions-test-fit-column-pads-and-truncates ()
  (should (equal (term-sessions--fit-column "dev" 5) "dev  "))
  (should (equal (string-width (term-sessions--fit-column "development" 5)) 5))
  (should (string-suffix-p "…" (string-trim-right
                                 (term-sessions--fit-column "development" 5)))))

(ert-deftest term-sessions-test-short-directory-name-abbreviates-home-path ()
  (should (equal (term-sessions--short-directory-name
                  "/home/term-sessions-test-user/project/")
                 "~/project"))
  (should (equal (term-sessions--short-directory-name
                  "/home/term-sessions-test-user/")
                 "~")))

(ert-deftest term-sessions-test-completion-table-registers-term-session-category ()
  (clrhash term-sessions--completion-entry-table)
  (let ((default-directory "/tmp/"))
    (cl-letf (((symbol-function 'term-sessions--zmx-list-sessions)
               (lambda ()
                 '((:name "dev" :clients "1" :cwd "/tmp/project" :current-cmd "nvim")))))
      (let ((table (term-sessions--session-completion-table)))
        (should (equal (all-completions "d" table) '("dev")))
        (should (eq (cdr (assq 'category (completion-metadata "" table nil)))
                    'term-session))
        (should (string-match-p "clients:1" (term-sessions--completion-annotate "dev")))
        (should (equal (plist-get (term-sessions--completion-entry "dev") :cwd)
                       "/tmp/project"))))))

(ert-deftest term-sessions-test-distribute-extra-width-respects-max-width ()
  (should (equal (term-sessions--distribute-extra-width
                  '((name 2 1 3)
                    (cwd 4 2 6))
                  10)
                 '((name . 3)
                   (cwd . 6)))))

(ert-deftest term-sessions-test-scaled-column-widths-uses-base-total ()
  (should (equal (term-sessions--scaled-column-widths
                  '((name 2 1 3)
                    (cwd 4 2 6))
                  8)
                 '((name . 3)
                   (cwd . 5)))))

(ert-deftest term-sessions-test-list-column-widths-scale-with-window ()
  (let ((narrow (term-sessions-list--column-widths 100))
        (wide (term-sessions-list--column-widths 180)))
    (should (> (alist-get 'cwd wide) (alist-get 'cwd narrow)))
    (should (> (alist-get 'command wide) (alist-get 'command narrow)))
    (should (= (alist-get 'clients narrow) 7))
    (should (= (alist-get 'clients wide) 7))))

(ert-deftest term-sessions-test-list-time-string-keeps-nonnumeric-strings ()
  (should (equal (term-sessions-list--time-string "unknown") "unknown"))
  (should (equal (term-sessions-list--time-string "123abc") "123abc"))
  (should (string-match-p "1970-" (term-sessions-list--time-string "0"))))

(ert-deftest term-sessions-test-list-updated-seconds-ignores-nonnumeric-strings ()
  (should-not (term-sessions-list--updated-seconds '(:updated-raw "unknown")))
  (should (= (term-sessions-list--updated-seconds '(:updated-raw "12.5")) 12.5)))

(ert-deftest term-sessions-test-list-narrowing-filters-name-client-and-recency ()
  (let* ((now (float-time))
         (dev (list :name "dev" :directory "/tmp/" :where "local"
                    :clients "1" :cwd "/tmp/project" :project "project"
                    :command "nvim" :updated-raw (- now 60)))
         (logs (list :name "logs" :directory "/ssh:host:/" :where "ssh:host"
                     :clients "0" :cwd "/var/log" :project "log"
                     :command "tail" :updated-raw (- now 7200)))
         (rows (list (list dev []) (list logs []))))
    (with-temp-buffer
      (term-sessions-list-mode)
      (let ((term-sessions-list--narrow-criteria
             (list (cons "Name: dev"
                         (lambda (entries)
                           (seq-filter (lambda (row)
                                         (string-match-p "dev" (plist-get (car row) :name)))
                                       entries)))
                   (cons "Attached"
                         (lambda (entries)
                           (seq-filter (lambda (row)
                                         (> (term-sessions-list--clients-number
                                             (plist-get (car row) :clients))
                                            0))
                                       entries)))
                   (cons "Recent"
                         (lambda (entries)
                           (seq-filter (lambda (row)
                                         (< (- now (term-sessions-list--updated-seconds (car row)))
                                            3600))
                                       entries))))))
        (should (equal (mapcar (lambda (row) (plist-get (car row) :name))
                               (term-sessions-list--filtered-entries rows))
                       '("dev")))))))

(ert-deftest term-sessions-test-list-marked-or-current-selection ()
  (let ((entry (list :name "dev" :directory "/tmp/")))
    (with-temp-buffer
      (term-sessions-list-mode)
      (cl-letf (((symbol-function 'term-sessions-list--entry-at-point)
                 (lambda () entry)))
        (should (equal (term-sessions-list--selected-entries) (list entry)))
        (setq term-sessions-list--marked-entries (list entry))
        (should (equal (term-sessions-list--selected-entries) (list entry)))))))

(ert-deftest term-sessions-test-list-stable-mark-key-survives-updated-fields ()
  (let ((old (list :name "dev" :directory "/tmp/" :clients "0" :updated "old"))
        (new (list :name "dev" :directory "/tmp/" :clients "2" :updated "new")))
    (with-temp-buffer
      (term-sessions-list-mode)
      (setq term-sessions-list--marked-entries (list old))
      (should (term-sessions-list--entry-marked-p new)))))

(ert-deftest term-sessions-test-list-mark-helpers-use-entry-key ()
  (let ((old (list :name "dev" :directory "/tmp/" :clients "0"))
        (new (list :name "dev" :directory "/tmp/" :clients "2"))
        (other (list :name "logs" :directory "/tmp/")))
    (with-temp-buffer
      (term-sessions-list-mode)
      (term-sessions-list--mark-entry old)
      (term-sessions-list--mark-entry new)
      (should (= (length term-sessions-list--marked-entries) 1))
      (term-sessions-list--mark-entry other)
      (term-sessions-list--unmark-entry new)
      (should (equal term-sessions-list--marked-entries (list other))))))

(ert-deftest term-sessions-test-list-unmark-entries-preserves-hidden-marks ()
  (let ((visible (list :name "dev" :directory "/tmp/"))
        (visible-new (list :name "dev" :directory "/tmp/" :updated "new"))
        (hidden (list :name "logs" :directory "/tmp/")))
    (with-temp-buffer
      (term-sessions-list-mode)
      (setq term-sessions-list--marked-entries (list visible hidden))
      (term-sessions-list--unmark-entries (list visible-new))
      (should (equal term-sessions-list--marked-entries (list hidden))))))

(ert-deftest term-sessions-test-list-narrowing-prefix-does-not-shadow-next-line ()
  (should (eq (lookup-key term-sessions-list-mode-map (kbd "n")) 'next-line))
  (should (eq (lookup-key term-sessions-list-mode-map (kbd "/ n"))
              'term-sessions-list-narrow-name)))

(ert-deftest term-sessions-test-consult-display-aligns-columns ()
  (clrhash term-sessions--completion-entry-table)
  (clrhash term-sessions-consult--entry-table)
  (cl-letf (((symbol-function 'frame-width) (lambda (&optional _frame) 100)))
    (let* ((entry (list :name "dev" :directory "/tmp/" :where "local"
                        :clients "1" :cwd "/tmp/project" :project "project"
                        :command "nvim" :updated "2026-01-01"))
           (candidate (term-sessions-consult--display entry)))
      (should (string-match-p "\\`dev +local +/tmp/project +nvim" candidate))
      (should (equal (plist-get (term-sessions--completion-entry candidate) :name)
                     "dev"))
      (should (string-match-p "c:1"
                              (term-sessions-consult--annotate candidate)))
      (should (string-match-p "\\[project\\]"
                              (term-sessions-consult--annotate candidate)))
      (should (string-match-p "updated:2026-01-01"
                              (term-sessions-consult--annotate candidate))))))

(ert-deftest term-sessions-test-consult-annotate-omits-empty-metadata ()
  (clrhash term-sessions-consult--entry-table)
  (let* ((entry (list :name "dev" :directory "/tmp/" :where "local"
                      :clients "" :cwd "/tmp/project" :project ""
                      :command "" :updated ""))
         (candidate (term-sessions-consult--display entry)))
    (should (equal (term-sessions-consult--annotate candidate) ""))))

(ert-deftest term-sessions-test-consult-display-disambiguates-truncated-collisions ()
  (clrhash term-sessions--completion-entry-table)
  (clrhash term-sessions-consult--entry-table)
  (cl-letf (((symbol-function 'frame-width) (lambda (&optional _frame) 80)))
    (let* ((first (list :name "development-alpha" :directory "/tmp/one"
                        :where "local" :clients "0"
                        :cwd "/very/long/path/that/truncates/one"))
           (second (list :name "development-alpha" :directory "/tmp/two"
                         :where "local" :clients "0"
                         :cwd "/very/long/path/that/truncates/one"))
           (first-candidate (term-sessions-consult--display first))
           (second-candidate (term-sessions-consult--display second)))
      (should-not (equal first-candidate second-candidate))
      (should (string-suffix-p "#2" second-candidate))
      (should (equal (plist-get (term-sessions-consult--entry first-candidate) :directory)
                     "/tmp/one"))
      (should (equal (plist-get (term-sessions-consult--entry second-candidate) :directory)
                     "/tmp/two")))))

(ert-deftest term-sessions-test-consult-items-filter-and-register-entry ()
  (clrhash term-sessions--completion-entry-table)
  (let ((local (list :name "dev" :directory "/tmp/" :where "local"
                     :clients "1" :cwd "/tmp/project" :project "project"
                     :command "nvim" :updated "2026-01-01"))
        (remote (list :name "prod" :directory "/ssh:host:/" :where "ssh:host"
                      :clients "0" :cwd "/srv" :project "srv"
                      :command "bash" :updated "2026-01-01")))
    (cl-letf (((symbol-function 'term-sessions-consult--entries)
               (lambda () (list local remote)))
              ((symbol-function 'frame-width) (lambda (&optional _frame) 100)))
      (let ((items (term-sessions-consult--items #'term-sessions-consult--local-p)))
        (should (= (length items) 1))
        (should (string-match-p "\\`dev +local +/tmp/project +nvim" (car items)))
        (should (equal (plist-get (term-sessions--completion-entry (car items)) :name)
                       "dev"))
        (should (string-match-p "c:1"
                                (term-sessions-consult--annotate (car items))))))))

(ert-deftest term-sessions-test-consult-current-project-stays-on-current-host ()
  (let ((default-directory "/ssh:host:/repo/")
        (same-host (list :name "same" :directory "/rpc:host:/" :cwd "/repo/sub"))
        (other-host (list :name "other" :directory "/ssh:other:/" :cwd "/repo/sub"))
        (local (list :name "local" :directory "/tmp/" :cwd "/repo/sub")))
    (should (term-sessions-consult--current-project-p same-host))
    (should-not (term-sessions-consult--current-project-p other-host))
    (should-not (term-sessions-consult--current-project-p local))))

(ert-deftest term-sessions-test-consult-open-reuses-existing-buffer ()
  (clrhash term-sessions-consult--entry-table)
  (let* ((entry (list :name "dev" :directory "/tmp/project/" :where "local"))
         (candidate (term-sessions-consult--display entry))
         (buffer (generate-new-buffer " *term-sessions-test-consult-open*"))
         ensured opened)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq default-directory "/tmp/project/"
                  term-sessions-current-name "dev"
                  term-sessions-current-backend 'zmx
                  term-sessions-current-terminal-p t))
          (cl-letf (((symbol-function 'term-sessions--ensure-zmx)
                     (lambda () (setq ensured t)))
                    ((symbol-function 'term-sessions--open-command-frontend)
                     (lambda (&rest args) (setq opened args))))
            (term-sessions-consult--open candidate)
            (should (eq (current-buffer) buffer))
            (should-not ensured)
            (should-not opened)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest term-sessions-test-consult-session-allows-new-name ()
  (let (options opened)
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional _filename _noerror)
                 (eq feature 'consult)))
              ((symbol-function 'consult--multi)
               (lambda (_sources &rest args)
                 (setq options args)
                 '("new-session" :match nil)))
              ((symbol-function 'term-sessions-open)
               (lambda (name &optional command)
                 (setq opened (list name command)))))
      (term-sessions-consult-session)
      (should-not (plist-get options :require-match))
      (should (equal opened '("new-session" nil))))))

(ert-deftest term-sessions-test-consult-session-does-not-create-on-match ()
  (let (opened)
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional _filename _noerror)
                 (eq feature 'consult)))
              ((symbol-function 'consult--multi)
               (lambda (_sources &rest _args)
                 '("dev" :match t)))
              ((symbol-function 'term-sessions-open)
               (lambda (&rest args)
                 (setq opened args))))
      (term-sessions-consult-session)
      (should-not opened))))

(ert-deftest term-sessions-test-consult-sources-have-common-properties ()
  (dolist (source-symbol term-sessions-consult-sources)
    (let ((source (symbol-value source-symbol)))
      (should (plist-get source :name))
      (should (eq (plist-get source :category) 'term-session))
      (should (eq (plist-get source :annotate) 'term-sessions-consult--annotate))
      (should (eq (plist-get source :action) 'term-sessions-consult--open))
      (should (functionp (plist-get source :items))))))

(ert-deftest term-sessions-test-action-history-variants-pass-format-flags ()
  (let ((candidate "dev @ local /tmp/project")
        calls)
    (term-sessions--register-completion-entry
     candidate (list :name "dev" :directory "/tmp/"))
    (cl-letf (((symbol-function 'term-sessions-history)
               (lambda (&rest args)
                 (push args calls))))
      (term-sessions-action-history-full candidate)
      (term-sessions-action-history-vt candidate)
      (term-sessions-action-history-html candidate))
    (should (equal (nreverse calls)
                   '(("dev" nil)
                     ("dev" nil t nil)
                     ("dev" nil nil t))))))

(ert-deftest term-sessions-test-action-copy-history-copies-full-history ()
  (let ((candidate "dev @ local /tmp/project"))
    (term-sessions--register-completion-entry
     candidate (list :name "dev" :directory "/tmp/"))
    (cl-letf (((symbol-function 'term-sessions--zmx)
               (lambda (&rest args)
                 (should (equal args '("history" "dev")))
                 "line 1\nline 2\n")))
      (term-sessions-action-copy-history candidate))
    (should (equal (current-kill 0 t) "line 1\nline 2\n"))))

(ert-deftest term-sessions-test-action-entry-cwd-preserves-remote-prefix ()
  (should (equal (term-sessions-action--entry-cwd-directory
                  (list :name "dev" :directory "/ssh:user@example:/" :cwd "/repo"))
                 "/ssh:user@example:/repo/"))
  (should (equal (term-sessions-action--entry-cwd-directory
                  (list :name "dev" :directory "/tmp/backend/" :cwd "/repo"))
                 "/repo/")))

(ert-deftest term-sessions-test-action-directory-actions-use-session-cwd ()
  (require 'project)
  (let ((candidate "dev @ local /tmp/project")
        calls)
    (term-sessions--register-completion-entry
     candidate (list :name "dev" :directory "/tmp/backend/" :cwd "/tmp/project/"))
    (cl-letf (((symbol-function 'dired)
               (lambda (directory &optional _switches)
                 (push (cons 'dired directory) calls)))
              ((symbol-function 'compile)
               (lambda (command &optional _comint)
                 (push (list 'compile default-directory command) calls)))
              ((symbol-function 'project-compile)
               (lambda ()
                 (interactive)
                 (push (cons 'project-compile default-directory) calls))))
      (term-sessions-action-dired-cwd candidate)
      (term-sessions-action-compile-cwd candidate "make check")
      (term-sessions-action-project-compile candidate))
    (should (equal (nreverse calls)
                   '((dired . "/tmp/project/")
                     (compile "/tmp/project/" "make check")
                     (project-compile . "/tmp/project/"))))))

(ert-deftest term-sessions-test-action-send-target-actions-prompt-for-session ()
  (let (calls)
    (cl-letf (((symbol-function 'term-sessions-read-existing-session-entry)
               (lambda (_prompt)
                 (list :name "dev" :directory "/tmp/")))
              ((symbol-function 'term-sessions-send)
               (lambda (&rest args) (push (cons 'send args) calls)))
              ((symbol-function 'term-sessions-send-command)
               (lambda (&rest args) (push (cons 'send-command args) calls)))
              ((symbol-function 'term-sessions-run)
               (lambda (&rest args) (push (cons 'run args) calls))))
      (term-sessions-action-send-text-to-session "echo hi")
      (term-sessions-action-send-command-text-to-session "make check")
      (term-sessions-action-send-file-path-to-session "/tmp/file name")
      (term-sessions-action-run-file-in-session "/tmp/script name"))
    (should (equal (nreverse calls)
                   '((send "dev" "echo hi")
                     (send-command "dev" "make check")
                     (send "dev" "/tmp/file\\ name")
                     (run "dev" "/tmp/script\\ name" nil))))))

(ert-deftest term-sessions-test-action-run-and-wait-call-zmx-helpers ()
  (let ((candidate "dev @ local /tmp/project")
        calls)
    (term-sessions--register-completion-entry
     candidate (list :name "dev" :directory "/tmp/"))
    (cl-letf (((symbol-function 'term-sessions-run)
               (lambda (&rest args) (push (cons 'run args) calls)))
              ((symbol-function 'term-sessions-run-async)
               (lambda (&rest args) (push (cons 'run-async args) calls)))
              ((symbol-function 'term-sessions-wait)
               (lambda (&rest args) (push (cons 'wait args) calls)))
              ((symbol-function 'term-sessions-wait-async)
               (lambda (&rest args) (push (cons 'wait-async args) calls))))
      (term-sessions-action-run-command candidate "make check")
      (term-sessions-action-run-async candidate "make watch")
      (term-sessions-action-wait candidate)
      (term-sessions-action-wait-async candidate))
    (should (equal (nreverse calls)
                   '((run "dev" "make check" nil)
                     (run-async "dev" "make watch")
                     (wait "dev")
                     (wait-async "dev"))))))

(ert-deftest term-sessions-test-action-copy-metadata ()
  (let ((candidate "dev @ local /tmp/project")
        (term-sessions-current-time-function (lambda () 0)))
    (term-sessions--register-completion-entry
     candidate (list :name "dev" :directory "/tmp/backend/" :cwd "/tmp/project/"
                     :command "nvim main.c" :where "local"))
    (term-sessions-action-copy-cwd candidate)
    (should (equal (current-kill 0 t) "/tmp/project/"))
    (term-sessions-action-copy-command candidate)
    (should (equal (current-kill 0 t) "nvim main.c"))
    (term-sessions-action-copy-where candidate)
    (should (equal (current-kill 0 t) "local"))
    (term-sessions-action-copy-spec-link candidate)
    (should (string-match-p "\\`term-session:spec:.*name=dev" (current-kill 0 t)))
    (should (string-match-p "cwd=%2Ftmp%2Fproject%2F" (current-kill 0 t)))))

(ert-deftest term-sessions-test-action-copy-and-insert-org-link ()
  (let ((candidate "dev @ local /tmp/project")
        (term-sessions-current-time-function (lambda () 0)))
    (term-sessions--register-completion-entry
     candidate (list :name "dev" :directory "/tmp/project/"))
    (term-sessions-action-copy-org-link candidate)
    (should (string-match-p "\\`\\[\\[term-session:spec:.*name=dev" (current-kill 0 t)))
    (with-temp-buffer
      (term-sessions-action-insert-org-link candidate)
      (should (equal (buffer-string) (current-kill 0 t))))))

(ert-deftest term-sessions-test-action-org-link-target-finds-raw-link ()
  (with-temp-buffer
    (insert "see term-session:spec:backend=zmx&name=dev&cwd=%2Ftmp%2F now")
    (search-backward "name=dev")
    (let ((target (term-sessions-action-org-link-target)))
      (should (eq (nth 0 target) 'term-session-link))
      (should (equal (nth 1 target)
                     "term-session:spec:backend=zmx&name=dev&cwd=%2Ftmp%2F"))
      (should (equal (buffer-substring-no-properties (nth 2 target) (cdr (cddr target)))
                     (nth 1 target))))))

(ert-deftest term-sessions-test-action-open-org-link-strips-type-prefix ()
  (let (opened)
    (cl-letf (((symbol-function 'term-sessions--open-org-path)
               (lambda (path arg)
                 (setq opened (list path arg)))))
      (term-sessions-action-open-org-link "term-session:spec:backend=zmx&name=dev"))
    (should (equal opened '("spec:backend=zmx&name=dev" nil)))))

(ert-deftest term-sessions-test-action-list-row-target-registers-entry ()
  (let ((entry (list :name "dev" :directory "/tmp/" :where "local" :cwd "/tmp/project")))
    (cl-letf (((symbol-function 'derived-mode-p)
               (lambda (&rest modes)
                 (memq 'term-sessions-list-mode modes)))
              ((symbol-function 'tabulated-list-get-id)
               (lambda () entry)))
      (let ((target (term-sessions-action-list-row-target)))
        (should (eq (car target) 'term-session))
        (should (equal (term-sessions--completion-entry (cdr target)) entry))))))

(ert-deftest term-sessions-test-action-current-buffer-target-registers-session ()
  (let ((term-sessions-current-time-function (lambda () 0)))
    (with-temp-buffer
      (setq default-directory "/tmp/project/")
      (term-sessions--mark-buffer
       "dev" (term-sessions-spec-current "dev" "make" 'term))
      (should (equal (term-sessions-action-current-buffer-target)
                     '(term-session . "dev")))
      (should (equal (term-sessions--completion-entry "dev")
                     (list :name "dev" :directory "/tmp/project/"
                           :cwd "/tmp/project/" :command "make"))))))

(ert-deftest term-sessions-test-action-copy-name-decodes-registered-candidate ()
  (let ((candidate "dev @ local /tmp/project"))
    (term-sessions--register-completion-entry
     candidate (list :name "dev" :directory "/tmp/"))
    (term-sessions-action-copy-name candidate)
    (should (equal (current-kill 0 t) "dev"))))

(ert-deftest term-sessions-test-action-copy-attach-command-copies-local-command ()
  (let ((candidate "dev @ local /tmp/project")
        (term-sessions-zmx-program "zmx"))
    (term-sessions--register-completion-entry
     candidate (list :name "dev" :directory "/tmp/"))
    (term-sessions-action-copy-attach-command candidate)
    (should (equal (current-kill 0 t) "zmx attach dev"))))

(ert-deftest term-sessions-test-action-copy-attach-command-rejects-remote-entry ()
  (let ((candidate "dev @ ssh:host /tmp/project"))
    (term-sessions--register-completion-entry
     candidate (list :name "dev" :directory "/ssh:host:/tmp/"))
    (should-error (term-sessions-action-copy-attach-command candidate)
                  :type 'user-error)))

(provide 'term-sessions-tests)
;;; term-sessions-tests.el ends here
