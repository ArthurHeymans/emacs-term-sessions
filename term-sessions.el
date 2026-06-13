;;; term-sessions.el --- Persistent terminal sessions via zmx -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Copyright (C) 2026

;; Author: Arthur
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: terminals, processes, tools
;; URL: https://github.com/arthur/term-sessions

;;; Commentary:

;; A small greenfield Emacs frontend for persistent terminal sessions.
;; The first backend is zmx: https://github.com/neurosnap/zmx
;;
;; Sessions are owned by zmx, not by Emacs.  Emacs shells out for control
;; operations and opens interactive attaches through a selected terminal
;; frontend such as vterm, eat, term, or shell.

;;; Code:

(require 'term-sessions-core)
(require 'term-sessions-zmx)
(require 'term-sessions-tramp)
(require 'term-sessions-frontends)
(require 'term-sessions-org)
(require 'term-sessions-list)
(require 'term-sessions-actions)
(require 'term-sessions-consult)

(provide 'term-sessions)
;;; term-sessions.el ends here
