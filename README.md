# term-sessions.el

Greenfield Emacs frontend for persistent terminal sessions owned by an external backend.

The initial backend is [`zmx`](https://github.com/neurosnap/zmx). Emacs does not own session lifecycle state; it shells out to `zmx` for list/start/attach/kill/history/tail/send/run/wait operations and opens interactive attaches in a terminal frontend.

## Development

Enter a shell with Emacs and zmx available:

```sh
nix develop
```

## Basic usage

Add the repo to `load-path`, then:

```elisp
(require 'term-sessions)
(setq term-sessions-preferred-frontend 'term) ; built in; or 'vterm, 'eat, 'shell
```

`zmx` must be available at runtime wherever the session commands run. The dev shell provides it locally; for TRAMP paths, install/configure `zmx` on the remote host or set `term-sessions-zmx-program` connection-locally.

Current remote status: noninteractive/control calls (`list`, `kill`, `history`, `send`, `run`, `wait`, and `tail`) use Emacs process APIs that can run through TRAMP `process-file`/`start-file-process`. Interactive attach is controlled by `term-sessions-attach-transport`: local directories attach locally, and `term`, `eat`, `ghostel`, `vterm`, and `shell` now prefer TRAMP process attaches for remote directories, including `/rpc:` when tramp-rpc provides that method and TRAMP multi-hop paths. The local SSH wrapper remains as an explicit fallback and is also used automatically when an `auto` TRAMP attach fails for a simple SSH-like path.

`term-sessions-start` creates-or-attaches. `term-sessions-open` only opens existing sessions.

Commands:

- `M-x term-sessions-start`
- `M-x term-sessions-open`
- `M-x term-sessions-list`
- `M-x term-sessions-kill`
- `M-x term-sessions-history`
- `M-x term-sessions-tail`
- `M-x term-sessions-send`
- `M-x term-sessions-send-command`
- `M-x term-sessions-run`
- `M-x term-sessions-run-async`
- `M-x term-sessions-wait`
- `M-x term-sessions-wait-async`
- `M-x term-sessions-store-org-link`

Org links store a session spec with the original TRAMP directory/cwd, command,
frontend, project, timestamp, and recreate policy.  Legacy local/SSH links are
still accepted:

```org
[[term-session:spec:backend=zmx&name=work&cwd=%2Fsshx%3Ahost%3A%2Frepo%2F&frontend=term]]
[[term-session:zmx:local:work]]
[[term-session:zmx:ssh:user%40host:work]]
```

Attach transport can be customized:

```elisp
(setq term-sessions-attach-transport 'auto)          ; default
(setq term-sessions-attach-transport 'tramp-process) ; force TRAMP process attach
(setq term-sessions-attach-transport 'ssh-wrapper)   ; force local SSH wrapper
```

## Frontends

`vterm`, `eat`, `ghostel`, `term`, and `shell` are implemented for local interactive attach. For remote directories they prefer TRAMP/tramp-rpc process attaches; `ghostel` uses its native TRAMP-aware `ghostel-exec` path and keeps Ghostel title/directory tracking while prefixing buffer names with the term session name. `term-sessions-ghostel-open-function` remains customizable for users who want a different Ghostel opener.

## Internal modules

The package is split by responsibility:

- `term-sessions-core.el`: shared custom variables, structs, and helpers
- `term-sessions-zmx.el`: zmx backend/control commands
- `term-sessions-tramp.el`: TRAMP location parsing, session specs, attach transport selection
- `term-sessions-frontends.el`: terminal frontend adapters and open/start commands
- `term-sessions-org.el`: Org link store/follow
- `term-sessions-list.el`: tabulated list UI

## Nix lockfile

`flake.lock` is kept in the repository so the zmx/nixpkgs inputs used by the dev shell and checks are reproducible. Update it intentionally with `nix flake update`.
