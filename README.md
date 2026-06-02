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

Current remote status: noninteractive/control calls (`list`, `kill`, `history`, `send`, `run`, `wait`, and `tail`) use Emacs process APIs that can run through TRAMP `process-file`/`start-file-process`. For `/ssh:`-style TRAMP directories, interactive attach runs a local `ssh host 'cd DIR && zmx attach NAME'` command inside the chosen terminal frontend, so it attaches to the remote zmx session without accidentally creating a local one. Other TRAMP methods are refused for interactive attach until implemented.

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

Org links use local or SSH-backed zmx formats:

```org
[[term-session:zmx:local:work]]
[[term-session:zmx:ssh:user%40host:work]]
```

## Frontends

`vterm`, `eat`, `term`, and `shell` are implemented for local interactive attach and SSH-backed remote attach. `ghostel` is pluggable through `term-sessions-ghostel-open-function` until its Emacs API settles.

## Nix lockfile

`flake.lock` is kept in the repository so the zmx/nixpkgs inputs used by the dev shell and checks are reproducible. Update it intentionally with `nix flake update`.
