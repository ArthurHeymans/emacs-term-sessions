# term-sessions continuation and TRAMP integration plan

## Summary

The current separate `term-sessions` project is the right shape. It overlaps with `tramp-rpc` in remote process/PTY transport, but it should not become part of `tramp-rpc`.

Recommended split:

```text
term-sessions.el = user-facing persistent session manager
tramp-rpc        = optional high-performance TRAMP transport
zmx              = current external persistent PTY/session owner
```

`term-sessions` should use standard TRAMP process APIs so that `/ssh:` uses normal TRAMP and `/rpc:` uses `tramp-rpc` automatically.

## Design match with original goal

Original goal:

```text
laptop Emacs as frontend
main desktop owns/runs the real sessions
closing laptop does not stop work
TRAMP or tramp-rpc should make remote use pleasant
```

Current status:

```text
Emacs frontend              yes
external session owner      yes, zmx
survives Emacs exit         yes, via zmx
TRAMP control calls         partially, via process-file/start-file-process
TRAMP interactive attach    implemented for term/eat/ghostel/vterm/shell, with SSH fallback
/rpc: support               implemented through TRAMP/tramp-rpc process APIs
```

So the architecture is correct, but proper TRAMP/tramp-rpc integration remains the main missing piece.

## Why keep this separate from tramp-rpc?

`tramp-rpc` operates at the transport/file-handler layer:

- remote file operations
- `process-file`
- `make-process`
- `start-file-process`
- remote PTYs
- resize handling
- SSH ControlMaster / ProxyJump handling
- remote server deployment

`term-sessions` operates at the UX/session layer:

- stable named terminal sessions
- zmx/shpool/keepty/custom backend adapters
- Org links
- session metadata and recreate policy
- frontend adapters for vterm/eat/ghostel/term/shell
- list/history/tail/send/run/wait commands
- detached.el-style session UX

`tramp-rpc` PTYs are not durable session owners. They are transport processes tied to the RPC server/connection. Persistence should come from `zmx` or a future session daemon, not from tramp-rpc itself.

## What might belong in tramp-rpc later

Do not move zmx/session UX into `tramp-rpc`. If needed, add small generic public helpers to `tramp-rpc`, such as:

```elisp
tramp-rpc-make-remote-pty-process
tramp-rpc-ssh-argv-for-vec
tramp-rpc-remote-deploy-binary
```

These should remain transport/deployment helpers, not session-manager logic.

Avoid depending on private `tramp-rpc--...` functions from `term-sessions` unless they are promoted to public APIs.

## Continuation plan

### Milestone 1: Refactor around explicit seams

Split the current one-file prototype into clearer layers without changing user-visible behavior.

Suggested modules:

```text
term-sessions.el              public commands
term-sessions-core.el         session/location/spec structs
term-sessions-zmx.el          zmx backend
term-sessions-frontends.el    vterm/eat/ghostel/term/shell adapters
term-sessions-tramp.el        TRAMP location and attach transport logic
term-sessions-org.el          Org link store/follow
term-sessions-list.el         tabulated/consult UI
```

Add internal concepts:

```elisp
term-sessions-location
term-sessions-session
term-sessions-spec
term-sessions-backend
term-sessions-frontend
```

### Milestone 2: Session spec, metadata, and naming

Add a session spec containing:

```text
name
backend
location
cwd
command
frontend
project
tags
created-at
recreate policy
```

Add deterministic naming helpers for common workflows:

```text
PROJECT
PROJECT-pi
PROJECT-build
HOST:PROJECT
```

### Milestone 3: Better Org links

Current Org links encode only backend/location/target/name. They should preserve enough information to reopen or safely recreate a session.

Store at least:

```text
backend
full TRAMP directory
session name
cwd
command
preferred frontend
project name
created-at
```

Opening an Org link should:

1. Restore the original `default-directory`.
2. Query the backend.
3. Attach if active.
4. If missing, offer to recreate.
5. Recreate in the original cwd, not `~/`.
6. Never silently recreate.

Compatibility note: the implementation may keep parsing old
`term-session:zmx:local:...` and `term-session:zmx:ssh:...` links during the
transition, but new stored links should use the richer session-spec format.
After existing notes are migrated, remove the legacy parser branch and tests.

### Milestone 4: zmx hardening

Add:

- `zmx version` / capability checks.
- Better diagnostics for missing local or remote `zmx`.
- Connection-local `term-sessions-zmx-program`.
- Optional `ZMX_DIR` support.
- Optional `ZMX_SESSION_PREFIX` support.
- Clear unsupported-version errors.
- Integration tests against real `zmx`.

### Milestone 5: UX improvements from detached.el

Borrow UX ideas, not architecture:

- consult source
- richer tabulated list
- annotations: host, cwd, project, branch
- shell/eshell integration
- compile/job integration
- notifications for completed jobs
- history/copy/diff actions

Avoid inheriting detached.el pitfalls:

- no Emacs Lisp DB as source of truth
- no socket/log file inference as lifecycle protocol
- no `tail` prepended into interactive terminal attach
- no hidden attach buffers for control operations

## Proper TRAMP integration plan

### Phase A: Add a real location model

Replace ad hoc remote parsing with a TRAMP-aware location object.

Use public APIs where possible:

```elisp
(file-remote-p default-directory)
(file-remote-p default-directory 'method)
(file-remote-p default-directory 'user)
(file-remote-p default-directory 'host)
(file-remote-p default-directory 'localname)
(file-remote-p default-directory 'hop)
```

When separate port/hop/localname fields are needed, use TRAMP parsing:

```elisp
(let ((v (tramp-dissect-file-name default-directory)))
  (list (tramp-file-name-method v)
        (tramp-file-name-user v)
        (tramp-file-name-host v)
        (tramp-file-name-port v)
        (tramp-file-name-hop v)
        (tramp-file-name-localname v)))
```

Preserve full TRAMP identity, including method, user, host, port, hop, and localname. Do not reduce remote identity to `user@host`.

### Phase B: Keep control calls through TRAMP

For noninteractive control operations, keep using `process-file` and `start-file-process` with the target `default-directory` bound.

Example:

```elisp
(let ((default-directory remote-dir)
      (process-file-side-effects nil))
  (process-file "zmx" nil t nil "list" "--short"))
```

This lets `/ssh:` use normal TRAMP and `/rpc:` use tramp-rpc.

### Phase C: Add attach transport abstraction

Add an attach transport setting:

```elisp
term-sessions-attach-transport
```

Possible values:

```text
auto
local
tramp-process
tramp-rpc
ssh-wrapper
```

Resolution in `auto` mode:

```text
local path       -> local
/rpc: path       -> tramp-process/tramp-rpc
/ssh: path       -> tramp-process, fallback to ssh-wrapper if needed
complex TRAMP    -> tramp-process if it works, otherwise explicit unsupported error
```

### Phase D: Prefer TRAMP process APIs for interactive attach

Long-term interactive attach should prefer this shape:

```elisp
(let ((default-directory remote-dir)
      (process-connection-type t))
  (start-file-process "zmx-attach" buffer "zmx" "attach" name))
```

or:

```elisp
(make-process
 :name "zmx-attach"
 :buffer buffer
 :command (list "zmx" "attach" name)
 :connection-type 'pty
 :file-handler t)
```

This lets TRAMP/tramp-rpc handle:

- host
- user
- port
- hops/proxies
- auth
- ControlMaster
- method-specific behavior
- remote cwd
- remote PATH

Keep the current local `ssh ... zmx attach` wrapper only as a fallback.

### Phase E: `/rpc:` support through tramp-rpc

For `/rpc:` paths:

1. Use `process-file` for zmx control calls.
2. Use `start-file-process` or `make-process :connection-type 'pty` for `zmx attach`.
3. Let tramp-rpc choose direct SSH PTY or RPC PTY.
4. Treat tramp-rpc only as the attach transport.
5. Treat zmx as the persistent session owner.

Target architecture:

```text
laptop Emacs
  -> /rpc:desktop:/repo
  -> tramp-rpc process/PTY transport
  -> remote zmx attach
  -> zmx persistent session
```

When laptop Emacs disconnects, the attach transport dies but the zmx session remains alive.

### Phase F: Connection-local configuration

Document and support per-host settings:

```elisp
(connection-local-set-profile-variables
 'desktop-term-sessions
 '((term-sessions-zmx-program . "/home/arthur/.nix-profile/bin/zmx")
   (term-sessions-zmx-dir . "/run/user/1000/zmx")
   (term-sessions-preferred-frontend . vterm)))

(connection-local-set-profiles
 '(:application tramp :protocol "rpc" :machine "desktop")
 'desktop-term-sessions)
```

Also support binding zmx-related environment variables for process calls:

```text
ZMX_DIR
ZMX_SESSION_PREFIX
```

## Validation plan

### Unit tests

Add tests for:

- `/ssh:host:/path`
- `/ssh:user@host:/path`
- `/ssh:user@host#2222:/path`
- multi-hop paths
- `/rpc:user@host:/path`
- paths with spaces and shell metacharacters
- full TRAMP location parsing
- Org link roundtrip preserving full TRAMP directory
- connection-local `term-sessions-zmx-program`
- attach transport selection
- unsupported method errors

### Manual integration tests

Run on real local and laptop/desktop setups:

```text
local session survives Emacs restart
/ssh:desktop: session survives Emacs restart
/ssh:desktop: session survives laptop sleep/disconnect
/rpc:desktop: session survives laptop sleep/disconnect
vterm attach works
eat attach works
resize while running nvim/fzf/less
history works after reconnect
send/run/wait work remotely
Org link opens original cwd
Org link recreate offers safe recreate for missing session
```

### Pi-specific validation

For the original workflow:

```text
Open /rpc:desktop:/repo
M-x term-sessions-start RET PROJECT-pi RET
Run pi inside the session
Close laptop
Reconnect from laptop
M-x term-sessions-open RET PROJECT-pi RET
Confirm the same pi process/session is alive
```

If the native `pi-coding-agent` Emacs JSON-RPC UI must reconnect to the same remote `pi` process, that is a separate future milestone: a persistent stdio/RPC bridge using tramp-rpc-style transport. Terminal persistence alone does not solve JSON-RPC UI reconnection.

## Immediate next implementation step

The original immediate TRAMP integration step is complete:

1. `term-sessions-location` parsing.
2. Attach transport abstraction.
3. Current SSH wrapper retained as fallback.
4. Tests for `/ssh:user@host#2222:` and `/rpc:user@host:`.
5. TRAMP process attach for `term`, `eat`, `ghostel`, `vterm`, and `shell`.

Next hardening work: improve diagnostics for frontend-specific TRAMP failures, add more real integration tests, and decide whether simple SSH-like paths should keep automatic SSH-wrapper fallback or require users to opt into it explicitly.
