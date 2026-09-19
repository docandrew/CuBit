# Retained CCL handlers and discrete callback events

Run from the repository root (Linux-hosted, inside Nix):

```sh
nix develop -c bash tests/ccl-callbacks/run.sh
```

`handler_tests` prepares a checked `() -> Boolean` entry point without executing
the program's main expression, destroys the caller's source/handler copies,
and invokes the retained code. It covers invalid source/profile/name, absent or
rebound grants, fresh fuel, owned label results, queue capacity, failure isolation
and Boolean false as a normal result (not an implicit retry).

`queue_tests` covers capacity, repeated ring wrap, one in-flight event, rejected
duplicate/stale completions, enqueue while executing, explicit close/discards,
draining a noncancelable invocation, close/reopen with old-generation events,
failure/discards, and generation/sequence exhaustion using a small counter limit.
These model tests do not claim to exercise actual asynchronous IPC cancellation.

Focused proof:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/ccl-callbacks/callbacks_tests.gpr --subdirs=queue-proof -u queue_model.ads --mode=all --level=1 --timeout=5 -j2'
```

The instantiation explicitly enables SPARK; inspecting a generic template alone
is not proof coverage. The queue proof includes runtime bounds, event-count
postconditions, the exact close/draining transition, and acceptance of a completion
if and only if its ticket matches the active generation/invocation. The latter
two specifications use Ghost functions. This is not a full temporal/FIFO proof,
nor proof of the interpreter, dispatch host, concurrent execution or Desktop IPC.

Handler/dispatcher flow check:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/ccl-callbacks/callbacks_tests.gpr --subdirs=handler-flow -u ccl-language-handlers.adb ccl-callbacks.adb --mode=flow -j2'
```

These are owner/event-loop-confined objects. References are owner-local, not
globally routable capabilities. Hosts must route events through the authenticated
owner and serialize access. The host supplies current grants and must keep its
binding identities stable until outstanding uses drain; IPC itself must still
check live endpoint/session authority. A grant check at dispatch is not a
replacement for those checks at the service boundary.

All new runtime packages also compile against CuBit's userspace runtime. The
Workbench build and existing hosted UI/source-view/remote regressions pass.
`button_tests` exercises real CCL `(handler name)` registration, retained source
ownership across edits, active-registration rejection, wrong profiles/types,
pre-effect grant admission, missing dispatch grants, self-close with pending
click discards, and Lisp/BASIC conversion. No GUI is needed for these checks.

The actual shared Workbench button can also be exercised on Linux:

```sh
nix develop -c bash tests/ccl-callbacks/run-preview.sh
```

It drives SDL input and checks captured rendered frames for the pressed state,
the label update on click, and Stop removing the button. Native integration is
covered by `tests/usb-optical/run-live.py --cpus 4 --ccl-ui-hooks` after rebuilding
`make -C kernel usb-live-iso`, all inside Nix.

This native test passed with four KVM CPUs and the USB optical image: the REPL
registered a button, native mouse input invoked its retained handler, and the
label changed to `Clicked!` (also confirmed in the captured screenshot).

Focused handler-reference / typed-host-value / queue proof:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/ccl-callbacks/callbacks_tests.gpr --subdirs=button-proof -u ccl-handler_references.adb ccl-host_values.adb queue_model.ads --mode=all --level=1 --timeout=5 -j2'
```

Result: 77 checks, comprising 34 flow, 38 runtime and 5 functional; zero
unproved or justified. This does not prove the interpreter or the UI host.
The separate flow run over `ccl-ui_buttons`, `ccl-language`,
`ccl-language-handlers`, and `ccl-callbacks` discharged 151 initialization,
non-aliasing and termination checks; that run does not establish runtime-error
absence or functional correctness of the interpreter.
The owned source/name reference is code, not authority. Registration re-analyzes
it once in the owner's catalog/grants; clicks run the retained checked tree.
