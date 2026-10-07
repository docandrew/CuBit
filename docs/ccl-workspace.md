# CCL Workbench workspace, REPL, and widget milestones

Status: native named-source storage and shared Open/Save dialog implemented,
with a Linux mock adapter. Shared REPL sessions now execute admitted host calls
and expose one scoped numeric label; general UI service bindings remain planned.
September 2026.

## Native CuBit: usable now

The existing toolbar Open/Save controls and Ctrl+O/Ctrl+S operate on a bounded,
explicitly manifest-authorized workspace. This is native filesystem IPC through
generation-tagged loans and file/directory handles, not Ada.Command_Line or
hosted file I/O. Workbench requests no filesystem-policy administrative power.

- Save opens a **Save new** dialog. It suggests `ccl-0001.ccl`, `ccl-0002.ccl`,
  etc., but accepts a chosen name such as `clock.ccl`. Existing names are
  rejected, never truncated or overwritten. The name appears above the editor.
- Open lists the workspace's published `.ccl` files and loads the selected
  name. It refuses to discard unsaved source. Both dialogs require an inactive
  debugger session; they do not silently suspend a running CCL program.
  The loaded editor state is prepared separately before replacing the source;
  failed reads/validation leave the editor unchanged. A successful load resets
  undo history, cursors, breakpoints, and compiled/debug state.
- The picker supports arrows, Page Up/Down, Enter, Escape, Tab/Shift+Tab, mouse
  selection, double-click, wheel/scrollbar dragging, and the shared filename
  editor. Errors leave the dialog open and the document unchanged. Source and
  debugger shortcuts cannot leak through the modal. Modal updates repaint only
  its rectangle; closing restores the underlying Workbench.
- Listings are bounded to 64 files, names to 64 characters. Names begin with a
  letter/digit and otherwise use letters, digits, spaces, `.`, `_`, or `-`, end
  in `.ccl`, and cannot contain a path or trailing space. No folder navigation
  or authority expansion is offered in this first picker.
- Sources are at most 4096 bytes, using printable ASCII plus tab/CR/LF, matching
  the current editor's input model. Empty sources are supported.
- The workspace is selected on first use: `@nvme:0/work`, or `@mem:0/work` if
  the disk workspace cannot be opened. The chosen location is displayed;
  subsequent errors never silently switch an established workspace.

The initial scopes use today's path-profile mechanism. This is not yet a
trusted chooser, final installation-policy approval engine, general object-root
delegation, or a capability that evaluated CCL automatically inherits.

### Write protocol and limits

1. Enumerate the workspace. Published files populate the picker; numbered
   pending names also reserve revision numbers for automatic suggestions.
2. Reject an already-listed destination, then exclusively create its pending
   file (`clock.ccl.pending`, or the existing `ccl-0001.pending` naming). FS
   checks and creates within one
   serialized request; a collision fails rather than opening an existing file.
   Checked path lookup distinguishes absent names from malformed/unreadable
   metadata before exclusive creation proceeds.
3. Write the bounded snapshot, check the byte count, close, reopen, and compare
   the entire source plus EOF.
4. Publish using the non-overwriting rename operation. Only `.ccl` names are
   considered published. Abandoned pending files remain visible/recoverable;
   another save advances past them. Failed saves leave editor content intact.

Directory scanning is bounded to 64 pages. An incomplete/oversized listing
reports a limit instead of presenting a partial result as complete. Revision
suggestions stop at 9999 instead of wrapping; a distinct chosen name is still
possible. Two Workbenches can race for the same name: exclusive
creation rejects one, whose next manual retry rescans. There is no unbounded
retry loop or global latest-file pointer to update.

This prevents ordinary overwrite/race errors; it is **not** a power-loss-safe
transaction. Metadata allocation/creation is still legacy ext2 code, storage
flush/journal recovery is unfinished, and readback is not proof of durability.
The synchronous, bounded adapter is an initial Workbench boundary, not the
eventual asynchronous CCL file-I/O API. Move storage work off the UI event loop
when adding larger files and long-latency backends; report accepted operations
and cancellation semantics explicitly.

### Persistence and launchers

Revisions survive closing and reopening Workbench in the same guest session.
The Live CD's memory workspace is lost on reboot and is labeled accordingly.
The current `make -C kernel run-desktop` recreates `desktop_disk.img` from its
base image; it is not a persistent desktop launcher. Do not use that command
expecting previous guest edits to survive. The `run-desktop-fast` launcher also
uses a disposable copy, not a persistence solution. A non-destructive persistent
development-disk workflow remains a follow-up before relying on this for work.

Fresh NVMe and interactive desktop images receive a `work` directory. Existing
live-memory images already contain it. Tests modify only temporary image copies.

## Linux-hosted preview

The shared editor/compiler/debugger and dialog build against SDL. Open and Save
use a deterministic **Linux mock workspace (memory only)** seeded with
`hello.ccl` and `arithmetic.ccl`. Named saves survive dialog close/reopen, but
not process exit. The adapter never reads or writes host files.

Both adapters implement `CCL_Workspace.List_Files`, `Load`, `Save_New`, and
`Suggest_Name`, with bounded results and explicit errors. The previous automatic
Save/Load_Latest entry points are removed. Native implementation uses CuBit
filesystem requests, loans, and handles; Linux implements the same application
boundary in memory, not the wire protocol or kernel capability checks.

`CuBit.UI.File_Dialogs` is a shared toolkit component: it consumes a bounded
`CuBit.File_Selection.File_List` and produces submit/cancel actions. It has no
filesystem access itself. The host Workbench performs I/O, and evaluated CCL
does not inherit that authority. This is an in-scope picker, not a trusted
cross-process approval broker.

Hover hints remain in the status bar. The pointer-motion handler now compares
the previous hover target before updating coordinates; previously the old/new
comparison saw the same coordinates and skipped the hint repaint.

## Next: REPL sessions and typed UI bindings

Update: the session API and explicit host-call path are implemented; see
[interactive composition](ccl-interactive-composition.md) and the
[current label hooks](ccl-ui.md). The sequence below remains the broader roadmap,
not a claim that the general UI service or persistent environments already exist.

These are complementary front ends to the same CCL execution and authority
model, not two languages or a new Unix shell.

1. Add a bounded REPL session API: submitted source, typed result/diagnostic,
   fuel budget, explicit catalog/granted bindings, and reset/stop outcomes.
   Start with complete expressions and explicit history. Decide persistent
   definition/value lifetimes before advertising a stateful environment.
   The Workbench, a recovery console, and a remote typed management transport
   should use this same API; none requires stdin/stdout, a TTY, or user identity
   to imply authority.
2. Share analyser/compiler/VM and import-linking paths. Do not give REPL
   snippets the host Workbench's filesystem or desktop authority implicitly.
   Do not concatenate unbounded history and silently replay effectful commands
   to simulate persistent bindings. Pending noncancelable imports survive as
   explicit obligations rather than disappearing when the prompt is reset.
3. Implement the first small [CCL UI](ccl-ui.md) interface: a surface containing
   text and a button, a bounded batch to update text, and typed click/close
   events. A trusted UI host uses the existing toolkit internally. CCL sees
   typed handles, never native widget pointers or unrestricted callbacks.
   Surface ownership confers no clipboard, global-input, or cross-app authority.
4. Add a separately granted timer source and bounded event-handler dispatch.
   Build a clock widget using the existing clock interface and string formatting
   functions. Timer cadence belongs to the runtime/services, not a busy loop or
   hard-coded compiler feature. Retained widgets redraw only changed content.
5. Run that saved widget independently of the Workbench debugger, with visible
   authority, explicit ownership/lifetime, and a stop operation. An audio or
   network monitor follows once those typed interfaces exist.

The existing Workbench is a native application using shared toolkit routines;
it is **not yet evidence that CCL itself can create widgets over IPC**. The UI
document specifies the intended client/server boundary and batched protocol.
Native widget hosting and the Linux emulator still need implementation.

## Verification

```sh
nix develop -c make -C kernel filesystem ccl-workbench storage-check
nix develop -c make -C kernel ccl-workspace-test ccl-workspace-prove
nix develop -c bash tests/ccl-file-dialog/run-preview.sh
nix develop -c tests/headless/run.sh --test ccl-workspace --accel kvm --timeout 40
nix develop -c tests/headless/run.sh --test storage-grants --accel kvm --timeout 40
```

Hosted tests exhaust all 9999 revision numbers for pending/published names,
non-1-based strings, and malformed names. The name helper proves 20 checks with
no assumptions or SPARK-Off sections. This does not prove the IPC adapter,
filesystem, or persistence end to end.

The dedicated guest test sends real QEMU keyboard events to save `41`, edit to
`42`, attempt Open while dirty, save again, select/reopen, and save `clock.ccl`.
An attempted overwrite is rejected. It checks native markers and all three
stored files after QEMU stops. The storage test additionally rejects
exclusive creation over an existing file and creates a fresh file exclusively.
Existing GPU-flipping smoke assertions remain in their separate test.

The new model passes 18 GNATprove checks without assumptions or SPARK-Off
sections. This is not a proof of the complete dialog or IPC adapter. Hosted
tests cover names, bounded capacity, overwrite rejection, keyboard/mouse modal
interaction, scrollbar dragging, and source round-trips. The SDL-driven
Workbench regression captures actual Open/Save/loaded frames and checks that
hover changes the status bar but not source pixels, and modal typing does not
modify the document. Its synthetic events stay at the SDL2 application boundary
(not SDL2-compat's SDL3 text queue). Native picker test log:
`/tmp/cubit-file-dialog-final.serial` (final API-cleanup rerun passed).

Final validation passed native and Linux builds, the dedicated workspace test,
the unchanged GPU-flipping smoke test, and storage-grants. An earlier graphics
run missed the page-flipping startup marker despite completing workspace I/O;
the later graphics rerun passed. That intermittent marker failure has not been
diagnosed or fixed by this storage change.

Logs: `/tmp/cubit-ccl-workspace-final-build.log`,
`/tmp/cubit-ccl-workspace-proof.log`, and
`/tmp/cubit-ccl-workspace-final-guest.log`. Guest logs use
`/tmp/cubit-ccl-workspace-final.serial`, `/tmp/cubit-ccl-workspace-gpu.serial`,
and `/tmp/cubit-ccl-exclusive-final.serial`.
