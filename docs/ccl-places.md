# CCL places: `here`, `fs.list`, `fs.watch`, and gestures as CCL

Status (2026-10-02): steps 1 and 3 are implemented; watch, the typed clipboard and gestures are next. See "Order of work" and "Landed".

## Landed

- **Filesystem service:**
  - `OP_READ_DIRECTORY_INSPECTED` (0x13) returns, in one request, a `Directory.Page.V1` followed by a page of 64-byte `Entry_Inspection` records: size, the modified, created and accessed times, mode, links and owner, each with a valid bit.
  - For ext2 the service fills them from each entry's inode, and fixes the listing's missing sizes. CPIO fills the size only.
  - The existing page request, and everything that parses it (libc `getdents`, Files, the shell), are unchanged.
- **CCL:**
  - `interfaces/fs.schema` provides `File_Kind`, `Place`, `Child`, `File_Metadata` and `Listing`, and the operations `fs.home`, `fs.enter`, `fs.up` and `fs.list`. They are installed in the shared host environment, so the console and the Workbench both have them.
  - `CCL_Places` has a native body (the filesystem service) and a Linux-preview body (host files).
  - The native body lists over the filesystem queue (`CuBit.Filesystem_Queues`, `Queue_Read_Directory_Inspected` = 14), not one IPC per request. It lends a queue and a 64 KiB arena once. Each listing is a submit and a reap, with a kick only when the service's wake word is armed.
  - The metadata fields are typed by unit: `size : Bytes`, `modified : Timestamp`, `mode : UNIX_File_Permissions`. Front ends humanize them through `CCL.Units` (and the Observatory's matching `units.js`): "1.2 KiB", "2026-10-02 20:46", "drwxr-xr-x". The value stays a number for sorting and arithmetic. The name `UNIX_File_Permissions` is deliberate: ext2/3 is a stop-gap, and a native filesystem's rights will not be Unix modes.
- **First cut: a `Place` is data, `(Place root path)`, not yet a handle.**
  - It grants nothing; the service checks the process's manifest scope on every request.
  - `fs.enter` takes one name component (no separators, `.` or `..`), and `fs.up` never goes above the root.
  - Directory-handle capabilities stay the upgrade path for narrower delegation.
- **Console:** `:cd`, `:cd name`, `:cd ..` and `:ls` write the CCL they mean into the transcript. `(fs.list here)` presents as a sortable table.
- **Limit:** one listing carries at most 31 entries (one result image). Larger directories come next, as pages or a stream.
- **Verified:**
  - Hosted console tests (75 checks) cover `:cd`, `:ls`, entering, going up, the root boundary and refused names. All 29 hosted suites pass.
  - The native console, Workbench, ccl-control and filesystem build.
  - `CCL.Interfaces.Files` and `CCL.Interfaces.Console` prove at level 1 (0 unproved).
- **Verified on the guest:** `:cd` and `:ls` of `@nvme:0/work` over the queue, with humanized metadata (the 2026-10-02 demo, `CCL_CONSOLE_DEMO=1` in `tests/headless/run.sh`).
- **Failures explain themselves:** a refused call says which call, why, and what would allow it (`CuBit.Failures`; see `ccl-console.md`). A path outside the scope names the `filesystem-scope` line the manifest needs.

## Why not a current working directory

A Unix process has an ambient current directory: a string every relative path silently depends on, inherited by every child, and able to wander anywhere the process may reach. CuBit has no ambient authority anywhere else, and the console should not reintroduce it.

A **place** is a value instead: a directory handle the session holds, granted by the filesystem service within the console's manifest scope.
- Commands take a place as an explicit argument: `(fs.list here)`.
- A child place can be derived for free (`fs.enter`), and is read-only unless the parent grants more. Going up, or out of the scope, needs a place the session already holds or a new grant. A cell can never reach `/` by accident.
- `here` is an ordinary session binding. The console shows it in the header and completes names against it, and `:cd name` is sugar for `(define here (fs.enter here "name"))`. It feels like a current directory without being ambient.
- This is the filesystem's existing model, surfaced in CCL. `Directory_Handle` and `Open_Child_Directory` already behave this way: a single name component, no `..`, no symlinks, and policy re-checked.

## What exists today (filesystem service)

| Need | Today | Gap |
| --- | --- | --- |
| A directory handle | `OP_OPEN_DIRECTORY`, `OP_OPEN_CHILD_DIRECTORY` (generation-tagged, owner-bound) | none |
| A listing | `OP_READ_DIRECTORY_PAGE`, `Directory.Page.V1`: 14 entries of name, kind and inode hint per 4 KiB page | **ext2 leaves size empty**; no times or permissions |
| Metadata (stat) | none. libc fakes it by opening the file | **a metadata op** (the ext2 inode has mode, uid, gid, link count, and accessed, created and modified times) |
| Change notification | none. The namespace generation word is only a staleness hint | **watch: a right, an event ring, bounded loss** |
| Authority | per-process scope prefixes and rights (`CuBit.File_Access`), from the manifest's `filesystem-scope`, installed by procmgr | a scope verb for watch (`docs/filesystem-maturity.md`, "watch") |

## Filesystem service work

1. **Listings with metadata: `Directory.Page.V2`.**
   - A page request may ask for metadata. Each entry then carries:
     - its kind and size;
     - its modified, created and accessed times (the volume's seconds, widened to milliseconds);
     - its permission bits as stored;
     - its link count.
   - The service loads each entry's inode through the block cache. Twelve entries fit per page.
   - One request answers a page's worth, so listing a large directory costs a few IPCs, not one per file.
   - CPIO and ISO 9660 fill what they have, and say which fields are valid (a valid-fields word).
2. **Inspect one object:** `OP_INSPECT_CHILD` (directory handle, name), returning the same metadata for one entry. File panes and drag previews need it.
3. **Watch: `OP_WATCH_DIRECTORY`.** This is the first stream source that delivers through a shared ring.
   - Setup is control plane: an IPC request on a directory handle that holds the watch right. The client lends a page, and that page holds a `CuBit.Slot_Rings` ring of fixed-size `File_Event` entries.
   - Each event carries its kind (created, removed, renamed from, renamed to, contents changed, metadata changed), the entry's name, its kind, and a sequence number.
   - The service is the producer. It signals only when the client armed its wake word. When the ring is full it counts lost events instead of blocking, so a watcher can never stall the filesystem.
   - Events never name anything outside the watched directory. Closing the handle, or the watcher's process exiting, ends the watch.
4. **Runtime client package `CuBit.Places`.**
   - Typed Ada wrappers: open a scope root, enter a child, list pages with metadata, inspect, watch.
   - The CCL host, the Files app and later libc's `stat` share it. Today's builders are called by hand from three places.

## CCL surface

Types live in `interfaces/fs.schema`:
```
File_Kind = File | Directory | Link | Other
Rights = (read: Boolean, write: Boolean, execute: Boolean)
File_Metadata = (name: String, kind: File_Kind, size: Integer, modified: Integer, created: Integer, accessed: Integer, mode: Integer, links: Integer)
File_Change = Created | Removed | Renamed_From | Renamed_To | Contents | Metadata
File_Event = (change: File_Change, name: String, kind: File_Kind, sequence: Integer)
```

`Place` is a session handle like `Stream<T>`: opaque, held in the session's place table, and generation-checked. A kept binding reads back as `(place n)`, which names only a place this session already holds.

| Operation | Type | Notes |
| --- | --- | --- |
| `(fs.place "work")` | `String -> Place` | a scope root from the manifest, by its name |
| `(fs.enter p "name")` | `Place -> Place` | a child: one name component, read-only unless the parent allows more |
| `(fs.list p)` | `List<File_Metadata>` | presents as a table: sort by any column, Ctrl+click filters |
| `(fs.inspect p "name")` | `File_Metadata` | |
| `(fs.watch p)` | `Stream<File_Event>` | lossy with a counted gap; `lost` reports it |
| `(fs.move p "name" q)`, `(fs.remove p "name")`, `(fs.make p "name")` | `Boolean` | write and create rights on the places involved |

**Live listings.**
- `(define changes (fs.watch here))`, then `:watch` on `(fs.list here)`. The listing reruns when a change arrives, because live cells already rerun on stream arrivals.
- Later, dependency marking narrows reruns to the cells that read that stream ([streams phase 1](ccl-streams.md#not-yet)).

## Gestures are CCL

Every direct manipulation is a CCL expression. It shows in the transcript before it runs, so it is visible, replayable, scriptable and checked like typed code.
- **Drag a `File_Metadata` row onto a place:** `(fs.move here "notes.txt" archive)`. It needs write rights, and it can be undone because it is recorded.
- **Double-click a value:** open it with an application that declares its type in its manifest (an image editor declares `Image`). This is a typed launch, the same mechanism as launching programs with typed arguments.
- **Copy a cell:** the clipboard holds the typed value: its schema key and image, plus the Lisp and BASIC text for text-only targets. Pasting gives back the value.
  - **Authority never travels through the clipboard.** Copying a place, a stream or any handle copies a description. Pasting it into another session must obtain the handle again under that session's own grants.
  - **A live cell's bytecode state can be cloned:** the VM's machine state is bounded and verified, so a suspended run can be snapshotted and resumed elsewhere, under the same rule for anything it holds.
- **Between windows:** drag and drop and the desktop clipboard are shared desktop work (the graphics and browser agents). They come after the console's own copy and paste, through the usual coordination.

## Order of work

1. **Service:** `Directory.Page.V2` with metadata, and `OP_INSPECT_CHILD`, with hosted ext2 tests and proofs of the page encoder.
2. **`CuBit.Places`:** the client package, with the CCL workspace moved onto it.
3. **CCL:** `fs.schema`, `Place` handles, `fs.place`, `fs.enter`, `fs.list` and `fs.inspect`, in both engines; the console's `here` and `:cd`; table presentation.
4. **Service:** `OP_WATCH_DIRECTORY` and its event ring, then `fs.watch` as a stream source fed from that ring.
5. **Console:** in-process copy and paste of typed values.
6. **With desktop coordination:** the desktop clipboard, drag and drop, and open-with.

## Proof and test obligations

- **Page V2 encoder and decoder:** no field outside the entry; a valid-fields word that matches what was filled; names bounded and never containing separators.
- **Watch ring:** the `Slot_Rings` instance (already proved), the producer's admission of the client's consumed index, and loss counted rather than overwriting unconsumed events.
- **Places:** a child never widens rights; a closed or replaced handle never resolves (generation); `(place n)` resolves only within its own session.
- **Native:** the headless console lists `@mem:0/work` with metadata, creates a file through the shell or Files app, and a watch-driven live cell picks up the change.
