# CCL console

The CCL console (`ccl-console.app`) is a full-window desktop REPL. It does
everything CCL does: evaluation, definitions, host services such as logs,
config, clock and execution, and workspace files. It is built to make a
text terminal unnecessary. It is not a TTY emulator. Every entry is typed,
and every result is a value you can act on.

The console and the CCL Observatory web UI are two front ends to one model.
Whatever one can show (types, provenance, links, tables, charts), the other
should be able to show the same way.

## Status

**Slice 1 is in place:**

- **The app:**
  - Native app: `userspace/ccl/apps/ccl-console`, with Makefile target `ccl-console`.
  - Linux preview: `build/ccl-ui-preview/ccl-console-preview` from `userspace/ccl/ccl_ui_preview.gpr`.
- **Typing:**
  - Highlighting: syntax colours, plus parentheses coloured by depth, from the shared SPARK classifier `CCL.Highlighting`.
  - Multi-line input: Enter runs a complete form and continues an open one on a new, indented line. Shift+Enter always breaks the line. Ctrl+Enter (or F5) runs regardless.
  - Matching-parenthesis boxes.
  - A status line saying what Enter will do, with line, column and length.
- **Completion:**
  - It covers host operations, special forms and built-ins. It opens as you type a name in operator position.
  - Ghost text shows the rest of the selected name; Tab or Enter accepts it.
  - While you type arguments, the call's signature is shown.
- **The transcript, as cards:**
  - the source, highlighted;
  - the result's value;
  - a type badge;
  - elapsed time and fuel used;
  - a green or red status bar;
  - a squiggle under a diagnostic's position.
- **Tables:** a record, or a list of records (such as `logs.recent`), renders as a table.
  - Field names head the columns; numbers align right.
  - Rows are zebra-striped; at most 20 are shown, then "+N more rows".
  - Hovering a cell shows `field : Type = value`; clicking inserts that value.
  - The row shape (`CCL.Types.Shapes`) is computed where the result is produced, by the VM.
  - `CCL.Literal_Tables` (SPARK) splits the canonical literal into cells, so the Observatory can use the same cells.
- **Pictures:** an `Image` result is drawn inline.
  - It is enlarged by a whole-number factor while it fits, otherwise reduced, with a caption giving its size.
  - Clicking a picture inserts its value.
- **The web Observatory, in step:**
  - Its REPL transcript renders the same `CCL.Presentations` description over the wire: operation 7, with image rows on operation 8 (`userspace/ccl/remote/README.md`).
  - It uses the same cards, type badges, cost line, tables (header click sorts, Ctrl+click filters, by writing the same CCL), pictures, and Enter/Shift+Enter behaviour.
  - Its highlighter (`highlight.js`) is checked mark for mark against `CCL.Highlighting` by golden vectors (`tests/ccl-console/vectors.adb`).
  - Its wire decoder is checked against bytes the native encoder writes (`tests/ccl-remote/wire_vectors.adb`).
- **Live cells:**
  - `:watch N` makes the newest entry re-run every N seconds (1 when omitted, at most one hour) and update in place.
  - A green LIVE mark shows the interval and the run count; clicking it, or `:unwatch`, stops the cell.
  - Only a plain expression re-runs (`CCL.Sessions.Reevaluate_With_Values`): a live cell never defines, names a value or changes the history order.
  - The next run is timed from the previous run's completion, so a slow cell never queues up runs. At most four cells are live at once.
  - With images this gives live charts: `(image.plot ...)` over changing data.
  - The Observatory has `:watch` too, with one live cell.
    - The cell runs inside CuBit, in the lab guest's single native periodic slot (every second, 4096 fuel).
    - The page observes its presentation with operation 9 (tables and pictures included) and shows the same LIVE mark. A click or `:unwatch` stops it.
- **Pointer:**
  - Click a past source to edit it again, or a past result to insert its value at the caret.
  - Click an example to try it.
  - Hover a service name for its signature.
- **Commands:** `:env`, `:reset`, and the Workbench's `:files`, `:save` and `:load`. These are shared through `CCL_REPL_Commands`.
- **Authority:** the same as the Workbench, through the shared `CCL_Host_Environment`. procmgr approves the console as a log viewer like the Workbench.

**Tests:**

- `tests/ccl-console`, hosted: drives the view with real events and renders frames to PPM for review.
- `tests/headless/run.sh --test ccl-console`, native: types a multi-line form, accepts a completion with Tab, and queries `logs.recent` on CuBit.

## Images

`interfaces/image.schema` declares `Image = (width: Integer, height: Integer, id: Integer)`.
- An Image is an ordinary typed value. Its `id` is the content digest of pixels kept in the process's bounded image store (`CCL.Image_Store`).
- So images copy, compare by content, live in session values, and cross the wire like any value.
- The id names pixels; it is not a capability. It grants nothing, because it can only name pixels the same process produced.
- When the store is full, the least recently used image is replaced. A replaced id shows as *expired*, never as other pixels.

The `image` interface draws data. It needs no service and no authority, and is installed in every CCL host environment (console, Workbench, ccl-control):

| Operation | Argument | Result |
| --- | --- | --- |
| `image.plot` | `Series` (List Integer) | 320 x 120 line chart |
| `image.bars` | `Series` | 320 x 120 bar chart, negatives in red |
| `image.heatmap` | `(Grid width height values)` | width x height, values coloured cold to hot |
| `image.pixels` | `(Grid width height values)` | width x height, values as 16#RRGGBB# |
| `image.gradient` | `(Size width height)` | a test pattern |
| `image.stack` | `Images` (List Image) | the images top to bottom |
| `image.beside` | `Images` | the images left to right |
| `image.scale` | `(Scaled image factor)` | the image enlarged by a whole factor, up to 16 |
| `image.load` | a file name in the workspace | the picture in a QOI or binary PPM (P6) file |

`image.load` reads only from the host's authorized workspace (`@nvme:0/work` or `@mem:0/work`), through the same grant as `:load`, with Observe authority.
- The decoder (`CCL.Image_Formats`, SPARK) works a byte at a time, in bounded memory.
- It accepts exactly one well-formed image: any trailing byte is an error. QOI alpha is composited over black.
- A failed decode stores nothing.
- `tests/ccl-console/make-picture.py` writes QOI test pictures using every chunk kind.

```lisp
(image.load "picture.qoi")
(image.plot (list 3 1 4 1 5 9 2 6))
(image.heatmap (Grid 15 15 (each (fn ((i Integer)) (* (mod i 15) (/ i 15))) (range 0 224))))
```

Composition keeps images immutable: each operation makes a new image from stored pixels, and an expired or mis-sized input is refused. Pictures larger than one call's data can be built in pieces. This Mandelbrot set is computed in CCL itself, using integer fixed point with the iteration state packed into one Integer, as five 32 x 7 strips:

```lisp
(define (escape (cx Integer) (cy Integer)) Integer ...)   ; tests/ccl-console/main.adb
(define s1 (image.heatmap (Grid 32 7 (each (fn ((i Integer)) (escape ...)) (range 0 223)))))
...
(image.scale (Scaled (image.stack (list s1 s2 s3 s4 s5)) 7))
```

**Current bounds.**
- Arguments cross the host boundary as typed object images of at most 256 cells, so a grid holds at most about 250 values.
- Generated pictures are therefore small and shown enlarged.
- An evaluation's value arena (512 records) and list storage (4096 elements) are not reclaimed until it ends.
  - A computation that builds a record or a range per step runs out of space; the fractal packs its state and uses 12 iterations for this reason.
  - Freeing a call's temporaries when it returns, in the VM, is the next memory-model step.
- Screenshots and GPU output as images come next.

## Structure

- `CCL_Console_View` owns presentation only. It is platform-free: native, the Linux preview and tests drive the same state with the same events. Evaluation, history and the session environment stay in `CCL.Sessions`.
- Each frame records the regions it drew: source, result, service name, type badge, example, input and suggestion.
  - Pointer handling and tooltips use these regions.
  - So does `Region (State, Kind, Index)`, which lets tests and automation act on what a person sees.
- `CCL_Desktop_Platform` is the window boundary shared with the Workbench. It has typed `Window_Event`s and takes each app's name and title.

## Next

1. **Richer tables.**
   - Sort by a column with a click, generating the `sort-by` CCL into the input.
   - Expand past 20 rows.
   - Severity colours, and source links for `logs.recent`.
   - Drill into a nested record or list cell.
2. **Presentation protocol and provenance.**
   - One typed description of a cell: type, value, which service produced it, and under what grant.
   - The console and the Observatory both render from it.
3. **Links.** A service name, a process or a capability in any result is a link that opens its own typed view (signature, logs, grants). This feeds the capability graph planned after VM parity.
4. **Charts.** Numeric lists and metric streams render inline as sparklines and charts, live while subscribed.
5. **Live cells.** An entry can stay subscribed (`logs.tail`, metrics) and update in place under a fuel and rate budget.
6. **Retire `shell.app`** once the console covers its commands.


## The console as an object: console.* (2026-10-02)

The console publishes its own typed interface, defined in `interfaces/console.schema`, and grants it only in the console process. Cells program the console like any other object:

| Call | Type | Effect |
| --- | --- | --- |
| `(console.title "…")` | `String -> Boolean` | sets the window title |
| `(console.notation Notation.Basic)` | `Notation -> Notation` | rewrites every cell and the input in BASIC, or back in Lisp |
| `(console.theme Theme.Daylight)` | `Theme -> Theme` | switches the palette: Midnight or Daylight |
| `(console.stats)` | `Console_Stats` | cells, live cells, live runs, the newest and slowest cell's time, open streams |

`(field (console.stats) live_runs)` is ordinary data, so a live cell can watch the console's own latency.

**Notation:**
- F8, `:lisp`, `:basic` and `console.notation` all switch it.
- Cells are converted, not re-run. A cell that uses the session's definitions is rendered in their context (`CCL.Sessions.View_Source`), so `(twice 21)` reads `twice(21)`.
- BASIC typed in BASIC mode runs as BASIC: the console adds the `#!ccl basic` marker the session reads, and hides it in cells.

**Tested:** the hosted console tests drive all four endpoints from cells, check the BASIC and Lisp renderings both ways, and write `console-basic` and `console-daylight` frames. The native build passes. A native demo on CuBit is scripted in `tests/headless/run.sh`, but is currently blocked by desktop key loss under other agents' in-flight changes.


## Failures that explain themselves (2026-10-02)

On a locked-down CuBit system, a newcomer's first surprise is a refusal. Each refusal says which call failed, why, and what would change it:

```
! fs.list reaches outside what this program was granted: @nvme:0/etc is outside this
  program's filesystem scope. To allow it: the program's manifest must declare
  (filesystem-scope (rights read) "@nvme:0/etc")
! fs.enter rejected its argument: "../etc" is not one entry's name (no '/', '\', '.' or
  '..'). Instead: enter one level at a time, and use fs.up for the parent
! logs.recent found nothing there: no running service is named "netstak". Instead:
  name a running service, or give its process number
```

**How it works:**
- **`CuBit.Failures`** is in the shared runtime, so any program can use it. It is a pure SPARK package and proves at level 1.
  - It defines a `Reason` (`Not_Granted`, `Outside_Scope`, `Refused`, `Not_Found`, `Invalid_Argument`, `Unavailable`, `Exhausted`, `Device_Error`) and a bounded `Failure` record: Why, a Detail, and a Remedy.
  - `Explain` writes the sentence. A grant question ends "To allow it: …"; anything else ends "Instead: …".
- **Host bindings** return a `Failure` in `Call_Result.Why`. The `fs.*`, `logs.recent`, `timer.every`, `clock.monotonic-ms` and `image.load` bindings fill it, and workspace storage results map through `CCL_Workspace.Failure_Of`.
- **Evaluation** (`CCL.Evaluation`) records the failing operation's name and the failure (`Interpretation_Result.Failed_Operation`/`Failure`), and sessions render them in the cell.
- **Refused before running:** a program that names an operation its grants lack is refused before anything runs, and the message says so: "this program's grants do not include it, so nothing was run". The remedy names the manifest request and the system grant it needs.

**Tested:** the hosted console tests check the refused-name and missing-directory messages word for word.

**Not yet covered:** the native scope refusal from the filesystem service, which needs a guest run.

## What runs: :ps and proc.list (2026-10-02)

`:ps` writes `(proc.list)`: every process as typed data, a `List<Process>`.

```lisp
(type Run_State (enum Ready Running Sleeping Waiting Waiting_Event Sending Receiving
                      Waiting_Reply Waiting_Completion Suspended Futex_Waiting))
(type Process (record (pid Integer) (name String) (identity String) (state Run_State)
                      (memory Bytes) (launcher Integer) (age Milliseconds)))
```

- **The table is ordinary data:** sort it, filter it and watch it, for example `(sort-by (fn ((p Process)) (- 0 (field p memory))) (proc.list))`. `memory` is a `Bytes`, so it shows as KiB/MiB.
- **The types are CCL source** (`CCL.Interfaces.Processes.TYPE_SOURCE`), checked by the CCL type checker when the interface is published. There is no Ada description of them. `tests/ccl-console/check_interface_keys.py` keeps the schema keys equal to SHA-256 of that source.
- **Native (CuBit):** `native/ccl_processes.adb` asks procmgr through the process-observer role.
- **Linux preview:** reads the host's `/proc`. That is a Linux-hosted stand-in, not CuBit.
- **Limit:** one result holds 28 processes. Result literals now have their own 4 KiB buffer (`CCL.Language.Literal_Text`), so a full table fits. Host text values stay at 1 KiB.
- **Verified:** hosted console tests (`:ps`, filtering, sorting) and live on the guest: 23 processes from the kernel's table.

**Authority: procmgr's process-observer role (landed 2026-10-02):**
- The CCL Console and CCL Workbench manifests request `process-observer` (catalog role 26, fixed slot 29). procmgr grants it under the same narrow desktop-launched exception as `log-observer`, or at trusted startup. It is never ambient (`CuBit.Authority_Policy.Process_Observation`).
- **Protocol:** like `log-observer`, it is procmgr's own endpoint under a kernel-stamped tag (`CuBit.Process_Observer`). `List` (label `0x0107`) is answered only for that tag. The client lends one page, and procmgr writes 128-byte records: the kernel's table joined with each process's manifest identity, launcher and start time.
- **Without the role,** `proc.list` says so and names the manifest line. There is no fallback to an ambient query.
- **Fields:** `Process` has `pid name identity state memory launcher age`. Scheduler details were dropped so that a result holds 28 processes.
- **Recorded data:** identity and age are known for processes procmgr launched. Services devmgr started before procmgr show neither yet.
- **Still open:** the kernel's `SYSCALL_PROCLIST` accepts any `CAP_PROCESS` with read rights, and every process holds one for itself. It should require a system-wide read capability once shell and desktop move to the role (raised in coordination).

## Hints for the language's own words (2026-10-02)

Built-ins, special forms and operators describe themselves like host operations do (`CCL.Hints`, through `CCL.Completions.Describe`):

- **Status line:** shows the call being typed, e.g. `(each f xs) : (a -> b) (List a) -> (List b)  apply f to every element`.
- **Completion popup:** the detail row shows the selected candidate's hint.
- **Hover tooltips:** hovering a name in a past cell shows its hint.
- **The Observatory** gets the same text over wire operation 10.

Every built-in has a hint (tests/ccl-completion).
