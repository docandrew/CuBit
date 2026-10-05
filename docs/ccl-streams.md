# CCL streams and reactive cells (design)

Status: **design for review**, 2026-10-02. Nothing here is implemented.

This is the CCL side of the stream model in [typed IPC](typed-ipc.md#calls-and-streams-share-one-interface-model) and [stream wiring](stream-wiring.md). Those documents define what a stream contract must declare (shape, delivery, flow control, ownership, lifetime, budgets) and how connections are authorized. This one says how a stream appears in CCL, in a session, and in the console and the Observatory.

## What it is for

- **Live views:**
  - an oscilloscope over the mixer's output;
  - a load monitor over the web server's connections;
  - a log tail that updates as records arrive.
  - Each is one CCL expression and redraws when data arrives, not on a timer.
- **Launching programs:** typed arguments with completion and tooltips, and output streams that can be redirected while the program runs.
- **Applets:** a set of live cells with their streams and grants, torn off into a window.

The non-negotiables are the ones CCL already keeps:
- every evaluation is bounded (fuel and storage);
- authority is explicit and visible;
- the interpreter and the VM behave the same.

## Streams are session resources; evaluations see windows

The central decision: **an evaluation never waits on, or drains, a stream.** A stream is held by the session (the console tab, an applet). An evaluation sees only bounded views of what has arrived:

| Operation | Type | Meaning |
| --- | --- | --- |
| `(latest s)` | `T` | the newest element (a failure before the first) |
| `(window n s)` | `List<T>` | the newest `n` elements, oldest first; `n` is at most the stream's declared capacity |
| `(since s t)` | `List<T>` | elements delivered after time `t`, within capacity |
| `(arrived s)` | `Integer` | elements delivered since subscription |
| `(lost s)` | `Integer` | elements lost to overflow (the stream's gap count) |

So `(image.scope (window 1024 mic))` is an ordinary bounded expression: 1024 samples, one picture. There is no unbounded fold and no blocking read. Aggregation over time is always over an explicit window, as [composition](ccl-interactive-composition.md#typed-pipelines-proposed--surface-syntax) requires.

### Element types

`T` must be **persistable data**, the same rule (`CCL.Objects.Persistable`) that decides what crosses a host boundary. Data is scalars, strings, records, variants and lists of those.
- **Not allowed:** a stream (no `Stream<Stream<T>>`), a function, a handler or a resource handle. These are not persistable, so the type system already refuses them; streams need no special case.
- **To share another stream's data, send its elements,** or a derived stream. Never send the subscription itself.

### `Stream<T>` is a type

- **The type:** `Stream<T>` is a type form like `List<T>`, with an element type. It also carries the stream's declared profile:
  - its delivery policy, one of the variants of `CuBit.Protocols.Stream_Policies`: lossless, ordered with gaps, or latest value;
  - its capacity.
- **The value is a handle.** A stream value is a handle into the session's stream table: the stream, plus a generation.
  - Only an authorized operation opens one, such as `(logs.tail "netstack")`. Source may name a stream its own session already holds, `(stream T n)`; that grants nothing, and every read is checked against `T` (see [phase 1](#naming-a-stream-stream-t-n)).
  - It prints as a description, `#<Stream LogEntry logs.tail "netstack">`, never as a literal that reads back.
  - It works like an image id, except that an image id names data, while a stream handle names a live subscription the session holds.
- **Both engines read a stream the same way.** `latest`, `window`, `since`, `arrived` and `lost` read the session's buffer through one callback that both engines call.
  - Elements are copied in as values of `T`, exactly as host lists are today.
  - Bytecode never holds a stream's storage.

### Delivery into the session

- **Arrival:** elements arrive as async completions, on the path the console already pumps (`Poll_Completion` → `CCL_Native_Execution.Deliver`).
- **Buffering:** the session keeps each stream's newest elements in a ring of the declared capacity.
- **Overflow:** a lossy stream counts drops (`lost`); a lossless stream refuses before acceptance, as `Stream_Policies` requires.
- **Validation:** each element is validated against `T`'s pinned schema on arrival. The schema advertisement is never trusted as proof.

## Fuel: per element and per second, not per evaluation

A one-shot entry gets a fuel budget for one evaluation. A program fed by a stream is different: its work is proportional to what arrives. Its fuel is therefore **a rate**.
- **Per element:** each stream operation that processes arrivals (a derived stream's `where` or `each`, below) is charged per element. Its budget is fuel per element, or per byte for bulk streams such as audio.
- **Per second:** a reactive cell's reruns, and a derived stream's processing, also draw from a fuel-per-second allowance of the session or applet.
  - Overrunning it throttles the cell (shown on its card) and never delays input or the frame.
  - On a lossy stream, throttling becomes visible loss; on a lossless stream, it becomes backpressure on the source, never silent dropping.
- **The ceiling:** a live cell's cost is bounded by its source's rate times its per-element fuel, capped by the per-second allowance. The manifest's budget is that cap, so an applet's worst-case CPU use is stated before it runs.

## Stream cells: live history you can work with after the fact

- **A stream cell:** an entry whose value is a stream is shown as a **stream cell**, the stream's live history.
  - It is a scrolling list of elements with their types, bounded by a history limit: the stream's capacity, adjustable within the session's budget.
  - Its header shows the full type (`Stream<HttpResponse>`), the source, the delivery policy, and the counts arrived and lost.
- **Naming a cell:** every cell has a name you can use in later entries. The transcript numbers them (`%7`), and `(define name ...)` gives one a stable name.
- **Working after the fact:** a new entry can build on an earlier stream cell, using what has already arrived:

  ```lisp
  (logs.tail "netstack")                                  ; cell %7: the live tail
  (where (fn ((e LogEntry)) (= (field e severity) Severity.Error)) %7)   ; %8
  (window 20 %8)                                          ; the last 20 errors, as a table
  ```

  - Applied to a stream, `where`, `each` and `take` make a **derived stream**. It starts from the source's retained history, then follows it live.
  - It has its own history, its own type (`each` may change it) and its own per-element fuel.
  - You can filter or tail after the fact without re-running anything or losing what already went by. Retained history is bounded by capacity, and the card says how far back it reaches.

## Typed redirection

- **Every stream cell's header shows its type,** and every destination has one too: a file sink takes `Stream<String>` or `Stream<Bytes>`, an image processor's input takes `Stream<Image>`, and a program's inlet takes whatever its manifest declares.
- **A redirection must type-check before it is offered.** Dragging a stream cell's output highlights only compatible destinations. A `Stream<HttpResponse>` cannot be dropped on an image processor; the same `connect` typed as text fails with a type error naming both types.
- **Converting is an explicit, visible cell:** `(each (fn ((r HttpResponse)) (field r body)) %9)` gives a `Stream<String>`, which can then go to a file.
- **No silent conversions:** types are never coerced, and an adapter is never inserted unseen, as [stream wiring](stream-wiring.md) requires.

## Reactive cells

- **What makes a cell reactive:** a console entry (or applet cell) whose expression reads a stream, directly or through definitions, is **reactive**. The session's static analysis already knows which names an entry uses, so its dependencies are known.
- **When it reruns:**
  1. A delivery marks the stream changed.
  2. Every cell that depends on it is marked dirty.
  3. At the next frame, dirty cells re-run once each, in dependency order, coalescing however many deliveries arrived.
  4. Each run is an ordinary bounded evaluation with its own fuel.
- **Reactive definitions:**
  - `(define rate (/ (arrived conns) ...))` over a stream is a **derived signal**: the session recomputes it, rather than keeping a literal.
  - Cells that use `rate` depend on it in turn.
  - Cycles are rejected when defined (a feedback loop needs an explicit delayed boundary).
- **Budgets:**
  - The session has a per-frame budget (time and fuel) across all reactive cells.
  - A cell that overruns is marked *throttled* on its card and runs at a lower rate. It never delays input handling or the frame.
  - This is the input-to-photon priority applied to live cells.
- **Replaces `:watch`.** A timer is just a stream, `(timer.every 1000)` of type `Stream<Integer>`, so `:watch` becomes ordinary dependency on a timer stream and its special case goes away.

## Programs: typed arguments and live redirection

Following [program parameters](ccl-launch-parameters.md), with two additions.

1. **Subcommands are variants.** A program's parameter type may be a variant whose members carry their own records:

   ```lisp
   (type JjCommand (variant
     (Log (record (revisions String "@") (limit Integer 20)))
     (Diff (record (from String "@-") (to String "@") (stat Boolean false)))))
   (parameters (command JjCommand))
   ```

   - Completion and the signature tooltip come from the manifest, so `(launch jj (Diff :stat true))` offers `:from`, `:to` and `:stat` only after `Diff`.
   - A flag from another subcommand is a type error before anything runs.
   - Argv rendering is declared in the manifest, so unmodified programs (jj, other Rust or C tools) receive ordinary argv while the console works with types.
2. **A launch returns a process** whose inlets and outlets, any number declared in its
   manifest, are reached through the program's own accessors
   (docs/ccl-launch-parameters.md, "Inlets and outlets, not stdio"), each by its fully
   qualified name: `(jj.unix.stdout p)` of type `Stream<String>` (one
   element per line, a declared maximum line length), and typed inlets and outlets such
   as `(jj.com.cubit.stdlog p)` as `Stream<LogRecord>`. CuBit has no stdout
   or stderr; `unix.*` inlets and outlets are a ported program's legacy convention.
   - Every outlet gets a card: a live transcript of that outlet.
   - `(where ... (jj.unix.stdout p))` is a live filter.
   - `(table (window 50 (jj.com.cubit.stdlog p)))` is a live table.

**Redirection is wiring**, with [stream wiring](stream-wiring.md)'s three approvals and prepare, commit and retire. `(connect (jj.unix.stdout p) (file "out.txt"))` and `(connect (jj.unix.stdout p) (tool.input q))` are explicit, authorized edges.
- Re-pointing one while the program runs moves the binding to a new generation at a record boundary, with any gap visible.
- In the console an edge is visible: a card shows where its stream goes, and dragging changes it, through the same authorized operation.

## Safe and visible

- **Opening a stream is an authorized operation,** like any call. Discovering one is not a grant.
- **The authority bar:** every open stream appears in the session's authority bar with its source, rights and buffer budget. Closing the cell, or the tab, closes the subscription.
- **What a card shows:** the stream's identity, its delivery policy and its loss count. "Ordered with gaps: 3 lost" is shown, never hidden.
- **Applets:** an applet's manifest lists its streams and grants. Tearing a card off makes that list explicit, for approval once.

## First sources

| Source | Type | Delivery | Work needed |
| --- | --- | --- | --- |
| `(timer.every ms)` | `Stream<Integer>` | latest value | done (phase 1) |
| `(logs.tail "service")` | `Stream<LogEntry>` | ordered with gaps | logstore push delivery through a completion (today `Read_Next` polls) |
| `(net.connections port)` | `Stream<Connection>` | latest-value snapshots, plus event deltas | netstack listing and per-connection counters (none exist today) |
| `(mixer.output)` | `Stream<Samples>` | ordered with gaps, bulk | a mixer tap (`Audio.Tap` is designed in [audio graph](audio-graph.md), not built) |
| `(jj.unix.stdout p)` (any declared outlet) | `Stream<String>` | ordered with gaps | process launch with typed parameters, then reading that outlet's console-owned ring |

## Bulk streams: 4K/30 video on a set-top box (direction)

The target: `(video.play (net.stream "…"))` over a `Stream<Compressed_Frame>`, playing live at 3840×2160 and 30 frames a second on a set-top box running CuBit. Phase 1 cannot do this, and the design must not paint us into a corner.

### The budget

| Stage | Per frame | Per second |
| --- | --- | --- |
| Compressed (HEVC or AV1, 15–40 Mbit/s) | 60–170 KB on average; keyframes 1–2 MB | 2–5 MB |
| Decoded (NV12, 3840×2160) | 12.4 MB | about 370 MB |
| Display deadline | 33.3 ms per frame, vblank-paced | 30 presents |

### Control plane and data plane, as everywhere else in CuBit

CCL streams follow the split the rest of the system already uses ([async rings](async-rings.md), [filesystem data plane](filesystem-data-plane.md), [audio zero-copy](audio-zero-copy.md)): IPC with CCL objects coordinates, and shared-memory rings carry the data. Nothing here is a CCL-only mechanism.

- **Control plane: CCL values over IPC.**
  - Opening a stream, wiring endpoints, choosing a profile and reading counters are authority-bearing requests, typed as CCL values and admitted like any call.
  - A stream handle, a buffer lease and an endpoint are generation-checked handles. They resolve only in their owner's table, as ring handles do.
- **Data plane: the proved ring primitives.**
  - A stream's elements travel in a `CuBit.Slot_Rings` ring of fixed-size entries, shared by producer and consumer. Request/response sources use a `CuBit.Submission_Queues` pair.
  - The ring is set up by one control-plane request, and its memory is the owner's own grant.
  - Signals are sent only when the other side armed a wake, so a steady stream costs no IPC per element.
- **An element is a slot entry.**
  - The entry is fixed-size metadata. For video that is the codec, the presentation and decode timestamps, the size, keyframe or not, and the payload, named as (registered buffer, offset, length) within a pool lent when the ring was set up. Never a pointer.
  - The consumer checks every reference against the registered extent.
- **What a CCL evaluation sees** follows the rings' "snapshot only what decisions depend on" rule:
  - The metadata an evaluation reads is copied in and validated against `T` once. Phase 1's `Capture_Local` copy-in is already that step.
  - The payload is never copied into an evaluation. CCL code holds a **lease**: a read-only, resource-shaped value naming the slot's buffer. It can pass the lease to an endpoint that accepts it, but cannot read the bytes.
  - So `(window 30 frames)` lists sizes and timestamps, and the 255-cell image limit never touches payload.
- **Wiring moves the data; evaluation does not.**
  - `video.play` asks, through the [stream-wiring](stream-wiring.md) approvals, to connect typed endpoints: network demux, then decoder, then compositor plane.
  - The type check is the safety: a `Stream<Compressed_Frame>` of HEVC goes only to an endpoint that declares HEVC.
  - Once wired, the endpoints share rings directly. About 30 entries a second for video, batched per frame, with no evaluation and no CCL-side copy per frame.
- **The same path serves small streams.**
  - Phase 1's session table is the consumer half of this design, with the ring held locally.
  - The next sources should deliver into a slot ring shared with the session rather than one IPC per element: `logs.tail` (from logstore), `net.connections` (snapshots from the netstack), and the mixer tap. The table then reads the ring instead of being pushed to.

### Consequences

3. **Decode in hardware, scan out without copying.**
   - The decoder is the media engine: Intel's VDBox through the i915 driver (the graphics agent's), or a set-top SoC's VPU. It writes NV12 into GPU-visible buffers.
   - The compositor puts those buffers on an overlay plane, with no composition pass and no CPU copy.
   - Software decode at 4K/30 is not a fallback a set-top CPU can afford.
4. **The clock is the audio clock.**
   - Presentation follows timestamps against the mixer's clock, for audio/video sync. Frames are paced to vblank.
   - The jitter buffer is the stream's declared capacity: 1–2 s of frames, about 4–10 MB of leases, sized in the manifest.
5. **Scheduling:** the receive, demux, decode and present chain runs under real-time grants by capability ([scheduler](scheduler.md)). A missed deadline is a counted `lost` frame, never a stall in the console.
6. **Visibility:**
   - Every stage publishes its own `Stream<Frame_Stats>`: arrival rate, bytes a second, decode latency, presented, late and dropped. A console cell can plot them live while the video plays.
   - Per-byte fuel never applies, because CCL code never touches payload.

### What it needs, in order

- **CCL:** a lease type (the resource family, read-only, pool-backed) and bulk stream profiles, where elements are metadata plus a lease and the capacity is in bytes. Phase 5's wiring and redirection.
- **Network:** bulk receive straight into the lease pool, with no copy out of the netstack.
- **Decode:** media-engine support in the i915 driver: HEVC first, then AV1.
- **Display:** NV12 overlay planes, and vblank-timed presentation in the compositor.
- **Audio:** the mixer clock as the presentation master.
- **Proof of the path:**
  - the same pipeline at 1080p30 under QEMU with software decode, a Linux-hosted demonstration only;
  - then 4K30 on real hardware with the media engine.
  Both counted frame by frame by the `Frame_Stats` streams.


## Phases

1. **Stream type and session table** (with the element-type rule and the fuel model):
   - the `Stream<T>` type form;
   - handles, the five window operations and the session's stream table, in the interpreter and the VM together, with proofs;
   - `timer.every` as the first source.
   - Stream cells with history, `%N` cell references, and derived streams (`where`, `each`, `take`).
   - Reactive cells and the frame budget in the console then replace `:watch`.
2. **Log tail:** logstore push delivery and `logs.tail`.
3. **Network:** `net.connections` and counters in the netstack; the web-server load monitor demo.
4. **Mixer tap:** after coordinating with whoever claims audio; the oscilloscope demo.
5. **Programs:** typed launch arguments (with variants), `launch`, every declared outlet as a stream, and redirection by authorized wiring.
6. **Observatory:** a stream wire profile, so the browser observes the same live cells. The guest runs them; the browser renders.
7. **Applets:** live cells torn off into windows.

jj is the flagship for phase 5. It also needs file writes in the Unix std and its git backend on CuBit; both are tracked separately.

## Phase 1: what landed (2026-10-02)

Status: implemented in both engines.
- **Proved (GNATprove):**
  - level 1: `CCL_Stream_Table`, `CCL.Interfaces.Timer` and `CCL.Types` (all of it);
  - level 2: every changed line of `CCL.Language`, `CCL.VM`, `CCL.VM.Native_Objects`, `CCL.Host_Values` and `CCL.Scheduler`.
- **Regression-tested:**
  - hosted: the suites below and all 29 CCL suites;
  - native: devmgr, the console and the Workbench build, and the headless `ccl-console` test passes on a CuBit guest. That test does not exercise streams yet.
- **Mutation-checked:** the tests catch three mutants: a dropped object-slot release, ignored handle generations, and an off-by-one VM window bound. A fourth, skipping element validation, behaves the same as the original, because the following load refuses an unvalidated snapshot with the same status.

### The type and the value

- **`(Stream T)`** is a type form. `CCL.Types.Specialize_Stream` defines `Stream-T` and refuses elements that are not persistable data: no streams of streams, functions, handlers or resources.
- **Where a stream may not go:** record fields and variant payloads (diagnostic `Stream_Not_Data`), and lists (`Unsupported_List_Element`).
- **What it carries:** the wire form (`Wire_Stream`, `SHAPE_STREAM`) carries the type.
- **The value is a handle:** an Integer the session's table issues, carrying the stream type.
  - The type checker already refuses arithmetic, comparison and `to-string` on it.
  - In the VM, the verifier's exact `Data_Type` matching refuses the same, so bytecode cannot treat a handle as a number either.

### Naming a stream: `(stream T n)`

Change from the design above: source *can* name a stream, but only one its session already holds.
- **What it means:** `(stream T n)` refers to stream `n` of this session's table, like a file descriptor.
- **Why it is safe:**
  - It grants nothing: opening a stream is the authorized step.
  - The table checks the handle's generation, so a closed stream's number never reads its slot's next stream.
  - Every element read is validated against `T` before use (`Stream_Element_Mismatch` otherwise).
- **How a session keeps a binding:** `(define ticks (timer.every 100))` is kept as `(stream Integer 1)`.
- **The later `%N` cell references** will be sugar for the same form.
- **A result prints as a description,** `#<stream 1>`, typed `Stream<Integer>`. `#` starts a comment, so it cannot be typed back in.

### Views, in both engines

- **The built-ins:**
  - `(latest s)` : `T`, failing with `Stream_Empty` before the first element;
  - `(window n s)` : `List<T>`, the newest n elements, oldest first;
  - `(arrived s)` and `(lost s)` : `Integer`.
- **Window size:** a window is at most 255 elements, one image: its count cell, then the elements.
- **One reader contract,** `CCL.Streams.View_Request` and `View_Reply`. The reply's elements are an image laid out as the evaluation's own `T` or `List<T>`, validated with `Capture_Local` before use.
- **Interpreter:**
  - `Interpret_With_Values` and the session generics gain a `Read_Stream` formal. It defaults to a null reader, so existing hosts are unchanged.
  - A scalar element frees its object slot at once, so a cell may read `latest` any number of times.
- **VM:**
  - `Push_Stream` (53) pushes a handle.
  - `Stream_View` (54) suspends with `Waiting_For_Host` and `Stream_Requested`. The suspension is separate from import waits, so the import lifecycle and `Is_Well_Formed` are untouched.
  - `Native_Objects.Complete_Stream_View` answers it.
  - New statuses: `Stream_Unavailable`, `Stream_Empty`, `Stream_Window_Out_Of_Range` and `Stream_Element_Mismatch`.
  - Scheduled isolates have no session, so a stream view fails them.

### Sources and the session table

- **Contract:** a host import may declare `Result_Stream`, meaning its result is a stream of its declared result kind or schema, and its reply is the handle.
  - The VM types the bare handle as the import's declared stream type.
- **The first source is `(timer.every ms)`**, `interfaces/timer.ccl-interface`: a `Stream<Integer>` of tick times, 10 ms to 1 h apart.
  - It is named `timer`, not `clock`, because `clock` is the clock service's pinned interface.
- **The table:** `CCL_Stream_Table`, in the shared `CCL_Host_Environment`, so every front end has it.
  - Up to 16 streams. Each history is a 256-slot `CuBit.Slot_Rings` ring, the proved generic of the [async rings](async-rings.md) data planes. The table is the ring's producer and, when it is full, consumes the oldest element itself and counts it lost. A window still shows at most 255 elements.
  - Hosted builds get the ring generics through `userspace/ccl/cubit_rings.gpr`; native builds get them from the runtime library.
  - Handles carry a generation.
  - A host that falls more than a ring behind (because it was suspended) skips ahead rather than replaying.
- **Closing what nothing holds:** after every entry and live run, the console closes each stream no kept value holds (`CCL.Sessions.Holds_Stream`, `Retain_Streams`). A bare `(timer.every 100)` or a rebound name does not leak a slot.

### Observatory remote sessions

The wire protocol is now version 2. A request is `[2, id, session, op, …]`, where the session is a random 64-bit id the browser keeps for each tab (`sessionStorage`); 0 asks for a throwaway session.
- **On the guest:** ccl-control keeps four session slots, replacing the least recently used. Each slot has its own `CCL.Sessions` session and stream table, so definitions, kept values and `timer.every` streams persist between a tab's entries.
- **Live cells:** the monitor (`:watch`) runs the tab's expression in that session's environment (`CCL.Sessions.Expression_Program`).
- **Completion** includes the tab's own definitions.
- **Not a credential.** The session id separates tabs; every tab has the guest's same grants. Client certificates or a login come later.
- **Verified on a CuBit guest:** the real-guest smoke binds `timer.every` in one request, reads its window and latest value in later ones, and checks that another tab cannot see it.

### Live cells

The console pumps the table every frame. When an element arrives, every live cell (`:watch`) reruns once at the next refresh, however many elements arrived. A slow cell never queues runs.

### Not yet

- **Live cells:**
  - dependency marking: today every live cell reruns on any arrival;
  - fuel per element and per second: today a view costs one step.
- **Stream cells:** history and `%N` references; derived streams (`where`, `each`, `take`).
- **Closures:** a lambda cannot capture a stream yet. Closures capture scalars and enumerations only, the same limit records have.
- **Record elements:** records arriving on streams (phase 2, `logs.tail`) are not supported yet.
- **A live demo on the guest,** the console's live cells reading `timer.every` (the headless test does not script it yet).

### Tests

- `tests/ccl-streams/main.adb`: 62 checks, each expression run by the interpreter and as verified bytecode against one fake table:
  - every view;
  - Strings and records;
  - streams through functions and conditionals;
  - every typed failure in both engines;
  - opacity, and the definition-site refusals.
- `tests/ccl-streams/session_tests.adb`:
  - the table: rings, loss, catch-up, generations, `Retain`, a full table;
  - a session with the real `timer.every` contract: binding, reading, composing, refusing arithmetic, and letting go on rebind.
- **The core suite** (`make ccl-test-native`) gained 7 type-form checks.

## Open questions

1. **Window operations as built-ins or host operations?** Built-ins (`latest`, `window`) read naturally and type-check over any `Stream<T>`. Host operations would need one per element type. This design takes built-ins, reading through the session callback.
2. **Should a derived signal be kept across sessions** (in a saved workspace), or re-derived when the session opens? Re-deriving is simpler and safer.
3. **Default capacity** for a stream opened in the console: 256 elements for records, and 1 second of samples for audio, both overridable within the manifest's budget.
