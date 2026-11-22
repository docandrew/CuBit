# Audio graph: from mixer.svc to a capability-scoped real-time graph

Status: design (2026-09-29). Nothing in this document is implemented except
what "Starting point" lists as implemented. Sections mark each item
**implemented**, **designed** (this document or a cited one settles it and it
is the plan) or **speculative** (a direction, not a commitment).

Related documents:
- [audio-zero-copy.md](audio-zero-copy.md): the current period path;
- [audio-volume-control.md](audio-volume-control.md): stream and master gain;
- [scheduler.md](scheduler.md): REALTIME class, `CAP_SCHEDULING`, admission;
- [stream-wiring.md](stream-wiring.md): authority to use versus authority to wire;
- [security-model.md](security-model.md), [agent-security.md](agent-security.md);
- [ccl-ui.md](ccl-ui.md), [control-language.md](control-language.md);
- [input-latency.md](input-latency.md): measurement boundaries.

## Goals

1. **Power.** Any graph a user or an authorized agent can describe (mixing,
   routing, effects, capture, monitoring) is expressible as typed CCL and can
   be changed while audio runs.
2. **Protection.** No ambient audio authority. Playing, capturing, tapping,
   linking, loading an effect and real-time CPU are separate capabilities.
   A faulty or hostile node costs at most its own output and its admitted
   budget.
3. **Performance.** A 64-frame quantum at 48 kHz (1.33 ms) with no underruns
   under CPU, storage and network load. Larger quanta for power saving.
4. **Visibility.** The live graph, its timing and who hears what are
   queryable, traced and explainable, as scheduler.md §8 requires of the
   scheduler.

## Starting point (implemented)

- `hda.drv` (`userspace/services/hda/`) owns the controller, the BDL and one
  DMA page holding four 1 KiB PCM periods (`PCM_PERIOD_BYTES`,
  `NUM_BDL_ENTRIES` in `hda.ads`). Each period is 256 stereo S16LE frames,
  5.33 ms at 48 kHz. It grants only that page to the mixer and sends a one-way
  `AUDIO_PERIOD_COMPLETE` with a sequence number after each period interrupt.
- `mixer.svc` (`userspace/services/mixer/`) holds up to `MAX_STREAMS` = 8
  client streams. Each is a two-page SPSC ring (64-byte header, 8128 data
  bytes, 2032 frames). On each completion it sums the running output streams
  into a 32-bit `MixBuffer`, applies stream and master gain, clamps, and writes
  the DMA period directly (`Mixer.mixPeriod`). It blocks otherwise.
- Clients use `CuBit.Audio` (`userspace/runtime/gnat/cubit-audio.ads`):
  `reserveWrite`/`commitWrite` render in place; `notify` wakes the mixer. A
  client is never woken per period; it keeps its ring filled.
- Only output, 48 kHz, stereo S16LE is supported. HDA capture is not
  implemented, although `Mixer.DIRECTION_INPUT` exists.
- The mixer calls `setLatencyContract (LATENCY_REALTIME, 5_000, 1_500)`. The
  contract is not yet enforced. Its 5,000 µs period also does not match the
  5,333 µs device period, which matters once EDF uses it (see Timing).
- `Realtime_Admission` (`kernel/src/realtime_admission.ads`, in the working
  tree, hosted tests in `tests/realtime-admission`) is the proved admission
  arithmetic. `CAP_SCHEDULING` does not yet exist in `capabilities.ads`.
- `bench-audio` (`userspace/apps/bench-audio`, `tests/headless/run.sh`) drives
  a synthetic producer at queue targets of 2032, 1024, 512 and 256 frames and
  reports underruns, `audio-mix-and-copy` and
  `audio-driver-publication-to-mixer`. One QEMU run
  (`tests/performance/results/audio-sweep.json`) measured mix p99 ≤ 5.4 µs and
  publication-to-mixer p99 ≤ 43 µs, with zero underruns at every target. These
  are QEMU figures, not hardware.

The structure is already the kernel of a graph: a device-clocked cycle, a
single executor and zero-copy into the device period. It lacks a graph model,
a policy boundary, capture, per-cycle clients and enforced real-time budgets.

## Prior art

All of these are summaries from public documentation. Details marked
*approximate* may be wrong in specifics.

**PipeWire.** One daemon holds a graph of nodes with ports joined by links. A
*driver* node, usually a device, starts each cycle. Each node has a shared
activation record with a pending-input counter; a node that finishes
decrements its dependants' counters and wakes the ones that reach zero
through an eventfd, so the daemon is not in every hop. Clients are nodes in
their own processes. Buffers are shared memory (memfd or dmabuf). The
*quantum* is the cycle length in frames; it is chosen from the nodes'
requested latencies within configured bounds (defaults of about 1024 frames,
minimum about 32; approximate). Policy (which device, who links to what,
permissions) is a separate session manager, WirePlumber, scripted in Lua.
Real-time threads come from RTKit or rlimits.

**JACK.** A synchronous graph for pro audio. Every client's process callback
runs once per period in dependency order, in its own process. A client that
does not finish in time causes an xrun. A client that repeatedly overruns may
be disconnected ("zombified"; approximate). There is no policy layer: any
client can connect any ports.

**macOS coreaudiod.** The HAL owns devices and their clocks. Clients register
IOProcs that run on a time-constraint real-time thread *in the client
process*. The HAL wakes them ahead of the device position from a clock model
rather than per interrupt (approximate). Third-party device drivers run as
audio server plug-ins; Audio Unit v3 effects can run out of process.

**Windows Audio Engine.** `audiodg.exe` mixes in shared mode and hosts APO
effect plugins (stream, mode and endpoint effects) from driver vendors inside
that isolated process. Clients use WASAPI, event-driven or exclusive mode,
with smaller shared-mode periods on newer APIs (approximate). MMCSS gives
registered "Pro Audio" threads real-time priority for most of each period.

What CuBit takes:
- PipeWire's graph shape: nodes, ports, links, a driver-clocked cycle, client
  nodes in their own processes, shared buffers per port, a quantum;
- PipeWire's split of the data-plane engine from the policy manager;
- JACK's synchronous cycle and the rule that one late client may not stall
  the others;
- coreaudiod's real-time work in the client's own thread, with the device
  clock as the timebase;
- the Windows lesson that third-party effects belong outside the engine.

What CuBit changes:
- **Authority.** PipeWire's permissions and JACK's open graph become
  capabilities per operation, enforced by the engine and the kernel, not by a
  scriptable manager with full access (stream-wiring.md).
- **Real-time CPU is admitted, not requested.** RTKit and MMCSS hand out
  priority with rate limits. CuBit admits a budget per node under a proved
  system-wide cap and falls back to NORMAL on overrun (scheduler.md §5).
- **Effects are isolated per plugin**, not all in one `audiodg` process.
- **Built-in DSP is proved** free of run-time errors and overflow.
- **Visibility is part of the interface**, not a debugging tool: the graph,
  timing and listeners are typed CCL values under an observe capability.

## Architecture (designed)

```text
                      CCL policy, UI, agents
                               |
                  audio-policy.svc  (routing, devices, permissions)
                               |  validated graph transactions
                               v
  client node ---+                                 +--> hda.drv period
  (app process)  |       audio engine              |    (DMA, zero copy)
  plugin node ---+--> (graph executor, built-in --+
  (sandbox)      |     DSP, one RT thread per      |
  hda capture ---+     driver clock)               +--> taps (authorized)
```

### Engine (evolution of mixer.svc)

- Executes the graph once per cycle for each driver clock. Holds the port
  buffers, runs built-in DSP, wakes out-of-process nodes, enforces their
  deadlines and writes the device period.
- Decides nothing about policy. It accepts graph changes only as
  transactions from a holder of the engine's control capability, normally
  audio-policy.svc, and validates each one itself: acyclic, formats
  compatible, buffers within bounds, each node's authority present.
- Performs no allocation, discovery, file access or logging in the cycle
  (audio-zero-copy.md, "Scheduling"). Node slots, port buffers and link
  tables are preallocated with static bounds.
- Owns one REALTIME thread per driver clock. With one HDA device that is one
  thread, as today.

### Policy service (audio-policy.svc)

- Holds the authority to create nodes and links, select devices and grant
  playback, capture and tap handles. It has no data-plane access: it never
  maps a sample buffer.
- Configured in CCL: default routes, per-application rules, device
  preferences, which packages may request capture, quantum limits.
- Implements stream-wiring.md's three approvals for each link: the
  controller's authority to rewire, release of the source's audio to that
  recipient, and the destination's acceptance.
- Replaces the ownership and admission code now in `mixer.svc`
  (`Mixer_Control`, stream open and close). Being out of the cycle, it may
  block, ask the user and call Config.

### Device nodes (hda.drv)

- A playback device node is a sink with one input port per channel group; a
  capture device node is a source. The driver keeps the controller, BDL,
  CORB/RIRB and MMIO, and grants only PCM period pages, as today.
- The driver node is the **clock**: its period completion starts a cycle. It
  reports its period size, periods queued and codec latency (when the codec
  reports it; otherwise a documented estimate).
- Capture is designed the same way: HDA grants the capture period page
  read-only to the engine and signals each completed period.

### Client nodes (applications)

Two modes, chosen when the node is created:

- **Buffered** (today's model). The app fills its SPSC ring when it likes; the
  engine reads one quantum per cycle. Needs no real-time authority. Latency
  is the ring fill plus the graph latency. Games, media players and browsers
  use this.
- **Cycle-synchronous** (JACK and CoreAudio style). The app's audio thread is
  woken each cycle, renders exactly one quantum into its port buffer and
  signals done. Needs an admitted REALTIME budget. Instruments, DAWs and
  monitoring use this.

Capture clients are the mirror image: the engine writes into a ring (buffered)
or an input port buffer (cycle-synchronous).

### Effect nodes

- **Built-in DSP**, inside the engine, in SPARK: mix, gain and pan with ramps,
  EQ (biquad cascades), sample-rate conversion, channel mapping, peak and RMS
  meters, and an output limiter. They run in the engine's thread with no IPC
  cost and are proved (see "Proof and test targets").
- **Third-party plugins**, each a separate sandboxed process that is a
  cycle-synchronous node. It holds its port buffers, a parameter page and a
  REALTIME budget, and nothing else: no filesystem, network, device or other
  audio. A plugin instance is one process, so a crash or overrun is confined
  to it.
- Whether a built-in DSP block can later be supplied as proved, bounded code
  loaded into the engine (like sched_ext, scheduler.md §7) is speculative.

## Data plane

### Buffers per port (designed)

- The engine allocates every port buffer. A buffer holds one quantum, or the
  largest quantum the node admits.
- Mapping follows direction. A node's output buffers are mapped read-write in
  the node and read-only in the engine. Its input buffers are mapped
  read-only in the node and read-write in the engine. A node cannot write a
  buffer another node reads, or read one it was not linked to.
- Output buffers are double-buffered by cycle parity. Cycle *n* uses buffer
  *n mod 2*, so a node that is late on cycle *n* never writes a buffer the
  engine reads in cycle *n+1*.
- Samples from out-of-process nodes are untrusted data. Any bit pattern is
  valid for integer formats. For float ports the engine replaces NaN and
  infinity with zero and clamps to a bounded range at the port boundary, with
  a counter.
- Engine-internal format: 32-bit integer with headroom, as `MixSample` is
  today, so SPARK can prove the arithmetic. Plugin ports may declare float32,
  converted at the boundary. The choice of internal format is an open
  question.

### Zero copy to the device (implemented, kept)

The final stage writes directly into the DMA period that hda.drv granted.
Buffered clients are read directly from their rings. The irreducible traffic
per cycle is reading each input once and writing each output once.
Out-of-process nodes add one read and one write per port per cycle and no
other copy.

### Per-cycle protocol for out-of-process nodes (designed)

```text
driver IRQ ─> hda.drv: ack, AUDIO_PERIOD_COMPLETE(seq)
engine (cycle n, deadline = period end):
  1. run built-in DSP that feeds out-of-process nodes
  2. for each ready out-of-process node: write cycle header
     (n, frames, deadline), send one-way WAKE(n)
  3. meanwhile, run built-in DSP that does not depend on them
node: WAKE(n) ─> read inputs, write outputs[n mod 2] ─> DONE(n)
engine: at DONE(n), or at the node's cut-off time, whichever comes first,
  4. consume the node's output (or its miss policy)
  5. run the remaining DSP, write the DMA period
```

- WAKE and DONE are one-way capability notifications without payload, like
  today's period message. The header is in a shared control page: the engine
  writes it, and the node only reads it.
- The first version is a star: every hop passes through the engine. A chain
  of two plugins costs two round trips per cycle. PipeWire-style activation
  counters, where a node wakes its dependant directly, save those hops. They
  also let one node trigger another early, so they are deferred until a
  benchmark shows the star is the bottleneck (open question 2).
- The engine waits with a timeout: a blocking receive bounded by the next
  node cut-off. It never polls.

### Missed deadlines (designed)

The engine never waits past a node's cut-off, and the graph never stalls.

- **Miss policy per node**, declared in its package and selectable by policy:
  - `silence`: default for clients and sources;
  - `bypass`: default for effects with matching input and output formats. The
    engine passes the dry input through;
  - `fade`: a short ramp to silence, to avoid a click after a loud signal.

  Hold-last-buffer is not offered: repeating a quantum is audible as buzz.
- Switching to or from the miss output crossfades over one quantum.
- A DONE(n) that arrives after the cut-off is ignored, and the output of
  cycle *n* is never read. A DONE with a sequence other than the current
  cycle's is ignored and counted.
- **Counters per node:** cycles, misses, late DONE messages, sanitized
  samples, and consecutive misses. They are visible in the graph (see
  "Visibility").
- **Escalation:** after a configured number of consecutive misses, policy
  quarantines the node: bypass, then unlink, and tell the owning UI. A node
  process that dies is handled the same way, through its process-exit
  notification. Its buffers are retired only after the engine no longer
  references them (stream-wiring.md, "prepare/commit/retire").

## Timing

### Device-driven cycle (implemented at 256 frames, designed below that)

- A cycle starts when the driver clock's period completes; there is no timer
  polling. This keeps the graph locked to the device crystal, not the TSC.
- A graph with two devices on independent clocks has two driver clocks. Links
  between them go through an adaptive resampler that is driven by measured
  drift. That design is not settled (open question 5).
- Timer-scheduled cycles, which wake at a predicted position from the DMA
  position register instead of every interrupt, reduce interrupt rate at
  small quanta. CoreAudio and PipeWire's timer mode do this (approximate).
  Speculative for CuBit; measure the IRQ path first.

### Quantum and period sizes (designed)

| Mode | Quantum | Cycle | Periods queued | Use |
|---|---|---|---|---|
| pro | 64 frames | 1.33 ms | 2 | instruments, monitoring |
| low latency | 128 frames | 2.67 ms | 2 | games, calls |
| default | 256 frames | 5.33 ms | 4 | today's setting |
| power saving | 1024 frames | 21.3 ms | 2–4 | media playback on battery |

- The quantum is chosen by policy from the smallest latency any active node
  requests, bounded by configured limits and by what the device supports.
- HDA BDL entries have alignment and size constraints; 64 stereo S16 frames
  is 256 bytes. That this is valid on real controllers must be confirmed.
- Changing the quantum is a graph transaction. It takes effect at a cycle
  boundary, with every node's buffer and budget re-admitted first, and is
  refused if admission fails.
- With no running output the device stops, as `hasRunningOutput` does today.

### REALTIME mapping (designed; depends on scheduler.md §5)

- Each thread that runs in the cycle, the engine's and each
  cycle-synchronous node's, holds an admitted REALTIME contract with
  **period = cycle length**. Its EDF key is the end of the current period.
- **Budget per node** is its declared worst-case work per quantum plus a
  margin, admitted through `CAP_SCHEDULING`. The `Scheduling_Budgets` ledger
  limits any budget to half its period, so at 64 frames no thread may reserve
  more than 666 µs per cycle.
- **Sub-deadlines.** Out-of-process nodes must finish before the engine's
  final stage, so their effective deadline is earlier than the period end.
  EDF by period end alone orders them correctly relative to NORMAL work but
  not relative to the engine's tail. The scheduler needs a constrained
  deadline (deadline < period) in the contract. This is an addition to
  scheduler.md.
- **Release by event, not by wall clock.** `Scheduling_Budgets` aligns
  periods to the monotonic microsecond clock, but cycles follow the device
  crystal. Two cycles can fall in one ledger window, so a node doing exactly
  its budget each cycle could be cut off. The contract should either release
  a period on the cycle's wake (a sporadic task with a minimum
  inter-arrival time) or admit a budget with room for two cycles per window.
  Open question 3.
- The mixer's current 5,000 µs period against the 5,333 µs device period is
  the same mismatch in miniature, and is fixed in plan step 1.
- Beyond its budget a node runs NORMAL until its next period (ISO fallback).
  For audio that nearly always means a miss, which the engine absorbs.

### Admission when a node joins (designed)

When a node joins, policy checks, in order:

1. The node's authority: port capabilities and, for cycle-synchronous nodes,
   a `CAP_SCHEDULING` that covers its budget.
2. Kernel admission of the budget under `Realtime_Admission`'s 70% cap.
3. Graph feasibility for the driver clock: along each path, the sum of the
   out-of-process nodes' budgets, the engine's own stages and a fixed margin
   must fit in one quantum. This is the SPARK-proved `Cycle_Admission`
   (see "Proof and test targets").
4. Buffer memory within the engine's static bounds.

Failure at any step leaves the graph unchanged and returns a typed reason,
such as "quantum 64 needs 1.4 ms on path mic → fuzz → speakers". Policy may
then offer the next larger quantum.

### Latency accounting (designed)

Each path from a source to a sink reports its latency in frames and
nanoseconds, split by component:

- the client ring fill (buffered clients, measured each cycle);
- graph quanta between source and sink (1 for a straight path);
- the declared latency of each plugin (lookahead);
- resampler delay;
- device periods queued ahead of the DMA position;
- codec and converter latency, reported by the driver or estimated.

Round trip for a capture-to-playback path is the sum of both halves. The
figure is computed from the graph and the device state, then checked against
loopback measurement (see "Proof and test targets"). A reported number that
the measurement contradicts is a bug.

## Protection (designed)

### Capabilities

| Authority | Scope | Grants |
|---|---|---|
| `Audio.Play` | one client output node | write its own buffer; linked by policy |
| `Audio.Capture` | one source device or input group | read that source's samples |
| `Audio.Tap` | one named node's output | read another node's audio (monitor, loopback, recording) |
| `Audio.Link` | a set of nodes | create and remove links among them |
| `Audio.Insert_Effect` | one path position | insert an installed plugin there |
| `Audio.Control` | own nodes, or master | gain, mute, pan, parameters |
| `Audio.Observe` | own nodes, or system-wide | read the graph, timing and listeners |
| `CAP_SCHEDULING` | a budget and period | REALTIME admission for a thread |

- There is no ambient access. An app with `Audio.Play` cannot list other
  nodes, read their audio or change routing. Tapping the whole output mix is
  `Audio.Tap` on the device sink node, and it is shown as such.
- `Audio.Observe` system-wide reveals which apps are running and who is
  listening. It is a separate, explicit authority (security-model.md,
  "Security observability protocol").
- Installing a plugin package is package authority (ccl-packages.md).
  Inserting it into a path is `Audio.Insert_Effect`. Holding one does not
  imply the other.
- Real-time CPU is `CAP_SCHEDULING` only. devmgr mints it for hda.drv and the
  engine (scheduler.md §5). For nodes, policy needs a pool it can divide:
  derived capabilities with a smaller budget, revocable, each counted against
  the parent. scheduler.md does not yet describe derivation (open question 4).

### What the policy service may grant

- Without asking: `Audio.Play` to a manifest that requests it, linked to the
  default sink; `Audio.Control` over the app's own nodes; `Audio.Observe` of
  its own nodes.
- With a CCL policy rule or user approval: `Audio.Capture`, `Audio.Tap`,
  cycle-synchronous mode with a `CAP_SCHEDULING` budget, and
  `Audio.Insert_Effect`.
- To configured holders only: `Audio.Link` over nodes it names (a patchbay
  app, the Workbench) and system-wide `Audio.Observe`.
- Never more than its own ceiling. A grant carries the process generation,
  the node identity and an expiry, and becomes invalid when either end exits.
- Every active capture and tap is shown by the desktop in an indicator that
  the capturing app cannot draw over. The indicator reads from the engine's
  own listener table, not from policy's intentions.

## Visibility (designed)

- **The graph as CCL values.** An `audio.graph` interface descriptor
  (ccl-interface-descriptors.md) gives typed snapshots and events:
  - nodes: kind, package identity and digest, process and generation, mode,
    miss policy, declared and admitted budget;
  - ports and their formats; links; driver clocks and quantum;
  - timing per node: cycles, cycle-time histogram (p50, p99, max), misses,
    late DONEs, sanitized samples, budget used;
  - the listener table: every capture and tap, and who holds it;
  - latency per path, split as in "Latency accounting".

  Snapshots carry a generation; events carry sequence numbers, so a
  subscriber detects loss and requests a new snapshot.
- **Workbench view:** the live graph with meters on each link, per-node
  timing bars across one cycle, the listener list and path latencies. It uses
  the same interface as scripts, so the view shows nothing a script with the
  same authority could not query.
- **Trace events** in the kernel trace ring (scheduler.md §8), on the
  scheduler's TSC timebase: period IRQ, engine wake, node WAKE, node DONE,
  node miss, engine stage start and end, and DMA period written. Together
  with dispatch and preemption records, one exported window explains a miss:
  whether the node was late to run, ran too long or was preempted.
- Serial output never runs in the cycle. The existing `Clock.Report`
  histograms move into `audio.graph`.

## CCL and agents

### Typed interfaces

Nodes, ports, links and parameters are typed CCL values. Operations such as
`audio.link`, `audio.insert` and `audio.set-parameter` are typed host
operations that require the capabilities above. The same operations back the
settings UI, the patchbay, scripts and agents (stream-wiring.md, "Application
contract").

### Example: "write me a guitar fuzz plugin with pedal knobs and a signal graph"

The agent runs in a mission (agent-security.md) with a sandboxed build tool,
`Audio.Observe` scoped to the user's current path, and the right to *propose*
an install and an insert. It holds no `Audio.Link`, no `Audio.Insert_Effect`
and no `CAP_SCHEDULING`.

1. **Generate.** The agent writes an effect-node package:
   - the DSP source (SPARK preferred);
   - a declaration of ports, parameters, latency, budget and miss policy;
   - a CCL UI with knobs bound to the parameters and a graph widget bound to
     the live graph.
2. **Build and check in the sandbox:**
   - compile;
   - run GNATprove if the DSP is SPARK;
   - render test signals offline (silence, sine sweep, full-scale square,
     impulses) and record the peak output, any NaN, the latency measured
     against the declared latency, and the worst-case time per quantum.
3. **Review.** The proposal shows:
   - what the plugin can do: its ports, and that it requests no filesystem,
     network, device or other-audio authority;
   - the budget it asks for at the current quantum;
   - proof and test results, with unproved checks listed;
   - the source digest;
   - where it will be inserted.
4. **Approve.** There are two use-once approvals (agent-security.md,
   "Propose, approve, and commit"): `Install<AudioPlugin>` bound to the
   package digest, and `Insert<Effect>` bound to that digest and one path
   position. The agent cannot approve its own proposal.
5. **Hot insert** (prepare/commit/retire):
   - prepare: policy starts the plugin process, admits its budget, maps its
     buffers and runs it in shadow for some cycles. It processes a copy of
     the real input; its output is discarded and its timing measured;
   - commit: at a cycle boundary, crossfade from dry to wet over one quantum;
   - removal: the same in reverse.
6. **Contain.** Crash, overrun or miss means bypass with a crossfade, a
   counter and quarantine after repeats. NaN or out-of-range samples are
   sanitized and counted. The output limiter on the device sink bounds
   level. The UI runs in the CCL host, not the DSP process; a UI failure does
   not touch audio, and a DSP failure leaves the UI showing "bypassed".

Parameters reach the plugin through a parameter page. The UI host writes it
through `Audio.Control` on that node only; the plugin reads it once per
cycle and smooths values over the declared time. Parameters are data: a
knob cannot relink, capture or load anything.

### Illustrative CCL

**Illustrative only.** None of this syntax exists. It follows the Lisp form
of `executable-manifest` (for example `userspace/apps/bench-audio/manifest.ccl`)
and ccl-ui.md's declarative widget model. `knob` and `audio-graph-view` are
widgets ccl-ui.md does not yet list.

```lisp
# Illustrative: an effect-node package declaration.
(audio-node-package v1
  (identity "local.agent.fuzz")
  (version "0.1.0")
  (kind effect)
  (executable "fuzz.node")
  (ports
    (input  in  (audio (channels 1) (rate 48000) (format f32)))
    (output out (audio (channels 1) (rate 48000) (format f32))))
  (parameters
    (drive (range 0.0 1.0) (default 0.6) (smoothing-ms 5))
    (tone  (range 0.0 1.0) (default 0.5) (smoothing-ms 5))
    (level (decibels -24.0 6.0) (default 0.0) (smoothing-ms 5))
    (bypass boolean (default false)))
  (latency-frames 0)
  (realtime (quantum-frames 64) (budget-us 60))
  (on-miss bypass)
  (requests))  # No authority beyond its ports, parameters and budget.
```

```lisp
# Illustrative: the plugin's UI. Knobs bind to the node's parameters
# (Audio.Control on this node). The graph view binds to audio.graph,
# limited to this node and its immediate neighbours (scoped Audio.Observe).
(ui.surface "Fuzz"
  (column
    (row
      (knob "Drive" (bind fuzz.drive))
      (knob "Tone"  (bind fuzz.tone))
      (knob "Level" (bind fuzz.level))
      (toggle "Bypass" (bind fuzz.bypass)))
    (audio-graph-view (bind (audio.neighbourhood fuzz 1))
                      (show meters timing misses))
    (text (bind (audio.path-latency-text fuzz)))))
```

```lisp
# Illustrative: the agent's mission.
(mission write-fuzz-plugin
  (using agent sound-assistant)
  (tool build.sandbox)
  (observe audio.graph (scope current-path))
  (permit propose (install audio-plugin))
  (permit propose (insert effect (path "guitar-in" "speakers")))
  (deny external-network)
  (expires-after (minutes 30)))
```

## Proof and test targets

### Proved (SPARK, level 1, hosted like `Scheduling_Budgets`)

- **Graph model** (`Audio_Graph`): every link joins an output port to an
  input port of a compatible format; the graph stays acyclic under every
  accepted transaction; the execution order is a topological order; a rejected
  transaction leaves the graph unchanged; node and link counts stay within
  their static bounds.
- **Buffer bounds:** every port access is within its buffer for any admitted
  quantum; ring reserve, commit, peek and release keep the SPSC indices
  consistent. This extends the current `CuBit.Audio` checks.
- **DSP arithmetic:** mix, gain ramp, pan, clamp, EQ in fixed point, meters
  and the limiter are free of overflow for all inputs, with headroom stated as
  a precondition on the number of mixed inputs. Output is within range.
- **Cycle bookkeeping:** a node's output is consumed only for the current
  cycle's sequence and only before its cut-off; buffer parity means a late
  node never writes a buffer being read; each cycle ends with exactly one
  device period written.
- **Admission:** `Cycle_Admission` accepts a graph only if each path's
  budgets fit in the quantum; with `Realtime_Admission`, total admitted real
  time stays under the cap; release returns exactly what was admitted.
- **Authority checks:** the engine's transaction validator accepts a link,
  tap or capture only with the matching capability record (as
  `Stream_Connections.Check` does for wiring).

Not proved: the kernel's enforcement of budgets on real hardware, device
behaviour, DMA without an IOMMU (audio-zero-copy.md), plugin DSP that is not
SPARK, or audible click-freedom.

### Tested

- `bench-audio` at quanta of 256, 128 and 64 frames, under each load from
  scheduler.md's benchmark: none, CPU burn on every CPU, fs-bench, net-bench,
  a wake/sleep spammer and a polling service. Pass: zero underruns at the
  device over the run.
- 64-frame periods with all CPUs busy and four cycle-synchronous test nodes:
  zero device underruns. Node misses are allowed only for a node made to
  overrun deliberately.
- p99 IRQ-to-engine latency under 50 µs, measured from the period interrupt
  to the start of the engine stage (today's `audio-driver-publication-to-mixer`
  boundary plus the IRQ path). QEMU and hardware reported separately.
- Fault injection: a node that sleeps, spins, crashes, returns NaN or sends
  forged DONE sequences. Pass: device output continues, counters match,
  quarantine happens after the configured count.
- Capability tests in `capability-security` style: an app without
  `Audio.Tap` cannot read another stream, forged tags are rejected, and
  handles die with the process.
- Round-trip latency on real hardware (the N95 reference machine) with a
  loopback cable: the measured value agrees with the reported path latency.
- A/B against the previous commit for every engine change (mix time,
  IRQ-to-engine, underruns), as the kernel-change verification rules require.

## Incremental plan

Each step ships separately and has a measurable result.

1. **Admitted real time for the existing mixer.** `CAP_SCHEDULING` minted by
   devmgr for hda.drv and mixer.svc; `setLatencyContract` REALTIME admitted
   through `Realtime_Admission`; the contract period corrected to the device
   period (5,333 µs).
   Result: contract refused without the capability (a capability-security
   case); `bench-audio` at 256 frames under CPU burn with zero underruns.
2. **Engine/policy split.** Move stream open and close, ownership, master
   control and admission from mixer.svc into audio-policy.svc. The mixer
   becomes the engine and accepts only validated transactions from policy's
   capability.
   Result: `audio-grants`, `bench-audio` and the SameBoy volume regression
   unchanged; `audio-mix-and-copy` p99 within noise of before.
3. **Graph model in SPARK.** `Audio_Graph` and `Cycle_Admission`, proved and
   hosted-tested. The engine runs today's topology (clients → mix → gain →
   device) from the graph.
   Result: proof passes at level 1; PCM output bit-identical to step 2 on the
   `bench-audio` signal.
4. **Visibility.** `audio.graph` read-only interface, per-node counters,
   trace events, Workbench view.
   Result: counters agree with `bench-audio`; a deliberately stalled client
   appears in both the graph and the trace window.
5. **Smaller quanta.** Variable BDL period size; quantum as a graph
   transaction; 128, then 64 frames.
   Result: the "Tested" matrix at 64 frames; IRQ-to-engine p99 under 50 µs.
6. **Capture.** HDA input stream, capture device node, `Audio.Capture`,
   desktop indicator.
   Result: QEMU loopback round trip reported and measured; unauthorized
   capture refused.
7. **Cycle-synchronous out-of-process nodes.** WAKE/DONE protocol, cut-offs,
   miss policies, quarantine, constrained deadlines and event release in the
   scheduler.
   Result: fault-injection tests pass; the cost of each out-of-process node
   per cycle measured in µs.
8. **Built-in DSP.** EQ, resampler, meters and limiter, proved.
   Result: proofs, plus offline tests against reference outputs.
9. **Plugin packages and hot insertion.** Package declaration, sandbox, shadow
   run, crossfaded insert and removal, CCL UI widgets (knob, graph view).
   Result: the fuzz example end to end, first by a person, then by an agent
   under a mission with approvals.
10. **Power mode and timer-scheduled cycles** (speculative). Large quanta on
    battery; wakeups from predicted DMA position.
    Result: CPU package wakeups per second and power draw during playback,
    measured on hardware.

## Open questions

1. **Internal sample format.** 32-bit fixed point is easy to prove but is
   not what plugins and most decoders use. Float32 is the ecosystem standard
   but is harder to prove (NaN, rounding). Convert at the boundary, or prove
   a float subset?
2. **Star or activation counters.** Engine-mediated wakes cost a round trip
   per out-of-process node. Direct dependant wakes save IPC, but let a node
   influence another's timing. Measure the star at 64 frames first.
3. **Periods on the device clock.** Event-released (sporadic) budgets in
   the kernel, or double budgets over wall-clock windows? The first is
   precise, the second needs no kernel change.
4. **Dividing `CAP_SCHEDULING`.** Policy needs to hand out sub-budgets from
   a pool, with revocation. scheduler.md does not define derivation yet.
5. **Several clocks.** Two devices, USB audio or network audio: one engine
   thread per clock and adaptive resampling between them. Who owns the drift
   estimate, and is it proved?
6. **One engine process or one per device.** One process shares built-in
   DSP and link state; one per device isolates device failures.
7. **Real HDA limits.** The smallest period the N95 controller and codec
   sustain, interrupt behaviour at 750 Hz, and the DMA risk without an
   IOMMU.
8. **Parameter transport.** A shared parameter page per node, or parameter
   events in the cycle header? Sample-accurate automation needs the latter.
9. **Compatibility.** Should ported software (Servo, SDL, games) see a
   PipeWire- or PulseAudio-shaped client library over buffered nodes, or
   only `CuBit.Audio` and libc? A compatibility shim must not bring ambient
   graph access with it.
10. **The capture indicator.** The desktop must be able to show every
    listener it cannot be tricked about. Is the engine's listener table,
    delivered over a desktop capability, sufficient?
