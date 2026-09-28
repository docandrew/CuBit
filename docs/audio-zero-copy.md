# Zero-Copy Audio and Real-Time Media

## Purpose

Audio is CuBit's first reference implementation for copy-free bulk streams and
deadline-sensitive userspace services.  The design must preserve process
isolation and explicit authority while keeping allocation, payload IPC, and
unbounded work out of the real-time path.

The target is not merely successful playback.  CuBit should provide low and
predictable latency, explain every stream connection, and contain faulty or
malicious clients without granting applications direct hardware authority.

## Implemented reference path

The previous path was:

```text
application buffer
    -> copy into mixer-owned granted ring
    -> mixer reads and combines client samples
    -> mixer writes a granted staging buffer
    -> HDA copies staging into its DMA buffer
    -> hardware
```

Kernel grants already mapped the same physical pages into both processes, but
the HDA boundary still introduced a redundant staging copy.  The implemented
playback path is now:

```text
application produces directly into its granted ring
    -> mixer reads each input and writes final output into an HDA DMA period
    -> hardware reads that period
```

HDA owns the DMA allocation and creates a capability-directed grant giving the
mixer read/write access to only the single page containing its four PCM
periods. The mixer acquires that generation-tagged grant through its HDA
endpoint capability, which pins the backing frame until the acquisition is
returned. Its BDL, CORB/RIRB, MMIO, and other DMA pages remain private. The
former mixer staging buffers and the HDA copy have been removed.

Mixing is a transformation, so reading each input and producing one distinct
output period is irreducible memory traffic.  A client decoder or synthesizer
can avoid an additional producer-side copy by reserving one or two ring spans,
writing samples into them directly, and committing the initialized frames.

## Authority boundaries

An audio application receives a session-scoped stream handle and access only
to its own sample ring.  It receives no HDA endpoint, MMIO mapping, DMA
descriptor access, IRQ authority, or access to another application's stream.

The mixer receives:

* read access to active client sample rings;
* write access to the isolated PCM data pages used by HDA playback;
* an endpoint for bounded HDA control operations;
* an admitted real-time CPU budget.

The mixer must not be able to alter the HDA buffer descriptor list, controller
registers, CORB/RIRB, or unrelated DMA pages.  The HDA driver retains those
authorities and grants only its PCM period page to the mixer.

The current pinning mechanism prevents CPU-side allocator reuse but does not
constrain a bus-mastering device. HDA and its driver therefore remain inside
the memory-isolation trusted computing base today. With an IOMMU, each device
must have a domain containing only its
queues, descriptors, and currently authorized data pages.  Direct DMA into a
client grant additionally requires pinning the grant for the complete in-flight
interval.  Revocation becomes a state transition which completes only after
the device can no longer access the pages.

## Period ownership

Each HDA playback period has one owner at a time:

```text
Free_For_Mixer -> Ready_For_Device -> Playing -> Free_For_Mixer
```

For the first implementation, HDA owns the DMA allocation and grants the PCM
period page read/write to the mixer.  HDA sends a capability-directed,
fire-and-forget `AUDIO_PERIOD_COMPLETE` message after acknowledging the device
interrupt.  The message identifies the newly free period and a monotonic
completion sequence.  The mixer writes only periods returned by HDA.

Period messages carry no sample payload and require no reply.  Lost or delayed
messages are detectable from the sequence.  A later shared control page may
coalesce wakeups, but must not permit the mixer to modify HDA-owned state.

## Client ring ownership

The audio client ring is single-producer/single-consumer:

* the application owns the write position and sample space it has reserved;
* the mixer owns the read position and samples it has acquired;
* publishing a write uses release ordering;
* observing published samples uses acquire ordering;
* releasing consumed samples uses release ordering;
* observing released space uses acquire ordering.

The zero-copy client API uses explicit ownership verbs:

```text
Reserve_Write -> one or two writable spans
Commit_Write  -> publish initialized frames
Peek_Read     -> one or two borrowed-constant spans
Release_Read  -> return consumed frames
```

The convenience `write` operation is implemented in terms of these primitives
and uses at most two bulk copies across a ring wrap.  Decoders and synthesizers
should render directly into reserved spans.

The current header counters are Ada `Atomic`, which provides a conservative
sequentially consistent publication boundary.  The reservation is an opaque,
single-use runtime value: commit validates that it still belongs to the stream
and current write position, then consumes it.  A later layout revision should
put producer and consumer counters on separate cache lines and expose explicit
acquire/release operations without weakening this state machine.

## Scheduling

The five-millisecond polling loop has been replaced by device consumption:

1. HDA acknowledges a buffer-completion interrupt.
2. HDA publishes the completed period to the mixer using one-way IPC.
3. The scheduler wakes the admitted mixer service immediately.
4. The mixer consumes bounded work from each active stream and fills that
   period directly.
5. The mixer blocks when there is no control request or completed period.

Only the mixer and the short HDA interrupt path need real-time scheduling.
Playing audio does not grant arbitrary applications real-time priority.  The
scheduler must enforce admitted period/budget pairs so a faulty mixer cannot
starve security or kernel work.

The real-time path performs no allocation, service discovery, configuration,
filesystem access, verbose logging, or page fault recovery.  Code, stack,
stream metadata, and period pages are resident before playback begins.

## Diagnostics and targets

Per stream and per device, retain bounded counters for:

* periods completed and mixed;
* client underruns and overruns;
* missed or coalesced period notifications;
* mixer execution time and wakeup latency histograms;
* deadline misses and longest observed scheduling delay;
* current period size, queued depth, and estimated buffered latency.

Initial correctness work keeps 256-frame periods at 48 kHz.  After interrupt
ownership is reliable, test 128, 64, and 32-frame periods.  Claims are based on
minimum stable settings and tail latency under CPU, GUI, storage, and network
load, not idle average latency.

The first QEMU WAV-backend validation sustained 186--187 completion messages
per second, matching the expected 187.5 Hz period rate.  After application
startup it sustained fully active intervals with zero missed notifications and
zero underruns.  The `desktop-doom` headless test uses a WAV backend and
requires evidence of a real HDA period interrupt, so initial buffer priming can
no longer produce a false pass.

QEMU HDA currently uses MSI, avoiding shared legacy INTx ambiguity.  Static MSI
vector selection is temporary; CuBit needs a kernel-mediated vector allocator.
Legacy level-triggered PCI fallback also needs mask/ack/unmask support before
it is suitable for latency-sensitive devices.

## Generic streams

The existing generic stream implementation also maps shared pages, but
`streamWrite` and `streamRead` copy payload bytes.  It additionally keeps the
subscriber's live cursor only in local subscriber state while producer-side
flow control reads a producer-owned cursor table.  Therefore its current
backpressure and flush behavior cannot be the basis of the audio protocol.

Generic streams should later gain the same reserve/commit and peek/release
operations.  Multi-subscriber streams need isolated consumer control pages or
batched capability-directed acknowledgements; consumers must not receive write
access to the producer index or other consumers' cursors.

## MP3 workload

The first media application should be a small capability-contained MP3 player:

* a file handle scoped to the selected track;
* a decoder with no filesystem, network, HDA, or mixer-control authority;
* one negotiated 48 kHz stereo output stream;
* direct decode into reserved audio spans;
* a small desktop controller showing position, buffer depth, underruns,
  execution time, and granted authorities.

The decoder library should be selected for a narrow, auditable API.  Until a
SPARK decoder exists, place non-SPARK decoding in its own process so malformed
media cannot compromise the mixer, HDA driver, or desktop.

## Implementation status

1. [x] Grant the isolated HDA PCM period page directly to the mixer.
2. [x] Remove the mixer staging buffer and HDA copy.
3. [x] Acknowledge HDA period interrupts and send one-way completion messages.
4. [x] Replace timer polling and synchronous per-period calls with blocking
   mixed IPC reception in the mixer.
5. [x] Add period sequence and underrun diagnostics plus a real-IRQ regression.
6. [x] Add the SPSC reserve/commit client API with atomic publication.
7. [ ] Add bounded wakeup-latency and mixer-execution-time histograms.
8. [ ] Build the isolated MP3 decoder/player workload.
9. [ ] Generalize the ownership model into typed CuBit streams and direct
   NVMe/network buffer transfer.
