# Netstack redesign: verified parsing, verified TCP, fast data path

Goal (user, 2026-09-26): a robust network stack, formally verified to a
reasonable extent; each piece states what is proved and what is tested.
Performance competitive with Linux; correctness, reliability and security
far beyond it, aiming at no vulnerabilities at all. What stands between
the proofs and that aim is listed under "Trust boundary" below, and is
covered by fuzzing, differential testing and review.

Status: phase 1 begun (2026-09-25): `userspace/net/` holds the upstream
RecordFlux v0.26.0 specifications (byte-identical, Apache-2.0, attributed in
`userspace/net/NOTICE`) and their generated SPARK units, which netstack now
builds from. Everything else below is plan. The current
netstack keeps running, with capacity stopgaps for Servo (deferred TX 256,
64 TCP connections, 64 channels; see `userspace/services/netstack`).

## Why

Running Servo against real sites (docs/servo-port.md) showed the limits of
today's netstack:

- TCP has no retransmission: one dropped segment stalls its connection.
- Fixed small queues drop frames under ordinary browser load.
- One frame per driver submission; data is copied several times.
- Generated RecordFlux parsers exist for Ethernet, ARP, IPv4, ICMP, TCP
  and UDP, but their `.rflx` specifications are not in the tree, and much
  handling around them (options, DNS, reassembly) is hand-written.

## Goals

1. **Nothing parsed by hand.** Every byte from the wire is read through
   RecordFlux-generated SPARK parsers (absence of runtime errors proved),
   from specifications kept in the tree, as SPARKTLS does for TLS records:
   Ethernet, ARP, IPv4 (with options), ICMP, UDP, TCP (with options: MSS,
   window scale, SACK-permitted, SACK, timestamps) and DNS messages. The
   same specifications generate the serializers for everything sent.
2. **A proved TCP.** The connection state machine as a RecordFlux session
   where it fits, and hand-written SPARK where it does not, with proofs of
   functional properties, not only absence of runtime errors: the RFC 9293
   state transitions; sequence-number arithmetic modulo 2^32; the receive
   window never accepts data outside it; retransmission and acknowledgement
   bookkeeping keep every sent byte either acknowledged or queued. Where a
   property is beyond the prover, it is stated as an assumption and
   covered by tests, and labelled so.
3. **Robust.** Retransmission with RTT estimation (RFC 6298), congestion
   control (NewReno first), SACK, zero-window probing, keep-alive, TIME_WAIT,
   SYN-flood limits, and bounded memory per connection and per scope; no
   fixed tables that fail ordinary use.
4. **Fast.** Batched descriptor submission to the NIC driver, a zero-copy
   path from the driver's receive buffers to the application's lent buffer
   where possible, delayed acknowledgements, window scaling for high
   bandwidth-delay paths, and no per-frame console output. Targets are set
   by same-session A/B benchmarks (bulk throughput, request latency,
   connections per second), as for kernel work.
5. **Same authority model.** Applications keep the capability-scoped
   channel protocol (NET_OPEN/READ/WRITE/SHUT, scopes checked by name and
   port, DNS inside the scope); the redesign is below that interface.

## Scope: a complete modern TCP/IP stack

Everything a current general-purpose stack is expected to do (user request,
2026-09-26), not only retransmission:

- **Link and addressing:** Ethernet; ARP with a bounded cache, timeouts and
  gratuitous ARP; IPv6 Neighbor Discovery (RFC 4861); DHCPv4 (RFC 2131,
  RFC 2132 options); IPv6 SLAAC (RFC 4862) and DHCPv6 (RFC 8415); static
  configuration; loopback.
- **IP:** IPv4 and IPv6 dual stack; fragmentation and bounded reassembly
  with timeouts (RFC 791, RFC 8200); ICMPv4 and ICMPv6 including
  destination-unreachable and packet-too-big handling; Path MTU discovery
  (RFC 1191, RFC 8201) and packetization-layer PMTUD (RFC 8899) against
  black holes; routing table with default route; ECN (RFC 3168).
- **TCP (RFC 9293):** the full state machine including simultaneous open and
  close, TIME_WAIT, RST handling per RFC 5961; retransmission timer and RTT
  estimation (RFC 6298); fast retransmit and recovery, NewReno (RFC 6582);
  SACK (RFC 2018, RFC 6675); window scaling and timestamps with PAWS
  (RFC 7323); RACK-TLP loss detection (RFC 8985); congestion control CUBIC
  (RFC 9438) with NewReno available; delayed ACK and Nagle (RFC 1122,
  RFC 896) with application control; zero-window probing and persist timer;
  keep-alive; MSS negotiation and clamping; initial window 10 (RFC 6928);
  SYN cookies and bounded half-open queues; initial sequence numbers per
  RFC 6528; urgent pointer parsed (RFC 6093) but not surfaced.
- **UDP:** checksums (mandatory for IPv6), connected and unconnected use,
  bounded queues.
- **DNS, as its own service (dns.svc; decided 2026-09-26), resolver and
  server:** answering authoritative zones as well as resolving (user
  request: a verified alternative to BIND's history of parsing and memory
  bugs), from the same RecordFlux message specifications. As a resolver: a caching stub
  resolver: A and AAAA, CNAME chains, TTLs, retries and timeouts, randomized
  IDs and source ports, TCP fallback for truncated answers; DNSSEC and
  encrypted DNS later. Netstack keeps checking scopes by the name an
  application asked for and asks dns.svc for addresses through a
  capability; applications keep naming hosts (`@net:tcp:host:port`). This
  keeps DNS parsing, caching and retries out of netstack's trusted code and
  lets the resolver restart, and change transport, on its own. Design:
  docs/dns-service.md.
- **Performance:** batched submission to the NIC, virtio-net checksum
  offload and TSO/GSO where the device offers them, zero-copy receive into
  application buffers where possible, bounded per-connection and per-scope
  memory with autotuned buffers.

Progress, phase 2 (2026-09-26): `userspace/net/src` has the first proved
TCP units (sequence order, segment acceptability and trimming, RFC 6298 RTO,
the send/retransmission queue with a functional specification, and the
connection state machine with RFC 5961's defences; 407/407 checks proved,
13/13 mutants killed, Linux-hosted lifecycle scenarios; tests/net-tcp/README.md).
Rule: each unit is proved and mutation-checked before the next is written.
Since then: out-of-order reassembly (`TCP_Receive_Queue`, 825/825 checks
proved; its mutation run caught an under-specified `Read`, now fixed, and
is to be rerun). Written but not yet proved (drafted while the build machine's /tmp was
full; nothing uses them until their proofs and mutants pass):
`TCP_Congestion` (NewReno), `TCP_Options`, `TCP_Timestamps` (PAWS),
`TCP_Scoreboard` (SACK), `SipHash`, `TCP_Isn` and `TCP_Syn_Cookies`.

Spec changes found while writing them, to make in `specs/tcp.rflx` and
record in `CHANGES_FROM_UPSTREAM.md`: an unknown option kind makes the
whole segment unparseable (RFC 9293 3.1 requires skipping it by its
length); a non-zero urgent pointer without URG is rejected (RFC 9293 says
to ignore it); and the reserved bits (below).

Next units, in order, each standalone and Linux-hosted testable:

1. `TCP_Options`: MSS, window scale, SACK-permitted, SACK blocks and
   timestamps from the RecordFlux TCP option parser into checked values;
   negotiation (RFC 7323 2.2: scaling only if both SYNs carry it; shift
   at most 14; MSS clamped to the path).
2. `TCP_Timestamps`: RTT samples from TSecr (RFC 7323 4) and PAWS
   (RFC 7323 5): reject a segment whose TSval is older than TS.Recent.
3. `TCP_Scoreboard`: SACK (RFC 2018) receiver blocks from the receive
   queue's presence map, and the sender's scoreboard with RFC 6675 loss
   recovery (IsLost, NextSeg, pipe); proved never to retransmit a SACKed
   byte and to count `pipe` exactly.
4. `TCP_Timers`: retransmission (RFC 6298 5), persist (zero-window
   probes with backoff), keep-alive, delayed ACK (at most 500 ms, every
   second full segment, RFC 1122 4.2.3.2), TIME-WAIT; one deadline per
   connection kept in a timer wheel, so the cost does not grow with the
   number of connections.
5. `TCP_Cubic` (RFC 9438) behind the same interface as NewReno.
6. `TCP_Isn` and SYN cookies (RFC 6528, RFC 4987) with the half-open
   queue bounded per listener.
7. Integration: a connection engine composing these, differential-tested
   against the host's stack, then netstack switches over.

Order of work: TCP correctness first (it is what stalls today), then DNS,
IPv6 and the rest; each piece lands with its proofs or tests, and the
documentation says which.

## Scale and denial of service

Today's netstack has fixed tables (32 TCP connections, 64 channels): fine
for one browser, not for a server. The design below targets hundreds of
thousands of connections, with memory bounded and attributed.

**Connections.**
- A control block holds the connection's state, sequence variables, RTO,
  congestion, options, timestamps and SACK scoreboard: about 0.5 KiB. It
  comes from a slab of fixed-size blocks that grows in steps up to a
  configured cap (Config: `net.tcp.max-connections`).
- Connections are named by a handle (index plus generation), so a stale
  handle can never reach a reused block.
- Lookup by 4-tuple uses a hash table keyed with SipHash and a boot-time
  secret, so an attacker cannot aim collisions at one bucket (hash
  flooding).

**Buffers.**
- Send and receive data do not get fixed per-connection rings: 256 KiB
  each way times 100k connections is 50 GB.
- Queues are lists of fixed-size chunks (4 KiB) from a shared pool,
  allocated as data arrives and freed as it is read or acknowledged.
- The advertised window follows the memory actually available to the
  connection (autotuning between a minimum and a maximum, as Linux's
  `tcp_rmem`), so an idle connection costs its control block only.
- The proved ring queues become chunked queues with the same functional
  contracts. The pool is its own proved unit: a chunk belongs to at most
  one queue, a freed chunk is never referenced, and the accounting matches
  what is allocated.

**Quotas by authority.**
- Every application's network capability carries limits: connections,
  listeners, and buffer memory. Netstack charges each allocation to the
  scope that caused it.
- One application, or one remote peer, cannot exhaust memory that others
  need. A global cap holds the whole pool, and under pressure
  out-of-order data is pruned first: it is unacknowledged, so dropping it
  is safe.

**Denial of service.**
- **SYN floods.** A half-open connection is a small request entry
  (about 64 bytes), not a control block, in a table bounded per listener
  and globally. When it is full, SYN cookies take over (`TCP_Syn_Cookies`).
  The accept queue is bounded per listener.
- **Blind injection.** RFC 5961 challenge ACKs (proved) are rate-limited
  per connection with randomized budgets, not with the single global
  counter behind CVE-2016-5696. ISNs are unpredictable (RFC 6528).
  ICMP errors are honoured only when they quote an in-window sequence
  number (RFC 5927).
- **Crafted SACKs** (the Linux SACK panic and slowness, CVE-2019-11477
  and 11478). A minimum MSS, a bounded scoreboard, and work per ACK bounded
  by the scoreboard's size.
- **Slow readers and idle holders.** Persist probes are capped; FIN-WAIT-2
  times out for orphaned connections; TIME-WAIT is capped and reused
  safely with timestamps (RFC 6191); keep-alive is available.
- **Fragments.** IPv4 and IPv6 reassembly buffers are capped in memory and
  time, and overlapping IPv6 fragments are rejected (RFC 5722).
- **Neighbour caches.** ARP and Neighbor Discovery caches are bounded, and
  entries are never created from unsolicited traffic.
- **Rate limits.** Outgoing ICMP errors, RSTs and challenge ACKs go
  through token buckets, proved never to exceed their rate.
- **Amplification.** The DNS server's response rate limiting
  (docs/dns-service.md).

**To prove.**
- The chunk pool's ownership and accounting.
- Handle generation checks.
- Token-bucket rate bounds.
- Bounded half-open tables.
- That every allocation is charged to a scope and released on close.

## Trust boundary

What the proofs do not cover, each kept as small as possible and named:

- **Specifications.** A proof shows the code meets its contract; a
  missing or wrong contract is not caught. Mutation testing finds
  contracts too weak to notice plausible bugs; contracts are reviewed
  against the RFCs; and differential tests against Linux catch behaviour
  no contract describes.
- **Unproved code.** The netstack service's IPC and driver glue, and the
  NIC driver. It shrinks as each part moves into proved units; what
  remains is fuzzed.
- **Assumed primitives.** SipHash's unpredictability; the entropy
  service; SPARKTLS for DNS over TLS.
- **Toolchain and platform.** GNAT, gnatprove and its provers, the kernel
  and the hardware.
- **Resource exhaustion** that stays within the quotas is by design, not
  a bug; the quotas are the defence.

## Performance

Measured against Linux on the same hardware (same-session A/B, as for
kernel work), with targets set before optimizing:

- Bulk throughput on one connection and on many.
- Request latency (small request, small response).
- Connections per second, accepted and opened.
- Memory per idle and per busy connection.
- Behaviour under loss and reordering, where SACK, RACK-TLP and CUBIC
  matter.

Means to those targets: batched NIC descriptor submission, checksum
offload and TSO/GSO where virtio-net offers them, zero-copy receive into
lent buffers, delayed ACKs, a timer wheel (constant cost per connection),
and hash lookup of connections. No per-packet console output.

## The connection engine (next)

The proved units compose into one engine, a SPARK package that is itself
proved and that netstack drives. It owns no memory of its own; everything
comes from the proved structures below.

- **State per connection** lives in parallel arrays indexed by
  `Connection_Table` slots: the state machine (`TCP_Connection`), RTO,
  congestion control, the negotiated options, timestamps, the SACK
  scoreboard and two queue descriptors.
- **Queues** are chunked views over `Chunk_Pool`:
  - The send queue is a bounded ring of chunk ids plus the byte offsets of
    SND.UNA and SND.NXT in it, with the same functional contract as
    `TCP_Send_Queue` (segments carry exactly the bytes written; ACKs free
    only acknowledged bytes). Chunks are allocated as the application
    writes and released as ACKs cover them.
  - The receive queue keeps `TCP_Receive_Queue`'s contract. Presence bits
    stay per byte for the window's span. Chunks are allocated on first
    arrival in their range and released when read.
  - The pool's owner is the connection slot; its per-owner counts are the
    quota basis.
  - The byte-to-chunk mapping avoids division in proofs: the queue keeps
    (chunk, index) cursors and advances them incrementally, as the send
    queue's ring does with Head.
- **Timers:** one `Timer_Heap` id per connection-timer pair, with
  retransmission, persist, keep-alive, delayed ACK and TIME-WAIT
  multiplexed by kind.
- **Entry points**, each proved to keep every connection's invariants:
  - `Segment_Arrived`: parsed header plus payload view, giving the reply
    to send and the data to deliver.
  - `Application_Wrote` and `Application_Read`.
  - `Open` and `Close`.
  - `Timer_Fired`.
  - `Next_Segments`: what to transmit now, bounded by
    `TCP_Congestion.Allowance`, SACK recovery (NextSeg) and Nagle.
- **Proof obligations of the engine:**
  - It changes only the addressed connection's state.
  - Quotas are respected.
  - Every unit's preconditions hold at each call.
  - Its outputs match the per-unit contracts: the reply is what the
    state machine decided, and the delivered bytes are those the receive
    queue returned.

**Integration.** netstack keeps its IPC and scope checks, and replaces its
TCP session code with calls into the engine. Before the switch, the old
and new stacks run side by side on the host against recorded packet
traces, and every difference is explained. After it, all native
regressions run (network-authority, Servo pages over HTTP and HTTPS).

### Progress: the endpoint (2026-09-26)

`TCP_Endpoint` couples the state machine to its chunked send queue, proved
at level 1: in SYN-SENT and SYN-RECEIVED only the SYN is in flight; once
synchronized SND.UNA and SND.NXT are the queue's, plus one for a FIN sent
after all data. Accepted ACKs free exactly the acknowledged bytes, a
timeout rewinds both views, a reset returns the chunks.
`tests/net-tcp/tcp_endpoint_sim` runs two endpoints over a link losing a
quarter of its segments: the byte stream arrives exact, retransmitted on
timeout, and both sides close cleanly.

Sequence numbers are modelled as integers modulo 2^32 for proof
(`No_Bitwise_Operations`): as bit-vectors, the provers could not relate
counts to sequence-number sums, even at higher levels.

Next, the receive side, split the same way: the state machine keeps
control (acceptability, RST, SYN, ACK, consuming a FIN); segment text goes
into the receive queue at its offset (out-of-order data kept, as proved
for `TCP_Receive_Queue`), RCV.NXT follows the queue's contiguous edge, and
a FIN is remembered at its sequence number and consumed once everything
before it has arrived. "Delivered in order, exactly as sent" becomes the
receive queue's contract rather than the state machine's.

### Progress: netstack runs on the proved engine (2026-09-26)

Linux-hosted work, then live CuBit integration:

- **Proved (level 1, `tests/net-tcp`, 81 of 81 mutants killed):** the
  receive side and the retransmission point joined `TCP_Endpoint`;
  `TCP_Flow` adds RFC 6298 timing with Karn's rule, NewReno and pipe-based
  allowance. The retransmission timer runs whenever any sequence space
  (data, SYN or FIN) is in flight: an ACK for data no longer stops the timer
  that covers a lone FIN. It restarts, backed off, when it fires, and the
  flow counts timeouts without progress (`Exhausted`: 6 for a handshake,
  15 otherwise, as Linux's defaults). `tests/net-tcp/tcp_flow_sim` runs a
  whole connection over a 10% lossy link with a forced first-SYN loss and a
  forced loss of the closer's FIN while its data is still in flight.
- **Integrated (native):** netstack's `TCPSession` state machine is gone.
  Each connection slot (`TCP_Slots`: addresses and lifetime) owns a
  `TCP_Flow` (`TCP_Engine` instantiates the proved units: a 512-chunk
  pool of 4 KiB, 60 KiB send queues, 64 KiB receive queues without window
  scaling). The glue in `main.adb` sends what the flow allows, resends on
  its timer, answers the replies the state machine decides, and moves
  TIME-WAIT out of the slot into a 128-entry table (ACKs retransmitted
  FINs, ignores RSTs per RFC 1337). Writes are queued, and an application
  whose write does not fit waits for ACKs (no silent truncation).
  Orphaned connections (owner gone) finish closing within 120 s. SYNs
  carry an MSS option and read the peer's (`TCP_Wire`, proved). Initial
  sequence numbers follow RFC 6528 (SipHash keyed from RDRAND at start).
- **Native results:** `network-authority` passes (inbound accepts,
  fragmentation, half-close, 40 channel lifetimes, outbound connects).
  The `servo` case fails at its first page with both the old and the new
  netstack (nothing rendered), so that failure predates this change.
- **Not yet:** window scaling, SACK, timestamps, delayed ACKs, a persist
  timer for zero windows, challenge-ACK rate limiting, and an `Abort`
  operation in `TCP_Flow` (the glue sets CLOSED directly, which keeps every
  invariant, but outside the proofs). The glue itself is not SPARK.

### What ships without run-time checks, and what backs it (2026-09-26)

netstack and virtio-net build with `-O3 -gnatp`: no Ada run-time checks.
A failed check in a driver or service is no recovery, and costs cycles on
every packet; correctness is to come from proof. Status:

- **Proved at level 1 (absence of run-time errors plus the functional
  contracts):** every unit in `userspace/net/src` (tests/net-tcp), including
  the `TCP_Header` codec, and netstack's `TCP_Slots`, `TCP_Wire`,
  `TCP_Listeners`, `Network_Channel_Handles` and `TCP_Engine` - the actual
  generic instances netstack runs (64 KiB receive queues, 256-chunk send
  queues, 1,024-chunk pool), not only the test instances (tests/tcp-session,
  1,173 checks for the instances).
- **Not yet proved (tested only):** netstack's `main.adb` glue and `net.adb`
  helpers (IPC, packet dispatch, the TCP glue around the flows, ARP, ICMP,
  UDP, DNS), the virtio-net driver, and the RecordFlux-generated UDP code as
  instantiated there. Under `-gnatp` their safety rests on tests
  (network-authority, bench-net, the hosted suites) until they are brought
  under SPARK. That is the next assurance step, in this order: the TCP
  glue (it already calls only proved units), packet dispatch and the
  frame/ring handling, then the driver's rings.

### Tried and reverted: per-CPU netstack workers (2026-09-26)

With the program on netstack's CPU (2 vCPUs) the round trip is 67-70 us,
as with 1 vCPU, against 97-102 us on 4 vCPUs: cross-CPU wake-ups cost ~30
us per round trip. A worker thread per CPU (the kernel preferring a
receiving thread on the sender's CPU) moved only the program's requests
onto its own CPU; transmission still wakes the driver on the network CPU
and received data still arrives there, and two workers contended for
netstack's lock (download fell from 3.3 to 2.3 Gbit/s, round trip within
noise). Reverted. Kept: netstack's state at library level
(`Netstack_Service`), `No_Secondary_Stack` for all its units (one
secondary stack per process would be shared by threads), and the kernel's
same-CPU receiver preference (no cost in bench-ipc A/B). Merging the driver
into netstack is ruled out: netstack must serve many NIC drivers.

### Receive-path hardening (2026-09-26)

Moving the receive path onto proved codecs surfaced defects in the old
glue, fixed together:

- **Memory safety:** the DNS query encoder checked for room once per
  label but wrote the whole label through unchecked address writes into an
  80-byte stack buffer; a long host-name label from a program overflowed
  it. Names are now encoded by `DNS_Name` (proved: bounds, labels 1..63,
  at most 255 bytes, host-name characters only, lower case).
- **DNS spoofing:** transaction IDs were sequential and every query used
  one source port, so an off-path attacker could forge answers; a fixed
  "self-test" ID was also accepted. IDs and source ports are now a keyed
  PRF (SipHash, keyed from RDRAND at start; RFC 5452); a response must come
  from the configured server's port 53 to a pending query's port with its
  ID, carry exactly that query's question name (compared by keyed hash, read
  by the proved `DNS_Name.Read_Name`, no compression in the question), and
  only then its first A record of class IN completes that one request.
- **ARP spoofing:** any ARP reply claiming the gateway's address replaced
  the gateway's MAC, and any ARP packet updated the cache. `ARP_Cache`
  (proved) learns a mapping only from the answer to our own request, or
  from a request for our address (RFC 826 merge) for an address not
  already resolved; a resolved hardware address never changes; bogus
  senders (zero, loopback, multicast, broadcast) are never learned. ARP
  fields are checked by `ARP_Packet` (proved).
- **Unverified checksums:** UDP checksums were never checked (DNS answers
  included); they are now (zero still means none, as IPv4 permits).
  ICMP checksums are checked and echo replies rate-limited (1 per ms,
  bursts of 50).
- **Console floods:** each malformed TCP segment, malformed UDP datagram,
  ARP packet and ping printed a console line; a flood of any became a
  console flood. They are counted instead (a line per 4,096 at most).
- IPv4 (`IPv4_Header`) and UDP (`UDP_Header`) headers are parsed by
  proved codecs. `tests/net-tcp` has host checks for each (including the
  ARP spoofing attempts) and 96 of 96 mutants killed.

A resolver query that is never answered now fails after 5 s (it held a
pending slot forever; 32 such queries denied service). The in-netstack
resolver is to move to `dns.svc` (docs/dns-service.md), which should keep
these rules.

### Where a round trip's time goes (2026-09-26)

The kernel's trace (`LATENCY_TRACE=1`, one vCPU, five 1-byte round trips)
showed per round trip about six direct IPC handoffs, seven wake-ups and 39
system calls, ~35 us of idle waiting for QEMU and the host, and two
avoidable hypervisor exits in virtio-net: reading the ISR register on each
interrupt (6.8 us; unnecessary with MSI-X) and kicking the receive queue
after every batch (2.9 us; now only when the device asks, per the used
ring's NO_NOTIFY flag). Without them a traced round trip fell from ~82 to
~53 us. With four vCPUs, wake-ups to other CPUs add IPI and HLT-exit costs;
the kernel's policy (equal-priority wakes stay FIFO, no busy polling; see
docs/input-latency.md) is kept: the next step is keeping the network
pipeline on one CPU so that its hops are direct handoffs.

## Async channels (implemented, 2026-09-26)

Each TCP read or write is a synchronous call to netstack today, and a read
that finds no data parks a deferred reply. A request/response round trip
therefore costs two client calls and two wake-ups on top of the driver's
hops. The libc also runs a reader thread per socket to emulate readiness.
CuBit's I/O is async-first (submit with a token, collect a completion), so
channels become shared rings. Data moves without IPC while both sides are
busy. IPC is needed only to wake a side that is idle.

This replaces OP_NET_READ and OP_NET_WRITE outright, with no compatibility
layer (TCP first, then connected UDP; see "Datagram channels"). The legacy OP_NET_CONNECT/SEND/RECV/CLOSE
handlers, unreachable since admission stopped accepting them, are removed.

### The channel grant

The client allocates the channel memory and grants it to netstack. Netstack
acquires the grant with OP_NET_OPEN or ACCEPT, as before. The client writes
the send and receive ring sizes (each a power of two from 4 KiB to 1 MiB)
and the channel's wait bit into the header before OPEN. Netstack reads them
once, checks that they fit the grant, and keeps its own copy; it never
reads them from shared memory again. The layout is
`CuBit.Net_Channel_Layout` (C: `userspace/c/cubit_net_channel.h`).

| Offset | Written by | Contents |
|---|---|---|
| 0 | client | `Tx_Produced`, `Rx_Consumed`, `Want` (notify-me flags: readable, writable), `Shut_Write`; read once at open: `Tx_Size`, `Rx_Size`, `Wait_Bit` |
| 64 | netstack | `Tx_Consumed`, `Rx_Produced`, `Kick_Wanted`, `Status` |
| 256 | client, before OPEN | the target text (for every OPEN, datagram channels too) |
| 4096 | client produces, netstack consumes | send ring |
| 4096 + send size | netstack produces, client consumes | receive ring |

- **Indices.** Every index is a free-running 32-bit byte count, and a
  ring's fill is `Produced - Consumed` (mod 2^32). Each header line has
  exactly one writer. No flag is ever written by both sides.
- **Untrusted client values.** Netstack reads each client-written value once
  per operation into a local. It keeps its own indices privately and never
  reads them back from the grant, and it remembers the last client index it
  accepted. It accepts a new client index only if it moves forward within
  the bounds of the ring:
  - `Tx_Produced - Tx_Consumed <= send size`;
  - `Rx_Consumed` lies between the previously accepted value and
    `Rx_Produced`.
  Any other value sets `Status` to `Protocol_Error` and aborts the
  connection. A client can corrupt only its own stream. Clients check
  netstack's indices through the same code.
- **What is proved.** The ring logic is a proved unit shared by both sides,
  `CuBit.Channel_Rings` in the runtime (`tests/channel-rings`), level 1,
  with mutants. Its inputs are values that were already read, so
  the proof never reasons about shared memory. It proves that:
  - every copy stays inside its ring;
  - private indices only move forward;
  - an accepted client index keeps the fill between 0 and the ring size;
  - a rejected client index changes nothing.
- **What is tested, not proved.** SPARK does not model memory ordering. The
  volatile accesses and fences in the glue are covered by tests instead.

### Notifications

A notification is sent only when the other side is idle, using the classic
store / fence / re-check pattern. On x86 a store can be reordered after a
later load, so each side issues a full fence between publishing its index
and reading the other side's flag. The same fence separates publishing its
flag from re-reading the other side's index.

- **Client to netstack (doorbell).** Netstack sets `Kick_Wanted` in two
  cases:
  - it has drained the send ring;
  - the receive ring is too full to open the TCP window.
  After a client moves an index, it checks `Kick_Wanted` (`Kick_On_Send`
  after writing, `Kick_On_Receive` after reading). If its flag is set, the
  client submits a one-way OP_NET_KICK naming the channel's wait bit: the
  kernel's submit without a completion token, so no reply and no
  capability slot, with the sender and authority tag stamped by the
  kernel. While netstack is busy it pulls from the send ring whenever ACKs
  free send space, so no doorbell is needed.
- **Netstack to client (OP_NET_WAIT).** The client sets `Want` on the
  channels it cares about and re-checks their rings. If none is ready, it
  submits one OP_NET_WAIT with `capSubmit` and a token. That single
  submission covers the process's channels named in its interest mask
  (the wait bits of the channels some thread waits on):
  - Netstack completes it with the mask of the ready channels of interest.
    A channel is ready if its `Want` flags ask for what it has: received
    data or a final status (readable), send ring room or a failure
    (writable). The interest mask keeps a channel nobody waits on, with
    unread data and a stale `Want`, from completing every WAIT at once.
  - Readiness is level-triggered. A WAIT that arrives while something is
    ready completes at once, so no wake-up is lost.
  - An optional deadline word completes the WAIT with an empty mask when it
    passes. That replaces the per-read deadline.
  - The WAIT's words also carry a kick mask, so one submission can be both
    the doorbell and the wait.
  - A KICK with `End_Wait` completes the process's WAIT at once. The libc
    sends it when something outside netstack becomes ready (a pipe written)
    while a thread blocks for netstack, and when a thread starts waiting on
    a socket the outstanding WAIT does not cover.
- **Capacity.** A process may have one parked WAIT. A second one is refused
  with an error completion. So the number of deferred reply capabilities
  netstack holds is bounded by the number of client processes, not by the
  number of channels. Capability slots are 0..63, and 32 of them hold
  deferred replies. Netstack's channel table (64) matches the mask width.

### Status and control

- **Status.** `Status` is one of `Opening`, `Open`, `Peer_Finished`,
  `Reset`, `Timed_Out`, `Unreachable` or `Protocol_Error`. At end of stream
  it is set only after the last received byte is in the ring. A client that
  has drained the ring and sees `Peer_Finished` is at EOF.
- **Half-close.** The client sets `Shut_Write` and kicks. Netstack sends FIN
  once it has sent everything up to `Tx_Produced`.
- **Control operations.** OPEN and ACCEPT may be async submits (their
  completion carries the handle) or calls. SHUT (release the channel and
  its grant) takes what is left in the send ring first, then sends FIN.

### Clients

- **Ada: `CuBit.Net_Channels`.** `Prepare` (lend and lay out the grant),
  `Reset`, `Submit_Open`, `Submit_Accept`, `Opened`; zero-copy views
  (`Readable` then `Consume`, `Writable` then `Commit`) and copying
  `Read`/`Write`; `Want`, `Status`, `Shut_Write`, `Close`; `Submit_Wait`.
  `Await` and `Wait_For` are blocking helpers for programs whose only
  asynchronous work is this. Ported: tls.svc (ciphertext is encrypted
  straight into the send ring and fed to SPARKTLS straight from the
  receive ring, one WAIT for all its channels), network-check, tls-probe,
  ccl-control.
- **C, C++, Rust: the libc.** Sockets keep their rings with the proved
  `CuBit.Channel_Rings`, compiled into libc.a through C entry points
  (`CuBit.Channel_Rings_C`; pure code, no Ada run-time library). The
  per-socket reader thread is gone. Blocking reads, writes, `connect` and
  `poll` wait through one process-wide waiter: waiting threads register
  their sockets, one thread blocks for completions (WAIT and OPEN), the
  others on the descriptor futex. Rust's std uses the libc.
- **NetSurf.** Plain HTTP reads its response from the ring on each fetch
  poll, with no IPC per read; HTTPS goes through tls.svc, whose own client
  protocol is unchanged for now.

### Copies

- **Step 1.** The rings sit next to the engine's own buffers. Netstack
  copies:
  - from the send ring into the chunk pool when it drains;
  - from the receive queue into the receive ring when data becomes in order.
  That keeps the proved engine unchanged, and it already removes all of the
  per-read and per-write IPC.
- **Step 2.** The rings become the engine's buffers:
  - The send ring is the retransmission store. `Tx_Consumed` advances on
    ACK rather than on transmit. A client that overwrites bytes that are not
    yet acknowledged only corrupts its own stream: netstack copies each
    segment into the driver frame once and checksums that copy.
  - The receive ring is the in-order receive buffer, and out-of-order data
    stays private until it is in order.
  - The advertised window is the free space in the receive ring.
  - One copy remains per direction, between the driver frame and the ring:
    the same as Linux without MSG_ZEROCOPY.

### Datagram channels (implemented 2026-09-27)

Connected UDP channels use the same grant and rings, holding records
(`CuBit.Datagram_Rings`, proved in `tests/channel-rings`): a 4-byte header
(length, kind) and the payload padded to 4 bytes; a record never wraps, a
pad record fills the end of the ring, and every header from the peer is
checked. netstack puts an arriving datagram straight into the matching
channel's receive ring (dropped if full, as UDP allows; `UDP_Channels` now
only matches ports and peers) and sends the records in the send ring when
serviced; datagrams longer than 1,472 bytes are refused by the client API
and dropped by netstack. With this OP_NET_READ and OP_NET_WRITE are gone
from netstack. Ported: timesync, network-check.

Not yet ported: netmgr's DHCP client, which already sent OP_NET_WRITE with
a grant id where a channel handle belongs (so it could not work); DHCP
needs a broadcast-capable raw channel of its own.

## Listening, capacity and startup limits (design, 2026-09-27)

This is the next step after the async channels. It turns to completeness:
serving from any language, and connection counts set by configuration
rather than by constants. Performance stays under regression checks
(`tests/net-bench`), but it is no longer the focus.

### What is wrong today

- **Fixed limits.** netstack has 128 connection slots, 64 channels, 32
  deferred replies and 128 TIME-WAIT entries, all compile-time constants.
  Each connection slot also carries its own 64 KiB receive queue.
- **One grant per connection.** Every connection needs its own grant from
  the client, and the kernel allows 16 grants per process, so a process
  can have at most about 16 sockets.
- **One round trip per accept.** Accepting takes an ACCEPT call per
  connection, with one waiting per listener.
- **No listening from C or Rust.** The libc had no listening sockets (it has now: see the status notes below).
- **Shared exhaustion.** Nothing stops one program from filling the
  shared tables for everyone.

### Native API: no socket, bind, listen or accept

The BSD sequence builds an object in stages (socket, setsockopt, bind,
listen), and each stage can fail on its own. It then pays an IPC per
accepted connection. In CuBit the capability already says what a program
may listen on. So the native API opens things in one step, and fixes their
properties at that step:

- **Listener.** `OPEN_LISTENER` names the address and port (checked against
  the capability's `tcp-listen` scope), a backlog, and the properties its
  connections get: ring sizes, no-delay, keepalive. There is no separate
  bind or listen.
  - Arrivals come through a ring in the listener's own grant. netstack
    writes one record per established connection: its channel handle, the
    peer's address and port, and its slot in the arena (below).
  - The listener has a wait bit like any channel, so one WAIT covers
    connections and arrivals alike. Accepting N connections costs no IPC
    per connection.
- **Channel arena.** A process lends netstack one grant cut into N equal
  channel buffers (header page and both rings). A new connection, inbound
  or outbound, takes a free buffer. When the connection is released, the
  buffer comes back to the process for reuse.
  - This removes the grant-per-connection limit.
  - It puts connection memory on the owner's account, not netstack's.
- **Adjustments.** Properties are fixed at open, as a typed record. A
  property that genuinely changes during a connection gets its own narrow
  operation, not a generic option bag.
- **libc compatibility.** The libc maps `socket`, `bind`, `listen`,
  `accept` and `setsockopt` onto listeners and arenas. Ported software
  keeps working; that mapping lives in the libc, not in netstack's protocol.

### Capacity charged per process

A program declares what it needs in its manifest. It is approved at
launch, as network scopes are, and netstack enforces it:

    (request-network tcp-listen (ipv4 "0.0.0.0" 0) (ports 8080 8080)
                     (connections 256) (arena-buffers 64) network)

- **Enforcement.** netstack counts a program's connections, channels and
  arena buffers against its declaration. It refuses anything beyond it
  (a connection arriving over the limit is reset), and never takes from
  anyone else's share.
- **Resource exhaustion.** Exhausting shared tables becomes bounded by
  what was declared and approved.
- **Admission.** The sum of approved declarations is checked against
  netstack's configured limits when each scope is installed, so capacity
  is guaranteed rather than best-effort.
- **Dynamism later.** Growing a declaration at run time is left for later.

**Status (2026-09-27): listeners implemented.** OPEN of
`@net:tcp-listen:<address>:<port>` makes a listener in one step, and SHUT
closes it. The process offers arena buffers in its send ring; netstack
reports each connection that arrives open in one, in its receive ring
(`CuBit.Net_Channel_Layout`, "Listeners"). `OP_BIND`, `OP_ACCEPT` and
`OP_CLOSE_LISTENER` are gone, along with the pending-accept machinery. The
libc maps bind, listen and accept onto it, keeping four sockets offered.
It finds its scopes with `OP_NET_SCOPE` and routes each socket to the one
that permits it, so C and Rust programs can serve. Listeners are fixed at
16, with 16 pending connections each, until the startup limits land.

**Status (2026-09-27): channel arenas implemented.** `OP_NET_ARENA` lends
one grant cut into equal buffers; OPEN and ACCEPT name an arena handle and
a buffer index instead of a grant. The bookkeeping (`Channel_Arenas`) is
proved at level 1: a claimed buffer lies inside its arena, and no buffer
backs two channels. The libc lends arenas of 16 sockets on demand, which
ends the per-socket grant and the limit of about 16 sockets. The TLS
service lends one arena for its 8 channels. Process exit (`OP_RELEASE_OWNER`,
sent by procmgr) releases arenas along with channels and scopes. The
procmgr half of exit notification is pending.

**Status (2026-09-27): connection counts implemented.** Each
`request-network` now requires `(connections N)`, 1–32767, carried in
scope-descriptor bits 49–63. Admission at `OP_INSTALL_SCOPE` and a charge
per open channel are in place, and `Network_Grants` proves at level 1 that
reservations stay within capacity and open channels within each declaration.
The capacity is still the fixed 64-entry channel table, until startup
limits land. Arena buffers are not declared yet, since the arena does not
exist yet. The counts are per scope (grant) rather than summed per process:
a process with two scopes gets both allowances, each approved separately.

### netstack limits as typed startup parameters

netstack's limits become startup parameters: a CCL record, typed and
checked before netstack runs. netstack sizes its tables from them, once,
at start:

    (start "netstack.svc" (arguments (Netstack_Limits
      (connections 4096) (channels 1024) (time-wait 8192) (waits 256))))

This needs typed launch parameters for any process. CuBit has no argument
mechanism today: the kernel's spawn takes no argument buffer and the libc
fakes `argv`. Sketch:

- **Declaration.** A program's manifest declares its parameter type, a CCL
  record with defaults and bounds.
- **Supply.** The launcher (init profile, devmgr, shell) supplies a value.
  The launcher, or procmgr on its behalf, type-checks the value against the
  declared type before the child runs, and encodes it canonically.
- **Delivery.** The encoded value reaches the child read-only, mapped at
  spawn like a stack or auxiliary page. A C program's `argv` becomes one
  such parameter (a list of strings).
- **Decoding.** The child decodes it with the same generated or proved
  decoder that checked it.
- **Ownership.** This crosses the kernel (spawn), procmgr, devmgr and CCL,
  so it is designed together with their owners (coordination/networking.md).

Until then, netstack's tables are sized from constants in one place, with
the limits named and the sizes derived from them.

### Memory per connection

At thousands of connections, netstack cannot keep a 64 KiB receive queue
inside every slot. Step 2 of the channel design ("Copies") makes the
receive ring the in-order receive buffer. netstack then keeps only
out-of-order data (bounded) and the connection's state, and the memory
lives in the owner's arena. Connection lookup moves from a scan to the
proved `Connection_Table` (hash of the 4-tuple, generation-checked
handles).

## Benchmarks against Linux

Same QEMU/KVM guest configuration, virtio-net and host; CuBit guest
against a Linux guest with the same memory and vCPUs, same session,
alternating runs.

- **Throughput:** bulk transfer to and from the host, one connection and
  16 in parallel.
- **Latency:** 64-byte request/response round trips, p50 and p99.
- **Connection rate:** short connections per second, accepted and
  opened.
- **Scale:** memory and CPU at 10k and 100k idle connections.
- **Loss:** throughput at 1% loss and with reordering (netem on the host
  bridge).

"Competitive" means within 10% of Linux on throughput and latency, and at
least equal on connections per second and memory per connection. Any gap
beyond that is investigated before the stack is called done. Results are
recorded here with their dates and configurations.

The harness is `tests/net-bench` (one client for both systems, the same
QEMU configuration and host fixture; see its README).

**Linux reference (2026-09-26):** Linux 6.18.45 (nixpkgs), busybox
initramfs, KVM, 4 vCPUs, 512 MiB, virtio-net on QEMU 11.1 user
networking. Three rounds each:

| Workload | Linux |
|---|---|
| download, 64 MiB | 8.8 - 10.8 Gbit/s |
| upload, 64 MiB | 1.75 - 1.82 Gbit/s |
| 1-byte round trip | 42 - 46 us |
| connect + 1 byte + close | 2,900 - 3,300 per second |

**CuBit on the proved engine (2026-09-26, same configuration; best of
three rounds after each step):**

| Step | download | upload | round trip | connects/s |
|---|---|---|---|---|
| first run (driver dropped frames; a console line per drop) | 69 Mbit/s | stalled | - | - |
| driver accepts TX only with a free descriptor; netstack keeps frame order, throttles TCP on its transmit queue, rate-limits the message; delayed ACKs | 381 | 99 | 120 us | 2,439 |
| receive queue: bulk slice copies (proved), 64 KiB ring | 537 | 97 | 120 us | 2,469 |
| driver hands netstack up to 32 frames per IPC | 856 | 91 | 120 us | 2,326 |
| driver drains queued TX requests, then one virtio kick | 1,036 | 607 | 116 us | 1,695 |
| per RX batch: one reader reply and one ACK per connection | 1,275 | 550 | 120 us | 1,739 |
| TX ring shared with the driver (one doorbell per burst); segments built in place in the ring (zero-copy send); chunk-pool slice copies (proved) | 1,370 | 706 | 115 us | 1,818 |
| word-wise checksums; one clock read per batch | 1,420 | 621 | 118 us | 1,681 |
| proved TCP header codec instead of RecordFlux on the data path (header parse 4,200 -> 80 cycles) | 1,826 | 829 | 115 us | 1,770 |
| `-O3 -gnatp` (proved units; see below) | 1,694 | 863 | 115 us | 1,852 |
| virtio-net: no ISR read under MSI-X, RX kick only when the device asks; proved ARP/IPv4/UDP codecs on receive (quiet host, 21:48) | 3,420 | 1,424 | 97 us | 2,020 |
| async ring channels: no IPC per read or write; libc reader thread gone; one WAIT per process (2026-09-27, three rounds) | 3,579 - 3,703 | 1,409 - 1,463 | 83 - 85 us | 1,980 - 2,128 |
| netstack: timers once per millisecond tick (were scanned on every message), no send attempt with nothing to send, touched-connection list (2026-09-27) | 3,600 - 3,700 | 1,500 - 1,520 | 80 - 84 us | 1,900 - 2,170 |
| runtime `memmove` with `rep movsb` (was a byte loop; GNAT uses it for most array copies) (2026-09-27) | 4,590 - 5,110 | 1,500 - 1,530 | 82 - 98 us | 1,980 - 2,150 |
| driver to netstack through a receive ring (one-way doorbell only when netstack is idle, no call and reply per batch); 256 receive buffers posted (were 32); 127-slot ring (2026-09-27) | 4,230 - 4,510 | 1,450 - 1,530 | 62 - 96 us | 1,640 - 2,270 |
| frame-ring arithmetic proved (`Frame_Ring`), descriptor ownership proved (`Descriptor_Pool`), 64-bit checksum words, one ACK per batch after the reader's data moves (window reopened) (2026-09-27) | 4,440 - 4,670 | 1,490 - 1,540 | 80 - 98 us | 1,840 - 2,300 |
| crossing FINs finish (FIN retransmitted from CLOSING; the ACK on a resent FIN ending at RCV.NXT is taken, as Linux does); libc reuses socket buffers (2026-09-27) | 4,330 - 4,710 | 1,520 - 1,530 | 85 - 87 us | 2,410 - 2,820 |
| TX doorbell only when the driver published that it is idle (epoch, as for RX) (2026-09-27) | 4,010 - 4,630 | 1,440 - 1,520 | 82 - 101 us | 2,170 - 2,670 |
| virtio-net: TX completion interrupts only for a lone frame or two in flight (bulk sending reclaims descriptors on the loop's passes) (2026-09-27) | 4,400 - 4,630 | 1,810 - 1,830 | 81 - 95 us | 2,270 - 2,820 |
| Linux (same session, rerun) | 10,000 - 11,200 | 1,770 - 1,870 | 38 - 42 us | 3,320 - 4,030 |

One vCPU each (21:48, same session): CuBit download 1,920 - 1,980 Mbit/s,
upload 1,090 - 1,110, round trip 66 - 68 us, 2,060 - 2,170 connections/s;
Linux 8,170 - 8,790, 1,750 - 1,860, 36 - 39 us, 3,610 - 4,110. With four
vCPUs CuBit's round trip grows by ~35 us and Linux's does not: cross-CPU
wake-ups between the program and netstack (pinned with the driver on CPU 1)
are now the largest single latency cost. Numbers between 21:06 and 21:40
were taken on a loaded host and are not reported.

Findings so far (netstack cycle counters, 3.77 GHz TSC): the byte-at-a-time
receive queue cost ~18,000 cycles per 1,460-byte segment (now ~3,400);
building and submitting one outgoing segment costs ~13,000-22,000 cycles
(RecordFlux serialization, checksum, three copies); QEMU's user network
(slirp) does not offer window scaling, so uploads run with a 64 KiB window
on both systems, and every frame used to cost a netstack-to-driver IPC and
a virtio kick. With the codec, netstack spends ~6,000 cycles per received packet
(checksum ~500, header parse ~80, flow arrival ~2,500, events ~800) while
~38,500 pass per packet: it is idle 85% of the time, waiting on hand-offs
(driver call per batch, the libc reader thread, idle vCPUs waking). A
round trip spends ~84 us from netstack's write to its read of the answer
and ~42 us in the program's libc (reader thread, then program thread).

**Per-packet profile (2026-09-27, one vCPU, download).** A kernel built
with `LATENCY_TRACE=1` and `tests/net-bench/trace-profile.py` showed the
download CPU-bound: netstack 45% of the CPU, virtio-net 43% (mostly in
its own code), the program 8%, idle 4%. netstack's cycle counters
(`Netstack_Profile`, compiled out unless enabled) per packet: checksum
~500, header parse 90, lookup 55, flow arrival ~2,200, events ~1,100,
batch end ~750, and ~1,200 for scanning every timer on every message.
The timers now run once per millisecond tick (~30). The runtime's
`memmove` was a byte loop; with string instructions flow arrival fell to
~500-900 and the driver's frame copy likewise. One vCPU: download 2.0 ->
2.9 Gbit/s.

**Receive ring (2026-09-27).** The driver used to hand each batch to
netstack with a call and wait for the reply; the reply handed the CPU
back to the driver before the woken program ran (~5 us of each round
trip). Received frames now go through a ring in the packet grant, as sent
frames already did: the driver rings a one-way doorbell only when
netstack published that it is idle, and netstack rings the driver's only
when the ring was full. One vCPU: download 3.1 - 3.3 Gbit/s, round trip
51 - 59 us. With four vCPUs download stayed at 4.2 - 4.5 (the call version
measured 4.6 - 5.1 in one session); round trips vary with where the
program runs relative to netstack (62 - 96 us).

Posting 256 receive buffers exposed a driver bug: TX descriptors were
indexed by buffer number, which ran past the 256-entry descriptor table
once receive buffers filled it (unchecked code: silent corruption). TX
descriptors are now 0 .. 79, mapped to their buffers, and descriptor ids
returned by the device are bounds-checked. That the driver runs without
run-time checks and without a proof is exactly this risk; proving its ring
handling is on the list.

**Scheduler stalls (2026-09-27).** A one-vCPU trace of the download
showed the CPU idle for 90 - 114 us with netstack or the driver ready, each
time until a late timer; four stalls were half the snapshot. Building the
kernel with `ONESHOT_SCHEDULING=0` or `WAKEUP_SCHEDULING=0` (the kernel's
experimental wake-aware scheduling, on by default) gave, same session: one
vCPU download 3.2 - 3.3 -> 3.6 - 3.8 Gbit/s and round trip 52 - 62 -> 46 - 50
us; four vCPUs 4.3 - 4.6 -> 4.7 - 5.1 Gbit/s and 82 - 92 -> 66 - 75 us. The
scheduler is the kernel owners' design; the data and trace are reported to
them (coordination/networking.md). Numbers above use the default kernel.

**Connection churn (2026-09-27).** 3,000 short connections per round
exhausted all 128 connection slots. A capture showed why: the client's FIN
crossed the server's, the connection entered CLOSING, and the server's ACK
of our FIN arrived only on its resent FIN, a segment ending exactly at
RCV.NXT that RFC 9293 drops whole; CLOSING then never retransmitted our FIN
(the proved engine's `Next_Segment` precondition left CLOSING out), so
every such connection held its slot for the two-minute orphan timeout.
Both are fixed (proved; `tests/net-tcp` README) and the libc's per-socket
buffer leak that followed (ENOMEM after thousands of sockets) is gone:
9,000 connections per run complete. The rate falls across rounds (about
2,640, 2,160, 1,700 per second), and Linux's does too under the same load
(3,507, 2,414, 1,719; `linux.sh` with `NET_BENCH_CFLAGS="-DCONNECT_ONLY
-DCONNECTS=3000"`): that decline is the host's user-mode network (slirp),
not either guest.

**CuBit as a server, and a regression check (2026-09-27).** After the
connection declarations, channel arenas, listeners and the libc's listening
sockets, `net-bench` gained a serve workload: the guest listens (the libc's
bind, listen and accept over a netstack listener) and the host connects in.
Same session, KVM, 4 vCPUs, three rounds:

| Workload | CuBit | Linux |
|---|---|---|
| download, 64 MiB | 4.16 - 4.19 Gbit/s | 8.7 - 9.6 Gbit/s |
| upload, 64 MiB | 1.64 - 1.73 Gbit/s | 1.63 - 1.77 Gbit/s |
| 1-byte round trip | 97 - 101 us | 44 - 47 us |
| connect + 1 byte + close | 2,130 - 2,470 per second | 2,480 - 3,610 per second |
| accept + 1 byte + close (serve) | 3,450 - 3,850 per second | 6,240 - 7,230 per second |
| serve 64 MiB to the host | 1.54 - 1.65 Gbit/s | 1.58 - 1.61 Gbit/s |

Linux's download was lower this session than on 2026-09-26 (8.8 - 10.8),
and CuBit's ratios to it are where they were: no major regression from the
completeness work. Serving bulk data matches Linux; accepting runs at about
half its rate.

**Ring index costs (2026-09-27).** Two techniques from lock-free SPSC
ring tuning, measured as two bench-net runs before and two after, same
session:
- Positions are a mask, not a division (sizes are powers of two). It is
  proved as before, and a mutant masking with the size itself is killed.
- Each side reads the peer's index only when its own view is not enough:
  - The libc reads it when the cached space or data is less than the
    request.
  - netstack reads it only when something of its own is still unconsumed.
    Readiness counts unread bytes, so a stale view there reported a ready
    channel that was not. That added a spurious WAIT per round trip, about
    +15 us, until it was fixed.

Upload rose about 4% (1.61 - 1.75 -> 1.68 - 1.80 Gbit/s). Everything else
moved less than the run-to-run spread (round trip 92 - 110 -> 85 - 108 us).
The index traffic is nanoseconds; wake-ups are still the microseconds.

**Asynchronous close (2026-09-27).** The libc now submits SHUT instead of
calling it. A closed socket keeps its buffer and wait bit until the
completion arrives, and a listener's close stays synchronous because it
must drain arrivals afterwards. Two runs:
- connects 2,080 - 2,440 per second, unchanged;
- accepts 3,700 - 4,080 per second (was 3,500 - 4,000);
- round trip 84.5 - 91 us.

The one synchronous call per connection was not the limit. A connection
costs about 425 us against Linux's 300, most of it the handshake's trips
through wake-ups.

**virtio-net: what the device returns (2026-09-27).**
- Receive buffers now have the same proved ownership as transmit
  descriptors (`Descriptor_Pool`). A buffer id from the device's used ring
  is read and posted again only if the device holds that buffer.
  - Before, any id was re-posted, so a repeated id put one buffer in
    the device's hands twice.
- Both queues take only as many used entries as the device holds
  (`Virtqueue_Index`, proved). Before, a used index far ahead made the
  driver walk up to 65,535 stale entries.
- The driver's remaining code is glue around these proved checks.

**Channel targets (2026-09-27).** netstack parsed the client's target
text in unproved code without run-time checks.
- An IP literal's octet accumulated without a digit limit and wrapped, so
  `10.0.2.4294967298` read as `10.0.2.2`. The scope check still applied
  to the parsed address, but the connection went somewhere other than what
  was written.
- Empty octets and text after the port were accepted too.

The proved `CuBit.Net_Locator` (tests/locators) now does all of it, on the
shared locator syntax, including bracketed IPv6 literals.

**IPv6 groundwork (2026-09-27).** Every address is one 16-byte type
(IPv4 mapped, no family tag; tests/locators). Three new proved units in
`userspace/net/src` have 8 of 8 mutants killed:
- `IPv6_Header`: an IPv4-mapped address never enters or leaves on IPv6,
  stated as its own postcondition;
- `ND_Message`: Neighbor Discovery, requiring hop limit 255, with an option
  walk that ends;
- `Neighbor_Cache`: hardened like ARP, so unsolicited advertisements
  (including Override) never change an entry.

Two more proved units cover autoconfiguration, with 5 of 5 mutants killed:
- `RA_Message`: router advertisements, only from the link;
- `SLAAC_Table`: addresses used only after duplicate detection, never
  when duplicate, and the two-hour rule against forged lifetimes; stable
  RFC 7217 identifiers.

**Connection lookup is proved (2026-09-27).** netstack now finds a
segment's connection through the proved, hashed `Connection_Table`:
- keys are 4-tuples of 16-byte addresses (IPv4 mapped), hashed by SipHash
  under a boot secret;
- handles carry generations, so a stale one never reaches a reused slot;
- the per-packet scan of 128 slots in `TCP_Slots` is gone;
- netstack's parameters (64 connections, 64 buckets of 8) are a proved
  instance (`tests/net-tcp/table_netstack.ads`).

Benchmarks were unchanged within noise (download 4.1 - 4.5 Gbit/s, round
trip 84 - 94 us, accepts 3.8 - 4.2k per second).

**IPv6 on the link (2026-09-27).** netstack's `IPv6_Link` wires the proved
units in. Every received byte passes `IPv6_Header`, `ND_Message` or
`RA_Message`, and every address and neighbor decision is `SLAAC_Table`'s or
`Neighbor_Cache`'s.
- It forms a link-local address, and a global one per advertised prefix,
  each only after duplicate address detection. It answers solicitations and
  echo requests for its addresses.
- Tested natively under QEMU's IPv6, and now required by the
  network-authority test: link-local and `fec0::/64` addresses configured;
  the router resolved through Neighbor Discovery; an echo exchanged with it.
- Not yet: TCP and UDP over IPv6 (the connection record still holds an
  IPv4 peer), routing beyond the link's router, and extension headers
  (packets with any are dropped).
- The glue itself is proved too (level 1, through
  `tests/tcp-session/ipv6_link_proof.ads`). It has no run-time errors for
  any frame or clock value, and the address and neighbor tables keep their
  invariants. Every frame it sends is `IPv6_Frame.Emittable`: that is the
  precondition of its `Send`, so no IPv4-mapped address can leave on IPv6.
  Checksums come from the proved `Internet_Checksum`. What remains
  unproved at this boundary is one overlay of the driver's buffer in
  netstack, plus the assumption that `Send` and `Log` leave the link's
  state alone. Mutants that drop the address guard or a checksum fold are
  rejected by the proof.

**DNS responses are proved (2026-09-27).** netstack's hand-written
response walk (raw-address reads) is replaced by `DNS_Response.Parse`
(`userspace/net/src`, level 1). The proof covers:
- no access outside the message and a terminating walk, for any bytes;
- an accepted message is a response with one A/IN question;
- a returned address is the four data bytes of an A/IN answer of length
  four, inside the message.

netstack now also requires the question's name to match the pending
query's before it takes the answer (before, it matched on ID and port
first). Checks:
- hosted: a CNAME chain, truncation, damaged counts and lengths, and
  20,000 random tails (`tests/net-tcp`);
- four mutants rejected by the proof;
- natively, `wget-https` resolves example.com over the internet through
  it and completes an HTTPS GET.

This is the manual lane; no deterministic native DNS test exists yet.

**Where the stalls are not (2026-09-27).** Scheduler probes on one vCPU
put each ~100 us stall right after a virtio-net interrupt readies the
driver, ending at a timer interrupt 40 - 160 us late. Experiments, each
reverted:
- a 20 us wake rotation instead of 100: stalls remain, and download falls;
- pinning QEMU's vCPU thread to its own host core: stalls remain;
- polling 50 us before `hlt` (as Linux's guest haltpoll does): no change,
  at one vCPU or four.

The host's own accounting (`/proc/<vcpu>/schedstat`) shows the vCPU
thread waiting runnable for 5.7 s against 14.75 s running, over 285,000
waits (about 20 us each): 28% of its runnable time on a lightly loaded
host. The pinned core was not isolated from other host programs. Not yet
known: whether that wait causes the stalls, and why a Linux guest suffers
less from it.

The RecordFlux TCP parser, measured on the same 1,480-byte segment and
built as netstack is: 879 ns per parse; the proved codec: 10 ns. RecordFlux
stays the specification and the oracle: `tests/net-headers` finds no
disagreement over 360,000 built, mutated and random segments.

The largest gains came from sending fewer messages, not
from faster ones; round-trip latency and connection rate are next, and
depend on how many processes and wake-ups one exchange crosses (program,
its libc reader thread, netstack, driver).

## Layout

A tree of its own, not the GNAT runtime (`userspace/runtime/gnat` stays the
runtime; see docs/development-backlog.md on separating the public CuBit
runtime from GNAT internals):

    userspace/net/
      specs/        RecordFlux specifications (*.rflx), the source of truth
      generated/    pristine `rflx generate` output (never hand-edited;
                    README records the RecordFlux version and any library edit)
      src/          SPARK: TCP, IPv4 reassembly, ARP cache, DNS resolver,
                    timers, buffers
      tests/        proofs (gnatprove), mutation checks, an interleaving or
                    scenario explorer for TCP, differential tests against a
                    host stack, fuzzing of the parsers
    userspace/services/netstack/   the service: IPC, scopes, driver glue

## Phases

1. **Specs first.** Done so far: Ethernet, ARP, IPv4, ICMP, UDP, TCP and
   protocol numbers from RecordFlux's examples at v0.26.0, generated with
   `--ignore-unsupported-checksum` (the IPv4 checksum aspect has no Ada
   generator in 0.26.0; netstack checks it). They replace netstack's
   earlier generated units, whose modified specifications were never
   committed; upstream TCP adds option parsing (MSS, window scale, SACK,
   timestamps). Still to do: the proof run of the generated units in CI, a
   drift check, and the first recorded change: RFC 9293 requires receivers
   to ignore the reserved bits (one is now AE, Accurate ECN), while the
   upstream spec rejects non-zero values.
2. **TCP core** in `userspace/net/src`: a new connection engine with
   retransmission and proper windows, proved; run beside the old code in
   hosted tests, then switch netstack over.
3. **Data path**: batching, zero copy, delayed ACK, window scaling;
   benchmarks.
4. **DNS** through RecordFlux with caching and retries; IPv6 afterwards.

Proved properties, assumptions and regression-tested behaviour are kept
apart in the documentation of each phase.
