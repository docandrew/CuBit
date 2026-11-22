# TCP building blocks: proofs and checks

SPARK units in `userspace/net/src` (docs/netstack-redesign.md). Every unit
below is proved at gnatprove level 1, has Linux-hosted executable checks
in `main.adb`, and has mutants in `mutations.sh`. The live netstack runs
on `TCP_Flow` (one per connection slot; `userspace/services/netstack/tcp_engine.ads`).

| Unit | What is proved |
| --- | --- |
| `TCP_Limits` | shared protocol constants, named after their RFC meaning |
| `TCP_Sequence` | modulo-2^32 order (RFC 9293 3.4): irreflexive, antisymmetric, total except at exactly half the space, transitive when the outer pair is within half the space; the window test agrees with the order for windows up to half the space |
| `TCP_Acceptance` | RFC 9293 3.10.7.4 acceptability; `Trim` keeps only data inside both the segment and the receive window, keeps something from every acceptable segment, and never re-keeps data before RCV.NXT |
| `TCP_RTO` | RFC 6298 estimator: no overflow; RTO always within [200 ms, 120 s]; backoff never shortens RTO and saturates |
| `TCP_Connection` | the state machine (RFC 9293 3.10.7, RFC 5961): every state change is an edge of RFC 9293's diagram (plus the resets and the one-segment ACK+FIN edge); a RST ends a synchronized connection only exactly at RCV.NXT; a SYN never moves a synchronized connection; data is delivered only in the data states (or with the handshake-completing ACK), in order from RCV.NXT and within the window; RCV.NXT advances by exactly what is delivered (+1 for FIN); SND.UNA only moves forward and never past SND.NXT |
| `TCP_Send_Queue` (generic; 64 B and 256 KiB) | segments carry exactly the bytes written at their sequence numbers; an acknowledgement frees only acknowledged bytes and leaves the rest in place; acknowledgements of unsent data are refused; retransmission after `Rewind` resends identical bytes |
| `TCP_Receive_Queue` (generic; 64 B and 256 KiB) | out-of-order reassembly: every stored byte is the one that arrived at its sequence number; everything below RCV.NXT is present; bytes already present are never altered; RCV.NXT only advances; a read returns exactly the in-order bytes, moves the rest along unchanged and frees the slots it read |
| `TCP_Congestion` | NewReno (RFC 5681, 6582, 6928): the window stays within [SMSS, 2^30] and ssthresh at or above 2 SMSS; outside recovery an ACK grows the window by at most one SMSS (by no more than it acknowledged in slow start, and in congestion avoidance only after a window's worth); fast retransmit exactly on the third duplicate, outside recovery, never for pre-loss data; a loss sets ssthresh by equation 4 and a repeated timeout does not lower it again; recovery ends exactly on an ACK covering recover; the allowance keeps data in flight within both windows |
| `TCP_Options` | option negotiation: each in effect only if both SYNs carry it (RFC 7323, RFC 2018); shift capped at 14; MSS within the peer's (or the default) and our link, less the timestamp option, never below the floor; advertised windows never exceed the buffer and fall short by less than one scale unit |
| `TCP_Timestamps` | PAWS (RFC 7323 5): refused exactly when not a RST, TS.Recent is fresh and TSval is older; TS.Recent expires after 24 days, is taken only from a segment covering Last.ACK.sent, and never moves backwards |
| `TCP_Scoreboard` (generic; 8 and 32 ranges) | SACK (RFC 2018, 6675): a byte is marked SACKed only if a block covered it; nothing SACKed is forgotten; a block is recorded whenever there is room; advancing SND.UNA moves marks and never creates them; IsLost is monotone and needs a SACKed byte above; the next unSACKed byte is found exactly |
| `SipHash`, `TCP_Isn`, `TCP_Syn_Cookies` | absence of runtime errors (SipHash is checked against the reference vectors; its security is assumed); SYN cookies (RFC 4987): a cookie is accepted in its period and the next, returns the encoded MSS, and is refused once older |
| `Chunk_Pool` (generic) | every chunk is free or owned by one owner, and only its owner writes, reads or releases it; free chunks hold only zeros, so no bytes pass between owners; per-owner counts are exact; an operation on one chunk changes no other |
| `Chunked_Send_Queue` (generic; 4 × 16 B and 64 × 4 KiB) | `TCP_Send_Queue`'s contract over pool chunks, plus isolation (no other owner's chunks, bytes or counts change); chunks are allocated as data is written and released when acknowledged; closing returns every chunk |
| `Connection_Table` (generic) | `Find` returns a slot exactly when an open connection has that 4-tuple; `Insert` refuses a tuple already open; a removed connection's handle never becomes current again (the generation advances); counts are exact |
| `TCP_Endpoint` (generic) | the state machine coupled to its chunked send queue and its receive queue. Send side: in SYN-SENT and SYN-RECEIVED only the SYN is in flight; once synchronized SND.UNA and SND.NXT are the queue's, plus one for a FIN sent after all data; accepted ACKs free exactly the acknowledged bytes; SND.NXT never moves back, and resending runs from a retransmission point between SND.UNA and SND.NXT. Receive side: text is kept at its offset, out of order included; RCV.NXT is the receive queue's contiguous edge (plus one after the peer's FIN) and the window its free space. A reset returns the chunks; nothing of any other owner in the pool changes |
| `TCP_Flow` (generic) | the endpoint with its sending policy (RFC 6298 timing with Karn's rule, NewReno): segments never exceed the MSS, the congestion allowance or the peer's window; congestion decisions count the pipe (RFC 6675), not bytes presumed lost; the retransmission timer runs whenever sequence space (data, SYN or FIN) is in flight, and restarts backed off when it fires; timeouts without progress are counted (`Exhausted`); the endpoint and the controller stay valid |
| `Timer_Heap` (generic) | the root is the earliest deadline, so `Next_Due` never reports a timer before it is due; arming and cancelling change only that timer; counts are exact |
| `Descriptor_Pool` (generic; 80, virtio-net's transmit descriptors) | every descriptor is free or in flight; `Take` hands out only a free one; a device-returned id is accepted exactly when it names a descriptor in flight (out-of-range, free and repeated ids change nothing), so no buffer is handed out twice; the free list never holds a descriptor twice or one in flight |
| `Channel_Geometry` | a channel's rings lie in the client's grant: for sizes netstack accepted at open, every send- or receive-ring position is an offset inside the acquired grant (with `CuBit.Channel_Rings` keeping positions below ring sizes, no channel access leaves the grant) |
| `Channel_Arenas` | channel arenas (one grant cut into channel buffers): a registered arena fits one grant (16 MiB); a claimed buffer lies wholly inside its arena, at its index times the buffer size; only a free buffer of the caller's own arena can be claimed, so no buffer backs two channels; an arena with a claimed buffer cannot be unregistered; handles are never reused, so a stale handle names nothing; an exiting owner's arenas all go |
| `Virtqueue_Index` | a virtqueue's used index as the device writes it: the driver takes only as many new entries as it handed the device and has not seen back (an index further ahead yields none, so stale entries are never read as new), and every ring position is inside the queue |
| `Channel_Service` | netstack's stream-channel decisions: a failed channel asks for no kick; Kick_On_Send only once all the client's data was taken, Kick_On_Receive only when data waits and the receive ring is full; a write-shutdown closes once, after the client's data, never during the handshake; after asking for a kick, netstack looks again whenever the client's index moved (no lost wakeup) and never when nothing moved |
| `IPv4_Header` | the IPv4 header (RFC 791): `Well_Formed` is exactly the accepted rule (version 4, a 20 to 60 byte header, total length within the bytes received, no fragment); every parsed field is its bytes on the wire; `Build` writes a version-4, 20-byte, unfragmented header whose length, protocol, TTL and addresses are those given, leaving the payload alone. Tested, not proved: its checksum verifies and `Parse` reads it back (10,000 random headers) |
| `Internet_Checksum` | RFC 1071 over a byte array: no overflow and no access outside the bytes for any length up to 2^17. Tested, not proved: agreement with RFC 1071's 16-bit sum and netstack's word-wise sum (`tests/tcp-session`) |
| `UDP_Frame` | UDP over IPv4 on Ethernet (RFC 768), used for every datagram netstack sends: no run-time errors for any payload up to 1,472 bytes, and every built frame is `IPv4_Frame.Emittable` (unicast source and destination). Tested, not proved (`tests/tcp-session`): ports, length and payload are those given, and both checksums verify and agree with netstack's word-wise sum for every payload length |
| `IPv6_Header` | the IPv6 header (RFC 8200): `Well_Formed` is exactly the accepted rule (version 6, the payload inside the bytes received, a source neither IPv4-mapped nor multicast, a destination neither IPv4-mapped nor unspecified); every parsed field is its bytes on the wire; `Build` writes a header `Well_Formed` accepts and `Parse` reads back, and cannot be asked to write a mapped address, so a mapped address never enters or leaves on IPv6 |
| `ND_Message` | Neighbor Solicitation and Advertisement (RFC 4861): an accepted message had hop limit 255 (cannot have crossed a router), type 135 or 136 with code 0, and a target that is its bytes 8 .. 23 and not multicast; the option walk ends (a zero-length option rejects the message) and never reads outside it |
| `Neighbor_Cache` | the IPv6 neighbor cache, hardened as `ARP_Cache`: only an advertisement answering our own solicitation, or a solicitation to us, is learned; an unsolicited advertisement (even with Override) changes nothing; a resolved link address never changes; IPv4-mapped, multicast and unspecified neighbors are never learned; at most one entry per address; an entry changes only for its own address |
| `RA_Message` | Router Advertisements and their prefixes (RFC 4861 4.2, 4.6.2): accepted only with hop limit 255 and a link-local source; the option walk ends; a returned prefix is an autonomous /64, not link-local and not multicast (stated on its own), with its preferred lifetime no longer than its valid one |
| `SLAAC_Table` | stateless address autoconfiguration (RFC 4862) with RFC 7217 stable identifiers: an address starts Tentative and becomes usable only after its detection period passes with no conflict; a Duplicate never becomes usable; the two-hour rule (5.5.3 e): no advertisement shortens a valid lifetime below the lesser of what remained and two hours; at most one address per prefix |
| `DNS_Response` | a DNS response to one A/IN question: no access outside the message and the answer walk ends, for any bytes; an accepted message has QR set and one question for A/IN; a returned address is the four data bytes of an A/IN answer of length four, inside the message (`DNS_Name.Read_Name` now also proves it moves forward) |
| `TCP_Header` | the TCP header on the wire: each parsed field is its bytes (big-endian); writing then parsing gives back each field; writing touches only the header; SYN options written exactly (MSS, NOP, window scale). The acceptance rule is RFC 9293 3.1's |

Some properties are definitional (an expression function's result is its
own specification) and have no mutant: `Peer_Window`'s SYN rule, IsLost's
thresholds, and SYN cookies' binding to the counter across a wrap (a
SipHash property).

Run (Nix shell):

    gprbuild -P net_tcp_tests.gpr && build/main          # executable checks
    build/tcp_scenarios                                   # hosted connection lifecycles
    build/tcp_endpoint_sim                                # two endpoints over a lossy link
    gnatprove -P net_tcp_tests.gpr -j0 --level=1 --checks-as-errors=on
    bash mutations.sh                                     # proofs must reject bugs
    build/tcp_flow_sim                                    # the same with congestion control and time
    build/tcp_flag_storm                                  # all SYN/ACK/FIN/RST combinations in every state
    DRY=1 bash mutations.sh                               # each mutant still applies

`mutations.sh` applies plausible bugs and requires each to fail a proof,
plus a behaviour-preserving control that must still prove. The harness
and gnatprove keep their scratch files under `build/`; set `TMPDIR` to a
directory in the repo for gnatprove runs outside it too.

Results, 2026-09-27: 4269/4269 checks proved at level 1 (sequence
numbers are modelled as integers modulo 2^32 for proof; see
`TCP_Sequence`). Mutants: 81/81 killed, control surviving. Mutation
testing found
two real specification gaps, both fixed: the receive queue's `Read` did
not promise to clear the presence bits it consumed, and the connection
table's `Remove` did not require the generation to advance (a stale
handle could have come back to life when its slot was reused).

Live testing (2026-09-27, 3,000 short connections per round under QEMU's
slirp) found a liveness gap the safety proofs could not: when FINs cross,
the connection waits in CLOSING, and the endpoint's `Next_Segment` (and
`TCP_Flow.Send`) did not allow CLOSING, so the FIN was never retransmitted
and the connection held its slot until the orphan timeout. CLOSING is now
allowed there (proved), and `tcp_flow_sim` covers crossing FINs whose ACKs
are lost. Separately, `TCP_Connection.Arrive` now takes the acknowledgement
of an otherwise unacceptable segment that ends exactly at RCV.NXT (the
peer's FIN resent with the ACK of ours), as Linux does; RFC 9293 would
drop it whole and wait for our retransmission. Its text and FIN are still
ignored, and the ACK passes the usual RFC 5961 checks (`tcp_scenarios`).

The simulations found two design errors the unit proofs could not, since
each unit met its contract: rewinding SND.NXT on a timeout made valid ACKs
for data sent before the timeout look like acknowledgements of unsent data
(fixed with a retransmission point; SND.NXT only moves forward, as in
Linux), and counting presumed-lost bytes as in flight blocked their own
retransmission (fixed by counting the pipe). Wiring the flow into netstack
found a third: an ACK for data stopped the retransmission timer while a
lone FIN was still unacknowledged, so a lost FIN was never resent (the
timer now covers all sequence space in flight; `Arrive`'s postcondition
says so and a mutant checks it).

`tcp_flow_sim` runs a whole connection as netstack drives it (replies,
pumping, timers) over a link losing 10% of segments, with the first SYN and
the closer's first FIN (sent while its data is still in flight) forced
lost: 2000 bytes arrive in order, both sides close (TIME-WAIT and CLOSED),
no timer is left running, and most losses are recovered without the timer
(2026-09-26: 658 segments, 78 lost, 17 timeouts).

`tcp_flag_storm` sends segments no legitimate stack sends ("Christmas
tree": SYN, FIN, RST and ACK together, with and without data) to a
connection in each of the eleven states: all 16 combinations of the flags
that reach the state machine, six sequence numbers (exact, in window, stale,
far) and five acknowledgements (old, current, unsent). 2026-09-26: 10,560
segments; every transition allowed by RFC 9293's diagram; a RST honoured
only exactly at RCV.NXT (640 times); no SYN moves a synchronized connection;
the full tree off RCV.NXT changes nothing and delivers nothing; a listener
opens only on a bare SYN. These restate proved properties as run-time checks
over hostile input; the proofs are what cover all inputs.

gnatprove analyzes what `main.adb` reaches, so every unit to prove is
referenced from it; realistic sizes are withed but unused. Delete
`build/gnatprove` (or run `gnatprove --clean`) before trusting a summary:
gnatprove reuses stored results for units it considers unchanged, and on
2026-09-28 a clean run showed three checks that an incremental summary had
reported proved (SLAAC's two-hour rule, the connection table's bucket
invariant after Insert, and the endpoint's pool isolation after a segment).
Each was restructured (stepping assertions, an `Assert_And_Cut`). Three
units whose checks prove alone but can miss level 1's 1 s budget in a
saturated `-j0` run get level 1's provers with `--timeout=60
--memlimit=4000` (per-file `Proof_Switches` in `net_tcp_tests.gpr`, as
SPARKTLS does); every other unit is plain `--level=1`.

Results, 2026-09-28 (clean run): 5,125/5,125 checks proved; mutants
146/146 killed, control surviving (the last survivor showed that
`IPv4_Header.Build_Header` did not promise a non-fragment; it does now).

Not yet covered: SACK-driven recovery (the scoreboard exists but the flow
does not use it yet), RACK-TLP, CUBIC, delayed ACK and Nagle, persist and
keep-alive timers, the chunked receive queue, and the engine that runs many
flows over one pool, table and timer heap inside netstack.
