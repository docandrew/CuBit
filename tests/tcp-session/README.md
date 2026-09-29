# TCP session and listener tests

Run from the repository root using the project's Nix/Alire toolchain:

```sh
nix develop -c make -C kernel test-tcp-session
nix develop -c make -C kernel prove-tcp-session
nix develop -c make -C kernel netstack
```

Assertions are enabled only in the Linux-hosted test executable. Native netstack
builds remain optimized, without `-gnata`.

The tests exercise the service's `TCP_Slots` connection slots, the
`TCP_Wire` option parser, the `TCP_Listeners` ownership/backlog ADT and
`Network_Channel_Handles`, plus netstack's IPv4 ICMP (`IPv4_ICMP`, through
`ipv4_icmp_proof.ads`: every frame it sends is `IPv4_Frame.Emittable`, with
unicast source and destination, so it is never a broadcast amplifier), its
IPv6 link glue (`IPv6_Link`, through
the instance in `ipv6_link_proof.ads`) and the proved `Internet_Checksum`. The
IPv6 proof covers every frame and clock value; every frame sent is
`IPv6_Frame.Emittable`. The checksum is compared with RFC 1071 and netstack's
word-wise sum for every length up to 1,600 at four alignments. (The TCP protocol itself is the proved engine in
`userspace/net/src`, tested and proved in `tests/net-tcp`.) They cover listener
ownership, duplicate bind rejection, stale listener handles, bounded backlog,
accept readiness, and expiry/close cleanup. Channel identity tests cover
owner/tag separation, zero and oversized handles, 1,000 reuses of one slot,
stale-handle rejection, table exhaustion, and recovery. Slot tests check that
an owned or still-closing slot is not reused. Option tests cover Linux's SYN options (MSS, SACK-permitted, timestamps,
window scale), NOPs, End of Option List, and truncated or malformed lengths.

GNATprove checks absence of run-time errors, initialization, and the reported
termination obligations in these packages. This is **not** a proof of TCP
protocol correctness (that is `tests/net-tcp`), listener policy, the pointer-based packet IO boundary,
or the complete netstack service. Behavioral claims above are regression tests.

The listener ADT is connected to native `NET_BIND`/accept/close and passive receive.
The channel identity table and connection reservations are used by the native
outbound path today. These are hosted tests; `tests/network-authority` covers
native IPC, inbound TCP, and channel reuse. The focused proof does not analyze
the pointer/IPC-based main service integration.
