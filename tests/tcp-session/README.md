# TCP session and listener tests

Run from the repository root using the project's Nix/Alire toolchain:

```sh
nix develop -c make -C kernel test-tcp-session
nix develop -c make -C kernel prove-tcp-session
nix develop -c make -C kernel netstack
```

Assertions are enabled only in the Linux-hosted test executable. Native netstack
builds remain optimized, without `-gnata`.

The tests exercise the actual service's pure `TCPSession` state machine and the
`TCP_Listeners` ownership/backlog ADT and `Network_Channel_Handles`. They cover active and passive handshake
ACK validation, duplicate SYN, receive credit, sequence wrap, peer half-close,
data plus FIN on the final handshake ACK, reset sequence validation, listener
ownership, duplicate bind rejection, stale listener handles, bounded backlog,
accept readiness, and expiry/close cleanup. Channel identity tests cover
owner/tag separation, zero and oversized handles, 1,000 reuses of one slot,
stale-handle rejection, table exhaustion, and recovery. TCP reservation tests
distinguish CLOSED protocol state from permission to reuse a still-owned slot.

GNATprove checks absence of run-time errors, initialization, and the reported
termination obligations in these three packages. This is **not** a proof of TCP
protocol correctness, listener policy, the pointer-based packet IO boundary,
or the complete netstack service. Behavioral claims above are regression tests.

The listener ADT is connected to native `NET_BIND`/accept/close and passive receive.
The channel identity table and connection reservations are used by the native
outbound path today. These are hosted tests; `tests/network-authority` covers
native IPC, inbound TCP, and channel reuse. The focused proof does not analyze
the pointer/IPC-based main service integration.
