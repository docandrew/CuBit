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
new `TCP_Listeners` ownership/backlog ADT. They cover active and passive handshake
ACK validation, duplicate SYN, receive credit, sequence wrap, peer half-close,
data plus FIN on the final handshake ACK, reset sequence validation, listener
ownership, duplicate bind rejection, stale listener handles, bounded backlog,
accept readiness, and expiry/close cleanup.

GNATprove checks absence of run-time errors, initialization, and the reported
termination obligations in these two packages. This is **not** a proof of TCP
protocol correctness, listener policy, the pointer-based packet IO boundary,
or the complete netstack service. Behavioral claims above are regression tests.

The listener ADT is not yet connected to native `NET_BIND`/`NET_ACCEPT` IPC.
These are hosted foundation tests, not an end-to-end inbound networking test.
