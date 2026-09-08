# Network authority regression

Run from the repository root, using the Nix/Alire toolchain:

```sh
nix develop -c make -C kernel test-network-authority prove-network-authority
nix develop -c make -C kernel cubit_kernel user_runtime network-check capability-test procmgr devmgr netstack
nix develop -c tests/headless/run.sh --test network-authority --timeout 40 --keep-logs
```

The hosted test checks scope encoding/validation, CIDR and port boundaries,
direction, DNS permission, containment, owner/tag separation, stale tags and
bounded-table exhaustion. Its runtime assertions are Linux-hosted only.
Focused GNATprove checks the scope and grant ADTs, not the complete netstack.

The headless test uses a temporary copy of the ext2 disk and an alternate init
profile. It launches the same application without and with explicit boot
approval, exercises denied and accepted bind/close operations, forged policy
tags, configuration denial, outbound subnet/port/DNS restrictions and stale
shared-memory generations. An approved connection to 10.0.2.2:18443 exchanges
PING/PONG with a **loopback-only** host TCP peer. No internet service is used.
It also verifies that retired syscall 23 and an authorityless `capSubmit` fail.

This is not an incoming TCP acceptance test: bind currently reserves the
authorized listener, while passive receive/accept integration is still pending.
The peer uses local port 18443, which must be free for the test.
