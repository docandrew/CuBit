# Retained local notification fanout

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop -c bash tests/acpi-retained-notify-owner/run.sh

Requires pinned GNAT/GPRbuild, Node and ACPICA tools on PATH. Inputs are hash-checked, unlisted Ada/ASL inputs refused, and compilation uses copied sources with -O0 -gnatp. Builds/runs are limited to 180/30 seconds and 24 GiB address space.

Actual snapshot/Core/device initialization creates three _INI notifications. Two modeled local consumers receive the same retained receipt and matching private Core through With_Retained_Notification, composed with the existing guarded With_Notification_Routing adapter. Partial fanout and repeated claims/ACKs preserve the exact receipt and earlier idempotent delivery. ACK can retire it only after both consumers accept. A consumer checks the Core context and rejects a foreign owner's identical numeric receipt. Empty queues, repeated accepted ACKs, and unauthorized/malformed/stale ACKs do not fabricate delivery. The final claimed receipt remains pending until ACK.

The continuation itself runs no AML, IPC, clock, dequeue or ACK. Trusted consumers must be bounded, non-reentrant, normally returning and idempotent, and must validate their own Core context. These are hosted regressions with modeled consumers; no new SPARK proof, physical hardware, native fanout activation or policy-manager behavior is claimed.
