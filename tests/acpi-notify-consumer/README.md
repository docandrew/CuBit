# Bounded Notify consumer policy

This hosted policy assembles a copied notification observation using exact host-owned request/session correlation. It never ACKs before explicit broker delivery confirmation. A reconnect retires the staged local delivery and never reuses local counters; exhaustion fails closed. The trusted transport must retain this state throughout completion-routing lifetime. It must not reconstruct tokens from reply words or payload PIDs.

The trusted broker keeps the provider endpoint; this is not a native CCL binding or permission to give that endpoint to handlers. Native completion routing, nominal CCL schema/import and launcher endpoint distribution remain coordinated integration work. Observer authority remains read-only. Delivery is not claimed exactly once across process crashes.

Run from the pinned Nix development environment with TMPDIR=/home/doc/cubit-build-tmp before and inside Nix:

    python3 tests/acpi-notify-consumer/run.py --validate-only
    python3 tests/acpi-notify-consumer/run.py --mode release
    python3 tests/acpi-notify-consumer/run.py --mode checked

Each build is bounded to 180 seconds; runtime to 30 seconds, with a 64 MiB stack and 1 MiB log. Source inventories, actual compiler paths, generated binder and every ALI dependency are checked.

The exact client and test bodies passed 243 checks in each profile in private worker20605 (receipt acpi-notify-consumer-final-3t1we67u). That evidence predates any subsequent shared Arg integration; this portable registration does not claim a rerun or identical whole dependency closure. The real endpoint fixture covers loss/retry, staged receipt identity, stale/denied/wrong-order replies, reconnect, malformed replies, explicit delivery and exhaustion. The zero-length path is a clearly synthetic authenticated-reply test. No proof or live-native result is claimed.
