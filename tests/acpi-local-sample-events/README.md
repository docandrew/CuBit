# Serialized local sample event owner

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop -c bash tests/acpi-local-sample-events/run.sh

Requires pinned GNAT/GPRbuild, Node and ACPICA on PATH. Sources are hash-checked and copied; unlisted Ada/ASL inputs refuse. The actual request/Core/lid-cache/local-runtime/guarded-notification pipeline runs734 checks in17 cases for each AML width. Provider, clock, source-health and publication callbacks are modeled; the early-start case tests lifecycle guards without loading AML. Builds use -O0 -gnatp with180-second/24GiB limits; execution is limited to30seconds. Compiler profiles and binary/ALI hashes are recorded.

Collection opens one finite window, completes retained cleanup, and defers publication. Pending queues AND held receipts fence publication. Every consumer must route before guarded ACK. Query-generated notifications invalidate the collection without triggering repeated AML; additional receipts from a later external source coalesce one resample. Metadata is read through the same private Core and collection-time FIFO count, assuming no producer interleaves with Collect and callbacks never claim/dequeue/ACK. The external scenario exercises real AML Notify and a second actual AC-source query window, not synthetic receipt data.

Cases include normal/absent-family publication, early/foreign/stop refusal, blocked fanout, missing/regressing/overflowing clocks, delayed publication without freshness renewal, collection/window failure and source loss during/between phases. The publication/logging hook is called only after successful window close and with idle runtime, no pending notifications and healthy sources. No failure logging occurs inside the controller. Callback exceptions, reentry and concurrent execution are outside this serialized trusted-callback contract.

Native wiring must use the EXACT same Core/runtime child, actual source/loss/overflow/dependency health, and the same microsecond clock domain as the runtime/Field window owner. The SDK fixture deliberately refuses clock/source health and performs no activation. Stop does not cancel work, release backing or prove native retirement. No new SPARK proof, native event-loop execution, hardware test or battery/HID event-owner port is claimed.

ACPICA with repair disabled matches16 typed integer results across four executions and the relative Notify-handler count (self-notifying fixture twice the baseline). This validates AML fixture behavior, not the event policy. Raw tool diagnostics remain in logs.
