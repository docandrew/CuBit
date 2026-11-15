# Local HID/I2C inventory events

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop -c bash tests/acpi-local-hid-events/run.sh

The concrete HID_Devices.With_Local_Events child uses the same private request Core/discovery/resolver and shared local runtime. Complete bounded inventories contain copied device/controller identities and resource facts; they grant no hardware access or readiness. Maximum_Devices and Maximum_Resource_Bytes are explicit. One operation budget covers all searches and probes. A retained CRS is copied once without AML replay, or cleaned once on capacity refusal. Uncertainty or a stopped owner preserves retained cleanup state; consumer stop cannot retire backing.

394 checks across18 processes cover normal/empty inventories, small buffers, device limits, fuel limits, missing controller sources, self-generated Notify, source loss and runtime stop while CRS is retained. Actual Core/request/discovery/resolver/runtime/routing execute with modeled clock/provider/health/fact callbacks. Root counts verify normal/refused capacity cleanup and retention on stopped owner; no DSM or further AML follows those refusals. Inventory publication is deferred until routing completes; self-Notify invalidates without an automatic retry loop.

Pinned GNAT/GPRbuild, Node and ACPICA are required. Inputs are hash-checked and copied, unlisted Ada/ASL refuses, and binary/non-binder ALI hashes/profiles are recorded. Whole-Core functionality uses -O0 -gnatp, one worker,180-second/24GiB compilation and30-second execution limits. Repair-disabled ACPICA checks status/DSM registers, equal complete CRS buffers and self-Notify in six executions; this is fixture semantics, not an ownership policy oracle. Selected native SDK bindings are checked separately, with clock/health deliberately refusing. No new proof/native/hardware execution is claimed.

Callbacks must be serialized, bounded, normally returning, non-reentrant and must not mutate aliased state. No producer interleaves with Collect or claims/dequeues/ACKs receipts there. All consumers route before ACK. Report_Publication stages copied facts; Main independently checks actual owner quiet before using the existing logger. The launcher must separately validate current resources, dependencies, device lifetime and controller authority before any handoff. Cached descriptions do not establish those conditions.

Retirement observation regression adds fresh/receiving/complete/stopped checks and a dedicated real CRS retain/cancel sequence. The read-only protocol observation preserves revision, reports the held root, and clears only after actual cleanup. This does not alone authorize native hardware retirement.
