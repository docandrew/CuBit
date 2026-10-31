# Allocated passive-query transport codec

This suite tests the four-word ACPI service request/reply envelope allocated by
coordination/mail/archive/to-acpi/20261027-004500-claude-framing-approved.md.
It does not run a native service or validate a grant, peer, payload or AML method.

Run in the pinned Nix development environment, with the designated TMPDIR:

```
export TMPDIR=/home/doc/cubit-build-tmp
bash tests/acpi-query-codec/run.sh
```

`--verify` runs the selected native semantic checks and level1/2 pure codec
proofs (CVC5,5seconds per goal,180seconds per command).

`--validate-only` checks the source catalog without compiling or executing.
The normal runner uses separate checked and release builds. Explicit test checks
remain active with `-gnatp`; checked builds additionally enable Ada assertions
and overflow checking. Commands are nice19/j1, compile-bounded to180seconds and
runtime-bounded to30seconds, with64MiB stack and core dumps disabled. Every command
has retained inputs, tool hashes, arguments, status and bounded output.

The matrix checks canonical label29/length4/zero flags and reserved fields,
all other8-bit lengths and flag bytes, all nonzero16-bit reserved headers,
each reserved payload bit, full unsigned64-bit reference/extent round trips,
all13 allocated statuses, signed CCL reply count bounds, minimum256-byte reply
header, declared-extent checks, error output clearing and invalid encoder input.
It composes decoded requests with actual query admission and exercises a high
64-bit sender and encoded reference. Structural acceptance is not authority.

A selected malformed label29 packet remains selected even when decoding fails,
so a dispatcher cannot accidentally treat it as an unrelated request. The
native adapter copies raw header values before validation. It neither receives
messages nor grants access. It binds the existing native input decoder and
saved-reply encoder generic signatures; the native owner still must enforce
sender/tag checks, exact-owner acquisition, private copies, single ownership,
return-before-reply and uncertain-cleanup/no-replay rules.

The reply encoder rejects invalid status/count combinations with
Invalid_Response and zero counts. It has no admitted extent argument: the owner
must establish Written<=the admitted extent. Client-side decoding checks that
bound. The inner ACPIREP2 payload needs separate typed validation and request
correlation after transfer and retirement.

Formal proof scope is the pure ACPI_Query_Transport unit's runtime safety and
stated framing/round-trip contracts. The native adapter, request dispatcher,
loan/session lifetime, AML interpreter and entire ACPI service are outside that
proof. Native semantic compilation produces no objects and establishes no boot,
IPC execution, QEMU or laptop result.
