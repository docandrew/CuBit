# Hosted live-field Load controls

These synthetic AML fixtures exercise the modern Boolean Load result through its
namespace effect (WVAL=42). The hosted byte provider is simulated memory/IO;
its default named authority and exact IO counts are tested. This is not native
hardware acceptance or the complete ACPICA suite. Direct OperationRegion Load
remains unsupported; its expected refusal is explicit, not counted as AML PASS.

Inside the pinned Nix shell, export TMPDIR=/home/doc/cubit-build-tmp and run:

    python3 tests/aml-live-field-load/run.py --output /absolute/new/private/output

The default matrix runs18 checked cases and19 production cases, with explicit
240s-j2 builds and12s/24GiB address-space/128MiB stack process limits. Checked
528-bit roundtrip retains an earlier12s timeout and is skipped here; production
528-bit remains active under12s. No capraise or changed expectation clears it.
A completed run reports matched expected outcomes separately from raw AML PASS;
quota0, denied provider, bad checksum, missing target and direct-region refusal
remain visible. Source and tool hashes are checked before/after every profile.

Successful Load must capture a table once, preserve source bit/byte bounds and
load distinct valid tables without repeating a duplicate. Missing target must
fail before reads, denial performs no IO, and bad checksum reads one44byte field.
The new GC fixtures clear a4096byte local before Load, then require completed
collection and at least4096 reclaimed bytes with no rejected collection. The
no-Load control requires no collection or reads. Recorded observed reclamation
is2objects/8192bytes (temporary plus copy). Aggregate counters and the no-Load
control cover the actual pre-read hook, not a hypothetical post-read callback.

No complete caller/GC absence-of-runtime-errors proof is claimed. Unchanged
external byte-memory helper proof results are reused; namespace caller and whole
GC ARO remain major open. Service and opt-in RC collector remain OFF. There is
no explicit post-refusal re-invocation/held-root census check in these fixtures.
