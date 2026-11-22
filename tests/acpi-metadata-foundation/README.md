# Owning-Core metadata and context identity (hosted)

Run `node tests/acpi-metadata-foundation/run.mjs` in the pinned Nix environment
with TMPDIR=/home/doc/cubit-build-tmp, stack 64 MiB and core dumps disabled.
The suite uses release -gnatp for whole-Core compilation, one worker, nice 19,
180-second builds, 30-second executables and 5-second iASL bounds. Outputs remain
private under TMPDIR. ACPI_RESULT_DIR may select an existing private receipt
directory; ACPI_METADATA_REUSE may select this suite's previous private build
directory. Normal dependency checking remains enabled and all cases rerun.

Actual Core, request protocol, snapshot loading, device initialization and AML
Notify execution run on Linux. The explicit hosted configuration uses 64 nodes
and 16 retained roots; no native hardware/runtime path is exercised. Seventy-four
checks pass at each AML width. The suite covers uninitialized/foreign opaque
keys; owner-key stability across member initialization; metadata refusal before
snapshot completion or while a table is open; callback failure propagation;
unchanged revision/phase/import/pending observations; real queued notifications;
and an empty queue whose still-claimed receipt must remain pending until ACK.

Context equality is an owner-lifetime observation, not readiness, a node, an
address or authority to execute AML or access hardware. Metadata continuations
are trusted local callbacks; they may inspect the owning Core but must not run
AML, mutate or reenter it, and must return normally in bounded time. These tests
do not establish that arbitrary callbacks obey those assumptions. Protocol Model
equality remains a ghost contract; the executable tests check public observations
rather than claiming bitwise equality of the private model.

There is no new SPARK proof, native execution, hardware validation or whole-
service correctness claim. Reset/replacement behavior is inherited from the
existing namespace identity lifetime; this fixture tests foreign and fresh
owners and does not exercise every identity-exhaustion/reset path.

Startup anchor regression: 79 checks per AML width. A successful Execute_Frozen leaves root Describe_Node at Wrong_Phase until Initialize_Members. The early opaque context is already current, excludes the default/foreign owner, and stays current across member completion. Keep the descriptor status separate from the successful execution status. Pooled_Fields uses that context as its lifetime anchor; native body semantics are checked separately. This hosted suite does not execute native Pooled_Fields hardware code.
