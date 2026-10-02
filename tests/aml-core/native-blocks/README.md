# Native ACPI bulk request tests

Run through Nix, with the hosted service's checked-contract stack allowance:

```sh
nix develop -c bash -c 'set -e; ulimit -S -s 65536; cd kernel; alr exec -- gprbuild -p -P ../tests/aml-core/native-blocks/blocks.gpr; ../tests/aml-core/build/blocks/block_tests'
```

This target uses mocked grant acquisition/return with the actual ACPI request,
endpoint and native block-import code. It does not emulate kernel authentication.
The standard AML runner already includes this target. The adapter is native
boundary code, not itself SPARK-proved; proofs of the pure core are separate.

Label 8 requests a bulk table import with four words:

1. Current snapshot revision.
2. Grant generation in bits 32..63, global slot in bits 0..31; unused slot bits
   must be zero. Generation zero is invalid.
3. Positive table identity, representable as Ada Positive.
4. Table kind in bits 32..63 (0 DSDT, 1 SSDT, 2 Description), exact byte length in
   bits 0..31 (36 through the server instance's Table_Byte_Limit, which must fit
   Ada Positive). The current executable still selects a 65536-byte default.

Length must be four, flags/reserved zero. Metadata carries identity, never
addresses or authority. The kernel-received stamp must match the configured
provider tag; Provider_Slot is trusted startup configuration, not payload.
All other labels continue through the scalar endpoint dispatcher.

Malformed or unauthorized requests acquire no grant. Acquisition failure returns
Denied without changing service state. If import ran and grant return failed,
its service reply is retained and the adapter remains pending: retry only the
return, never the import. The serialized future loop must keep that adapter alive
and service its cleanup state. Successful import owns its own retained table copy.

2026-10-01: 104 checks pass, covering authorization, malformed fields, stale
revisions, phase checks, acquisition failure, maximum legal grant reference,
cleanup failure, no repeated import, and scalar dispatch. The unified
ACPI_Native_Endpoint also compiled against the real runtime in the private
library snapshot /tmp/cubit-acpi-block-wire-native-ixye1073. All 404 inputs matched
the checkout afterward. Native compilation is not a launch, receive-loop test,
proof of the adapter, or hardware test. Logs: /tmp/cubit-acpi-block-wire.log and
/tmp/cubit-acpi-block-wire-native.log.

Runtime-capacity follow-up (verified): the adapter checks the
instance's per-table quota before acquisition, and request metrics page 4 reports
that instance's three capacities. An isolated draft passed 150 checks, including
a 1048577-byte table, 35-table completion, over-quota/unauthorized rejection
without acquisition, exact read-only acquisition extent, source mutation after
import, failed return/retry without repeated import, and readback beyond 64 KiB.
The integrated standard hosted workflow and native build passed; focused
ACPICA comparisons passed 80 table-bit reads and 36 lookups. All 108 request/endpoint proof checks now pass (86013), with zero unproved
checks or assumptions. See the main AML README for the precise proof and
regression scope. Kernel
acquisition is still mocked in this target; it is not a live grant test. Startup
allocation from discovered firmware lengths remains to be connected.
