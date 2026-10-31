# Owned ACPI passive-query bridge

Run `bash tests/acpi-query-owner/run.sh` in the pinned Nix environment, with
TMPDIR=/home/doc/cubit-build-tmp set before and inside Nix. `--validate-only`
checks pinned source inputs without compiling or executing tests. The runner
works from another working directory and uses disjoint TMPDIR outputs.
ACPICA_BIN may select the pinned ACPICA20260408 compiler; the recorded Nix tool
path is a fallback, and its exact SHA is verified.

Actual Requests/Bootstrap/Core/AML state is used. Checked and release builds
each run AML revisions1/2 in separate normal, revision-ceiling, malformed-module
and unconfigured-hardware-region lifecycle processes. Tests cover catalog/member
readiness, denial, internally derived generations, full64-bit instance identities,
stale references, live-parent ADR, complete Describe and Find paging, resources
and revision preservation. A method-valued HID increments a counter when invoked;
its aliased UID stays zero through passive queries. Failed module startup never
exposes the partial namespace. Static queries remain available while hardware
initialization is stopped at Regions_Unavailable; no hardware rights follow.

The actual owner dispatcher also composes with ACPI_Observer_Loans and
ACPI_Query_Sessions, using modeled backend grant/copy/reply callbacks. Each
completed-startup lifecycle adds155 explicit checks: all four query operations,
client mutation after private copy, full64-bit expected-owner/opaque-reference
checks, role/extent/save/read/query/write failures, lost delivery, deferred
return, busy admission and bounded cleanup without evaluation/reply replay.
Every answer checks original acquisition return and exact saved authority.
These are hosted callback models, not kernel grant/IPC validation.

Normal/ceiling cases each check411 conditions, failed9 and hardware419:
2500checks/profile across both AML widths. Compiler/ALI switches, source/tool and
fixture hashes are checked. Build180s, each runtime30s, iASL5s,64MiBstack,core0,
log1MiB,nice19/j1. The runtime readiness predicate is separate from the Ghost
phase accessor; checked contracts relate the two. No new SPARK proof, native
execution, grant/IPC/kernel/hardware behavior, driver launch or whole-service
correctness is established. Native integration still needs the generated query
request/reply binding, existing expected-owner loan lifecycle and owner pump.
