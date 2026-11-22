# Local battery inventory events

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop -c bash tests/acpi-local-battery-events/run.sh

Pinned GNAT/GPRbuild, Node and ACPICA must be on PATH. Sources are hash-checked and copied; unlisted Ada/ASL inputs refuse. Builds use -O0 -gnatp with 180-second/24GiB limits, executions 30 seconds. Binary and non-binder ALI profiles/hashes are recorded. No full assertion-enabled Core generation.

390 checks across20 processes: ten cases at each AML width. The concrete ACPI_Requests.Battery_Observations.With_Local_Events child builds a bounded, copied inventory using the existing same-Core collector and local runtime. One operation budget covers all searches and observations. Cases include mixed BIX/BIF provenance, empty inventory, known absence, malformed BIX without BIF fallback, presence changing during acquisition, query-generated Notify, capacity exhaustion, budget exhaustion, stopped runtime and lost source health. Whole-inventory publication is deferred until notification routing and health gates pass; no partial inventory is published on capacity/budget failure.

ACPICA with repairs disabled matches 122 integers and 24 strings across eight executions, including absence, presence change and self-Notify. This checks fixture semantics, not inventory, cleanup or fallback policy. The suite exercises actual request/bootstrap/Core/collector/runtime and guarded routing with modeled clock/provider/health/fact callbacks. Native SDK binding is checked separately and deliberately refuses clock/source health. No new proof or native/hardware execution is claimed.

Callbacks must be serialized, bounded, normally returning, non-reentrant and non-mutating through aliases. No producer may interleave with Collect or claim/dequeue/ACK during it. Every consumer must route before ACK. Report_Publication stages copied facts; Main uses the existing logger after Advance returns and independently checks actual owner quiet. Consumer stop never authorizes backing retirement. Descriptors and copied diagnostics are metadata, not hardware authority or live retained handles.

Ordinary search/fuel/inventory refusals now suspend only this consumer after
same-context/source and clean End_Event barriers. A stopped, uncertain or
contract-violating owner is not isolated. Suspended consumers retain receipt
routing, hide partial data and never retry collection. Native integration emits
copied method/access/execution/collection diagnostics so a laptop refusal can
be attributed without another AML call or register read.
