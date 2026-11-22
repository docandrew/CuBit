# Independent ACPI register backend

The native process is declared in native/component.ccl and native/manifest.ccl.
It owns the launcher's ACPI hardware-window grant and an independent sealed-table
snapshot. The AML frontend receives a scoped register endpoint, not raw windows.
The first slice admits I/O windows only and publishes one read-only PM-timer ID.
Zero admissible windows cause the launcher to refuse startup.

Startup validates the actual handles and exact ranges, copies/validates the FADT,
installs the timer descriptor and seals the catalog. Scalar requests contain
[epoch, register ID, value, zero]; payloads cannot choose addresses or widths.
Kernel-stamped full-width scope tags control visibility. Unknown completion
quarantines aliases and retains backing without replay. The serialized server
handles requests/events and waits when idle; it never calls the AML frontend.

The application dependency gate rejects AML/interpreter imports from the native
entry. The generic hosted provider and envelope adapter remain available for
existing model compositions, outside that executable's used dependency closure.
The former sibling acpi-backend directory is consolidated here; no native .gpr
program build or compatibility source copies remain.

Hosted/model tests and native semantic checks do not establish actual hardware
behavior. Launcher grant testing is separately coordinator-reported. MMIO,
additional named writes, EC/controller provisioning, live event delivery and
full SPARK verification remain unfinished. Wire contracts live in shared/acpi;
see docs/acpi-service-contract.md and coordination/acpi-region-boundary.md.
