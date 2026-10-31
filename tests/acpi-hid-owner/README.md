# Service-owned HID-I2C discovery and controller association

Run `bash tests/acpi-hid-owner/run.sh` in the pinned Nix development environment,
setting TMPDIR=/home/doc/cubit-build-tmp before and inside Nix. `--validate-only`
checks the exact catalog. The runner snapshots sources, pins tools/fixtures,
checks actual ALI profiles and requires checked/release result agreement.
Limits: nice19/j1, build180s, runtime30s, iASL5s, stack64MiB, core0, log1MiB.

ACPI_Bootstrap.HID_Devices and ACPI_Requests.HID_Devices are trusted generic
owner bridges to the SAME private Core used by service requests. Discovery and
Resolver both match that Core. Begin_Probe accepts a snapshot-local device node,
requires successful device initialization, and retains the existing discovery
session's _CRS. Copy_And_Resolve retries only the retained copy for capacity,
then completes fixed _DSM discovery and resolves both controller paths using
the original device, exact copied prefix and observed descriptor register.
Resolver is not attempted on partial/failed discovery. Its report never exposes
a partially resolved value. No arbitrary wire AML execution, register address,
authority, provider override, endpoint or receive loop is introduced.

Keep Probe with its Server and serialize all owner access. While Pending,
exclude other AML, catalog mutation and reset, as required by the underlying
Discovery API; retention alone does not freeze mutable AML data. Busy calls
cannot replace the saved scope. Cancel is available without readiness and
delegates foreign-context checks. Foreign copies zero output and retain the
original owner's _CRS. These owner calls preserve Requests protocol revisions.
Current checks descriptive device/controller identities in the owning Core;
it does NOT establish _STA/dependency completion, provider readiness, controller
binding, source provenance, resource ownership or permission to issue hardware IO.
Recheck it before consuming descriptions after other AML execution.

Current hosted matrix: checked/release each222checks, across32/64bit AML and
fresh normal46, unconfigured-region21, missing-GPIO44 lifecycles per width.
404source/test pins and20commands. Actual CID/STA/CRS discovery runs once before
capacity retry; counted fixed evaluators/invokers show three named evaluations,
zero descriptor invocations until full copy, then exactly two _DSM invocations.
No replay on busy or repeated copy. Rooted I2C/GPIO controllers are selected in
the actual Requests-owned Core, with the observed descriptor register0x20.
Missing GPIO produces GPIO_Source_Failed, empty resolved value and Current=False.
Foreign service copy/cancel, explicit cancellation and zero fuel are exercised.
The hardware fixture has no admitted provider and remains barred after catalog
completion. Enumeration now selects the touchpad used by the probe. Expected fixture node
positions only check that the correct device/controllers were selected.

`oracle.mjs COMPILED_AML` independently observes CID=PNP0C50, STA=0x0F and the
57-byte CRS buffer with both rooted source names. It records exact bytes. This
does not compare our resolution/bridge or exercise _DSM. The pinned ACPICA
build's exact fourteen-allocation exit diagnostic remains explicit, clean_tool_log
is false, and other ACPI errors are rejected. No clean teardown claim.

These are hosted functional checks, not new SPARK proof or native/hardware
acceptance. Trusted bounded, non-raising, non-reentrant evaluator/provider
contracts and pending-session serialization remain obligations. Native enumeration wiring, launch/provider/source/endpoint contracts, restricted
controller channels, typed observer transport and full AML/service verification
remain incomplete. See docs/acpi-native-bringup.md for coordinator prerequisites.


## Bounded PNP0C50 enumeration

Find searches strictly after a snapshot-local node cursor; Root starts. It uses
the SAME fixed Discovery.Queries identity evaluator as the probe, with one shared
AML budget and no automatic retries. The finite namespace bounds metadata
scanning separately from Charged; Charged is not total CPU/wall time. The full
captured topology is checked after each identity evaluation. Root is returned
unless Found. Missing/incompatible identities are skipped, exhausted fuel yields
Incomplete, and other identification/topology failures yield Failed. Incomplete
is not a resumable invocation token. Do not call Find while ANY discovery is
Pending: this is AML owner work and must obey the pending-session exclusion.

The hosted matrix finds string and package CID candidates past unrelated I2C/
GPIO devices, reaches No_Match and passes the discovered node to Begin_Probe.
Zero budget, invalid cursor and unready state perform no identification. A CID
package with a match followed by a Buffer is constructed at runtime in the
negative fixture: the query must validate the whole package, and enumeration
fails without a node rather than accepting its initial match. Normal iASL
compilation is used; its initial rejection of the static malformed form was
kept in private failed-run evidence, not bypassed with forced compilation.

Prior checkpoint checked/release each222checks across both AML widths;404pins,20commands.
New exact-fixture ACPICA CID/STA/CRS reference retains its narrower scope and
qualified exit diagnostic; it does not validate our enumeration implementation.
No new SPARK proof, native/hardware execution, hardware admission or launch API.


Begin_Next_Probe composes enumeration and probe start on the same private Core.
Search receives the caller's Budget; the probe receives only Budget minus the
search charge. That remaining budget covers CID/STA/CRS and later DSM work.
A pending Run returns Busy before any search or report replacement. No match,
invalid cursor, zero fuel or unready owner starts no new probe; inspect Search
and Action, not a previous completed Run report. Other pending probes remain
subject to the existing owner-wide exclusion rule. Capacity retries use only
Copy_And_Resolve on the same Run; never repeat search for buffer sizing.

The new budget fixture uses an actual AML CID method so enumeration consumes
fuel. One operation below measured complete discovery must report exhaustion,
produce no source resolution and release its retained CRS. Actual callback
charges plus reported search/probe charges stay within the original budget.
Checked/release each382checks across both widths/four fixtures,26commands.
An independent exact budget-fixture ACPICA CID/STA/CRS reference is recorded,
with its teardown diagnostic qualified; it does not validate our orchestration
or fuel policy. No new proof, native wiring/launch or hardware acceptance.


Shared STA regression:current HID public status report delegates to common
ACPI_Device_Status. This suite was rerun in checked/release against that body;
382checks/profile. New checked lid/AC APIs are tested separately by
tests/acpi-checked-power; no native power wiring or new proof claim.


## Typed channel preparation

Actual Core discovery/source resolution now feeds ACPI_HID_I2C_Channels and
checks the resulting address, speed, GPIO signaling and descriptor-register
facts. The policy matrix checks codec boundaries, all trigger/polarity pairs,
unsupported roles, exact encoding/decoding and full copied-fact preservation.
Checked/release each pass2998checks in8fresh runtime processes across both AML
widths and four fixture modes. A fresh ACPICA budget-fixture observation confirms
CID, STA and57resource bytes; its known14cache-allocation teardown diagnostic is
retained and qualified. No DSM, typed-adapter or authority equivalence is inferred
from those reference values. No new proof, launcher/native integration or
hardware acceptance is claimed. See userspace/services/acpi/hid-i2c/CHANNELS.md.
