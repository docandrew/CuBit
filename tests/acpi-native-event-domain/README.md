# Native ACPI event-domain hosted regression

Run in the Nix environment with TMPDIR exported before Nix:

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop -c bash tests/acpi-native-event-domain/run.sh

Run --validate-only for inventory/hash checks without compilation. --source-root selects a guarded candidate tree. The final JSON prints the unique output directory; run the companion ACPICA check in the same environment:

    node tests/acpi-native-event-domain/oracle.mjs /absolute/output/RESULT.json

Actual code: concrete native event domain, EC source/selector/shared command engine, receiver/methods, four-family consumers, AML/Core/runtime and guarded receipt pump. Models: native Fields/ports/clock/SCI/logger boundaries. All six scenarios run at AML32 and AML64: normal, startup _INI Notify, stop with Notify, no EC backing, no SCI and retained uncertain hardware. The actual _REG(3,0) method is invoked on the same Core during shutdown to check route lifetime; this harness does not replace separate region-registry tests. Fixed callbacks copy only; logger model rejects active/retained Field ownership. Observer receipt mutations are separately covered by the predecessor four-family suite.

Whole Core uses -O0 -gnatp, with explicit Check failures. Compile limit180s, test30s, address space24GiB, stack64MiB, core dumps0, one compiler job, nice19. ACPICA5s/128MiB/2MiB stack. No native/image/kernel builds or hardware execution. Semantic SDK checks and SPARK proof are separate; these tests prove neither full interpreter correctness nor physical interrupt/port behavior.

Notification-interest follow-up: this suite now drives the ACTUAL ACPI_Snapshot_Startup.With_Driver allocation/copy/completion path. It performs no manual Configure_Notifications(True). The native adapter must request capture before Finish_Snapshot and the event domain rejects a deliberately disabled policy before source enable. Hosted aligned allocation supplies the CuAlloc ABI; it is not native allocator validation. Seven modes run at each AML width.

Battery recovery follow-up: the battery-recovery mode uses actual AML methods and
captured Notify(0x80/0x81) on the same Core. Malformed _BST and _BIX data must
publish explicit failed acquisitions without the previous capacity; subsequent
valid notifications must publish recovered capacity. No hardware is exercised.

EC absence follow-up: three more modes at each width distinguish a complete namespace scan without any EC (with and without early backing), a described EC without backing, and an invalid identity result. Only the successful no-match scan certifies absence. All degraded cases retain fixed-button handling and refuse to invent live family samples.

The image19 port adds explicit absent-domain test boundaries for the power-service entry owner, saved-reply button watches and PNP0C0C discovery. This fixture sends no power IPC, has no FADT fixed-power source and no PNP0C0C device. Its real event-domain fixed callback still copies/logs the original press counts. These cases validate EC/lid/battery dispatch, not power-watch delivery, shutdown or button discovery; those owners have separate suites. Original battery assertions are retained.

Recovered cached-read and general-GPE coverage: the current fixture runs 18 modes
at each AML width. Seven additional modes exercise a general AML handler with
and without an EC, overlapping EC/general notifications, and mask, admission,
clear and rearm failures. Same-Core scalar witnesses check handler counts and
HID descriptor changes. Source callbacks perform no AML, register commands or
logging; clear/rearm requires completed handler and receipt retirement.

The no-EC cases now exercise independent lid/AC/battery/HID consumers. A failed
EC identity remains an explicit discovery failure, grants no EC access, and the
consumer domain subsequently refuses the invalid family observation. This
updates earlier expected behavior; it does not certify EC absence on failure.

Cached lid/AC reads exercise the actual native endpoint, dispatch and consumer
cache. Tests check canonical replies, received observer classification, no
refresh/AML/register commands during reads, freshness and unknown clocks,
generation-coherent subpages, and empty payloads before admission, during held
events, on failure, and after shutdown. Both generic test pages and actual
native pages are exercised. The separate acpi-cached-power-codec suite runs
the pure framing/encoding cases with assertions enabled and suppressed.
These are hosted models of hardware, SCI, clock and native logging boundaries;
no native launch, physical interrupt or laptop result is claimed here.

Battery observer follow-up:25 modes run at each AML width (50 processes).
The fixture namespace is512 nodes to separate valid17-battery table startup
from the production16-entry inventory budget; the real native profile remains
16K and is checked separately with -gnatc. A named16384-byte fixture input
buffer supports the expanded table; the original4096-byte snapshot chunks and
1000 absolute deadline are unchanged. Cases cover no batteries, known absence,
unknown quantities, real missing-BIX BIF fallback, multiple batteries, complete
16-entry inventory and attributed17th-entry refusal without partial data.

The actual native battery endpoint uses read-only page22 with generation-bound
slots/subpages. Queries preserve unit/source metadata and unknown values,
require received observer classification and strict framing, and read the
same cache without AML, IO, logging or power/passive dispatch. All prior lid/AC
and core metric routing remains covered. Pure checked/release codec/dispatcher
tests and concrete L1/L2 functional proof are reproducible through
tests/acpi-cached-battery-codec/run.sh and proof.sh. Codec and pure dispatcher reply/generation policy are proved; actual callback/cache
projection/owner lifetime and native/hardware execution remain separate. Existing synthetic ACPICA fixtures are unchanged.

Dispatcher proof registration: the same suite also instantiates the actual
dispatch generic with an explicitly SPARK-analyzed passive default-copy model,
checks all six codec/dispatcher posts at levels1/2, and rejects incomplete
proof artifacts. It does not establish native callback/cache ownership.

The actual native power-button discovery/retained-receipt path now has its own
hosted regression: tests/acpi-native-button-discovery. It covers discovery
and uncertain-owner cleanup rather than the unavailable stub used here.
Physical power-watch transport and hardware remain separate validation.

That standalone button suite also joins actual Native_Power_Buttons and
Native_Power_Watches: saved press/Stopping replies, quiet-boundary deferral,
lost-caller authority consumption and refusal/supersession. Kernel message
and saved-reply operations remain modeled; no native delivery is claimed.

EC failure controls preserve the first query cause when closing the runtime
window also fails. An indeterminate command remains retained after SCI disarm;
an ordinary close failure stays Window_Refused and can drain once command
ownership is quiet. Both controls forbid command replay; stopped consumers
do not publish fresh family observations from queued notifications.

Pre-EC startup preparation is covered before SCI/EC enrollment: metadata-only consumer routes, captured Notify routing, deferred routing while another runtime window is active, subsequent routing after quiet close, stop/close before event enrollment, and a five-Notify burst spanning bounded batches. The combined event-domain suite recorded 9,762 checks across 68 AML32/AML64 processes, including expired quiet runtime cleanup and rejection of incomplete/held-I/O cleanup. Low-level hardware/kernel/clock boundaries remain modeled; Native_Instance startup ordering and preservation require coordinator native validation.

EC candidate diagnostics separately preserve the first successfully described controller node/path and the last namespace scan checkpoint. A described candidate is not enrolled authority: readiness and arming remain separate. The candidate-expiry case advances only the modeled clock beyond the original event deadline after candidate capture, then checks distinct scan/candidate identities, live independent consumers, no EC arming or register reads/writes, and orderly stop. The expanded suite records 10,028 checks across 70 AML32/AML64 hosted processes. The 500 ms selection allowance is unchanged; no native timing or hardware correctness is established.

The metadata-only relevance pass uses descending propagation with validated parent IDs and the existing two Boolean arrays. Active status/identity scanning retains its original live ancestor checks because AML can change the namespace. Shared-ancestry fixtures have 32 method-identified leaves under eight Devices and one ThermalZone, plus the real fixture EC: the relevance pass visits 42 relevant parent links. Absent parents prune descendants through the non-device node; functioning-only parents preserve all 32 leaf identity evaluations. The full suite records 10,364 checks across 76 AML32/AML64 processes, retaining every previous case. No timing allowance was extended and no native timing/hardware result is claimed.

Final optimization scope: only the metadata-only relevance closure is linearized. Active ancestor checks are retained verbatim from the earlier native candidate because AML can mutate namespace state. The three shared-tree cases continue to pass with this scope: 42 relevance parent links, pruned identities under an absent ancestor, and 32 identities under present/functioning-only ancestors. All 10,364 checks across 76 processes pass; native laptop timing remains unvalidated.

## Discovery and query deadline controls

`slow-discovery` advances the modeled clock by 700ms after the candidate is
described: the once-only startup window is exactly 5s, while the subsequent
ordinary query window remains 500ms. `selection-no-clock` and
`selection-no-room` refuse before a runtime window or register IO. The
shared/pruned/functioning ancestor cases remain in the runner at both AML
widths. These are hosted observations, not physical timing or new proof.
The runner invokes the absolute, hash-validated ACPICA/GPRbuild tools.
