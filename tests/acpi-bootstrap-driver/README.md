# Controller-backed bootstrap completion

`ACPI_Bootstrap.With_Driver` accepts an actual `ACPI_Device_Initialization`
instance, including its region barrier and provisioned evaluator. Its `Finish`
uses the same table freeze, module execution, member initialization and
notification setup as default `ACPI_Bootstrap.Finish`. Immutable snapshot
completion and device readiness are separate outcomes. The caller owns a
limited, one-shot driver session; reusing a consumed session on another snapshot
fails device readiness without repeating device effects.

The hosted test composes the real bootstrap, device walk, region coordinator and
Field interpreter with a modeled named-register provider. A normal 148-byte
ACPICA table supplies `_STA`, `_INI` and a byte Field. Four ordered typed ACPICA
observations establish the expected register value `0x5A`; `check_reference.sh`
checks retained tool results, table extent/checksum and exact Ada embedding.

Six process-isolated cases cover a permitted initializer, unavailable handler,
consumed driver reuse, unchanged default policy, zero startup budget and an
indeterminate register completion after its modeled effect. Repeated Finish
never repeats an operation or charges fuel again. Connection-attempt admission
is distinct from readiness: a missing handler produces a failed device phase
and zero register operations. The current selected ten-case integration matrix passes 228 checks per profile;
strict independent audits are accepted. The separate 49-check default regression
has earlier evidence and was not rerun for this lid integration step.

Use the existing hosted regression runner inside the pinned Nix environment:

```
python3 tests/acpi-bootstrap-driver/run.py --mode release
python3 tests/acpi-bootstrap-driver/run.py --mode checked
```

Each compilation is limited to 180 seconds, each test case to 30 seconds, with
one compiler job, a 64 MiB stack and bounded logs. Exact source inventories,
reference hashes, compiler profiles and ALI dependencies are independently
audited. This legacy hosted harness is separate from the successor native
`component.ccl` build integration.

This is tested behavior, not a new SPARK proof or a native CuBit/QEMU result.
The trusted barrier must preserve service identity and charge its work honestly;
actual register transport, launch authorization, clock, grant lifetime and
hardware delivery require native integration. Default native startup still has
no installed hardware provider. The full interpreter proof remains incomplete.

`ACPI_Bootstrap.Lids` now integrates the internal lid adapter with bootstrap-owned
Core state. Its trusted controller `Find` searches for the first PNP0C0D match,
requires successful device initialization, preserves a namespace descriptor
snapshot across identity evaluations, and bounds charged AML fuel. A private
Binding carries descriptive identity; it grants no endpoint/hardware authority.
Query rejects missing bindings or unready sessions and invokes only the fixed
lid operation through the actual provisioned evaluator.

Thirty new integration checks exercise discovery, zero-budget behavior, binding
admission, actual Field reads after initializer completion, other-session
readiness, denied/default/uncertain startup, and unchanged startup accounting.
The normal ACPICA startup fixture includes the lid over the same VAL0 register;
its four retained startup observations remain the oracle for initializer effects.
Actual lid queries here are hosted integration observations, not native hardware.
Predefined namespace devices make the candidate count larger than the two
fixture devices; identity, selected query result and effects are checked directly.

Immutable runtime origin: request-driver-spPffAMM/source, release-hsk6ezja and
checked result recorded in the publication receipt. This final checkpoint changes documentation/hash metadata only;
consumed source closure and GPR files must match that origin. These two new
SPARK-mode child units are not freshly proved. Multiple-lid enumeration,
per-device readiness with initialization errors, controller CCL routing and
native SCI/GPE/EC provisioning remain unfinished. No native build rules added.

`ACPI_Requests.With_Driver` makes provider-selected-by-the-service completion
reachable through the existing request protocol. The owner supplies the actual
Device_Initialization instance and persistent Run; only a valid Snapshot_Provider
Finish_Snapshot uses it. All other requests delegate to the original handler.
The default handler, authority enum, wire labels and startup budget stay intact.
No incoming word can select an evaluator, register address or driver instance.
The actual backend must not synchronously call the peer blocked in Finish.

Four added process cases contribute112checks (228current matrix total): allowed
wire completion, denied region admission, indeterminate completed write and
unchanged default completion. They cover denied/zeroed unauthenticated replies,
observer denial, malformed/stale/out-of-order input, revision exhaustion, delegated
snapshot import, public readiness metrics, repeated Finish and consumed Run reuse.
Snapshot completion can be OK with failed/incomplete device readiness; observers
must inspect the separate readiness metrics. Original six lid/bootstrap cases
still run. Separate49-check legacy regression is not rerun for this step.

This is hosted request-core integration. Native endpoint/server-loop selection,
controller lid-query routing, real backend provisioning and native boot tests
remain unfinished. The new request child is SPARK-mode source with the same
public server/revision/frame contracts as Handle, not a fresh proof result.

The configured endpoint regression now routes all48 ordinary Send calls through
ACPI_Endpoint.With_Handler.Route and checks exact wire reply encoding. Its10-case
matrix passes276checks/profile (the earlier228 plus48encoding checks). This
covers real request/driver completion under classified provider/observer/unknown
stamps; native delivery remains covered only by separate modeled-I/O fixtures.

Region-attempt diagnostics extend the matrix to 318 checks per profile. The
read-only page 17 distinguishes a region callback attempt from an exit before
that phase; its outcome is separate from device readiness. Tests cover pre-start,
zero budget, callback failure/unavailability/indeterminate effects, observer reads,
unknown-caller zeroing and no replay. No hardware authority is conveyed.
