# ACPI power observations

`ACPI_Power_Source` is a service-owner consumer of Core. `Identify` observes
the exact string hardware identifier `ACPI0003`. It supports named values,
methods and aliases, without parent-scope fallback, prefix matching or coercion.
An integer EISA ID is a nonmatch. Returned objects are released on every path.

`Query` observes only the selected device's canonical direct `_PSR`. It accepts
the defined integer values zero/offline and one/online. Missing, failed or
wrong-type observations remain Unknown. An invalid integer is retained whole
for diagnostics, without narrowing it into a valid value. Accepted work and
an offending provider's last charge remain separate.

Online describes this particular power source being in use. It does not prove
physical cable presence or establish the system's overall battery/AC policy.
The owner separately establishes device identity/status, initialization and
admitted hardware context. AML evaluation can have effects; ordinary observers
receive cached samples rather than invocation authority. This package adds no
CCL endpoint, transition sequencing or direct hardware access.

The [ACPI6.6 power-source definition](https://uefi.org/specs/ACPI/6.6/10_Power_Source_and_Power_Meter_Devices.html#psr-power-source)
defines the identity and status semantics. Hosted tests exercise both valid
values, unknown/error distinctions, aliases, budgets and retained cleanup.
Current evidence is private Linux-hosted testing, not native validation or a
new proof. Sleep/resume belongs to the separate power coordinator and drivers.

The published service-owner path is ACPI_Bootstrap.Power_Sources, its Cached
child and ACPI_Requests.Power_Sources. The trusted owner discovers the first
matching ACPI0003 device after device initialization, prepares one fixed PSR
observation, fans out the same owner's actual notifications and publishes a
copied cached sample. The complete typed query diagnostics retain invalid raw
values; only sampled0/1 observations become offline/online samples. Read never
invokes AML. Multiple power sources and system policy require separate work.

Requests.Power_Sources also accounts for events retained in broker receipts
after the Core FIFO empties. Observe_Claimed handles the actual staged event
once per active receipt without claim/drain/ACK. Queued/unseen events suppress
freshness. Use ACPI_Requests.With_Notification_Routing on all Claim/ACK requests,
with a bounded idempotent callback that includes BOTH lid and power consumers
and every other local cache before ACK. One cache belongs to one serialized
Server for its lifetime; use one clock epoch/unit and admitted evaluator context.

[Hosted owner tests](../../../../tests/acpi-power-owner/README.md) pass330checks
per profile across both AML widths,including two consumers,receipt routing,
invalid2/wrong-type results,budgets,clocks,maxgeneration and unavailable hardware.
Five selected native semantic units/profile instantiate the actual native-clock
API and emit zeroobjects. Fresh normal repair-disabled ACPICA confirms the raw
fixture return sequence;PSR0/1 consumer validation is separate. Four outstanding
cache allocations remain in reference logs. No new SPARK proof,native execution
or hardware validation is claimed. Native providers,SCI/GPE/EC/event-loop hooks,
typed cached-result IPC/subscriptions and power-policy delivery remain unfinished.

The shared observation cache now has a separate scoped proof milestone:
[acpi-observation-cache](../../../../tests/acpi-observation-cache/README.md).
Its actual owned lid/power record types live in ACPI_Observation_Values; driver
state names are aliases/constants and both Bootstrap caches use those exact
record types. Descriptive values carry no interpreter identity or authority.
The executable cache policy is unchanged; Ghost observations and exact contracts
specify initial state,invalidation,stale publication preservation,exhaustion,
timestamp normalization and freshness/no-payload outcomes. Each level1/2 gate
proves14checks plus16flow/termination checks over23SPARKentities,with52ValidCVC5
goals,no explicit Assume/skip and no external claim dependency. This proves
these two cache instances,not the adapters,notification fanout,AML or arbitrary
other Sample_Type instances. Owner serialization,admitted callbacks and one
clock epoch/unit remain integration assumptions. Native/hardware execution
remains unfinished. The small pure-value proof avoids dragging mutable Core
into the payload proof; the prior full-context attempts timed out at180s
during Global-contract generation,not cache verification or a compiler crash.
