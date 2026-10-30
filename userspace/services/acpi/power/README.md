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
