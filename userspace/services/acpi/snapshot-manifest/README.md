# Public snapshot manifest preflight

ACPI_Snapshot_Manifest measures a complete address-free snapshot inventory before
allocation. It checks advertised count, ordinal IDs, first/unique DSDT and FACS
exclusion. A valid complete manifest yields exact count, wide aggregate bytes and
largest extent; caller quotas decide acceptance. Invalid/prefix input yields no
partial requirements. The function preserves table order and accepts shifted
array bounds. An ID or successful sizing decision conveys no authority.

ACPI_Provider_Transport.Initialize_Snapshot consumes these descriptors with an
exact packed byte copy and derives upload kinds/offsets. Its single attempt is
shared with Initialize, including rejection. Trusted export provenance, immutable
storage, table payload validation and grant lifetime remain separate obligations.
The transport's fixed quotas and the service's own capacities still apply;
discovery-sized native construction and kernel export are not implemented here.

The unit is SPARK source with hosted regression tests; it has no fresh proof.
See tests/acpi-provider-native manifest_policy, manifest and manifest_composition.
