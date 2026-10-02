# Named hardware permission rules

Hardware_Authority is a pure SPARK unit shared by hardware resource classes.
A Permission describes one already admitted resource ID, read/write rights,
allowed write bits, and whether further delegation is permitted. It contains no
address, access width, process ID, or capability slot.

ACPI_Region_IO.Dispatch now calls Permits using the trusted epoch-bound record.
The surrounding region policy still checks authenticated endpoint identity,
epoch, resource lifetime and complete transaction bounds. Default permissions
deny access. The wire caller cannot install or modify permission records.

Is_Subset checks an installed permission against its resource ceiling without
requiring delegation rights merely to use it. Can_Derive checks an exact proposed child: same resource, no added read/write
rights or write bits, and parent delegation authority. Derivation_Preserves_Access
is proved for arbitrary 64-bit values: any access allowed by a derivable child
is also allowed by its parent. This is a permission relation, not capability
minting, resource discovery, group membership or revocation enforcement.

The kernel's existing ordinary capability derivation preserves object identity.
Selecting a group member will therefore need a trusted kernel operation that
validates membership and installs a child authority with lifetime/revocation
linkage. Changing an endpoint authority tag is not a substitute. Kernel catalog
admission must also establish safe register semantics; a write mask alone does
not establish that writing zeros outside the mask is safe.

Validation is integrated in tests/aml-core/run.sh --prove. The hosted region
suite checks permission combinations and mask restrictions; the native-envelope
suite checks the production adapter with mock hardware. No live hardware or
kernel group-delegation implementation is claimed.
