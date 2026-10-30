# Retained resource-buffer reader

Instantiate ACPI_Resource_Reader with the service Core. The caller first evaluates
its chosen _CRS/_PRS object through its normal authenticated evaluator and retains
the returned Buffer handle. Copy_Template borrows that handle, checks its kind
and complete length, copies into caller-owned bytes, then validates resource
framing/checksum/quota. Successful bytes can go to ACPI_Resource_Data decoders.
It never invokes AML, resolves names/references, releases a handle, allocates
storage or grants resource authority. The caller owns evaluation policy, result
release and exclusive synchronous access to the service throughout the copy.

Insufficient_Capacity reports Required without truncation or scanning. Allocate
larger caller storage and retry with the SAME retained handle; do not re-evaluate
the AML method. Its preceding effects are not rolled back. A retained Buffer is
a snapshot, not a promise that hardware configuration remains unchanged.

Every failure defaults the entire output and reports Copied=0; unused success
bytes are also zero. Template_Checked distinguishes framing diagnostics from
kind/handle/capacity failures. Framing diagnostics retain their exact cause,
position and counted items. Wrong types, stale handles and handles from another
service fail without consuming the owner's retained root. No automatic type/reference
coercion or predefined-method semantics are added by this reader.

Both hosted profiles passed 480 actual Core integration checks, including all
seventeen retained resource buffers, high output bounds, unchanged retained-root
counts, caller release, capacity retry, a single-effect _CRS method counter,
wrong-service/stale/invalid handles, malformed/checksum/trailing/empty buffers,
nonbuffer String/Package results and item quota. Independent audits verify 90 ALI
files/89 strict commands per profile plus input/tool/runtime/artifact hashes.
Accepted immutable context: resource-buffer-reader-clean-CKA8ugtH/source,
tests/acpi-resource-data/results/release-rs4tbwns and checked-qxkiersu.

Normal ACPICA compiled 1,364-byte AML and observed counter0, one _CRS buffer
4BF803087900, counter1. Its seventeen original buffers remain byte-identical.
The intentionally malformed named buffers are constructed error fixtures, not
valid resource-template conformance examples. Five warnings, one remark and
four outstanding cache allocations at oracle teardown remain retained. The
first candidate's duplicate-use-clause build diagnostics are preserved; no
compiler crash, weakened strict flags or increased limits occurred.

This generic is SPARK-mode code with checked contracts, not a new accepted proof
milestone. All existing parser sources/proof scopes remain unchanged. No native
execution, CCL resource delivery, routing/admission or complete ACPI/AML claim.
