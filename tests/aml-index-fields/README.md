# Hosted IndexField declaration and named-leaf access regression

In the pinned repository Nix shell, with TMPDIR=/home/doc/cubit-build-tmp:

    python3 tests/aml-index-fields/run.py --validate-only
    bash tests/aml-index-fields/check_reference.sh
    python3 tests/aml-index-fields/run.py --mode release
    python3 tests/aml-index-fields/run.py --mode checked

The real ACPI_Service_Core imports the exact normal iASL DSDT, initializes its
namespace, and invokes methods through Field_IO. Two independently named,
64-bit register IDs provide a simulated index/data pair. No physical memory or
ports are accessed. An indexed read requires permission to write the index
leaf and read the data leaf. Describing a field never grants either permission.

The normal ACPICA fixture checks ByteAcc/Preserve reads, full and partial writes,
AccessAs ByteAcc, and a serialized method-local IndexField that is created and
removed twice. Nine successful observations include the final selected index.
One compiler remark about runtime named objects, the first dynamic method's
allocation diagnostic and four final cache-allocation diagnostics are retained.
Process exit zero alone is not accepted; the cached checker verifies successful
observations, exact embedded bytes and checksum. An earlier alias experiment
produced incomplete observations and is preserved privately, excluded from the
positive fixture. No crash investigation or permissive oracle settings were used.

The focused regression adds policy tests for independent index/data permissions,
Preserve read permission, full writes without data-read permission, revocation
after a known index write, unknown index/data reads and writes, reconciliation
without replay, known failed completions, overflow, unsupported Global Lock and
exhausted AML fuel. A separately constructed reserved declaration access code
followed by valid AccessAs must fail before any provider request. That malformed
variant is not an ACPICA oracle. The full focused selection has 168 checks.

## Implementation and ownership

IndexField headers use the authenticated declaration lease and reserve the full
encoding charge before mutation. Complete FieldList framing, name conflicts,
capacity and offset arithmetic are checked before committing descriptors. The
index/data links retain earlier node incarnations and compatible method/module
lifetimes. A shared private descriptor predicate is used by both namespace
validity and the type invariant; direct-region constraints are preserved.

The direct-leaf transport expands each bounded datum into an index write then a
data read/write. Preserve writes select the index for both the old-data read and
the resulting write. Complete permission/geometry/index-range preflight precedes
the first effect, and permission is rechecked between effects. Known completed
effects survive later failure. Unknown completion retains the actual leaf
request and blocks further operations until authoritative reconciliation; no
automatic replay occurs. Direct and indexed procedures have no recursive call
cycle. Logical datum offsets never become physical addresses or register IDs.

## Scope and remaining work

This is hosted functionality in SPARK-mode source, not a fresh proof or native
CuBit result. The initial checked run caught an invariant mismatch that release
could not catch; both validity users now share the descriptor predicate. Original
failed sources/results remain private. The existing Store error protocol is
preserved: denied writes report Unsupported, denied reads Unsupported_Value.

Transport currently accepts the existing bounded NoLock scalar/short-buffer
range with ordinary attributes and direct region-field leaves. Nested indexed
sources, table/buffer-field leaves, Global Lock, protocol connections and wider
than 64-bit fields remain explicit unsupported cases. The new normal oracle is
ByteAcc/Preserve with a 64-bit DSDT; remaining access widths, update-rule and AML
32-bit partitions need dedicated indexed tests. Merely accepting a descriptor
is not complete support. Native providers still need platform admission and
serialization for the index/data pair; exclusive hosted context ownership is an
assumption, not evidence of native hardware atomicity. BankField, dynamic table
loading and complete AML semantics/proofs remain unfinished.

Build/runtime limits stay 180/30 seconds, nice 19, one compiler job, 64 MiB stack
and 1 MiB logs. Normal runs use cached pinned reference evidence and disjoint
outputs. No kernel/image/headless build or Git mutation is involved.

## Accepted hosted runs

The exact OzYYTLwz source passed 15,332 checks per profile: 168 new indexed,
14,600 across all six existing Field I/O mains, 522 replay and 42 reset. This is
nine programs/18 build/runtime stages per profile, not the complete AML corpus.
Independent audits checked source/reference/tool/runtime/ALI/binary/artifact
hashes and strict compiler switches: 716 ALIs and 707 compile commands/profile,
9,789 release and 12,705 checked dependency entries. Review receipts with exact
paths are in reference/hosted. The checked run exposed and helped resolve the
initial type-invariant mismatch; an earlier test expectation also differed from
the established Store error protocol. Earlier failures remain preserved.
No production or Ada/GPR changes were made when adding this documentation,
reference checker, mirrors metadata or audit receipts for publication.

The later aml-index-widths registration adds 342 hosted checks across both AML
integer widths: AnyAcc, unaligned Word/DWord/QWord reads and Preserve writes,
WriteAsOnes/WriteAsZeros without data-read permission, selector traces and wide
Buffer ownership. It closes those selected coverage gaps without changing this
milestone's production code. It does not establish exhaustive indexed geometry,
all protocol attributes, nested routing, Global Lock or fresh proof.
