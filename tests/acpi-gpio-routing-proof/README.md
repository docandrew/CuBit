# GPIO wire correspondence and PCI numeric-policy proof milestone

Actual GPIO_Connections and ACPI_PCI_Routes discharge 247 proof checks/419 goals
and 32 flow checks. All fourteen entities are in SPARK with no assumptions,
skipped or unresolved checks. GPIO has 238 checks/405 goals/30 flow; PCI numeric
policy has 9/14/2. This supersedes the earlier 108/190/19 milestone.

For every successful GPIO Decode, contracts establish framing identity relative
to Read_Frame (not an independent proof of the raw framing-header decoder),
absolute offsets corresponding to raw descriptor-relative offsets, span lengths
and pin count, and common and connection-specific field values. The ghost
models read little-endian wire words and express bit fields arithmetically.
Read_Pin proves availability and the corresponding little-endian pin value;
Decode's stronger contracts now link its reported pin span/count to wire fields.
Initialization, dependencies, termination and runtime safety are included.
PCI Decode_Address proves validity iff functionFFFF and device0..31, and the
exact device value on success.

This is successful-result correspondence, not a proof that all valid encodings
are accepted or that each failure has the exact required classification. Source
names remain opaque spans; name/controller validity and hardware authority are
outside this proof. Service readers, metadata getter, namespace/package binding
and whole AML/native correctness also remain outside it. Earlier parser proofs
retain their independent scopes and exact source sets.

The only operational refinement is To_Absolute, a bounded addition helper with
an inverse-offset postcondition, replacing three equivalent Offset + Relative
expressions. No parser decisions, accepted encodings or limits changed. New
contracts and ghost expression functions expose wire correspondence to proof.
The combined and split predecessor proofs retained an unresolved offset check;
all were inspected and preserved before this candidate. No assumptions, weakened
contracts, extra proof limits or compiler-crash investigation were introduced.

Run separate phases inside pinned Nix with TMPDIR=/home/doc/cubit-build-tmp
before and inside Nix:

    nice -n19 bash tests/acpi-gpio-routing-proof/run.sh check
    nice -n19 bash tests/acpi-gpio-routing-proof/run.sh flow
    nice -n19 bash tests/acpi-gpio-routing-proof/run.sh prove
    bash tests/acpi-gpio-routing-proof/check-result.sh "$PWD" "$RESULT_DIRECTORY" "$RECEIPT_JSON"

Level1/VC5s/j1/phase180s/stack64MiB/log1MiB. Closed local source list; only the
two actual bodies are selected. The checker verifies input/prover hashes,
all-SPARK/no-skips, resolved flow and a valid attempt for every proof check tree.
Process exit zero alone is insufficient and was rejected for prior unresolved
candidates.

Accepted immutable source gpio-wire-absolute-4c5ij0wW/source, proof output
 tests/acpi-gpio-routing-proof/results/prove-oEpJFUT2. The original GPIO-wire
publication carried identical production and Ada/GPR inputs; README/checker/
receipt and updated docs were metadata added after that proof. The later serial
extension leaves these two units and their closed proof inputs unchanged. Its
new serial decoder and expanded service fixture are outside this proof.

The same immutable source passed 854,979 focused checks/profile
(590956 GPIO +263543 PCI +480 actual Core), three mains/six stages in both
release and assertion-enabled profiles: tests/acpi-resource-data/results/
release-08znltkb and checked-yhsz59om. Independent audits verify175 ALI and172
strict compiler commands/profile plus input, reference, tool/runtime, compiled
source, binary and artifact hashes. The original GPIO-wire publication carried
all217 production identities and1046 Ada/GPR files unchanged from that source.
The serial extension preserves those217 production identities and adds two new
production files; it changes test/fixture inputs, so does not claim that earlier
whole1046-file carry for the expanded suite. This is not an
all-twelve rerun or a whole-interpreter proof.
