# Serial, pin and clock proof milestone

The three actual decoder bodies discharge311 proof checks/526 goals plus48 flow
checks, all eighteen entities in SPARK with no assumptions/skips/unresolved
checks. Serial has139/234/15 (five entities), pin114/197/21 (seven), clock58/95/12
(six). This proves runtime safety, declared extent/span contracts, initialization,
dependencies and termination for the selected bodies.

The functional additions have specific scope:
- In_Hertz proves exact numerator scaling by1/1000/1000000 according to the typed
  scale and preserves the nonzero divisor for every Clock_Description.
- Read_Pin proves availability iff Decode succeeds and the index is within its
  REPORTED count, and the exact little-endian value at its REPORTED pin span.
  Decode's span/count contract bounds those accesses. This does not establish
  that every decoded pin offset/count matches the corresponding wire fields.

Full successful field-to-wire correspondence, acceptance/error-classification
completeness, configuration-value constraints, provider/label/source resolution,
controller admission, service/namespace and whole AML/native correctness are
outside this milestone. Source opacity and raw configuration values remain as
specified by the decoder interfaces. Earlier six proof suites retain their
independent exact inputs/scopes; this suite is the seventh registration.

The operational refinement removes only five ineffective local initializations
in the pin decoder. Remaining source/label defaults remain intentional for flow.
Pin Decode gains bounded pin-span postconditions and a ghost inline observation
model; clock conversion gains its exact postcondition. Parser decisions and
wire encodings are unchanged. Initial flow diagnostics were inspected/preserved;
there was no compiler crash, assumption, weakened contract or larger limit.

Run separate phases in pinned Nix with TMPDIR=/home/doc/cubit-build-tmp before
and inside Nix:

    nice -n19 bash tests/acpi-connections-proof/run.sh check
    nice -n19 bash tests/acpi-connections-proof/run.sh flow
    nice -n19 bash tests/acpi-connections-proof/run.sh prove
    bash tests/acpi-connections-proof/check-result.sh "$SOURCE" "$RESULT" "$RECEIPT"

Level1/VC5s/j1/CVC5+Z3/phase180s/stack64MiB/log1MiB. The closed ten-Ada-file
inventory selects only the three actual bodies. The checker verifies source/
prover hashes, all-SPARK/noAssume/skips, clean flow and a valid attempt for every
proof-check goal tree; process exit zero alone is insufficient.

Actual accepted source connections-contracts-A3wVB00I/source, output
 tests/acpi-connections-proof/results/prove-sBYjnlGN. The imported hosted log
collector matches its immutable4497-file snapshot pins; that full snapshot was
independently rechecked after proving. Publication also pins the imported
collector and checker explicitly in the phase manifest for future runs. These
metadata additions do not alter the ten Ada proof inputs or project. Keep the
accepted output bound to its original source, not a later README/recipe hash.

The SAME accepted A3wVB00I source passed3,227,583 focused runtime checks/profile:
clock197879+pin1969581+serial795976+PCI263543+actualCore604, five mains/ten stages
in release-cwuqcsj7 and checked-ug4xc2lj under tests/acpi-resource-data/results.
Independent audits verify188ALI/183strict compiler commands per profile plus
source/reference/tool/runtime/compiled/binary/artifact hashes. All223production
and1062Ada/GPR inputs carry unchanged into publication, including the closed
proof project. README/checker/evidence and recipe metadata are added afterward.
This is not an all-fifteen rerun, full interpreter proof or native validation.
