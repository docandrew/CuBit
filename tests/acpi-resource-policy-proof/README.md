# Additional resource policy/decoder proof milestone

Actual Address_Spaces.Geometry, Memory_Ranges and Generic_Registers discharged
79 checks / 198 goals plus 17 flow checks at level 1. All nine entities are in
SPARK, without assumptions, skips or unresolved messages. Per-unit checks/goals:
geometry 21/33, memory 30/56, registers 28/109. The scope is runtime safety,
decoder extent contracts and the geometry feasible-witness postcondition,
initialization/dependencies/termination. It does not establish feasibility
completeness, full parsing/checksum equivalence, allocation/bridge/translation
policy, resource authority or whole AML correctness.

Use the pinned Nix shell with TMPDIR=/home/doc/cubit-build-tmp before and inside
Nix; run check, flow and prove separately and inspect each phase:

    nice -n19 bash tests/acpi-resource-policy-proof/run.sh check
    nice -n19 bash tests/acpi-resource-policy-proof/run.sh flow
    nice -n19 bash tests/acpi-resource-policy-proof/run.sh prove
    bash tests/acpi-resource-policy-proof/check-result.sh "$PWD" "$RESULT_DIRECTORY" "$RECEIPT_JSON"

The phase limit is 180 seconds, VC limit 5 seconds, level 1, one job, 64 MiB
stack and 1 MiB log. The closed project includes decoder and reused FADT-record
dependencies; only these three bodies are selected. The earlier four-unit proof
is retained separately on identical dependency sources. Do not count process
status alone as acceptance. The checker verifies input/prover hashes, selected
unit eligibility, resolved flow and a valid proof attempt at every check tree.

Retained acceptance: resource-policy-proof-04NavOxW/source,
tests/acpi-resource-policy-proof/results/prove-5InLNd20, flow-5XLJ3Wbc and
check-pPgdehdA. Publication carries identical Ada/GPR inputs. The publication's
updated documentation, checker and receipt are metadata added after that run.
No body or public contract changes were needed for this proof. The associated
seven-main hosted suite passed 2,431,438 checks/profile in release and checked
builds, independently auditing source/tool/runtime/ALI/compiler/artifact hashes.
