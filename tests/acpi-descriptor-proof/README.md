# Resource descriptor milestone proof

The actual parent framing body and Basic_Descriptors, Extended_Interrupts and
Address_Spaces bodies discharged 297 checks / 517 goals plus 33 flow checks at
level 1, with all 14 entities in SPARK, no skipped checks, assumptions or unresolved
messages. Per-unit proof checks/goals: parent 51/93, basic 69/112, extended 86/156,
address 91/156. This proves selected runtime safety and existing contracts for
framing/extent, progress, quotas and unused interrupt suffixes, initialization,
dependencies and termination. It does not prove full field-value or checksum
functional equivalence, address geometry, platform authority or whole AML.

Use the pinned Nix shell, setting TMPDIR=/home/doc/cubit-build-tmp before and
inside Nix. Run phases separately and inspect each before advancing:

    nice -n19 bash tests/acpi-descriptor-proof/run.sh check
    nice -n19 bash tests/acpi-descriptor-proof/run.sh flow
    nice -n19 bash tests/acpi-descriptor-proof/run.sh prove
    bash tests/acpi-descriptor-proof/check-result.sh "$PWD" "$RESULT_DIRECTORY" "$RECEIPT_JSON"

Per-phase 180-second wall limit, per-VC 5-second limit, level 1, one job and
64 MiB stack are unchanged. The result checker requires exact registered inputs,
prover hash, all four selected units, resolved flow and every proof tree valid.
Process status alone is insufficient; a proof run may exit zero with unresolved
checks. Result paths must belong to this source tree's results directory.

Retained acceptance: resource-descriptor-proof-contract-B7IuHOBY/source,
results/prove-4IDPuq9t, with flow-kXi9jWPc. Publication carries identical Ada/GPR
inputs. Publication documentation and result checker are new metadata, not
represented as having been present during the retained run. Original parent-only
proof remains separately retained and unchanged in source scope.

The milestone made absent optional-source initialization explicit in two bodies
and gave the numeric-field helper a validated full-span precondition. Original
six unresolved flow checks, two helper proof obligations and an intermediate Ada
aspect-placement syntax error are preserved in predecessor snapshots. No proof
assumption, weakened postcondition, numeric limit increase or algorithm change
was used. Hosted resource regressions check both release and assertion profiles.
