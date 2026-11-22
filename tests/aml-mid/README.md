# Hosted Mid tests and cached ACPICA comparisons

From the repository root in its Nix shell:

    python3 tests/acpi-hosted/run.py --group mid --mode release
    python3 tests/acpi-hosted/run.py --group mid --mode checked

This explicit group builds the runner and three fixtures: collecting (148 checks), owner (4,294 checks), and pure slices (15,151 checks). It also runs 56 selected cached reference comparisons. Six normal compiler rejections are recorded separately and are not executed.

The original 88 attempts include superseded constant-folded results, compiler internal failures, and failed result externalizations. They are provenance, not 88 independent passing cases. Eight ordering cases require the exact post-failure SEEN marker. Returned types, complete bytes and nested package values are checked. The current String output grammar supports these ASCII fixtures without embedded newlines. See the reference manifest and compare.py. No ACPICA installation or network is needed for replay.

Owner and slices fixtures were copied exactly from their frozen origins. The parent integration passed its collecting, oracle and caller gates; this canonical registration passed both strict profiles (worker 71104): 19,593 focused checks and 56 cached comparisons per profile, with six compiler rejections recorded separately. No production code changed. This does not establish full ASLTS conformance, native execution or a new proof. The existing all group is unchanged.

Set TMPDIR before entering Nix, use nice19 and one build job, and allow the documented 64 MiB hosted stack according to coordination rules.
