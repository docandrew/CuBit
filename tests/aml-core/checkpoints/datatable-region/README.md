# Unpromoted DataTableRegion checkpoint

Saved for safekeeping at repository base `209c46e650d9fe4c22f777406e2c3614084c9b20`. This is work in progress,
not production integration or a completion claim. `candidate.patch` contains
the four changed implementation files and two new tests. It is deliberately
not applied to the shared interpreter. The separate project files and reference
script preserve the private verification setup. No generated executables are
included.

## Evidence at checkpoint

- Core executor: 175 lifecycle checks and 2099 legacy checks passed.
- Core/legacy instantiation proof: 527 prover +91 flow checks, none unproved
  or justified; see executor-proof.out. This does not prove service callbacks.
- Real service candidate: 634 checks passed. Instantiated service proof was
  still running when saved; service-proof-in-progress.log is only a snapshot.
- Eight actual DataTableRegion/Field comparisons matched ACPICA for revisions
  1 and 2. The exact shutdown allocation diagnostic also occurred in a minimal
  control and is recorded in reference-report.json. Not an error-free ACPICA run.
- Candidate source hashes were checked against the frozen proof inputs.

## Required repairs and remaining work

The candidate incorrectly uses AML_Names.Valid for table signatures, rejecting
ASF! and leading-digit signatures. Fix this and test Description-table lookup
before promotion. Signature lengths, conversion error ordering, wildcard and
OEM matching need broader specification/reference checks. Native service build
and instantiated proof remain required. Adapt readonly_input_tests to the two
new generic callbacks before running the full suite. Module-level deferred
declarations remain unimplemented; upstream ASLTS has not advanced.

## Recovery

In a separate checkout based on the commit above, first run
`git apply --check tests/aml-core/checkpoints/datatable-region/candidate.patch`,
then apply that patch. Preserve any newer work before doing so. The recorded
hashes identify the exact four base files.

For the original private project layout, copy the checkout's Ada sources from
userspace/lib/acpi to lib/, shared/firmware to firmware/, userspace/services/acpi
to service/, and tests/aml-core to test/ in a new independent directory. Copy
region.gpr, region_service.gpr, reference.gpr and reference.py there. Build with
`nix develop -c gprbuild -p -P <directory>/region_service.gpr` and run
`<directory>/build/region_service_tests` with the hosted 64 MiB stack budget.
The reference script currently names the pinned local ACPICA Nix store path;
adjust only that tool path if restoring elsewhere.
