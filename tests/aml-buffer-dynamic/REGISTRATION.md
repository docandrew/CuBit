# Hosted dynamic Buffer counts

Run in the repository Nix development shell:

```sh
python3 tests/acpi-hosted/run.py --group buffer-dynamic --mode release
python3 tests/acpi-hosted/run.py --group buffer-dynamic --mode checked
```

Export `TMPDIR=/home/doc/cubit-build-tmp` before entering `nix develop`.
The explicit group preserves existing default and `all` selections. Both strict
profiles pass all eight stages: 473 real-owner checks, twenty cached ACPICA
comparisons, 2,774 protocol checks and eighteen evidence-guard checks. The twenty
rows contain sixteen integer flows, two divide-error mappings and two explicitly
coarse target-only error mappings. No compiler exclusions are hidden. Source and
raw oracle evidence hashes are verified before replay; TEST/MARK/POST share one
loaded namespace. The four mapped errors do not imply complete status parity.

The focused fixtures cover count evaluation once, local/argument/method/nested
counts, raw initializer bytes, count side effects before errors or quota failure,
package-bound truncations through targets/Inspect/method actuals, depth and budget
limits, metadata and cyclic-reference rejection, and actual collection retaining
caller operands and captured metadata. Malformed/depth/budget fixtures are local
policy tests, not ACPICA conformance comparisons. Both release and checked modes
run with 64 MiB stacks, single-job builds bounded to 180 seconds, test/replay
children bounded to 30 seconds, and captured logs bounded to 1 MiB. Replay children
also have a 1 GiB address-space bound. The suite does not run proofs or native builds.

Raw public and broad validation results remain in private evidence snapshots
identified by [the validation receipt](VALIDATION.json), including their hashes.
The portable oracle source/evidence corpus is checked in separately under
`../aml-buffer-dynamic-reference/reference`; execution does not require the
private validation paths.
Before the subsequent proved span/layout update, the dynamic production passed
buffer-sizing, metadata and Notify/Alias in both profiles. The combined current
sources subsequently passed this dynamic group in checked mode again; release
evidence remains from the preceding source snapshot. Dynamic declaration loading, nested Package/VarPackage count evaluation,
FieldData sizing, complete AML semantics and current-source SPARK proof remain
unfinished. No live hardware/service deployment follows from these hosted results.
