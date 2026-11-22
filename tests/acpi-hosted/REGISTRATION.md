# Additional hosted groups

capacity and static-buffer are included in all. static-buffer-oracle is separately selectable because it replays cached reference evidence. service-api selects migrated Core/Bootstrap callers in disjoint aml-core and acpi-integration projects; use --mode release or checked. Legacy raw namespace fixtures remain supported and are not service API migrations.

compile-original builds tests/aml-core/aml.gpr in its existing checked profile regardless --mode. original-tests intentionally does not build: run compile-original immediately beforehand against unchanged sources. Argument-taking *_runner.adb mains are compiled but not implicitly executed. Compile success is not CLI execution evidence.

All new paths are repository-relative; no private snapshot script is used. Do not import an adapted TEST oracle over the original ASLTS MN00 runner.

`mixed-comparison` is a separately selectable focused group: pure66, owner46 and Buffer226 checks (338 total) plus60 cached ACPICA classifications per profile (58 scalar outcomes,2 expected empty-Buffer errors). It preserves `all` and existing groups. See tests/aml-mixed/README.md for resource bounds, provenance and proof scope.

The canonical Buffer comparison fixture is also selected by this group (226 checks per profile); its security/current-byte cases remain intact.

`buffer-fields` is a separately selectable focused group. It preserves the
existing `all` selection and runs eleven explicit runtime programs, the complete
102-case cached ACPICA corpus, and compile-only checks for ten direct evaluator
adapters. Invoke inside the Nix development shell:

```sh
python3 tests/acpi-hosted/run.py --group buffer-fields --mode release
python3 tests/acpi-hosted/run.py --group buffer-fields --mode checked
```

Export `TMPDIR=/home/doc/cubit-build-tmp` before entering `nix develop` in the
shared development environment. Both profiles use strict warnings and a single
compiler job; release disables runtime assertions and checked enables assertions
and overflow checks. The driver uses bounded streamed subprocess logs, explicit
timeouts and a 64 MiB stack. It hashes input sources/reference evidence and checks
actual compiler-selected paths for duplicate-named adapter units.

Reference outcomes distinguish returned values from expected runtime errors.
Four normal compiler rejections and two inconclusive attempts remain separate
exclusions; no forced compilation or missing-result success is counted. Skipped
mains, if explicitly requested, do not count as executed coverage. This group is
hosted test evidence only: no full SPARK proof, full ASLTS pass, native deployment
or hardware validation follows from its success.

The explicit `metadata` group runs ten focused mains plus the complete cached
82-case metadata replay and its guard/protocol checks in either strict profile.
See [metadata registration](../aml-metadata/REGISTRATION.md) for commands, limits
and the distinction between tested behavior and remaining proof/conformance work.

The explicit `buffer-dynamic` group runs the real-owner dynamic-count boundary
and collection fixtures plus twenty cached ACPICA comparisons. Existing default
and `all` selections are unchanged. See [dynamic Buffer registration](../aml-buffer-dynamic/REGISTRATION.md).


The explicit `table-fields` group passes both strict profiles with five mains
(662,835 checks), twelve cached ACPICA endpoint/type/length comparisons,
1,862 protocol checks and 13 evidence guards. Default and `all` remain unchanged.
See [registration and proof scope](../aml-table-fields/README.md).


The opt-in `capture` hosted selection passed 13,798 checks in each profile on the integrated operand-capture and region-selector source. Separate compatibility validation completed through preserved runs: all 26 adapter compilation stages and five runtime mains per profile, including the original checked typed test (26,138 checks in 74.477 seconds under an explicit one-run 120-second exception). Earlier 30/60-second typed timeouts remain evidence; portable limits stay unchanged. Release bootstrap counters are not assertion coverage; checked bootstrap assertions passed separately. See `tests/aml-capture/README.md` for predecessor and carried evidence scopes. Complete owner sharing/Store width behavior, whole-interpreter proofs and native integration remain unfinished. Logger behavior is unchanged.


## Integer ownership portable registration

The opt-in `integer-ownership` hosted group selects the reviewed wide integer/ownership regression. Its dedicated runner also exposes `conversion` (seven focused ToInteger mains) and `completion` (the exact ten-case ToInteger completion replay). See `tests/aml-integer-ownership/README.md`. Private functional evidence is the 98-stage wide gate, the 30-stage conversion gate (29,190 checks/profile, including 28 pure-parser checks), and the separately dependency-matched 151-check/20-observation completion replay. The portable completion selection passed both profiles (151 checks/20 observations each) with explicit checked overflow checks, and 14 bounded guard/cleanup checks passed. The complete portable wide/conversion selections were not rerun; their identical private source gates remain the stated evidence. Repinned older manifests are not new execution evidence.

Existing capture and package-store manifests are refreshed for the current source inventory while their historical receipts remain scoped to their recorded predecessor sources. No new SPARK proof, native result, full sharedness/cycle equivalence or complete AML-support claim is made. Event and device-initialization work remains separate.


The proposed opt-in `service-milestone` group registers the Event/field/startup milestone. See `tests/acpi-service-milestone/README.md` for exact scope and pending integration validation. Existing selections remain unchanged.
