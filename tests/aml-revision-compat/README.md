# CuBit interpreter Revision compatibility

The proposed interpreter build identifier is20261122 in calendar form, following
ACPICA's convention while identifying CuBit. It is distinct from `_REV` and the
table revision. ACPI6.6§19.6.121 permits an implementation revision without fixing
its numeric format. Original ASLTS constant assertions remain unchanged.

Export TMPDIR=/home/doc/cubit-build-tmp before Nix, then run
`bash tests/aml-revision-compat/run.sh`. `--validate-only` checks admission only. Unchanged accepted decoder proofs are reused by default; `--prove` explicitly reruns changed units after an edit or at the operator's request.
Fresh source/tool/ALI/binary/report hashes and commands are preserved.

Actual Read_Integer has18checks/level1+2 (17prover,1flow) for AoRTE,termination,
consumed bounds and exact Revision/truncated/unsupported extension behavior.
Standard compiler intrinsic Interfaces.Shift_Left effects/AoRTE are explicitly
trusted; this is not zero external dependency proof. Ordinary literal numeric
correctness, full opcode execution, whole interpreter/service and native behavior
are not established by this decoder contract. Existing pure decoder regressions
pass789825checks/profile checked+release. The separate full original constantn64
gate passes on this proposal; the original baseline failure is retained.
