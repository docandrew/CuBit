# Cached CreateField comparator

Portable private replay package. Run `python3 -B compare.py --runner /absolute/path/field_runner --output /new/output/directory`. Runner interface is exactly `[runner, aml_path, "TEST"]`, emitting ASCII STATUS, optional INTEGER/BUFFER/PACKAGE tree, and MARK. Each invocation has a 30-second deadline, 1 GiB address-space cap, 64 MiB stack cap and 1 MiB disk-backed output cap. Runner and all reference inputs are hashed before/after. No ACPICA executable is invoked by this package.

102 executable references: 74 returned results and 28 explicit AML runtime errors, across witnessed 32/64-bit DSDTs. `excluded.json` preserves four normal compiler syntax rejections and two inconclusive original HUGE observations, without scheduling either class for execution. All 108 replay-set attempts have unique source-prefixed IDs. `evidence/` contains complete copies of all six completed oracle directories plus the prior CopyObject disassembly-gate failure directory, including excluded evidence, original logs, disassembly, compilation records, tools metadata and parent manifests. Parent audit entries were checked before import; `parent-inputs.json` hashes every copied file. `provenance.json` names exact parents and observation sources.

The original set's explicit errors use `aml_status`; the two small sets use singleton `ae_statuses`. Each runtime error was cross-checked against the exact `Evaluation of \\TEST failed with status ...` execution-log line. Normalized `exact_error` retains that code. CuBit status mapping:

* AE_AML_OPERAND_VALUE on zero field length -> INVALID_FIELD_LENGTH.
* AE_AML_BUFFER_LIMIT on field extent -> BUFFER_FIELD_LIMIT.
* AE_AML_BUFFER_LIMIT on small ERRORS count conversion from an empty Buffer -> EMPTY_BUFFER (existing CuBit integer-conversion status, confirmed by both integration owners).
* AE_AML_OPERAND_TYPE -> UNSUPPORTED_VALUE.
* AE_ALREADY_EXISTS -> DUPLICATE_NAME.

Thus mapping preserves the responsible operation; it does not globally conflate all ACPICA BUFFER_LIMIT errors with field extents. Exact result bytes, scalar value, and observable MARK must match. Compiler rejection is not a feature failure, missing observations are not successes, and nonzero/timeout runner termination cannot match an expected runtime error.

`test_protocol.py` generates synthetic outputs from expected records and rejects malformed framing, status/result mismatch, changed values/markers, nonzero exits, overlong numbers, recursion, byte-array and output limits. These checks validate comparator behavior and cached evidence integrity only; they are not CuBit interpreter execution, formal proof, native integration or clean ACPICA teardown claims. Retained ACPICA cache diagnostics remain in the evidence. Integration replay remains pending.

The first successor added four normally compiled runtime package capture cases at both integer widths, preserving generic field content65 after a later write produces66 (result16706, MARK3). This is captured-content evidence; unnamed Store is not a separate object-identity proof. Original74-case comparator remains frozen.

This successor preserves the prior78 case records exactly and adds12 CopyObject indexed-reference observations (six shapes at witnessed32/64-bit widths), all Integer66/MARK3. Named and Local targets preserve byte/package indexed references, including initially empty package slots. The first disassembly-gate failure is retained for provenance only and is not an additional replay case. No CuBit replay or formal proof is implied.

This successor preserves prior90 case records and adds12 explicit CopyObject indexed-reference to fixed/generic buffer-field errors. Exact AE_AML_OPERAND_TYPE maps to UNSUPPORTED_VALUE, with MARK1. Source observations do not independently prove destination rollback; local runtime tests remain required.
