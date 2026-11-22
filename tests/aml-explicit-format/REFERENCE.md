# Explicit Decimal/Hex conversion observations

This bundle imports every declared audit entry from frozen explicit-format-oracle-bppdchco and explicit-format-local-oracle-t_4a2m9o, preserving exact bytes and relative paths under reference/group1 and group2. The combined inventory has 66 executable observations: 60 returned values and six AE_AML_OPERAND_TYPE results mapped exactly to UNSUPPORTED_VALUE. There are no compiler rejections. Neither input audit declares a nested cache; do not filter any future manifest-declared opaque evidence.

Every case binds DSDT checksum, signature, revision, declared length and byte hash, plus runtime WIDT witness and the mode-specific opcode in TEST disassembly. Returned String observations are full ToBuffer-wrapped bytes, including the terminator, not raw newline-delimited text. Scalar type/length witnesses and MARK effects remain exact separate outcomes. The same STATUS/tree/MARK runner protocol supports both modes; no additional width invocation is required of the interpreter runner.

Use `python3 compare.py --mode release --runner /absolute/path/runner --output /new/disjoint/results` (or checked). The driver applies 1 GiB address-space and 64 MiB stack limits and a 30-second per-case timeout; it preserves partial output and incremental outcomes on failure. It binds manifest, cases and runner digests before and after execution. Existing raw evidence remains distinct from semantic expectations.

Lightweight synthetic testing passed: 90 generic parser checks and 528 full-corpus checks (66 independent valid renderings plus corruptions/process-failure cases). No compiler, Nix build, solver, native execution or actual interpreter replay was run. Host command wrapper emitted stream-fd warnings; both Python test logs report success and the combined tool command exited 0. Actual replay is pending implementation integration. Source tests exercise byte framing/cardinality/type/value/marker/error matching, not conversion correctness or subprocess timeout behavior.

Successor adds four late Local-source observations from explicit-format-late-slot-7wmyapdv without changing the original66 expectations. Final inventory70:64returns and6exact operand-type errors. Historical synthetic logs copied from66case parent remain provenance; successor logs are separately named. No actual interpreter replay yet.

Successor lightweight tests passed90 generic parser checks and560 corpus synthetic checks, including all70 positive expected outputs. Production/interpreter replay remains pending.
