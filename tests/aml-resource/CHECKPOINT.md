# Strict resource-template portable comparator

Successor of preserved restemplate-portable-5fnfv5ay draft. Reference72case union unchanged:679 original audited files+cases. Reuses frozen Match byte_protocol.py with only four appended resource statuses. Exact ASCII STATUS/value/MARK grammar rejects unknown, duplicate, trailing, malformed, signed/out-of-range, nonASCII and oversized output. Buffer values0..255, length<=65536, integer/marker<=unsigned64, parser tree/line/output bounds retained.

capture uses a disk-backed TemporaryFile and subprocess RLIMIT_FSIZE1MiB, no unbounded PIPE; reads at most1MiB+1, timeout30sec, AS1GiB/stack64MiB. Overflow child tested and rejected. Filesize limit supplements parser envelope. Runner/reference/driver/protocol hashes checked before and after replay. Output failures produce nonzero gate. Exact four resource error mappings plus OperandType retained; no broad Unsupported mapping.

1449 Python selftests passed:72valid expected outputs, failure exit, unknown/duplicate/trailing/malformed statuses and markers, signs/oversized/nonnumeric/nonASCII values, excessive Buffer/output, and actual bounded-output child. This is parser/capture validation, not an Ada build or CuBit oracle replay. No production edits. Reference manifests remain portable relativepaths; origins retained for provenance only.
