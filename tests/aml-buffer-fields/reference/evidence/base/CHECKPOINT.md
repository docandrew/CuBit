# CreateField oracle checkpoint

Worker 16461 terminal 0. 32 normal DSDT attempts, 16 cases at each verified integer width: 24 returned, 6 explicit AML errors, 2 inconclusive extreme-offset cases. No compiler rejects. All 32 runtime width and method-specific opcode gates passed. Original observations and logs are preserved; reviewed-observations.json corrects the automated HUGE classification without changing raw evidence.

Zero-length generic fields fail AE_AML_OPERAND_VALUE; one-bit-overrun and empty-buffer fields fail AE_AML_BUFFER_LIMIT, at both widths. HUGE exits 0 with output ending at Evaluating TEST, without result/error/MARK, and is inconclusive. No crash or acceptance claim.

Fixed-size writes preserve adjacent bits; QWord writing Ones writes four FF plus four zero bytes at 32-bit width, eight FF at 64-bit. Generic 9-bit write spans only requested bits. Anonymous backing remains readable. Rebinding the source name leaves the field attached to old backing (result 1891 = 7*256+99). ObjectType returns 14. A 72-bit generic field reads nine bytes at either width. Source/index/count evaluation marker is 123. Full returned bytes are in reviewed observations.

Pinned ACPICA 20260408, normal iasl -oa only; controlling DSDT revisions plus WIDT witnesses. Each subprocess uses 5 s wall / 128 MiB address space / 64 MiB stack, nice19, pinned Nix tools. Cache allocation diagnostics are retained. This is oracle evidence, not CuBit implementation, GC validation, synchronization, or native proof. Resource-template integration has priority next.
