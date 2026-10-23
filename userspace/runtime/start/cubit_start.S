# CuBit program entry (CAP-003, docs/declaration-checking.md section 3).
#
# The one _start every CuBit program has, in GNU as Intel syntax: the
# Ada and freestanding C start code (userspace/c/crt0.S), the libc's
# (userspace/libc/crt/crt1.S) and the Rust std runtime's (moto-rt-cubit)
# include this file, and so do Rust programs without std. Nothing else
# defines _start; tools/check_declarations.py refuses any other copy.
#
# The kernel starts the main thread at _start with RSP at the top of an
# empty stack (no return address) and RDI = the length of its launch
# block, mapped read-only at a fixed address (docs/process-arguments.md).
# The block may end with the startup table and a trailer (u64 table bytes,
# u64 magic; CuBit.Launch_Envelopes). This code keeps:
#   __cubit_launch_block_length      the whole block's length: the startup
#                                    table (CuBit.Launch_Block,
#                                    cubit::startup::Table::from_start);
#   __cubit_launch_arguments_length  its payload's (the block less the
#                                    padded table and trailer): the launch
#                                    arguments (Ada.Command_Line, the
#                                    launch-configuration readers).
# Both live in initialized data, so start code that clears BSS afterwards
# keeps them. Dropping the length (config-storage's hand-written start,
# CAP-003 R3) left every startup entry absent.
#
# Then it calls the language's start with RDI = the initial stack pointer
# and RSI = the payload's length, RBP = 0 (end of the frame chain), RSP
# 16-byte aligned at the call and the direction flag clear:
#   __cubit_language_start (initial_stack, payload_length), which never
#   returns.
#
# The constants below are the Ada declarations', checked to agree by
# tools/check_declarations.py.
# No braces anywhere in this file: Rust's global_asm reads it as a template.

    # CuBit.Launch_Block.Block_Address (kernel Process_Launch).
    .set CUBIT_LAUNCH_BLOCK_ADDRESS, 0x00005A0000000000
    # CuBit.Launch_Envelopes.Magic ("CUBITLB1" as a little-endian word).
    .set CUBIT_ENVELOPE_MAGIC, 0x31424C5449425543
    # CuBit.Launch_Envelopes.Trailer_Bytes: the table bytes and the magic.
    .set CUBIT_TRAILER_BYTES, 16
    .set CUBIT_TABLE_LENGTH_AT, 16
    .set CUBIT_MAGIC_AT, 8
    # CuBit.Startup_Tables.Maximum_Table_Bytes.
    .set CUBIT_MAXIMUM_TABLE_BYTES, 3216
    # The table is padded to a word.
    .set CUBIT_WORD_BYTES, 8

    .pushsection .data.cubit_launch,"aw",@progbits
    .balign 8
    .global __cubit_launch_block_length
__cubit_launch_block_length:
    .quad 0
    .global __cubit_launch_arguments_length
__cubit_launch_arguments_length:
    .quad 0
    .popsection

    .pushsection .text._start,"ax",@progbits
    .global _start
    .type _start,@function
_start:
    mov qword ptr [rip + __cubit_launch_block_length], rdi
    # The payload's length: the whole block unless a valid trailer says less.
    mov rax, rdi
    cmp rdi, CUBIT_TRAILER_BYTES
    jb 2f
    movabs rcx, CUBIT_LAUNCH_BLOCK_ADDRESS
    add rcx, rdi
    movabs rdx, CUBIT_ENVELOPE_MAGIC
    cmp rdx, qword ptr [rcx - CUBIT_MAGIC_AT]
    jne 2f
    mov rdx, qword ptr [rcx - CUBIT_TABLE_LENGTH_AT]
    cmp rdx, CUBIT_MAXIMUM_TABLE_BYTES
    ja 2f
    add rdx, CUBIT_WORD_BYTES - 1
    and rdx, -CUBIT_WORD_BYTES
    add rdx, CUBIT_TRAILER_BYTES
    cmp rdx, rdi
    ja 2f
    mov rax, rdi
    sub rax, rdx
2:
    mov qword ptr [rip + __cubit_launch_arguments_length], rax
    mov rsi, rax
    mov rdi, rsp
    xor ebp, ebp
    and rsp, -16
    cld
    call __cubit_language_start
    ud2
    .size _start, . - _start
    .popsection

    .pushsection .note.GNU-stack,"",@progbits
    .popsection
