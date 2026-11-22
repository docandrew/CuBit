-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- Interrupt Stack Frame
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;

package StackFrame is
    ---------------------------------------------------------------------------
    -- InterruptStackFrame is a combination of saved registers as well
    -- as information pushed there automatically by the CPU
    ---------------------------------------------------------------------------
    type InterruptStackFrame is
    record
        -- regs we push on stack to restore later
        rax : Unsigned_64;
        rbx : Unsigned_64;
        rcx : Unsigned_64;
        rdx : Unsigned_64;
        rbp : Unsigned_64;
        rsi : Unsigned_64;
        rdi : Unsigned_64;
        r8  : Unsigned_64;
        r9  : Unsigned_64;
        r10 : Unsigned_64;
        r11 : Unsigned_64;
        r12 : Unsigned_64;
        r13 : Unsigned_64;
        r14 : Unsigned_64;
        r15 : Unsigned_64;
        
        -- info about the exception we push for interrupt identification
        interruptNumber : Unsigned_64;  -- for syscalls, this is the syscall #
        errorCode : Unsigned_64;

        -- stuff placed on stack by the CPU
        rip     : System.Address;
        cs      : Unsigned_64;
        rflags  : Unsigned_64;
        rsp     : System.Address;
        ss      : Unsigned_64;
    end record with Size => 64 * 22;

    ---------------------------------------------------------------------------
    -- The errorCode word of a frame SYSCALL entry builds, and of a thread's
    -- first frame (no CPU error code reaches 2 ** 31), so the three kinds
    -- of saved user frame are told apart (DBG-001; Debug_ABI repeats them
    -- for userspace, and Debug_Sessions checks they agree). SYSCALL_FRAME_TAG
    -- in cubit.inc must match the first.
    ---------------------------------------------------------------------------
    System_Call_Frame_Tag : constant := 16#5359_5343#;   -- "SYSC"
    Initial_Frame_Tag     : constant := 16#494E_4954#;   -- "INIT"
end StackFrame;