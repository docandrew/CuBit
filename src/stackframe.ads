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
    --
    -- @TODO: maybe rename this to something more general, or create a similar
    --  object to be used strictly for syscalls
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
        rsp     : Integer_Address;
        ss      : Unsigned_64;
    end record with Size => 64 * 22;
end StackFrame;