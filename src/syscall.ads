-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2020 Jon Andrew
--
-- @summary
-- SPARK syscall entry point
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with StackFrame;

package Syscall with
    SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- The basic set of syscalls will be those required for Newlib support
    ---------------------------------------------------------------------------
    subtype SyscallNumber is Unsigned_64;
    SYSCALL_EXIT    : SyscallNumber := 0;
    SYSCALL_READ    : SyscallNumber := 1;
    SYSCALL_CLOSE   : SyscallNumber := 2;
    SYSCALL_EXECVE  : SyscallNumber := 3;
    SYSCALL_FORK    : SyscallNumber := 4;
    SYSCALL_FSTAT   : SyscallNumber := 5;
    SYSCALL_GETPID  : SyscallNumber := 6;
    SYSCALL_KILL    : SyscallNumber := 7;
    SYSCALL_SBRK    : SyscallNumber := 8;
    SYSCALL_TIMES   : SyscallNumber := 9;
    SYSCALL_UNLINK  : SyscallNumber := 10;
    SYSCALL_WAIT    : SyscallNumber := 11;
    SYSCALL_WRITE   : SyscallNumber := 12;

    ---------------------------------------------------------------------------
    -- syscallHandler
    --
    -- This is called from syscall_entry.asm after switching to the running
    --  process' kernel stack
    ---------------------------------------------------------------------------
    function syscallHandler(arg0,
                            arg1,
                            arg2,
                            arg3,
                            arg4,
                            arg5,
                            syscallNum : in Unsigned_64) return Unsigned_64
        with Export => True, Convention => C, External_Name => "syscallHandler";

    ---------------------------------------------------------------------------
    -- syscallReturn is in syscall_entry.asm
    ---------------------------------------------------------------------------
    procedure syscallReturn(retVal : in Unsigned_64)
        with Import => True, Convention => C, External_Name => "syscallReturn";

end Syscall;