-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2020 Jon Andrew
--
-- @summary
-- SPARK syscall entry point
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;

with Descriptors;

package Syscall with
    SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- System Calls
    ---------------------------------------------------------------------------
    subtype SyscallNumber is Unsigned_64;
    SYSCALL_EXIT          : constant SyscallNumber := 0;
    SYSCALL_READ          : constant SyscallNumber := 1;
    SYSCALL_CLOSE         : constant SyscallNumber := 2;
    SYSCALL_EXECVE        : constant SyscallNumber := 3;
    SYSCALL_FORK          : constant SyscallNumber := 4;
    SYSCALL_FSTAT         : constant SyscallNumber := 5;
    SYSCALL_GETPID        : constant SyscallNumber := 6;
    SYSCALL_KILL          : constant SyscallNumber := 7;
    SYSCALL_SBRK          : constant SyscallNumber := 8;
    SYSCALL_TIMES         : constant SyscallNumber := 9;
    SYSCALL_UNLINK        : constant SyscallNumber := 10;
    SYSCALL_WAIT          : constant SyscallNumber := 11;
    SYSCALL_WRITE         : constant SyscallNumber := 12;
    SYSCALL_OPEN          : constant SyscallNumber := 13;

    SYSCALL_INFO          : constant SyscallNumber := 15;
    SYSCALL_SEND          : constant SyscallNumber := 16;
    SYSCALL_RECEIVE       : constant SyscallNumber := 17;
    SYSCALL_REPLY         : constant SyscallNumber := 18;
    SYSCALL_SEND_EVENT    : constant SyscallNumber := 19;
    SYSCALL_RECEIVE_EVENT : constant SyscallNumber := 20;
    SYSCALL_CALL          : constant SyscallNumber := 21;
    SYSCALL_RECEIVE_NB    : constant SyscallNumber := 22;

    -- Async I/O Syscalls
    SYSCALL_SUBMIT          : constant SyscallNumber := 23;
    SYSCALL_WAIT_COMPLETION : constant SyscallNumber := 24;
    SYSCALL_POLL_COMPLETION : constant SyscallNumber := 25;
    SYSCALL_RECEIVE_EVENT_NB : constant SyscallNumber := 26;

    -- Time syscalls
    SYSCALL_GETTIME         : constant SyscallNumber := 27;
    SYSCALL_SLEEP           : constant SyscallNumber := 28;

    -- Framebuffer syscall
    SYSCALL_MAPFB           : constant SyscallNumber := 29;

    -- Port I/O syscalls (for userspace drivers)
    SYSCALL_INB             : constant SyscallNumber := 30;
    SYSCALL_OUTB            : constant SyscallNumber := 31;
    SYSCALL_INW             : constant SyscallNumber := 32;
    SYSCALL_OUTW            : constant SyscallNumber := 33;
    SYSCALL_INS16           : constant SyscallNumber := 34;
    SYSCALL_OUTS16          : constant SyscallNumber := 35;

    -- Capability-aware IPC syscalls
    SYSCALL_CAP_SEND        : constant SyscallNumber := 40;
    SYSCALL_CAP_CALL        : constant SyscallNumber := 41;
    SYSCALL_CAP_SUBMIT      : constant SyscallNumber := 42;

    -- Notification syscalls
    SYSCALL_NOTIFY          : constant SyscallNumber := 43;
    SYSCALL_NOTIFY_WAIT     : constant SyscallNumber := 44;
    SYSCALL_NOTIFY_POLL     : constant SyscallNumber := 45;

    -- Notification binding
    SYSCALL_BIND_NOTIFICATION   : constant SyscallNumber := 46;
    SYSCALL_UNBIND_NOTIFICATION : constant SyscallNumber := 47;

    -- Atomic reply+receive
    SYSCALL_REPLY_WAIT      : constant SyscallNumber := 48;

    -- Access Controller Syscalls
    SYSCALL_CONTROLACCESS : constant SyscallNumber := 100;
    SYSCALL_GETTICKET     : constant SyscallNumber := 101;
    SYSCALL_GRANT         : constant SyscallNumber := 102;
    SYSCALL_REVOKE        : constant SyscallNumber := 103;

    -- CONTROLACCESS sub-operation codes (passed in arg0)
    CONTROLACCESS_INSERT  : constant Unsigned_64 := 1;
    CONTROLACCESS_DERIVE  : constant Unsigned_64 := 2;
    CONTROLACCESS_MINT    : constant Unsigned_64 := 3;
    CONTROLACCESS_REMOVE  : constant Unsigned_64 := 4;
    CONTROLACCESS_REVOKE      : constant Unsigned_64 := 5;
    CONTROLACCESS_REVOKE_ALL  : constant Unsigned_64 := 6;

    -- Driver registration
    SYSCALL_REGISTER_DRIVER : constant SyscallNumber := 2000;

    ---------------------------------------------------------------------------
    -- syscallHandler
    --
    -- This is called from syscall_entry.asm after switching to the running
    --  process' kernel stack
    ---------------------------------------------------------------------------
    function syscallHandler (arg0,
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
    procedure syscallReturn (retVal : in Unsigned_64)
        with Import => True, Convention => C, External_Name => "syscallReturn";

private
    ---------------------------------------------------------------------------
    -- open syscall implementation. 
    -- @param filename
    -- @param flags
    -- @param mode
    -- @return a descriptor for the resource requested.
    ---------------------------------------------------------------------------
    function open (filenameLen : in Unsigned_64;
                   filename    : in System.Address;
                   flags       : in Unsigned_64;
                   mode        : in Unsigned_64) return Unsigned_64;

    ---------------------------------------------------------------------------
    -- write syscall implementation
    -- @param fd - open descriptor
    -- @param buf - address of the user buffer from which to get the bytes to
    --  write
    -- @param count - number of bytes to write.
    ---------------------------------------------------------------------------
    function write (fd       : in Descriptors.DescriptorNum;
                    buf      : in System.Address;
                    count    : in Unsigned_64) return Unsigned_64;

    ---------------------------------------------------------------------------
    -- read syscall implementation
    -- @param fd - open descriptor
    -- @param buf - address of the user buffer to place the read bytes
    -- @param count - number of bytes to read
    ---------------------------------------------------------------------------
    function read (fd        : in Descriptors.DescriptorNum;
                   buf       : in System.Address;
                   count     : in Unsigned_64) return Unsigned_64;
end Syscall;
