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
    
    -- Access Controller Syscalls
    SYSCALL_CONTROLACCESS : constant SyscallNumber := 100;
    SYSCALL_GETTICKET     : constant SyscallNumber := 101;
    SYSCALL_GRANT         : constant SyscallNumber := 102;
    SYSCALL_REVOKE        : constant SyscallNumber := 103;

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
