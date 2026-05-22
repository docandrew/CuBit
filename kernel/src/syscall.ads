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
    SYSCALL_POLL_ANY_IPC  : constant SyscallNumber := 22;

    -- Async I/O Syscalls
    SYSCALL_SUBMIT          : constant SyscallNumber := 23;
    SYSCALL_WAIT_COMPLETION : constant SyscallNumber := 24;
    SYSCALL_POLL_COMPLETION : constant SyscallNumber := 25;
    SYSCALL_RECEIVE_EVENT_NB : constant SyscallNumber := 26;

    -- Typed IPC polling.
    -- Service-request polling never consumes events/notifications. Mixed IPC
    -- polling is available through SYSCALL_POLL_ANY_IPC above and should be
    -- rare enough to look suspicious at review time.
    SYSCALL_POLL_SERVICE_REQUEST : constant SyscallNumber := 80;

    -- Time syscalls
    SYSCALL_GETTIME         : constant SyscallNumber := 27;
    SYSCALL_SLEEP           : constant SyscallNumber := 28;

    -- Framebuffer syscall
    SYSCALL_MAPFB           : constant SyscallNumber := 29;

    -- Port I/O syscalls (for userspace drivers)
    SYSCALL_INP8            : constant SyscallNumber := 30;
    SYSCALL_OUTP8           : constant SyscallNumber := 31;
    SYSCALL_INP16           : constant SyscallNumber := 32;
    SYSCALL_OUTP16          : constant SyscallNumber := 33;
    SYSCALL_INPS16          : constant SyscallNumber := 34;
    SYSCALL_OUTPS16         : constant SyscallNumber := 35;
    SYSCALL_INP32           : constant SyscallNumber := 36;
    SYSCALL_OUTP32          : constant SyscallNumber := 37;

    -- Virtual-to-physical address translation
    SYSCALL_VIRT_TO_PHYS    : constant SyscallNumber := 50;

    -- Move reply cap from slot 63 to another slot (for deferred replies)
    SYSCALL_SAVE_REPLY_CAP  : constant SyscallNumber := 51;
    SYSCALL_REPLY_CAP       : constant SyscallNumber := 52;

    -- Process spawning
    SYSCALL_SPAWN           : constant SyscallNumber := 60;

    -- Device MMIO mapping (for userspace drivers)
    SYSCALL_MAP_DEVICE      : constant SyscallNumber := 70;

    -- Process listing (for ps command)
    SYSCALL_PROCLIST        : constant SyscallNumber := 71;

    -- Capability minting (for process managers)
    SYSCALL_MINT_CAP        : constant SyscallNumber := 72;

    -- Resume a suspended process
    SYSCALL_RESUME          : constant SyscallNumber := 73;

    -- Device manager syscalls
    SYSCALL_ALLOC_DMA       : constant SyscallNumber := 74;
    SYSCALL_ENABLE_IRQ      : constant SyscallNumber := 75;
    SYSCALL_MAP_INTO        : constant SyscallNumber := 76;
    SYSCALL_SET_SYSINFO     : constant SyscallNumber := 77;
    SYSCALL_SET_CPU         : constant SyscallNumber := 78;
    SYSCALL_SET_SUPERVISOR  : constant SyscallNumber := 79;

    -- Scheduler latency contracts. arg0 = class, arg1 = period us,
    -- arg2 = budget us, arg3 = reserved flags. This is process-local for now;
    -- privileged cross-process scheduling policy belongs in procmgr later.
    SYSCALL_SET_LATENCY_CONTRACT : constant SyscallNumber := 81;

    -- Kernel trace controls. TRACE_RESET enables the low-overhead per-CPU
    -- trace ring; TRACE_SUMMARY disables tracing and prints aggregate counts.
    SYSCALL_TRACE_RESET      : constant SyscallNumber := 82;
    SYSCALL_TRACE_SUMMARY    : constant SyscallNumber := 83;

    -- Read-only capability inspection. arg0 = target PID, arg1 = cap slot,
    -- arg2 = user buffer for a 48-byte packed capability summary. Requires
    -- CAP_PROCESS + RIGHT_READ for the target process.
    SYSCALL_INSPECT_CAP      : constant SyscallNumber := 84;

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

    -- Service discovery syscalls
    SYSCALL_GRANT_VIA_CAP   : constant SyscallNumber := 106;
    SYSCALL_SET_WELL_KNOWN  : constant SyscallNumber := 107;

    -- CONTROLACCESS sub-operation codes (passed in arg0)
    -- CONTROLACCESS_INSERT (1) removed: capability bypass vulnerability.
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
