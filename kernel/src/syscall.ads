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
    type SyscallNumber is (
        SYSCALL_EXIT,
        SYSCALL_GETPID,
        SYSCALL_KILL,
        SYSCALL_SBRK,
        SYSCALL_WRITE,
        SYSCALL_INFO,
        SYSCALL_RECEIVE,
        SYSCALL_REPLY,
        SYSCALL_SEND_EVENT,
        SYSCALL_RECEIVE_EVENT,
        SYSCALL_POLL_ANY_IPC,
        SYSCALL_SUBMIT,
        SYSCALL_WAIT_COMPLETION,
        SYSCALL_POLL_COMPLETION,
        SYSCALL_RECEIVE_EVENT_NB,
        SYSCALL_GETTIME,
        SYSCALL_SLEEP,
        SYSCALL_MAPFB,
        SYSCALL_INP8,
        SYSCALL_OUTP8,
        SYSCALL_INP16,
        SYSCALL_OUTP16,
        SYSCALL_INP32,
        SYSCALL_OUTP32,
        SYSCALL_CAP_SEND,
        SYSCALL_CAP_CALL,
        SYSCALL_CAP_SUBMIT,
        SYSCALL_NOTIFY,
        SYSCALL_REPLY_WAIT,
        SYSCALL_VIRT_TO_PHYS,
        SYSCALL_SAVE_REPLY_CAP,
        SYSCALL_REPLY_CAP,
        SYSCALL_SPAWN,
        SYSCALL_MAP_DEVICE,
        SYSCALL_PROCLIST,
        SYSCALL_MINT_CAP,
        SYSCALL_RESUME,
        SYSCALL_ALLOC_DMA,
        SYSCALL_ENABLE_IRQ,
        SYSCALL_MAP_INTO,
        SYSCALL_SET_SYSINFO,
        SYSCALL_SET_CPU,
        SYSCALL_POLL_SERVICE_REQUEST,
        SYSCALL_SET_LATENCY_CONTRACT,
        SYSCALL_TRACE_RESET,
        SYSCALL_TRACE_SUMMARY,
        SYSCALL_INSPECT_CAP,
        SYSCALL_GRANT,
        SYSCALL_REVOKE,
        SYSCALL_GRANT_VIA_CAP,
        SYSCALL_SET_WELL_KNOWN,
        SYSCALL_REGISTER_DRIVER)
    with Size => Unsigned_64'Size;

    for SyscallNumber use (
        SYSCALL_EXIT                 => 0,
        SYSCALL_GETPID               => 6,
        SYSCALL_KILL                 => 7,
        SYSCALL_SBRK                 => 8,
        SYSCALL_WRITE                => 12,
        SYSCALL_INFO                 => 15,
        SYSCALL_RECEIVE              => 17,
        SYSCALL_REPLY                => 18,
        SYSCALL_SEND_EVENT           => 19,
        SYSCALL_RECEIVE_EVENT        => 20,
        SYSCALL_POLL_ANY_IPC         => 22,
        SYSCALL_SUBMIT               => 23,
        SYSCALL_WAIT_COMPLETION      => 24,
        SYSCALL_POLL_COMPLETION      => 25,
        SYSCALL_RECEIVE_EVENT_NB     => 26,
        SYSCALL_GETTIME              => 27,
        SYSCALL_SLEEP                => 28,
        SYSCALL_MAPFB                => 29,
        SYSCALL_INP8                 => 30,
        SYSCALL_OUTP8                => 31,
        SYSCALL_INP16                => 32,
        SYSCALL_OUTP16               => 33,
        SYSCALL_INP32                => 36,
        SYSCALL_OUTP32               => 37,
        SYSCALL_CAP_SEND             => 40,
        SYSCALL_CAP_CALL             => 41,
        SYSCALL_CAP_SUBMIT           => 42,
        SYSCALL_NOTIFY               => 43,
        SYSCALL_REPLY_WAIT           => 48,
        SYSCALL_VIRT_TO_PHYS         => 50,
        SYSCALL_SAVE_REPLY_CAP       => 51,
        SYSCALL_REPLY_CAP            => 52,
        SYSCALL_SPAWN                => 60,
        SYSCALL_MAP_DEVICE           => 70,
        SYSCALL_PROCLIST             => 71,
        SYSCALL_MINT_CAP             => 72,
        SYSCALL_RESUME               => 73,
        SYSCALL_ALLOC_DMA            => 74,
        SYSCALL_ENABLE_IRQ           => 75,
        SYSCALL_MAP_INTO             => 76,
        SYSCALL_SET_SYSINFO          => 77,
        SYSCALL_SET_CPU              => 78,
        SYSCALL_POLL_SERVICE_REQUEST => 80,
        SYSCALL_SET_LATENCY_CONTRACT => 81,
        SYSCALL_TRACE_RESET          => 82,
        SYSCALL_TRACE_SUMMARY        => 83,
        SYSCALL_INSPECT_CAP          => 84,
        SYSCALL_GRANT                => 102,
        SYSCALL_REVOKE               => 103,
        SYSCALL_GRANT_VIA_CAP        => 106,
        SYSCALL_SET_WELL_KNOWN       => 107,
        SYSCALL_REGISTER_DRIVER      => 2000);

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
                             syscallNumRaw : in Unsigned_64) return Unsigned_64
        with Export => True, Convention => C, External_Name => "syscallHandler";

    ---------------------------------------------------------------------------
    -- syscallReturn is in syscall_entry.asm
    ---------------------------------------------------------------------------
    procedure syscallReturn (retVal : in Unsigned_64)
        with Import => True, Convention => C, External_Name => "syscallReturn";

private
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

end Syscall;
