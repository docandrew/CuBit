-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2020 Jon Andrew
--
-- @summary
-- SPARK syscall entry point
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;

with Boot_Timeline_Layout;
with Descriptors;
with Wait_ABI;

package Syscall with
    SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- System Calls
    ---------------------------------------------------------------------------
    type SyscallNumber is (
        SYSCALL_EXIT,
        SYSCALL_KILL,
        SYSCALL_SBRK,
        SYSCALL_WRITE,
        SYSCALL_REPLY,
        SYSCALL_SEND_EVENT,
        SYSCALL_MAPFB,
        SYSCALL_INP8,
        SYSCALL_OUTP8,
        SYSCALL_INP16,
        SYSCALL_OUTP16,
        SYSCALL_INP32,
        SYSCALL_OUTP32,
        SYSCALL_SUBMIT_VIA_ENDPOINT_CAPABILITY,
        SYSCALL_REPLY_WAIT,
        SYSCALL_MOVE_REPLY_CAPABILITY,
        SYSCALL_REPLY_AND_CONSUME_REPLY_CAPABILITY,
        SYSCALL_SPAWN,
        SYSCALL_MAP_DEVICE,
        SYSCALL_RESUME,
        SYSCALL_ALLOC_DMA,
        SYSCALL_ENABLE_IRQ,
        SYSCALL_MAP_INTO,
        SYSCALL_SET_CPU,
        SYSCALL_SET_LATENCY_CONTRACT,
        SYSCALL_INSPECT_CAPABILITY,
        SYSCALL_THREAD_CREATE,
        SYSCALL_THREAD_EXIT,
        SYSCALL_FUTEX_WAKE,
        SYSCALL_CREATE_SHARED_MEMORY_GRANT_FOR_PROCESS_ID,
        SYSCALL_REVOKE_SHARED_MEMORY_GRANT,
        SYSCALL_CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY,
        SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION,
        SYSCALL_ACQUIRE_SHARED_MEMORY_GRANT,
        SYSCALL_REVOKE_SHARED_MEMORY_GRANT_REFERENCE,
        SYSCALL_RETURN_SHARED_MEMORY_GRANT_ACQUISITION,
        SYSCALL_ACQUIRE_SHARED_MEMORY_GRANT_VIA_CAPABILITY,
        SYSCALL_YIELD,
        SYSCALL_DERIVE_SHARED_MEMORY_GRANT_VIA_CAPABILITY,
        SYSCALL_INSTALL_LAUNCH_ARGUMENTS,
        SYSCALL_SEND_CONTROL,
        SYSCALL_CALL_VIA_ENDPOINT_CAPABILITY,
        SYSCALL_SEND_VIA_ENDPOINT_CAPABILITY,
        SYSCALL_ALLOCATE_DEVICE_VECTORS,
        SYSCALL_RELEASE_DEVICE_VECTORS,
        SYSCALL_READ_DEVICE_VECTOR,
        SYSCALL_PUBLISH,
        SYSCALL_CLOSE_HANDLE,
        SYSCALL_INSTALL_CAPABILITY,
        SYSCALL_REPLY_WITH_CAPABILITY,
        SYSCALL_CALL_OFFERING_CAPABILITY,
        SYSCALL_REPLY_SAVED_WITH_CAPABILITY,
        SYSCALL_DERIVE_CAPABILITY,
        SYSCALL_RESERVE_OWNED_REGION,
        SYSCALL_CHANGE_OWNED_RANGE,
        SYSCALL_RELEASE_OWNED_REGION,
        SYSCALL_WAIT,
        SYSCALL_REGISTER_DRIVER)
    with Size => Unsigned_64'Size;

    for SyscallNumber use (
        SYSCALL_EXIT                 => 0,
        SYSCALL_KILL                 => 7,
        SYSCALL_SBRK                 => 8,
        -- docs/process-arguments.md
        SYSCALL_INSTALL_LAUNCH_ARGUMENTS => 126,
        -- docs/data-plane.md, "Control messages"
        SYSCALL_SEND_CONTROL         => 127,
        SYSCALL_YIELD                => 118,
        SYSCALL_DERIVE_SHARED_MEMORY_GRANT_VIA_CAPABILITY => 122,
        SYSCALL_WRITE                => 12,
        SYSCALL_REPLY                => 18,
        SYSCALL_SEND_EVENT           => 19,
        SYSCALL_MAPFB                => 29,
        SYSCALL_INP8                 => 30,
        SYSCALL_OUTP8                => 31,
        SYSCALL_INP16                => 32,
        SYSCALL_OUTP16               => 33,
        SYSCALL_INP32                => 36,
        SYSCALL_OUTP32               => 37,
        -- Synchronous calls take a deadline (docs/ipc-fastpath.md, "Call
        -- deadlines"); renumbered from 40 and 41 when it was added, so an
        -- old binary gets an unknown call, not a garbage deadline.
        SYSCALL_SEND_VIA_ENDPOINT_CAPABILITY   => 129,
        SYSCALL_CALL_VIA_ENDPOINT_CAPABILITY   => 128,
        -- Device vectors (docs/msi-interrupts.md)
        SYSCALL_ALLOCATE_DEVICE_VECTORS => 130,
        SYSCALL_RELEASE_DEVICE_VECTORS  => 131,
        SYSCALL_READ_DEVICE_VECTOR      => 132,
        -- docs/boot-timeline.md
        -- docs/syscall-retirement.md: a typed self query and the KERN-002
        -- publication writer. Retired numbers stay unused: 15 (INFO), 20
        -- (RECEIVE_EVENT), 50 (VIRT_TO_PHYS), 77 (SET_SYSINFO), 107
        -- (SET_WELL_KNOWN).
        SYSCALL_PUBLISH         => 135,
        -- CAP-002 (docs/capability-handles.md): give up a handle; place a
        -- narrowed copy in a child through its CSPACE; answer a request
        -- that accepts one with a capability; offer one with a call (the
        -- server takes it explicitly or not at all). Retired: 72 and 120 (policy
        -- mint), 121 (delegate endpoint), 136 (restore reply).
        SYSCALL_CLOSE_HANDLE          => 137,
        SYSCALL_INSTALL_CAPABILITY    => 138,
        SYSCALL_REPLY_WITH_CAPABILITY => 139,
        -- A call that offers one capability, and the server taking it.
        SYSCALL_CALL_OFFERING_CAPABILITY  => 140,
        -- REPLY_WITH_CAPABILITY for a saved reply.
        SYSCALL_REPLY_SAVED_WITH_CAPABILITY => 142,
        -- A derivation of one of the caller's capabilities in its own table.
        SYSCALL_DERIVE_CAPABILITY => 143,
        -- The one wait (ABI-002 slice 2, Wait_ABI): RECEIVE (17),
        -- RECEIVE_UNTIL_MONOTONIC_MILLISECOND (21), POLL_ANY_IPC (22),
        -- WAIT_COMPLETION (24), POLL_COMPLETION (25), RECEIVE_EVENT_NB (26),
        -- SLEEP (28), POLL_SERVICE_REQUEST (80), FUTEX_WAIT (92),
        -- WAIT_FOR_IPC_OR_COMPLETION_UNTIL_MONOTONIC_MILLISECOND (113),
        -- SLEEP_UNTIL_MONOTONIC_MICROSECOND (119) and
        -- ACCEPT_OFFERED_CAPABILITY (141) are retired.
        SYSCALL_WAIT => Wait_ABI.System_Call_Number,
        -- Owned memory, one region API (ABI-002, Process.Owned_Memory):
        -- reserve a region, one operation on a range of it, release it.
        -- Retired, never reused: 115 (ALLOCATE), 116 (RELEASE), 117
        -- (PROTECT), 123 (RESERVE), 124 (COMMIT_PREFIX), 125 (RELEASE
        -- RESERVATION). 145 is the SCI port's.
        SYSCALL_RESERVE_OWNED_REGION => 146,
        SYSCALL_CHANGE_OWNED_RANGE   => 147,
        SYSCALL_RELEASE_OWNED_REGION => 148,
        SYSCALL_SUBMIT_VIA_ENDPOINT_CAPABILITY => 42,
        SYSCALL_REPLY_WAIT           => 48,
        SYSCALL_MOVE_REPLY_CAPABILITY              => 51,
        SYSCALL_REPLY_AND_CONSUME_REPLY_CAPABILITY => 52,
        SYSCALL_SPAWN                => 60,
        SYSCALL_MAP_DEVICE           => 70,
        SYSCALL_RESUME               => 73,
        SYSCALL_ALLOC_DMA            => 74,
        SYSCALL_ENABLE_IRQ           => 75,
        SYSCALL_MAP_INTO             => 76,
        SYSCALL_SET_CPU              => 78,
        SYSCALL_SET_LATENCY_CONTRACT => 81,
        SYSCALL_INSPECT_CAPABILITY => 84,
        -- Threads and futexes (docs/threads.md)
        SYSCALL_THREAD_CREATE        => 90,
        SYSCALL_THREAD_EXIT          => 91,
        SYSCALL_FUTEX_WAKE           => 93,
        SYSCALL_CREATE_SHARED_MEMORY_GRANT_FOR_PROCESS_ID => 102,
        SYSCALL_REVOKE_SHARED_MEMORY_GRANT                  => 103,
        SYSCALL_CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY => 106,
        SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION => 108,
        SYSCALL_ACQUIRE_SHARED_MEMORY_GRANT => 109,
        SYSCALL_REVOKE_SHARED_MEMORY_GRANT_REFERENCE => 110,
        SYSCALL_RETURN_SHARED_MEMORY_GRANT_ACQUISITION => 111,
        SYSCALL_ACQUIRE_SHARED_MEMORY_GRANT_VIA_CAPABILITY => 112,
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
                             syscallNumRaw,
                             arg6 : in Unsigned_64) return Unsigned_64
        with SPARK_Mode => Off, -- effectful dispatch, process/VM/hardware state
             Export => True, Convention => C, External_Name => "syscallHandler";

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
                    count    : in Unsigned_64) return Unsigned_64
        with SPARK_Mode => Off; -- user-address overlay and device writes

end Syscall;
