-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- Global configuration constants
-------------------------------------------------------------------------------
with System.Parameters;

with Serial; use Serial;

-- Global configuration items
package Config with
    SPARK_Mode => On
is
   
    -- Mirror console output to serial port
    serialMirror : constant Boolean         := True;
    serialMirrorPort : constant SerialPort  := Serial.COM1;

    -- Max supported number of logical CPUs
    MAX_CPUS : constant := 128;

    -- ms before context switch takes place.
    TIME_SLICE : constant := 10;

    -- Debug flags that can be set at runtime
    debugFlag : Boolean := False;

    -- Amount of memory that BootAllocator can allocate (in whole frames)
    MAX_BOOT_ALLOC : constant := 2**26; -- (64 MiB)

    -- Physical allocators will ignore everything below this address.
    MIN_PHYS_ALLOC : constant := 16#100000#;

    -- 2^MAX_BUDDY_ORDER is the largest contiguous memory chunks that
    -- we keep track of in our physical BuddyAllocator
    MAX_BUDDY_ORDER : constant := 12;

    -- This is the number of times that a slab allocator will expand and
    -- grab more underlying physical memory from the BuddyAllocator
    MAX_SLAB_EXPAND_TIMES : constant := 8;

    -- Note that the actual secondary stack size will be approx. 24 bytes
    -- smaller due to fields in the stack structure itself (see s-secsta.ads)
    SECONDARY_STACK_SIZE : constant
        := System.Parameters.Runtime_Default_Sec_Stack_Size;

    -- Number of device blocks to cache in memory
    NUM_BLOCK_BUFFERS : constant := 16;

    -- Max number of processes/threads we can run
    MAX_PROCESSES : constant := 512;

    -- Average number of pages per process
    PAGES_PER_PROCESS : constant := 256;

    -- File/device descriptors per process
    PER_PROCESS_DESCRIPTORS : constant := 256;

    -- Capabilities per process (capability table slots).
    -- 64 is sufficient for early bring-up; expect to grow this (256+) once
    -- capabilities replace file descriptors and per-connection endpoints.
    PER_PROCESS_CAPABILITIES : constant := 64;

    -- When True, legacy send() from USER processes requires a matching
    -- CAP_ENDPOINT. Kernel threads are exempt.
    ENFORCE_IPC_CAPS : constant Boolean := True;

    -- Number of files open on the system at once
    MAX_OPEN_FILES : constant := 256;

    -- Maximum number of chars in a file name and full path.
    -- @TODO consider moving this to a limits.ads file later on
    FILENAME_MAX_LENGTH : constant := 256;
    FILEPATH_MAX_LENGTH : constant := 4096;

    -- Per-CPU idle processes use PIDs 1..MAX_SMP_CPUS (reserved range).
    -- CPU 0 = PID 1, CPU 1 = PID 2, etc.
    MAX_SMP_CPUS : constant := 8;
    IDLE_PID_BASE : constant := 1;

    -- Process IDs for CuBit services (above idle PID range)
    SERVICE_IDLE_PID       : constant := IDLE_PID_BASE;  -- CPU 0's idle
    SERVICE_FILESYSTEM_PID : constant := 10;
end Config;
