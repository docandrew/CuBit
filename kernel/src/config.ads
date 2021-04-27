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
    TIME_SLICE : constant := 20;

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

    -- File/device descriptors per process
    PER_PROCESS_DESCRIPTORS : constant := 256;

    -- Number of files open on the system at once
    MAX_OPEN_FILES : constant := 256;

    -- Maximum number of chars in a file name and full path.
    -- @TODO consider moving this to a limits.ads file later on
    FILENAME_MAX_LENGTH : constant := 256;
    FILEPATH_MAX_LENGTH : constant := 4096;

    -- Process IDs for CuBit services
    SERVICE_IDLE_PID     : constant := 1;
    SERVICE_KEYBOARD_PID : constant := 2;
end Config;
