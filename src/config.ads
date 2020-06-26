-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- Global configuration constants
-------------------------------------------------------------------------------
with System.Parameters;

with Serial; use Serial;

-- Global configuration items
package config with
    SPARK_Mode => On
is
   
    -- Mirror console output to serial port
    serialMirror : constant Boolean         := True;
    serialMirrorPort : constant SerialPort  := Serial.COM1;

    -- Max supported number of logical CPUs
    MAX_CPUS : constant := 128;

    -- ms before context switch takes place.
    TIME_SLICE : constant := 15;

    -- Debug flags that can be set at runtime
    debugFlag : Boolean := False;

    -- Amount of memory that BootAllocator can allocate (in whole frames)
    MAX_BOOT_ALLOC : constant := 2**26; -- (64 MiB)

    -- Physical allocators will ignore everything below this address.
    MIN_PHYS_ALLOC : constant := 16#100000#;

    -- 2^MAX_BUDDY_ORDER is the largest contiguous memory chunks that
    -- we keep track of in our physical allocator
    MAX_BUDDY_ORDER : constant := 12;

    -- Note that the actual secondary stack size will be approx. 24 bytes
    -- smaller due to fields in the stack structure itself (see s-secsta.ads)
    SECONDARY_STACK_SIZE : constant
        := System.Parameters.Runtime_Default_Sec_Stack_Size;

    -- Number of device blocks to cache in memory
    NUM_BLOCK_BUFFERS : constant := 16;

    -- File/device descriptors per process
    PER_PROCESS_DESCRIPTORS : constant := 256;
end config;
