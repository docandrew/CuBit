-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
-- Early Boot Physical Memory Allocator
-- Single-threaded: used only until the buddy allocator takes ownership.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Buddy_Boot_Admission;
with Boot_Frame_Allocator;
with MemoryAreas;
with Firmware_Frames;
with Virtmem; use Virtmem;

package BootAllocator with
    Abstract_State => BitmapState,
    Initializes => BitmapState,
    SPARK_Mode => On
is
    -- The boot allocator covers the bootstrap window that boot.asm maps
    -- (identity and linear, 512 x 2 MiB), and only that window. This is the
    -- one physical ceiling, not a capacity budget: Mem_mgr.setup writes the
    -- page-table frames it allocates here through the bootstrap linear map,
    -- before switchAddressSpace maps the rest of RAM. Boot modules must lie
    -- in this window too (Boot_Modules.Window_End). WHICH frames are free is
    -- derived from the firmware memory map in setup: usable RAM, minus the
    -- retained boot arena (kernel, stack, loader metadata and every module,
    -- wherever the loader placed them), minus anything reserved.
    BOOTSTRAP_FRAMES : constant :=
      Virtmem.BOOTSTRAP_PHYSICAL_LIMIT / Virtmem.FRAME_SIZE;
    FRAMES_PER_BITMAP_WORD : constant := Buddy_Boot_Admission.Bit_Index'Last + 1;
    BITMAP_WORDS : constant := BOOTSTRAP_FRAMES / FRAMES_PER_BITMAP_WORD;
    pragma Compile_Time_Error
      (BOOTSTRAP_FRAMES mod FRAMES_PER_BITMAP_WORD /= 0,
       "Bootstrap window must fill whole boot bitmap words");
    MAX_BOOT_PFN : constant := Buddy_Boot_Admission.Last_Frame (BITMAP_WORDS);
    subtype AllocSize is Positive range 1 .. MAX_BOOT_PFN;

    -- Unset until setup; Active while it hands out frames; Retired once the
    -- buddy allocator owns every frame it left free. A retired allocator's
    -- bitmap is no longer the kernel's: it lies in the reclaimable boot
    -- section (linker.ld), which goes to the buddy allocator, so nothing may
    -- read or reserve after Retire.
    type Allocator_Phase is (Unset, Active, Retired);
    function Phase return Allocator_Phase with
        Global => (Input => BitmapState);

    OutOfMemoryException : exception;
    OutOfBoundsException : exception;
    RetiredException     : exception;

    procedure setup (areas : in MemoryAreas.MemoryAreaArray;
                     Map : Firmware_Frames.Region_Array) with
        Global => (In_Out => BitmapState,
                   Output => (Virtmem.MAX_PHYS_ADDRESSABLE, Virtmem.MAX_PHYS_USABLE)),
        Pre => Phase = Unset,
        Post => Phase = Active;

    function isFree (frame : Virtmem.PFN) return Boolean with
        Global => (Input => BitmapState),
        Pre => Phase = Active and then frame <= MAX_BOOT_PFN;

    -- High-water mark is inclusive and owned by the reservation core.
    function highestPFNAllocated return Virtmem.PFN with
        Global => (Input => BitmapState),
        Pre => Phase = Active,
        Post => highestPFNAllocated'Result <= MAX_BOOT_PFN;

    -- Allocation failure raises before any bitmap/high-water mutation.
    procedure allocFrame (addr : out Virtmem.PhysAddress) with
        Global => (In_Out => BitmapState),
        Pre => Phase = Active,
        Post => Phase = Active and then addr <= MAX_BOOT_PFN * Virtmem.FRAME_SIZE;

    procedure allocFrames (num : AllocSize; addr : out Virtmem.PhysAddress) with
        Global => (In_Out => BitmapState),
        Pre => Phase = Active,
        Post => Phase = Active and then addr <= MAX_BOOT_PFN * Virtmem.FRAME_SIZE;

    -- Diagnostics only; computed from the authoritative bitmap on demand.
    function numFreeFrames return Unsigned_64 with
        Global => (Input => BitmapState),
        Pre => Phase = Active;

    -- The buddy allocator has taken every frame left free here; no further
    -- query or reservation is possible (each entry also refuses at run time,
    -- since the kernel is built without assertion checks).
    procedure Retire with
        Global => (In_Out => BitmapState),
        Pre => Phase = Active,
        Post => Phase = Retired;

    -- The bitmap's own storage: page-aligned and alone in its pages (the
    -- linker's reclaimable boot section), returned once Retired.
    BOOT_RECLAIM_SECTION : constant String := ".bss.boot_reclaim";

private
    package Frames is new Boot_Frame_Allocator (MAX_BOOT_PFN);
    reservations : Frames.State with Part_Of => BitmapState,
        Linker_Section => BOOT_RECLAIM_SECTION;
    -- Outside the reclaimable section: it must outlive the bitmap.
    currentPhase : Allocator_Phase := Unset with Part_Of => BitmapState;
    function Phase return Allocator_Phase is (currentPhase);
end BootAllocator;
