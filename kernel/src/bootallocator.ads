-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
-- Early Boot Physical Memory Allocator
-- Single-threaded: used only until the buddy allocator takes ownership.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Config;
with Buddy_Boot_Admission;
with Boot_Frame_Allocator;
with MemoryAreas;
with Firmware_Frames;
with Virtmem; use Virtmem;

package BootAllocator with
    Abstract_State => BitmapState,
    Initializes => (BitmapState, initialized),
    SPARK_Mode => On
is
    -- 2**18 = 4 KiB/page * 64 pages/bitmap word.
    MAX_BITMAP_BLOCKS : constant := Config.MAX_BOOT_ALLOC / 2**18;
    MAX_BOOT_PFN : constant := Buddy_Boot_Admission.Last_Frame (MAX_BITMAP_BLOCKS);
    subtype AllocSize is Positive range 1 .. MAX_BOOT_PFN;

    initialized : Boolean := False with Ghost;
    OutOfMemoryException : exception;
    OutOfBoundsException : exception;

    procedure setup (areas : in MemoryAreas.MemoryAreaArray;
                     Map : Firmware_Frames.Region_Array) with
        Global => (Output => (BitmapState, initialized,
                              Virtmem.MAX_PHYS_ADDRESSABLE, Virtmem.MAX_PHYS_USABLE)),
        Post => initialized;

    function isFree (frame : Virtmem.PFN) return Boolean with
        Global => (Input => BitmapState, Proof_In => initialized),
        Pre => initialized and then frame <= MAX_BOOT_PFN;

    -- High-water mark is inclusive and owned by the reservation core.
    function highestPFNAllocated return Virtmem.PFN with
        Global => (Input => BitmapState),
        Post => highestPFNAllocated'Result <= MAX_BOOT_PFN;

    -- Allocation failure raises before any bitmap/high-water mutation.
    procedure allocFrame (addr : out Virtmem.PhysAddress) with
        Global => (In_Out => BitmapState, Proof_In => initialized),
        Pre => initialized,
        Post => addr <= MAX_BOOT_PFN * Virtmem.FRAME_SIZE;

    procedure allocFrames (num : AllocSize; addr : out Virtmem.PhysAddress) with
        Global => (In_Out => BitmapState, Proof_In => initialized),
        Pre => initialized,
        Post => addr <= MAX_BOOT_PFN * Virtmem.FRAME_SIZE;

    -- Diagnostics only; computed from the authoritative bitmap on demand.
    function numFreeFrames return Unsigned_64 with
        Global => (Input => BitmapState);

private
    package Frames is new Boot_Frame_Allocator (MAX_BOOT_PFN);
    reservations : Frames.State with Part_Of => BitmapState;
end BootAllocator;
