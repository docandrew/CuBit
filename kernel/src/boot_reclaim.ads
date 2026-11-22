-------------------------------------------------------------------------------
-- CuBitOS
-- Boot-only memory returned to the buddy allocator once its last reader is
-- done: the boot allocator's bitmap section and the loader's metadata.
-- (Boot modules have their own lifetime: Modules, Boot_Module_Lifetime.)
-------------------------------------------------------------------------------
with System.Storage_Elements; use System.Storage_Elements;

-- Linker symbols, page tables and physical admission: kernel glue.
package Boot_Reclaim with SPARK_Mode => Off is
    ReclaimException : exception;

    -- Once, right after BuddyAllocator.setup: retire the boot allocator (it
    -- refuses any later use), then give the pages of the reclaimable boot
    -- section (linker.ld: sboot_reclaim .. eboot_reclaim) to the buddy
    -- allocator, their kernel-image alias unmapped.
    procedure Retire_Boot_Allocator;

    -- Once, after Retire_Boot_Allocator: every loader metadata span
    -- (Multiboot.Loader_Metadata) goes to the buddy allocator. Nothing reads
    -- them after getMemoryAreas' snapshot. Pages below the buddy floor
    -- (Config.MIN_PHYS_ALLOC) stay with the legacy low-memory window.
    procedure Release_Loader_Metadata;

    -- Bytes each step returned (diagnostics and tests).
    function Section_Bytes return Storage_Count;
    function Metadata_Bytes return Storage_Count;
end Boot_Reclaim;
