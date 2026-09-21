with Interfaces; use Interfaces;
-- Singleton metadata adapter. Callers serialize access and initialize before
-- use. Offsets refer to caller-owned, disjoint 16 MiB slab/extent arenas.
package Heap_Runtime is
   Arena_Bytes : constant := 16_777_216;
   Total_Bytes : constant := 2 * Arena_Bytes;
   Maximum_Alignment : constant := 1_048_576;
   No_Allocation : constant Unsigned_64 := Unsigned_64'Last;
   procedure Initialize with Export, Convention => C, External_Name => "cubit_heap_init";
   function Allocate (Bytes, Alignment : Unsigned_64) return Unsigned_64
     with Export, Convention => C, External_Name => "cubit_heap_allocate";
   function Release (Offset : Unsigned_64) return Unsigned_64
     with Export, Convention => C, External_Name => "cubit_heap_release";
   function Usable_Size (Offset : Unsigned_64) return Unsigned_64
     with Export, Convention => C, External_Name => "cubit_heap_usable_size";
end Heap_Runtime;
