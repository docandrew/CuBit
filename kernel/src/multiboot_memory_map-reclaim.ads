pragma Ada_2022;

-- A byte-range classification, not permission to map or reclaim these pages.
-- Any overlapping non-reclaim entry wins, regardless of map order.
package Multiboot_Memory_Map.Reclaim with SPARK_Mode, Pure is
   function Unambiguous (Map : Entries; First, Last : Unsigned_64) return Boolean is
     (for all R of Map => R.Empty or else
       (R.First <= R.Last and then
        (R.Last < First or else R.First > Last or else R.Kind = ACPI_Reclaim)));
   function Known_Byte (Map : Entries; A : Unsigned_64) return Boolean with Ghost;
   function Known_Range (Map : Entries; First, Last : Unsigned_64) return Boolean
     with Ghost, Pre => First <= Last;
   function Covers (Map : Entries; First, Last : Unsigned_64) return Boolean
     with Post => (if Covers'Result then First <= Last
       and then Unambiguous (Map, First, Last)
       and then Known_Range (Map, First, Last));
end Multiboot_Memory_Map.Reclaim;
