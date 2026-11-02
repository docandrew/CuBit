pragma Ada_2022;
with Interfaces;
package body ACPI_Target_Heap is
   use System.Storage_Elements;
   function Reserve (Bytes, Alignment : Interfaces.Unsigned_64) return Interfaces.Unsigned_64
     with Import, Convention => C, External_Name => "cualloc_allocate";
   procedure Free (Item : Interfaces.Unsigned_64)
     with Import, Convention => C, External_Name => "cualloc_free";
   function Allocate
     (Bytes : Storage_Count; Alignment : Storage_Count) return System.Address is
     (To_Address (Integer_Address (Reserve
       (Interfaces.Unsigned_64 (Bytes), Interfaces.Unsigned_64 (Alignment)))));
   procedure Release (Item : System.Address) is
   begin Free (Interfaces.Unsigned_64 (To_Integer (Item))); end Release;
end ACPI_Target_Heap;
