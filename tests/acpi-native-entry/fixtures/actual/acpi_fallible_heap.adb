pragma Ada_2022;
with System.Storage_Elements;
package body ACPI_Fallible_Heap is
   function Allocate (Bytes, Alignment : Interfaces.Unsigned_64)
     return Interfaces.Unsigned_64
     with Import, Convention => C, External_Name => "cualloc_allocate";
   procedure Free (Address : Interfaces.Unsigned_64)
     with Import, Convention => C, External_Name => "cualloc_free";
   function Reserve (Bytes, Alignment : Interfaces.Unsigned_64)
     return System.Address is
     (System.Storage_Elements.To_Address
       (System.Storage_Elements.Integer_Address (Allocate (Bytes, Alignment))));
   procedure Release (Address : System.Address) is
   begin
      Free (Interfaces.Unsigned_64 (System.Storage_Elements.To_Integer (Address)));
   end Release;
end ACPI_Fallible_Heap;
