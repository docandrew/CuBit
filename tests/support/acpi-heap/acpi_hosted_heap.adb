pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements;
package body ACPI_Hosted_Heap is
   function Aligned_Allocate (Alignment, Bytes : Unsigned_64) return System.Address
     with Import, Convention => C, External_Name => "aligned_alloc";
   procedure Release (Item : System.Address)
     with Import, Convention => C, External_Name => "free";
   function Allocate (Bytes, Alignment : Unsigned_64) return Unsigned_64
     with Export, Convention => C, External_Name => "cualloc_allocate";
   procedure Free (Item : Unsigned_64)
     with Export, Convention => C, External_Name => "cualloc_free";
   function Allocate (Bytes, Alignment : Unsigned_64) return Unsigned_64 is
      Minimum_Alignment : constant := 16;
   begin
      if Alignment < Minimum_Alignment or else
        (Alignment and (Alignment - 1)) /= 0 or else
        Bytes > Unsigned_64'Last - (Alignment - 1)
      then return 0; end if;
      return Unsigned_64 (System.Storage_Elements.To_Integer
        (Aligned_Allocate (Alignment, ((Bytes + Alignment - 1) / Alignment) * Alignment)));
   end Allocate;
   procedure Free (Item : Unsigned_64) is
   begin
      Release (System.Storage_Elements.To_Address (System.Storage_Elements.Integer_Address (Item)));
   end Free;
end ACPI_Hosted_Heap;
