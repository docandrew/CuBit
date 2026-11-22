pragma Ada_2022;
with System;
with System.Storage_Elements;
package ACPI_Target_Heap is
   -- Null means allocation was refused; no ownership was acquired.
   function Allocate
     (Bytes : System.Storage_Elements.Storage_Count;
      Alignment : System.Storage_Elements.Storage_Count) return System.Address;
   procedure Release (Item : System.Address);
end ACPI_Target_Heap;
