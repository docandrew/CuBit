pragma Ada_2022;
with Interfaces;
with System;
-- Native CuAlloc adapter: no Ada allocator, exception translation or fixed quota.
package ACPI_Fallible_Heap with SPARK_Mode => Off is
   function Reserve (Bytes, Alignment : Interfaces.Unsigned_64)
     return System.Address;
   procedure Release (Address : System.Address);
end ACPI_Fallible_Heap;
