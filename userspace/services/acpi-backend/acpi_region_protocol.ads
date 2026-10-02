pragma Ada_2022;
with Interfaces;
-- Wire representation only: no namespace, interpreter, resource authority or
-- startup capability dependency. Labels are scoped to the backend endpoint.
package ACPI_Region_Protocol with SPARK_Mode, Pure is
   use Interfaces;
   type Words is array (Natural range 0 .. 3) of Unsigned_64;
   type Packet is record
      Label : Unsigned_32 := 0;
      Length : Unsigned_8 := 4;
      Flags : Unsigned_8 := 0;
      Reserved : Unsigned_16 := 0;
      Data : Words := [others => 0];
   end record;
end ACPI_Region_Protocol;
