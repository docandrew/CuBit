pragma Ada_2022;
with Interfaces;
package ACPI_Clock_Source is
   procedure Read (Value : out Interfaces.Unsigned_64; Available : out Boolean);
end ACPI_Clock_Source;
