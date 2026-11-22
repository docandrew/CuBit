pragma Ada_2022;
package body ACPI_Field_Clock with SPARK_Mode is
   procedure Unavailable (Now : out Interfaces.Unsigned_64; Available : out Boolean) is
   begin Now := 0; Available := False; end Unavailable;
end ACPI_Field_Clock;
