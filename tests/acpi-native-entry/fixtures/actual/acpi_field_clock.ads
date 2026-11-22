pragma Ada_2022;
with Interfaces;
package ACPI_Field_Clock with Pure, SPARK_Mode is
   use type Interfaces.Unsigned_64;
   -- Default for hosted/unprovisioned owners. Never invent a usable epoch.
   procedure Unavailable (Now : out Interfaces.Unsigned_64; Available : out Boolean)
     with Global => null, Post => Now = 0 and then not Available;
end ACPI_Field_Clock;
