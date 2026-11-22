pragma Ada_2022;
with Interfaces;
-- Stable protocol names. These are not handles, addresses or window offsets.
package ACPI_Backend_Register_IDs with Pure, SPARK_Mode is
   PM_Timer : constant Interfaces.Unsigned_64 := 16#4143_5049_0000_0001#;
end ACPI_Backend_Register_IDs;
