pragma Ada_2022;
-- Paired frontend/backend literal firmware-data read-only profile.
-- Requires explicit launch approval and current-boot kernel range exclusions.
-- Writes remain disabled independently in both native assemblies.
package ACPI_Firmware_Data_Activation with Pure, SPARK_Mode is
   Enabled : constant Boolean := True;
end ACPI_Firmware_Data_Activation;
