pragma Ada_2022;
-- Paired frontend/backend experimental gate. Keep disabled until current-boot
-- memory/device exclusions and native tests are validated by the coordinator.
package ACPI_Firmware_Data_Activation with Pure, SPARK_Mode is
   Enabled : constant Boolean := False;
end ACPI_Firmware_Data_Activation;
