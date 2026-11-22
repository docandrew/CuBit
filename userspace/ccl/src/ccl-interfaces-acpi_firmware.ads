-- Proposed declared firmware compatibility profile. No default activation.
package CCL.Interfaces.ACPI_Firmware with SPARK_Mode, Pure is
   TYPE_SOURCE : constant String :=
     "(type ACPI_Profile (enum Neutral Known_Windows)) " &
     "(type ACPI_Firmware_Profile (record (kind ACPI_Profile)))";
   ROOT_TYPE_NAME : constant String := "ACPI_Firmware_Profile";
end CCL.Interfaces.ACPI_Firmware;
