-- Immutable launch schema; CMOS fields are overwritten from the boot receipt.
package CCL.Interfaces.ACPI_Firmware with SPARK_Mode, Pure is
   TYPE_SOURCE : constant String :=
     "(type ACPI_Profile (enum Neutral Known_Windows))" &
     "(type CMOS_Profile (enum No_CMOS PC_AT_64 Intel_128 Intel_256))" &
     "(type ACPI_Firmware_Profile (record (kind ACPI_Profile)" &
     "(cmos_profile CMOS_Profile CMOS_Profile.No_CMOS)" &
     "(cmos_known Boolean false) (cmos_nmi_disabled Boolean false)))";
   ROOT_TYPE_NAME : constant String := "ACPI_Firmware_Profile";
end CCL.Interfaces.ACPI_Firmware;
