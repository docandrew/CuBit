pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_Device_Policy;
generic
   with package Core is new ACPI_Service_Core (<>);
package ACPI_Unprovisioned_Regions with SPARK_Mode is
   -- Shared default policy when no hardware handlers have been installed.
   -- Only an established empty hardware requirement can pass this barrier.
   procedure Initialize
     (Service : aliased in out Core.State; Budget : Natural;
      Charged : out Natural; Outcome : out ACPI_Device_Policy.Region_Outcome);
end ACPI_Unprovisioned_Regions;
