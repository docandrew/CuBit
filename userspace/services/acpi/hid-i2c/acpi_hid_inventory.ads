pragma Ada_2022;
with ACPI_Service;
with ACPI_HID_I2C_Resolved;
with ACPI_Sample_Cache;
generic
   with package Resolver is new ACPI_HID_I2C_Resolved (Core=>ACPI_Service,others=><>);
   Maximum_Devices : Positive;
-- Owned copied descriptions only: no endpoints, retained AML handles or grants.
-- Current identity is necessary but not sufficient for hardware readiness.
package ACPI_HID_Inventory with SPARK_Mode is
   subtype Device_Count is Natural range 0..Maximum_Devices;
   subtype Slot is Positive range 1..Maximum_Devices;
   type Entries is array(Slot) of Resolver.Observation;
   type Observation is record
      Count : Device_Count := 0;
      Items : Entries := [others=>Resolver.Empty_Observation];
   end record;
   Empty_Observation : constant Observation := (others=><>);
   package Samples is new ACPI_Sample_Cache (Observation,Empty_Observation);
end ACPI_HID_Inventory;
