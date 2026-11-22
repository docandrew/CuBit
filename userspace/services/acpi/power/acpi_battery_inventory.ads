pragma Ada_2022;
with ACPI_Service;
with ACPI_Battery_Observation;
with ACPI_Sample_Cache;
generic
   with package Collector is new ACPI_Battery_Observation (Core=>ACPI_Service,others=><>);
   Maximum_Batteries : Positive;
-- Owned bounded inventory values. Device descriptions are copied metadata,
-- never invocation/hardware authority or a borrowed Core/result handle.
package ACPI_Battery_Inventory with SPARK_Mode is
   subtype Battery_Count is Natural range 0..Maximum_Batteries;
   subtype Slot is Positive range 1..Maximum_Batteries;
   type Availability is (Sampled,Known_Absent,Acquisition_Failed);
   type Entry_Value is record
      Device : ACPI_Service.Node_Descriptor:=ACPI_Service.Values.No_Node;
      Status : Availability:=Acquisition_Failed;
      Reason : Collector.Outcome:=Collector.Identification_Failed;
      Value : Collector.Observation:=Collector.Empty_Observation;
   end record;
   type Entries is array(Slot) of Entry_Value;
   type Observation is record Count:Battery_Count:=0;Items:Entries;end record;
   Empty_Observation : constant Observation:=(others=><>);
   package Samples is new ACPI_Sample_Cache (Observation,Empty_Observation);
end ACPI_Battery_Inventory;
