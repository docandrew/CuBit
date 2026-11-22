pragma Ada_2022;
with Interfaces;
with ACPI_Cached_Power_Values;
with ACPI_Battery_Status;
-- Owned descriptive observations only: no Core descriptors, result handles,
-- firmware callbacks or hardware authority. Slots belong to this generation.
package ACPI_Cached_Battery_Values with Pure, SPARK_Mode is
   Maximum_Batteries : constant Positive := 16;
   subtype Battery_Count is Natural range 0 .. Maximum_Batteries;
   subtype Slot is Positive range 1 .. Maximum_Batteries;
   type Entry_Status is (Sampled, Known_Absent, Acquisition_Failed);
   type Acquisition_Reason is (No_Failure, Not_Battery, Absent, Not_Ready,
      Identification_Failed, Presence_Failed, Information_Failed, Status_Failed,
      Presence_Changed, Budget_Exhausted, Cleanup_Failed, Provider_Contract_Error);
   type Information_Source is (None_Reported, Extended_Reported, Legacy_Reported);
   type Unit_Kind is (Unspecified_Unit, Energy_And_Power, Charge_And_Current);
   type Quantity is record
      Known : Boolean := False;
      Value : Interfaces.Unsigned_32 := 0;
   end record;
   type Sample is record
      Source : Information_Source := None_Reported;
      Units : Unit_Kind := Unspecified_Unit;
      Design_Capacity, Last_Full_Capacity, Design_Voltage : Quantity;
      Dynamic : ACPI_Battery_Status.Sample := ACPI_Battery_Status.Empty_Sample;
   end record;
   Empty_Sample : constant Sample := (others => <>);
   type Entry_Value is record
      Status : Entry_Status := Acquisition_Failed;
      Reason : Acquisition_Reason := Identification_Failed;
      Value : Sample := Empty_Sample;
   end record;
   Empty_Entry : constant Entry_Value := (others => <>);
   type Entries is array (Slot) of Entry_Value;
   Empty_Entries : constant Entries := [others => (others => <>)];
   type View is record
      Revision : ACPI_Cached_Power_Values.Generation := 0;
      Status : ACPI_Cached_Power_Values.Availability := ACPI_Cached_Power_Values.Not_Ready;
      Time : ACPI_Cached_Power_Values.Sample_Time;
      Count : Battery_Count := 0;
      Items : Entries := Empty_Entries;
   end record;
end ACPI_Cached_Battery_Values;
