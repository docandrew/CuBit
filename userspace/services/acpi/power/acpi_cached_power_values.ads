pragma Ada_2022;
with Interfaces;
with ACPI_Observation_Values;
-- Copied descriptive cache data only; no Core aliases, handles or authority.
package ACPI_Cached_Power_Values with Pure, SPARK_Mode is
   type Generation is new Interfaces.Unsigned_64 range
     0 .. Interfaces.Unsigned_64 (Interfaces.Integer_64'Last);
   type Microseconds is new Interfaces.Unsigned_64;
   type Availability is (Not_Ready, Available, Missing, Failure, Too_Old,
      Clock_Unknown, Clock_Regressed, Counter_Exhausted);
   type Sample_Time is record
      Known : Boolean := False;
      Value : Microseconds := 0;
   end record;
   type Lid_View is record
      Revision : Generation := 0;
      Status : Availability := Not_Ready;
      Time : Sample_Time;
      Value : ACPI_Observation_Values.Lid_Sample;
   end record;
   type Source_View is record
      Revision : Generation := 0;
      Status : Availability := Not_Ready;
      Time : Sample_Time;
      Value : ACPI_Observation_Values.Power_Source_Sample;
   end record;
   -- The two observations are historical ordered samples, not an atomic
   -- physical snapshot. They are read against one trusted clock observation.
   type Snapshot is record
      Lid : Lid_View;
      Source : Source_View;
   end record;
end ACPI_Cached_Power_Values;
