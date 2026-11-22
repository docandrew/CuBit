pragma Ada_2022;
with Interfaces;
-- Fixed read-only pages on ACPI's existing observer Read_Metrics operation.
-- Request [page,max-age-microseconds,subpage,expected-cache-generation].
-- Summary [generation,availability,state,time-known].
-- Time [generation,time-known,time-low32,time-high32].
-- Raw [generation,current-availability,raw-low32,raw-high32].
-- Auxiliary reads require the summary generation; changed generation is Stale.
-- Generation belongs to one owner/session, not physical identity or authority.
package ACPI_Power_Observation_Protocol with Pure, SPARK_Mode is
   Read_Metrics_Label : constant Interfaces.Unsigned_32 := 0;
   Lid_Page : constant Interfaces.Unsigned_64 := 20;
   Source_Page : constant Interfaces.Unsigned_64 := 21;
   Summary_Page : constant Interfaces.Unsigned_64 := 0;
   Time_Page : constant Interfaces.Unsigned_64 := 1;
   Raw_Page : constant Interfaces.Unsigned_64 := 2;
end ACPI_Power_Observation_Protocol;
