pragma Ada_2022;
with AML_Profile_Events;
-- Diagnostic proposal only. Default disabled; no native activation here.
package ACPI_Native_AML_Profile with SPARK_Mode => Off is
   Enabled : constant Boolean := False;
   procedure Start;
   procedure Observe (Item : AML_Profile_Events.Event);
   procedure Stop;
   procedure Report;
   -- Failure-only, at most eight copied dispatch observations, after Stop.
   -- Frame-relative syntax context, not a failing-location or provenance claim.
   procedure Report_Tail (IO_Pending, IO_Uncertain : Boolean);
end ACPI_Native_AML_Profile;
