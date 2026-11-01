pragma Ada_2022;
with Interfaces;
with ACPI_Region_Protocol;
with ACPI_Timer_Probe_Types;
generic
   Maximum_Registers : Positive;
   with procedure Load
     (Deadline : ACPI_Timer_Probe_Types.Time_Instant;
      Complete : out Boolean; Count : out Natural;
      Items : out ACPI_Timer_Probe_Types.Descriptions);
   with procedure Read_Clock
     (Now : out ACPI_Timer_Probe_Types.Time_Instant; Available : out Boolean);
   with procedure Read
     (Deadline : ACPI_Timer_Probe_Types.Time_Instant;
      Item : ACPI_Region_Protocol.Register_Request;
      Status : out ACPI_Timer_Probe_Types.Read_Status;
      Value : out Interfaces.Unsigned_64);
package ACPI_Timer_Probe is
   type Context is limited private;
   -- One attempt, one absolute millisecond deadline, no failed-read replay.
   -- Metadata never supplies an address to Read; only the named ID/epoch do.
   procedure Run
     (State : in out Context;
      Deadline : ACPI_Timer_Probe_Types.Time_Instant;
      Result : out ACPI_Timer_Probe_Types.Report);
private
   type Context is limited record
      Started : Boolean := False;
      Saved : ACPI_Timer_Probe_Types.Report;
   end record;
end ACPI_Timer_Probe;
