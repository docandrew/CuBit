pragma Ada_2022;
generic
   with procedure Advance_Entry (Server : ACPI_Requests.State; Did_Work : out Boolean);
   with function Entry_Runnable return Boolean;
   with function Blocks_Runtime return Boolean;
   with function Entry_Deadline_Milliseconds return Interfaces.Unsigned_64;
   with procedure Advance_Normal (Server : in out ACPI_Requests.State; Result : out Owner_Outcome);
   with procedure Maintain_Normal (Deadline_Milliseconds : out Interfaces.Unsigned_64; May_Continue : out Boolean);
   with function Diagnostics_Normal (Server : ACPI_Requests.State) return Boolean;
-- One serialized entry step, then normal work only before final-entry attempt.
-- All deadlines are already in the IPC wait-clock domain, never lease ticks.
package ACPI_Native_Server.With_Power_Entry is
   procedure Advance (Server : in out ACPI_Requests.State; Result : out Owner_Outcome);
   procedure Maintain (Wake_Deadline_Milliseconds : out Interfaces.Unsigned_64; May_Continue : out Boolean);
   function Diagnostics_Quiet (Server : ACPI_Requests.State) return Boolean;
end ACPI_Native_Server.With_Power_Entry;
