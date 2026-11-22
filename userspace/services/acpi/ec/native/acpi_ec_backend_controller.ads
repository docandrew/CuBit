pragma Ada_2022;
with Interfaces;
with ACPI_EC_Event_Types;
with ACPI_EC_Types;
with ACPI_EC_Registers;
with ACPI_EC_Native_Calls;
with ACPI_EC_Event_Publisher;
with ACPI_EC_Backend_Progress;
with ACPI_EC_Backend_Waits;
with CuBit.Messages;
-- Trusted serialized backend owner only. Sole entry point for submitting and
-- advancing work in these actual package instances. No hardware authority,
-- receive loop, interrupt admission or recovery/reset mechanism is supplied.
-- Read_Clock must be bounded/nonreentrant and share the engine clock epoch.
-- All actual packages must be the same serialized controller instances.
generic
   with package Registers is new ACPI_EC_Registers (<>);
   with package Calls is new ACPI_EC_Native_Calls (Registers => Registers, others => <>);
   with package Events is new ACPI_EC_Event_Publisher (Registers => Registers, others => <>);
   with package Progress is new ACPI_EC_Backend_Progress
     (Registers => Registers, Calls => Calls, Events => Events);
   with package Waits is new ACPI_EC_Backend_Waits
     (Registers => Registers, Progress => Progress, others => <>);
   with procedure Read_Clock
     (Value : out ACPI_EC_Types.Microseconds; Available : out Boolean);
   Request_Window : ACPI_EC_Types.Microseconds;
   Request_Budget : ACPI_EC_Types.Work_Budget;
   Pause_Budget : ACPI_EC_Types.Work_Budget;
   with function Answer_Current
     (Sender : CuBit.Messages.Process_ID; Msg : CuBit.Messages.Message)
      return Interfaces.Unsigned_64 is CuBit.Messages.reply;
package ACPI_EC_Backend_Controller is
   type Admission_Status is (Admitted, Refused, Clock_Unavailable,
                            Deadline_Overflow, Invalid_Policy);
   type Call_Report is record
      Status : Admission_Status := Refused;
      Replied : Boolean := False;
      Field : Calls.Report;
   end record;
   -- Sender/request are the exact authenticated kernel receive. Failed policy
   -- or clock admission denies the current request without saving a reply or
   -- submitting a transaction. Windows are backend policy, never wire input.
   procedure Begin_Call
     (Sender : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
      Result : out Call_Report);
   -- Only independently admitted EC event sources may call this entry point.
   procedure Start_Query
     (Delivery_Attempts : ACPI_EC_Event_Types.Delivery_Budget;
      Status : out Admission_Status; Result : out Registers.Submission);
   -- At most one bounded wait plus one owning adapter step. On a failed outer
   -- clock sample, request stop and give the existing owner one engine check;
   -- no fabricated clock sample is passed to the hardware callbacks.
   procedure Advance (Result : out Waits.Report; Clock_Available : out Boolean);
   function Remaining_Pauses return ACPI_EC_Types.Work_Budget;
   -- Reset the pause budget only after a new Field/query transaction starts.
   -- Busy/refused requests, publication retries and failed clock samples never
   -- replenish it. Completion/delivery never release recovery obligations.
end ACPI_EC_Backend_Controller;
