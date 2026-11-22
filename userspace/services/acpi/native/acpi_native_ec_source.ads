pragma Ada_2022;
with ACPI_Requests;
with ACPI_EC_Types;
with ACPI_SCI_Events;
with ACPI_SCI_Registers;
with ACPI_SCI_Quiesce;
with ACPI_Native_Server;
-- One serialized local EC source owner. Configure after successful _INI and
-- runtime handoff; Arm only after SCI and all local Notify/fact consumers are
-- installed. This unit is not independently wired into Main until those gates
-- exist. No callback logs, IPC, dynamic session allocation or second Core.
package ACPI_Native_EC_Source is
   type Phase is (Fresh, Configured, Armed, Query_Pending, Awaiting_Notifications,
      Failed, Stopped);
   type Failure is (None, Admission_Refused, Selection_Refused, Enrollment_Refused,
      Window_Refused, Source_Refused, Query_Refused, Query_Uncertain,
      Handler_Refused, Retirement_Refused, Context_Changed, Fact_Retained);
   type Source_State is (Unarmed, Enabled, Masked, Unknown);
   type Report is record
      Stage : Phase := Fresh;
      Why : Failure := None;
      Source : Source_State := Unarmed;
      GPE : ACPI_SCI_Registers.GPE_Number := 0;
      Queries : Natural := 0;
      Delivery_Pending, Fact_Pending : Boolean := False;
   end record;
   function Observe return Report;
   -- Allocation-free configuration within a finite window on the SAME Core,
   -- provider, command sequence and runtime used by device initialization.
   procedure Configure (Server : in out ACPI_Requests.State; Accepted : out Boolean);
   -- Consumers_Ready is trusted local owner evidence, never an observer input.
   -- Does not clear a startup status: preserve a latched event for delivery.
   procedure Arm (Consumers_Ready : Boolean; Accepted : out Boolean);
   -- SCI callback: mask, acknowledge the current EC edge, then defer.
   -- No AML/EC command. ACPI 12.6 requires EC SCI edge-event semantics;
   -- rearm preserves any new edge latched while masked.
   procedure GPE_Event (GPE : ACPI_SCI_Registers.GPE_Number;
      Result : out ACPI_SCI_Events.Disposition);
   -- At most one actual query plus one _Qxx invocation per turn, with one
   -- original finite window and command budget. Zero is never an AML handler.
   -- Compose AFTER all-consumer notification routing and quiet fact draining.
   procedure Advance (Server : in out ACPI_Requests.State;
      Result : out ACPI_Native_Server.Owner_Outcome);
   type Query_Fact is record
      Number : ACPI_EC_Types.Byte := 0;
      Missing_Handler : Boolean := False;
   end record;
   -- One copied observation slot, taken only after Field/command quiescence.
   -- A full slot prevents another query, so unmapped hotkey numbers cannot be
   -- overwritten. Logger calls belong to the enclosing quiet owner.
   procedure Take_Query (Fact : out Query_Fact; Present : out Boolean);
   -- Stop admission and mask. Never resets/releases a failed command, delivery
   -- or root. Confirm_Disarm consumes the actual SCI owner's typed outcome.
   procedure Stop;
   -- After all receipts/facts are drained, retire only a known successful or
   -- missing-handler delivery. Any failed/uncertain record remains retained.
   procedure Close (Server : in out ACPI_Requests.State; Completed : out Boolean);
   procedure Confirm_Disarm (Result : ACPI_SCI_Quiesce.Result);
   function Quiet return Boolean;
end ACPI_Native_EC_Source;
