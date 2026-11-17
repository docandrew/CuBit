pragma Ada_2022;
with Interfaces;
with ACPI_Requests; with ACPI_SCI_Events; with ACPI_SCI_Registers;
with ACPI_SCI_Quiesce; with ACPI_Native_Server;
generic
   -- Required trusted local ownership guard: handler/provider access cannot
   -- change this owner's GPE control state. Never a wire flag or permissive
   -- default. Hardware faithfulness and serialized access remain required.
   with function Control_Exclusive return Boolean;
   -- Bounded same-Core all-consumer fanout, no AML/peer/upward blocking call.
   -- Complete only after every original queued/staged receipt is acknowledged.
   with procedure Drain_Notifications (Server : in out ACPI_Requests.State; Complete : out Boolean);
package ACPI_Native_GPE_Source is
   type Phase is (Fresh, Live, Failed, Stopping, Stopped);
   type Failure is (None, Not_Ready, Control_Not_Exclusive, Inventory_Refused,
      Source_IO_Refused, Capture_Refused, Queue_Refused, Clock_Refused,
      Window_Refused, Handler_Refused, Routing_Refused, Boundary_Exception);
   type Source_State is (Unobserved, Known_Masked, Known_Enabled, Unknown);
   type Report is record
      Stage : Phase := Fresh;
      Why : Failure := None;
      Sources, Pending : Natural range 0 .. ACPI_SCI_Registers.GPE_Count := 0;
      Active, Result_Pending, Window_Open : Boolean := False;
      At_Source : ACPI_SCI_Registers.GPE_Number := 0;
      Deliveries : Interfaces.Unsigned_64 := 0;
   end record;
   function Observe return Report;
   function Control (Source : ACPI_SCI_Registers.GPE_Number) return Source_State;
   procedure Start (Server : ACPI_Requests.State; Consumers_Ready, EC_Present : Boolean;
      EC_GPE : ACPI_SCI_Registers.GPE_Number; Accepted : out Boolean);
   -- Callback: verify/mask an enrolled source and copy one receipt only.
   -- No AML, logger, method lookup, runtime opening or consumer fanout.
   procedure GPE_Event (Source : ACPI_SCI_Registers.GPE_Number;
      Result : out ACPI_SCI_Events.Disposition);
   function Runnable return Boolean;
   -- One source/handler per turn, original finite window, bounded fanout.
   -- Rearm follows successful handler/result cleanup/window close/Notify drain.
   procedure Advance (Server : in out ACPI_Requests.State;
      Result : out ACPI_Native_Server.Owner_Outcome);
   procedure Stop;
   procedure Confirm_Disarm (Result : ACPI_SCI_Quiesce.Result);
   procedure Close (Server : in out ACPI_Requests.State; Complete : out Boolean);
end ACPI_Native_GPE_Source;
