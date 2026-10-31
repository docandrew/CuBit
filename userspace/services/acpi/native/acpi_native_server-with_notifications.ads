with ACPI_Requests.Notification_Pump;
generic
   -- All local consumers must be installed before serving. This bounded local
   -- callback must route or invalidate every consumer,idempotently on retry.
   -- No AML/IPC/reset/reentry or Server mutation; return normally.
   with procedure Route_Local (Server : ACPI_Requests.State; Routed : out Boolean);
package ACPI_Native_Server.With_Notifications is
   package Pump is new ACPI_Requests.Notification_Pump (Route_Local);
   Claims_Per_Turn : constant Pump.Batch_Limit := 4;
   -- Compatible with With_Dispatch.Advance_Owner. Readiness/fanout/broker
   -- refusal stops the serving attempt while preserving the retained receipt.
   -- No automatic retry,source enrollment,hardware ACK or cache publication.
   procedure Advance (Server : in out ACPI_Requests.State; Result : out Owner_Outcome);
   -- An exception propagates to the serving owner. A report is current only
   -- when Observation_Complete is True; incomplete work never reports an old
   -- successful empty queue. No automatic retry or cleanup is performed.
   function Observation_Complete return Boolean;
   function Observation return Pump.Report;
private
   Last : Pump.Report;
   Completed : Boolean := False;
end ACPI_Native_Server.With_Notifications;
