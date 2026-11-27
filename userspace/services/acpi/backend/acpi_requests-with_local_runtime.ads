pragma Ada_2022;
with Interfaces;
with ACPI_Service;
with ACPI_Local_Runtime_Status;
generic
   with procedure Enter_Owner (Service : ACPI_Service.State; Result : out ACPI_Local_Runtime_Status.Outcome);
   with procedure Begin_Owner (Service : ACPI_Service.State; Deadline_Microseconds : Interfaces.Unsigned_64;
      Result : out ACPI_Local_Runtime_Status.Outcome);
   with procedure End_Owner (Service : ACPI_Service.State; Completed : Boolean;
      Result : out ACPI_Local_Runtime_Status.Outcome);
   type Region_Report is private;
   with procedure Drain_Owner
     (Service : aliased in out ACPI_Service.State; Turn_ID : Positive;
      Report : out Region_Report);
-- Trusted process-local bridge. It selects the owner's private Core, never a
-- caller-supplied pointer, context key or wire-selected method. Bind exactly
-- the local runtime child belonging to that same startup provider instance.
package ACPI_Requests.With_Local_Runtime is
   procedure Enter_Runtime (Server : State; Result : out ACPI_Local_Runtime_Status.Outcome)
     with Pre => Valid (Server);
   procedure Begin_Event (Server : State; Deadline_Microseconds : Interfaces.Unsigned_64;
      Result : out ACPI_Local_Runtime_Status.Outcome) with Pre => Valid (Server);
   -- End remains reachable without a second readiness check: the runtime
   -- authenticates the original Core and decides whether to stop. Completed
   -- covers evaluation/result cleanup only, not Notify ACK or source retirement.
   procedure End_Event (Server : State; Completed : Boolean;
      Result : out ACPI_Local_Runtime_Status.Outcome) with Pre => Valid (Server);
   procedure Drain_Regions
     (Server : in out State; Turn_ID : Positive; Report : out Region_Report)
     with Pre => Valid (Server) and then Device_Walk_Finished (Server);
end ACPI_Requests.With_Local_Runtime;
