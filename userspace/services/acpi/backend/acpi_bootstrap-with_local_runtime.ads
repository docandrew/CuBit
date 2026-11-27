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
package ACPI_Bootstrap.With_Local_Runtime is
   procedure Enter_Runtime (Session : State; Result : out ACPI_Local_Runtime_Status.Outcome)
     with Pre => Valid (Session);
   procedure Begin_Event (Session : State; Deadline_Microseconds : Interfaces.Unsigned_64;
      Result : out ACPI_Local_Runtime_Status.Outcome) with Pre => Valid (Session);
   -- End remains reachable without a second readiness check: the runtime
   -- authenticates the original Core and decides whether to stop. Completed
   -- covers evaluation/result cleanup only, not Notify ACK or source retirement.
   procedure End_Event (Session : State; Completed : Boolean;
      Result : out ACPI_Local_Runtime_Status.Outcome) with Pre => Valid (Session);
   --  Same private Core and serialized owner as runtime evaluation.
   procedure Drain_Regions
     (Session : in out State; Turn_ID : Positive; Report : out Region_Report)
     with Pre => Valid (Session) and then Device_Walk_Finished (Session);
end ACPI_Bootstrap.With_Local_Runtime;
