pragma Ada_2022;
with Interfaces;
generic
   with procedure Advance_Normal(Server:in out ACPI_Requests.State;Result:out Owner_Outcome) is Idle_Owner;
   with procedure Maintain_Normal(Deadline:out Interfaces.Unsigned_64;May_Continue:out Boolean) is No_Maintenance;
package ACPI_Native_Server.With_Passive_Queries is
   -- One serialized cleanup step before the existing owner. Only confirmed
   -- cleanup counts as work; an uncertain return waits for its retry deadline.
   procedure Advance(Server:in out ACPI_Requests.State;Result:out Owner_Outcome);
   procedure Maintain(Deadline:out Interfaces.Unsigned_64;May_Continue:out Boolean);
end ACPI_Native_Server.With_Passive_Queries;
