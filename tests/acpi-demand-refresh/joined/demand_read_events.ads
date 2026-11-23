with ACPI_Requests;with ACPI_Native_Server;
generic
 with procedure Owner_Advance(Server:in out ACPI_Requests.State;Outcome:out ACPI_Native_Server.Owner_Outcome);
package Demand_Read_Events is
 procedure Run(Server:in out ACPI_Requests.State;Checks:out Natural);
end Demand_Read_Events;
