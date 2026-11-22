with ACPI_Requests;with Interfaces;
package ACPI_Native_Passive_Queries is
 procedure Advance(Server:in out ACPI_Requests.State;Worked:out Boolean);
 procedure Maintain(Deadline:out Interfaces.Unsigned_64;Healthy:out Boolean);
end ACPI_Native_Passive_Queries;
