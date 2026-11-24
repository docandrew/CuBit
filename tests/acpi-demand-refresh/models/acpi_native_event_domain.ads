with Interfaces;with ACPI_Requests;
package ACPI_Native_Event_Domain is
 procedure Preview_Battery_Refresh(Server:ACPI_Requests.State;Now:out Interfaces.Unsigned_64;Allowed:out Boolean);
 procedure Commit_Battery_Refresh(Server:ACPI_Requests.State;Now:Interfaces.Unsigned_64;Ticket:out Interfaces.Unsigned_64;Accepted:out Boolean);
 function Battery_Refresh_Ready(Ticket:Interfaces.Unsigned_64) return Boolean;
end ACPI_Native_Event_Domain;
