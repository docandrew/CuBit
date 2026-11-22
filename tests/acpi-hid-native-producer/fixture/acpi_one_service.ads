with Interfaces; with ACPI_Requests;
package ACPI_One_Service is
   package Runtime is
      package Status is
         type Outcome is (Event_Opened, Event_Completed, Rejected, IO_Pending);
      end Status;
   end Runtime;
   package Runtime_Access is
      procedure Begin_Event (Server : ACPI_Requests.State; Deadline_Microseconds : Interfaces.Unsigned_64;
         Result : out Runtime.Status.Outcome);
      procedure End_Event (Server : ACPI_Requests.State; Completed : Boolean; Result : out Runtime.Status.Outcome);
   end Runtime_Access;
end ACPI_One_Service;
