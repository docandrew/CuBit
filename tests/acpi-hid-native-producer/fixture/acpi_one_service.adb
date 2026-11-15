with Producer_Model; with Sender_Model;
package body ACPI_One_Service is
   package M renames Producer_Model;
   use type Interfaces.Unsigned_64;
   package body Runtime_Access is
      procedure Begin_Event (Server : ACPI_Requests.State; Deadline_Microseconds : Interfaces.Unsigned_64;
         Result : out Runtime.Status.Outcome) is
         pragma Unreferenced (Server);
      begin
         M.Opens := M.Opens + 1;
         M.Check (not M.Window_Open and then Sender_Model.Calls = 0, "one initial runtime window before IPC");
         M.Check (Deadline_Microseconds = M.Original_US, "original microsecond deadline preserved");
         if M.Mode in 10 | 24 then Result := Runtime.Status.Rejected;
         else M.Window_Open := True; Result := Runtime.Status.Event_Opened; end if;
      end Begin_Event;
      procedure End_Event (Server : ACPI_Requests.State; Completed : Boolean; Result : out Runtime.Status.Outcome) is
         pragma Unreferenced (Server);
      begin
         M.Ends := M.Ends + 1;
         M.Check (M.Window_Open and then Sender_Model.Calls = 0, "all collection closes before first IPC");
         M.Check (Completed = (M.Mode /= 12), "recovery never reported as complete");
         if M.Mode in 12 | 13 then Result := Runtime.Status.IO_Pending;
         else M.Window_Open := False; Result := Runtime.Status.Event_Completed; end if;
         if M.Mode = 19 then M.Boundary := False; end if;
      end End_Event;
   end Runtime_Access;
end ACPI_One_Service;
