with Owner_Test_Control;
package body ACPI_Native_Server is
   procedure Idle_Owner (Server : in out ACPI_Requests.State; Result : out Owner_Outcome) is
      pragma Unreferenced (Server);
   begin Result := Owner_Idle; end Idle_Owner;
   procedure No_Maintenance (Wake_Deadline_Milliseconds : out Interfaces.Unsigned_64; May_Continue : out Boolean) is
   begin Wake_Deadline_Milliseconds := CuBit.Messages.Wait_Forever;May_Continue := True;end No_Maintenance;
   function Always_Quiet (Server:ACPI_Requests.State) return Boolean is
      pragma Unreferenced(Server);
   begin return True;end Always_Quiet;
   function Reply_Immediately return Boolean is (True);
   procedure Run (Server : in out ACPI_Requests.State; Reason : out Stop_Reason) is
      package T renames Owner_Test_Control;
      use type ACPI_Requests.Phase;
      use type T.Scenario;
   begin
      if ACPI_Requests.Current (Server) /= ACPI_Requests.Complete then
         raise Program_Error with "partial owner served";
      end if;
      T.Serves := T.Serves + 1;
      T.Sequence := T.Sequence + 1; T.Served_At := T.Sequence;
      Reason := (if T.Selected = T.Stop_Request then Stop_Requested else Wait_Unavailable);
   end Run;
   package body With_Dispatch is
      pragma Unreferenced (Dispatch_Message, Handle_Event, Advance_Owner, Maintain_Owner, Diagnostics_Quiet, Reply_Now);
      procedure Serve (Server : in out ACPI_Requests.State; Reason : out Stop_Reason) is
      begin Run (Server, Reason); end Serve;
   end With_Dispatch;
end ACPI_Native_Server;
