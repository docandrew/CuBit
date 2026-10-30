with Owner_Test_Control;
package body ACPI_Native_Server is
   procedure Run (Server : in out ACPI_Requests.State; Reason : out Stop_Reason) is
      package T renames Owner_Test_Control;
      use type ACPI_Requests.Phase;
   begin
      if ACPI_Requests.Current (Server) /= ACPI_Requests.Complete then
         raise Program_Error with "partial owner served";
      end if;
      T.Serves := T.Serves + 1;
      T.Sequence := T.Sequence + 1; T.Served_At := T.Sequence;
      Reason := Wait_Unavailable;
   end Run;
   package body With_Dispatch is
      pragma Unreferenced (Dispatch_Message);
      procedure Serve (Server : in out ACPI_Requests.State; Reason : out Stop_Reason) is
      begin Run (Server, Reason); end Serve;
   end With_Dispatch;
end ACPI_Native_Server;
