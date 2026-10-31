with Delay_Owner_Model;
package body ACPI_Native_Delays is
   package M renames Delay_Owner_Model;
   procedure Begin_Startup (Deadline : Interfaces.Unsigned_64; Accepted : out Boolean) is
      pragma Unreferenced (Deadline);
   begin
      Accepted := M.Arms = 0 and M.Closes = 0;
      if Accepted then M.Arms := M.Arms + 1; M.Active := True; end if;
   end Begin_Startup;
   procedure Close_Startup is
   begin M.Active := False; M.Closes := M.Closes + 1; end Close_Startup;
   procedure Perform (Item : AML_Delays.Request; Result : out AML_Delays.Outcome) is
      pragma Unreferenced (Item);
   begin Result := AML_Delays.Unavailable; end Perform;
end ACPI_Native_Delays;
