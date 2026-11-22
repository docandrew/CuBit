pragma Ada_2022;
package body ACPI_Bootstrap.With_Local_Runtime is
   procedure Enter_Runtime (Session : State; Result : out ACPI_Local_Runtime_Status.Outcome) is
   begin
      Result := ACPI_Local_Runtime_Status.Rejected;
      if not Device_Walk_Finished (Session) then return; end if;
      Enter_Owner (Session.Core,Result);
   end Enter_Runtime;
   procedure Begin_Event (Session : State; Deadline_Microseconds : Interfaces.Unsigned_64;
      Result : out ACPI_Local_Runtime_Status.Outcome) is
   begin
      Result := ACPI_Local_Runtime_Status.Rejected;
      if not Device_Walk_Finished (Session) then return; end if;
      Begin_Owner (Session.Core,Deadline_Microseconds,Result);
   end Begin_Event;
   procedure End_Event (Session : State; Completed : Boolean; Result : out ACPI_Local_Runtime_Status.Outcome) is
   begin
      End_Owner (Session.Core,Completed,Result);
   end End_Event;
end ACPI_Bootstrap.With_Local_Runtime;
