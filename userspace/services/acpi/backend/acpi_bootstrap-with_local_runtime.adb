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
   procedure Drain_Regions
     (Session : in out State; Turn_ID : Positive; Report : out Region_Report) is
   begin
      Drain_Owner (Session.Core, Turn_ID, Report);
   end Drain_Regions;
end ACPI_Bootstrap.With_Local_Runtime;
