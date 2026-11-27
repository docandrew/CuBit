pragma Ada_2022;
with ACPI_Bootstrap.With_Local_Runtime;
package body ACPI_Requests.With_Local_Runtime is
   package Owner is new ACPI_Bootstrap.With_Local_Runtime
     (Enter_Owner, Begin_Owner, End_Owner, Region_Report, Drain_Owner);
   procedure Enter_Runtime (Server : State; Result : out ACPI_Local_Runtime_Status.Outcome) is
   begin Owner.Enter_Runtime (Server.Boot,Result); end Enter_Runtime;
   procedure Begin_Event (Server : State; Deadline_Microseconds : Interfaces.Unsigned_64;
      Result : out ACPI_Local_Runtime_Status.Outcome) is
   begin Owner.Begin_Event (Server.Boot,Deadline_Microseconds,Result); end Begin_Event;
   procedure End_Event (Server : State; Completed : Boolean; Result : out ACPI_Local_Runtime_Status.Outcome) is
   begin Owner.End_Event (Server.Boot,Completed,Result); end End_Event;
   procedure Drain_Regions
     (Server : in out State; Turn_ID : Positive; Report : out Region_Report) is
   begin
      Owner.Drain_Regions (Server.Boot, Turn_ID, Report);
   end Drain_Regions;
end ACPI_Requests.With_Local_Runtime;
