pragma Ada_2022;
with S5_Boundary;
package body ACPI_One_Service.Power_Preparation is
   procedure Read_Clock (Now_Microseconds : out Interfaces.Unsigned_64; Available : out Boolean) is
   begin Now_Microseconds := S5_Boundary.Microseconds; Available := S5_Boundary.Clock_Known; end Read_Clock;
   function May_Continue (Service : ACPI_Service.State) return Boolean is
      pragma Unreferenced (Service);
   begin return False; end May_Continue;
   procedure Begin_Event (Service : ACPI_Service.State; Deadline_Microseconds : Interfaces.Unsigned_64;
      Result : out ACPI_Local_Runtime_Status.Outcome) is
      pragma Unreferenced (Service, Deadline_Microseconds);
   begin S5_Boundary.Windows := S5_Boundary.Windows + 1; Result := ACPI_Local_Runtime_Status.Owner_Refused; end Begin_Event;
   procedure End_Event (Service : ACPI_Service.State; Completed : Boolean;
      Result : out ACPI_Local_Runtime_Status.Outcome) is
      pragma Unreferenced (Service, Completed);
   begin S5_Boundary.Windows := S5_Boundary.Windows + 1; Result := ACPI_Local_Runtime_Status.Owner_Refused; end End_Event;
end ACPI_One_Service.Power_Preparation;
