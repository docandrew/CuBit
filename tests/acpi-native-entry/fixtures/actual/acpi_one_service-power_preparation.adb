pragma Ada_2022;
with ACPI_Backend_Kernel_Handles; with ACPI_Backend_Binding_Types;
package body ACPI_One_Service.Power_Preparation is
   use type Runtime.Status.Phase;
   procedure Read_Clock (Now_Microseconds : out Interfaces.Unsigned_64; Available : out Boolean) is
      Value : ACPI_Backend_Binding_Types.Tick;
   begin
      ACPI_Backend_Kernel_Handles.Read_Clock (Value, Available);
      Now_Microseconds := Interfaces.Unsigned_64 (Value);
   end Read_Clock;
   function May_Continue (Service : ACPI_Service.State) return Boolean is
     (ACPI_Service.Valid (Service) and then Runtime.Current = Runtime.Status.Active
      and then Root.Hardware.Admitting and then Root.Fields.Control_Idle
      and then Root.Fields.EC_Commands.Quiescent);
end ACPI_One_Service.Power_Preparation;
