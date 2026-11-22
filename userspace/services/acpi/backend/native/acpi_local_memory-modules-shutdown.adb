pragma Ada_2022;
package body ACPI_Local_Memory.Modules.Shutdown is
   function Observe return Control.Report is (Control.Observe (State));
   function May_Retire return Boolean is (Control.May_Retire (State));
   procedure Request_Stop (Accepted : out Boolean) is
   begin Control.Request_Stop (State,Accepted); end Request_Stop;
   procedure Advance is
   begin Control.Advance (State); end Advance;
end ACPI_Local_Memory.Modules.Shutdown;
