pragma Ada_2022;
with ACPI_Local_Shutdown;
generic
   with procedure Close_Consumers (Accepted : out Boolean);
   with procedure Other_Owners_View (Quiescent : out Boolean;
      Wake_Deadline_Milliseconds : out Word);
package ACPI_Local_Memory.Modules.Shutdown is
   package Control is new ACPI_Local_Shutdown
     (Close_Consumers,Close_Local,Hardware.Begin_Stop,Hardware.Maintain,
      Fields.Cleanup,Hardware.May_Exit,Other_Owners_View);
   function Observe return Control.Report;
   function May_Retire return Boolean;
   procedure Request_Stop (Accepted : out Boolean);
   procedure Advance;
   -- Root keeps processing only stop/retirement/cleanup work between these
   -- calls. It must not resume normal dispatch or free Core/owner state until
   -- May_Retire, and must retain stopped backing to its approved disposal step.
private
   State : Control.Context;
end ACPI_Local_Memory.Modules.Shutdown;
