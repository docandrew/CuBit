pragma Ada_2022;
with Interfaces; with S5_Boundary; with ACPI_Local_Runtime_Status; with ACPI_Sleep_Entry;
package ACPI_One_Service is
   package Runtime is
      package Status renames ACPI_Local_Runtime_Status;
      function Current return Status.Phase is (if S5_Boundary.Runtime_Fresh then Status.Fresh
         elsif S5_Boundary.Runtime_Stopped then Status.Stopped
         elsif S5_Boundary.Runtime_Idle then Status.Idle else Status.Active);
   end Runtime;
   package Root is
      package Hardware is
         function Admitting return Boolean is (S5_Boundary.Hardware_Ready);
         function Sleep_Register_Model return ACPI_Sleep_Entry.Register_Model is (S5_Boundary.Model);
         procedure Sleep_Action (Command : ACPI_Sleep_Entry.Command; Deadline_Microseconds : Interfaces.Unsigned_64;
            Output : out ACPI_Sleep_Entry.Control_Word; Result : out ACPI_Sleep_Entry.Completion);
      end Hardware;
      package Fields is
         function Control_Idle return Boolean is (S5_Boundary.Fields_Quiet);
         package EC_Commands is
            function Quiescent return Boolean is (S5_Boundary.EC_Quiet);
         end EC_Commands;
      end Fields;
   end Root;
end ACPI_One_Service;
