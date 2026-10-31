with ACPI_Service;
with ACPI_Device_Policy;
package body ACPI_Startup_Diagnostics is
   use type ACPI_Service.Execution_Phase_Result;
   use type ACPI_Bootstrap.Device_Phase;
   function Format (Item : ACPI_Bootstrap.Finish_Report) return String is
   begin
      case Item.Stage is
         when ACPI_Bootstrap.Not_Attempted =>
            return "acpi: stage aml-startup fail NOT_ATTEMPTED";
         when ACPI_Bootstrap.Driver_Reused =>
            return "acpi: stage aml-startup fail DRIVER_REUSED";
         when ACPI_Bootstrap.Freeze_Failed =>
            return "acpi: stage startup-freeze fail " & Item.Freeze'Image;
         when ACPI_Bootstrap.Execution_Failed =>
            if Item.Execution.Phase = ACPI_Service.Module_Execution_Reported then
               return "acpi: stage aml-startup fail " & Item.Execution.Modules.Failure'Image &
                 " table=" & Item.Execution.Failed_Table_ID'Image &
                 " completed=" & Item.Execution.Modules.Completed'Image &
                 " charged=" & Item.Execution.Modules.Charged'Image &
                 " outcome=" & Item.Execution.Modules.Outcome'Image &
                 " access=" & Item.Execution_Status'Image;
            end if;
            return "acpi: stage aml-prepare fail " & Item.Execution.Preparation'Image &
              " phase=" & Item.Execution.Phase'Image &
              " access=" & Item.Execution_Status'Image;
         when ACPI_Bootstrap.Members_Failed =>
            return "acpi: stage startup-members fail " & Item.Member_Status'Image;
         when ACPI_Bootstrap.Finished =>
            return "acpi: stage aml-startup ok FINISHED";
      end case;
   end Format;
   function Device_Initialization_Completed
     (Item : ACPI_Bootstrap.Device_Report) return Boolean is
     (Item.Stage = ACPI_Bootstrap.Devices_Completed and then
      Item.Regions_Attempted and then Item.Region_Status in
        ACPI_Device_Policy.Regions_Connected | ACPI_Device_Policy.No_Regions_Required
      and then Item.Errors = 0 and then not Item.Fatal.Present);
   function Format (Item : ACPI_Bootstrap.Device_Report) return String is
   begin
      return "acpi: stage devices " &
        (if Device_Initialization_Completed (Item) then "ok " else "fail ") &
        Item.Stage'Image & " regions=" &
        (if Item.Regions_Attempted then Item.Region_Status'Image else "NOT_ATTEMPTED") &
        " errors=" & Item.Errors'Image & " fuel=" & Item.Charged'Image &
        (if Item.Errors > 0 then " node=" & Item.First_Error_Node'Image &
           " reason=" & Item.First_Error'Image else "");
   end Format;
end ACPI_Startup_Diagnostics;
