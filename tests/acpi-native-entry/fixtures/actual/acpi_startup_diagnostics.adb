with ACPI_Service;
with ACPI_Device_Policy;
with AML_Execute;
package body ACPI_Startup_Diagnostics is
   use type ACPI_Service.Execution_Phase_Result;
   use type ACPI_Bootstrap.Device_Phase;
   use type ACPI_Bootstrap.Finish_Stage;
   function Execution_Name (Item : AML_Execute.Execution_Status) return String is
     (case Item is
        when AML_Execute.Returned => "RETURNED",
        when AML_Execute.Object_Returned => "OBJECT_RETURNED",
        when AML_Execute.Reference_Returned => "REFERENCE_RETURNED",
        when AML_Execute.Metadata_Returned => "METADATA_RETURNED",
        when AML_Execute.No_Return => "NO_RETURN",
        when AML_Execute.Truncated => "TRUNCATED",
        when AML_Execute.Unsupported => "UNSUPPORTED",
        when AML_Execute.Uninitialized => "UNINITIALIZED",
        when AML_Execute.Missing_Argument => "MISSING_ARGUMENT",
        when AML_Execute.Budget_Exceeded => "BUDGET_EXCEEDED",
        when AML_Execute.Invalid_Method => "INVALID_METHOD",
        when AML_Execute.Argument_Mismatch => "ARGUMENT_MISMATCH",
        when AML_Execute.Mutex_Order => "MUTEX_ORDER",
        when AML_Execute.Duplicate_Name => "DUPLICATE_NAME",
        when AML_Execute.Namespace_Limit => "NAMESPACE_LIMIT",
        when AML_Execute.Expression_Limit => "EXPRESSION_LIMIT",
        when AML_Execute.Block_Limit => "BLOCK_LIMIT",
        when AML_Execute.Bad_Package => "BAD_PACKAGE",
        when AML_Execute.Invalid_Control => "INVALID_CONTROL",
        when AML_Execute.Unknown_Name => "UNKNOWN_NAME",
        when AML_Execute.Unsupported_Value => "UNSUPPORTED_VALUE",
        when AML_Execute.Bad_Name => "BAD_NAME",
        when AML_Execute.Call_Limit => "CALL_LIMIT",
        when AML_Execute.Division_By_Zero => "DIVISION_BY_ZERO",
        when AML_Execute.Empty_Buffer => "EMPTY_BUFFER",
        when AML_Execute.Missing_Result => "MISSING_RESULT",
        when AML_Execute.Value_Limit => "VALUE_LIMIT",
        when AML_Execute.Numeric_Overflow => "NUMERIC_OVERFLOW",
        when AML_Execute.Invalid_Delay => "INVALID_DELAY",
        when AML_Execute.Delay_Failed => "DELAY_FAILED",
        when AML_Execute.Package_Limit => "PACKAGE_LIMIT",
        when AML_Execute.Invalid_Match_Operation => "INVALID_MATCH_OPERATION",
        when AML_Execute.No_Resource_End_Tag => "NO_RESOURCE_END_TAG",
        when AML_Execute.Invalid_Resource_Type => "INVALID_RESOURCE_TYPE",
        when AML_Execute.Bad_Resource_Length => "BAD_RESOURCE_LENGTH",
        when AML_Execute.Resource_Buffer_Length => "RESOURCE_BUFFER_LENGTH",
        when AML_Execute.Invalid_Field_Length => "INVALID_FIELD_LENGTH",
        when AML_Execute.Buffer_Field_Limit => "BUFFER_FIELD_LIMIT",
        when AML_Execute.Notify_Limit => "NOTIFY_LIMIT",
        when AML_Execute.Mutex_Not_Acquired => "MUTEX_NOT_ACQUIRED",
        when AML_Execute.Invalid_Table_Length => "INVALID_TABLE_LENGTH",
        when AML_Execute.Field_Indeterminate => "FIELD_INDETERMINATE",
        when AML_Execute.Fatal_Requested => "FATAL_REQUESTED",
        when AML_Execute.Module_Completed => "MODULE_COMPLETED");

   function Freeze_Name (Item : ACPI_Service.Freeze_Status) return String is
     (case Item is
        when ACPI_Service.Catalog_Frozen_OK => "CATALOG_FROZEN_OK",
        when ACPI_Service.Freeze_Wrong_Phase => "FREEZE_WRONG_PHASE",
        when ACPI_Service.Freeze_Empty => "FREEZE_EMPTY");

   function Preparation_Name (Item : ACPI_Service.Values.Preparation_Outcome) return String is
     (case Item is
        when ACPI_Service.Values.Preparation_Not_Attempted => "PREPARATION_NOT_ATTEMPTED",
        when ACPI_Service.Values.Preparation_Rejected => "PREPARATION_REJECTED",
        when ACPI_Service.Values.Firmware_Prepared => "FIRMWARE_PREPARED",
        when ACPI_Service.Values.Namespace_Capacity_Exceeded => "NAMESPACE_CAPACITY_EXCEEDED",
        when ACPI_Service.Values.Value_Capacity_Exceeded => "VALUE_CAPACITY_EXCEEDED");

   function Execution_Phase_Name (Item : ACPI_Service.Execution_Phase_Result) return String is
     (case Item is
        when ACPI_Service.Execution_Not_Started => "EXECUTION_NOT_STARTED",
        when ACPI_Service.Preparation_Failed => "PREPARATION_FAILED",
        when ACPI_Service.Module_Execution_Reported => "MODULE_EXECUTION_REPORTED");

   function Access_Name (Item : ACPI_Service.Values.Access_Status) return String is
     (case Item is
        when ACPI_Service.Values.Available => "AVAILABLE",
        when ACPI_Service.Values.Invalid_Value => "INVALID_VALUE",
        when ACPI_Service.Values.Uninitialized_Element => "UNINITIALIZED_ELEMENT",
        when ACPI_Service.Values.Wrong_Kind => "WRONG_KIND",
        when ACPI_Service.Values.Out_Of_Bounds => "OUT_OF_BOUNDS",
        when ACPI_Service.Values.Root_Limit => "ROOT_LIMIT",
        when ACPI_Service.Values.Identity_Exhausted => "IDENTITY_EXHAUSTED",
        when ACPI_Service.Values.Busy => "BUSY",
        when ACPI_Service.Values.Wrong_Phase => "WRONG_PHASE");

   function Module_Name (Item : ACPI_Service.Values.Module_Outcome) return String is
     (case Item is
        when ACPI_Service.Values.Catalog_Rejected => "CATALOG_REJECTED",
        when ACPI_Service.Values.Modules_Completed => "MODULES_COMPLETED",
        when ACPI_Service.Values.Module_Rejected => "MODULE_REJECTED");

   function Device_Name (Item : ACPI_Bootstrap.Device_Phase) return String is
     (case Item is
        when ACPI_Bootstrap.Devices_Not_Started => "DEVICES_NOT_STARTED",
        when ACPI_Bootstrap.Devices_Preparing => "DEVICES_PREPARING",
        when ACPI_Bootstrap.Devices_Awaiting_Regions => "DEVICES_AWAITING_REGIONS",
        when ACPI_Bootstrap.Devices_Walking => "DEVICES_WALKING",
        when ACPI_Bootstrap.Devices_Completed => "DEVICES_COMPLETED",
        when ACPI_Bootstrap.Devices_Completed_With_Errors => "DEVICES_COMPLETED_WITH_ERRORS",
        when ACPI_Bootstrap.Devices_Incomplete => "DEVICES_INCOMPLETE",
        when ACPI_Bootstrap.Devices_Failed => "DEVICES_FAILED");

   function Region_Name (Item : ACPI_Device_Policy.Region_Outcome) return String is
     (case Item is
        when ACPI_Device_Policy.Regions_Connected => "REGIONS_CONNECTED",
        when ACPI_Device_Policy.No_Regions_Required => "NO_REGIONS_REQUIRED",
        when ACPI_Device_Policy.Regions_Unavailable => "REGIONS_UNAVAILABLE",
        when ACPI_Device_Policy.Region_Initialization_Failed => "REGION_INITIALIZATION_FAILED");

   function Format_Field_Failure
     (Item : ACPI_Bootstrap.Finish_Report;
      Capacity : ACPI_Capacity_Diagnostics.Report := ACPI_Capacity_Diagnostics.No_Report) return String is
      function Space_Name return String is
      begin
         if not Item.Execution.Modules.Field_Failure.Region_Known then return "UNKNOWN"; end if;
         case Item.Execution.Modules.Field_Failure.Space is
            when 0 => return "SYSTEM_MEMORY";
            when 1 => return "SYSTEM_IO";
            when 2 => return "PCI_CONFIG";
            when 3 => return "EMBEDDED_CONTROL";
            when 4 => return "SMBUS";
            when 5 => return "SYSTEM_CMOS";
            when 8 => return "GPIO";
            when 9 => return "GENERIC_SERIAL_BUS";
            when others => return "OTHER" & Item.Execution.Modules.Field_Failure.Space'Image;
         end case;
      end Space_Name;
   begin
      if Item.Stage /= ACPI_Bootstrap.Execution_Failed or else
        Item.Execution.Phase /= ACPI_Service.Module_Execution_Reported or else
        not Item.Execution.Modules.Field_Failure.Present
      then return "acpi: stage aml-field ok NONE"; end if;
      return "acpi: stage aml-field fail " & (if ACPI_Capacity_Diagnostics.Present (Capacity)
           and then Item.Execution.Modules.Field_Failure.Failure in AML_Execute.Unsupported_Value
           then "CAPACITY"
           else Execution_Name (Item.Execution.Modules.Field_Failure.Failure))
        & " name=" & Item.Execution.Modules.Field_Failure.Name
        & " region=" & Item.Execution.Modules.Field_Failure.Region
        & " space=" & Space_Name
        & " direction=" & (if Item.Execution.Modules.Field_Failure.For_Write then "WRITE" else "READ")
        & (if ACPI_Capacity_Diagnostics.Present (Capacity)
           then " " & ACPI_Capacity_Diagnostics.Format (Capacity) else "");
   end Format_Field_Failure;

   function Format (Item : ACPI_Bootstrap.Finish_Report) return String is
   begin
      case Item.Stage is
         when ACPI_Bootstrap.Not_Attempted =>
            return "acpi: stage aml-startup fail NOT_ATTEMPTED";
         when ACPI_Bootstrap.Driver_Reused =>
            return "acpi: stage aml-startup fail DRIVER_REUSED";
         when ACPI_Bootstrap.Freeze_Failed =>
            return "acpi: stage startup-freeze fail " & Freeze_Name (Item.Freeze);
         when ACPI_Bootstrap.Execution_Failed =>
            if Item.Execution.Phase = ACPI_Service.Module_Execution_Reported then
               return "acpi: stage aml-startup fail " & Execution_Name (Item.Execution.Modules.Failure) &
                 " table=" & Item.Execution.Failed_Table_ID'Image &
                 " completed=" & Item.Execution.Modules.Completed'Image &
                 " charged=" & Item.Execution.Modules.Charged'Image &
                 " outcome=" & Module_Name (Item.Execution.Modules.Outcome) &
                 " access=" & Access_Name (Item.Execution_Status);
            end if;
            return "acpi: stage aml-prepare fail " & Preparation_Name (Item.Execution.Preparation) &
              " phase=" & Execution_Phase_Name (Item.Execution.Phase) &
              " access=" & Access_Name (Item.Execution_Status);
         when ACPI_Bootstrap.Members_Failed =>
            return "acpi: stage startup-members fail " & Access_Name (Item.Member_Status);
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
        Device_Name (Item.Stage) & " regions=" &
        (if Item.Regions_Attempted then Region_Name (Item.Region_Status) else "NOT_ATTEMPTED") &
        " errors=" & Item.Errors'Image & " fuel=" & Item.Charged'Image &
        (if Item.Errors > 0 then " node=" & Item.First_Error_Node'Image &
           " reason=" & Execution_Name (Item.First_Error) else "");
   end Format;
end ACPI_Startup_Diagnostics;
