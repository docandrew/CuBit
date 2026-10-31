pragma Ada_2022;
with Ada.Text_IO;
with ACPI_Bootstrap;
with AML_Execute;
with AML_Decode;
with ACPI_Service;
with ACPI_Device_Policy;
with Ada.Strings.Fixed;
with ACPI_Startup_Diagnostics;
procedure Startup_Diagnostic_Format_Tests is
   package B renames ACPI_Bootstrap;
   package D renames ACPI_Startup_Diagnostics;
   use type B.Device_Phase;
   Item : B.Finish_Report;
   Device : B.Device_Report;
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   Check (D.Format (Item) = "acpi: stage aml-startup fail NOT_ATTEMPTED");
   Item.Stage := B.Freeze_Failed;
   Item.Freeze := ACPI_Service.Freeze_Empty;
   Check (D.Format (Item) = "acpi: stage startup-freeze fail FREEZE_EMPTY");
   Item.Stage := B.Execution_Failed;
   Item.Execution.Phase := ACPI_Service.Preparation_Failed;
   Item.Execution.Preparation := ACPI_Service.Values.Value_Capacity_Exceeded;
   Check (D.Format (Item) = "acpi: stage aml-prepare fail VALUE_CAPACITY_EXCEEDED" &
     " phase=PREPARATION_FAILED access=WRONG_PHASE");
   Item.Execution.Phase := ACPI_Service.Module_Execution_Reported;
   Item.Execution.Failed_Table_ID := 17;
   Item.Execution.Modules.Completed := 3;
   Item.Execution.Modules.Charged := 91;
   Item.Execution.Modules.Outcome := ACPI_Service.Values.Module_Rejected;
   Item.Execution.Modules.Failure := AML_Execute.Unsupported_Value;
   Item.Execution_Status := ACPI_Service.Values.Available;
   Check (D.Format (Item) = "acpi: stage aml-startup fail UNSUPPORTED_VALUE" &
     " table= 17 completed= 3 charged= 91 outcome=MODULE_REJECTED access=AVAILABLE");
   Item.Stage := B.Members_Failed;
   Item.Member_Status := ACPI_Service.Values.Invalid_Value;
   Check (D.Format (Item) = "acpi: stage startup-members fail INVALID_VALUE");
   Item.Stage := B.Finished;
   Check (D.Format (Item) = "acpi: stage aml-startup ok FINISHED");
   for Stage in B.Finish_Stage loop
      Item.Stage := Stage;
      Item.Execution.Failed_Table_ID := Natural'Last;
      Item.Execution.Modules.Completed := Natural'Last;
      Item.Execution.Modules.Charged := Natural'Last;
      Check (D.Format (Item)'Length <= 256);
   end loop;
   Check (D.Format (Device) = "acpi: stage devices fail DEVICES_NOT_STARTED" &
     " regions=NOT_ATTEMPTED errors= 0 fuel= 0");
   Device := (Stage => B.Devices_Completed, Regions_Attempted => True,
     Region_Status => ACPI_Device_Policy.No_Regions_Required, others => <>);
   Check (D.Format (Device) = "acpi: stage devices ok DEVICES_COMPLETED" &
     " regions=NO_REGIONS_REQUIRED errors= 0 fuel= 0");
   Device.Stage := B.Devices_Incomplete;
   Device.Region_Status := ACPI_Device_Policy.Regions_Unavailable;
   Check (D.Format (Device) = "acpi: stage devices fail DEVICES_INCOMPLETE" &
     " regions=REGIONS_UNAVAILABLE errors= 0 fuel= 0");
   Device := (Stage => B.Devices_Completed_With_Errors, Regions_Attempted => True,
     Region_Status => ACPI_Device_Policy.No_Regions_Required, Errors => 1,
     Charged => 35, First_Error_Node => 7, First_Error => AML_Execute.Unsupported_Value,
     others => <>);
   Check (D.Format (Device) = "acpi: stage devices fail DEVICES_COMPLETED_WITH_ERRORS" &
     " regions=NO_REGIONS_REQUIRED errors= 1 fuel= 35 node= 7 reason=UNSUPPORTED_VALUE");
   for Stage in B.Device_Phase loop
      for Region in ACPI_Device_Policy.Region_Outcome loop
         for Attempted in Boolean loop
            for Errors in Boolean loop
               Device := (Stage => Stage, Regions_Attempted => Attempted,
                 Region_Status => Region, Errors => (if Errors then Natural'Last else 0),
                 Charged => Natural'Last, First_Error_Node => ACPI_Service.Namespace.Node_ID'Last,
                 First_Error => AML_Execute.Execution_Status'Last, others => <>);
               declare
                  Text : constant String := D.Format (Device);
                  Complete : constant Boolean := Stage = B.Devices_Completed and then Attempted
                    and then Region in ACPI_Device_Policy.Regions_Connected |
                      ACPI_Device_Policy.No_Regions_Required and then not Errors;
               begin
                  Check (Text'Length <= 256);
                  Check ((Ada.Strings.Fixed.Index (Text, "devices ok ") > 0) = Complete);
                  Check ((Ada.Strings.Fixed.Index (Text, "regions=NOT_ATTEMPTED") > 0) = not Attempted);
               end;
            end loop;
         end loop;
      end loop;
   end loop;
   Device := (Stage => B.Devices_Completed, Regions_Attempted => True,
     Region_Status => ACPI_Device_Policy.Regions_Connected, others => <>);
   Device.Fatal := (Present => True, Item =>
     (Literal => (Kind => 0, Code => 0), Evaluated_Argument => 0, Width => AML_Decode.Bits_64));
   Check (not D.Device_Initialization_Completed (Device));
   Check (Ada.Strings.Fixed.Index (D.Format (Device), "devices fail ") > 0);
   Ada.Text_IO.Put_Line ("Startup diagnostic formatting checks" & Checks'Image);
end Startup_Diagnostic_Format_Tests;
