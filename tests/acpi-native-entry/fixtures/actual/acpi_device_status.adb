pragma Ada_2022;
with AML_Names;
with ACPI_Device_Policy;
package body ACPI_Device_Status with SPARK_Mode is
   package V renames Core.Values;
   package N renames Core.Namespace;
   use type V.Access_Status, V.Value_Handle, N.Object_Kind, N.Lookup_Status;
   use type AML_Execute.Execution_Status;
   STA_Member : constant AML_Names.Segment := "_STA";
   Enabled_Mask : constant Interfaces.Unsigned_64 := 16#02#;
   procedure Observe_Status
     (Service : aliased in out Core.State; Device : Core.Node_Descriptor;
      Budget : Natural; Report : out Status_Report) is
      Before_Roots : constant Natural := Core.Retained_Results (Service);
      Root : V.Value_Handle := V.No_Value;
      Member : Core.Node_Descriptor;
      Found : N.Lookup_Result;
      Value : V.Result;
      procedure Run is
      begin
         if not Core.Current_Node (Service, Device)
           or else V.Node_Kind (Device) /= N.Device_Object then return; end if;
         if Budget = 0 then Report.Outcome := Budget_Exhausted; return; end if;
         Core.Find_Object (Service, V.Position (Device), STA_Member, Found, Report.Access_Result);
         if Report.Access_Result /= V.Available then Report.Outcome := Lookup_Failed; return; end if;
         if Found.Status = N.Not_Found then
            Report.Flags := ACPI_Device_Policy.Missing_Status_Default;
            Report.Defaulted := True;
         else
            if Found.Status /= N.Found then Report.Outcome := Lookup_Failed; return; end if;
            Core.Describe_Node (Service, Found.Node, Member, Report.Access_Result);
            if Report.Access_Result /= V.Available then Report.Outcome := Lookup_Failed; return; end if;
            Evaluate_Named (Service, Member, Budget, Value, Report.Access_Result);
            Report.Execution := Value.Status; Report.Last_Charge := Value.Charged;
            if Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
              AML_Execute.Metadata_Returned then Root := Value.Handle; end if;
            if Value.Charged > Budget then Report.Outcome := Provider_Contract_Error; return; end if;
            Report.Charged := Value.Charged;
            if not Core.Current_Node (Service, Device) or else not Core.Current_Node (Service, Member) then
               Report.Outcome := Identity_Changed; return;
            end if;
            if Report.Access_Result /= V.Available then Report.Outcome := Evaluation_Failed; return; end if;
            if Value.Status = AML_Execute.Budget_Exceeded then Report.Outcome := Budget_Exhausted; return; end if;
            if Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
              AML_Execute.Metadata_Returned | AML_Execute.No_Return then
               Report.Outcome := Wrong_Result_Type; return;
            elsif Value.Status /= AML_Execute.Returned then
               Report.Outcome := Evaluation_Failed; return;
            end if;
            Report.Flags := Interfaces.Unsigned_64 (Value.Number);
         end if;
         Report.Present := (Report.Flags and ACPI_Device_Policy.Present_Mask) /= 0;
         Report.Enabled := (Report.Flags and Enabled_Mask) /= 0;
         Report.Functioning := (Report.Flags and ACPI_Device_Policy.Functioning_Mask) /= 0;
         Report.Outcome := Status_Available;
      end Run;
   begin
      Report := (others => <>);
      Run;
      if Root /= V.No_Value then
         Core.Release_Result (Service, Root, Report.Cleanup_Result);
         if Report.Cleanup_Result /= V.Available or else Root /= V.No_Value then
            Report.Outcome := Cleanup_Failed;
         end if;
      end if;
      if Report.Outcome /= Cleanup_Failed and then Core.Retained_Results (Service) /= Before_Roots then
         Report.Outcome := Provider_Contract_Error;
      end if;
      if Report.Outcome /= Status_Available then
         Report.Flags := 0; Report.Present := False; Report.Enabled := False;
         Report.Functioning := False; Report.Defaulted := False;
      end if;
   end Observe_Status;
end ACPI_Device_Status;
