pragma Ada_2022;
with AML_Names;
package body ACPI_Power_Source with SPARK_Mode is
   package V renames Core.Values;
   package N renames Core.Namespace;
   use type V.Access_Status;
   use type V.Value_Handle;
   use type V.Value_Kind;
   use type N.Object_Kind;
   use type N.Lookup_Status;
   use type AML_Execute.Execution_Status;
   PSR_Member : constant AML_Names.Segment := "_PSR";
   HID_Member : constant AML_Names.Segment := "_HID";
   -- Exact ACPI0003 hardware identifier in the AML string byte representation.
   Hardware_Bytes : constant AML_Decode.Bytes :=
     [16#41#, 16#43#, 16#50#, 16#49#, 16#30#, 16#30#, 16#30#, 16#33#];
   type Evaluation_Outcome is (Member_Evaluated, Bad_Device, No_Budget,
      No_Member, Lookup_Error, Evaluation_Error, Binding_Changed, Provider_Error);
   procedure Evaluate
     (Service : aliased in out Core.State; Device : Core.Node_Descriptor;
      Name : AML_Names.Segment; Budget : Natural; Value : out V.Result;
      Outcome : out Evaluation_Outcome; Charged, Last_Charge : out Natural;
      Access_Result : out V.Access_Status) is
      Found : N.Lookup_Result;
      Member : Core.Node_Descriptor;
   begin
      Value := (Status => AML_Execute.No_Return, Charged => 0);
      Charged := 0; Last_Charge := 0; Access_Result := V.Available;
      Outcome := Bad_Device;
      if not Core.Current_Node (Service, Device)
        or else V.Node_Kind (Device) /= N.Device_Object then return; end if;
      if Budget = 0 then
         Value := (Status => AML_Execute.Budget_Exceeded, Charged => 0);
         Outcome := No_Budget; return;
      end if;
      Core.Find_Object (Service, V.Position (Device), Name, Found, Access_Result);
      if Access_Result /= V.Available then Outcome := Lookup_Error; return; end if;
      if Found.Status = N.Not_Found then Outcome := No_Member; return; end if;
      if Found.Status /= N.Found then Outcome := Lookup_Error; return; end if;
      Core.Describe_Node (Service, Found.Node, Member, Access_Result);
      if Access_Result /= V.Available then Outcome := Lookup_Error; return; end if;
      Evaluate_Named (Service, Member, Budget, Value, Access_Result);
      Last_Charge := Value.Charged;
      if Value.Charged > Budget then Outcome := Provider_Error; return; end if;
      Charged := Value.Charged;
      if not Core.Current_Node (Service, Device) or else not Core.Current_Node (Service, Member) then
         Outcome := Binding_Changed;
      elsif Access_Result /= V.Available then Outcome := Evaluation_Error;
      elsif Value.Status = AML_Execute.Budget_Exceeded then Outcome := No_Budget;
      elsif Value.Status in AML_Execute.Returned | AML_Execute.Object_Returned |
        AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned
      then Outcome := Member_Evaluated;
      else Outcome := Evaluation_Error;
      end if;
   end Evaluate;
   procedure Release_Observation
     (Service : aliased in out Core.State; Value : V.Result;
      Cleanup : out V.Access_Status; Failed : out Boolean) is
      Handle : V.Value_Handle;
   begin
      Cleanup := V.Available; Failed := False;
      if Value.Status not in AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
        AML_Execute.Metadata_Returned then return; end if;
      Handle := Value.Handle;
      Core.Release_Result (Service, Handle, Cleanup);
      Failed := Cleanup /= V.Available or else Handle /= V.No_Value;
   end Release_Observation;
   procedure Query (Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Budget : Natural; Report : out Query_Report) is
      Before_Roots : constant Natural := Core.Retained_Results (Service);
      Value : V.Result;
      Outcome : Evaluation_Outcome;
      Failed : Boolean;
   begin
      Report := (others => <>);
      Evaluate (Service, Device, PSR_Member, Budget, Value, Outcome,
        Report.Charged, Report.Last_Charge, Report.Access_Result);
      Report.Execution := Value.Status;
      case Outcome is
         when Bad_Device => Report.Outcome := Invalid_Device;
         when No_Budget => Report.Outcome := Budget_Exhausted;
         when No_Member => Report.Outcome := Missing_PSR;
         when Lookup_Error => Report.Outcome := Lookup_Failed;
         when Evaluation_Error => Report.Outcome := Evaluation_Failed;
         when Binding_Changed => Report.Outcome := Identity_Changed;
         when Provider_Error => Report.Outcome := Provider_Contract_Error;
         when Member_Evaluated =>
            if Value.Status /= AML_Execute.Returned then Report.Outcome := Wrong_Result_Type;
            else
               Report.Raw := Value.Number;
               if Report.Raw not in Offline_Code .. Online_Code then Report.Outcome := Invalid_Status_Value;
               else
                  Report.Outcome := Sampled;
                  Report.State := (if Report.Raw = Offline_Code then Offline else Online);
               end if;
            end if;
      end case;
      Release_Observation (Service, Value, Report.Cleanup_Result, Failed);
      if Failed then Report.Outcome := Cleanup_Failed;
      elsif Core.Retained_Results (Service) /= Before_Roots then Report.Outcome := Provider_Contract_Error;
      end if;
      if Report.Outcome /= Sampled then Report.State := Unknown; end if;
      if Report.Outcome not in Sampled | Invalid_Status_Value then Report.Raw := 0; end if;
   end Query;
   procedure Identify (Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Budget : Natural; Report : out Identification_Report) is
      use type AML_Decode.Bytes;
      Before_Roots : constant Natural := Core.Retained_Results (Service);
      Value : V.Result;
      Outcome : Evaluation_Outcome;
      Failed : Boolean;
      Description : V.Value_Description;
      Data : AML_Decode.Bytes (Hardware_Bytes'Range);
      Copied : Natural;
      procedure Inspect is
      begin
         if Value.Status = AML_Execute.Returned then Report.Outcome := Not_Power_Source; return; end if;
         if Value.Status /= AML_Execute.Object_Returned then Report.Outcome := Wrong_HID_Type; return; end if;
         Core.Describe_Result (Service, Value.Handle, Description, Report.Access_Result);
         if Report.Access_Result /= V.Available then Report.Outcome := Evaluation_Failed; return; end if;
         if Description.Kind /= V.String_Description then Report.Outcome := Wrong_HID_Type; return; end if;
         if Description.Length /= Hardware_Bytes'Length then Report.Outcome := Not_Power_Source; return; end if;
         Core.Read_Result_Bytes (Service, Value.Handle, 0, Data, Copied, Report.Access_Result);
         if Report.Access_Result /= V.Available or else Copied /= Data'Length then
            Report.Outcome := Evaluation_Failed; return;
         end if;
         Report.Outcome := (if Data = Hardware_Bytes then Matched else Not_Power_Source);
      end Inspect;
   begin
      Report := (others => <>);
      Evaluate (Service, Device, HID_Member, Budget, Value, Outcome,
        Report.Charged, Report.Last_Charge, Report.Access_Result);
      Report.Execution := Value.Status;
      case Outcome is
         when Bad_Device => Report.Outcome := Invalid_Device;
         when No_Budget => Report.Outcome := Budget_Exhausted;
         when No_Member => Report.Outcome := Missing_HID;
         when Lookup_Error => Report.Outcome := Lookup_Failed;
         when Evaluation_Error => Report.Outcome := Evaluation_Failed;
         when Binding_Changed => Report.Outcome := Identity_Changed;
         when Provider_Error => Report.Outcome := Provider_Contract_Error;
         when Member_Evaluated => Inspect;
      end case;
      Release_Observation (Service, Value, Report.Cleanup_Result, Failed);
      if Failed then Report.Outcome := Cleanup_Failed;
      elsif Core.Retained_Results (Service) /= Before_Roots then Report.Outcome := Provider_Contract_Error;
      end if;
   end Identify;
end ACPI_Power_Source;
