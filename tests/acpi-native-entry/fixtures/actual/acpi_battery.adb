pragma Ada_2022;
with AML_Names;
with ACPI_Device_Policy;
package body ACPI_Battery with SPARK_Mode is
   package V renames Core.Values;
   package N renames Core.Namespace;
   package B renames ACPI_Battery_Status;
   use type V.Access_Status;
   use type V.Value_Handle;
   use type V.Value_Kind;
   use type N.Object_Kind;
   use type N.Lookup_Status;
   use type AML_Execute.Execution_Status;
   use type AML_Decode.Integer_Value;
   BST_Member : constant AML_Names.Segment := "_BST";
   HID_Member : constant AML_Names.Segment := "_HID";
   STA_Member : constant AML_Names.Segment := "_STA";
   Enabled_Mask : constant AML_Decode.Integer_Value := 16#02#;
   Battery_Present_Mask : constant AML_Decode.Integer_Value := 16#10#;
   Hardware_EISA : constant AML_Decode.Integer_Value := 16#0A0C_D041#;
   Hardware_Bytes : constant AML_Decode.Bytes :=
     [16#50#, 16#4E#, 16#50#, 16#30#, 16#43#, 16#30#, 16#41#];
   Fields : constant array (Positive range 1 .. B.Expected_Elements) of B.Field :=
     [B.State_Field, B.Rate_Field, B.Capacity_Field, B.Voltage_Field];
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
      if Budget = 0 then Outcome := No_Budget; return; end if;
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
   procedure Query (Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Budget : Natural; Report : out Query_Report) is
      Before_Roots : constant Natural := Core.Retained_Results (Service);
      Value : V.Result;
      Evaluation : Evaluation_Outcome;
      Root, Element : V.Value_Handle := V.No_Value;
      Root_Tried, Element_Tried, Cleanup_Error : Boolean := False;
      Description : V.Value_Description;
      Decoded : B.Decode_Result;
      procedure Drop (Handle : in out V.Value_Handle; Tried : in out Boolean) is
         Released : V.Access_Status;
      begin
         if Tried or else Handle = V.No_Value then return; end if;
         Tried := True;
         Core.Release_Result (Service, Handle, Released);
         if Released /= V.Available or else Handle /= V.No_Value then
            Report.Cleanup_Result := Released; Cleanup_Error := True;
         end if;
      end Drop;
      procedure Inspect is
      begin
         if Value.Status /= AML_Execute.Object_Returned then Report.Outcome := Wrong_Result_Type; return; end if;
         Core.Describe_Result (Service, Root, Description, Report.Access_Result);
         if Report.Access_Result /= V.Available then Report.Outcome := Evaluation_Failed; return; end if;
         if Description.Kind /= V.Package_Description then Report.Outcome := Wrong_Result_Type; return; end if;
         Report.Elements := Description.Length;
         if Report.Elements /= B.Expected_Elements then Report.Outcome := Wrong_Element_Count; return; end if;
         for I in Report.Raw'Range loop
            Report.Failure_Field := Fields (I);
            if Report.Charged = Budget then Report.Outcome := Budget_Exhausted; return; end if;
            Report.Charged := Report.Charged + 1;
            Element_Tried := False;
            Core.Read_Result_Element (Service, Root, I - Report.Raw'First, Element, Report.Access_Result);
            if Report.Access_Result /= V.Available then Report.Outcome := Evaluation_Failed; return; end if;
            Core.Describe_Result (Service, Element, Description, Report.Access_Result);
            if Report.Access_Result /= V.Available then Report.Outcome := Evaluation_Failed; return; end if;
            if Description.Kind /= V.Integer_Description then Report.Outcome := Wrong_Element_Type; return; end if;
            Report.Raw (I) := Description.Number;
            Report.Fields_Read := Report.Fields_Read + 1;
            Drop (Element, Element_Tried);
            if Cleanup_Error then return; end if;
         end loop;
         Decoded := B.Decode (Report.Raw);
         Report.Decode_Status := Decoded.Status;
         Report.Failure_Field := Decoded.Failure_Field;
         if Decoded.Status /= B.Decoded then Report.Outcome := Invalid_Package_Value; return; end if;
         Report.Value := Decoded.Value; Report.Outcome := Sampled;
      end Inspect;
   begin
      Report := (others => <>);
      Evaluate (Service, Device, BST_Member, Budget, Value, Evaluation,
        Report.Charged, Report.Last_Charge, Report.Access_Result);
      Report.Execution := Value.Status;
      if Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
        AML_Execute.Metadata_Returned then Root := Value.Handle; end if;
      case Evaluation is
         when Bad_Device => Report.Outcome := Invalid_Device;
         when No_Budget => Report.Outcome := Budget_Exhausted;
         when No_Member => Report.Outcome := Missing_BST;
         when Lookup_Error => Report.Outcome := Lookup_Failed;
         when Evaluation_Error => Report.Outcome := Evaluation_Failed;
         when Binding_Changed => Report.Outcome := Identity_Changed;
         when Provider_Error => Report.Outcome := Provider_Contract_Error;
         when Member_Evaluated => Inspect;
      end case;
      Drop (Element, Element_Tried); Drop (Root, Root_Tried);
      if Cleanup_Error then Report.Outcome := Cleanup_Failed;
      elsif Core.Retained_Results (Service) /= Before_Roots then Report.Outcome := Provider_Contract_Error;
      end if;
      if Report.Outcome /= Sampled then Report.Value := B.Empty_Sample; end if;
   end Query;
   procedure Identify (Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Budget : Natural; Report : out Identification_Report) is
      use type AML_Decode.Bytes;
      Before_Roots : constant Natural := Core.Retained_Results (Service);
      Value : V.Result;
      Evaluation : Evaluation_Outcome;
      Handle : V.Value_Handle := V.No_Value;
      Description : V.Value_Description;
      Data : AML_Decode.Bytes (Hardware_Bytes'Range);
      Copied : Natural;
      procedure Inspect is
      begin
         if Value.Status = AML_Execute.Returned then
            Report.Outcome := (if Value.Number = Hardware_EISA then Matched else Not_Battery);
            return;
         end if;
         if Value.Status /= AML_Execute.Object_Returned then Report.Outcome := Wrong_HID_Type; return; end if;
         Core.Describe_Result (Service, Handle, Description, Report.Access_Result);
         if Report.Access_Result /= V.Available then Report.Outcome := Evaluation_Failed; return; end if;
         if Description.Kind /= V.String_Description then Report.Outcome := Wrong_HID_Type; return; end if;
         if Description.Length /= Hardware_Bytes'Length then Report.Outcome := Not_Battery; return; end if;
         Core.Read_Result_Bytes (Service, Handle, 0, Data, Copied, Report.Access_Result);
         if Report.Access_Result /= V.Available or else Copied /= Data'Length then
            Report.Outcome := Evaluation_Failed; return;
         end if;
         Report.Outcome := (if Data = Hardware_Bytes then Matched else Not_Battery);
      end Inspect;
   begin
      Report := (others => <>);
      Evaluate (Service, Device, HID_Member, Budget, Value, Evaluation,
        Report.Charged, Report.Last_Charge, Report.Access_Result);
      Report.Execution := Value.Status;
      if Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
        AML_Execute.Metadata_Returned then Handle := Value.Handle; end if;
      case Evaluation is
         when Bad_Device => Report.Outcome := Invalid_Device;
         when No_Budget => Report.Outcome := Budget_Exhausted;
         when No_Member => Report.Outcome := Missing_HID;
         when Lookup_Error => Report.Outcome := Lookup_Failed;
         when Evaluation_Error => Report.Outcome := Evaluation_Failed;
         when Binding_Changed => Report.Outcome := Identity_Changed;
         when Provider_Error => Report.Outcome := Provider_Contract_Error;
         when Member_Evaluated => Inspect;
      end case;
      if Handle /= V.No_Value then
         Core.Release_Result (Service, Handle, Report.Cleanup_Result);
         if Report.Cleanup_Result /= V.Available or else Handle /= V.No_Value then
            Report.Outcome := Cleanup_Failed;
         end if;
      end if;
      if Report.Outcome /= Cleanup_Failed and then Core.Retained_Results (Service) /= Before_Roots then
         Report.Outcome := Provider_Contract_Error;
      end if;
   end Identify;
   procedure Query_Presence (Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Budget : Natural; Report : out Presence_Report) is
      Before_Roots : constant Natural := Core.Retained_Results (Service);
      Value : V.Result;
      Evaluation : Evaluation_Outcome;
      Handle : V.Value_Handle := V.No_Value;
   begin
      Report := (others => <>);
      Evaluate (Service, Device, STA_Member, Budget, Value, Evaluation,
        Report.Charged, Report.Last_Charge, Report.Access_Result);
      Report.Execution := Value.Status;
      if Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
        AML_Execute.Metadata_Returned then Handle := Value.Handle; end if;
      case Evaluation is
         when Bad_Device => Report.Outcome := Invalid_Device;
         when No_Budget => Report.Outcome := Budget_Exhausted;
         when No_Member =>
            Report.Flags := ACPI_Device_Policy.Missing_Status_Default;
            Report.Defaulted := True; Report.Outcome := Observed;
         when Lookup_Error => Report.Outcome := Lookup_Failed;
         when Evaluation_Error => Report.Outcome := Evaluation_Failed;
         when Binding_Changed => Report.Outcome := Identity_Changed;
         when Provider_Error => Report.Outcome := Provider_Contract_Error;
         when Member_Evaluated =>
            if Value.Status /= AML_Execute.Returned then Report.Outcome := Wrong_Result_Type;
            else Report.Flags := Value.Number; Report.Outcome := Observed;
            end if;
      end case;
      if Handle /= V.No_Value then
         Core.Release_Result (Service, Handle, Report.Cleanup_Result);
         if Report.Cleanup_Result /= V.Available or else Handle /= V.No_Value then
            Report.Outcome := Cleanup_Failed;
         end if;
      end if;
      if Report.Outcome /= Cleanup_Failed and then Core.Retained_Results (Service) /= Before_Roots then
         Report.Outcome := Provider_Contract_Error;
      end if;
      if Report.Outcome = Observed then
         Report.Device_Present := (Report.Flags and ACPI_Device_Policy.Present_Mask) /= 0;
         Report.Battery_Present := (Report.Flags and Battery_Present_Mask) /= 0;
         Report.Enabled := (Report.Flags and Enabled_Mask) /= 0;
         Report.Functioning := (Report.Flags and ACPI_Device_Policy.Functioning_Mask) /= 0;
      else
         Report.Flags := 0; Report.Defaulted := False;
      end if;
   end Query_Presence;
end ACPI_Battery;
