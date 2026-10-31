pragma Ada_2022;
with AML_Names;
package body ACPI_Lid with SPARK_Mode is
   package V renames Core.Values;
   package N renames Core.Namespace;
   use type V.Access_Status;
   use type V.Value_Handle;
   use type N.Object_Kind;
   use type N.Lookup_Status;
   use type AML_Execute.Execution_Status;
   Lid_Member : constant AML_Names.Segment := "_LID";
   procedure Query (Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Budget : Natural; Report : out Query_Report) is
      Found : N.Lookup_Result;
      Member : Core.Node_Descriptor;
      Value : V.Result;
      Handle : V.Value_Handle;
      Released : V.Access_Status;
   begin
      Report := (others => <>);
      if not Core.Current_Node (Service, Device)
        or else V.Node_Kind (Device) /= N.Device_Object
      then
         return;
      end if;
      if Budget = 0 then
         Report.Outcome := Budget_Exhausted;
         Report.Execution := AML_Execute.Budget_Exceeded;
         return;
      end if;
      Core.Find_Object (Service, V.Position (Device), Lid_Member, Found, Report.Access_Result);
      if Report.Access_Result /= V.Available then
         Report.Outcome := Lookup_Failed; return;
      elsif Found.Status = N.Not_Found then
         Report.Outcome := Missing_Lid; return;
      elsif Found.Status /= N.Found then
         Report.Outcome := Lookup_Failed; return;
      end if;
      Core.Describe_Node (Service, Found.Node, Member, Report.Access_Result);
      if Report.Access_Result /= V.Available then
         Report.Outcome := Lookup_Failed; return;
      end if;
      Evaluate_Named (Service, Member, Budget, Value, Report.Access_Result);
      Report.Charged := Value.Charged;
      Report.Execution := Value.Status;
      if Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
        AML_Execute.Metadata_Returned
      then
         Handle := Value.Handle;
         Core.Release_Result (Service, Handle, Released);
         if Released /= V.Available or else Handle /= V.No_Value then
            Report.Outcome := (if Report.Charged > Budget then Provider_Contract_Error else Cleanup_Failed); return;
         end if;
      end if;
      if Report.Charged > Budget then
         Report.Outcome := Provider_Contract_Error;
      elsif not Core.Current_Node (Service, Device) or else not Core.Current_Node (Service, Member) then
         Report.Outcome := Identity_Changed;
      elsif Report.Access_Result /= V.Available then
         Report.Outcome := Evaluation_Failed;
      elsif Value.Status = AML_Execute.Budget_Exceeded then
         Report.Outcome := Budget_Exhausted;
      elsif Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
        AML_Execute.Metadata_Returned
      then
         Report.Outcome := Wrong_Result_Type;
      elsif Value.Status /= AML_Execute.Returned then
         Report.Outcome := Evaluation_Failed;
      else
         Report.Outcome := Sampled;
         Report.Raw := Value.Number;
         Report.State := (if Value.Number = 0 then Closed else Open);
      end if;
   end Query;
   HID_Member : constant AML_Names.Segment := "_HID";
   Lid_Hardware_ID : constant AML_Decode.Integer_Value := 16#0D0C_D041#;
   Lid_Hardware_Name : constant AML_Decode.Bytes := [16#50#,16#4E#,16#50#,16#30#,16#43#,16#30#,16#44#];
   procedure Identify (Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Budget : Natural;
      Report : out Identification_Report) is
      use type AML_Decode.Bytes;
      Found : N.Lookup_Result;
      Member : Core.Node_Descriptor;
      Value : V.Result;
      Handle : V.Value_Handle;
      Released : V.Access_Status;
      Description : V.Value_Description;
      Data : AML_Decode.Bytes (Lid_Hardware_Name'Range);
      Copied : Natural;
   begin
      Report := (others => <>);
      if not Core.Current_Node (Service, Device)
        or else V.Node_Kind (Device) /= N.Device_Object
      then
         return;
      end if;
      if Budget = 0 then
         Report.Outcome := No_Budget; Report.Execution := AML_Execute.Budget_Exceeded; return;
      end if;
      Core.Find_Object (Service, V.Position (Device), HID_Member, Found, Report.Access_Result);
      if Report.Access_Result /= V.Available then
         Report.Outcome := Lookup_Error; return;
      elsif Found.Status = N.Not_Found then
         Report.Outcome := No_HID; return;
      elsif Found.Status /= N.Found then
         Report.Outcome := Lookup_Error; return;
      end if;
      Core.Describe_Node (Service, Found.Node, Member, Report.Access_Result);
      if Report.Access_Result /= V.Available then
         Report.Outcome := Lookup_Error; return;
      end if;
      Evaluate_Named (Service, Member, Budget, Value, Report.Access_Result);
      Report.Charged := Value.Charged; Report.Execution := Value.Status;
      Report.Outcome := Evaluation_Error;
      if Report.Access_Result = V.Available and then Value.Status = AML_Execute.Returned then
         Report.Outcome := (if Value.Number = Lid_Hardware_ID then Matched else Not_Lid);
      elsif Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
        AML_Execute.Metadata_Returned
      then
         Handle := Value.Handle;
         if Report.Access_Result = V.Available then
            Core.Describe_Result (Service, Handle, Description, Report.Access_Result);
            if Report.Access_Result = V.Available then
               case Description.Kind is
                  when V.String_Description =>
                     Report.Outcome := Not_Lid;
                     if Description.Length = Lid_Hardware_Name'Length then
                        Core.Read_Result_Bytes (Service, Handle, 0, Data, Copied, Report.Access_Result);
                        if Report.Access_Result /= V.Available or else Copied /= Data'Length then
                           Report.Outcome := Evaluation_Error;
                        elsif Data = Lid_Hardware_Name then
                           Report.Outcome := Matched;
                        end if;
                     end if;
                  when others => Report.Outcome := Wrong_HID_Type;
               end case;
            end if;
         end if;
         Core.Release_Result (Service, Handle, Released);
         if Released /= V.Available or else Handle /= V.No_Value then
            Report.Outcome := (if Report.Charged > Budget then Evaluator_Error else Cleanup_Error);
            return;
         end if;
      elsif Value.Status = AML_Execute.Budget_Exceeded then
         Report.Outcome := No_Budget;
      end if;
      if Report.Charged > Budget then
         Report.Outcome := Evaluator_Error;
      elsif not Core.Current_Node (Service, Device) or else not Core.Current_Node (Service, Member) then
         Report.Outcome := Binding_Changed;
      end if;
   end Identify;

end ACPI_Lid;
