pragma Ada_2022;
with AML_Decode;
with AML_Names;
package body ACPI_Sleep_Preparation with SPARK_Mode is
   package V renames Core.Values;
   package N renames Core.Namespace;
   use type V.Access_Status, V.Value_Handle, N.Lookup_Status, N.Object_Kind;
   use type AML_Execute.Execution_Status;
   Names : constant array (Method_Phase) of AML_Names.Segment :=
     [Transition_To_State => "_TTS", Prepare_To_Sleep => "_PTS"];
   State_Argument_Count : constant Natural := 1;
   State_Argument : constant Natural := 0;
   procedure Prepare (Service : aliased in out Core.State; Target : Sleep_State;
      Budget : Natural; Report : out Preparation_Report;
      Phase : Method_Phase := Prepare_To_Sleep) is
      Before_Roots : constant Natural := Core.Retained_Results (Service);
      Found : N.Lookup_Result;
      Member : Core.Node_Descriptor;
      Value : V.Result;
      Root : V.Value_Handle := V.No_Value;
      Args : constant AML_Execute.Arguments :=
        [State_Argument => AML_Decode.Integer_Value (Sleep_State'Enum_Rep (Target)), others => 0];
      procedure Run is
      begin
         if Budget = 0 then Report.Status := Budget_Exhausted; return; end if;
         Core.Find_Object (Service, N.Root, Names (Phase), Found, Report.Access_Result);
         if Report.Access_Result /= V.Available then return; end if;
         if Found.Status = N.Not_Found then Report.Status := Not_Declared; return; end if;
         if Found.Status /= N.Found then return; end if;
         Report.Present := True;
         Core.Describe_Node (Service, Found.Node, Member, Report.Access_Result);
         if Report.Access_Result /= V.Available then return; end if;
         if V.Node_Kind (Member) /= N.Method_Object then
            Report.Status := Wrong_Object_Type; return;
         end if;
         Report.Invoked := True;
         Invoke (Service, Found.Node, Args, State_Argument_Count, Budget, Value, Report.Access_Result);
         Report.Execution := Value.Status;
         Report.AML_Charged := Value.Charged;
         if Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
           AML_Execute.Metadata_Returned then
            Root := Value.Handle;
            if Root = V.No_Value then Report.Status := Provider_Contract_Error; return; end if;
         end if;
         if Value.Charged > Budget then Report.Status := Provider_Contract_Error; return; end if;
         Report.Charged := Value.Charged;
         if not Core.Current_Node (Service, Member) then Report.Status := Identity_Changed; return; end if;
         if Report.Access_Result /= V.Available then Report.Status := Invocation_Failed; return; end if;
         if Value.Status = AML_Execute.Budget_Exceeded then Report.Status := Budget_Exhausted; return; end if;
         if Value.Status in AML_Execute.No_Return | AML_Execute.Returned | AML_Execute.Object_Returned |
           AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned then
            Report.Status := Method_Completed;
         else
            Report.Status := Invocation_Failed;
         end if;
      end Run;
   begin
      Report := (Target => Target, Phase => Phase, others => <>);
      Run;
      if Root /= V.No_Value then
         Core.Release_Result (Service, Root, Report.Cleanup_Result);
         if Report.Cleanup_Result /= V.Available or else Root /= V.No_Value then
            Report.Status := Cleanup_Failed;
            return;
         end if;
      end if;
      if Core.Retained_Results (Service) /= Before_Roots then
         Report.Status := Provider_Contract_Error;
      end if;
   end Prepare;
end ACPI_Sleep_Preparation;
