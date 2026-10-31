pragma Ada_2022;
with AML_Names;
package body ACPI_Sleep_Objects with SPARK_Mode is
   package V renames Core.Values;
   package N renames Core.Namespace;
   use type V.Access_Status;
   use type V.Value_Handle;
   use type V.Value_Kind;
   use type N.Lookup_Status;
   use type AML_Execute.Execution_Status;
   use type AML_Decode.Integer_Value;
   Names : constant array (System_State) of AML_Names.Segment :=
     [S0 => "_S0_", S1 => "_S1_", S2 => "_S2_", S3 => "_S3_", S4 => "_S4_", S5 => "_S5_"];
   First_Field : constant Natural := 0;
   Second_Field : constant Natural := 1;
   Single_Field_Count : constant Natural := 1;
   Paired_Field_Count : constant Natural := 2;
   procedure Query (Service : aliased in out Core.State; Kind : System_State;
      Registers : Register_Model; Budget : Natural; Report : out Query_Report) is
      Before_Roots : constant Natural := Core.Retained_Results (Service);
      Found : N.Lookup_Result;
      Member : Core.Node_Descriptor;
      Value : V.Result;
      Root, Element : V.Value_Handle := V.No_Value;
      Root_Tried, Element_Tried, Cleanup_Error : Boolean := False;
      Description : V.Value_Description;
      Required_Elements : constant Natural :=
        (if Registers = PM1A_And_B then Paired_Field_Count else Single_Field_Count);
      procedure Drop (Handle : in out V.Value_Handle; Tried : in out Boolean) is
         Released : V.Access_Status;
      begin
         if Tried or else Handle = V.No_Value then return; end if;
         Tried := True;
         Core.Release_Result (Service, Handle, Released);
         if Released /= V.Available or else Handle /= V.No_Value then
            Report.Cleanup_Result := Released;
            Cleanup_Error := True;
         end if;
      end Drop;
      procedure Read_Field (Index : Natural; Field : Sleep_Field; OK : out Boolean) is
         Raw : AML_Decode.Integer_Value;
      begin
         OK := False; Report.Failure_Field := Field;
         if Report.Charged = Budget then Report.Outcome := Budget_Exhausted; return; end if;
         Report.Charged := Report.Charged + 1;
         Element_Tried := False;
         Core.Read_Result_Element (Service, Root, Index, Element, Report.Access_Result);
         if Report.Access_Result /= V.Available then Report.Outcome := Evaluation_Failed; return; end if;
         Core.Describe_Result (Service, Element, Description, Report.Access_Result);
         if Report.Access_Result /= V.Available then Report.Outcome := Evaluation_Failed; return; end if;
         if Description.Kind /= V.Integer_Description then Report.Outcome := Wrong_Element_Type; return; end if;
         Raw := Description.Number;
         if Field = Field_A then Report.Raw_A := Raw; else Report.Raw_B := Raw; end if;
         if Raw > AML_Decode.Integer_Value (Sleep_Type'Last) then
            Report.Outcome := Invalid_Type_Value; return;
         end if;
         if Field = Field_A then Report.Type_A := Sleep_Type (Raw); Report.A_Available := True;
         else Report.Type_B := Sleep_Type (Raw); Report.B_Available := True;
         end if;
         OK := True; Report.Failure_Field := No_Field;
      end Read_Field;
      procedure Run is
         OK : Boolean;
      begin
         if Budget = 0 then Report.Outcome := Budget_Exhausted; return; end if;
         Core.Find_Object (Service, N.Root, Names (Kind), Found, Report.Access_Result);
         if Report.Access_Result /= V.Available then return; end if;
         if Found.Status = N.Not_Found then
            Report.Support := (if Kind in S0 | S5 then Required_State else No_Declaration);
            Report.Outcome := Not_Declared; return;
         end if;
         if Found.Status /= N.Found then return; end if;
         Report.Present := True; Report.Support := Declared_State;
         Core.Describe_Node (Service, Found.Node, Member, Report.Access_Result);
         if Report.Access_Result /= V.Available then return; end if;
         Evaluate_Named (Service, Member, Budget, Value, Report.Access_Result);
         Report.Execution := Value.Status; Report.AML_Charged := Value.Charged;
         if Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
           AML_Execute.Metadata_Returned then Root := Value.Handle; end if;
         if Value.Charged > Budget then Report.Outcome := Provider_Contract_Error; return; end if;
         Report.Charged := Value.Charged;
         if not Core.Current_Node (Service, Member) then Report.Outcome := Identity_Changed; return; end if;
         if Report.Access_Result /= V.Available then Report.Outcome := Evaluation_Failed; return; end if;
         if Value.Status = AML_Execute.Budget_Exceeded then Report.Outcome := Budget_Exhausted; return; end if;
         if Value.Status not in AML_Execute.Returned | AML_Execute.Object_Returned |
           AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned | AML_Execute.No_Return then
            Report.Outcome := Evaluation_Failed; return;
         end if;
         if Registers = No_Sleep_Registers then Report.Outcome := Return_Ignored; return; end if;
         if Value.Status /= AML_Execute.Object_Returned then Report.Outcome := Wrong_Result_Type; return; end if;
         Core.Describe_Result (Service, Root, Description, Report.Access_Result);
         if Report.Access_Result /= V.Available then Report.Outcome := Evaluation_Failed; return; end if;
         if Description.Kind /= V.Package_Description then Report.Outcome := Wrong_Result_Type; return; end if;
         Report.Package_Inspected := True; Report.Elements := Description.Length;
         if Report.Elements < Required_Elements then Report.Outcome := Missing_Elements; return; end if;
         Report.Ignored_Elements := Report.Elements - Required_Elements;
         Read_Field (First_Field, Field_A, OK);
         Drop (Element, Element_Tried);
         if not OK or else Cleanup_Error then return; end if;
         if Registers = PM1A_And_B then
            Read_Field (Second_Field, Field_B, OK);
            Drop (Element, Element_Tried);
            if not OK or else Cleanup_Error then return; end if;
         end if;
         Report.Outcome := Types_Available;
      end Run;
   begin
      Report := (Kind => Kind, Registers => Registers, others => <>);
      Run;
      Drop (Element, Element_Tried);
      Drop (Root, Root_Tried);
      if Cleanup_Error then Report.Outcome := Cleanup_Failed;
      elsif Core.Retained_Results (Service) /= Before_Roots then Report.Outcome := Provider_Contract_Error;
      end if;
      if Report.Outcome /= Types_Available then
         Report.A_Available := False; Report.B_Available := False;
         Report.Type_A := 0; Report.Type_B := 0;
      end if;
   end Query;
end ACPI_Sleep_Objects;
