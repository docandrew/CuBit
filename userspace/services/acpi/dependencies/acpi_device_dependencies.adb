pragma Ada_2022;
with AML_Names;
package body ACPI_Device_Dependencies with SPARK_Mode is
   package V renames Core.Values;
   package N renames Core.Namespace;
   use type V.Access_Status, V.Value_Handle, V.Value_Kind;
   use type N.Object_Kind, N.Lookup_Status;
   use type AML_Execute.Execution_Status, Reader.Outcome;
   Dependency_Member : constant AML_Names.Segment := "_DEP";
   function Pending (Run : Session) return Boolean is (Run.Handle /= V.No_Value);
   function Observe (Run : Session) return Report is (Run.Info);
   procedure Release (Run : in out Session; Service : in out Core.State) is
   begin
      if not Pending (Run) then Run.Can_Copy := False; return; end if;
      Core.Release_Result (Service, Run.Handle, Run.Info.Cleanup_Result);
      if Run.Info.Cleanup_Result /= V.Available or else Run.Handle /= V.No_Value then
         Run.Info.Status := Cleanup_Failed;
      end if;
      Run.Can_Copy := False;
   end Release;
   procedure Begin_Observation
     (Run : in out Session; Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Budget : Natural; Action : out Action_Result) is
      Found : N.Lookup_Result;
      Value : V.Result;
      Shape : V.Value_Description;
      procedure Start is
      begin
         if not Core.Current_Node (Service, Device) or else V.Node_Kind (Device) /= N.Device_Object then
            Run.Info.Status := Invalid_Device; return;
         end if;
         if Budget = 0 then Run.Info.Status := Budget_Exhausted; Run.Info.Execution := AML_Execute.Budget_Exceeded; return; end if;
         Core.Find_Object (Service, V.Position (Device), Dependency_Member, Found, Run.Info.Access_Result);
         if Run.Info.Access_Result /= V.Available then Run.Info.Status := Lookup_Failed; return; end if;
         if Found.Status = N.Not_Found then Run.Info.Status := Missing; return; end if;
         if Found.Status /= N.Found then Run.Info.Status := Lookup_Failed; return; end if;
         Core.Describe_Node (Service, Found.Node, Run.Member, Run.Info.Access_Result);
         if Run.Info.Access_Result /= V.Available then Run.Info.Status := Lookup_Failed; return; end if;
         Evaluate_Named (Service, Run.Member, Budget, Value, Run.Info.Access_Result);
         Run.Info.Reported_Charge := Value.Charged; Run.Info.Execution := Value.Status;
         if Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned then Run.Handle := Value.Handle; end if;
         if Value.Charged > Budget then Run.Info.Status := Provider_Contract_Error; return; end if;
         Run.Info.Charged := Value.Charged;
         if not Core.Current_Node (Service, Device) or else not Core.Current_Node (Service, Run.Member) then Run.Info.Status := Identity_Changed; return; end if;
         if Run.Info.Access_Result /= V.Available then Run.Info.Status := Evaluation_Failed; return; end if;
         if Value.Status = AML_Execute.Budget_Exceeded then Run.Info.Status := Budget_Exhausted; return; end if;
         if Value.Status /= AML_Execute.Object_Returned then
            Run.Info.Status := (if Value.Status in AML_Execute.Returned | AML_Execute.Reference_Returned |
              AML_Execute.Metadata_Returned | AML_Execute.No_Return then Wrong_Result_Type else Evaluation_Failed);
            return;
         end if;
         if Run.Handle = V.No_Value then Run.Info.Status := Provider_Contract_Error; return; end if;
         Core.Describe_Result (Service, Run.Handle, Shape, Run.Info.Access_Result);
         if Run.Info.Access_Result /= V.Available then Run.Info.Status := Evaluation_Failed; return; end if;
         if Shape.Kind /= V.Package_Description then Run.Info.Status := Wrong_Result_Type; return; end if;
         Run.Info.Required := Reader.Element_Count (Shape.Length);
         Run.Info.Status := Retained; Run.Can_Copy := True;
      end Start;
   begin
      if Pending (Run) then Action := Busy; return; end if;
      Action := Accepted; Run.Info := (others => <>); Run.Device := Device;
      Run.Member := V.No_Node; Run.Can_Copy := False;
      Start;
      if Run.Info.Status /= Retained then Release (Run, Service); end if;
   end Begin_Observation;
   procedure Copy
     (Run : in out Session; Service : in out Core.State;
      Maximum_Elements : Reader.Element_Count; Items : out Reader.Device_Array;
      Result : out Reader.Report; Action : out Action_Result) is
   begin
      Items := [others => V.No_Node]; Result := (others => <>);
      if not Pending (Run) or else not Run.Can_Copy then Action := Not_Pending; return; end if;
      if not Core.Current_Node (Service, Run.Device) or else not Core.Current_Node (Service, Run.Member) then Action := Wrong_Context; return; end if;
      Action := Accepted;
      Reader.Copy (Service, Run.Handle, Maximum_Elements, Items, Result);
      Run.Info.Last_Copy := Result;
      if Result.Status = Reader.Cleanup_Failed then Run.Info.Status := Cleanup_Failed; Run.Can_Copy := False; end if;
   end Copy;
   procedure Close (Run : in out Session; Service : in out Core.State; Action : out Action_Result) is
   begin
      if not Pending (Run) then Action := Not_Pending; return; end if;
      if not Core.Current_Binding (Service, Run.Device) or else not Core.Current_Binding (Service, Run.Member) then Action := Wrong_Context; return; end if;
      Action := Accepted; Release (Run, Service);
      if not Pending (Run) then Run.Info.Status := Closed; end if;
   end Close;
end ACPI_Device_Dependencies;
