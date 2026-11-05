pragma Ada_2022;
with AML_Names;
package body ACPI_HID_I2C_Discovery with SPARK_Mode is
   package V renames Core.Values;
   package N renames Core.Namespace;
   use type V.Value_Handle;
   use type V.Access_Status;
   use type N.Object_Kind;
   use type N.Lookup_Status;
   use type AML_Execute.Execution_Status;
   use type Queries.Identification_Outcome;
   use type Queries.Status_Outcome;
   use type Queries.Query_Outcome;
   use type Reader.Copy_Status;
   use type Resources.Analysis_Outcome;
   use type Resources.R.Byte_Count;
   CRS_Member : constant AML_Names.Segment := "_CRS";
   function Observe (Run : Session) return Report is (Run.Info);
   function Pending (Run : Session) return Boolean is (Run.Handle /= V.No_Value);
   procedure Drop (Run : in out Session; Service : aliased in out Core.State) is
   begin
      if Run.Handle = V.No_Value then return; end if;
      if Run.Release_Tried then Run.Info.Result := Cleanup_Failed; return; end if;
      Run.Release_Tried := True;
      Core.Release_Result (Service, Run.Handle, Run.Info.Cleanup_Result);
      if Run.Info.Cleanup_Result /= V.Available or else Run.Handle /= V.No_Value then
         Run.Info.Result := Cleanup_Failed;
      end if;
   end Drop;
   procedure Begin_Discovery
     (Run : in out Session; Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Budget : Natural;
      Action : out Action_Result;
      Maximum_Items : Resources.R.Item_Count := Resources.R.Default_Item_Limit) is
      Found : N.Lookup_Result;
      Value : V.Result;
      procedure Start is
      begin
         if not Core.Current_Node (Service, Device)
           or else V.Node_Kind (Device) /= N.Device_Object then
            Run.Info.Result := Invalid_Device; return;
         end if;
         Run.Info.Step := Identity;
         Queries.Identify (Service, Device, Budget, Run.Info.Candidate);
         if Run.Info.Candidate.Charged > Budget then
            Run.Info.Result := Provider_Contract_Error; return;
         end if;
         Run.Info.Charged := Run.Info.Candidate.Charged;
         if Run.Info.Candidate.Outcome /= Queries.Compatible then
            Run.Info.Result := (if Run.Info.Candidate.Outcome = Queries.Budget_Exhausted
              then Budget_Exhausted else Identity_Failed); return;
         end if;
         Run.Info.Step := Device_Status;
         Queries.Observe_Status (Service, Device, Budget - Run.Info.Charged, Run.Info.Status);
         if Run.Info.Status.Charged > Budget - Run.Info.Charged then
            Run.Info.Result := Provider_Contract_Error; return;
         end if;
         Run.Info.Charged := Run.Info.Charged + Run.Info.Status.Charged;
         if Run.Info.Status.Outcome /= Queries.Status_Available then
            Run.Info.Result := (if Run.Info.Status.Outcome = Queries.Budget_Exhausted
              then Budget_Exhausted else Status_Failed); return;
         end if;
         Run.Info.Step := Current_Resources;
         if Run.Info.Charged = Budget then Run.Info.Result := Budget_Exhausted; return; end if;
         Core.Find_Object (Service, V.Position (Device), CRS_Member, Found, Run.Info.Access_Result);
         if Run.Info.Access_Result /= V.Available then Run.Info.Result := Lookup_Failed; return; end if;
         if Found.Status = N.Not_Found then Run.Info.Result := Missing_Resources; return; end if;
         if Found.Status /= N.Found then Run.Info.Result := Lookup_Failed; return; end if;
         Core.Describe_Node (Service, Found.Node, Run.Resource_Node, Run.Info.Access_Result);
         if Run.Info.Access_Result /= V.Available then Run.Info.Result := Lookup_Failed; return; end if;
         Evaluate_Named (Service, Run.Resource_Node, Budget - Run.Info.Charged, Value, Run.Info.Access_Result);
         Run.Info.Execution := Value.Status; Run.Info.Resource_Charge := Value.Charged;
         if Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
           AML_Execute.Metadata_Returned then Run.Handle := Value.Handle; end if;
         if Value.Charged > Budget - Run.Info.Charged then
            Run.Info.Result := Provider_Contract_Error; return;
         end if;
         Run.Info.Charged := Run.Info.Charged + Value.Charged;
         if not Core.Current_Node (Service, Device)
           or else not Core.Current_Node (Service, Run.Resource_Node) then
            Run.Info.Result := Identity_Changed; return;
         end if;
         if Run.Info.Access_Result /= V.Available then Run.Info.Result := Evaluation_Failed; return; end if;
         if Value.Status = AML_Execute.Budget_Exceeded then Run.Info.Result := Budget_Exhausted; return; end if;
         if Value.Status /= AML_Execute.Object_Returned then
            Run.Info.Result := (if Value.Status in AML_Execute.Returned | AML_Execute.Reference_Returned |
              AML_Execute.Metadata_Returned | AML_Execute.No_Return
              then Wrong_Result_Type else Evaluation_Failed); return;
         end if;
         if Run.Handle = V.No_Value then Run.Info.Result := Provider_Contract_Error; return; end if;
         Run.Info.Result := Awaiting_Buffer;
      end Start;
   begin
      if Pending (Run) then Action := Busy; return; end if;
      Action := Accepted;
      Run.Info := (others => <>); Run.Device := Device;
      Run.Resource_Node := V.No_Node; Run.Budget := Budget;
      Run.Maximum_Items := Maximum_Items; Run.Release_Tried := False;
      Start;
      if Run.Info.Result /= Awaiting_Buffer then Drop (Run, Service); end if;
   end Begin_Discovery;
   procedure Copy_And_Finish
     (Run : in out Session; Service : aliased in out Core.State;
      Data : out Firmware_Tables.Bytes; Action : out Action_Result) is
   begin
      Data := [others => 0];
      if not Pending (Run) or else Run.Info.Result /= Awaiting_Buffer then
         Action := Not_Pending; return;
      end if;
      if not Core.Current_Node (Service, Run.Device)
        or else not Core.Current_Node (Service, Run.Resource_Node) then
         Action := Wrong_Context; return;
      end if;
      Action := Accepted;
      Run.Info.Step := Resource_Copy;
      Reader.Copy_Template (Service, Run.Handle, Data, Run.Info.Copy, Run.Maximum_Items);
      if Run.Info.Copy.Status = Reader.Insufficient_Capacity then return; end if;
      if Run.Info.Copy.Status /= Reader.Copied_Template then
         Run.Info.Result := Copy_Failed; Drop (Run, Service); return;
      end if;
      Run.Info.Step := Resource_Analysis;
      if Run.Info.Copy.Copied = 0 then
         Run.Info.Result := Analysis_Failed; Drop (Run, Service); return;
      end if;
      Resources.Analyze (Data (Data'First .. Data'First + (Natural (Run.Info.Copy.Copied) - 1)),
        Run.Info.Analysis, Run.Maximum_Items);
      Run.Info.Result := (if Run.Info.Analysis.Outcome = Resources.Connections_Found
        then Not_Attempted else Analysis_Failed);
      Drop (Run, Service);
      if Run.Info.Result /= Not_Attempted then return; end if;
      Run.Info.Step := Descriptor_Query;
      Queries.Query_Descriptor (Service, Run.Device, Run.Budget - Run.Info.Charged, Run.Info.Descriptor);
      if Run.Info.Descriptor.Charged > Run.Budget - Run.Info.Charged then
         Run.Info.Result := Provider_Contract_Error; return;
      end if;
      Run.Info.Charged := Run.Info.Charged + Run.Info.Descriptor.Charged;
      if Run.Info.Descriptor.Outcome /= Queries.Descriptor_Available then
         Run.Info.Result := (if Run.Info.Descriptor.Outcome = Queries.Budget_Exhausted
           then Budget_Exhausted else Descriptor_Failed); return;
      end if;
      Run.Info.Step := Finished; Run.Info.Result := Observed;
   end Copy_And_Finish;
   procedure Cancel
     (Run : in out Session; Service : aliased in out Core.State;
      Action : out Action_Result) is
   begin
      if not Pending (Run) then Action := Not_Pending; return; end if;
      if not Core.Current_Node (Service, Run.Device)
        or else not Core.Current_Node (Service, Run.Resource_Node) then
         Action := Wrong_Context; return;
      end if;
      Action := Accepted;
      Run.Info.Result := Cancelled;
      Drop (Run, Service);
   end Cancel;
end ACPI_HID_I2C_Discovery;
