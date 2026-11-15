pragma Ada_2022;
with AML_Decode;
package body ACPI_GPIO_Bootstrap_Discovery with SPARK_Mode is
   package V renames Core.Values;package N renames Core.Namespace;
   use type V.Access_Status,V.Value_Handle,V.Value_Kind,N.Object_Kind,N.Lookup_Status;
   use type Core.Node_Descriptor,AML_Execute.Execution_Status;
   use type Observation.Outcome,Observation.Action_Result;
   function Observe(Run:Session) return Report is (Run.Info);
   function Consumed(Run:Session) return Boolean is (Run.Used);
   function Pending(Run:Session) return Boolean is
     (Run.Working or else Run.Handle/=V.No_Value or else Observation.Pending(Run.Selected));
   function Description(Run:Session) return Observation.Metadata is
     (if Run.Info.Status=Described and then not Pending(Run) then Observation.Description(Run.Selected) else (others=><>));
   function Owns(Run:Session;Service:Core.State) return Boolean is (Core.Current_Binding(Service,Run.Root));
   function Current(Run:Session;Service:Core.State) return Boolean is
      Found:N.Lookup_Result;Member:Core.Node_Descriptor;Status:V.Access_Status;
   begin
      if not Owns(Run,Service) or else Core.Namespace_Node_Count(Service)/=Run.Last_Node then return False;end if;
      if Run.Info.Candidates=0 then return True;end if;
      if not Core.Current_Node(Service,Run.Candidate) or else not Core.Current_Node(Service,Run.Identity) then return False;end if;
      Core.Find_Object(Service,V.Position(Run.Candidate),"_HID",Found,Status);
      if Status/=V.Available or else Found.Status/=N.Found then return False;end if;
      Core.Describe_Node(Service,Found.Node,Member,Status);
      return Status=V.Available and then Member=Run.Identity;
   end Current;
   function Is_Current(Run:Session;Service:Core.State) return Boolean is
     (Run.Info.Status=Described and then Current(Run,Service) and then Observation.Is_Current(Run.Selected,Service));
   procedure Release_Identity(Run:in out Session;Service:in out Core.State) is
   begin
      if Run.Handle=V.No_Value then return;end if;
      if not Owns(Run,Service) then Run.Info.Status:=Cleanup_Failed;return;end if;
      Release_Result(Service,Run.Handle,Run.Info.Cleanup_Result);
      if Run.Info.Cleanup_Result/=V.Available or else Run.Handle/=V.No_Value
        or else Core.Retained_Results(Service)/=0 then Run.Info.Status:=Cleanup_Failed;end if;
   end Release_Identity;
   procedure Identify(Run:in out Session;Service:aliased in out Core.State;
      Device:Core.Node_Descriptor;Matched,Good:out Boolean;Member:out Core.Node_Descriptor) is
      Found:N.Lookup_Result;Value:V.Result;Shape:V.Value_Description;
      Bytes:AML_Decode.Bytes(1..Observation.Hardware_Identifier'Length);Copied:Natural;
      Text:Observation.Hardware_Identifier;
   begin
      Matched:=False;Good:=False;Member:=V.No_Node;
      Core.Find_Object(Service,V.Position(Device),"_HID",Found,Run.Info.Access_Result);
      if Run.Info.Access_Result/=V.Available or else Found.Status not in N.Found|N.Not_Found then Run.Info.Status:=Lookup_Failed;return;end if;
      if Found.Status=N.Not_Found then Good:=True;return;end if;
      Core.Describe_Node(Service,Found.Node,Member,Run.Info.Access_Result);
      if Run.Info.Access_Result/=V.Available then Run.Info.Status:=Lookup_Failed;return;end if;
      if Run.Info.Remaining=0 then Run.Info.Status:=Budget_Exhausted;return;end if;
      Run.Info.Probes:=Run.Info.Probes+1;
      Evaluate_Named(Service,Member,Run.Info.Remaining,Value,Run.Info.Access_Result);
      Run.Info.Last_Reported_Charge:=Value.Charged;Run.Info.Execution:=Value.Status;
      Run.Info.Field_Uncertainty:=Run.Info.Field_Uncertainty or Value.Status=AML_Execute.Field_Indeterminate;
      if Value.Status=AML_Execute.Fatal_Requested then Run.Info.Fatal:=(Present=>True,Item=>Value.Fatal);end if;
      if Value.Status in AML_Execute.Object_Returned|AML_Execute.Reference_Returned|AML_Execute.Metadata_Returned then
         Run.Handle:=Value.Handle;
         if Run.Handle=V.No_Value then Run.Info.Status:=Provider_Contract_Error;return;end if;
      end if;
      if Value.Charged>Run.Info.Remaining or else Core.Retained_Results(Service)/=(if Run.Handle=V.No_Value then 0 else 1) then Run.Info.Status:=Provider_Contract_Error;return;end if;
      Run.Info.Remaining:=Run.Info.Remaining-Value.Charged;Run.Info.Charged:=Run.Info.Initial_Budget-Run.Info.Remaining;
      if not Hardware_Quiet(Service) then Run.Info.Status:=Hardware_Not_Quiet;return;
      elsif not Current(Run,Service) or else not Core.Current_Node(Service,Device) or else not Core.Current_Node(Service,Member) then Run.Info.Status:=Identity_Changed;return;
      elsif Value.Status=AML_Execute.Budget_Exceeded then Run.Info.Status:=Budget_Exhausted;return;
      elsif Run.Info.Access_Result/=V.Available then Run.Info.Status:=Identity_Evaluation_Failed;return;
      elsif Value.Status=AML_Execute.Returned then Good:=True;return;
      elsif Value.Status/=AML_Execute.Object_Returned then Run.Info.Status:=Identity_Evaluation_Failed;return;end if;
      Core.Describe_Result(Service,Run.Handle,Shape,Run.Info.Access_Result);
      if Run.Info.Access_Result/=V.Available then Run.Info.Status:=Identity_Evaluation_Failed;return;end if;
      if Shape.Kind=V.Integer_Description then Good:=True;return;end if;
      if Shape.Kind/=V.String_Description then Run.Info.Status:=Wrong_Identity_Type;return;end if;
      Good:=True;if Shape.Length/=Bytes'Length then return;end if;
      Core.Read_Result_Bytes(Service,Run.Handle,0,Bytes,Copied,Run.Info.Access_Result);
      if Run.Info.Access_Result/=V.Available or else Copied/=Bytes'Length then Good:=False;Run.Info.Status:=Identity_Evaluation_Failed;return;end if;
      for I in Bytes'Range loop Text(I):=Character'Val(Bytes(I));end loop;
      Matched:=Text in "INTC1057"|"INTC1055";
   end Identify;
   procedure Finish_Observation(Run:in out Session;Service:Core.State) is
   begin
      Run.Info.Observation_Result:=Observation.Observe(Run.Selected);
      Run.Info.Field_Uncertainty:=Run.Info.Field_Uncertainty or Run.Info.Observation_Result.Field_Uncertainty;
      if Run.Info.Observation_Result.Fatal.Present then Run.Info.Fatal:=Run.Info.Observation_Result.Fatal;end if;
      case Run.Info.Observation_Result.Status is
         when Observation.Described=>
            if Current(Run,Service) and then Hardware_Quiet(Service) and then not Observation.Pending(Run.Selected)
              and then Core.Retained_Results(Service)=0 then Run.Info.Status:=Described;
            else Run.Info.Status:=Identity_Changed;end if;
         when Observation.Capacity_Required=>Run.Info.Status:=Capacity_Required;
         when Observation.Budget_Exhausted=>Run.Info.Status:=Budget_Exhausted;
         when Observation.Cleanup_Failed=>Run.Info.Status:=Cleanup_Failed;
         when Observation.Provider_Contract_Error=>Run.Info.Status:=Provider_Contract_Error;
         when others=>Run.Info.Status:=Observation_Failed;
      end case;
   end Finish_Observation;
   procedure Discover(Run:in out Session;Service:aliased in out Core.State;
      Budget:Natural;Data:out Firmware_Tables.Bytes;Action:out Action_Result) is
      Node,Member:Core.Node_Descriptor;Matched,Good:Boolean;Work:Observation.Action_Result;
      procedure Scan is
      begin
         if not Hardware_Quiet(Service) or else Core.Retained_Results(Service)/=0 then Run.Info.Status:=Hardware_Not_Quiet;return;end if;
         Core.Describe_Node(Service,N.Root,Run.Root,Run.Info.Access_Result);
         if Run.Info.Access_Result/=V.Available then Run.Info.Status:=Lookup_Failed;return;end if;
         Run.Last_Node:=Core.Namespace_Node_Count(Service);
         for Position in N.Node_ID range N.Root..Run.Last_Node loop
            if Run.Info.Remaining=0 then Run.Info.Status:=Budget_Exhausted;return;end if;
            if not Current(Run,Service) then Run.Info.Status:=Identity_Changed;return;end if;
            Core.Describe_Node(Service,Position,Node,Run.Info.Access_Result);
            if Run.Info.Access_Result=V.Available and then V.Node_Kind(Node)=N.Device_Object then
               Run.Info.At_Device:=Node;Identify(Run,Service,Node,Matched,Good,Member);
               Release_Identity(Run,Service);if Run.Info.Status=Cleanup_Failed then return;end if;
               if not Good then return;end if;
               if Matched then
                  if Run.Info.Candidates=1 then Run.Info.Candidates:=2;Run.Info.Status:=Multiple_Observed_Candidates;return;end if;
                  Run.Info.Candidates:=1;Run.Candidate:=Node;Run.Identity:=Member;
               end if;
            elsif Run.Info.Access_Result not in V.Available|V.Invalid_Value then Run.Info.Status:=Lookup_Failed;return;end if;
         end loop;
         if Run.Info.Candidates=0 then Run.Info.Status:=No_Supported_Identity_Observed;return;end if;
         if not Current(Run,Service) then Run.Info.Status:=Identity_Changed;return;end if;
         Observation.Describe(Run.Selected,Service,Run.Candidate,Run.Info.Remaining,Data,Work);
         Run.Info.Observation_Result:=Observation.Observe(Run.Selected);
         if Work/=Observation.Accepted or else Run.Info.Observation_Result.Charged>Run.Info.Remaining then Run.Info.Status:=Provider_Contract_Error;return;end if;
         Run.Info.Remaining:=Run.Info.Remaining-Run.Info.Observation_Result.Charged;Run.Info.Charged:=Run.Info.Initial_Budget-Run.Info.Remaining;
         Finish_Observation(Run,Service);
      end Scan;
   begin
      Data:=[others=>0];Action:=Already_Consumed;if Run.Used then return;end if;
      Run.Used:=True;Run.Working:=True;Action:=Accepted;Run.Info.Initial_Budget:=Budget;Run.Info.Remaining:=Budget;
      Scan;Run.Working:=False;
      if Run.Info.Status not in Described|Capacity_Required then Data:=[others=>0];end if;
   end Discover;
   procedure Copy_Resources(Run:in out Session;Service:in out Core.State;
      Data:out Firmware_Tables.Bytes;Action:out Action_Result) is
      Work:Observation.Action_Result;
   begin
      Data:=[others=>0];Action:=Busy;if Run.Working then return;end if;
      Action:=Not_Awaiting_Capacity;if Run.Info.Status/=Capacity_Required then return;end if;
      if not Owns(Run,Service) then Action:=Wrong_Context;return;end if;
      Action:=Accepted;Run.Working:=True;
      Observation.Copy_Resources(Run.Selected,Service,Data,Work);Finish_Observation(Run,Service);
      if Work/=Observation.Accepted then Run.Info.Status:=Provider_Contract_Error;end if;
      Run.Working:=False;if Run.Info.Status/=Described then Data:=[others=>0];end if;
   end Copy_Resources;
   procedure Close(Run:in out Session;Service:in out Core.State;Action:out Action_Result) is
      Work:Observation.Action_Result;
   begin
      Action:=Busy;if Run.Working then return;end if;
      Action:=No_Pending_Result;if not Pending(Run) then return;end if;
      if not Owns(Run,Service) then Action:=Wrong_Context;return;end if;
      Action:=Accepted;Release_Identity(Run,Service);
      if Observation.Pending(Run.Selected) then Observation.Close(Run.Selected,Service,Work);Run.Info.Observation_Result:=Observation.Observe(Run.Selected);end if;
      if Pending(Run) or else Core.Retained_Results(Service)/=0 or else Run.Info.Cleanup_Result/=V.Available
        or else Run.Info.Observation_Result.Cleanup_Result/=V.Available then Run.Info.Status:=Cleanup_Failed;
      else Run.Info.Status:=Closed;end if;
   end Close;
end ACPI_GPIO_Bootstrap_Discovery;
