pragma Ada_2022;
package body ACPI_Device_Initialization with SPARK_Mode is
   package N renames Core.Namespace;
   package V renames Core.Values;
   use type N.Object_Kind;
   use type N.Lookup_Status;
   use type V.Access_Status;
   use type V.Value_Handle;
   use type AML_Execute.Execution_Status;
   use type ACPI_Device_Policy.Region_Outcome;
   use type ACPI_Device_Policy.Status_Decision;
   function Observe (Run : Session) return Report is (Run.Info);
   function Work_Limit (Run : Session) return Natural is (Run.Budget);
   function Startup_Claimed (Run : Session) return Boolean is
     (Run.Claimed or else Run.Info.Stage /= Not_Started);
   procedure Claim_Startup (Run : in out Session; Accepted : out Boolean) is
   begin
      Accepted := not Startup_Claimed (Run);
      Run.Claimed := True;
   end Claim_Startup;
   function Observe_Node (Run : Session; Node : Node_ID) return Node_Report is (Run.Items (Node));
   function Device_Kind (Kind : N.Object_Kind) return Boolean;
   function Current_Topology (Run : Session; Service : Core.State) return Boolean;
   function Initialization_Path
     (Run : Session; Service : Core.State; Item : Core.Node_Descriptor) return Path_Outcome is
      Cursor, Parent : Node_ID;
      Found : N.Lookup_Result;
      Status : V.Access_Status;
      Known_Errors : Boolean := False;
   begin
      if Run.Info.Stage = Not_Started then return History_Unknown; end if;
      if Run.Info.Stage not in Completed | Completed_With_Errors then return Initialization_Not_Complete; end if;
      if not Current_Topology (Run, Service) or else not Core.Current_Node (Service, Item) then
         return Stale_History;
      end if;
      Cursor := V.Position (Item);
      if Cursor > Run.Last or else not Run.Live (Cursor) then return Stale_History; end if;
      -- Validate the COMPLETE path before deciding whether an initializer
      -- was missed. This distinguishes a pruned ancestor from a failed INI.
      loop
         if not Run.Live (Cursor) or else not Core.Current_Node (Service, Run.Nodes (Cursor)) then
            return Stale_History;
         end if;
         if Run.Items (Cursor).Status_Evaluation in Missing | Succeeded
           and then Run.Items (Cursor).Decision = ACPI_Device_Policy.Prune_Subtree then
            return Pruned_Path;
         end if;
         exit when Cursor = N.Root;
         Parent := V.Parent_Position (Run.Nodes (Cursor));
         if Parent >= Cursor then return Stale_History; end if;
         Cursor := Parent;
      end loop;
      Cursor := V.Position (Item);
      loop
         Known_Errors := Known_Errors
           or else Run.Items (Cursor).Status_Evaluation in Evaluation_Failed | Wrong_Result_Type
           or else Run.Items (Cursor).Initialization in Evaluation_Failed | Wrong_Result_Type;
         -- Only device/root/system-bus initializers belong to this walk.
         -- Functioning-only ANCESTORS intentionally skip their own INI;
         -- the candidate's own declared INI must have completed.
         if (Device_Kind (V.Node_Kind (Run.Nodes (Cursor))) or else Cursor = N.Root
           or else Cursor = Run.System_Bus)
           and then (Cursor = V.Position (Item)
             or else Run.Items (Cursor).Decision = ACPI_Device_Policy.Initialize_And_Descend
             or else Cursor = N.Root or else Cursor = Run.System_Bus) then
            Core.Find_Object (Service, Cursor, "_INI", Found, Status);
            if Status /= V.Available then return Stale_History; end if;
            if Found.Status = N.Found and then Run.Items (Cursor).Initialization /= Succeeded then
               if Run.Items (Cursor).Initialization in Evaluation_Failed | Wrong_Result_Type
                 or else Run.Items (Cursor).Status_Evaluation in Evaluation_Failed | Wrong_Result_Type then
                  Known_Errors := True;
               else return Initialization_Not_Complete;
               end if;
            elsif Found.Status not in N.Found | N.Not_Found then return Stale_History;
            end if;
         end if;
         exit when Cursor = N.Root;
         Parent := V.Parent_Position (Run.Nodes (Cursor));
         if Parent >= Cursor then return Stale_History; end if;
         Cursor := Parent;
      end loop;
      return (if Known_Errors then Initialization_With_Errors else Initialization_Ready);
   end Initialization_Path;
   function Device_Kind (Kind : N.Object_Kind) return Boolean is
     (Kind in N.Device_Object | N.Processor_Object | N.Thermal_Zone_Object);
   procedure Increment (Value : in out Natural) is
   begin if Value < Natural'Last then Value := Value + 1; end if; end Increment;
   function Captured_Errors (Run : Session) return Error_Count is (Run.Error_Used);
   function Omitted_Errors (Run : Session) return Natural is
     (Run.Info.Errors - Natural (Run.Error_Used));
   function Error_At (Run : Session; Position : Error_Position) return Error_Report is
     (Run.Saved_Errors (Position));
   procedure Error (Run : in out Session; Node : Node_ID;
      Status : AML_Execute.Execution_Status;
      Member : AML_Names.Segment := "____";
      Method : Node_ID := N.Root;
      Access_Result : V.Access_Status := V.Wrong_Phase;
      Evaluation : AML_Execute.Execution_Status := AML_Execute.No_Return;
      Reported_Charge : Natural := 0) is
   begin
      if Run.Info.Errors = 0 then
         Run.Info.First_Error_Node := Node; Run.Info.First_Error := Status;
      end if;
      if Run.Error_Used < Error_Count'Last then
         Run.Error_Used := Run.Error_Used + 1;
         Run.Saved_Errors (Run.Error_Used) :=
           (Scope => Node, Method => Method, Member => Member, Failure => Status,
            Access_Result => Access_Result, Evaluation => Evaluation,
            Reported_Charge => Reported_Charge);
      end if;
      Increment (Run.Info.Errors);
   end Error;
   procedure Stop_Field_Indeterminate
     (Run : in out Session; Node : Core.Namespace.Node_ID) is
   begin
      Run.Info.Stage := Failed;
      Error (Run, Node, AML_Execute.Field_Indeterminate);
   end Stop_Field_Indeterminate;
   function Structural (Kind : N.Object_Kind) return Boolean is
     (Kind in N.Scope_Object | N.Device_Object | N.Processor_Object |
       N.Thermal_Zone_Object | N.Power_Resource_Object);
   function Current_Topology (Run : Session; Service : Core.State) return Boolean is
      Item : Core.Node_Descriptor;
      Status : V.Access_Status;
      Last : constant Node_ID := Core.Namespace_Node_Count (Service);
   begin
      Core.Describe_Node (Service, N.Root, Item, Status);
      if Status /= V.Available then return False; end if;
      for I in Node_ID loop
         if Run.Live (I) then
            if not Core.Current_Binding (Service, Run.Nodes (I)) then return False; end if;
            Core.Describe_Node (Service, I, Item, Status);
            if Status /= V.Available then return False; end if;
            if (Structural (V.Node_Kind (Item)) or else Structural (V.Node_Kind (Run.Nodes (I))))
              and then V.Node_Kind (Item) /= V.Node_Kind (Run.Nodes (I))
            then return False; end if;
         elsif I <= Last then
            Core.Describe_Node (Service, I, Item, Status);
            if Status = V.Available then return False; end if;
         end if;
      end loop;
      return True;
   end Current_Topology;
   procedure Evaluate (Run : in out Session; Service : aliased in out Core.State;
      Scope : Node_ID; Part : AML_Names.Segment; Integer_Required : Boolean;
      Step : out Step_Result; Number : out AML_Decode.Integer_Value) is
      Found : N.Lookup_Result;
      Access_Result : V.Access_Status := V.Wrong_Phase;
      Released : V.Access_Status;
      Description : Core.Node_Descriptor := V.No_Node;
      Value : V.Result;
      Handle : V.Value_Handle;
      Is_Method : Boolean;
      Before_Roots : constant Natural := Core.Retained_Results (Service);
      Raw_Execution : AML_Execute.Execution_Status := AML_Execute.No_Return;
      Raw_Charge : Natural := 0;
      procedure Fail (Why : AML_Execute.Execution_Status) is
      begin
         Error (Run, Scope, Why, Part, V.Position (Description), Access_Result,
           Raw_Execution, Raw_Charge);
      end Fail;
   begin
      Step := Evaluation_Failed; Number := 0;
      if not Current_Topology (Run, Service) then
         Step := Identity_Changed; Run.Info.Stage := Failed;
         Fail (AML_Execute.Unsupported_Value); return;
      end if;
      Core.Find_Object (Service, Scope, Part, Found, Access_Result);
      if Access_Result /= V.Available then
         Fail (AML_Execute.Unsupported_Value); return;
      elsif Found.Status = N.Not_Found then Step := Missing; return;
      elsif Found.Status /= N.Found then
         Fail (AML_Execute.Unsupported_Value); return;
      end if;
      Core.Describe_Node (Service, Found.Node, Description, Access_Result);
      if Access_Result /= V.Available then
         Fail (AML_Execute.Unsupported_Value); return;
      end if;
      Is_Method := V.Node_Kind (Description) = N.Method_Object;
      Evaluate_Named (Service, Description,
        Run.Budget - Run.Info.Charged, Value, Access_Result);
      Raw_Execution := Value.Status; Raw_Charge := Value.Charged;
      if Is_Method and then not Integer_Required then
         Note_Initializer (Service, Scope, Description, Access_Result, Value.Status, Value.Charged);
      end if;
      if Value.Charged > Run.Budget - Run.Info.Charged then
         Run.Info.Stage := Failed;
         Fail (AML_Execute.Budget_Exceeded); return;
      end if;
      Run.Info.Charged := Run.Info.Charged + Value.Charged;
      if Is_Method then
         if Integer_Required then Increment (Run.Info.Status_Methods);
         else Increment (Run.Info.Initialization_Methods); end if;
      end if;
      if Value.Status = AML_Execute.Field_Indeterminate then
         Stop_Field_Indeterminate (Run, Scope); return;
      end if;
      if Access_Result in V.Busy | V.Wrong_Phase | V.Identity_Exhausted then
         Run.Info.Stage := Failed; Fail (AML_Execute.Unsupported_Value); return;
      end if;
      if Value.Status = AML_Execute.Fatal_Requested then
         if not Run.Info.Fatal.Present then
            Run.Info.Fatal := (Present => True, Item => Value.Fatal);
            Run.Info.Fatal_Initializer := Scope;
         end if;
         Run.Info.Stage := Failed; Fail (Value.Status); return;
      end if;
      -- A recoverable AML error must not hide a leaked or foreign root.
      if Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned then
         if Core.Retained_Results (Service) <= Before_Roots
           or else Core.Retained_Results (Service) - Before_Roots /= 1 then
            Run.Info.Stage := Failed; Fail (AML_Execute.Unsupported_Value); return;
         end if;
      elsif Core.Retained_Results (Service) /= Before_Roots then
         Run.Info.Stage := Failed; Fail (AML_Execute.Unsupported_Value); return;
      end if;
      if not Current_Topology (Run, Service) then
         Step := Identity_Changed; Run.Info.Stage := Failed;
         Fail (AML_Execute.Unsupported_Value); return;
      end if;
      if Access_Result /= V.Available then
         Fail (AML_Execute.Unsupported_Value); return;
      end if;
      if Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
        AML_Execute.Metadata_Returned
      then
         Handle := Value.Handle;
         Core.Release_Result (Service, Handle, Released);
         if Released /= V.Available or else Handle /= V.No_Value
           or else Core.Retained_Results (Service) /= Before_Roots then
            Run.Info.Stage := Failed; Fail (AML_Execute.Unsupported_Value); return;
         end if;
         if Integer_Required then
            Step := Wrong_Result_Type; Fail (AML_Execute.Unsupported_Value);
         else Step := Succeeded; end if;
      elsif Value.Status = AML_Execute.Returned then
         Number := Value.Number; Step := Succeeded;
      elsif not Integer_Required and then Value.Status = AML_Execute.No_Return then
         Step := Succeeded;
      else
         Fail (Value.Status);
         if Value.Status = AML_Execute.Budget_Exceeded then
            Step := Budget_Exhausted; Run.Info.Stage := Incomplete;
         end if;
      end if;
      if not Current_Topology (Run, Service) then
         Step := Identity_Changed; Run.Info.Stage := Failed;
         Fail (AML_Execute.Unsupported_Value);
      end if;
   end Evaluate;
   procedure Begin_Initialization (Run : in out Session;
      Service : aliased in out Core.State; Budget : Natural; Accepted : out Boolean) is
      Status : V.Access_Status;
      Parent, Cursor : Node_ID;
      Tail : Links := [others => N.Root];
      Found : N.Lookup_Result;
      Root_Description : Core.Node_Descriptor;
   begin
      Accepted := False;
      if Run.Info.Stage /= Not_Started then return; end if;
      Core.Describe_Node (Service, N.Root, Root_Description, Status);
      if Status /= V.Available then return; end if;
      Run.Info.Stage := Preparing; Run.Budget := Budget; Accepted := True;
      Run.Last := Core.Namespace_Node_Count (Service);
      for I in N.Root .. Run.Last loop
         Core.Describe_Node (Service, I, Run.Nodes (I), Status);
         Run.Live (I) := Status = V.Available;
         if Run.Live (I) and then I /= N.Root then
            Parent := V.Parent_Position (Run.Nodes (I));
            if Parent >= I or else not Run.Live (Parent) then
               Run.Info.Stage := Failed; Error (Run, I, AML_Execute.Unsupported_Value); return;
            end if;
            if Run.First_Child (Parent) = N.Root then Run.First_Child (Parent) := I;
            else Run.Next_Sibling (Tail (Parent)) := I; end if;
            Tail (Parent) := I;
         end if;
      end loop;
      if not Run.Live (N.Root) then
         Run.Info.Stage := Failed; Error (Run, N.Root, AML_Execute.Unsupported_Value); return; end if;
      -- Match ACPICA's prepass: only _INI names under device/processor/thermal
      -- mark ancestors. Root and _SB initializers are handled separately below.
      for I in Node_ID range 1 .. Run.Last loop
         if Run.Live (I) and then V.Node_Name (Run.Nodes (I)) = "_INI" then
            Parent := V.Parent_Position (Run.Nodes (I));
            if Device_Kind (V.Node_Kind (Run.Nodes (Parent))) then
               Cursor := Parent;
               loop
                  Run.Has_Ini (Cursor) := True;
                  exit when Cursor = N.Root;
                  Cursor := V.Parent_Position (Run.Nodes (Cursor));
               end loop;
            end if;
         end if;
      end loop;
      Core.Find_Object (Service, N.Root, "_SB_", Found, Status);
      if Status /= V.Available then
         Run.Info.Stage := Failed; Error (Run, N.Root, AML_Execute.Unsupported_Value); return;
      elsif Found.Status = N.Found then Run.System_Bus := Found.Node; end if;
      if Run.Info.Stage = Preparing then Run.Info.Stage := Awaiting_Regions; end if;
   end Begin_Initialization;
   procedure Complete_Initialization (Run : in out Session;
      Service : aliased in out Core.State; Accepted : out Boolean) is
      Charge : Natural;
      Node : Node_ID;
      Descend : Boolean;
      Ignored : AML_Decode.Integer_Value;
      Step : Step_Result;
      Number : AML_Decode.Integer_Value;
      Staged : Boolean := False;
      Scope_Entered : Boolean;
      Planned, Prepared : Boolean;
      procedure Enter_Scope (Scope : Node_ID; Entered : out Boolean) is
         Scope_Charge : Natural := 0;
         Outcome : ACPI_Device_Policy.Region_Outcome := ACPI_Device_Policy.Regions_Unavailable;
      begin
         Entered := False;
         if not Staged or else Run.Activated (Scope) then Entered := True; return; end if;
         Run.Activated (Scope) := True;
         -- Known ordinary AML errors complete this scope's ordering step.
         -- Fatal, uncertain, stale and overcharged evaluations already stopped
         -- the walk. An intentionally skipped inactive initializer stays false.
         After_Initializer (Service, Scope,
            Run.Items (Scope).Initialization in Missing | Succeeded | Evaluation_Failed | Wrong_Result_Type
              or else Run.Items (Scope).Status_Evaluation in Evaluation_Failed | Wrong_Result_Type,
            Run.Budget - Run.Info.Charged, Scope_Charge, Outcome);
         if Scope_Charge > Run.Budget - Run.Info.Charged then
            Run.Info.Stage := Failed; Error (Run, Scope, AML_Execute.Budget_Exceeded); return;
         end if;
         Run.Info.Charged := Run.Info.Charged + Scope_Charge;
         if Outcome not in ACPI_Device_Policy.Regions_Connected | ACPI_Device_Policy.No_Regions_Required then
            Run.Info.Region_Status := Outcome;
            Run.Info.Stage := (if Outcome = ACPI_Device_Policy.Regions_Unavailable then Incomplete else Failed);
            return;
         end if;
         Entered := True;
      end Enter_Scope;
      procedure Plan_Prerequisites (Planned : out Boolean) is
         Required, Classified : Boolean;
         Cursor : Node_ID;
      begin
         Planned := False;
         for I in N.Root .. Run.Last loop
            if Run.Live(I) and then Structural(V.Node_Kind(Run.Nodes(I))) then
               Required := False; Classified := False;
               Classify_Prerequisite(Service,I,Required,Classified);
               if not Classified then return; end if;
               if Required then
                  Cursor := I;
                  loop
                     Run.Prerequisites(Cursor) := True;
                     exit when Cursor=N.Root;
                     Cursor := V.Parent_Position(Run.Nodes(Cursor));
                  end loop;
               end if;
            end if;
         end loop;
         Run.Prerequisites(N.Root) := True;
         Run.Prerequisites(Run.System_Bus) := True;
         Planned := Current_Topology(Run,Service);
      end Plan_Prerequisites;
      procedure Finish_Region_Phase (Finished : out Boolean) is
      begin
         Finished := False;
         if Staged then
            Charge := 0; Run.Info.Region_Status := ACPI_Device_Policy.Regions_Unavailable;
            Finish_Regions(Service,Run.Budget-Run.Info.Charged,Charge,Run.Info.Region_Status);
            if Charge>Run.Budget-Run.Info.Charged then
               Run.Info.Stage:=Failed;Error(Run,N.Root,AML_Execute.Budget_Exceeded);return;
            end if;
            Run.Info.Charged:=Run.Info.Charged+Charge;
            if Run.Info.Region_Status not in ACPI_Device_Policy.Regions_Connected | ACPI_Device_Policy.No_Regions_Required then
               Run.Info.Stage:=(if Run.Info.Region_Status=ACPI_Device_Policy.Regions_Unavailable then Incomplete else Failed);return;
            end if;
            Staged:=False;
         end if;
         Finished:=Current_Topology(Run,Service);
         if not Finished then Run.Info.Stage:=Failed;Error(Run,N.Root,AML_Execute.Unsupported_Value);end if;
      end Finish_Region_Phase;
      procedure Walk (Prerequisites_Only : Boolean) is
         Process_Node : Boolean;
      begin
      Node := Run.First_Child (N.Root);
      while Node /= N.Root loop
         Descend := Run.Initialize_Subtree (V.Parent_Position (Run.Nodes (Node)));
         Process_Node := not Run.Walked(Node) and then
           (not Prerequisites_Only or else Run.Prerequisites(Node));
         if Run.Walked(Node) then Descend:=Run.Initialize_Subtree(Node);end if;
         if Process_Node then
         if Descend and then Device_Kind (V.Node_Kind (Run.Nodes (Node))) then
            if not Run.Has_Ini (Node) then Descend := False;
            else
               Evaluate (Run, Service, Node, "_STA", True, Step, Number);
               Run.Items (Node).Status_Evaluation := Step;
               if Run.Info.Stage /= Walking then return; end if;
               if Step = Missing then Number := ACPI_Device_Policy.Missing_Status_Default; end if;
               if Step in Missing | Succeeded then
                  Run.Items (Node).Flags := Number;
                  Run.Items (Node).Decision := ACPI_Device_Policy.Decide (Number);
                  Descend := Run.Items (Node).Decision /= ACPI_Device_Policy.Prune_Subtree;
                  if Run.Items (Node).Decision = ACPI_Device_Policy.Initialize_And_Descend
                    and then Node /= Run.System_Bus
                  then
                     Evaluate (Run, Service, Node, "_INI", False, Step, Ignored);
                     Run.Items (Node).Initialization := Step;
                     if Run.Info.Stage /= Walking then return; end if;
                  end if;
               end if;
               -- A failed _STA skips own _INI but continues children, matching
               -- the primary implementation. Report keeps the error visible.
            end if;
         end if;
         if Structural (V.Node_Kind (Run.Nodes (Node))) then
            Enter_Scope (Node, Scope_Entered); if not Scope_Entered then return; end if;
         end if;
         Run.Initialize_Subtree (Node) := Descend;Run.Walked(Node):=True;
         end if;
         if (Descend or else Staged or else Prerequisites_Only) and then Run.First_Child (Node) /= N.Root then
            Node := Run.First_Child (Node);
         else
            while Node /= N.Root and then Run.Next_Sibling (Node) = N.Root loop
               Node := V.Parent_Position (Run.Nodes (Node));
            end loop;
            if Node /= N.Root then Node := Run.Next_Sibling (Node); end if;
         end if;
      end loop;

      end Walk;
   begin
      Accepted := False;
      if Run.Info.Stage /= Awaiting_Regions then return; end if;
      Accepted := True;
      if not Current_Topology (Run, Service) then
         Run.Info.Stage := Failed; Error (Run, N.Root, AML_Execute.Unsupported_Value); return; end if;
      -- Consume before entering the trusted callback, preventing replay.
      Run.Info.Stage := Incomplete;
      Run.Info.Regions_Attempted := True;
      Initialize_Regions (Service, Run.Budget - Run.Info.Charged, Charge, Run.Info.Region_Status);
      if Charge > Run.Budget - Run.Info.Charged then
         Run.Info.Stage := Failed; Error (Run, N.Root, AML_Execute.Budget_Exceeded); return; end if;
      Run.Info.Charged := Run.Info.Charged + Charge;
      if Run.Info.Region_Status = ACPI_Device_Policy.Region_Initialization_Failed then
         Run.Info.Stage := Failed; return;
      elsif Run.Info.Region_Status = ACPI_Device_Policy.Regions_Unavailable then return;
      elsif not Current_Topology (Run, Service) then
         Run.Info.Stage := Failed; Error (Run, N.Root, AML_Execute.Unsupported_Value); return;
      end if;
      Staged := Run.Info.Region_Status = ACPI_Device_Policy.Regions_Prepared;
      Run.Info.Stage := Walking;
      if Consumer_Preparation_Enabled then
         Plan_Prerequisites(Planned);
         if not Planned then Run.Info.Stage:=Failed;return;end if;
      end if;
      Evaluate (Run, Service, N.Root, "_INI", False, Step, Ignored);
      Run.Items (N.Root).Initialization := Step;Run.Walked(N.Root):=True;
      if Run.Info.Stage /= Walking then return; end if;
      Enter_Scope (N.Root, Scope_Entered); if not Scope_Entered then return; end if;
      if Run.System_Bus /= N.Root then
         Evaluate (Run, Service, Run.System_Bus, "_INI", False, Step, Ignored);
         Run.Items (Run.System_Bus).Initialization := Step;Run.Walked(Run.System_Bus):=True;
         if Run.Info.Stage /= Walking then return; end if;
         Enter_Scope (Run.System_Bus, Scope_Entered); if not Scope_Entered then return; end if;
      end if;
      if Consumer_Preparation_Enabled then
         Walk(True); if Run.Info.Stage/=Walking then return;end if;
         Finish_Region_Phase(Prepared);if not Prepared then return;end if;
         Run.Info.Consumer_Preparation_Attempted:=True;
         Run.Info.Stage:=Incomplete;Charge:=0;Prepared:=False;
         Prepare_Consumers(Service,Run.Budget-Run.Info.Charged,Charge,Prepared);
         if Charge>Run.Budget-Run.Info.Charged then
            Run.Info.Stage:=Failed;Error(Run,N.Root,AML_Execute.Budget_Exceeded);return;
         end if;
         Run.Info.Charged:=Run.Info.Charged+Charge;
         if not Current_Topology(Run,Service) then
            Run.Info.Stage:=Failed;Error(Run,N.Root,AML_Execute.Unsupported_Value);return;
         end if;
         if not Prepared then return;end if;
         Run.Info.Consumers_Prepared:=True;Run.Info.Stage:=Walking;
      end if;
      Walk(False);if Run.Info.Stage/=Walking then return;end if;
      Finish_Region_Phase(Prepared);if not Prepared then return;end if;
      Run.Info.Stage := (if Run.Info.Errors = 0 then Completed else Completed_With_Errors);
   end Complete_Initialization;
end ACPI_Device_Initialization;
