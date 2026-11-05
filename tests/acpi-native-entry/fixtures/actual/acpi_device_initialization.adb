pragma Ada_2022;
with AML_Names;
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
   function Startup_Claimed (Run : Session) return Boolean is
     (Run.Claimed or else Run.Info.Stage /= Not_Started);
   procedure Claim_Startup (Run : in out Session; Accepted : out Boolean) is
   begin
      Accepted := not Startup_Claimed (Run);
      Run.Claimed := True;
   end Claim_Startup;
   function Observe_Node (Run : Session; Node : Node_ID) return Node_Report is (Run.Items (Node));
   function Device_Kind (Kind : N.Object_Kind) return Boolean is
     (Kind in N.Device_Object | N.Processor_Object | N.Thermal_Zone_Object);
   procedure Increment (Value : in out Natural) is
   begin if Value < Natural'Last then Value := Value + 1; end if; end Increment;
   procedure Error (Run : in out Session; Node : Node_ID;
      Status : AML_Execute.Execution_Status) is
   begin
      if Run.Info.Errors = 0 then
         Run.Info.First_Error_Node := Node; Run.Info.First_Error := Status;
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
      Access_Result, Released : V.Access_Status;
      Description : Core.Node_Descriptor;
      Value : V.Result;
      Handle : V.Value_Handle;
      Is_Method : Boolean;
   begin
      Step := Evaluation_Failed; Number := 0;
      if not Current_Topology (Run, Service) then
         Step := Identity_Changed; Run.Info.Stage := Failed;
         Error (Run, Scope, AML_Execute.Unsupported_Value); return;
      end if;
      Core.Find_Object (Service, Scope, Part, Found, Access_Result);
      if Access_Result /= V.Available then
         Error (Run, Scope, AML_Execute.Unsupported_Value); return;
      elsif Found.Status = N.Not_Found then Step := Missing; return;
      elsif Found.Status /= N.Found then
         Error (Run, Scope, AML_Execute.Unsupported_Value); return;
      end if;
      Core.Describe_Node (Service, Found.Node, Description, Access_Result);
      if Access_Result /= V.Available then
         Error (Run, Scope, AML_Execute.Unsupported_Value); return;
      end if;
      Is_Method := V.Node_Kind (Description) = N.Method_Object;
      Evaluate_Named (Service, Description,
        Run.Budget - Run.Info.Charged, Value, Access_Result);
      if Value.Charged > Run.Budget - Run.Info.Charged then
         Run.Info.Stage := Failed;
         Error (Run, Scope, AML_Execute.Budget_Exceeded); return;
      end if;
      Run.Info.Charged := Run.Info.Charged + Value.Charged;
      if Is_Method then
         if Integer_Required then Increment (Run.Info.Status_Methods);
         else Increment (Run.Info.Initialization_Methods); end if;
      end if;
      if Value.Status = AML_Execute.Field_Indeterminate then
         Stop_Field_Indeterminate (Run, Scope); return;
      end if;
      if Access_Result /= V.Available then
         Error (Run, Scope, AML_Execute.Unsupported_Value); return;
      end if;
      if Value.Status = AML_Execute.Fatal_Requested then
         if not Run.Info.Fatal.Present then
            Run.Info.Fatal := (Present => True, Item => Value.Fatal);
            Run.Info.Fatal_Initializer := Scope;
         end if;
         Run.Info.Stage := Failed; Error (Run, Scope, Value.Status); return;
      end if;
      if Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
        AML_Execute.Metadata_Returned
      then
         Handle := Value.Handle;
         Core.Release_Result (Service, Handle, Released);
         if Released /= V.Available or else Handle /= V.No_Value then
            Run.Info.Stage := Failed; Error (Run, Scope, AML_Execute.Unsupported_Value); return;
         end if;
         if Integer_Required then
            Step := Wrong_Result_Type; Error (Run, Scope, AML_Execute.Unsupported_Value);
         else Step := Succeeded; end if;
      elsif Value.Status = AML_Execute.Returned then
         Number := Value.Number; Step := Succeeded;
      elsif not Integer_Required and then Value.Status = AML_Execute.No_Return then
         Step := Succeeded;
      else
         Error (Run, Scope, Value.Status);
         if Value.Status = AML_Execute.Budget_Exceeded then
            Step := Budget_Exhausted; Run.Info.Stage := Incomplete;
         end if;
      end if;
      if not Current_Topology (Run, Service) then
         Step := Identity_Changed; Run.Info.Stage := Failed;
         Error (Run, Scope, AML_Execute.Unsupported_Value);
      end if;
   end Evaluate;
   procedure Begin_Initialization (Run : in out Session;
      Service : aliased in out Core.State; Budget : Natural; Accepted : out Boolean) is
      Status : V.Access_Status;
      Parent, Cursor : Node_ID;
      Tail : Links := [others => N.Root];
      Found : N.Lookup_Result;
      Ignored : AML_Decode.Integer_Value;
      Step : Step_Result;
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
      Evaluate (Run, Service, N.Root, "_INI", False, Step, Ignored);
      Run.Items (N.Root).Initialization := Step;
      if Run.Info.Stage /= Preparing then return; end if;
      Core.Find_Object (Service, N.Root, "_SB_", Found, Status);
      if Status /= V.Available then
         Run.Info.Stage := Failed; Error (Run, N.Root, AML_Execute.Unsupported_Value); return;
      elsif Found.Status = N.Found then
         Run.System_Bus := Found.Node;
         Evaluate (Run, Service, Found.Node, "_INI", False, Step, Ignored);
         Run.Items (Found.Node).Initialization := Step;
      end if;
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
      Run.Info.Stage := Walking;
      Node := Run.First_Child (N.Root);
      while Node /= N.Root loop
         Descend := True;
         if Device_Kind (V.Node_Kind (Run.Nodes (Node))) then
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
         if Descend and then Run.First_Child (Node) /= N.Root then
            Node := Run.First_Child (Node);
         else
            while Node /= N.Root and then Run.Next_Sibling (Node) = N.Root loop
               Node := V.Parent_Position (Run.Nodes (Node));
            end loop;
            if Node /= N.Root then Node := Run.Next_Sibling (Node); end if;
         end if;
      end loop;
      Run.Info.Stage := (if Run.Info.Errors = 0 then Completed else Completed_With_Errors);
   end Complete_Initialization;
end ACPI_Device_Initialization;
