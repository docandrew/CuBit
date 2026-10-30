pragma Ada_2022;
with AML_Decode;
package body ACPI_Region_Initialization with SPARK_Mode is
   package N renames Core.Namespace;
   package V renames Core.Values;
   use type N.Object_Kind;
   use type N.Operation_Region_Attributes;
   use type N.Region_Space_ID;
   use type V.Access_Status;
   use type V.Value_Handle;
   use type V.Node_Descriptor;
   use type AML_Execute.Execution_Status;
   Memory_Space : constant N.Region_Space_ID := 0;
   IO_Space : constant N.Region_Space_ID := 1;
   PCI_Space : constant N.Region_Space_ID := 2;
   Table_Space : constant N.Region_Space_ID := 126;
   Connect_Code : constant AML_Decode.Integer_Value := 1;
   Disconnect_Code : constant AML_Decode.Integer_Value := 0;
   Notice_Arguments : constant Natural := 2;
   function Observe (Run : Session) return Report is (Run.Info);
   function Observe_Region (Run : Session; Node : Node_ID) return Notice_Result is
     (Run.Items (Node).Notice);
   function Outcome (Run : Session) return ACPI_Device_Policy.Region_Outcome is
     (if Run.Info.Stage = Connected then
        (if Run.Info.Regions = 0 then ACPI_Device_Policy.No_Regions_Required
         else ACPI_Device_Policy.Regions_Connected)
      elsif Run.Info.Stage = Not_Started then ACPI_Device_Policy.Regions_Unavailable
      else ACPI_Device_Policy.Region_Initialization_Failed);
   procedure Stop (Run : in out Session; Node : Node_ID;
      Error : AML_Execute.Execution_Status) is
   begin
      Run.Info.Stage := Failed; Run.Info.Error_Node := Node; Run.Info.Error := Error;
      if Run.Items (Node).Present then Run.Items (Node).Notice := Evaluation_Failed; end if;
   end Stop;
   procedure Find_Method
     (Service : Core.State; Scope : Node_ID; Method : out Core.Node_Descriptor;
      Status : out V.Access_Status) is
      Found : N.Lookup_Result;
      use type N.Lookup_Status;
   begin
      Method := V.No_Node;
      -- Resolve only the local selector, following aliases to their canonical
      -- object. A missing local _REG never falls back to an ancestor method.
      Core.Find_Object (Service, Scope, "_REG", Found, Status);
      if Status /= V.Available or else Found.Status = N.Not_Found then return; end if;
      if Found.Status /= N.Found then Status := V.Invalid_Value; return; end if;
      Core.Describe_Node (Service, Found.Node, Method, Status);
      if Status = V.Available and then V.Node_Kind (Method) /= N.Method_Object then
         Status := V.Wrong_Kind;
      end if;
   end Find_Method;
   function Suppressed
     (Service : Core.State; Context : Controller; Item : Entry_State) return Boolean is
      Node : Node_ID;
      Status : V.Access_Status;
   begin
      if Item.Data.Space in Memory_Space | IO_Space | Table_Space then return True; end if;
      if Item.Data.Space /= PCI_Space or else not Permanent_PCI_Root (Context, Item.Region)
      then return False; end if;
      Core.Find_Child (Service, V.Parent_Position (Item.Region), "_BBN", Node, Status);
      return Status = V.Available and then Node /= N.Root;
   end Suppressed;
   function Current
     (Run : Session; Service : Core.State; Context : Controller) return Boolean is
      Description, Method : Core.Node_Descriptor;
      Data : N.Operation_Region_Attributes;
      Status : V.Access_Status;
   begin
      -- Reject a different service, changed region incarnation/geometry, a
      -- replacement _REG or newly added region before any subsequent effect.
      if not Core.Current_Node (Service, Run.Owner) then return False; end if;
      for Node in Node_ID range N.Root .. Core.Namespace_Node_Count (Service) loop
         Core.Describe_Node (Service, Node, Description, Status);
         if Status = V.Available and then V.Node_Kind (Description) = N.Uninitialized_Region_Object
         then return False; end if;
         if Status = V.Available and then V.Node_Kind (Description) = N.Operation_Region_Object
           and then not Run.Items (Node).Present then return False; end if;
      end loop;
      for Item of Run.Items loop
         if Item.Present then
            Core.Read_Region_Descriptor (Service, Item.Region, Data, Status);
            if Status /= V.Available or else Data /= Item.Data
              or else not Handler_Ready (Context, Item.Region, Data)
              or else Suppressed (Service, Context, Item) /= Item.Suppressed
            then return False; end if;
            if not Item.Suppressed then
               Find_Method (Service, V.Parent_Position (Item.Region), Method, Status);
               if Status /= V.Available or else Method /= Item.Method then return False; end if;
            end if;
         end if;
      end loop;
      return True;
   end Current;
   procedure Run_Notices
     (Run : in out Session; Service : aliased in out Core.State;
      Context : in out Controller; Budget : Natural; Connecting_Now : Boolean) is
      Result : V.Result;
      Status, Released : V.Access_Status;
      Handle : V.Value_Handle;
      Args : constant AML_Execute.Arguments := [others => 0];
      Actuals : AML_Execute.Arguments;
   begin
      if not Current (Run, Service, Context) then
         Stop (Run, N.Root, AML_Execute.Unsupported_Value); return;
      end if;
      for Node in Run.Items'Range loop
         if Run.Items (Node).Present then
            if not Current (Run, Service, Context) then
               Stop (Run, Node, AML_Execute.Unsupported_Value); return;
            elsif Run.Items (Node).Suppressed then Run.Items (Node).Notice := Not_Required;
            elsif Run.Items (Node).Method = V.No_Node then Run.Items (Node).Notice := Missing_Method;
            else
               Actuals := Args;
               Actuals (0) := AML_Decode.Integer_Value (Run.Items (Node).Data.Space);
               Actuals (1) := (if Connecting_Now then Connect_Code else Disconnect_Code);
               -- Consume before entering AML; an exception leaves a terminal
               -- in-progress stage that Connect/Disconnect cannot enter again.
               Run.Items (Node).Notice := Evaluation_Failed;
               Invoke (Service, Context, V.Position (Run.Items (Node).Method), Actuals,
                  Notice_Arguments, Budget - Run.Info.Charged, Result, Status);
               if Result.Charged > Budget - Run.Info.Charged then
                  Stop (Run, Node, AML_Execute.Budget_Exceeded); return;
               end if;
               Run.Info.Charged := Run.Info.Charged + Result.Charged;
               Run.Info.Methods := Run.Info.Methods + 1;
               if Result.Status = AML_Execute.Fatal_Requested then
                  Run.Info.Fatal := (Present => True, Item => Result.Fatal);
               end if;
               if Result.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
                 AML_Execute.Metadata_Returned
               then
                  Handle := Result.Handle;
                  Core.Release_Result (Service, Handle, Released);
                  if Released /= V.Available or else Handle /= V.No_Value then
                     Stop (Run, Node, AML_Execute.Unsupported_Value); return;
                  end if;
               elsif Result.Status not in AML_Execute.Returned | AML_Execute.No_Return then
                  Stop (Run, Node, Result.Status); return;
               end if;
               if Status /= V.Available or else not Current (Run, Service, Context) then
                  Stop (Run, Node, AML_Execute.Unsupported_Value); return;
               end if;
               Run.Items (Node).Notice := Completed;
            end if;
         end if;
      end loop;
      Run.Info.Stage := (if Connecting_Now then Connected else Disconnected);
   end Run_Notices;
   procedure Connect
     (Run : in out Session; Service : aliased in out Core.State;
      Context : in out Controller; Budget : Natural; Accepted : out Boolean) is
      Description : Core.Node_Descriptor;
      Status : V.Access_Status;
   begin
      Accepted := False;
      if Run.Info.Stage /= Not_Started then return; end if;
      Accepted := True; Run.Info.Stage := Preparing;
      Core.Describe_Node (Service, N.Root, Run.Owner, Status);
      if Status /= V.Available then Stop (Run, N.Root, AML_Execute.Unsupported_Value); return; end if;
      for Node in Node_ID range N.Root .. Core.Namespace_Node_Count (Service) loop
         Core.Describe_Node (Service, Node, Description, Status);
         if Status = V.Available and then V.Node_Kind (Description) = N.Operation_Region_Object then
            Run.Items (Node).Present := True; Run.Items (Node).Region := Description;
            Core.Read_Region_Descriptor (Service, Description, Run.Items (Node).Data, Status);
            if Status /= V.Available then Stop (Run, Node, AML_Execute.Unsupported_Value); return; end if;
            Run.Info.Regions := Run.Info.Regions + 1;
            Run.Items (Node).Suppressed := Suppressed (Service, Context, Run.Items (Node));
            if not Run.Items (Node).Suppressed then
               Find_Method (Service, V.Parent_Position (Description), Run.Items (Node).Method, Status);
               if Status /= V.Available then Stop (Run, Node, AML_Execute.Unsupported_Value); return; end if;
            end if;
         end if;
      end loop;
      -- Every region is ready before the first notification, including methods
      -- that access a different region than the one currently being announced.
      if not Current (Run, Service, Context) then
         Stop (Run, N.Root, AML_Execute.Unsupported_Value); return;
      end if;
      Run.Info.Stage := Connecting;
      Run_Notices (Run, Service, Context, Budget, True);
   end Connect;
   procedure Disconnect
     (Run : in out Session; Service : aliased in out Core.State;
      Context : in out Controller; Budget : Natural; Accepted : out Boolean) is
   begin
      Accepted := Run.Info.Stage = Connected;
      if not Accepted then return; end if;
      Run.Info.Stage := Disconnecting; Run.Info.Methods := 0; Run.Info.Charged := 0;
      for Item of Run.Items loop
         if Item.Present then Item.Notice := Not_Attempted; end if;
      end loop;
      Run_Notices (Run, Service, Context, Budget, False);
   end Disconnect;
end ACPI_Region_Initialization;
