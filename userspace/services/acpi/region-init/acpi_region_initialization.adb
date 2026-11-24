pragma Ada_2022;
with AML_Decode;
with Interfaces;
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
   Group_Word_Bits : constant Positive := Interfaces.Unsigned_64'Size;
   subtype Group_Word is Natural range 0 .. N.Region_Space_ID'Pos(N.Region_Space_ID'Last)/Group_Word_Bits;
   type Space_Bits is array(Group_Word) of Interfaces.Unsigned_64;
   type Group_Flags is array(Node_ID) of Space_Bits;
   use type Interfaces.Unsigned_64;
   function Marked(Bits:Group_Flags;Item:Entry_State) return Boolean is
     ((Bits(V.Parent_Position(Item.Region))(Natural(Item.Data.Space)/Group_Word_Bits)
       and Interfaces.Shift_Left(Interfaces.Unsigned_64'(1),Natural(Item.Data.Space) mod Group_Word_Bits))/=0);
   procedure Mark(Bits:in out Group_Flags;Item:Entry_State) is
      Parent:constant Node_ID:=V.Parent_Position(Item.Region);
      Word:constant Group_Word:=Natural(Item.Data.Space)/Group_Word_Bits;
   begin
      Bits(Parent)(Word):=Bits(Parent)(Word) or
        Interfaces.Shift_Left(Interfaces.Unsigned_64'(1),Natural(Item.Data.Space) mod Group_Word_Bits);
   end Mark;
   function Observe (Run : Session) return Report is (Run.Info);
   function Pending (Run : Session) return Boolean is (Run.Handle /= V.No_Value);
   function Observe_Region (Run : Session; Node : Node_ID) return Notice_Result is
     (Run.Items (Node).Notice);
   function Observe_Registration (Run : Session; Node : Node_ID)
     return Registration_Observation is
     (Present => Run.Items (Node).Present,
      Handler_Present => Run.Items (Node).Handler_Present,
      Group_Available => Run.Items (Node).Available,
      Suppressed => Run.Items (Node).Suppressed);
   function Has_Notification_Work (Run : Session; Scope : Node_ID) return Boolean is
     (Run.Notification_Scopes (Scope));
   function Has_Prepared_Group (Run : Session; Scope : Node_ID) return Boolean is
     (Run.Scopes (Scope));
   function Outcome (Run : Session) return ACPI_Device_Policy.Region_Outcome is
     (if Run.Info.Stage = Connected then
        (if Run.Info.Regions = 0 then ACPI_Device_Policy.No_Regions_Required
         else ACPI_Device_Policy.Regions_Connected)
      elsif Run.Info.Stage in Not_Started | Prepared then ACPI_Device_Policy.Regions_Unavailable
      else ACPI_Device_Policy.Region_Initialization_Failed);
   procedure Stop (Run : in out Session; Node : Node_ID;
      Error : AML_Execute.Execution_Status) is
   begin
      Run.Info.Stage := Failed; Run.Info.Error_Node := Node; Run.Info.Error := Error;
      if Run.Items (Node).Present then Run.Items (Node).Notice := Evaluation_Failed; end if;
   end Stop;
   procedure Release (Run : in out Session; Service : in out Core.State) is
   begin
      if not Pending (Run) then return; end if;
      Release_Result (Service, Run.Handle, Run.Info.Cleanup_Result);
      if Run.Info.Cleanup_Result /= V.Available or else Pending (Run) then
         Stop (Run, Run.Pending_Node, AML_Execute.Unsupported_Value);
      end if;
   end Release;
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
   -- PCI setup resolves a derived route lazily during ordinary AML reads.
   -- That cache fill does not replace the declaration or grant IO authority.
   -- Preserve every declaration field and any route already known at Prepare.
   function Same_Declaration
     (Now, Saved : N.Operation_Region_Attributes) return Boolean is
     (Now = Saved or else
       (Now.Space = PCI_Space and then not Saved.PCI.Known and then Now.PCI.Known
        and then (Now with delta PCI => Saved.PCI) = Saved));
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
            if Status /= V.Available or else not Same_Declaration (Data, Item.Data)
              or else Handler_Ready (Context, Item.Region, Data) /= Item.Handler_Present
              or else (Require_Every_Handler and then not Item.Handler_Present)
              or else Suppressed (Service, Context, Item) /= Item.Suppressed
            then return False; end if;
            if Item.Available and then not Item.Suppressed then
               Find_Method (Service, V.Parent_Position (Item.Region), Method, Status);
               if Status /= V.Available or else Method /= Item.Method then return False; end if;
            end if;
         end if;
      end loop;
      return True;
   end Current;
   procedure Run_Notices
     (Run : in out Session; Service : aliased in out Core.State;
      Context : in out Controller; Budget : Natural; Connecting_Now : Boolean;
      Scope_Filtered : Boolean := False; Scope : Node_ID := N.Root;
      Finalize_Connect : Boolean := True) is
      Result : V.Result;
      Status : V.Access_Status;
      Args : constant AML_Execute.Arguments := [others => 0];
      Actuals : AML_Execute.Arguments;
      Notified : Group_Flags := [others => [others => 0]];
      Starting_Charge : constant Natural := Run.Info.Charged;
      Step_Budget : Natural;
   begin
      if not Current (Run, Service, Context) then
         Stop (Run, N.Root, AML_Execute.Unsupported_Value); return;
      end if;
      for Node in Run.Items'Range loop
         if Run.Items (Node).Present
           and then (not Scope_Filtered or else V.Parent_Position (Run.Items (Node).Region) = Scope)
           and then (not Connecting_Now or else Run.Items (Node).Notice = Not_Attempted)
         then
            if not Current (Run, Service, Context) then
               Stop (Run, Node, AML_Execute.Unsupported_Value); return;
            elsif not Connecting_Now and then not Run.Items (Node).Was_Connected then
               -- No disconnect for groups never announced. Preserve copied
               -- reasons for suppressed, absent-method or unavailable entries.
               if not Run.Items (Node).Available then Run.Items (Node).Notice := Handler_Unavailable;
               elsif Run.Items (Node).Suppressed then Run.Items (Node).Notice := Not_Required;
               elsif Run.Items (Node).Method = V.No_Node then Run.Items (Node).Notice := Missing_Method;
               end if;
            elsif not Run.Items(Node).Available then Run.Items(Node).Notice := Handler_Unavailable;
            elsif Coalesce_Local_Notifications and then Marked(Notified,Run.Items(Node)) then
               Run.Items(Node).Notice := Completed; Run.Items(Node).Was_Connected := Connecting_Now;
            elsif Run.Items (Node).Suppressed then Run.Items (Node).Notice := Not_Required;
            elsif Run.Items (Node).Method = V.No_Node then Run.Items (Node).Notice := Missing_Method;
            else
               if Run.Info.Charged = Natural'Last then
                  Stop (Run, Node, AML_Execute.Budget_Exceeded); return;
               end if;
               Step_Budget := Natural'Min
                 (Budget - (Run.Info.Charged - Starting_Charge), Natural'Last - Run.Info.Charged);
               Actuals := Args;
               Actuals (0) := AML_Decode.Integer_Value (Run.Items (Node).Data.Space);
               Actuals (1) := (if Connecting_Now then Connect_Code else Disconnect_Code);
               -- Consume before entering AML; an exception leaves a terminal
               -- in-progress stage that Connect/Disconnect cannot enter again.
               Run.Items (Node).Notice := Evaluation_Failed;
               Invoke (Service, Context, V.Position (Run.Items (Node).Method), Actuals,
                  Notice_Arguments, Step_Budget, Result, Status);
               Run.Info.Methods := Run.Info.Methods + 1;
               Run.Info.Reported_Charge := Result.Charged;
               -- Capture ownership before inspecting budget/status: even a
               -- refused callback report may have created a retained result.
               if Result.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
                 AML_Execute.Metadata_Returned
               then
                  Run.Handle := Result.Handle;
                  Run.Pending_Node := Node;
                  if not Pending (Run) then
                     Stop (Run, Node, AML_Execute.Unsupported_Value); return;
                  end if;
               end if;
               if Result.Status = AML_Execute.Fatal_Requested then
                  Run.Info.Fatal := (Present => True, Item => Result.Fatal);
               end if;
               if Result.Charged > Step_Budget then
                  Stop (Run, Node, AML_Execute.Budget_Exceeded);
                  Release (Run, Service); return;
               end if;
               Run.Info.Charged := Run.Info.Charged + Result.Charged;
               if Result.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
                 AML_Execute.Metadata_Returned
               then
                  Release (Run, Service);
                  if Run.Info.Stage = Failed then
                     return;
                  end if;
               elsif Result.Status not in AML_Execute.Returned | AML_Execute.No_Return then
                  Stop (Run, Node, Result.Status); return;
               end if;
               if Status /= V.Available or else not Current (Run, Service, Context) then
                  Stop (Run, Node, AML_Execute.Unsupported_Value); return;
               end if;
               Run.Items (Node).Notice := Completed;
               Run.Items (Node).Was_Connected := Connecting_Now;
               if Coalesce_Local_Notifications then Mark(Notified,Run.Items(Node));end if;
            end if;
         end if;
      end loop;
      Run.Info.Stage := (if not Connecting_Now then Disconnected
         elsif Finalize_Connect then Connected else Prepared);
   end Run_Notices;
   procedure Prepare
     (Run : in out Session; Service : Core.State;
      Context : Controller; Accepted : out Boolean) is
      Description : Core.Node_Descriptor;
      Status : V.Access_Status;
      Blocked : Group_Flags := [others => [others => 0]];
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
            Run.Scopes (V.Parent_Position (Description)) := True;
            Core.Read_Region_Descriptor (Service, Description, Run.Items (Node).Data, Status);
            if Status /= V.Available then Stop (Run, Node, AML_Execute.Unsupported_Value); return; end if;
            Run.Info.Regions := Run.Info.Regions + 1;
            Run.Items(Node).Handler_Present := Handler_Ready(Context,Description,Run.Items(Node).Data);
            Run.Items(Node).Available := Run.Items(Node).Handler_Present;
            Run.Items (Node).Suppressed := Suppressed (Service, Context, Run.Items (Node));

         end if;
      end loop;
      -- A _REG announcement covers its whole local address-space group.
      -- Build parent/space bit sets in two linear passes; do not scan all
      -- namespace pairs merely to find one unavailable member.
      for Item of Run.Items loop
         if Item.Present and then not Item.Handler_Present then Mark(Blocked,Item);end if;
      end loop;
      for Node in Run.Items'Range loop
         if Run.Items(Node).Present then
            Run.Items(Node).Available:=not Marked(Blocked,Run.Items(Node));
            if not Run.Items(Node).Available then Run.Info.Unavailable:=Run.Info.Unavailable+1;
            elsif not Run.Items(Node).Suppressed then
               Find_Method(Service,V.Parent_Position(Run.Items(Node).Region),Run.Items(Node).Method,Status);
               if Status/=V.Available then Stop(Run,Node,AML_Execute.Unsupported_Value);return;end if;
            end if;
            if Run.Items (Node).Available and then not Run.Items (Node).Suppressed
              and then Run.Items (Node).Method /= V.No_Node then
               Run.Notification_Scopes (V.Parent_Position (Run.Items (Node).Region)) := True;
            end if;
         end if;
      end loop;
      -- All advertised groups remain stable before and after each notice.
      -- An unavailable group receives neither connect nor disconnect notice.
      if not Current (Run, Service, Context) then
         Stop (Run, N.Root, AML_Execute.Unsupported_Value); return;
      end if;
      Run.Info.Stage := Prepared;
   end Prepare;
   procedure Connect_Scope
     (Run : in out Session; Service : aliased in out Core.State;
      Context : in out Controller; Scope : Node_ID;
      Budget : Natural; Charged : out Natural; Action : out Scope_Action) is
      Has_Work : Boolean := False;
      Starting_Charge : constant Natural := Run.Info.Charged;
   begin
      Charged := 0; Action := Wrong_Phase;
      if Run.Info.Stage /= Prepared then return; end if;
      Action := Wrong_Context;
      if not Core.Current_Node (Service, Run.Owner) then return; end if;
      if not Run.Scopes (Scope) then Action := No_Prepared_Group; return; end if;
      if not Current (Run, Service, Context) then
         Stop (Run, N.Root, AML_Execute.Unsupported_Value); Action := Scope_Failed; return;
      end if;
      for Item of Run.Items loop
         if Item.Present and then V.Parent_Position (Item.Region) = Scope
           and then Item.Notice = Not_Attempted then Has_Work := True; exit; end if;
      end loop;
      if not Has_Work then Action := Already_Completed; return; end if;
      Run.Info.Stage := Connecting;
      Run_Notices (Run, Service, Context, Budget, True,
         Scope_Filtered => True, Scope => Scope, Finalize_Connect => False);
      Charged := Run.Info.Charged - Starting_Charge;
      Action := (if Run.Info.Stage = Prepared then Scope_Completed else Scope_Failed);
   end Connect_Scope;
   procedure Finish_Connect
     (Run : in out Session; Service : Core.State;
      Context : Controller; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Run.Info.Stage /= Prepared or else not Core.Current_Node (Service, Run.Owner) then return; end if;
      if not Current (Run, Service, Context) then
         Stop (Run, N.Root, AML_Execute.Unsupported_Value); return;
      end if;
      for Item of Run.Items loop
         if Item.Present and then Item.Notice = Not_Attempted then return; end if;
      end loop;
      Run.Info.Stage := Connected; Accepted := True;
   end Finish_Connect;
   procedure Connect
     (Run : in out Session; Service : aliased in out Core.State;
      Context : in out Controller; Budget : Natural; Accepted : out Boolean) is
   begin
      Prepare (Run, Service, Context, Accepted);
      if not Accepted or else Run.Info.Stage /= Prepared then return; end if;
      Run.Info.Stage := Connecting;
      Run_Notices (Run, Service, Context, Budget, True);
   end Connect;
   procedure Disconnect
     (Run : in out Session; Service : aliased in out Core.State;
      Context : in out Controller; Budget : Natural; Accepted : out Boolean) is
   begin
      Accepted := Run.Info.Stage in Prepared | Connected;
      if not Accepted then return; end if;
      Run.Info.Stage := Disconnecting; Run.Info.Methods := 0; Run.Info.Charged := 0;
      Run.Info.Reported_Charge := 0; Run.Info.Cleanup_Result := V.Available;
      Run_Notices (Run, Service, Context, Budget, False);
   end Disconnect;
   procedure Close
     (Run : in out Session; Service : in out Core.State;
      Action : out Cleanup_Action) is
   begin
      Action := No_Pending_Result;
      if not Pending (Run) then return; end if;
      Action := Wrong_Context;
      if not Core.Current_Binding (Service, Run.Owner) then return; end if;
      Action := Attempted;
      Release (Run, Service);
   end Close;
end ACPI_Region_Initialization;
