pragma Ada_2022;
with AML_Decode;
package body ACPI_Region_Initialization.Orphans with SPARK_Mode is
   package V renames Core.Values;
   package N renames Core.Namespace;
   use type V.Access_Status, V.Value_Handle, Core.Node_Descriptor;
   use type N.Object_Kind, N.Region_Space_ID, N.Lookup_Status;
   use type AML_Execute.Execution_Status;
   EC_Space : constant N.Region_Space_ID := 3;
   GPIO_Space : constant N.Region_Space_ID := 8;
   Connect_Code : constant AML_Decode.Integer_Value := 1;
   Disconnect_Code : constant AML_Decode.Integer_Value := 0;
   Notice_Arguments : constant Natural := 2;
   function Code (Space : Address_Space) return N.Region_Space_ID is
     (if Space = Embedded_Controller then EC_Space else GPIO_Space);
   function Observe (Run : State) return Report is (Run.Info);
   function Pending (Run : State) return Boolean is (Run.Handle /= V.No_Value);
   procedure Stop (Run : in out State; Reason : Outcome) is
   begin Run.Info.Stage := Failed; Run.Info.Status := Reason; end Stop;
   procedure Inspect
     (Service : Core.State; Device : Core.Node_Descriptor; Space : Address_Space;
      Method : out Core.Node_Descriptor; Result : out Outcome;
      Status : out V.Access_Status) is
      Item : Core.Node_Descriptor;
      Data : N.Operation_Region_Attributes;
      Found : N.Lookup_Result;
   begin
      Method := V.No_Node; Result := Invalid_Device; Status := V.Available;
      if not Core.Current_Node (Service, Device) or else V.Node_Kind (Device) /= N.Device_Object then return; end if;
      for Node in N.Root .. Core.Namespace_Node_Count (Service) loop
         Core.Describe_Node (Service, Node, Item, Status);
         if Status /= V.Available then Result := Lookup_Failed; return; end if;
         if V.Parent_Position (Item) = V.Position (Device) then
            if V.Node_Kind (Item) = N.Uninitialized_Region_Object then Result := Deferred_Region; return;
            elsif V.Node_Kind (Item) = N.Operation_Region_Object then
               Core.Read_Region_Descriptor (Service, Item, Data, Status);
               if Status /= V.Available then Result := Lookup_Failed; return; end if;
               if Data.Space = Code (Space) then Result := Region_Present; return; end if;
            end if;
         end if;
      end loop;
      Core.Find_Object (Service, V.Position (Device), "_REG", Found, Status);
      if Status /= V.Available then Result := Lookup_Failed; return;
      elsif Found.Status = N.Not_Found then Result := No_Method; return;
      elsif Found.Status /= N.Found then Result := Lookup_Failed; return; end if;
      Core.Describe_Node (Service, Found.Node, Method, Status);
      if Status /= V.Available then Result := Lookup_Failed; return; end if;
      Result := (if V.Node_Kind (Method) = N.Method_Object then Notice_Completed else Unsupported_Method);
   end Inspect;
   procedure Check_Current
     (Run : State; Service : Core.State; Context : Controller; Result : out Outcome) is
      Method : Core.Node_Descriptor;
      Candidate : Outcome;
      Status : V.Access_Status;
   begin
      Result := Identity_Changed;
      if not Core.Current_Binding (Service, Run.Owner) then return; end if;
      Inspect (Service, Run.Device, Run.Space, Method, Candidate, Status);
      if Candidate not in Notice_Completed | No_Method or else Method /= Run.Method then return; end if;
      Result := (if Provider_Ready (Context, Run.Device, Code (Run.Space)) then Notice_Completed else Provider_Unavailable);
   end Check_Current;
   procedure Release (Run : in out State; Service : in out Core.State) is
   begin
      if not Pending (Run) then return; end if;
      Release_Result (Service, Run.Handle, Run.Info.Cleanup_Result);
      if Run.Info.Cleanup_Result /= V.Available or else Pending (Run) then Stop (Run, Cleanup_Failed); end if;
   end Release;
   procedure Notify
     (Run : in out State; Service : aliased in out Core.State;
      Context : in out Controller; Budget : Natural; Connecting_Now : Boolean) is
      Value : V.Result;
      Binding_Status : Outcome;
      Args : constant AML_Execute.Arguments :=
        [0 => AML_Decode.Integer_Value (Code (Run.Space)),
         1 => (if Connecting_Now then Connect_Code else Disconnect_Code), others => 0];
   begin
      Run.Info.Charged := 0; Run.Info.Reported_Charge := 0;
      if Budget = 0 then Stop (Run, Budget_Exhausted); return; end if;
      Check_Current (Run, Service, Context, Binding_Status);
      if Binding_Status /= Notice_Completed then Stop (Run, Binding_Status); return; end if;
      if Run.Method = V.No_Node then
         Run.Info.Status := No_Method;
         Run.Info.Stage := (if Connecting_Now then Connected else Disconnected); return;
      end if;
      Run.Info.Stage := (if Connecting_Now then Connecting else Disconnecting);
      Invoke (Service, Context, V.Position (Run.Method), Args, Notice_Arguments, Budget, Value, Run.Info.Access_Result);
      Run.Info.Execution := Value.Status; Run.Info.Reported_Charge := Value.Charged;
      if Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned then
         Run.Handle := Value.Handle;
         if Run.Handle = V.No_Value then Stop (Run, Provider_Contract_Error); return; end if;
      end if;
      if Value.Status = AML_Execute.Fatal_Requested then Run.Info.Fatal := (Present => True, Item => Value.Fatal); end if;
      if Value.Charged > Budget then Stop (Run, Provider_Contract_Error);
      else
         Run.Info.Charged := Value.Charged;
         Check_Current (Run, Service, Context, Binding_Status);
         if Binding_Status /= Notice_Completed then Stop (Run, Binding_Status);
         elsif Run.Info.Access_Result /= V.Available then Stop (Run, Evaluation_Failed);
         elsif Value.Status = AML_Execute.Field_Indeterminate then Stop (Run, Indeterminate);
         elsif Value.Status = AML_Execute.Budget_Exceeded then Stop (Run, Budget_Exhausted);
         elsif Value.Status in AML_Execute.Returned | AML_Execute.No_Return |
           AML_Execute.Object_Returned | AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned
         then
            Run.Info.Status := Notice_Completed;
            Run.Info.Stage := (if Connecting_Now then Connected else Disconnected);
         else Stop (Run, Evaluation_Failed); end if;
      end if;
      Release (Run, Service);
   end Notify;
   procedure Connect
     (Run : in out State; Service : aliased in out Core.State;
      Context : in out Controller; Device : Core.Node_Descriptor;
      Space : Address_Space; Budget : Natural; Action : out Action_Result) is
      Result : Outcome;
   begin
      Action := Wrong_Phase; if Run.Info.Stage /= Fresh then return; end if;
      Action := Accepted; Run.Info.Stage := Checking; Run.Device := Device; Run.Space := Space;
      Core.Describe_Node (Service, N.Root, Run.Owner, Run.Info.Access_Result);
      if Run.Info.Access_Result /= V.Available then Stop (Run, Lookup_Failed); return; end if;
      Inspect (Service, Device, Space, Run.Method, Result, Run.Info.Access_Result);
      if Result = Region_Present then Run.Info.Stage := Skipped; Run.Info.Status := Result; return;
      elsif Result not in Notice_Completed | No_Method then Stop (Run, Result); return; end if;
      if not Provider_Ready (Context, Device, Code (Space)) then Stop (Run, Provider_Unavailable); return; end if;
      Notify (Run, Service, Context, Budget, True);
   end Connect;
   procedure Disconnect
     (Run : in out State; Service : aliased in out Core.State;
      Context : in out Controller; Budget : Natural; Action : out Action_Result) is
   begin
      Action := Wrong_Phase; if Run.Info.Stage /= Connected or else Pending (Run) then return; end if;
      Action := Wrong_Context; if not Core.Current_Binding (Service, Run.Owner) then return; end if;
      Action := Accepted; Run.Info.Stage := Disconnecting;
      Notify (Run, Service, Context, Budget, False);
   end Disconnect;
   procedure Close
     (Run : in out State; Service : in out Core.State; Action : out Action_Result) is
   begin
      Action := No_Pending_Result; if not Pending (Run) then return; end if;
      Action := Wrong_Context; if not Core.Current_Binding (Service, Run.Owner) then return; end if;
      Action := Accepted; Release (Run, Service);
   end Close;
end ACPI_Region_Initialization.Orphans;
