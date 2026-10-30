pragma Ada_2022;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;
with ACPI_Service_Core;
with ACPI_Region_Initialization;
with ACPI_Region_Initialization.Orphans;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with AML_Names;
with Firmware_Tables;
procedure Orphan_Reg_Tests is
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 96, Aggregate_Method_Capacity => 8192, Retained_Result_Capacity => 4);
   package E renames AML_Execute;
   package N renames C.Namespace;
   use type C.Stage_Status, C.Install_Status, C.Freeze_Status, C.Values.Access_Status, C.Values.Module_Outcome;
   use type C.Node_Descriptor, C.Values.Value_Handle, N.Region_Space_ID;
   use type E.Execution_Status, AML_Decode.Integer_Value;
   type Scenario is (EC_Normal, GPIO_Normal, Other_Space, No_Local_Method,
      EC_Region_Present, GPIO_Region_Present, Deferred, Nonmethod, Foreign_Device,
      Unavailable, Zero_Budget, Readiness_Lost, Unknown_Completion, Overcharge,
      Missing_Handle, Compound_Cleanup, Fatal_Handler, Wrong_Arity, Foreign_Disconnect);
   Selected : Scenario := EC_Normal;
   type Context is limited record
      Ready : Boolean := True;
      Device : C.Node_Descriptor := C.Values.No_Node;
      Space : N.Region_Space_ID := 3;
      Calls, Releases : Natural := 0;
      Refuse_Release : Boolean := False;
   end record;
   function Handler_Ready (Owner : Context; Region : C.Node_Descriptor;
      Data : N.Operation_Region_Attributes) return Boolean is
      pragma Unreferenced (Owner, Region, Data);
   begin return False; end Handler_Ready;
   function PCI_Root (Owner : Context; Region : C.Node_Descriptor) return Boolean is
      pragma Unreferenced (Owner, Region);
   begin return False; end PCI_Root;
   procedure Invoke (Service : aliased in out C.State; Owner : in out Context;
      Node : N.Node_ID; Args : E.Arguments; Argument_Count : Natural; Budget : Natural;
      Result : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin
      Owner.Calls := Owner.Calls + 1;
      if Selected = Unknown_Completion then
         Result := (Status => E.Field_Indeterminate, Charged => 1); Status := C.Values.Available;
      elsif Selected = Missing_Handle then
         Result := (Status => E.Object_Returned, Charged => 1, Handle => C.Values.No_Value); Status := C.Values.Available;
      else C.Invoke_Retained (Service, Node, Args, Argument_Count, Budget, Result, Status); end if;
      if Selected = Readiness_Lost then Owner.Ready := False; end if;
      if Selected = Overcharge then Result.Charged := Budget + 1; end if;
   end Invoke;
   package R is new ACPI_Region_Initialization (C, Context, Handler_Ready, PCI_Root, Invoke);
   function Provider_Ready (Owner : Context; Device : C.Node_Descriptor; Space : N.Region_Space_ID) return Boolean is
     (Owner.Ready and then Device = Owner.Device and then Space = Owner.Space);
   -- This is only an ordinary cleanup fault model, not a kernel provider.
   procedure Release (Service : in out C.State; Handle : in out C.Values.Value_Handle;
      Status : out C.Values.Access_Status);
   Owner : Context;
   procedure Release (Service : in out C.State; Handle : in out C.Values.Value_Handle;
      Status : out C.Values.Access_Status) is
   begin
      Owner.Releases := Owner.Releases + 1;
      if Owner.Refuse_Release then Owner.Refuse_Release := False; Status := C.Values.Invalid_Value;
      else C.Release_Result (Service, Handle, Status); end if;
   end Release;
   package O is new R.Orphans (Provider_Ready, Release);
   use type O.Phase, O.Outcome, O.Action_Result;
   Service, Foreign : aliased C.State (1, 4096, 4096);
   Device : C.Node_Descriptor;
   Space : O.Address_Space := O.Embedded_Controller;
   Run : O.State;
   Action : O.Action_Result;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; Text : String) is
   begin Checks := Checks + 1; if not Condition then raise Program_Error with Selected'Image & " " & Text; end if; end Check;
   function Child (State : C.State; Parent : N.Node_ID; Name : AML_Names.Segment) return C.Node_Descriptor is
      Node : N.Node_ID; Item : C.Node_Descriptor; Status : C.Values.Access_Status;
   begin
      C.Find_Child (State, Parent, Name, Node, Status); Check (Status = C.Values.Available and then Node /= N.Root, "fixture " & Name);
      C.Describe_Node (State, Node, Item, Status); Check (Status = C.Values.Available, "fixture descriptor"); return Item;
   end Child;
   function Calls return AML_Decode.Integer_Value is
      Value : C.Values.Result; Status : C.Values.Access_Status;
   begin
      C.Observe_Named_Value (Service, C.Values.Position (Child (Service, N.Root, "CALL")), Value, Status);
      Check (Status = C.Values.Available and then Value.Status = E.Returned, "namespace callback count"); return Value.Number;
   end Calls;
   procedure Prepare (State : aliased in out C.State; Image : Firmware_Tables.Bytes) is
      Stage : C.Stage_Status; Freeze : C.Freeze_Status;
      Installed : C.Install_Status;
      Report : C.Execution_Report; Members : N.Initialization_Report;
      Status : C.Values.Access_Status;
   begin
      if Selected = Deferred then
         -- Legacy declaration import preserves unresolved table-region metadata.
         -- Normal module execution refuses the missing table earlier; this
         -- case exercises the orphan guard at its own metadata boundary.
         C.Install (State, 1, C.DSDT, Image, Installed); Check (Installed = C.Installed, "deferred declaration import");
      else
         C.Stage_Table (State, 1, C.DSDT, Image, Stage); Check (Stage = C.Staged, "stage");
         C.Freeze_Catalog (State, Freeze); Check (Freeze = C.Catalog_Frozen_OK, "freeze");
         C.Execute_Frozen (State, 1000, Report, Status); Check (Status = C.Values.Available and then Report.Modules.Outcome = C.Values.Modules_Completed, "module preparation");
      end if;
      C.Initialize_Members (State, Members, Status); Check (Status = C.Values.Available, "member preparation");
   end Prepare;
begin
   Check (Ada.Command_Line.Argument_Count = 2, "fixture/scenario"); Selected := Scenario'Value (Ada.Command_Line.Argument (2));
   declare
      package IO renames Ada.Streams.Stream_IO;
      File : IO.File_Type; Data : Ada.Streams.Stream_Element_Array (1 .. 4096); Last : Ada.Streams.Stream_Element_Offset;
   begin
      IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1)); IO.Read (File, Data, Last); IO.Close (File);
      declare Image : Firmware_Tables.Bytes (1 .. Natural (Last)); begin
         for I in Image'Range loop Image (I) := Firmware_Tables.Byte (Data (Ada.Streams.Stream_Element_Offset (I))); end loop;
         Prepare (Service, Image); Prepare (Foreign, Image);
      end;
   end;
   Device := Child (Service, N.Root,
     (case Selected is when GPIO_Normal => "GP00", when Other_Space => "MEM0",
      when EC_Region_Present => "RGEC", when GPIO_Region_Present => "RGGP", when Deferred => "DFRD",
      when Nonmethod => "BAD0", when Compound_Cleanup => "CP00", when Wrong_Arity => "AR00",
      when Fatal_Handler => "FT00", when No_Local_Method => "PARN", when others => "EC00"));
   if Selected = No_Local_Method then Device := Child (Service, C.Values.Position (Device), "CHLD"); end if;
   if Selected in GPIO_Normal | GPIO_Region_Present then Space := O.GPIO_Controller; Owner.Space := 8; end if;
   Owner.Device := Device; Owner.Ready := Selected /= Unavailable; Owner.Refuse_Release := Selected = Compound_Cleanup;
   if Selected = Foreign_Device then Device := Child (Foreign, N.Root, "EC00"); end if;
   Check (Calls = 0, "discovery and deferred region do not execute AML");
   O.Connect (Run, Service, Owner, Device, Space, (if Selected = Zero_Budget then 0 else 1000), Action);
   Check (Action = O.Accepted and then O.Observe (Run).Charged <= 1000, "connection consumed and accounting bounded");
   case Selected is
      when EC_Normal | GPIO_Normal | Other_Space | Foreign_Disconnect =>
         Check (O.Observe (Run).Stage = O.Connected and then O.Observe (Run).Status = O.Notice_Completed and then Owner.Calls = 1 and then Calls = 1, "orphan callback once");
         if Selected = Foreign_Disconnect then
            O.Disconnect (Run, Foreign, Owner, 1000, Action);
            Check (Action = O.Wrong_Context and then O.Observe (Run).Stage = O.Connected and then Owner.Calls = 1, "foreign disconnect preserves owner state");
         end if;
         O.Disconnect (Run, Service, Owner, 1000, Action);
         Check (Action = O.Accepted and then O.Observe (Run).Stage = O.Disconnected and then Owner.Calls = 2 and then Calls = 2, "paired disconnect once");
      when No_Local_Method =>
         Check (O.Observe (Run).Stage = O.Connected and then O.Observe (Run).Status = O.No_Method and then Owner.Calls = 0 and then Calls = 0, "no ancestor method fallback");
         O.Disconnect (Run, Service, Owner, 1000, Action);
         Check (Action = O.Accepted and then O.Observe (Run).Stage = O.Disconnected and then Owner.Calls = 0, "absent method disconnect is effect-free");
      when EC_Region_Present | GPIO_Region_Present =>
         Check (O.Observe (Run).Stage = O.Skipped and then O.Observe (Run).Status = O.Region_Present and then Owner.Calls = 0, "matching region is ordinary coordinator work");
      when Deferred => Check (O.Observe (Run).Status = O.Deferred_Region and then Owner.Calls = 0 and then Calls = 0, "deferred local region cannot certify absence");
      when Nonmethod => Check (O.Observe (Run).Status = O.Unsupported_Method and then Owner.Calls = 0, "nonmethod _REG refused");
      when Foreign_Device => Check (O.Observe (Run).Status = O.Invalid_Device and then Owner.Calls = 0, "foreign descriptor refused");
      when Unavailable => Check (O.Observe (Run).Status = O.Provider_Unavailable and then Owner.Calls = 0, "explicit provider readiness required");
      when Zero_Budget => Check (O.Observe (Run).Status = O.Budget_Exhausted and then Owner.Calls = 0 and then Calls = 0, "zero budget no invocation");
      when Readiness_Lost => Check (O.Observe (Run).Status = O.Provider_Unavailable and then Owner.Calls = 1, "readiness rechecked after invocation");
      when Unknown_Completion => Check (O.Observe (Run).Status = O.Indeterminate and then Owner.Calls = 1, "uncertainty is terminal");
      when Overcharge | Missing_Handle => Check (O.Observe (Run).Status = O.Provider_Contract_Error and then Owner.Calls = 1, "provider contract error is terminal");
      when Compound_Cleanup =>
         Check (O.Observe (Run).Status = O.Cleanup_Failed and then O.Pending (Run) and then C.Retained_Results (Service) = 1, "failed cleanup retains ownership");
         O.Close (Run, Foreign, Action); Check (Action = O.Wrong_Context and then O.Pending (Run), "foreign cleanup refused");
         O.Close (Run, Service, Action); Check (Action = O.Accepted and then not O.Pending (Run) and then Owner.Releases = 2, "cleanup retries only release");
      when Fatal_Handler => Check (O.Observe (Run).Status = O.Evaluation_Failed and then O.Observe (Run).Fatal.Present and then Owner.Calls = 1, "copied Fatal without OS hook");
      when Wrong_Arity => Check (O.Observe (Run).Status = O.Evaluation_Failed and then Owner.Calls = 1 and then Calls = 0, "wrong method arity fails without body effects");
   end case;
   declare Before : constant Natural := Owner.Calls; begin
      O.Connect (Run, Service, Owner, Device, Space, 1000, Action); Check (Action = O.Wrong_Phase and then Owner.Calls = Before, "connect cannot replay");
      O.Disconnect (Run, Service, Owner, 1000, Action); Check (Action = O.Wrong_Phase and then Owner.Calls = Before, "terminal disconnect cannot replay");
      O.Close (Run, Service, Action); Check (Action = O.No_Pending_Result and then Owner.Calls = Before, "no cleanup replay");
   end;
   Check (C.Valid (Service) and then C.Retained_Results (Service) = 0, "valid owner no retained result leak");
   Put_Line ("Orphan REG checks" & Checks'Image);
end Orphan_Reg_Tests;
