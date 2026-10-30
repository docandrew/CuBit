pragma Ada_2022;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;
with ACPI_Service_Core;
with ACPI_Region_Initialization;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with AML_Names;
with Firmware_Tables;
procedure Region_Alias_Tests is
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 64, Aggregate_Method_Capacity => 8192,
      Retained_Result_Capacity => 2);
   package E renames AML_Execute;
   use type C.Stage_Status, C.Freeze_Status, C.Values.Access_Status, C.Values.Module_Outcome;
   use type E.Execution_Status, AML_Decode.Integer_Value;
   type Context is limited record
      Ready : Boolean := True;
      Calls : Natural := 0;
   end record;
   function Handler_Ready (Owner : Context; Region : C.Node_Descriptor;
      Data : C.Namespace.Operation_Region_Attributes) return Boolean is
      pragma Unreferenced (Region, Data);
   begin return Owner.Ready; end Handler_Ready;
   function PCI_Root (Owner : Context; Region : C.Node_Descriptor) return Boolean is
      pragma Unreferenced (Owner, Region);
   begin return False; end PCI_Root;
   procedure Invoke (Service : aliased in out C.State; Owner : in out Context;
      Node : C.Namespace.Node_ID; Args : E.Arguments; Argument_Count : Natural;
      Budget : Natural; Result : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin
      Owner.Calls := Owner.Calls + 1;
      C.Invoke_Retained (Service, Node, Args, Argument_Count, Budget, Result, Status);
   end Invoke;
   package R is new ACPI_Region_Initialization (C, Context, Handler_Ready, PCI_Root, Invoke);
   use type R.Phase, R.Notice_Result;
   Service, Foreign : aliased C.State (1, 4096, 4096);
   Owner : Context;
   Checks : Natural := 0;
   Run : R.Session;
   Accepted : Boolean;
   procedure Check (Condition : Boolean; Text : String) is
   begin Checks := Checks + 1; if not Condition then raise Program_Error with Text; end if; end Check;
   function Child (Parent : C.Namespace.Node_ID; Name : AML_Names.Segment) return C.Namespace.Node_ID is
      Node : C.Namespace.Node_ID; Status : C.Values.Access_Status;
   begin
      C.Find_Child (Service, Parent, Name, Node, Status);
      Check (Status = C.Values.Available and then Node /= C.Namespace.Root, "fixture child " & Name);
      return Node;
   end Child;
   function Integer_Value (Name : AML_Names.Segment) return AML_Decode.Integer_Value is
      Value : C.Values.Result; Status : C.Values.Access_Status;
   begin
      C.Observe_Named_Value (Service, Child (C.Namespace.Root, Name), Value, Status);
      Check (Status = C.Values.Available and then Value.Status = E.Returned, "observe " & Name);
      return Value.Number;
   end Integer_Value;
   procedure Prepare (State : aliased in out C.State; Image : Firmware_Tables.Bytes) is
      Stage : C.Stage_Status; Freeze : C.Freeze_Status;
      Report : C.Execution_Report; Members : C.Namespace.Initialization_Report;
      Status : C.Values.Access_Status;
   begin
      C.Stage_Table (State, 1, C.DSDT, Image, Stage); Check (Stage = C.Staged, "stage");
      C.Freeze_Catalog (State, Freeze); Check (Freeze = C.Catalog_Frozen_OK, "freeze");
      C.Execute_Frozen (State, 1000, Report, Status);
      Check (Status = C.Values.Available and then Report.Modules.Outcome = C.Values.Modules_Completed, "module initialization");
      C.Initialize_Members (State, Members, Status); Check (Status = C.Values.Available and then Members.Unsupported = 0, "members");
   end Prepare;
   procedure Load is
      package IO renames Ada.Streams.Stream_IO;
      File : IO.File_Type;
      Data : Ada.Streams.Stream_Element_Array (1 .. 4096);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1)); IO.Read (File, Data, Last); IO.Close (File);
      declare Image : Firmware_Tables.Bytes (1 .. Natural (Last)); begin
         for I in Image'Range loop Image (I) := Firmware_Tables.Byte (Data (Ada.Streams.Stream_Element_Offset (I))); end loop;
         Prepare (Service, Image); Prepare (Foreign, Image);
      end;
   end Load;
begin
   Check (Ada.Command_Line.Argument_Count = 2, "fixture and scenario"); Load;
   if Ada.Command_Line.Argument (2) = "invalid" then
      R.Connect (Run, Service, Owner, 1000, Accepted);
      Check (Accepted and then R.Observe (Run).Stage = R.Failed and then Owner.Calls = 0, "nonmethod alias refused before invocation");
   elsif Ada.Command_Line.Argument (2) = "unavailable" then
      Owner.Ready := False;
      R.Connect (Run, Service, Owner, 1000, Accepted);
      Check (Accepted and then R.Observe (Run).Stage = R.Failed and then Owner.Calls = 0, "no callback before whole inventory ready");
      Owner.Ready := True; R.Connect (Run, Service, Owner, 1000, Accepted);
      Check (not Accepted and then Owner.Calls = 0, "failed connection cannot replay");
   else
      R.Connect (Run, Service, Owner, 1000, Accepted);
      Check (Accepted and then R.Observe (Run).Stage = R.Connected, "local method aliases connect");
      Check (R.Observe (Run).Regions = 4 and then R.Observe (Run).Methods = 2 and then Owner.Calls = 2, "one notice for each non-suppressed local binding");
      Check (Integer_Value ("CALL") = 2 and then Integer_Value ("SPAC") = 3 and then Integer_Value ("CODE") = 1, "actual AML alias connect effects");
      Check (R.Observe_Region (Run, Child (Child (Child (C.Namespace.Root, "DV01"), "CHLD"), "REG2")) = R.Missing_Method,
        "ancestor _REG is not inherited");
      Check (R.Observe_Region (Run, Child (Child (C.Namespace.Root, "DV02"), "REG3")) = R.Not_Required,
        "memory-space notification stays suppressed");
      R.Connect (Run, Service, Owner, 1000, Accepted); Check (not Accepted and then Owner.Calls = 2, "duplicate connect refused");
      if Ada.Command_Line.Argument (2) = "foreign" then
         R.Disconnect (Run, Foreign, Owner, 1000, Accepted);
         Check (Accepted and then R.Observe (Run).Stage = R.Failed and then Owner.Calls = 2, "foreign context refuses disconnect without AML");
         R.Disconnect (Run, Service, Owner, 1000, Accepted);
         Check (not Accepted and then Owner.Calls = 2, "failed disconnect cannot replay");
      else
         R.Disconnect (Run, Service, Owner, 1000, Accepted);
         Check (Accepted and then R.Observe (Run).Stage = R.Disconnected and then Owner.Calls = 4, "same aliased callbacks disconnect once");
         Check (Integer_Value ("CALL") = 4 and then Integer_Value ("CODE") = 0, "actual disconnect effects");
         R.Disconnect (Run, Service, Owner, 1000, Accepted); Check (not Accepted and then Owner.Calls = 4, "duplicate disconnect refused");
      end if;
   end if;
   Check (C.Valid (Service) and then C.Retained_Results (Service) = 0, "valid owner no retained result leak");
   Put_Line ("Region alias checks" & Checks'Image);
end Region_Alias_Tests;
