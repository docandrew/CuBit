pragma Ada_2022;
with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with ACPI_Service_Core;
with ACPI_Device_Dependencies;
with ACPI_Passive_Dependencies;
with AML_Delays;
with AML_Names;
with AML_Execute;
with AML_Decode;
with Firmware_Tables;
procedure Evaluated_Dependency_Tests is
   package C is new ACPI_Service_Core
     (AML_Delays.Unavailable_Provider, Namespace_Node_Capacity => 128,
      Aggregate_Method_Capacity => 4096, Retained_Result_Capacity => 2);
   Calls : Natural := 0;
   Overcharge : Boolean := False;
   procedure Evaluate
     (Service : aliased in out C.State; Item : C.Node_Descriptor;
      Budget : Natural; Result : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin
      Calls := Calls + 1;
      C.Evaluate_Named (Service, Item, Budget, Result, Status);
      if Overcharge then Result.Charged := Budget + 1; end if;
   end Evaluate;
   package D is new ACPI_Device_Dependencies (C, Evaluate);
   package P is new ACPI_Passive_Dependencies (C);
   package R renames D.Reader;
   use type C.Stage_Status, C.Freeze_Status, C.Values.Access_Status;
   use type C.Values.Module_Outcome, C.Node_Descriptor, C.Values.Value_Handle;
   use type AML_Execute.Execution_Status, AML_Decode.Integer_Value;
   use type D.Outcome, D.Action_Result, R.Outcome, R.Element_Count, P.Outcome;
   Service, Other : aliased C.State (1, 4096, 4096);
   Run : D.Session;
   Action : D.Action_Result;
   Items : R.Device_Array (Natural'Last - 7 .. Natural'Last);
   Result : R.Report;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; Message : String) is
   begin Checks := Checks + 1; if not Condition then raise Program_Error with Message; end if; end Check;
   function Find (State : C.State; Name : AML_Names.Segment) return C.Node_Descriptor is
      Node : C.Namespace.Node_ID;
      Value : C.Node_Descriptor;
      Status : C.Values.Access_Status;
   begin
      C.Find_Child (State, C.Namespace.Root, Name, Node, Status); Check (Status = C.Values.Available, "find fixture node");
      C.Describe_Node (State, Node, Value, Status); Check (Status = C.Values.Available, "describe fixture node");
      return Value;
   end Find;
   procedure Prepare (State : aliased in out C.State; Bytes : Firmware_Tables.Bytes) is
      Staged : C.Stage_Status;
      Frozen : C.Freeze_Status;
      Execution : C.Execution_Report;
      Members : C.Namespace.Initialization_Report;
      Status : C.Values.Access_Status;
   begin
      C.Stage_Table (State, 1, C.DSDT, Bytes, Staged); Check (Staged = C.Staged, "stage fixture");
      C.Freeze_Catalog (State, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "freeze fixture");
      C.Execute_Frozen (State, 1000, Execution, Status);
      Check (Status = C.Values.Available and then Execution.Modules.Outcome = C.Values.Modules_Completed, "module completion");
      C.Initialize_Members (State, Members, Status); Check (Status = C.Values.Available, "initialize static members");
   end Prepare;
   function Hits return AML_Decode.Integer_Value is
      Value : C.Values.Result;
      Status : C.Values.Access_Status;
   begin
      C.Observe_Named_Value (Service, C.Values.Position (Find (Service, "HITS")), Value, Status);
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.Returned, "observe counter without AML execution");
      return Value.Number;
   end Hits;
   procedure Begin_Query (Name : AML_Names.Segment; Expected : D.Outcome; Budget : Natural := 100) is
   begin
      D.Begin_Observation (Run, Service, Find (Service, Name), Budget, Action);
      Check (Action = D.Accepted and then D.Observe (Run).Status = Expected,
        "begin " & Name & " expected " & Expected'Image & " got " & D.Observe (Run).Status'Image);
      Check (C.Valid (Service), "valid controller state");
   end Begin_Query;
   procedure Finish is
   begin
      D.Close (Run, Service, Action);
      Check (Action = D.Accepted and then not D.Pending (Run) and then D.Observe (Run).Status = D.Closed, "explicit close releases session");
      Check (C.Valid (Service), "valid after close");
   end Finish;
   procedure Copy_Query (Expected : R.Outcome; Maximum : R.Element_Count := 8) is
   begin
      D.Copy (Run, Service, Maximum, Items, Result, Action);
      Check (Action = D.Accepted and then Result.Status = Expected,
        "copy expected " & Expected'Image & " got " & Result.Status'Image);
      Check (D.Pending (Run) and then C.Valid (Service), "copy retains original root");
      if Expected /= R.Copied_List then
         Check (Result.Copied = 0 and then (for all Item of Items => Item = C.Values.No_Node), "failure clears partial output");
      end if;
   end Copy_Query;
begin
   Check (Ada.Command_Line.Argument_Count = 1, "fixture path");
   declare
      File : Ada.Streams.Stream_IO.File_Type;
      Stream : Ada.Streams.Stream_Element_Array (1 .. 4096);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Ada.Command_Line.Argument (1));
      Ada.Streams.Stream_IO.Read (File, Stream, Last); Ada.Streams.Stream_IO.Close (File);
      declare Image : Firmware_Tables.Bytes (1 .. Natural (Last)); begin
         for I in Image'Range loop Image (I) := Firmware_Tables.Byte (Stream (Ada.Streams.Stream_Element_Offset (I))); end loop;
         Prepare (Service, Image); Prepare (Other, Image);
      end;
   end;
   declare Passive_Items : P.Device_Array (0 .. 3); Passive_Report : P.Report; begin
      P.Read (Service, Find (Service, "DYN0"), 4, Passive_Items, Passive_Report);
      Check (Passive_Report.Status = P.Method_Valued and then Calls = 0 and then Hits = 0, "passive path never executes _DEP");
   end;
   Begin_Query ("DYN0", D.Retained);
   Check (D.Observe (Run).Required = 3 and then Calls = 1 and then Hits = 1
     and then C.Retained_Results (Service) = 1, "one controller evaluation retains complete package");
   D.Begin_Observation (Run, Service, Find (Service, "NONE"), 100, Action);
   Check (Action = D.Busy and then D.Observe (Run).Status = D.Retained and then Calls = 1, "busy begin cannot replay or replace");
   declare Small : R.Device_Array (0 .. 1); begin
      D.Copy (Run, Service, 8, Small, Result, Action);
      Check (Action = D.Accepted and then Result.Status = R.Capacity_Required and then Result.Required = 3
        and then (for all Item of Small => Item = C.Values.No_Node), "size refusal preserves complete root");
   end;
   Copy_Query (R.Work_Limit, 2);
   D.Copy (Run, Other, 8, Items, Result, Action);
   Check (Action = D.Wrong_Context and then Result.Status = R.Not_Read
     and then (for all Item of Items => Item = C.Values.No_Node), "foreign copy refused and cleared");
   D.Close (Run, Other, Action);
   Check (Action = D.Wrong_Context and then D.Pending (Run)
     and then C.Retained_Results (Service) = 1 and then C.Retained_Results (Other) = 0, "foreign close preserves owner");
   Copy_Query (R.Copied_List);
   Check (Result.Copied = 3 and then Items (Items'First) = Find (Service, "GPO0")
     and then Items (Items'First + 1) = Find (Service, "I2C0") and then Items (Items'First + 2) = Items (Items'First), "ordered duplicates at high output bounds");
   Check ((for all I in Items'First + 3 .. Items'Last => Items (I) = C.Values.No_Node), "unused success suffix cleared");
   Copy_Query (R.Copied_List);
   Check (Calls = 1 and then Hits = 1 and then C.Retained_Results (Service) = 1, "all sizing/copy/context retries avoid AML replay");
   Finish;
   D.Close (Run, Service, Action); Check (Action = D.Not_Pending and then Calls = 1, "repeat close is inert");
   D.Copy (Run, Service, 8, Items, Result, Action);
   Check (Action = D.Not_Pending and then Result.Status = R.Not_Read and then (for all Item of Items => Item = C.Values.No_Node), "closed copy has no output");
   Begin_Query ("NONE", D.Missing); Check (not D.Pending (Run), "missing is not empty");
   Begin_Query ("EMP0", D.Retained); Copy_Query (R.Copied_List, 0);
   Check (Result.Size_Known and then Result.Required = 0 and then Result.Copied = 0, "empty package succeeds"); Finish;
   Begin_Query ("ALDP", D.Retained); Copy_Query (R.Copied_List);
   Check (Result.Copied = 1 and then Items (Items'First) = Find (Service, "GPO0"), "static alias controller path"); Finish;
   Begin_Query ("WTYP", D.Wrong_Result_Type); Check (not D.Pending (Run) and then C.Retained_Results (Service) = 0, "wrong method type cleaned");
   Begin_Query ("STR0", D.Wrong_Result_Type);
   Check (not D.Pending (Run) and then C.Retained_Results (Service) = 0, "retained string result released on type refusal");
   Begin_Query ("ERR0", D.Evaluation_Failed);
   Check (not D.Pending (Run) and then C.Retained_Results (Service) = 0
     and then D.Observe (Run).Execution /= AML_Execute.Object_Returned, "execution failure remains distinct from wrong return type");
   Begin_Query ("BMOD", D.Retained); Copy_Query (R.Unsupported_Element);
   Check (Result.Size_Known and then Result.Required = 2 and then C.Retained_Results (Service) = 1, "late invalid element releases child only"); Finish;
   declare Before : constant Natural := Calls; begin
      Begin_Query ("DYN0", D.Budget_Exhausted, 0);
      Check (Calls = Before and then not D.Pending (Run), "zero budget prevents evaluation");
   end;
   Begin_Query ("BUD0", D.Budget_Exhausted, 2);
   Check (not D.Pending (Run) and then D.Observe (Run).Charged <= 2
     and then D.Observe (Run).Execution = AML_Execute.Budget_Exceeded, "bounded method reports budget exhaustion");
   Overcharge := True; Begin_Query ("DYN0", D.Provider_Contract_Error); Overcharge := False;
   Check (D.Observe (Run).Charged = 0 and then D.Observe (Run).Reported_Charge = 101
     and then not D.Pending (Run) and then C.Retained_Results (Service) = 0, "bad provider charge preserved and result cleaned");
   declare Before : constant Natural := Calls; begin
      D.Begin_Observation (Run, Service, Find (Other, "DYN0"), 100, Action);
      Check (Action = D.Accepted and then D.Observe (Run).Status = D.Invalid_Device and then Calls = Before, "foreign device never evaluated");
      Begin_Query ("HITS", D.Invalid_Device); Check (Calls = Before, "non-device never evaluated");
   end;
   declare
      Node : C.Namespace.Node_ID; Child : C.Node_Descriptor; Status : C.Values.Access_Status;
   begin
      C.Find_Child (Service, C.Values.Position (Find (Service, "PARN")), "CHLD", Node, Status);
      Check (Status = C.Values.Available, "nested lookup"); C.Describe_Node (Service, Node, Child, Status);
      Check (Status = C.Values.Available, "nested descriptor"); D.Begin_Observation (Run, Service, Child, 100, Action);
      Check (Action = D.Accepted and then D.Observe (Run).Status = D.Missing, "no ancestor dependency fallback");
   end;
   declare
      Node : C.Namespace.Node_ID; Member : C.Node_Descriptor;
      Held : C.Values.Result; Handle : C.Values.Value_Handle; Status : C.Values.Access_Status;
   begin
      C.Find_Child (Service, C.Values.Position (Find (Service, "GPO0")), "_HID", Node, Status);
      Check (Status = C.Values.Available, "caller string lookup"); C.Describe_Node (Service, Node, Member, Status);
      Check (Status = C.Values.Available, "caller string descriptor"); C.Evaluate_Named (Service, Member, 100, Held, Status);
      Check (Status = C.Values.Available and then Held.Status = AML_Execute.Object_Returned, "caller owns string root"); Handle := Held.Handle;
      R.Copy (Service, Handle, 8, Items, Result);
      Check (Result.Status = R.Not_A_Package and then C.Retained_Results (Service) = 1, "reader never consumes wrong-type borrowed root");
      Begin_Query ("DYN0", D.Retained); Copy_Query (R.Read_Failed);
      Check (C.Retained_Results (Service) = 2, "root pressure preserves both caller and session roots"); Finish;
      Check (C.Retained_Results (Service) = 1, "close releases only its own root");
      C.Release_Result (Service, Handle, Status);
      Check (Status = C.Values.Available and then Handle = C.Values.No_Value, "caller root remains releasable");
      R.Copy (Service, Handle, 8, Items, Result);
      Check (Result.Status = R.Invalid_Result and then Result.Copied = 0, "empty borrowed handle refused");
   end;
   Check (not D.Pending (Run) and then C.Retained_Results (Service) = 0 and then C.Valid (Service), "final state has no roots");
   Ada.Text_IO.Put_Line ("Evaluated dependency checks" & Checks'Image);
end Evaluated_Dependency_Tests;
