pragma Ada_2022;
with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with ACPI_GPE_Lifecycle;
with ACPI_GPE_Methods;
with ACPI_Service_Core;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with AML_Names;
with Firmware_Tables;
procedure GPE_Binding_Tests is
   package G renames ACPI_GPE_Lifecycle;
   package C is new ACPI_Service_Core
     (AML_Delays.Unavailable_Provider, Namespace_Node_Capacity => 128,
      Aggregate_Method_Capacity => 4096, Retained_Result_Capacity => 2);
   Calls, Releases : Natural := 0;
   Fail_Release, Overcharge, Empty_Handle, Force_Indeterminate : Boolean := False;
   procedure Evaluate
     (Service : aliased in out C.State; Item : C.Node_Descriptor;
      Budget : Natural; Result : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin
      Calls := Calls + 1;
      if Empty_Handle then
         Status := C.Values.Available; Result := (Status => AML_Execute.Object_Returned, Charged => 1, Handle => C.Values.No_Value);
      elsif Force_Indeterminate then
         Status := C.Values.Available; Result := (Status => AML_Execute.Field_Indeterminate, Charged => 1);
      else C.Evaluate_Named (Service, Item, Budget, Result, Status); end if;
      if Overcharge then Result.Charged := Budget + 1; end if;
   end Evaluate;
   procedure Release
     (Service : in out C.State; Handle : in out C.Values.Value_Handle; Status : out C.Values.Access_Status) is
   begin
      Releases := Releases + 1;
      if Fail_Release then Fail_Release := False; Status := C.Values.Invalid_Value;
      else C.Release_Result (Service, Handle, Status); end if;
   end Release;
   package M is new ACPI_GPE_Methods (C, Evaluate, Release);
   use type C.Stage_Status, C.Freeze_Status, C.Values.Access_Status, C.Values.Module_Outcome;
   use type AML_Execute.Execution_Status, AML_Decode.Integer_Value;
   use type M.Resolution_Outcome, M.Invocation_Outcome, M.Action_Result, M.Event_Number;
   use type G.Action_Result, G.Phase, G.Source_State, G.Trigger_Kind;
   Service, Other : aliased C.State (1, 4096, 4096);
   Checks : Natural := 0;
   Scope : C.Node_Descriptor;
   Edge, Level, Error_Item, Alias_Item, Package_Item, Argument_Item, Fatal_Item, Item : M.Binding;
   Resolution : M.Resolution_Report;
   procedure Check (Condition : Boolean; Message : String) is
   begin Checks := Checks + 1; if not Condition then raise Program_Error with Message; end if; end Check;
   function Find (State : C.State; Parent : C.Namespace.Node_ID; Name : AML_Names.Segment) return C.Node_Descriptor is
      Node : C.Namespace.Node_ID; Result : C.Node_Descriptor; Status : C.Values.Access_Status;
   begin
      C.Find_Child (State, Parent, Name, Node, Status); Check (Status = C.Values.Available, "fixture child lookup " & Name);
      C.Describe_Node (State, Node, Result, Status); Check (Status = C.Values.Available, "fixture descriptor"); return Result;
   end Find;
   function Hits return AML_Decode.Integer_Value is
      Result : C.Values.Result; Status : C.Values.Access_Status;
   begin
      C.Observe_Named_Value (Service, C.Values.Position (Find (Service, C.Namespace.Root, "HITS")), Result, Status);
      Check (Status = C.Values.Available and then Result.Status = AML_Execute.Returned, "observe counter without AML"); return Result.Number;
   end Hits;
   procedure Resolve (Number : M.Event_Number; Expected : M.Resolution_Outcome; Result : out M.Binding) is
   begin
      M.Resolve (Service, Scope, Number, Result, Resolution);
      Check (Resolution.Status = Expected, "resolve expected " & Expected'Image & " got " & Resolution.Status'Image);
      Check (M.Is_Bound (Result) = (Expected = M.Found), "only found methods bind");
      Check (C.Retained_Results (Service) = 0 and then C.Valid (Service), "discovery preserves owned roots/state");
   end Resolve;
   procedure Prepare (State : aliased in out C.State; Image : Firmware_Tables.Bytes) is
      Stage : C.Stage_Status; Freeze : C.Freeze_Status; Report : C.Execution_Report;
      Members : C.Namespace.Initialization_Report; Status : C.Values.Access_Status;
   begin
      C.Stage_Table (State, 1, C.DSDT, Image, Stage); Check (Stage = C.Staged, "stage binding fixture");
      C.Freeze_Catalog (State, Freeze); Check (Freeze = C.Catalog_Frozen_OK, "freeze");
      C.Execute_Frozen (State, 1000, Report, Status);
      Check (Status = C.Values.Available and then Report.Modules.Outcome = C.Values.Modules_Completed, "module preparation");
      C.Initialize_Members (State, Members, Status); Check (Status = C.Values.Available, "member preparation");
      C.Configure_Notifications (State, True, Status); Check (Status = C.Values.Available, "owner notification interest");
   end Prepare;
   procedure Invoke_Once (Bound : M.Binding; Expected : M.Invocation_Outcome; Budget : Natural := 100) is
      Run : M.Invocation; Action : M.Action_Result; Before : constant Natural := Calls;
   begin
      M.Invoke (Run, Service, Bound, Budget, Action);
      Check (Action = M.Accepted and then M.Consumed (Run) and then M.Observe (Run).Status = Expected,
        "invoke expected " & Expected'Image & " got " & M.Observe (Run).Status'Image);
      Check (not M.Pending (Run) and then C.Retained_Results (Service) = 0, "ordinary invocation releases all returned roots");
      M.Invoke (Run, Service, Bound, Budget, Action); Check (Action = M.Already_Consumed, "invocation cannot replay");
      Check (Calls = Before + (if Expected = M.Invalid_Binding or else (Expected = M.Budget_Exhausted and then Budget = 0) then 0 else 1), "exact evaluator call count");
      M.Close (Run, Service, Action); Check (Action = M.No_Pending_Result, "no cleanup replay");
   end Invoke_Once;
   type Source_ID is (No_Source, Edge_Source, Level_Source, Error_Source);
   Trace : String (1 .. 4) := [others => ' ']; Length : Natural := 0;
   procedure Add (Letter : Character) is
   begin Check (Length < Trace'Length, "bounded trace"); Length := Length + 1; Trace (Length) := Letter; end Add;
   procedure Mask (Source : Source_ID; Result : out G.IO_Outcome) is
   begin Check (Source /= No_Source, "admitted mask source"); Add ('M'); Result := G.Completed; end Mask;
   procedure Clear (Source : Source_ID; Result : out G.IO_Outcome) is
   begin Check (Source /= No_Source, "admitted clear source"); Add ('C'); Result := G.Completed; end Clear;
   procedure Enable (Source : Source_ID; Result : out G.IO_Outcome) is
   begin Check (Source /= No_Source, "admitted enable source"); Add ('E'); Result := G.Completed; end Enable;
   procedure Invoke_Event (Source : Source_ID; Budget : Natural; Result : out G.Handler_Outcome; Charged : out Natural) is
      Run : M.Invocation; Action : M.Action_Result;
      Bound : constant M.Binding := (case Source is when Edge_Source => Edge, when Level_Source => Level, when Error_Source => Error_Item, when No_Source => M.No_Binding);
   begin
      Add ('H'); M.Invoke (Run, Service, Bound, Budget, Action); Check (Action = M.Accepted, "lifecycle selects bound method");
      Charged := M.Observe (Run).Charged;
      Result := (case M.Observe (Run).Status is
         when M.Succeeded => G.Handler_Succeeded,
         when M.Budget_Exhausted => G.Handler_Budget_Exhausted,
         when M.Indeterminate => G.Handler_Completion_Unknown,
         when others => G.Handler_Failed);
      Check (not M.Pending (Run), "lifecycle fixture leaves no unresolved root");
   end Invoke_Event;
   package D is new G.Driver (Source_ID, No_Source, Mask, Clear, Enable, Invoke_Event);
begin
   Check (Ada.Command_Line.Argument_Count = 1, "fixture path");
   declare
      File : Ada.Streams.Stream_IO.File_Type; Stream : Ada.Streams.Stream_Element_Array (1 .. 4096); Last : Ada.Streams.Stream_Element_Offset;
   begin
      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Ada.Command_Line.Argument (1));
      Ada.Streams.Stream_IO.Read (File, Stream, Last); Ada.Streams.Stream_IO.Close (File);
      declare Image : Firmware_Tables.Bytes (1 .. Natural (Last)); begin
         for I in Image'Range loop Image (I) := Firmware_Tables.Byte (Stream (Ada.Streams.Stream_Element_Offset (I))); end loop;
         Prepare (Service, Image); Prepare (Other, Image);
      end;
   end;
   Scope := Find (Service, C.Namespace.Root, "_GPE");
   for Number in M.Event_Number loop
      declare
         Text : String (1 .. 8);
      begin
         Ada.Integer_Text_IO.Put (Text, Integer (Number), Base => 16);
         declare
            Hex : constant String := Ada.Strings.Fixed.Trim (Text, Ada.Strings.Both);
            Start : constant Positive := Ada.Strings.Fixed.Index (Hex, "#") + 1;
            Part : constant String := Hex (Start .. Hex'Last - 1);
            Expected : constant String := (if Part'Length = 1 then "0" & Part else Part);
         begin
            Check (M.Method_Name (Number, G.Edge_Triggered) = "_E" & Expected, "all edge names match independent standard hex formatting");
            Check (M.Method_Name (Number, G.Level_Triggered) = "_L" & Expected, "all level names match independent standard hex formatting");
         end;
         M.Resolve (Service, Scope, Number, Item, Resolution);
         Check (Calls = 0 and then C.Retained_Results (Service) = 0, "all event discovery is non-evaluating");
      end;
   end loop;
   Resolve (1, M.Found, Edge); Resolve (2, M.Found, Level); Resolve (3, M.Found, Error_Item);
   Resolve (4, M.Found, Alias_Item); Resolve (7, M.Found, Package_Item); Resolve (8, M.Found, Argument_Item);
   Resolve (9, M.Found, Fatal_Item);
   Resolve (0, M.Found, Item); Check (M.Number (Item) = 0 and then M.Trigger (Item) = G.Level_Triggered, "zero event number");
   Resolve (255, M.Found, Item); Check (M.Number (Item) = 255 and then M.Trigger (Item) = G.Edge_Triggered, "maximum event number");
   Resolve (5, M.Unsupported_Handler, Item); Check (not M.Current (Service, Item), "unsupported definition is not callable");
   Resolve (6, M.Ambiguous, Item); Check (not M.Current (Service, Item), "ambiguous definitions are not callable");
   Resolve (99, M.Missing, Item); Check (not M.Current (Service, Item), "missing definition is not callable");
   Check (Hits = 0 and then Calls = 0, "discovery never executes any candidate");
   M.Resolve (Service, Find (Other, C.Namespace.Root, "_GPE"), 1, Item, Resolution);
   Check (Resolution.Status = M.Invalid_Scope, "foreign scope refused");
   M.Resolve (Service, Find (Service, C.Namespace.Root, "_SI_"), 1, Item, Resolution);
   Check (Resolution.Status = M.Invalid_Scope, "unrelated root scope refused");
   M.Resolve (Service, Find (Service, C.Namespace.Root, "HITS"), 1, Item, Resolution);
   Check (Resolution.Status = M.Invalid_Scope, "non-scope refused");
   declare Block_Device : constant C.Node_Descriptor := Find (Service, C.Namespace.Root, "GPEB"); begin
      M.Resolve (Service, Block_Device, 10, Item, Resolution);
      Check (Resolution.Status = M.Found and then M.Trigger (Item) = G.Level_Triggered, "owner-selected block-device scope");
      M.Resolve (Service, Find (Service, C.Values.Position (Block_Device), "CHLD"), 10, Item, Resolution);
      Check (Resolution.Status = M.Missing, "no ancestor GPE handler fallback");
   end;
   Check (M.Current (Service, Edge) and then not M.Current (Other, Edge) and then not M.Current (Service, M.No_Binding), "binding current only in exact Core");
   declare Run : M.Invocation; Action : M.Action_Result; Before : constant Natural := Calls; begin
      M.Invoke (Run, Other, Edge, 100, Action);
      Check (Action = M.Accepted and then M.Observe (Run).Status = M.Invalid_Binding
        and then Calls = Before and then C.Retained_Results (Other) = 0, "foreign binding cannot execute in another Core");
   end;
   Invoke_Once (M.No_Binding, M.Invalid_Binding); Invoke_Once (Edge, M.Budget_Exhausted, 0);
   Invoke_Once (Edge, M.Budget_Exhausted, 1);
   Invoke_Once (Alias_Item, M.Succeeded); Invoke_Once (Package_Item, M.Succeeded);
   Invoke_Once (Error_Item, M.Evaluation_Failed); Invoke_Once (Argument_Item, M.Evaluation_Failed);
   declare Run : M.Invocation; Action : M.Action_Result; begin
      M.Invoke (Run, Service, Fatal_Item, 100, Action);
      Check (Action = M.Accepted and then M.Observe (Run).Status = M.Evaluation_Failed
        and then M.Observe (Run).Execution = AML_Execute.Fatal_Requested
        and then M.Observe (Run).Fatal.Present
        and then Natural (M.Observe (Run).Fatal.Item.Literal.Kind) = 1
        and then AML_Decode.Integer_Value (M.Observe (Run).Fatal.Item.Literal.Code) = 16#12345678#
        and then M.Observe (Run).Fatal.Item.Evaluated_Argument = 16#89#,
        "fatal request payload copied for owner diagnostics, never executed by host");
   end;
   Overcharge := True; Invoke_Once (Package_Item, M.Provider_Contract_Error); Overcharge := False;
   Empty_Handle := True; Invoke_Once (Edge, M.Provider_Contract_Error); Empty_Handle := False;
   Force_Indeterminate := True; Invoke_Once (Level, M.Indeterminate); Force_Indeterminate := False;
   declare Run : M.Invocation; Action : M.Action_Result; Before : constant Natural := Calls; Release_Before : constant Natural := Releases; begin
      Fail_Release := True; M.Invoke (Run, Service, Package_Item, 100, Action);
      Check (Action = M.Accepted and then M.Observe (Run).Status = M.Cleanup_Failed and then M.Pending (Run)
        and then C.Retained_Results (Service) = 1, "failed release retains root privately");
      M.Invoke (Run, Service, Package_Item, 100, Action); Check (Action = M.Already_Consumed and then Calls = Before + 1, "cleanup failure never replays method");
      M.Close (Run, Other, Action); Check (Action = M.Wrong_Context and then M.Pending (Run) and then Releases = Release_Before + 1, "foreign close cannot release retained root");
      M.Close (Run, Service, Action);
      Check (Action = M.Accepted and then not M.Pending (Run) and then M.Observe (Run).Status = M.Closed
        and then Calls = Before + 1 and then Releases = Release_Before + 2 and then C.Retained_Results (Service) = 0, "cleanup retries release only, in exact owner");
   end;
   for Source in Edge_Source .. Error_Source loop
      declare Run : D.Session; Action : G.Action_Result; Bound : constant M.Binding := (case Source is when Edge_Source => Edge, when Level_Source => Level, when Error_Source => Error_Item); begin
         Length := 0; Trace := [others => ' ']; D.Start (Run, Source, M.Trigger (Bound), Action); Check (Action = G.Accepted, "event source masked");
         D.Service (Run, 100, Action); Check (Action = G.Accepted, "bound method serviced");
         if Source = Error_Source then
            Check (D.Observe (Run).Stage = G.Failed and then D.Observe (Run).Source = G.Known_Masked and then Trace (1 .. Length) = "MCH", "bound AML error remains masked");
         else
            declare Event : C.Notification_Observation; Present : Boolean; Status : C.Values.Access_Status; begin
               C.Take_Notification (Service, Event, Present, Status);
               Check (Status = C.Values.Available and then Present and then Event.Current_Target and then Event.Number = 16#80#,
                 "bound actual handler queued lid notification");
            end;
            D.Finish (Run, Action); Check (Action = G.Accepted and then D.Observe (Run).Stage = G.Finished
              and then Trace (1 .. Length) = (if Source = Edge_Source then "MCHE" else "MHCE"), "binding-selected edge/level hardware order");
         end if;
      end;
   end loop;
   Check (C.Valid (Service) and then C.Retained_Results (Service) = 0 and then C.Pending_Notifications (Service) = 0, "final result and notification ownership");
   Ada.Text_IO.Put_Line ("GPE binding checks" & Checks'Image);
end GPE_Binding_Tests;
