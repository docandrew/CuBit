pragma Ada_2022;
with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with ACPI_GPE_Lifecycle;
with ACPI_Service_Core;
with AML_Delays;
with AML_Execute;
with AML_Decode;
with Firmware_Tables;
procedure GPE_Lifecycle_Tests is
   package G renames ACPI_GPE_Lifecycle;
   package C is new ACPI_Service_Core
     (AML_Delays.Unavailable_Provider, Namespace_Node_Capacity => 128,
      Aggregate_Method_Capacity => 4096, Retained_Result_Capacity => 2);
   use type G.Action_Result, G.Phase, G.Source_State, G.Report, G.Failure_Reason, G.Trigger_Kind;
   use type C.Stage_Status, C.Freeze_Status, C.Values.Access_Status, C.Values.Module_Outcome;
   use type AML_Execute.Execution_Status, AML_Decode.Integer_Value;
   type Source_ID is (No_Source, Edge_Source, Level_Source, Error_Source);
   type Fault_Kind is (No_Fault, Reject_Mask, Unknown_Mask, Raise_Mask,
      Reject_Clear, Unknown_Clear, Raise_Clear, Reject_Enable, Unknown_Enable,
      Raise_Enable, Fail_Handler, Unknown_Handler, Budget_Handler, Overcharge_Handler,
      Raise_Handler, Empty_Handler);
   Fault : Fault_Kind := No_Fault;
   Service : aliased C.State (1, 4096, 4096);
   Methods : array (Source_ID) of C.Node_Descriptor := [others => C.Values.No_Node];
   Lid_Node : C.Namespace.Node_ID;
   Trace : String (1 .. 8) := [others => ' '];
   Length, Checks, Invocations : Natural := 0;
   Reenter : Boolean := False;
   procedure Check (Condition : Boolean; Message : String) is
   begin Checks := Checks + 1; if not Condition then raise Program_Error with Message; end if; end Check;
   procedure Add (Value : Character) is
   begin Check (Length < Trace'Length, "bounded callback trace"); Length := Length + 1; Trace (Length) := Value; end Add;
   procedure Mask (Source : Source_ID; Result : out G.IO_Outcome) is
   begin
      Check (Source /= No_Source, "mask exact admitted source"); Add ('M');
      case Fault is
         when Reject_Mask => Result := G.Refused;
         when Unknown_Mask => Result := G.Completion_Unknown;
         when Raise_Mask => raise Program_Error with "modeled mask exception";
         when others => Result := G.Completed;
      end case;
   end Mask;
   procedure Clear (Source : Source_ID; Result : out G.IO_Outcome) is
   begin
      Check (Source /= No_Source, "clear exact admitted source"); Add ('C');
      case Fault is
         when Reject_Clear => Result := G.Refused;
         when Unknown_Clear => Result := G.Completion_Unknown;
         when Raise_Clear => raise Program_Error with "modeled clear exception";
         when others => Result := G.Completed;
      end case;
   end Clear;
   procedure Enable (Source : Source_ID; Result : out G.IO_Outcome) is
   begin
      Check (Source /= No_Source, "enable exact admitted source"); Add ('E');
      case Fault is
         when Reject_Enable => Result := G.Refused;
         when Unknown_Enable => Result := G.Completion_Unknown;
         when Raise_Enable => raise Program_Error with "modeled enable exception";
         when others => Result := G.Completed;
      end case;
   end Enable;
   procedure Invoke (Source : Source_ID; Budget : Natural; Result : out G.Handler_Outcome; Charged : out Natural);
   package D is new G.Driver (Source_ID, No_Source, Mask, Clear, Enable, Invoke);
   Reentrant_Run : D.Session;
   procedure Invoke (Source : Source_ID; Budget : Natural; Result : out G.Handler_Outcome; Charged : out Natural) is
      Value : C.Values.Result;
      Status : C.Values.Access_Status;
      Event : C.Notification_Observation;
      Present : Boolean;
      Action : G.Action_Result;
   begin
      Add ('H'); Invocations := Invocations + 1; Charged := 0;
      if Reenter then
         D.Service (Reentrant_Run, Budget, Action); Check (Action = G.Wrong_Phase, "reentrant invocation refused");
         D.Finish (Reentrant_Run, Action); Check (Action = G.Wrong_Phase, "reentrant finish refused");
         D.Start (Reentrant_Run, Source, G.Edge_Triggered, Action); Check (Action = G.Wrong_Phase, "reentrant restart refused");
      end if;
      case Fault is
         when Fail_Handler => Result := G.Handler_Failed; return;
         when Unknown_Handler => Result := G.Handler_Completion_Unknown; return;
         when Budget_Handler => Result := G.Handler_Budget_Exhausted; Charged := Budget; return;
         when Overcharge_Handler => Result := G.Handler_Succeeded; Charged := Budget + 1; return;
         when Raise_Handler => raise Program_Error with "modeled handler exception";
         when Empty_Handler => Result := G.Not_Invoked; return;
         when others => null;
      end case;
      C.Evaluate_Named (Service, Methods (Source), Budget, Value, Status); Charged := Value.Charged;
      Check (C.Retained_Results (Service) = 0, "event methods return no retained root");
      if Source = Error_Source then
         Check (Status = C.Values.Available and then Value.Status = AML_Execute.Division_By_Zero, "actual AML handler execution error");
         Result := G.Handler_Failed; return;
      end if;
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.No_Return, "actual GPE method completed");
      Check (C.Pending_Notifications (Service) = 1, "actual AML Notify queued once");
      C.Take_Notification (Service, Event, Present, Status);
      Check (Status = C.Values.Available and then Present and then Event.Current_Target
        and then Event.Node = Lid_Node and then Event.Number = 16#80#, "copied notification addresses actual lid");
      Result := G.Handler_Succeeded;
   end Invoke;
   procedure Reset_Trace is
   begin Trace := [others => ' ']; Length := 0; end Reset_Trace;
   procedure Expect_Trace (Expected : String) is
   begin Check (Trace (1 .. Length) = Expected, "callback order expected " & Expected & " got " & Trace (1 .. Length)); end Expect_Trace;
   procedure Inert (Run : in out D.Session) is
      Before : constant G.Report := D.Observe (Run);
      Saved_Length : constant Natural := Length;
      Saved_Calls : constant Natural := Invocations;
      Action : G.Action_Result;
   begin
      D.Start (Run, Edge_Source, G.Edge_Triggered, Action); Check (Action = G.Wrong_Phase, "consumed start refused");
      D.Service (Run, 100, Action); Check (Action = G.Wrong_Phase, "consumed service refused");
      D.Finish (Run, Action); Check (Action = G.Wrong_Phase, "consumed finish refused");
      Check (D.Observe (Run) = Before and then Length = Saved_Length and then Invocations = Saved_Calls, "terminal retries cannot replay callbacks");
   end Inert;
begin
   Check (Ada.Command_Line.Argument_Count = 1, "fixture path");
   declare
      File : Ada.Streams.Stream_IO.File_Type;
      Stream : Ada.Streams.Stream_Element_Array (1 .. 4096);
      Last : Ada.Streams.Stream_Element_Offset;
      Staged : C.Stage_Status;
      Frozen : C.Freeze_Status;
      Execution : C.Execution_Report;
      Members : C.Namespace.Initialization_Report;
      Status : C.Values.Access_Status;
      Scope, Node : C.Namespace.Node_ID;
   begin
      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Ada.Command_Line.Argument (1));
      Ada.Streams.Stream_IO.Read (File, Stream, Last); Ada.Streams.Stream_IO.Close (File);
      declare Image : Firmware_Tables.Bytes (1 .. Natural (Last)); begin
         for I in Image'Range loop Image (I) := Firmware_Tables.Byte (Stream (Ada.Streams.Stream_Element_Offset (I))); end loop;
         C.Stage_Table (Service, 1, C.DSDT, Image, Staged); Check (Staged = C.Staged, "stage GPE fixture");
      end;
      C.Freeze_Catalog (Service, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "freeze fixture");
      C.Execute_Frozen (Service, 1000, Execution, Status);
      Check (Status = C.Values.Available and then Execution.Modules.Outcome = C.Values.Modules_Completed, "execute definition block");
      C.Initialize_Members (Service, Members, Status); Check (Status = C.Values.Available, "initialize namespace");
      C.Configure_Notifications (Service, True, Status); Check (Status = C.Values.Available, "trusted notification interest");
      C.Find_Child (Service, C.Namespace.Root, "LID0", Lid_Node, Status); Check (Status = C.Values.Available, "lid device binding");
      C.Find_Child (Service, C.Namespace.Root, "_GPE", Scope, Status); Check (Status = C.Values.Available, "event scope binding");
      for Source in Edge_Source .. Error_Source loop
         C.Find_Child (Service, Scope,
           (case Source is when Edge_Source => "_E01", when Level_Source => "_L02", when Error_Source => "_E03"), Node, Status);
         Check (Status = C.Values.Available, "trusted event method binding");
         C.Describe_Node (Service, Node, Methods (Source), Status); Check (Status = C.Values.Available, "method descriptor");
      end loop;
   end;
   for Trigger in G.Trigger_Kind loop
      declare
         Run : D.Session;
         Action : G.Action_Result;
         Source : constant Source_ID := (if Trigger = G.Edge_Triggered then Edge_Source else Level_Source);
         Saved : G.Report;
      begin
         Reset_Trace; D.Start (Run, No_Source, Trigger, Action);
         Check (Action = G.Invalid_Source and then D.Observe (Run).Stage = G.Idle and then Length = 0, "invalid source has no effects");
         D.Start (Run, Source, Trigger, Action); Check (Action = G.Accepted and then D.Observe (Run).Stage = G.Ready, "mask/edge acknowledgement ready");
         Expect_Trace ((if Trigger = G.Edge_Triggered then "MC" else "M"));
         Saved := D.Observe (Run); D.Service (Run, 0, Action);
         Check (Action = G.No_Budget and then D.Observe (Run) = Saved, "zero budget defers without consuming handler");
         D.Finish (Run, Action); Check (Action = G.Wrong_Phase, "cannot enable before handler");
         D.Service (Run, 100, Action); Check (Action = G.Accepted and then D.Observe (Run).Stage = G.Handler_Done, "handler executed once");
         D.Service (Run, 100, Action); Check (Action = G.Wrong_Phase, "duplicate handler refused");
         Expect_Trace ((if Trigger = G.Edge_Triggered then "MCH" else "MH"));
         D.Finish (Run, Action);
         Check (Action = G.Accepted and then D.Observe (Run).Stage = G.Finished
           and then D.Observe (Run).Source = G.Known_Enabled, "successful event completed");
         Expect_Trace ((if Trigger = G.Edge_Triggered then "MCHE" else "MHCE")); Inert (Run);
      end;
   end loop;
   for Trigger in G.Trigger_Kind loop
      for Failure in Fault_Kind range Reject_Mask .. Empty_Handler loop
         declare Run : D.Session; Action : G.Action_Result; Info : G.Report; begin
            Fault := Failure; Reset_Trace;
            D.Start (Run, (if Trigger = G.Edge_Triggered then Edge_Source else Level_Source), Trigger, Action);
            if D.Observe (Run).Stage = G.Ready then D.Service (Run, 100, Action); end if;
            if D.Observe (Run).Stage = G.Handler_Done then D.Finish (Run, Action); end if;
            Check (Action = G.Accepted, "failed operation was consumed once");
            Info := D.Observe (Run);
            Check (Info.Stage = G.Failed and then Info.Reason /= G.No_Failure, "typed terminal failure");
            Check (Info.Source =
              (if Failure in Reject_Mask .. Raise_Mask or Failure in Unknown_Enable | Raise_Enable then G.Unknown_State else G.Known_Masked),
              "mask evidence distinguishes unknown enable completion");
            Check ((for all I in 1 .. Length => Trace (I) /= 'E') or else Failure in Reject_Enable .. Raise_Enable,
              "failed handler or acknowledge never enables");
            if Failure = Overcharge_Handler then
               Check (Info.Charged = 0 and then Info.Reported_Charge = 101 and then Info.Reason = G.Provider_Contract_Error, "overcharge evidence bounded");
            end if;
            Inert (Run);
         end;
      end loop;
   end loop;
   Fault := No_Fault; Reset_Trace; Reenter := True;
   declare Action : G.Action_Result; begin
      D.Start (Reentrant_Run, Edge_Source, G.Edge_Triggered, Action);
      D.Service (Reentrant_Run, 100, Action);
      Check (Action = G.Accepted and then D.Observe (Reentrant_Run).Stage = G.Handler_Done, "reentrant attempts cannot replace outer handler");
      D.Finish (Reentrant_Run, Action); Check (D.Observe (Reentrant_Run).Stage = G.Finished, "outer invocation finishes once");
      Expect_Trace ("MCHE"); Inert (Reentrant_Run);
   end;
   Reenter := False; Reset_Trace;
   declare Run : D.Session; Action : G.Action_Result; begin
      D.Start (Run, Error_Source, G.Edge_Triggered, Action); D.Service (Run, 100, Action);
      Check (Action = G.Accepted and then D.Observe (Run).Stage = G.Failed and then D.Observe (Run).Reason = G.Handler_Error
        and then D.Observe (Run).Source = G.Known_Masked, "AML execution error does not re-enable");
      Expect_Trace ("MCH"); Inert (Run);
   end;
   Check (C.Valid (Service) and then C.Retained_Results (Service) = 0 and then C.Pending_Notifications (Service) = 0, "final core/notification ownership");
   Ada.Text_IO.Put_Line ("GPE lifecycle checks" & Checks'Image);
end GPE_Lifecycle_Tests;
