with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with ACPI_Lid;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with ACPI_EC_Types;
with ACPI_EC_Transactions;
with ACPI_EC_Registers;
with ACPI_EC_Methods;
with ACPI_Service_Core;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with AML_Names;
with Firmware_Tables;
procedure EC_Query_Tests is
   use ACPI_EC_Types;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then
         raise Program_Error with "EC query check" & Checks'Image;
      end if;
   end Check;
   Now : Microseconds := 1;
   Last_Write : Microseconds := 0;
   Pending, Output_Ready : Boolean := False;
   Has_Event : Boolean := True;
   Event : Byte := 1;
   IO, Writes : Natural := 0;
   Uncertain : Boolean := False;
   procedure Clock (Time : out Microseconds; Available : out Boolean) is
   begin Time := Now; Available := True; end Clock;
   function Owned return Boolean is (True);
   procedure Read (Register : Read_Register; Value : out Byte;
                   Result : out Access_Result) is
   begin
      IO := IO + 1;
      Result := Completed;
      if Register = Status_Register then
         Value := (if Has_Event then Event_Pending else 0);
         if Pending then
            Check (Now >= Last_Write + Input_Assertion_Delay);
            Pending := False;
            Output_Ready := True;
         end if;
         if Output_Ready then Value := Value or Output_Full; end if;
      else
         Check (Output_Ready);
         Output_Ready := False;
         Value := Event;
      end if;
   end Read;
   procedure Write (Register : Write_Register; Value : Byte;
                    Result : out Access_Result) is
   begin
      IO := IO + 1;
      Writes := Writes + 1;
      Check (Register = Command_Register and Value = Query_Command);
      Check (not Pending and not Output_Ready);
      if Uncertain then
         Result := Indeterminate;
      else
         Result := Completed;
         Pending := True;
         Last_Write := Now;
      end if;
   end Write;
   package Engine is new ACPI_EC_Transactions (Read, Write, Clock, Owned);
   package Catalog is new ACPI_EC_Registers (Engine);
   use type Catalog.Submission, Catalog.Operation_ID;
   package C is new ACPI_Service_Core
     (AML_Delays.Unavailable_Provider, Namespace_Node_Capacity => 128,
      Aggregate_Method_Capacity => 4096, Retained_Result_Capacity => 2);
   Calls, Releases : Natural := 0;
   Fail_Release : Boolean := False;
   procedure Evaluate
     (Service : aliased in out C.State; Item : C.Node_Descriptor;
      Budget : Natural; Result : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin
      Calls := Calls + 1;
      C.Evaluate_Named (Service, Item, Budget, Result, Status);
   end Evaluate;
   procedure Release
     (Service : in out C.State; Handle : in out C.Values.Value_Handle;
      Status : out C.Values.Access_Status) is
   begin
      Releases := Releases + 1;
      if Fail_Release then
         Fail_Release := False;
         Status := C.Values.Invalid_Value;
      else
         C.Release_Result (Service, Handle, Status);
      end if;
   end Release;
   package M is new ACPI_EC_Methods (C, Evaluate, Release);
   package L is new ACPI_Lid (C);
   use type L.Lid_State, L.Query_Outcome, L.Identification_Outcome;
   use type C.Stage_Status, C.Freeze_Status, C.Values.Access_Status, C.Values.Module_Outcome;
   use type AML_Execute.Execution_Status, AML_Decode.Integer_Value;
   use type M.Resolution_Outcome, M.Invocation_Outcome, M.Action_Result, M.Event_Number;
   Service, Other : aliased C.State (1, 4096, 4096, 1);
   Scope : C.Node_Descriptor;
   Last_Token : Catalog.Operation_ID := 0;
   function Find (State : C.State; Parent : C.Namespace.Node_ID;
                  Name : AML_Names.Segment) return C.Node_Descriptor is
      Node : C.Namespace.Node_ID;
      Descriptor : C.Node_Descriptor;
      Status : C.Values.Access_Status;
   begin
      C.Find_Child (State, Parent, Name, Node, Status);
      Check (Status = C.Values.Available);
      C.Describe_Node (State, Node, Descriptor, Status);
      Check (Status = C.Values.Available);
      return Descriptor;
   end Find;
   function Hits return AML_Decode.Integer_Value is
      Value : C.Values.Result;
      Status : C.Values.Access_Status;
   begin
      C.Observe_Named_Value (Service,
        C.Values.Position (Find (Service, C.Namespace.Root, "HITS")), Value, Status);
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.Returned);
      return Value.Number;
   end Hits;
   procedure Prepare (State : aliased in out C.State; Image : Firmware_Tables.Bytes) is
      Stage : C.Stage_Status;
      Freeze : C.Freeze_Status;
      Report : C.Execution_Report;
      Members : C.Namespace.Initialization_Report;
      Status : C.Values.Access_Status;
   begin
      C.Stage_Table (State, 1, C.DSDT, Image, Stage);
      Check (Stage = C.Staged);
      C.Freeze_Catalog (State, Freeze);
      Check (Freeze = C.Catalog_Frozen_OK);
      C.Execute_Frozen (State, 1000, Report, Status);
      Check (Status = C.Values.Available and then Report.Modules.Outcome = C.Values.Modules_Completed);
      C.Initialize_Members (State, Members, Status);
      Check (Status = C.Values.Available);
      C.Configure_Notifications (State, True, Status);
      Check (Status = C.Values.Available);
   end Prepare;
   procedure Query (Number : Byte; Fact : out Catalog.Query_Completion) is
      Result : Catalog.Submission;
      Token, Rejected : Catalog.Operation_ID;
      Field : Catalog.Completion;
      Before : Natural;
   begin
      Event := Number;
      Catalog.Submit_Query (Now + 100, 100, Result, Token);
      Check (Result = Catalog.Started and Token > Last_Token);
      Last_Token := Token;
      Catalog.Submit_Query (Now + 100, 100, Result, Rejected);
      Check (Result = Catalog.Busy and Rejected = 0);
      Catalog.Submit (1, 1, 1, False, 0, Now + 100, 100, Result, Rejected);
      Check (Result = Catalog.Busy and Rejected = 0);
      for I in 1 .. 40 loop
         Before := IO;
         Catalog.Step;
         Check (IO <= Before + 1);
         Catalog.Take_Completion (Field);
         Check (not Field.Present); -- a Field reply cannot consume a query
         Catalog.Take_Query (Fact);
         exit when Fact.Present;
         Now := Now + 1;
      end loop;
      Check (Fact.Present and Fact.Token = Token);
      Before := IO;
      Check (IO = Before);
   end Query;
   procedure Deliver (Number : Byte; Resolution : M.Resolution_Outcome;
                      Outcome : M.Invocation_Outcome := M.Succeeded;
                      Budget : Natural := 100) is
      Fact, Duplicate : Catalog.Query_Completion;
      Binding : M.Binding;
      Report : M.Resolution_Report;
      Run : M.Invocation;
      Action : M.Action_Result;
      Before : constant Natural := Calls;
   begin
      Query (Number, Fact);
      Check (Fact.Status = Succeeded and Fact.Event = Number and not Fact.Needs_Recovery);
      Catalog.Take_Query (Duplicate);
      Check (not Duplicate.Present);
      if Fact.Event = 0 then
         Check (Calls = Before);
         return;
      end if;
      M.Resolve (Service, Scope, M.Event_Number (Fact.Event), Binding, Report);
      Check (Report.Status = Resolution and M.Is_Bound (Binding) = (Resolution = M.Found));
      if not M.Is_Bound (Binding) then
         Check (Calls = Before);
         return;
      end if;
      Check (M.Current (Service, Binding) and M.Number (Binding) = M.Event_Number (Number));
      M.Invoke (Run, Service, Binding, Budget, Action);
      Check (Action = M.Accepted and M.Consumed (Run) and M.Observe (Run).Status = Outcome);
      Check (not M.Pending (Run) and C.Retained_Results (Service) = 0);
      if Number = 5 then Check (M.Observe (Run).Fatal.Present); end if;
      M.Invoke (Run, Service, Binding, Budget, Action);
      Check (Action = M.Already_Consumed and Calls = Before + (if Budget = 0 then 0 else 1));
   end Deliver;
begin
   Check (Ada.Command_Line.Argument_Count = 1);
   declare
      File : Ada.Streams.Stream_IO.File_Type;
      Stream : Ada.Streams.Stream_Element_Array (1 .. 4096);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Ada.Command_Line.Argument (1));
      Ada.Streams.Stream_IO.Read (File, Stream, Last);
      Ada.Streams.Stream_IO.Close (File);
      declare Image : Firmware_Tables.Bytes (1 .. Natural (Last)); begin
         for I in Image'Range loop
            Image (I) := Firmware_Tables.Byte (Stream (Ada.Streams.Stream_Element_Offset (I)));
         end loop;
         Prepare (Service, Image);
         Prepare (Other, Image);
      end;
   end;
   Scope := Find (Service, C.Namespace.Root, "EC00");
   Check (M.Method_Name (1) = "_Q01" and M.Method_Name (255) = "_QFF");
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
            Check (M.Method_Name (Number) = "_Q" & Expected);
         end;
      end;
   end loop;
   declare
      Accepted : Boolean;
      Result : Catalog.Submission;
      Token : Catalog.Operation_ID;
      Field : Catalog.Completion;
      Fact : Catalog.Query_Completion;
      Before : constant Natural := IO;
   begin
      Catalog.Install ((Register => 1, Epoch => 1, Owner => 1, Cell => 0,
                        Readable => True, others => <>), Accepted);
      Check (Accepted);
      Catalog.Submit (1, 1, 1, False, 0, Now + 100, 100, Result, Token);
      Check (Result = Catalog.Started);
      Catalog.Submit_Query (Now + 100, 100, Result, Token);
      Check (Result = Catalog.Busy and Token = 0);
      Catalog.Request_Stop;
      Catalog.Step;
      Catalog.Take_Query (Fact);
      Check (not Fact.Present); -- query route cannot consume a Field completion
      Catalog.Take_Completion (Field);
      Check (Field.Present and Field.Status = Stopped and not Catalog.Active and IO = Before);
   end;
   declare
      Device : constant C.Node_Descriptor := Find (Service, C.Namespace.Root, "LID0");
      Identified : L.Identification_Report;
      Sample : L.Query_Report;
   begin
      L.Identify (Service, Device, 100, Identified);
      Check (Identified.Outcome = L.Matched);
      L.Query (Service, Device, 100, Sample);
      Check (Sample.Outcome = L.Sampled and Sample.State = L.Closed and Sample.Raw = 0);
   end;
   Deliver (0, M.Missing);
   Check (Hits = 0);
   Deliver (1, M.Found);
   declare
      Notification : C.Notification_Observation;
      Present : Boolean;
      Status : C.Values.Access_Status;
      Sample : L.Query_Report;
   begin
      C.Take_Notification (Service, Notification, Present, Status);
      Check (Status = C.Values.Available and Present and Notification.Current_Target
             and Notification.Number = 16#80# and Notification.Path_Length = 1
             and Notification.Path (1) = "LID0");
      L.Query (Service, Find (Service, C.Namespace.Root, "LID0"), 100, Sample);
      Check (Sample.Outcome = L.Sampled and Sample.State = L.Open and Sample.Raw = 1);
      C.Take_Notification (Service, Notification, Present, Status);
      Check (Status = C.Values.Available and not Present);
   end;
   Deliver (255, M.Found);
   Deliver (2, M.Missing); -- root _Q02 must not be inherited
   Deliver (3, M.Unsupported_Handler);
   Deliver (4, M.Found);
   Deliver (5, M.Found, M.Evaluation_Failed);
   Deliver (6, M.Found);
   Deliver (7, M.Found, M.Evaluation_Failed);
   Deliver (8, M.Found, M.Evaluation_Failed);
   Deliver (1, M.Found, M.Budget_Exhausted, 0);
   Check (Hits = 4 and Releases > 0);
   declare
      Binding : M.Binding;
      Report : M.Resolution_Report;
      Run : M.Invocation;
      Action : M.Action_Result;
      Before : constant Natural := Calls;
   begin
      M.Resolve (Service, Find (Service, C.Namespace.Root, "HITS"), 1, Binding, Report);
      Check (Report.Status = M.Invalid_Scope);
      M.Resolve (Service, Scope, 1, Binding, Report);
      Check (not M.Current (Other, Binding));
      M.Invoke (Run, Other, Binding, 100, Action);
      Check (Action = M.Accepted and M.Observe (Run).Status = M.Invalid_Binding and Calls = Before);
   end;
   declare
      Binding : M.Binding;
      Report : M.Resolution_Report;
      Run : M.Invocation;
      Action : M.Action_Result;
      Before : Natural;
   begin
      M.Resolve (Service, Scope, 6, Binding, Report);
      Check (Report.Status = M.Found);
      Fail_Release := True;
      M.Invoke (Run, Service, Binding, 100, Action);
      Check (Action = M.Accepted and M.Pending (Run)
             and M.Observe (Run).Status = M.Cleanup_Failed
             and C.Retained_Results (Service) = 1);
      Before := Releases;
      M.Close (Run, Other, Action);
      Check (Action = M.Wrong_Context and M.Pending (Run) and Releases = Before);
      M.Invoke (Run, Service, Binding, 100, Action);
      Check (Action = M.Already_Consumed and Releases = Before);
      M.Close (Run, Service, Action);
      Check (Action = M.Accepted and not M.Pending (Run)
             and M.Observe (Run).Status = M.Closed
             and C.Retained_Results (Service) = 0 and Releases = Before + 1);
   end;
   Has_Event := False;
   declare Fact : Catalog.Query_Completion; Before : constant Natural := Writes; begin
      Query (1, Fact);
      Check (Fact.Status = No_Event and Fact.Event = 0 and Writes = Before);
   end;
   Has_Event := True;
   Uncertain := True;
   declare Fact : Catalog.Query_Completion; Result : Catalog.Submission;
           Token : Catalog.Operation_ID; Before : Natural; begin
      Query (1, Fact);
      Check (Fact.Status = Access_Unknown and Fact.Event = 0 and Fact.Needs_Recovery);
      Check (Catalog.Retained_For_Recovery and Catalog.Active);
      Before := IO;
      Catalog.Submit_Query (Now + 100, 100, Result, Token);
      Check (Result = Catalog.Busy and Token = 0);
      Catalog.Step;
      Check (IO = Before);
   end;
   Ada.Text_IO.Put_Line ("EC query checks:" & Checks'Image);
end EC_Query_Tests;
