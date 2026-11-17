--  supervision-check (tests/supervision, headless test supervision): a
--  supervisor using Start_Child (CuBit.Child_Starts, SUP-001). It starts a
--  child with typed parameters and takes its exit event; checks that
--  procmgr refuses a parameter of the wrong type, a program outside its
--  start-children scope, a device-class start, badge bindings and an
--  unknown dependency, each with its typed reason; then starts a child
--  that lingers and ends itself, so procmgr must reap the child.
with CuBit.Waits;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with CuBit.Child_Exits;
with CuBit.Child_Specs;
with CuBit.Child_Starts;
with CuBit.Launch_Arguments;
with CuBit.Messages;
with CuBit.Process_Events;
with CuBit.Process_IDs;
with CuBit.Process_IDs.Text;
with Startup;

procedure Main is
   package CS renames CuBit.Child_Specs;
   package CST renames CuBit.Child_Starts;
   package LA renames CuBit.Launch_Arguments;
   use type CST.Receipt_Kind, CS.Refusal, LA.Launch_Failure, CuBit.Child_Exits.Termination_Kind;

   Child_Program : constant String := "supervision-child.app";
   --  Every receipt is answered at once; this bounds the wait anyway.
   Receipt_Deadline_Ms : constant := 10_000;
   --  How long the first child may take to run and exit.
   Exit_Deadline_Ms : constant := 20_000;
   Poll_Step_Ms : constant := 20;
   --  Time for the lingering child to start before its supervisor ends.
   Linger_Start_Ms : constant := 1_000;
   Stop_Grace_Ms : constant CS.Grace_Ms := 1_000;
   Child_Priority : constant CS.Priority_Value := 3;

   Through : constant CST.Starter := CST.Using (Startup.Process_Manager);
   Failures : Natural := 0;
   Ignore : Unsigned_64;

   procedure Say (Text : String) is
   begin
      CuBit.Messages.debugPrint ("supervision-check: " & Text & ASCII.LF);
   end Say;

   procedure Check (Condition : Boolean; What : String; Detail : String) is
   begin
      if Condition then
         Say (What & " PASS");
      else
         Failures := Failures + 1;
         Say (What & " FAIL (" & Detail & ")");
      end if;
   end Check;

   function Temporary_Child (Value : String) return CS.Spec is
      S : CS.Spec := CST.Child (Child_Program, CS.Temporary, Stop_Grace_Ms, Child_Priority);
   begin
      CST.Give_Value (S, Value);
      return S;
   end Temporary_Child;

   function Ask (S : CS.Spec) return CST.Receipt is
      Result : CST.Receipt;
   begin
      CST.Start (Through, S, CuBit.Messages.Deadline_After (Receipt_Deadline_Ms), Result);
      return Result;
   end Ask;

   procedure Expect_Refusal (S : CS.Spec; Why : CS.Refusal; What : String) is
      R : constant CST.Receipt := Ask (S);
   begin
      Check (R.Kind = CST.Refused and then R.Why = Why, What, CST.Image (R));
   end Expect_Refusal;

   First : CST.Receipt;
   Ended : CuBit.Child_Exits.Report;
   Found : Boolean := False;
begin
   --  1. A child with typed parameters; its exit comes to its supervisor.
   First := Ask (Temporary_Child
     ("(Child_Parameters label => Child_Label.First count => 7 mode => (Child_Mode.Exit_With 3))"));
   Check (First.Kind = CST.Started, "a child started with typed parameters", CST.Image (First));
   if First.Kind = CST.Started then
      for Step in 1 .. Exit_Deadline_Ms / Poll_Step_Ms loop
         CuBit.Process_Events.Take_Exit (First.Child, Found, Ended);
         exit when Found;
         Ignore := CuBit.Waits.Sleep_For (Poll_Step_Ms);
      end loop;
      Check (Found and then Ended.Kind = CuBit.Child_Exits.Exited and then Ended.Code = 3,
             "the child's exit reached its supervisor: pid " & CuBit.Process_IDs.Text.Image (First.Child) &
             " exited code 3",
             (if Found then "kind/code differ" else "no exit event"));
   end if;

   --  2. A parameter of the wrong type: the child's own record refuses it.
   declare
      R : constant CST.Receipt := Ask (Temporary_Child
        ("(Child_Parameters label => Child_Label.First count => ""seven"" mode => Child_Mode.Linger)"));
   begin
      Check (R.Kind = CST.Refused and then R.Why = CS.Launch_Refused and then R.Failure = LA.Parameters_Rejected,
             "a parameter of the wrong type is refused (LAUNCH_REFUSED, parameters rejected)", CST.Image (R));
   end;

   --  3. A program outside this supervisor's start-children scope.
   declare
      S : CS.Spec := CST.Child ("parameters-plain.app", CS.Temporary, Stop_Grace_Ms, Child_Priority);
   begin
      Expect_Refusal (S, CS.Not_Granted, "a program its may_launch does not name is NOT_GRANTED");
      --  4. A device-class start: only the device manager makes those.
      S := CST.Child (Child_Program, CS.Temporary, Stop_Grace_Ms, Child_Priority);
      CST.Give_Class (S, "PNP0C50");
      CST.Add_Fact (S, "descriptor_register", 32);
      Expect_Refusal (S, CS.Class_Not_Granted, "a device-class start from a program is CLASS_NOT_GRANTED");
      --  5. Badge bindings: only the device manager gives those.
      S := Temporary_Child ("(Child_Parameters label => Child_Label.First count => 1 mode => Child_Mode.Linger)");
      CST.Add_Binding (S, "i2c", 16#1234#);
      Expect_Refusal (S, CS.Bindings_Not_Granted, "badge bindings from a program are BINDINGS_NOT_GRANTED");
      --  6. A dependency that is not a running child of this supervisor.
      S := Temporary_Child ("(Child_Parameters label => Child_Label.First count => 1 mode => Child_Mode.Linger)");
      CST.Add_Dependency (S, First.Child);
      Expect_Refusal (S, CS.Dependency_Unknown, "an ended sibling as a dependency is DEPENDENCY_UNKNOWN");
      --  6b. A held start and a sink: only the device manager (it admits a
      --  driver's GPIO sink before the driver runs).
      S := Temporary_Child ("(Child_Parameters label => Child_Label.First count => 1 mode => Child_Mode.Linger)");
      CST.Hold (S);
      Expect_Refusal (S, CS.Sink_Not_Granted, "a held start from a program is SINK_NOT_GRANTED");
      S := Temporary_Child ("(Child_Parameters label => Child_Label.First count => 1 mode => Child_Mode.Linger)");
      CST.Want_Sink (S, 16#4750_1000_0001_004C#);
      Expect_Refusal (S, CS.Sink_Not_Granted, "a sink asked by a program is SINK_NOT_GRANTED");
   end;

   --  7. A child that lingers; then this supervisor ends and procmgr reaps it.
   declare
      R : constant CST.Receipt := Ask (Temporary_Child
        ("(Child_Parameters label => Child_Label.Lingering count => 1 mode => Child_Mode.Linger)"));
   begin
      Check (R.Kind = CST.Started, "a lingering child started", CST.Image (R));
      Ignore := CuBit.Waits.Sleep_For (Linger_Start_Ms);
      CuBit.Process_Events.Take_Exit (R.Child, Found, Ended);
      Check (not Found, "the lingering child still runs", "it ended early");
      --  6c. Releasing a child that was never held, and one this program
      --  does not supervise.
      declare
         Released : CST.Release_Receipt;
         use type CST.Release_Kind, CS.Release_Refusal;
      begin
         CST.Release (Through, R.Child, CS.Run, CuBit.Messages.Deadline_After (Receipt_Deadline_Ms), Released);
         Check (Released.Kind = CST.Release_Refused and then Released.Why = CS.Not_Held,
                "releasing a child that was not held is NOT_HELD", CST.Image (Released));
         CST.Release (Through, CuBit.Messages.Own_Process, CS.Withdraw,
                      CuBit.Messages.Deadline_After (Receipt_Deadline_Ms), Released);
         Check (Released.Kind = CST.Release_Refused and then Released.Why = CS.Not_Supervised,
                "releasing a process it does not supervise is NOT_SUPERVISED", CST.Image (Released));
      end;
   end;
   if Failures = 0 then
      Say ("all checks passed; ending with a child running (procmgr reaps it)");
   else
      Say ("failures:" & Natural'Image (Failures) & "; ending");
   end if;
   --  An Ada main's exit code is the status it sets (crt0), never left to chance.
   Ada.Command_Line.Set_Exit_Status (if Failures = 0 then Ada.Command_Line.Success else Ada.Command_Line.Failure);
end Main;
