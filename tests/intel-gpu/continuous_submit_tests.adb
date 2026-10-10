with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with System;
with CuBit.GPU_Queues;
with Intel_GPU_Queue_Service;
with Intel_GPU_Ring_Reservation;
with Intel_GPU_Context_Table;
with Intel_GPU_Context_Table.Waiting;
with Intel_GPU_Context_Table.Draining;
with Intel_GPU_GuC_CT_Receive;
with Intel_GPU_GuC_Context_Event;
with Intel_GPU_GuC_Context_Lifecycle;
with Intel_GPU_GuC_Context_Session;
with Intel_GPU_GuC_Fast_Fences;
with Intel_GPU_GuC_Submission_Policy;
-- Continuous rendering on ONE application context through the production
-- queue service's synchronous wrapper (Submit_Call, as 0x0A27), context
-- table, session, lifecycle and FAST-ID stream, driven like the service
-- loop (GPU-001 step 2): Submit_Call publishes and kicks without waiting,
-- then each modeled loop turn sleeps 1 ms, drains G2H and runs the
-- service's Turn; Call_Finished answers when the timeline reaches the job. The GuC/GPU is a hosted model: MODE_SET produces MODE_DONE (late,
-- after the GPU already ran the job, when Delay_Enable_Done is set), an
-- enable or SCHED_CONTEXT starts the published segment, which completes a
-- few turns later; DEREGISTER produces DONE.
-- This is not firmware, CT memory or hardware evidence.
procedure Continuous_Submit_Tests is
   package Events renames Intel_GPU_GuC_Context_Event;
   package Life renames Intel_GPU_GuC_Context_Lifecycle;
   package Fast renames Intel_GPU_GuC_Fast_Fences;
   use type Life.Phase;

   Submissions : constant := 40_000; -- crosses the 32768-ID FAST window
   Session_Tag : constant Unsigned_64 := 42;

   -- Modeled firmware/GPU state.
   Mode_Sets, Schedules, Deregisters, Others_Sent : Natural := 0;
   Model_Enabled : Boolean := False;
   Published_Sequence : Unsigned_64 := 1;
   Marker : Unsigned_64 := 1;
   GPU_Hangs : Boolean := False;
   Clock : Unsigned_64 := 0;
   -- Modeled GPU execution: a kicked segment completes after Countdown turns.
   Running : Boolean := False;
   Countdown : Natural := 0;
   -- MODE_DONE for an enable held back this many turns (the gate test).
   Delay_Enable_Done : Boolean := False;
   Enable_Done_Delay : constant := 2;
   Held_Done_Turns : Natural := 0;
   -- Waits inside the context-table waiter (park, setup). The submission
   -- path must never reach one.
   Pauses : Natural := 0;
   type Pending_Event is record
      Length : Natural range 0 .. 3 := 0;
      Words : Events.Words (0 .. 2) := [others => 0];
   end record;
   Pending : array (1 .. 8) of Pending_Event;
   Pending_Count : Natural range 0 .. 8 := 0;
   G2H_Events : Natural := 0;
   Stream : Fast.Stream;

   procedure Push (Item : Pending_Event) is
   begin
      pragma Assert (Pending_Count < Pending'Last);
      Pending_Count := Pending_Count + 1;
      Pending (Pending_Count) := Item;
   end Push;
   Held_Done : Pending_Event;
   -- Turns until the segment completes: SCHED_CONTEXT jobs vary; a job
   -- submitted by an enable completes on the next turn, before its delayed
   -- MODE_DONE, so the completion gate is exercised.
   procedure Run_Published (Turns_To_Complete : Natural) is
   begin
      if Published_Sequence /= Marker then
         Running := True; Countdown := Turns_To_Complete;
      end if;
   end Run_Published;

   function Ready return Boolean is (not Fast.Failed (Stream));

   procedure Queue (Payload : Events.Words; Result : out Life.Send_Result) is
      Fence : Unsigned_16;
      Accepted : Boolean;
      Action : constant Unsigned_32 := Payload (Payload'First) and 16#FFFF#;
   begin
      Result := Life.Uncertain;
      Fast.Prepare (Stream, Fence, Accepted);
      if not Accepted then return; end if;
      pragma Assert (Fast.Is_Fast (Fence));
      Fast.Sent (Stream, Fast.Published);
      Result := Life.Queued;
      case Action is
         when 16#1001# =>
            Mode_Sets := Mode_Sets + 1;
            Model_Enabled := Payload (Payload'First + 2) = 1;
            -- H6: enabling scheduling also submits the published tail.
            if Model_Enabled then Run_Published (0); end if;
            if Model_Enabled and Delay_Enable_Done then
               Held_Done := (3, [16#9000_1002#, Payload (Payload'First + 1),
                                 Payload (Payload'First + 2)]);
               Held_Done_Turns := Enable_Done_Delay;
            else
               Push ((3, [16#9000_1002#, Payload (Payload'First + 1),
                          Payload (Payload'First + 2)]));
            end if;
         when 16#1000# =>
            Schedules := Schedules + 1;
            pragma Assert (Model_Enabled);
            Run_Published (Natural (Published_Sequence mod 3));
         when 16#4503# =>
            Deregisters := Deregisters + 1;
            pragma Assert (not Model_Enabled);
            Push ((2, [16#9000_4600#, Payload (Payload'First + 1), 0]));
         when others => Others_Sent := Others_Sent + 1;
      end case;
   end Queue;

   procedure Retain (Payload : Events.Words; Fence : Unsigned_16; Success : out Boolean) is
      pragma Unreferenced (Payload, Fence);
   begin Success := False; end Retain;

   package Driver is new Intel_GPU_GuC_Context_Session (Ready, Queue, Retain);
   package Pool is new Intel_GPU_Context_Table (2, Driver, Ready, Retain);

   procedure Descriptor (Head, Tail, Status : out Unsigned_32; Success : out Boolean) is
   begin Head := 0; Tail := 0; Status := 0; Success := False; end Descriptor;
   procedure Read_Word (Index : Unsigned_32; Value : out Unsigned_32; Success : out Boolean) is
   begin Value := Index; Success := False; end Read_Word;
   procedure Finish (Success : out Boolean) is begin Success := False; end Finish;
   procedure Head (Value : Unsigned_32; Success : out Boolean) is
   begin Success := Value = 0; end Head;
   package Receiver is new Intel_GPU_GuC_CT_Receive (Descriptor, Read_Word, Finish, Head, Finish);

   procedure Poll (Item : out Receiver.Message; Status : out Receiver.Result) is
   begin
      Item := (others => <>); Status := Receiver.Empty;
      if Pending_Count = 0 then return; end if;
      Item.Length := Pending (1).Length;
      for I in 1 .. Item.Length loop
         Item.Payload (I) := Pending (1).Words (I - 1);
      end loop;
      Pending (1 .. Pending_Count - 1) := Pending (2 .. Pending_Count);
      Pending_Count := Pending_Count - 1;
      G2H_Events := G2H_Events + 1;
      Status := Receiver.Received;
   end Poll;
   function Now_Us return Unsigned_64 is (Clock);
   procedure Pause is begin Clock := Clock + 1; Pauses := Pauses + 1; end Pause;
   package Waiter is new Pool.Waiting (Receiver, Poll, Now_Us, Pause);
   function Work_Drained (Session : Unsigned_64) return Boolean;
   package Draining is new Pool.Draining (Now_Us, Work_Drained);
   use type Waiter.Result;
   use type Draining.Retirement_State;

   Table : Pool.Table;
   Context : Unsigned_32;
   Drain : Draining.Drain_State;

   procedure Read_Marker (Value : out Unsigned_64; OK : out Boolean) is
   begin Value := Marker; OK := True; end Read_Marker;
   function Service_Events return Boolean is
      Item : Receiver.Message;
      Received : Receiver.Result;
      ID : Unsigned_32;
      Delivery : Pool.Dispatch_Result;
      use type Receiver.Result;
      use type Pool.Dispatch_Result;
   begin
      for Index in 1 .. Pending'Length loop
         Poll (Item, Received);
         if Received = Receiver.Empty then return True; end if;
         Pool.Dispatch (Table, Events.Words (Item.Payload (1 .. Item.Length)),
                        Item.Fence, ID, Delivery);
         if Delivery /= Pool.Delivered then return False; end if;
      end loop;
      return True;
   end Service_Events;
   package Q renames CuBit.GPU_Queues;
   package Policy renames Intel_GPU_GuC_Submission_Policy;
   Deadline_Us : constant Unsigned_64 := 1_000_000;
   subtype Session_Id is Positive range 1 .. 1;
   function Select_Context (S : Session_Id; C : Q.Context_Index) return Boolean is
     (C = 0 and then Ready);
   function Batch (Handle, GPU, Offset, Bytes : Unsigned_64) return Boolean is (True);
   function Resident return Boolean is (Pool.State (Table, Context) = Life.Enabled);
   function Publish_Ready return Boolean is (Pool.Publish_Allowed (Table, Context));
   procedure Write_Segment
     (Operation : Q.Opcode; V, GPU : Unsigned_64; Plan : Intel_GPU_Ring_Reservation.Plan;
      Expected_Tail : Unsigned_32; OK : out Boolean) is
      pragma Unreferenced (Operation, Plan, Expected_Tail);
   begin
      Published_Sequence := V;
      OK := GPU /= 0 and then Pool.Publish_Allowed (Table, Context);
   end Write_Segment;
   function Segment_Bytes (Operation : Q.Opcode) return Unsigned_32 is (384);
   function Kick_Of (Status : Driver.Result) return Policy.Kick_Result is
     (case Status is
        when Driver.Queued => Policy.Kick_Queued,
        when Driver.Backpressure => Policy.Kick_Backpressure,
        when others => Policy.Kick_Failed);
   -- As main.adb: a non-blocking MODE_SET(enable) or SCHED_CONTEXT;
   -- MODE_DONE is drained by the loop. No Waiter here.
   procedure Kick (Enable : Boolean; Result : out Policy.Kick_Result) is
      Status : Driver.Result;
   begin
      if Enable then
         Pool.Submit (Table, Context, Life.Enable, Status);
      else
         Pool.Notify_Work (Table, Context, True, Status);
      end if;
      Result := Kick_Of (Status);
   end Kick;
   Quarantines : Natural := 0;
   procedure Quarantine (S : Session_Id; Why : Q.Fault_Reason) is
      pragma Unreferenced (S, Why);
   begin
      Quarantines := Quarantines + 1;
   end Quarantine;
   Call_Done, Call_OK : Boolean := False;
   Call_Value : Unsigned_64 := 0;
   procedure Call_Finished (S : Session_Id; V : Unsigned_64; OK : Boolean) is
      pragma Unreferenced (S);
   begin
      pragma Assert (not Call_Done, "each call answered once");
      Call_Done := True; Call_OK := OK; Call_Value := V;
   end Call_Finished;
   procedure Answer_Wake (S : Session_Id; Result : Q.Wake_Result) is null;
   function No_Region (S : Session_Id) return System.Address is (System.Null_Address);
   package Service is new Intel_GPU_Queue_Service
     (Session_Id, Select_Context, Ready, Batch, Resident, Publish_Ready, Write_Segment,
      Segment_Bytes, Kick, Read_Marker, Now_Us, Quarantine, Call_Finished, Answer_Wake,
      No_Region, No_Region, Deadline_Us);
   use type Service.Call_Result, Policy.Event_Count;
   T : Service.Table;
   function Work_Drained (Session : Unsigned_64) return Boolean is
     (Session = Session_Tag and then Service.Session_Idle (T, 1));

   Failures : Natural := 0;
   procedure Check (Condition : Boolean; Label : String) is
   begin
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & Label);
      end if;
   end Check;

   Status : Service.Call_Result;
   Completed : Natural := 0;
   Turns, Max_Turns, Gated_Turns : Natural := 0;
   -- One modeled service-loop turn: the 1 ms sleep, the GPU and GuC
   -- advancing, the full G2H drain, then the service's Turn.
   procedure Turn is
   begin
      Clock := Clock + 1_000;
      Turns := Turns + 1;
      if Held_Done_Turns > 0 then
         Held_Done_Turns := Held_Done_Turns - 1;
         if Held_Done_Turns = 0 then Push (Held_Done); end if;
      end if;
      if Running and then Model_Enabled and then not GPU_Hangs then
         if Countdown = 0 then
            Marker := Published_Sequence; Running := False;
         else
            Countdown := Countdown - 1;
         end if;
      end if;
      declare
         Drained : constant Boolean := Service_Events;
      begin
         pragma Assert (Drained);
      end;
      Service.Turn (T);
   end Turn;
   -- Submit_Call, then one service turn per loop pass until it is answered.
   procedure Submit_One (Result : out Service.Call_Result; OK : out Boolean;
                         Done : out Unsigned_64) is
      Before : constant Natural := Turns;
   begin
      Done := 0; OK := False; Call_Done := False;
      Service.Submit_Call (T, 1, 0, 1, 16#20000#, 0, 4096, Deadline_Us, Result);
      if Result /= Service.Submitted then return; end if;
      while not Call_Done loop
         Turn;
         if not Call_Done and then Published_Sequence = Marker and then
           Pool.State (Table, Context) /= Life.Enabled
         then
            Gated_Turns := Gated_Turns + 1;
         end if;
         -- The hang budget bounds every job: 1 s of 1 ms turns, plus slack.
         pragma Assert (Turns - Before <= 1_100);
      end loop;
      if Turns - Before > Max_Turns then Max_Turns := Turns - Before; end if;
      OK := Call_OK; Done := Call_Value;
   end Submit_One;
   OK_Call : Boolean;
   Value : Unsigned_64;
   Accepted : Boolean;
   Control : Driver.Result;
   Wait_Status : Waiter.Result;
   use type Driver.Result;
   use type Fast.Request_Count;
   procedure Park (OK : out Boolean) is
      Status : Waiter.Result;
   begin
      Waiter.Execute (Table, Context, Life.Disable, 1_000_000, Status);
      OK := Status = Waiter.Complete;
   end Park;
   procedure Open_Service_Context is
      Opened : Boolean;
   begin
      Service.Open_Context (T, 1, 0, 1, 384, Opened);
      pragma Assert (Opened);
   end Open_Service_Context;
   procedure Setup (Tag : Unsigned_64) is
   begin
      -- As Handle_Context_Registration: register, policy, enable for the
      -- setup marker, disable. Lifecycle requests only.
      Pool.Open (Table, 16#200000# + Unsigned_64 (Pool.Count (Table)) * 16#10000#,
                 4096, 1000, 500000, False, Context, Accepted, Session => Tag);
      pragma Assert (Accepted);
      Pool.Submit (Table, Context, Life.Register_Context, Control);
      pragma Assert (Control = Driver.Queued);
      Pool.Submit (Table, Context, Life.Set_Policy, Control);
      pragma Assert (Control = Driver.Queued);
      Waiter.Execute (Table, Context, Life.Enable, 1_000_000, Wait_Status);
      pragma Assert (Wait_Status = Waiter.Complete);
      Waiter.Execute (Table, Context, Life.Disable, 1_000_000, Wait_Status);
      pragma Assert (Wait_Status = Waiter.Complete);
      Marker := 1;
   end Setup;
begin
   Setup (Session_Tag);
   Open_Service_Context;
   Mode_Sets := 0; G2H_Events := 0;

   Delay_Enable_Done := True; Pauses := 0;
   for Index in 1 .. Submissions loop
      Submit_One (Status, OK_Call, Value);
      exit when Status /= Service.Submitted or else not OK_Call
        or else Value /= Unsigned_64 (Index + 1);
      Completed := Completed + 1;
   end loop;
   Delay_Enable_Done := False;
   Ada.Text_IO.Put_Line ("submissions completed=" & Natural'Image (Completed) &
     " of" & Natural'Image (Submissions) &
     " mode-set H2G=" & Natural'Image (Mode_Sets) &
     " sched H2G=" & Natural'Image (Schedules) &
     " G2H=" & Natural'Image (G2H_Events) &
     " turns=" & Natural'Image (Turns) & " max-turns/job=" & Natural'Image (Max_Turns) &
     " gated-turns=" & Natural'Image (Gated_Turns) &
     " fast-published=" & Fast.Request_Count'Image (Fast.Published_Count (Stream)) &
     " next-fence=" & Unsigned_16'Image (Fast.Next_Fence (Stream)) &
     " quarantines=" & Natural'Image (Quarantines));
   Check (Completed = Submissions, "every submission completes");
   -- Steady state: one enable for the first submission, then exactly one
   -- FAST SCHED_CONTEXT per submission and no lifecycle round trips.
   Check (Mode_Sets = 1 and Service.Stats (T).Enables = 1,
          "lifecycle requests independent of submission count");
   Check (Schedules = Submissions - 1,
          "one SCHED_CONTEXT per resident submission; the first rides on the enable");
   Check (G2H_Events = 1, "no per-submission G2H event");
   Check (Pauses = 0, "no wait loop on the submission path");
   Check (Gated_Turns = Enable_Done_Delay - 1 or Gated_Turns = Enable_Done_Delay,
          "reached timeline held until the late MODE_DONE is drained");
   Check (Max_Turns <= Enable_Done_Delay + 3, "deferred replies within a few turns");
   Check (Fast.Published_Count (Stream) > 32_768 and not Fast.Failed (Stream),
          "FAST IDs recycled across the 16-bit window");
   Check (Service.Stats (T).Latency_Count = Unsigned_64 (Submissions), "latency of every job");

   -- Exclusive work (VM update, buffer retirement) parks scheduling, only
   -- while quiescing with nothing in flight; the next job re-enables once,
   -- then returns to the steady state.
   Mode_Sets := 0;
   Service.Set_Quiesce (T, True);
   Check (Service.Park_Allowed (T), "park allowed: quiescing, nothing in flight");
   declare OK : Boolean; begin
      Park (OK);
      Check (OK and Pool.State (Table, Context) = Life.Disabled, "park acknowledged");
   end;
   Service.Submit_Call (T, 1, 0, 1, 16#20000#, 0, 4096, Deadline_Us, Status);
   Check (Status = Service.Busy, "no call while quiescing (the caller defers it)");
   Service.Set_Quiesce (T, False);
   Pauses := 0;
   for Index in 1 .. 100 loop
      Submit_One (Status, OK_Call, Value);
      Check (Status = Service.Submitted and OK_Call, "post-park submission");
   end loop;
   Check (Pauses = 0, "re-enable after park does not wait");
   Check (Mode_Sets = 2 and Service.Stats (T).Enables = 2,
          "park plus one re-enable, independent of later submissions");

   -- Clean retirement of the resident context: the drain disables, then
   -- deregisters. No reserved fence interval is needed for either.
   Pool.Retire_Session (Table, Session_Tag, Context);
   for Tick in 1 .. 100 loop
      declare Fault : Boolean; begin
         Draining.Tick (Table, Drain, Fault);
         Check (not Fault, "drain tick without fault");
      end;
      exit when Draining.Observe (Table, Session_Tag) = Draining.Deregistered;
      if not Service_Events then Check (False, "drain event dispatch"); exit; end if;
   end loop;
   Check (Draining.Observe (Table, Session_Tag) = Draining.Deregistered,
          "deregistration completes after continuous rendering");
   Check (Deregisters = 1 and not Fast.Failed (Stream), "one deregistration, stream intact");

   -- A GPU that never writes the timeline: each turn sleeps and observes;
   -- the hang watchdog ends the job with a failed answer and a quarantine
   -- instead of hanging the service loop or spinning.
   declare
      Started : Unsigned_64;
      Hung_Turns : Natural;
      Before_Quarantines : Natural;
      Forgotten : Boolean;
   begin
      Service.Forget_Session (T, 1, Forgotten);
      Check (Forgotten, "an idle retired session is forgotten");
      Setup (Session_Tag + 1);
      Open_Service_Context;
      GPU_Hangs := True; Pauses := 0;
      Started := Clock; Hung_Turns := Turns; Before_Quarantines := Quarantines;
      Submit_One (Status, OK_Call, Value);
      Hung_Turns := Turns - Hung_Turns;
      Check (Status = Service.Submitted and not OK_Call and Value = 2,
             "hung GPU: the call is answered failed, never OK");
      Check (Quarantines = Before_Quarantines + 1, "hung GPU quarantines the session");
      Check (Clock - Started >= Deadline_Us and Clock - Started <= Deadline_Us + 1_000,
             "hung GPU answered at its hang budget");
      Check (Hung_Turns <= Natural (Deadline_Us / 1_000) + 1 and Pauses = 0,
             "one observation per 1 ms turn, no spin, no wait loop");
      Ada.Text_IO.Put_Line ("hung-GPU answer after" &
        Unsigned_64'Image (Clock - Started) & " modeled us," & Natural'Image (Hung_Turns) &
        " turns");
   end;

   if Failures = 0 then
      Ada.Text_IO.Put_Line ("Continuous submission PASS:" & Natural'Image (Submissions) &
        " wrapper calls on one context, 1 asynchronous enable, FAST IDs wrapped," &
        " park/re-enable, clean deregistration, hung GPU ends at its hang budget");
   else
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Continuous_Submit_Tests;
