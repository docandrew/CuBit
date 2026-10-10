with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with CuBit.GPU_Queues;
with CuBit.GPU_Queue_Clients;
with GPU_Queue_Model;
with Intel_GPU_Context_Ledger;
with Intel_GPU_Queue_Admission;
with Intel_GPU_Queue_Service;
with Intel_GPU_Session_Queue;
-- GPU-001 step 2, Linux-hosted: the production queue service, session
-- queue, ledgers, ring windows, admission and wakes against a modelled GPU
-- and GuC, with the production client (CuBit.GPU_Queue_Clients) on the
-- other side of the shared regions. Not hardware evidence.
procedure GPU_Queue_Tests is
   package Q renames CuBit.GPU_Queues;
   package Clients renames CuBit.GPU_Queue_Clients;
   package M renames GPU_Queue_Model;
   package Ledgers renames Intel_GPU_Context_Ledger;
   package Sessions renames Intel_GPU_Session_Queue;
   use type Q.Timeline_Value, Q.Context_State, Q.Fault_Reason, Q.Wake_Result,
            Ledgers.Health, Ledgers.Value, Clients.Token;

   Hang_Budget : constant := 1_000_000;   -- 1 s, as the driver
   package Service is new Intel_GPU_Queue_Service
     (M.Session_Id, M.Select_Context, M.Owner_Ready, M.Batch_Ready, M.Scheduling_Resident, M.Publish_Ready,
      M.Write_Segment, M.Segment_Bytes, M.Kick, M.Read_Timeline, M.Now_Us, M.Quarantine,
      M.Call_Finished, M.Answer_Wake, M.Client_Region, M.Server_Region, Hang_Budget);
   use type Service.Call_Result, Service.Policy.Event_Count;

   T : access Service.Table;
   Client : array (M.Session_Id) of access Clients.Client :=
     [others => new Clients.Client];
   Failures, Checks : Natural := 0;
   Turns : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   procedure Turn is
   begin
      Service.Turn (T.all);
      M.Advance;
      Turns := Turns + 1;
   end Turn;

   function Status (R : Q.Completion_Status) return Unsigned_32 is
     (Q.Completion_Status'Enum_Rep (R));

   procedure Fresh (Contexts : Natural := 1) is
      Opened : Boolean;
   begin
      M.Reset;
      T := new Service.Table;
      for S in M.Session_Id loop
         for C in 0 .. Contexts - 1 loop
            Service.Open_Context (T.all, S, Q.Context_Index (C), 1, M.Setup_Bytes, Opened);
            Check (Opened, "open context");
         end loop;
         Service.Open_Queue (T.all, S, Opened);
         Check (Opened, "open queue");
         Clients.Attach (Client (S).all, M.Client_Region (S), M.Server_Region (S));
      end loop;
   end Fresh;

   function Batch (C : Q.Context_Index := 0) return Clients.Job is
     ((Operation => Q.Execute, Context => C, Handle => 7, GPU => 16#10_0000#, Offset => 0,
       Bytes => 64, First => (others => <>), Second => (others => <>),
       Deadline => Q.No_Deadline));

   procedure Submit (S : M.Session_Id; J : Clients.Job; Tag : out Clients.Token;
                     Signal : out Q.Timeline_Value; OK : out Boolean) is
      Kick : Boolean;
   begin
      Clients.Submit (Client (S).all, J, Tag, Signal, OK, Kick);
   end Submit;

   -- Turn until Count records were reaped from session 1; all Completed.
   procedure Drain (Count : Natural; Label : String) is
      Record_Item : Clients.Q.Completion;
      Got : Boolean;
      Reaped, Bad : Natural := 0;
      Limit : constant Natural := Turns + 100_000;
   begin
      while Reaped < Count and then Turns < Limit loop
         Turn;
         loop
            Clients.Reap (Client (1).all, Record_Item, Got);
            exit when not Got;
            Reaped := Reaped + 1;
            if Record_Item.Answer.Status /= Status (Q.Completed) then
               Bad := Bad + 1;
            end if;
         end loop;
      end loop;
      Check (Reaped = Count and Bad = 0, Label & ": drained, all Completed");
   end Drain;

   -- Total jobs on session 1 context 0, keeping Depth submitted and unreaped.
   procedure Run_Jobs (Total, Depth : Positive; Label : String) is
      Submitted, Reaped : Natural := 0;
      Expected : Q.Timeline_Value := Clients.Next_Signal (Client (1).all, 0);
      Expected_Tag : Clients.Token := 0;   -- the first record's
      Tag : Clients.Token;
      Signal : Q.Timeline_Value;
      OK, Got : Boolean;
      Record_Item : Clients.Q.Completion;
      Limit : constant Natural := Turns + Total * (M.Latency + 2) + 10_000;
      Bad : Natural := 0;
   begin
      Service.Reset_Peak (T.all);
      while Reaped < Total and then Turns < Limit loop
         while Submitted < Total and then Submitted - Reaped < Depth and then
           Clients.Can_Submit (Client (1).all)
         loop
            Submit (1, Batch, Tag, Signal, OK);
            exit when not OK;
            Submitted := Submitted + 1;
         end loop;
         Turn;
         loop
            Clients.Reap (Client (1).all, Record_Item, Got);
            exit when not Got;
            if Expected_Tag = 0 then
               Expected_Tag := Record_Item.Tag;
            end if;
            if Record_Item.Answer.Status /= Status (Q.Completed) or else
              Record_Item.Answer.Value /= Unsigned_64 (Expected) or else
              Record_Item.Tag /= Expected_Tag
            then
               Bad := Bad + 1;
            end if;
            Expected := Expected + 1;
            Expected_Tag := Expected_Tag + 1;
            Reaped := Reaped + 1;
         end loop;
      end loop;
      Check (Reaped = Total, Label & ": every job answered");
      Check (Bad = 0, Label & ": records Completed, in order, with their values");
      Check (M.Overwrites = 0, Label & ": no unretired ring byte overwritten");
      Check (M.Bad_Tails = 0, Label & ": every segment where the window planned it");
      Check (Service.Stats (T.all).In_Flight_Peak <= Ledgers.Max_In_Flight,
             Label & ": in flight within the limit");
      Check (Service.Stats (T.all).In_Flight_Peak >=
               Natural'Min (Natural'Min (Depth, Ledgers.Max_In_Flight),
                            Natural (M.Ring_Bytes / M.Execute_Bytes) - 1) - 1,
             Label & ": many in flight" &
             Natural'Image (Service.Stats (T.all).In_Flight_Peak));
      Check (M.Quarantines = 0, Label & ": no quarantine");
      Check (M.Completed (1, 0) = Unsigned_64 (Expected) - 1, Label & ": GPU reached the last value");
      Put_Line (Label & ":" & Natural'Image (Total) & " jobs, depth" & Natural'Image (Depth) &
                ", in-flight peak" & Natural'Image (Service.Stats (T.all).In_Flight_Peak) &
                ", kicks" & Natural'Image (M.Kicks) & ", turns" & Natural'Image (Turns));
   end Run_Jobs;

   procedure Throughput is
      type Depth_List is array (Positive range <>) of Positive;
      Depths : constant Depth_List := [1, 8, 32, 64];
   begin
      for Depth of Depths loop
         Fresh;
         M.Latency := 1;
         Run_Jobs (40_000, Depth, "depth" & Natural'Image (Depth));
         Check (M.Enables = 1, "one enable for 40,000 jobs");
      end loop;
      -- Kicks refused for backpressure are retried; nothing is lost.
      Fresh;
      M.Backpressure_Every := 3;
      Run_Jobs (5_000, 16, "kick backpressure");
      Check (Service.Stats (T.all).Kick_Retries > 0, "kick retries counted");
   end Throughput;

   -- A full ring holds the head (backpressure) until retirement frees it.
   procedure Ring_Backpressure is
      Tag : Clients.Token;
      Signal : Q.Timeline_Value;
      OK : Boolean;
   begin
      Fresh;
      M.Execute_Bytes := 2_048;   -- seven segments fill the ring
      M.Latency := 50;
      for I in 1 .. 20 loop
         Submit (1, Batch, Tag, Signal, OK);
         Check (OK, "submit into the queue");
      end loop;
      for I in 1 .. 5 loop
         Turn;
      end loop;
      Check (Ledgers.Count (Sessions.Ledger (Service.Session (T.all, 1), 0)) < 20,
             "a full ring holds descriptors back");
      Check (Sessions.Has_Head (Service.Session (T.all, 1)), "the head waits");
      Drain (20, "full ring");
      Check (Service.Stats (T.all).In_Flight_Peak <= 7, "full ring: at most 7 in flight");
      M.Latency := 2;
      Run_Jobs (400, 20, "big segments");
      Check (M.Overwrites = 0, "big segments: no overwrite");
   end Ring_Backpressure;

   -- The completion ring: 64 records owed fill it; the client stalls only
   -- itself.
   procedure Queue_Backpressure is
      Tag : Clients.Token;
      Signal : Q.Timeline_Value;
      OK : Boolean := True;
      Count : Natural := 0;
   begin
      Fresh;
      M.Latency := 1;
      while OK and then Count < 200 loop
         Submit (1, Batch, Tag, Signal, OK);
         if OK then Count := Count + 1; end if;
      end loop;
      Check (Count = Q.Slots, "the client can have" & Natural'Image (Count) & " outstanding");
      Check (not Clients.Can_Submit (Client (1).all), "a full queue says so");
      for I in 1 .. 300 loop
         Turn;
      end loop;
      Check (not Clients.Can_Submit (Client (1).all), "unreaped records still hold slots");
      Drain (Q.Slots, "after backpressure");
      Run_Jobs (640, 64, "after backpressure");
   end Queue_Backpressure;

   procedure Expect_Record (S : M.Session_Id; Want : Q.Completion_Status; What : String;
                            Detail : Q.Fault_Reason := Q.None) is
      Record_Item : Clients.Q.Completion;
      Got : Boolean;
   begin
      Clients.Reap (Client (S).all, Record_Item, Got);
      Check (Got and then Record_Item.Answer.Status = Status (Want),
             What & ": status" & Unsigned_32'Image (Record_Item.Answer.Status));
      if Detail /= Q.None then
         Check (Record_Item.Answer.Detail = Q.Fault_Reason'Enum_Rep (Detail),
                What & ": detail" & Unsigned_32'Image (Record_Item.Answer.Detail));
      end if;
   end Expect_Record;

   procedure Deadlines is
      Tag : Clients.Token;
      Signal : Q.Timeline_Value;
      OK : Boolean;
      J : Clients.Job := Batch;
   begin
      -- Expired before it was taken: Deadline_Expired, its context faults,
      -- and the next descriptor gets Context_Faulted.
      Fresh;
      J.Deadline := Q.Deadline_Us (M.Clock - 1);
      Submit (1, J, Tag, Signal, OK);
      Submit (1, Batch, Tag, Signal, OK);
      Turn;
      Expect_Record (1, Q.Deadline_Expired, "expired", Q.Deadline);
      Expect_Record (1, Q.Context_Faulted, "after the fault");
      Check (Clients.State (Client (1).all, 0) = Q.Faulted, "status line Faulted");
      -- In flight past its deadline: the watchdog loses the context.
      Fresh;
      M.Latency := 500;
      J := Batch;
      J.Deadline := Q.Deadline_Us (M.Clock + 20_000);
      Submit (1, J, Tag, Signal, OK);
      Submit (1, Batch, Tag, Signal, OK);
      for I in 1 .. 25 loop
         Turn;
      end loop;
      Expect_Record (1, Q.Device_Lost, "deadline in flight", Q.Deadline);
      Expect_Record (1, Q.Device_Lost, "behind it");
      Check (M.Quarantined (1), "deadline: session quarantined");
      Check (Clients.State (Client (1).all, 0) = Q.Hung, "status line Hung");
   end Deadlines;

   procedure Hang is
      Tag : Clients.Token;
      Signal : Q.Timeline_Value;
      OK, Got : Boolean;
      Record_Item : Clients.Q.Completion;
      Lost, Completed : Natural := 0;
   begin
      Fresh;
      M.Latency := 2;
      for I in 1 .. 10 loop
         Submit (1, Batch, Tag, Signal, OK);
      end loop;
      for I in 1 .. 7 loop Turn; end loop;
      M.Hung := True;
      for I in 1 .. 999 loop Turn; end loop;
      Check (not M.Quarantined (1), "a slow GPU is not hung before the budget");
      for I in 1 .. 5 loop Turn; end loop;
      loop
         Clients.Reap (Client (1).all, Record_Item, Got);
         exit when not Got;
         if Record_Item.Answer.Status = Status (Q.Completed) then
            Completed := Completed + 1;
            Check (Record_Item.Answer.Value <= M.Completed (1, 0), "OK only for completed work");
         elsif Record_Item.Answer.Status = Status (Q.Device_Lost) then
            Lost := Lost + 1;
            Check (Record_Item.Answer.Value > M.Completed (1, 0), "lost only for unfinished work");
         end if;
      end loop;
      Check (Completed + Lost = 10, "hang: every job answered once");
      Check (Lost > 0, "hang: device lost, never OK");
      Check (M.Quarantined (1), "hang: session quarantined");
      Check (Clients.State (Client (1).all, 0) = Q.Hung, "hang: status Hung");
      -- A timeline that goes back is never accepted.
      Fresh;
      Submit (1, Batch, Tag, Signal, OK);
      Submit (1, Batch, Tag, Signal, OK);
      for I in 1 .. 4 loop Turn; end loop;
      M.Regress := True;
      for I in 1 .. 4 loop Turn; end loop;
      Expect_Record (1, Q.Completed, "regress: first completed before it");
      Expect_Record (1, Q.Device_Lost, "regress: lost", Q.Timeline_Fault);
      Check (Clients.State (Client (1).all, 0) = Q.Lost, "regress: status Lost");
      -- A timeline past what was published is never accepted either.
      Fresh;
      Submit (1, Batch, Tag, Signal, OK);
      Turn;
      M.Ahead := True;
      Turn; Turn;
      Expect_Record (1, Q.Device_Lost, "ahead: lost", Q.Timeline_Fault);
      Check (Clients.State (Client (1).all, 0) = Q.Lost, "ahead: status Lost");
      -- A read that never settles is a fault too.
      Fresh;
      Submit (1, Batch, Tag, Signal, OK);
      M.Torn := True;
      Turn; Turn;
      Expect_Record (1, Q.Device_Lost, "torn reads", Q.Timeline_Fault);
   end Hang;

   procedure Malformed is
      Tag : Clients.Token;
      Signal : Q.Timeline_Value;
      OK : Boolean;
      J : Clients.Job;
   begin
      Fresh (Contexts => 2);
      J := Batch; J.Handle := M.Unmapped_Handle;
      Submit (1, J, Tag, Signal, OK);
      Turn;
      Expect_Record (1, Q.Rejected, "unmapped batch", Q.Batch_Not_Mapped);
      Check (Clients.State (Client (1).all, 0) = Q.Faulted, "unmapped batch faults the context");
      -- Context 1 is unaffected; malformed ones fault it one by one.
      J := Batch (1); J.Bytes := 3;
      Submit (1, J, Tag, Signal, OK);
      Turn;
      Expect_Record (1, Q.Rejected, "bad batch", Q.Bad_Batch);
      Fresh (Contexts => 1);
      J := Batch (2);
      Submit (1, J, Tag, Signal, OK);
      Turn;
      Expect_Record (1, Q.Rejected, "unopened context", Q.Bad_Context);
      J := Batch; J.Operation := Q.VM_Bind;
      Submit (1, J, Tag, Signal, OK);
      Turn;
      Expect_Record (1, Q.Rejected, "step 5 opcode", Q.Unsupported_Opcode);
      Check (Clients.State (Client (1).all, 0) = Q.Faulted, "unsupported opcode faults");
      -- A wait on a value never accepted could never be reached in order.
      Fresh;
      J := Batch; J.First := (Context => 0, Target => 50);
      Submit (1, J, Tag, Signal, OK);
      Turn;
      Expect_Record (1, Q.Rejected, "wait on the future", Q.Bad_Wait);
      -- The decision itself, on raw descriptors a client could write.
      declare
         package A renames Intel_GPU_Queue_Admission;
         View : A.Session_View;
         D : Q.Descriptor := (Operation => 1, Flags => 0, Context => 0, Wait_1_Context => 0,
                              Wait_2_Context => 0, Batch_Handle => 7, Signal_Value => 6,
                              Batch_GPU => 16#10_0000#, Batch_Offset => 0, Batch_Bytes => 64,
                              Wait_1_Value => 0, Wait_2_Value => 0, Deadline => Unsigned_64'Last);
         function Verdict (X : Q.Descriptor; Quiescing : Boolean := False) return A.Decision is
           (A.Decide (X, View, Quiescing, 100, Ledgers.Max_In_Flight));
         use type A.Verdict;
      begin
         View (0) := (Open => True, Taking => True, Failed => False, Accepted => 5,
                      Completed => 3, Owed => 2);
         View (1) := (Open => True, Taking => True, Failed => False, Accepted => 9,
                      Completed => 9, Owed => 0);
         Check (Verdict (D).Kind = A.Admit, "decide: admit");
         Check (Verdict (D, True).Kind = A.Await_Quiesce, "decide: quiesce holds");
         Check (Verdict ((D with delta Signal_Value => 7)).Cause = Q.Signal_Mismatch,
                "decide: signal mismatch");
         Check (Verdict ((D with delta Signal_Value => 5)).Kind = A.Reject, "decide: old value");
         Check (Verdict ((D with delta Operation => 9)).Cause = Q.Bad_Opcode, "decide: opcode");
         Check (Verdict ((D with delta Flags => 1)).Cause = Q.Bad_Flags, "decide: flags");
         Check (Verdict ((D with delta Context => 3)).Cause = Q.Bad_Context, "decide: context");
         Check (Verdict ((D with delta Wait_1_Context => 1, Wait_1_Value => 10)).Cause = Q.Bad_Wait,
                "decide: wait beyond accepted");
         Check (Verdict ((D with delta Wait_1_Context => 0, Wait_1_Value => 4)).Kind = A.Await_Waits,
                "decide: wait not reached");
         Check (Verdict ((D with delta Wait_2_Context => 1, Wait_2_Value => 9)).Kind = A.Admit,
                "decide: wait reached");
         Check (Verdict ((D with delta Wait_1_Context => 2, Wait_1_Value => 0)).Cause = Q.Bad_Wait,
                "decide: no-wait with a context");
         Check (Verdict ((D with delta Batch_Offset => 4)).Cause = Q.Bad_Batch, "decide: offset");
         Check (Verdict ((D with delta Operation => 2)).Cause = Q.Bad_Batch,
                "decide: signal with batch fields");
         Check (Verdict ((D with delta Operation => 2, Batch_Handle => 0, Batch_GPU => 0,
                          Batch_Bytes => 0)).Kind = A.Admit, "decide: signal");
         Check (Verdict ((D with delta Deadline => 100)).Kind = A.Expire, "decide: expired");
         View (0).Owed := Ledgers.Max_In_Flight;
         Check (Verdict (D).Kind = A.Await_Capacity, "decide: capacity");
         View (0).Taking := False;
         Check (Verdict ((D with delta Signal_Value => 99)).Kind = A.Refuse_Faulted,
                "decide: faulted context first");
         View (0) := (Open => True, Taking => True, Failed => False, Accepted => 5,
                      Completed => 3, Owed => 2);
         View (1).Failed := True;
         View (1).Completed := 7;
         Check (Verdict ((D with delta Wait_1_Context => 1, Wait_1_Value => 8)).Kind = A.Refuse_Lost,
                "decide: wait on a lost context");
      end;
   end Malformed;

   procedure Cross_Context_Waits is
      Tag : Clients.Token;
      Signal, Waited : Q.Timeline_Value;
      OK, Got : Boolean;
      J : Clients.Job;
      Record_Item : Clients.Q.Completion;
      Done_First : Boolean := False;
   begin
      Fresh (Contexts => 2);
      M.Latency := 20;
      Submit (1, Batch (0), Tag, Waited, OK);
      J := Batch (1); J.First := (Context => 0, Target => Waited);
      Submit (1, J, Tag, Signal, OK);
      for I in 1 .. 10 loop Turn; end loop;
      Check (Ledgers.Count (Sessions.Ledger (Service.Session (T.all, 1), 1)) = 0,
             "the waiting job is not published before its wait");
      for I in 1 .. 200 loop
         Turn;
         loop
            Clients.Reap (Client (1).all, Record_Item, Got);
            exit when not Got;
            Check (Record_Item.Answer.Status = Status (Q.Completed), "cross wait completed");
            if Record_Item.Answer.Context = 0 then
               Done_First := True;
            else
               Check (Done_First, "context 1 ran after context 0's value");
            end if;
         end loop;
      end loop;
      Check (M.Completed (1, 1) = Unsigned_64 (Signal), "context 1 reached its value");
   end Cross_Context_Waits;

   procedure Wakes is
      Tag : Clients.Token;
      Signal : Q.Timeline_Value;
      OK, Answer_Held, Answer_Now : Boolean;
      Result : Q.Wake_Result;
   begin
      Fresh;
      M.Latency := 10;
      Submit (1, Batch, Tag, Signal, OK);
      Service.Wake_Request (T.all, 1, 0, Sessions.Value (Signal), Answer_Held, Answer_Now, Result);
      Check (not Answer_Now and not Answer_Held, "wake held while unreached");
      -- Another session cannot hold one meanwhile: one saved-reply slot.
      Service.Wake_Request (T.all, 2, 0, 5, Answer_Held, Answer_Now, Result);
      Check (Answer_Now and then Result = Q.Not_Held, "second session: Not_Held");
      for I in 1 .. 5 loop Turn; end loop;
      Check (M.Wakes_Answered = 0, "not answered early");
      for I in 1 .. 20 loop Turn; end loop;
      Check (M.Wakes_Answered = 1 and then M.Last_Wake = Q.Woken, "answered Woken on reach");
      Check (not Service.Wake_Slot_Held (T.all, 1), "slot released");
      -- Reached already: answered at once.
      Service.Wake_Request (T.all, 1, 0, Sessions.Value (Signal), Answer_Held, Answer_Now, Result);
      Check (Answer_Now and then Result = Q.Woken, "reached: at once");
      -- Superseded: the held one is answered first.
      Submit (1, Batch, Tag, Signal, OK);
      Service.Wake_Request (T.all, 1, 0, Sessions.Value (Signal), Answer_Held, Answer_Now, Result);
      Service.Wake_Request (T.all, 1, 0, Sessions.Value (Signal), Answer_Held, Answer_Now, Result);
      Check (Answer_Held and not Answer_Now, "superseded: held one answered, new one held");
      -- The queue ends: the held one is answered No_Queue.
      Service.End_Queue (T.all, 1);
      Check (M.Last_Wake = Q.No_Queue, "queue end answers the wake");
      Service.Wake_Request (T.all, 1, 0, 1, Answer_Held, Answer_Now, Result);
      Check (Answer_Now and then Result = Q.No_Queue, "no queue: answered at once");
      -- A faulted context answers waits it can no longer meet.
      Fresh;
      declare
         J : Clients.Job := Batch;
      begin
         J.Handle := M.Unmapped_Handle;
         Submit (1, J, Tag, Signal, OK);
         Turn;
         Service.Wake_Request (T.all, 1, 0, Sessions.Value (Signal), Answer_Held, Answer_Now, Result);
         Check (Answer_Now and then Result = Q.Woken, "faulted: wake answered");
      end;
   end Wakes;

   procedure Calls is
      Result : Service.Call_Result;
   begin
      -- The synchronous wrapper: one call at a time, each answered once.
      Fresh;
      M.Latency := 2;
      Service.End_Queue (T.all, 2);
      for I in 1 .. 1_000 loop
         Service.Submit_Call (T.all, 2, 0, 7, 16#10_0000#, 0, 64, Hang_Budget, Result);
         Check (Result = Service.Submitted, "call submitted");
         Check (not Service.Call_Room (T.all, 2, 0), "no room for a second call");
         Service.Submit_Call (T.all, 2, 0, 7, 16#10_0000#, 0, 64, Hang_Budget, Result);
         Check (Result = Service.Busy, "a second call waits (deferred, not refused)");
         for J in 1 .. 4 loop Turn; end loop;
      end loop;
      Check (M.Calls_OK = 1_000 and M.Calls_Failed = 0, "1000 calls completed");
      Check (M.Last_Call_Value = 1_001, "64-bit values consecutive from the setup value");
      -- A queue opened later starts from the status line.
      declare
         Opened : Boolean;
      begin
         Service.Open_Queue (T.all, 2, Opened);
         Check (Opened, "reopen session 2's queue");
         Clients.Attach (Client (2).all, M.Client_Region (2), M.Server_Region (2));
         Check (Clients.Next_Signal (Client (2).all, 0) = 1_002,
                "the client's next value follows the calls");
         Service.End_Queue (T.all, 2);
      end;
      Service.Submit_Call (T.all, 2, 0, 7, 16#10_0000#, 3, 64, Hang_Budget, Result);
      Check (Result = Service.Malformed, "misaligned call batch");
      Service.Submit_Call (T.all, 2, 0, Unsigned_64 (M.Unmapped_Handle), 16#10_0000#, 0, 64,
                           Hang_Budget, Result);
      Check (Result = Service.Denied, "unmapped call batch");
      -- Queue jobs on session 1 run at the same time.
      Run_Jobs (2_000, 16, "queue beside calls");
      -- A hung call is answered failed, never OK.
      M.Hung := True;
      Service.Submit_Call (T.all, 2, 0, 7, 16#10_0000#, 0, 64, Hang_Budget, Result);
      for J in 1 .. 1_100 loop Turn; end loop;
      Check (M.Calls_Failed = 1, "hung call answered failed");
   end Calls;

   procedure Quiesce is
      Tag : Clients.Token;
      Signal : Q.Timeline_Value;
      OK : Boolean;
   begin
      Fresh;
      M.Latency := 5;
      for I in 1 .. 4 loop Submit (1, Batch, Tag, Signal, OK); end loop;
      Turn;
      Service.Set_Quiesce (T.all, True);
      Check (not Service.Park_Allowed (T.all), "no park with jobs in flight");
      for I in 1 .. 5 loop Submit (1, Batch, Tag, Signal, OK); end loop;
      for I in 1 .. 60 loop Turn; end loop;
      Check (Service.Idle (T.all), "in-flight work drains");
      Check (Service.Park_Allowed (T.all), "park allowed once idle and quiescing");
      Check (Ledgers.Accepted (Sessions.Ledger (Service.Session (T.all, 1), 0)) = 5,
             "nothing published while quiescing");
      Service.Set_Quiesce (T.all, False);
      for I in 1 .. 60 loop Turn; end loop;
      Check (Ledgers.Accepted (Sessions.Ledger (Service.Session (T.all, 1), 0)) = 10,
             "admission resumes");
   end Quiesce;

   procedure Gate is
      Tag : Clients.Token;
      Signal : Q.Timeline_Value;
      OK : Boolean;
   begin
      -- The first job's enable is acknowledged a turn after the GPU may
      -- already have finished it: completion waits for the acknowledgement.
      Fresh;
      M.Latency := 1;
      M.Enable_Delay := 3;
      Submit (1, Batch, Tag, Signal, OK);
      Turn;   -- published, enable kicked
      Turn;   -- the GPU has finished; MODE_DONE has not come
      Check (M.Completed (1, 0) = Unsigned_64 (Signal), "the GPU finished early");
      Turn;
      Turn;
      Check (not Clients.Reached (Client (1).all, 0, Signal), "not complete before MODE_DONE");
      for I in 1 .. 3 loop Turn; end loop;
      Check (Clients.Reached (Client (1).all, 0, Signal), "complete once enabled and reached");
      Expect_Record (1, Q.Completed, "gate");
   end Gate;

   procedure Ownership is
      Tag : Clients.Token;
      Signal : Q.Timeline_Value;
      OK : Boolean;
   begin
      Fresh;
      Submit (1, Batch, Tag, Signal, OK);
      Turn;
      M.Ownership := False;
      Turn;
      Expect_Record (1, Q.Device_Lost, "ownership lost", Q.Device_Fault);
      Check (M.Quarantined (1), "ownership loss quarantines");
   end Ownership;

begin
   Throughput;
   Ring_Backpressure;
   Queue_Backpressure;
   Deadlines;
   Hang;
   Malformed;
   Cross_Context_Waits;
   Wakes;
   Calls;
   Quiesce;
   Gate;
   Ownership;
   if Failures = 0 then
      Put_Line ("GPU queue PASS:" & Natural'Image (Checks) & " checks");
   else
      Put_Line ("GPU queue FAIL:" & Natural'Image (Failures) & " of" & Natural'Image (Checks));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end GPU_Queue_Tests;
