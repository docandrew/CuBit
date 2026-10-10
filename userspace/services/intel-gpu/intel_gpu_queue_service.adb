with System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Timeline;
package body Intel_GPU_Queue_Service is
   package QQ renames Q.Queues;
   package Admission renames Sessions.Admission;
   package Windows renames Sessions.Windows;
   package Wakes renames Sessions.Wakes;
   use type Ledgers.Health, Ledgers.Observation, Ledgers.Origin, Ledgers.Pop_Status,
            Policy.Kick_Result, Q.Opcode, Value, Sessions.Microseconds,
            Admission.Verdict, Intel_GPU_Ring_Reservation.Outcome, Wakes.Phase,
            QQ.Submissions.Index, Policy.Event_Count;

   subtype Microseconds is Sessions.Microseconds;

   -- Records for a queue that is not live go nowhere (Pop writes none).
   Discard : QQ.Completions.Ring;

   -- The compiler keeps ring and record writes before the index stores
   -- that publish them, and index loads before the reads they guard (x86
   -- keeps the order in hardware).
   procedure Barrier is
   begin
      System.Machine_Code.Asm ("", Volatile => True, Clobber => "memory");
   end Barrier;
   procedure Fence is
   begin
      System.Machine_Code.Asm ("mfence", Volatile => True, Clobber => "memory");
   end Fence;

   function At_Offset (Base : System.Address; Offset : Natural) return System.Address is
     (Base + Storage_Offset (Offset));

   function Stats (T : Table) return Statistics is (T.Counters);
   procedure Reset_Peak (T : in out Table) is
   begin
      T.Counters.In_Flight_Peak := T.Counters.In_Flight;
   end Reset_Peak;
   procedure Reset_Latency (T : in out Table) is
   begin
      T.Counters.Latency_Count := 0;
      T.Counters.Latency_Min_Us := 0;
      T.Counters.Latency_Max_Us := 0;
      T.Counters.Latency_Total_Us := 0;
   end Reset_Latency;

   function Session (T : Table; S : Session_Id) return Sessions.Session is (T.Items (S));
   function Live (T : Table; S : Session_Id) return Boolean is (Sessions.Live (T.Items (S)));
   function Session_Idle (T : Table; S : Session_Id) return Boolean is
     (Sessions.Idle (T.Items (S)));
   function Idle (T : Table) return Boolean is
     (for all S in Session_Id => Sessions.Idle (T.Items (S)));
   function Park_Allowed (T : Table) return Boolean is
     (for all S in Session_Id => Sessions.Park_Allowed (T.Items (S)));
   function Wake_Slot_Held (T : Table; S : Session_Id) return Boolean is
     (T.Wake_Owned and then T.Wake_Owner = S);

   function Produced_Index (S : Session_Id) return QQ.Submissions.Index is
      Word : constant Unsigned_32 with Import, Volatile,
        Address => At_Offset (Client_Region (S), Q.Client_Submitted_At);
   begin
      return QQ.Submissions.Index (Word);
   end Produced_Index;

   function Reaped_Index (S : Session_Id) return QQ.Completions.Index is
      Word : constant Unsigned_32 with Import, Volatile,
        Address => At_Offset (Client_Region (S), Q.Client_Reaped_At);
   begin
      return QQ.Completions.Index (Word);
   end Reaped_Index;

   function Work_Waiting (T : Table) return Boolean is
   begin
      for S in Session_Id loop
         if Sessions.Live (T.Items (S)) and then
           Produced_Index (S) /= Sessions.Server (T.Items (S)).Requests.Consumed
         then
            return True;
         end if;
      end loop;
      return False;
   end Work_Waiting;

   procedure Release_Wake_Slot (T : in out Table; S : Session_Id) is
   begin
      if T.Wake_Owned and then T.Wake_Owner = S and then
        Sessions.Wake (T.Items (S)).Current = Wakes.Idle
      then
         T.Wake_Owned := False;
      end if;
   end Release_Wake_Slot;

   procedure Note_Latency (T : in out Table; Submitted, Now : Microseconds) is
      Took : Unsigned_64;
   begin
      if Now = Intel_GPU_Timeline.Clock_Unavailable or else Now < Submitted then
         return;
      end if;
      Took := Unsigned_64 (Now - Submitted);
      if T.Counters.Latency_Count = 0 or else Took < T.Counters.Latency_Min_Us then
         T.Counters.Latency_Min_Us := Took;
      end if;
      if Took > T.Counters.Latency_Max_Us then
         T.Counters.Latency_Max_Us := Took;
      end if;
      T.Counters.Latency_Count := T.Counters.Latency_Count + 1;
      T.Counters.Latency_Total_Us := T.Counters.Latency_Total_Us + Took;
   end Note_Latency;

   -- Pop every job of S's context C that can be popped.
   procedure Pop_Ready (T : in out Table; S : Session_Id; C : Q.Context_Index;
                        Now : Microseconds) is
      Item : Ledgers.Job;
      V : Value;
      Source : Ledgers.Origin;
      Status : Ledgers.Pop_Status;
   begin
      for Bound in 1 .. Ledgers.Max_In_Flight loop
         exit when not Sessions.Can_Pop (T.Items (S), C);
         if Sessions.Live (T.Items (S)) then
            declare
               Ring : QQ.Completions.Ring with Import,
                 Address => At_Offset (Server_Region (S), Q.Server_Completions_At);
            begin
               Sessions.Pop (T.Items (S), C, Ring, Item, V, Source, Status);
            end;
         else
            Sessions.Pop (T.Items (S), C, Discard, Item, V, Source, Status);
         end if;
         if Status = Ledgers.Done then
            T.Counters.Completed := T.Counters.Completed + 1;
            Note_Latency (T, Item.Submitted, Now);
         else
            T.Counters.Lost := T.Counters.Lost + 1;
         end if;
         if Source = Ledgers.From_Call then
            Call_Finished (S, Unsigned_64 (V), Status = Ledgers.Done);
         end if;
      end loop;
   end Pop_Ready;

   procedure Answer_Held_Wake (T : in out Table; S : Session_Id) is
      Answer : Boolean;
   begin
      if Sessions.Wake (T.Items (S)).Current = Wakes.Held then
         Sessions.Wake_Step (T.Items (S), Sessions.Wake_Holds (T.Items (S)), Answer);
         if Answer then
            Answer_Wake (S, Q.Woken);
            Release_Wake_Slot (T, S);
         end if;
      end if;
   end Answer_Held_Wake;

   -- S's contexts are lost: every owed job fails now, with its record.
   procedure Fail_Core (T : in out Table; S : Session_Id; Why : Q.Fault_Reason;
                        Now : Microseconds) is
   begin
      Sessions.Lose_All (T.Items (S), Why);
      for C in Q.Context_Index loop
         Pop_Ready (T, S, C, Now);
         T.Kicks (S) (C) := No_Kick;
      end loop;
      Answer_Held_Wake (T, S);
   end Fail_Core;

   procedure Fail_Session (T : in out Table; S : Session_Id; Why : Q.Fault_Reason;
                           Now : Microseconds) is
   begin
      Fail_Core (T, S, Why, Now);
      Quarantine (S, Why);
   end Fail_Session;

   procedure Lose_Session (T : in out Table; S : Session_Id; Why : Q.Fault_Reason) is
   begin
      Fail_Core (T, S, Why, Microseconds (Now_Us));
   end Lose_Session;

   procedure Open_Context
     (T : in out Table; S : Session_Id; C : Q.Context_Index; Done : Value;
      First_Bytes : Unsigned_32; Opened : out Boolean) is
   begin
      Opened := False;
      if Sessions.Context_Open (T.Items (S), C) or else
        Ledgers.Count (Sessions.Ledger (T.Items (S), C)) /= 0 or else
        Done >= Ledgers.Last_Usable or else
        First_Bytes not in Windows.Command_Alignment .. Windows.Span_Limit or else
        First_Bytes mod Windows.Command_Alignment /= 0
      then
         return;
      end if;
      Sessions.Open_Context (T.Items (S), C, Done, First_Bytes, Microseconds (Now_Us));
      if T.Quiesce then
         Sessions.Begin_Quiesce (T.Items (S));
      end if;
      Opened := True;
   end Open_Context;

   procedure Write_Status (T : Table; S : Session_Id) is
      Lines : Q.Status_Lines with Import, Volatile,
        Address => At_Offset (Server_Region (S), Q.Server_Status_At);
      Version : Unsigned_64;
   begin
      for C in Q.Context_Index loop
         declare
            L : constant Ledgers.Ledger := Sessions.Ledger (T.Items (S), C);
            State : constant Q.Context_State :=
              (if not Sessions.Context_Open (T.Items (S), C) then Q.Unused
               else (case Ledgers.State (L) is
                       when Ledgers.Active => Q.Active,
                       when Ledgers.Faulted => Q.Faulted,
                       when Ledgers.Hung => Q.Hung,
                       when Ledgers.Lost => Q.Lost));
         begin
            Version := Lines (C).Version;
            Version := (if Version mod 2 = 1 then Version + 1 else Version + 2);
            Lines (C).Version := Version - 1;
            Barrier;
            Lines (C).State := Q.Context_State'Enum_Rep (State);
            Lines (C).Error := Q.Fault_Reason'Enum_Rep (Ledgers.Why (L));
            Lines (C).Accepted := Unsigned_64 (Ledgers.Accepted (L));
            Lines (C).Completed := Unsigned_64 (Ledgers.Completed (L));
            Barrier;
            Lines (C).Version := Version;
         end;
      end loop;
   end Write_Status;

   procedure Open_Queue (T : in out Table; S : Session_Id; Opened : out Boolean) is
   begin
      Opened := False;
      if Sessions.Live (T.Items (S)) or else Sessions.Queued_Total (T.Items (S)) /= 0 then
         return;
      end if;
      declare
         Region : Storage_Array (1 .. Q.Server_Pages * Q.Page_Bytes) with Import, Volatile,
           Address => Server_Region (S);
      begin
         Region := [others => 0];
      end;
      Sessions.Open_Queue (T.Items (S));
      Write_Status (T, S);
      Opened := True;
   end Open_Queue;

   procedure End_Queue (T : in out Table; S : Session_Id) is
      Answer : Boolean;
   begin
      Sessions.End_Queue (T.Items (S), Answer);
      if Answer then
         Answer_Wake (S, Q.No_Queue);
      end if;
      Release_Wake_Slot (T, S);
   end End_Queue;

   procedure Forget_Session (T : in out Table; S : Session_Id; Forgotten : out Boolean) is
   begin
      Forgotten := False;
      if Sessions.Live (T.Items (S)) or else not Sessions.Idle (T.Items (S)) then
         return;
      end if;
      T.Items (S) := Sessions.Empty;
      T.Kicks (S) := [others => No_Kick];
      if T.Quiesce then
         Sessions.Begin_Quiesce (T.Items (S));
      end if;
      Release_Wake_Slot (T, S);
      Forgotten := True;
   end Forget_Session;

   procedure Set_Quiesce (T : in out Table; On : Boolean) is
   begin
      T.Quiesce := On;
      for S in Session_Id loop
         if On then
            Sessions.Begin_Quiesce (T.Items (S));
         else
            Sessions.End_Quiesce (T.Items (S));
         end if;
      end loop;
   end Set_Quiesce;

   -- The selected context needs a kick this turn: the kind follows its
   -- residency when the first segment of the turn is published.
   procedure Note_Kick (T : in out Table; S : Session_Id; C : Q.Context_Index) is
   begin
      if T.Kicks (S) (C) = No_Kick then
         T.Kicks (S) (C) := (if Scheduling_Resident then Notify_Kick else Enable_Kick);
      end if;
   end Note_Kick;

   -- One kick for S's context C. Backpressure keeps it for a later turn.
   procedure Kick_One (T : in out Table; S : Session_Id; C : Q.Context_Index;
                       Now : Microseconds) is
      Outcome : Policy.Kick_Result;
      Enable : constant Boolean := T.Kicks (S) (C) = Enable_Kick;
   begin
      if not Select_Context (S, C) or else not Owner_Ready then
         Fail_Session (T, S, Q.Device_Fault, Now);
         return;
      end if;
      Kick (Enable, Outcome);
      if Outcome = Policy.Kick_Failed or else not Owner_Ready then
         Fail_Session (T, S, Q.Kick_Failed, Now);
      elsif Outcome = Policy.Kick_Queued then
         T.Kicks (S) (C) := No_Kick;
         if Enable then
            T.Counters.Enables := T.Counters.Enables + 1;
         else
            T.Counters.Notifies := T.Counters.Notifies + 1;
         end if;
      else
         T.Counters.Kick_Retries := T.Counters.Kick_Retries + 1;
      end if;
   end Kick_One;

   procedure Send_Kicks (T : in out Table; Now : Microseconds) is
   begin
      for S in Session_Id loop
         for C in Q.Context_Index loop
            if T.Kicks (S) (C) /= No_Kick then
               Kick_One (T, S, C, Now);
            end if;
         end loop;
      end loop;
   end Send_Kicks;

   -- Answer the head without running it.
   procedure Refuse
     (T : in out Table; S : Session_Id; Status : Q.Completion_Status;
      Detail : Q.Fault_Reason; Fault_It : Boolean; C : Q.Context_Index)
   is
      Ring : QQ.Completions.Ring with Import,
        Address => At_Offset (Server_Region (S), Q.Server_Completions_At);
   begin
      Sessions.Refuse_Head (T.Items (S), Ring, Status, Detail, Fault_It, C);
      T.Counters.Refused := T.Counters.Refused + 1;
   end Refuse;

   -- Take and publish S's descriptors in order while they may run.
   procedure Admit (T : in out Table; S : Session_Id; Now : Microseconds) is
      Requests : constant QQ.Submissions.Ring with Import,
        Address => At_Offset (Client_Region (S), Q.Client_Descriptors_At);
      D : Admission.Decision;
   begin
      for Bound in 1 .. Q.Slots loop
         if not Sessions.Has_Head (T.Items (S)) then
            exit when not Sessions.Can_Take (T.Items (S));
            Sessions.Take_Head (T.Items (S), Requests);
            T.Counters.Taken := T.Counters.Taken + 1;
         end if;
         D := Sessions.Decide_Head (T.Items (S), Now);
         case D.Kind is
            when Admission.Admit =>
               declare
                  Head : constant QQ.Submission := Sessions.Head (T.Items (S));
                  C : constant Q.Context_Index := D.Context;
                  Next : constant Value := Value (Head.Item.Signal_Value);
                  Bytes : constant Unsigned_32 := Segment_Bytes (D.Operation);
                  Plan : Intel_GPU_Ring_Reservation.Plan;
                  OK : Boolean;
               begin
                  if not Select_Context (S, C) or else not Owner_Ready then
                     Fail_Session (T, S, Q.Device_Fault, Now);
                     return;
                  end if;
                  -- An enable still unacknowledged: wait for its MODE_DONE.
                  exit when not Publish_Ready;
                  if D.Operation = Q.Execute and then
                    not Batch_Ready (Unsigned_64 (Head.Item.Batch_Handle), Head.Item.Batch_GPU,
                                     Unsigned_64 (Head.Item.Batch_Offset),
                                     Unsigned_64 (Head.Item.Batch_Bytes))
                  then
                     if not Owner_Ready then
                        Fail_Session (T, S, Q.Device_Fault, Now);
                        return;
                     end if;
                     Refuse (T, S, Q.Rejected, Q.Batch_Not_Mapped, True, C);
                  else
                     Plan := Windows.Plan (Sessions.Window (T.Items (S), C), Bytes);
                     -- A full ring holds the head until retirement frees it.
                     exit when Plan.Status /= Intel_GPU_Ring_Reservation.Ready;
                     if not Sessions.Can_Commit (T.Items (S), C, Next, Ledgers.From_Queue, Bytes) then
                        -- Only an exhausted timeline gets here.
                        Refuse (T, S, Q.Rejected, Q.Signal_Mismatch, True, C);
                     else
                        Write_Segment
                          (D.Operation, Unsigned_64 (Next), Head.Item.Batch_GPU, Plan,
                           Windows.Ring_Offset (Windows.Tail (Sessions.Window (T.Items (S), C))),
                           OK);
                        if not OK or else not Owner_Ready then
                           Fail_Session (T, S, Q.Ring_Fault, Now);
                           return;
                        end if;
                        Sessions.Commit_Head
                          (T.Items (S), Bytes,
                           (Token => Unsigned_64 (Head.Tag),
                            Deadline => Microseconds (Head.Item.Deadline),
                            Submitted => Now),
                           Now);
                        Note_Kick (T, S, C);
                     end if;
                  end if;
               end;
            when Admission.Await_Waits | Admission.Await_Capacity | Admission.Await_Quiesce =>
               exit;
            when Admission.Refuse_Faulted =>
               Refuse (T, S, Q.Context_Faulted,
                       Ledgers.Why (Sessions.Ledger (T.Items (S), D.Context)), False, D.Context);
            when Admission.Refuse_Lost =>
               Refuse (T, S, Q.Device_Lost, D.Cause, True, D.Context);
            when Admission.Expire =>
               Refuse (T, S, Q.Deadline_Expired, D.Cause, True, D.Context);
            when Admission.Reject =>
               Refuse (T, S, Q.Rejected, D.Cause, D.Has_Context, D.Context);
         end case;
      end loop;
   end Admit;

   procedure Serve_Session (T : in out Table; S : Session_Id; Now : Microseconds) is
      V : Unsigned_64;
      OK, Gate : Boolean;
      Seen : Ledgers.Observation;
   begin
      if Sessions.Live (T.Items (S)) then
         declare
            Wake : Unsigned_32 with Import, Volatile,
              Address => At_Offset (Server_Region (S), Q.Server_Wake_At);
         begin
            -- Awake: the client need not kick until the word is armed again.
            if Wake /= 0 then
               Wake := 0;
            end if;
         end;
         Sessions.Accept_Reaped (T.Items (S), Reaped_Index (S));
         Sessions.Accept_Produced (T.Items (S), Produced_Index (S));
         -- The descriptors were written before the count: read them after.
         Barrier;
      end if;
      for C in Q.Context_Index loop
         if Sessions.Context_Open (T.Items (S), C) and then
           (Ledgers.Count (Sessions.Ledger (T.Items (S), C)) > 0 or else
            T.Kicks (S) (C) /= No_Kick)
         then
            if not Select_Context (S, C) or else not Owner_Ready then
               Fail_Session (T, S, Q.Device_Fault, Now);
               return;
            end if;
            Read_Timeline (V, OK);
            Gate := T.Kicks (S) (C) = No_Kick and then Scheduling_Resident;
            if not Owner_Ready then
               Fail_Session (T, S, Q.Device_Fault, Now);
               return;
            end if;
            Sessions.Observe (T.Items (S), C, OK, Value (V), Gate, Now,
                              Microseconds (Hang_Budget_Us), Seen);
            if Seen = Ledgers.Failed then
               -- Until reset recovery exists, a hang or a bad timeline is
               -- device loss for the session.
               Fail_Session (T, S, Ledgers.Why (Sessions.Ledger (T.Items (S), C)), Now);
               return;
            end if;
            Pop_Ready (T, S, C, Now);
         end if;
      end loop;
      Answer_Held_Wake (T, S);
      if Sessions.Live (T.Items (S)) and then not T.Quiesce then
         Admit (T, S, Now);
      end if;
   end Serve_Session;

   procedure Publish (T : Table; S : Session_Id) is
      Taken : Unsigned_32 with Import, Volatile,
        Address => At_Offset (Server_Region (S), Q.Server_Taken_At);
      Completed : Unsigned_32 with Import, Volatile,
        Address => At_Offset (Server_Region (S), Q.Server_Completed_At);
   begin
      -- Records are written before the count that hands them over.
      Barrier;
      Completed := Unsigned_32 (Sessions.Server (T.Items (S)).Answers.Produced);
      Taken := Unsigned_32 (Sessions.Server (T.Items (S)).Requests.Consumed);
      Write_Status (T, S);
   end Publish;

   function Has_Work (T : Table; S : Session_Id) return Boolean is
     (Sessions.Live (T.Items (S)) or else not Sessions.Idle (T.Items (S)) or else
      (for some C in Q.Context_Index => T.Kicks (S) (C) /= No_Kick) or else
      Sessions.Wake (T.Items (S)).Current = Wakes.Held);

   procedure Count_In_Flight (T : in out Table) is
      Total : Natural := 0;
   begin
      for S in Session_Id loop
         for C in Q.Context_Index loop
            Total := Total + Ledgers.Count (Sessions.Ledger (T.Items (S), C));
         end loop;
      end loop;
      T.Counters.In_Flight := Total;
      if Total > T.Counters.In_Flight_Peak then
         T.Counters.In_Flight_Peak := Total;
      end if;
   end Count_In_Flight;

   procedure Turn (T : in out Table) is
      Now : constant Microseconds := Microseconds (Now_Us);
   begin
      for S in Session_Id loop
         if Has_Work (T, S) then
            Serve_Session (T, S, Now);
         end if;
      end loop;
      Send_Kicks (T, Now);
      for S in Session_Id loop
         if Sessions.Live (T.Items (S)) then
            Publish (T, S);
         end if;
      end loop;
      Count_In_Flight (T);
   end Turn;

   function Call_Room (T : Table; S : Session_Id; C : Q.Context_Index) return Boolean is
     (Sessions.Context_Open (T.Items (S), C) and then not T.Quiesce and then
      not Sessions.Quiescing (T.Items (S)) and then
      Ledgers.State (Sessions.Ledger (T.Items (S), C)) = Ledgers.Active and then
      Ledgers.Count (Sessions.Ledger (T.Items (S), C)) < Ledgers.Max_In_Flight and then
      not Ledgers.Call_Pending (Sessions.Ledger (T.Items (S), C)) and then
      Windows.Plan (Sessions.Window (T.Items (S), C), Segment_Bytes (Q.Execute)).Status =
        Intel_GPU_Ring_Reservation.Ready);

   procedure Submit_Call
     (T : in out Table; S : Session_Id; C : Q.Context_Index;
      Handle, GPU, Offset, Bytes, Budget_Us : Unsigned_64; Result : out Call_Result)
   is
      Now : constant Microseconds := Microseconds (Now_Us);
      Segment : constant Unsigned_32 := Segment_Bytes (Q.Execute);
      Plan : Intel_GPU_Ring_Reservation.Plan;
      Next : Value;
      OK : Boolean;
   begin
      Result := Faulted;
      if not Policy.Batch_Admissible (Handle, GPU, Offset, Bytes) then
         Result := Malformed;
         return;
      elsif not Sessions.Context_Open (T.Items (S), C) or else
        Ledgers.State (Sessions.Ledger (T.Items (S), C)) /= Ledgers.Active or else
        Now = Intel_GPU_Timeline.Clock_Unavailable or else Budget_Us = 0 or else
        Budget_Us >= Unsigned_64 (Intel_GPU_Timeline.Clock_Unavailable - Now)
      then
         return;
      elsif T.Quiesce or else Sessions.Quiescing (T.Items (S)) or else
        Ledgers.Count (Sessions.Ledger (T.Items (S), C)) >= Ledgers.Max_In_Flight or else
        Ledgers.Call_Pending (Sessions.Ledger (T.Items (S), C))
      then
         Result := Busy;
         return;
      end if;
      if not Select_Context (S, C) or else not Owner_Ready then
         return;
      elsif not Publish_Ready then
         Result := Busy;
         return;
      end if;
      if not Batch_Ready (Handle, GPU, Offset, Bytes) then
         if not Owner_Ready then
            Fail_Session (T, S, Q.Device_Fault, Now);
         else
            Result := Denied;
         end if;
         return;
      end if;
      Next := Ledgers.Accepted (Sessions.Ledger (T.Items (S), C)) + 1;
      Plan := Windows.Plan (Sessions.Window (T.Items (S), C), Segment);
      if Plan.Status /= Intel_GPU_Ring_Reservation.Ready then
         Result := Busy;
         return;
      elsif not Sessions.Can_Commit (T.Items (S), C, Next, Ledgers.From_Call, Segment) then
         -- An exhausted timeline: the context takes no more work.
         Sessions.Fault_Context (T.Items (S), C, Q.Signal_Mismatch);
         return;
      end if;
      Write_Segment (Q.Execute, Unsigned_64 (Next), GPU, Plan,
                     Windows.Ring_Offset (Windows.Tail (Sessions.Window (T.Items (S), C))), OK);
      if not OK or else not Owner_Ready then
         Fail_Session (T, S, Q.Ring_Fault, Now);
         return;
      end if;
      Sessions.Commit_Call
        (T.Items (S), C, Next, Segment,
         (Token => 0, Deadline => Now + Microseconds (Budget_Us), Submitted => Now), Now);
      Note_Kick (T, S, C);
      -- Kick now, not at the next turn: a failure from here on is
      -- answered through Call_Finished, like every later one.
      Kick_One (T, S, C, Now);
      Result := Submitted;
   end Submit_Call;

   procedure Wake_Request
     (T : in out Table; S : Session_Id; C : Q.Context_Index; Target : Value;
      Answer_Held, Answer_Now : out Boolean; Result : out Q.Wake_Result)
   is
      Arrival : Wakes.Arrival;
      Holds : Boolean;
   begin
      Answer_Held := False;
      Answer_Now := True;
      Result := Q.No_Queue;
      if not Sessions.Live (T.Items (S)) or else not Sessions.Context_Open (T.Items (S), C) then
         return;
      end if;
      declare
         L : constant Ledgers.Ledger := Sessions.Ledger (T.Items (S), C);
      begin
         Holds := Wakes.Condition
           (Ledgers.Completed (L), Target,
            Ledgers.State (L) in Ledgers.Failed_Health or else
              (Ledgers.State (L) = Ledgers.Faulted and then Target > Ledgers.Accepted (L)),
            False);
      end;
      Sessions.Wake_Arrive
        (T.Items (S), C, Target, Holds,
         not T.Wake_Owned or else T.Wake_Owner = S, Arrival);
      Answer_Held := Arrival.Answer_Held;
      Answer_Now := Arrival.Answer_Now;
      Result := Arrival.Result;
      if not Answer_Now then
         T.Wake_Owned := True;
         T.Wake_Owner := S;
      else
         Release_Wake_Slot (T, S);
      end if;
   end Wake_Request;

   procedure Wake_Hold_Failed (T : in out Table; S : Session_Id) is
   begin
      if Sessions.Wake (T.Items (S)).Current = Wakes.Held then
         Sessions.Wake_Hold_Failed (T.Items (S));
      end if;
      Release_Wake_Slot (T, S);
   end Wake_Hold_Failed;

   function Arm_Wake_Words (T : in out Table) return Boolean is
      Found : Boolean := False;
   begin
      T.Wake_Epoch := (if T.Wake_Epoch = Unsigned_32'Last then 1 else T.Wake_Epoch + 1);
      for S in Session_Id loop
         if Sessions.Live (T.Items (S)) then
            declare
               Wake : Unsigned_32 with Import, Volatile,
                 Address => At_Offset (Server_Region (S), Q.Server_Wake_At);
            begin
               Wake := T.Wake_Epoch;
               Fence;
               if Produced_Index (S) /= Sessions.Server (T.Items (S)).Requests.Consumed then
                  Found := True;
               end if;
            end;
         end if;
      end loop;
      return Found;
   end Arm_Wake_Words;

end Intel_GPU_Queue_Service;
