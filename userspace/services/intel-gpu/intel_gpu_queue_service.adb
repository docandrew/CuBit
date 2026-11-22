with Ada.Unchecked_Conversion;
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

   Entry_Bytes : constant Unsigned_64 :=
     Unsigned_64 ((Entry_Array'Component_Size + System.Storage_Unit - 1) /
                  System.Storage_Unit);
   function Storage_Bytes (Count : Natural) return Unsigned_64 is
     (Unsigned_64 (Count) * Entry_Bytes);
   procedure Attach (T : in out Table; Storage : System.Address) is
      function To_Entries is new Ada.Unchecked_Conversion (System.Address, Entry_Access);
   begin
      if T.Items = null and then T.Count < Session_Id'First and then
        System."/=" (Storage, System.Null_Address)
      then
         T.Items := To_Entries (Storage);
      end if;
   end Attach;
   procedure Grow (T : in out Table; To : Session_Id'Base; Accepted : out Boolean) is
   begin
      Accepted := T.Items /= null and then To >= T.Count and then To <= Session_Id'Last;
      if not Accepted then return; end if;
      for S in T.Count + 1 .. To loop
         T.Items (S) := (others => <>);
         if T.Quiesce then
            Sessions.Begin_Quiesce (T.Items (S).Item);
         end if;
      end loop;
      T.Count := To;
   end Grow;
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

   function Session (T : Table; S : Session_Id) return Sessions.Session is (T.Items (S).Item);
   function Live (T : Table; S : Session_Id) return Boolean is (Sessions.Live (T.Items (S).Item));
   function Session_Idle (T : Table; S : Session_Id) return Boolean is
     (Sessions.Idle (T.Items (S).Item));
   function Idle (T : Table) return Boolean is
     (for all S in Session_Id'First .. T.Count => Sessions.Idle (T.Items (S).Item));
   function Park_Allowed (T : Table) return Boolean is
     (for all S in Session_Id'First .. T.Count => Sessions.Park_Allowed (T.Items (S).Item));
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
      for S in Session_Id'First .. T.Count loop
         if Sessions.Live (T.Items (S).Item) and then
           Produced_Index (S) /= Sessions.Server (T.Items (S).Item).Requests.Consumed
         then
            return True;
         end if;
      end loop;
      return False;
   end Work_Waiting;

   procedure Release_Wake_Slot (T : in out Table; S : Session_Id) is
   begin
      if T.Wake_Owned and then T.Wake_Owner = S and then
        Sessions.Wake (T.Items (S).Item).Current = Wakes.Idle
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

   function Slot_Of (V : Value) return Job_Slot is
     (Job_Slot (Unsigned_64 (V) mod Q.Max_In_Flight_Per_Context));

   function Kind_Stats (T : Table) return Kind_Table is (T.Kind_Counters);
   procedure Reset_Kinds (T : in out Table) is
   begin
      T.Kind_Counters := [others => (others => <>)];
   end Reset_Kinds;

   -- A completed job's execution time (from its stamps) and latency, under
   -- its kinds. Called with S's context C selected.
   procedure Note_Job (T : in out Table; S : Session_Id; C : Q.Context_Index; V : Value;
                       Submitted, Now : Microseconds) is
      Info : constant Job_Info := T.Items (S).Jobs (C, Slot_Of (V));
      K : Kind_Statistics renames T.Kind_Counters (Info.Kinds);
      Exec : Unsigned_64 := 0;
      OK : Boolean := False;
   begin
      K.Jobs := K.Jobs + 1;
      if Info.Timed then
         Read_Job_Time (Unsigned_64 (V), Exec, OK);
      end if;
      if OK then
         K.Exec_Total_Us := K.Exec_Total_Us + Exec;
         K.Exec_Max_Us := Unsigned_64'Max (K.Exec_Max_Us, Exec);
      else
         K.Untimed := K.Untimed + 1;
      end if;
      if Now /= Intel_GPU_Timeline.Clock_Unavailable and then Now >= Submitted then
         K.Latency_Total_Us := K.Latency_Total_Us + Unsigned_64 (Now - Submitted);
         K.Latency_Max_Us := Unsigned_64'Max (K.Latency_Max_Us, Unsigned_64 (Now - Submitted));
      end if;
   end Note_Job;

   -- Pop every job of S's context C that can be popped.
   procedure Pop_Ready (T : in out Table; S : Session_Id; C : Q.Context_Index;
                        Now : Microseconds) is
      Item : Ledgers.Job;
      V : Value;
      Source : Ledgers.Origin;
      Status : Ledgers.Pop_Status;
   begin
      for Bound in 1 .. Ledgers.Max_In_Flight loop
         exit when not Sessions.Can_Pop (T.Items (S).Item, C);
         if Sessions.Live (T.Items (S).Item) then
            declare
               Ring : QQ.Completions.Ring with Import,
                 Address => At_Offset (Server_Region (S), Q.Server_Completions_At);
            begin
               Sessions.Pop (T.Items (S).Item, C, Ring, Item, V, Source, Status);
            end;
         else
            Sessions.Pop (T.Items (S).Item, C, Discard, Item, V, Source, Status);
         end if;
         if Status = Ledgers.Done then
            T.Counters.Completed := T.Counters.Completed + 1;
            Note_Latency (T, Item.Submitted, Now);
            Note_Job (T, S, C, V, Item.Submitted, Now);
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
      if Sessions.Wake (T.Items (S).Item).Current = Wakes.Held then
         Sessions.Wake_Step (T.Items (S).Item, Sessions.Wake_Holds (T.Items (S).Item), Answer);
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
      Sessions.Lose_All (T.Items (S).Item, Why);
      for C in Q.Context_Index loop
         Pop_Ready (T, S, C, Now);
         T.Items (S).Kicks (C) := No_Kick;
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
      if Sessions.Context_Open (T.Items (S).Item, C) or else
        Ledgers.Count (Sessions.Ledger (T.Items (S).Item, C)) /= 0 or else
        Done >= Ledgers.Last_Usable or else
        First_Bytes not in Windows.Command_Alignment .. Windows.Span_Limit or else
        First_Bytes mod Windows.Command_Alignment /= 0
      then
         return;
      end if;
      Sessions.Open_Context (T.Items (S).Item, C, Done, First_Bytes, Microseconds (Now_Us));
      if T.Quiesce then
         Sessions.Begin_Quiesce (T.Items (S).Item);
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
            L : constant Ledgers.Ledger := Sessions.Ledger (T.Items (S).Item, C);
            State : constant Q.Context_State :=
              (if not Sessions.Context_Open (T.Items (S).Item, C) then Q.Unused
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
      if Sessions.Live (T.Items (S).Item) or else Sessions.Queued_Total (T.Items (S).Item) /= 0 then
         return;
      end if;
      declare
         Region : Storage_Array (1 .. Q.Server_Pages * Q.Page_Bytes) with Import, Volatile,
           Address => Server_Region (S);
      begin
         Region := [others => 0];
      end;
      Sessions.Open_Queue (T.Items (S).Item);
      Write_Status (T, S);
      Opened := True;
   end Open_Queue;

   procedure End_Queue (T : in out Table; S : Session_Id) is
      Answer : Boolean;
   begin
      Sessions.End_Queue (T.Items (S).Item, Answer);
      if Answer then
         Answer_Wake (S, Q.No_Queue);
      end if;
      Release_Wake_Slot (T, S);
   end End_Queue;

   procedure Forget_Session (T : in out Table; S : Session_Id; Forgotten : out Boolean) is
   begin
      Forgotten := False;
      if Sessions.Live (T.Items (S).Item) or else not Sessions.Idle (T.Items (S).Item) then
         return;
      end if;
      T.Items (S) := (others => <>);
      if T.Quiesce then
         Sessions.Begin_Quiesce (T.Items (S).Item);
      end if;
      Release_Wake_Slot (T, S);
      Forgotten := True;
   end Forget_Session;

   procedure Set_Quiesce (T : in out Table; On : Boolean) is
   begin
      T.Quiesce := On;
      for S in Session_Id'First .. T.Count loop
         if On then
            Sessions.Begin_Quiesce (T.Items (S).Item);
         else
            Sessions.End_Quiesce (T.Items (S).Item);
         end if;
      end loop;
   end Set_Quiesce;

   -- The selected context needs a kick this turn: the kind follows its
   -- residency when the first segment of the turn is published.
   procedure Note_Kick (T : in out Table; S : Session_Id; C : Q.Context_Index) is
   begin
      if T.Items (S).Kicks (C) = No_Kick then
         T.Items (S).Kicks (C) := (if Scheduling_Resident then Notify_Kick else Enable_Kick);
      end if;
   end Note_Kick;

   -- One kick for S's context C. Backpressure keeps it for a later turn.
   procedure Kick_One (T : in out Table; S : Session_Id; C : Q.Context_Index;
                       Now : Microseconds) is
      Outcome : Policy.Kick_Result;
      Enable : constant Boolean := T.Items (S).Kicks (C) = Enable_Kick;
   begin
      if not Select_Context (S, C) or else not Owner_Ready then
         Fail_Session (T, S, Q.Device_Fault, Now);
         return;
      end if;
      Kick (Enable, Outcome);
      if Outcome = Policy.Kick_Failed or else not Owner_Ready then
         Fail_Session (T, S, Q.Kick_Failed, Now);
      elsif Outcome = Policy.Kick_Queued then
         T.Items (S).Kicks (C) := No_Kick;
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
      for S in Session_Id'First .. T.Count loop
         for C in Q.Context_Index loop
            if T.Items (S).Kicks (C) /= No_Kick then
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
      Sessions.Refuse_Head (T.Items (S).Item, Ring, Status, Detail, Fault_It, C);
      T.Counters.Refused := T.Counters.Refused + 1;
   end Refuse;

   -- Take and publish S's descriptors in order while they may run.
   procedure Admit (T : in out Table; S : Session_Id; Now : Microseconds) is
      Requests : constant QQ.Submissions.Ring with Import,
        Address => At_Offset (Client_Region (S), Q.Client_Descriptors_At);
      D : Admission.Decision;
   begin
      for Bound in 1 .. Q.Slots loop
         if not Sessions.Has_Head (T.Items (S).Item) then
            exit when not Sessions.Can_Take (T.Items (S).Item);
            Sessions.Take_Head (T.Items (S).Item, Requests);
            T.Counters.Taken := T.Counters.Taken + 1;
         end if;
         D := Sessions.Decide_Head (T.Items (S).Item, Now);
         case D.Kind is
            when Admission.Admit =>
               declare
                  Head : constant QQ.Submission := Sessions.Head (T.Items (S).Item);
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
                     Plan := Windows.Plan (Sessions.Window (T.Items (S).Item, C), Bytes);
                     -- A full ring holds the head until retirement frees it.
                     exit when Plan.Status /= Intel_GPU_Ring_Reservation.Ready;
                     if not Sessions.Can_Commit (T.Items (S).Item, C, Next, Ledgers.From_Queue, Bytes) then
                        -- Only an exhausted timeline gets here.
                        Refuse (T, S, Q.Rejected, Q.Signal_Mismatch, True, C);
                     else
                        Write_Segment
                          (D.Operation, Unsigned_64 (Next), Head.Item.Batch_GPU, Plan,
                           Windows.Ring_Offset (Windows.Tail (Sessions.Window (T.Items (S).Item, C))),
                           OK);
                        if not OK or else not Owner_Ready then
                           Fail_Session (T, S, Q.Ring_Fault, Now);
                           return;
                        end if;
                        Sessions.Commit_Head
                          (T.Items (S).Item, Bytes,
                           (Token => Unsigned_64 (Head.Tag),
                            Deadline => Microseconds (Head.Item.Deadline),
                            Submitted => Now),
                           Now);
                        -- Admission let only the kind bits through.
                        T.Items (S).Jobs (C, Slot_Of (Next)) :=
                          (Kinds => CuBit.GPU_Queues.Kinds.Job_Kinds
                             (Head.Item.Flags and CuBit.GPU_Queues.Kinds.Kinds_Mask),
                           Timed => D.Operation = Q.Execute);
                        Note_Kick (T, S, C);
                     end if;
                  end if;
               end;
            when Admission.Await_Waits | Admission.Await_Capacity | Admission.Await_Quiesce =>
               exit;
            when Admission.Refuse_Faulted =>
               Refuse (T, S, Q.Context_Faulted,
                       Ledgers.Why (Sessions.Ledger (T.Items (S).Item, D.Context)), False, D.Context);
            when Admission.Refuse_Lost =>
               Refuse (T, S, Q.Device_Lost, D.Cause, True, D.Context);
            when Admission.Expire =>
               Refuse (T, S, Q.Deadline_Expired, D.Cause, True, D.Context);
            when Admission.Reject =>
               Refuse (T, S, Q.Rejected, D.Cause, D.Has_Context, D.Context);
         end case;
      end loop;
   end Admit;

   procedure Publish (T : Table; S : Session_Id);
   -- The patience is never shorter than the budget (Observe's contract).
   Patience : constant Unsigned_64 := Unsigned_64'Max (Patience_Us, Hang_Budget_Us);
   procedure Serve_Session (T : in out Table; S : Session_Id; Now : Microseconds) is
      V : Unsigned_64;
      OK, Gate : Boolean;
      Seen : Ledgers.Observation;
   begin
      if Sessions.Live (T.Items (S).Item) then
         declare
            Wake : Unsigned_32 with Import, Volatile,
              Address => At_Offset (Server_Region (S), Q.Server_Wake_At);
         begin
            -- Awake: the client need not kick until the word is armed again.
            if Wake /= 0 then
               Wake := 0;
            end if;
         end;
         Sessions.Accept_Reaped (T.Items (S).Item, Reaped_Index (S));
         Sessions.Accept_Produced (T.Items (S).Item, Produced_Index (S));
         -- The descriptors were written before the count: read them after.
         Barrier;
      end if;
      for C in Q.Context_Index loop
         if Sessions.Context_Open (T.Items (S).Item, C) and then
           (Ledgers.Count (Sessions.Ledger (T.Items (S).Item, C)) > 0 or else
            T.Items (S).Kicks (C) /= No_Kick)
         then
            if not Select_Context (S, C) or else not Owner_Ready then
               Fail_Session (T, S, Q.Device_Fault, Now);
               return;
            end if;
            Read_Timeline (V, OK);
            Gate := T.Items (S).Kicks (C) = No_Kick and then Scheduling_Resident;
            if not Owner_Ready then
               Fail_Session (T, S, Q.Device_Fault, Now);
               return;
            end if;
            declare
               L : constant Ledgers.Ledger := Sessions.Ledger (T.Items (S).Item, C);
               Started : constant Boolean :=
                 Ledgers.Unfinished (L) and then
                 Job_Started (Unsigned_64 (Ledgers.Completed (L)) + 1);
            begin
               Sessions.Observe (T.Items (S).Item, C, OK, Value (V), Gate, Started, Now,
                                 Microseconds (Hang_Budget_Us), Microseconds (Patience), Seen);
            end;
            if Seen = Ledgers.Failed then
               -- A hang (GPU-005) or a bad timeline fails this session's
               -- jobs only; the caller evicts its context from the engine
               -- (Quarantine with Hang or Deadline). Other sessions go on.
               Fail_Session (T, S, Ledgers.Why (Sessions.Ledger (T.Items (S).Item, C)), Now);
               return;
            end if;
            Pop_Ready (T, S, C, Now);
         end if;
      end loop;
      -- The status lines and records first: a client woken by the answer
      -- must find its value there, not one turn later.
      if Sessions.Live (T.Items (S).Item) then
         Publish (T, S);
      end if;
      Answer_Held_Wake (T, S);
      if Sessions.Live (T.Items (S).Item) and then not T.Quiesce then
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
      Completed := Unsigned_32 (Sessions.Server (T.Items (S).Item).Answers.Produced);
      Taken := Unsigned_32 (Sessions.Server (T.Items (S).Item).Requests.Consumed);
      Write_Status (T, S);
   end Publish;

   function Has_Work (T : Table; S : Session_Id) return Boolean is
     (Sessions.Live (T.Items (S).Item) or else not Sessions.Idle (T.Items (S).Item) or else
      (for some C in Q.Context_Index => T.Items (S).Kicks (C) /= No_Kick) or else
      Sessions.Wake (T.Items (S).Item).Current = Wakes.Held);

   procedure Count_In_Flight (T : in out Table) is
      Total : Natural := 0;
   begin
      for S in Session_Id'First .. T.Count loop
         for C in Q.Context_Index loop
            Total := Total + Ledgers.Count (Sessions.Ledger (T.Items (S).Item, C));
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
      for S in Session_Id'First .. T.Count loop
         if Has_Work (T, S) then
            Serve_Session (T, S, Now);
         end if;
      end loop;
      Send_Kicks (T, Now);
      for S in Session_Id'First .. T.Count loop
         if Sessions.Live (T.Items (S).Item) then
            Publish (T, S);
         end if;
      end loop;
      Count_In_Flight (T);
   end Turn;

   function Call_Room (T : Table; S : Session_Id; C : Q.Context_Index) return Boolean is
     (Sessions.Context_Open (T.Items (S).Item, C) and then not T.Quiesce and then
      not Sessions.Quiescing (T.Items (S).Item) and then
      Ledgers.State (Sessions.Ledger (T.Items (S).Item, C)) = Ledgers.Active and then
      Ledgers.Count (Sessions.Ledger (T.Items (S).Item, C)) < Ledgers.Max_In_Flight and then
      not Ledgers.Call_Pending (Sessions.Ledger (T.Items (S).Item, C)) and then
      Windows.Plan (Sessions.Window (T.Items (S).Item, C), Segment_Bytes (Q.Execute)).Status =
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
      elsif not Sessions.Context_Open (T.Items (S).Item, C) or else
        Ledgers.State (Sessions.Ledger (T.Items (S).Item, C)) /= Ledgers.Active or else
        Now = Intel_GPU_Timeline.Clock_Unavailable or else Budget_Us = 0 or else
        Budget_Us >= Unsigned_64 (Intel_GPU_Timeline.Clock_Unavailable - Now)
      then
         return;
      elsif T.Quiesce or else Sessions.Quiescing (T.Items (S).Item) or else
        Ledgers.Count (Sessions.Ledger (T.Items (S).Item, C)) >= Ledgers.Max_In_Flight or else
        Ledgers.Call_Pending (Sessions.Ledger (T.Items (S).Item, C))
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
      Next := Ledgers.Accepted (Sessions.Ledger (T.Items (S).Item, C)) + 1;
      Plan := Windows.Plan (Sessions.Window (T.Items (S).Item, C), Segment);
      if Plan.Status /= Intel_GPU_Ring_Reservation.Ready then
         Result := Busy;
         return;
      elsif not Sessions.Can_Commit (T.Items (S).Item, C, Next, Ledgers.From_Call, Segment) then
         -- An exhausted timeline: the context takes no more work.
         Sessions.Fault_Context (T.Items (S).Item, C, Q.Signal_Mismatch);
         return;
      end if;
      Write_Segment (Q.Execute, Unsigned_64 (Next), GPU, Plan,
                     Windows.Ring_Offset (Windows.Tail (Sessions.Window (T.Items (S).Item, C))), OK);
      if not OK or else not Owner_Ready then
         Fail_Session (T, S, Q.Ring_Fault, Now);
         return;
      end if;
      Sessions.Commit_Call
        (T.Items (S).Item, C, Next, Segment,
         (Token => 0, Deadline => Now + Microseconds (Budget_Us), Submitted => Now), Now);
      T.Items (S).Jobs (C, Slot_Of (Next)) := (Kinds => 0, Timed => True);
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
      if not Sessions.Live (T.Items (S).Item) or else not Sessions.Context_Open (T.Items (S).Item, C) then
         return;
      end if;
      declare
         L : constant Ledgers.Ledger := Sessions.Ledger (T.Items (S).Item, C);
      begin
         Holds := Wakes.Condition
           (Ledgers.Completed (L), Target,
            Ledgers.State (L) in Ledgers.Failed_Health or else
              (Ledgers.State (L) = Ledgers.Faulted and then Target > Ledgers.Accepted (L)),
            False);
      end;
      Sessions.Wake_Arrive
        (T.Items (S).Item, C, Target, Holds,
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
      if Sessions.Wake (T.Items (S).Item).Current = Wakes.Held then
         Sessions.Wake_Hold_Failed (T.Items (S).Item);
      end if;
      Release_Wake_Slot (T, S);
   end Wake_Hold_Failed;

   function Arm_Wake_Words (T : in out Table) return Boolean is
      Found : Boolean := False;
   begin
      T.Wake_Epoch := (if T.Wake_Epoch = Unsigned_32'Last then 1 else T.Wake_Epoch + 1);
      for S in Session_Id'First .. T.Count loop
         if Sessions.Live (T.Items (S).Item) then
            declare
               Wake : Unsigned_32 with Import, Volatile,
                 Address => At_Offset (Server_Region (S), Q.Server_Wake_At);
            begin
               Wake := T.Wake_Epoch;
               Fence;
               if Produced_Index (S) /= Sessions.Server (T.Items (S).Item).Requests.Consumed then
                  Found := True;
               end if;
            end;
         end if;
      end loop;
      return Found;
   end Arm_Wake_Words;

end Intel_GPU_Queue_Service;
