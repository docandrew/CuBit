package body Intel_GPU_Session_Queue with SPARK_Mode is

   -- The total changes only by context C's share.
   procedure Lemma_Total_Change (Before, After : Session; C : Context_Index)
     with Ghost, Global => null,
          Pre => (for all D in Context_Index =>
                    (if D /= C then After.Ledgers_Of (D) = Before.Ledgers_Of (D))),
          Post => Queued_Total (After) + Ledgers.Queued (Before.Ledgers_Of (C)) =
                  Queued_Total (Before) + Ledgers.Queued (After.Ledgers_Of (C)) and then
                  Queued_Total (Before) >= Ledgers.Queued (Before.Ledgers_Of (C));
   procedure Lemma_Total_Change (Before, After : Session; C : Context_Index) is
   begin
      case C is
         when 0 => null;
         when 1 => null;
         when 2 => null;
         when 3 => null;
      end case;
   end Lemma_Total_Change;

   function Empty return Session is
      Result : Session;
   begin
      return Result;
   end Empty;

   procedure Open_Context
     (S : in out Session; C : Context_Index; Done : Value; First_Bytes : Unsigned_32;
      Now : Microseconds)
   is
      Old_Total : constant Natural := Queued_Total (S) with Ghost;
   begin
      S.Ledgers_Of (C) := Ledgers.Started (Done, Now);
      S.Windows_Of (C) := Windows.Initial (First_Bytes, Windows.Value (Done));
      S.Opened (C) := True;
      pragma Assert (Queued_Total (S) = Old_Total);
   end Open_Context;

   procedure Open_Queue (S : in out Session) is
   begin
      S.Answers := (others => <>);
      S.Taken := False;
      S.Queue_Live := True;
   end Open_Queue;

   procedure End_Queue (S : in out Session; Answer_Wake : out Boolean) is
   begin
      Wakes.Ended (S.Wake_State, Answer_Wake);
      S.Queue_Live := False;
      S.Taken := False;
   end End_Queue;

   procedure Accept_Produced (S : in out Session; Produced : QQ.Submissions.Index) is
      OK : Boolean;
   begin
      QQ.Submissions.Accept_Produced (S.Answers.Requests, Produced, OK);
      pragma Unreferenced (OK);
   end Accept_Produced;

   procedure Accept_Reaped (S : in out Session; Reaped : QQ.Completions.Index) is
      OK : Boolean;
   begin
      QQ.Accept_Reaped (S.Answers, Reaped, OK);
      pragma Unreferenced (OK);
   end Accept_Reaped;

   procedure Take_Head (S : in out Session; Ring : QQ.Submissions.Ring) is
   begin
      QQ.Take (S.Answers, Ring, S.Head_Item);
      S.Taken := True;
   end Take_Head;

   function View (S : Session) return Admission.Session_View is
      Result : Admission.Session_View;
   begin
      for C in Context_Index loop
         Result (C) :=
           (Open => S.Opened (C),
            Taking => S.Opened (C) and then Ledgers.State (S.Ledgers_Of (C)) = Ledgers.Active,
            Failed => Ledgers.State (S.Ledgers_Of (C)) in Ledgers.Failed_Health,
            Accepted => Ledgers.Accepted (S.Ledgers_Of (C)),
            Completed => Ledgers.Completed (S.Ledgers_Of (C)),
            Owed => Ledgers.Count (S.Ledgers_Of (C)));
      end loop;
      return Result;
   end View;

   procedure Commit
     (S : in out Session; C : Context_Index; Next : Value; Source : Ledgers.Origin;
      Bytes : Unsigned_32; Item : Ledgers.Job; Now : Microseconds)
     with Pre => Can_Commit (S, C, Next, Source, Bytes),
          Post => (for all D in Context_Index =>
                     (if D /= C then S.Ledgers_Of (D) = S'Old.Ledgers_Of (D)
                        and S.Windows_Of (D) = S'Old.Windows_Of (D))) and then
                  S.Opened = S'Old.Opened and then S.Answers = S'Old.Answers and then
                  S.Taken = S'Old.Taken and then S.Head_Item = S'Old.Head_Item and then
                  S.Queue_Live = S'Old.Queue_Live and then S.Quiesce = S'Old.Quiesce and then
                  S.Wake_State = S'Old.Wake_State and then
                  Context_Valid (S, C) and then
                  (for all D in Context_Index => Context_Valid (S, D)) and then
                  Queued_Total (S) = Queued_Total (S'Old) +
                    (if Source = Ledgers.From_Queue then 1 else 0) and then
                  Ledgers.Accepted (S.Ledgers_Of (C)) = Next and then
                  Ledgers.Count (S.Ledgers_Of (C)) = Ledgers.Count (S'Old.Ledgers_Of (C)) + 1 and then
                  Ledgers.Call_Pending (S.Ledgers_Of (C)) =
                    (Ledgers.Call_Pending (S'Old.Ledgers_Of (C)) or Source = Ledgers.From_Call) and then
                  Ledgers.Queued (S.Ledgers_Of (C)) =
                    Ledgers.Queued (S'Old.Ledgers_Of (C)) +
                      (if Source = Ledgers.From_Queue then 1 else 0);
   procedure Commit
     (S : in out Session; C : Context_Index; Next : Value; Source : Ledgers.Origin;
      Bytes : Unsigned_32; Item : Ledgers.Job; Now : Microseconds)
   is
      Old_Ledger : constant Ledgers.Ledger := S.Ledgers_Of (C) with Ghost;
      Before : constant Session := S with Ghost;
   begin
      pragma Assert (Valid (Before));
      pragma Assert (for all D in Context_Index => Context_Valid (Before, D));
      pragma Assert (Ledgers.Valid (Old_Ledger) and then Ledgers.State (Old_Ledger) = Ledgers.Active);
      Windows.Append (S.Windows_Of (C), Bytes, Windows.Value (Next));
      pragma Assert (S.Ledgers_Of (C) = Old_Ledger);
      Ledgers.Accept_Job (S.Ledgers_Of (C), Next, Source, Item, Now);
      pragma Assert (Windows.Last_Value (S.Windows_Of (C)) = Windows.Value (Next));
      pragma Assert (Ledgers.Accepted (S.Ledgers_Of (C)) = Next);
      pragma Assert (Context_Valid (S, C));
      pragma Assert (for all D in Context_Index => Context_Valid (S, D));
      Lemma_Total_Change (Before, S, C);
   end Commit;

   procedure Commit_Head
     (S : in out Session; Bytes : Unsigned_32; Item : Ledgers.Job; Now : Microseconds)
   is
      C : constant Context_Index := S.Head_Item.Item.Context;
   begin
      Commit (S, C, Value (S.Head_Item.Item.Signal_Value), Ledgers.From_Queue, Bytes, Item, Now);
      S.Taken := False;
   end Commit_Head;

   procedure Commit_Call
     (S : in out Session; C : Context_Index; Next : Value; Bytes : Unsigned_32;
      Item : Ledgers.Job; Now : Microseconds)
   is
   begin
      Commit (S, C, Next, Ledgers.From_Call, Bytes, Item, Now);
   end Commit_Call;

   procedure Refuse_Head
     (S : in out Session; Ring : in out QQ.Completions.Ring;
      Status : Q.Completion_Status; Detail : Q.Fault_Reason; Fault_It : Boolean;
      C : Context_Index)
   is
      Before : constant Session := S with Ghost;
   begin
      QQ.Complete
        (S.Answers, Ring, S.Head_Item.Tag,
         (Status => Q.Completion_Status'Enum_Rep (Status),
          Context => Unsigned_32 (S.Head_Item.Item.Context),
          Value => S.Head_Item.Item.Signal_Value,
          Detail => Q.Fault_Reason'Enum_Rep (Detail),
          Reserved => 0));
      S.Taken := False;
      if Fault_It and then S.Opened (C) then
         Ledgers.Fault (S.Ledgers_Of (C), Detail);
         pragma Assert (Context_Valid (S, C));
      end if;
      Lemma_Total_Change (Before, S, C);
      pragma Assert (for all D in Context_Index => Context_Valid (S, D));
   end Refuse_Head;

   procedure Observe
     (S : in out Session; C : Context_Index; Read_OK : Boolean; Observed : Value;
      Gate_Open : Boolean; Now, Hang_Budget : Microseconds;
      Result : out Ledgers.Observation)
   is
      Before : constant Session := S with Ghost;
   begin
      Ledgers.Observe (S.Ledgers_Of (C), Read_OK, Observed, Gate_Open, Now, Hang_Budget, Result);
      Windows.Retire (S.Windows_Of (C), Windows.Value (Ledgers.Completed (S.Ledgers_Of (C))));
      Lemma_Total_Change (Before, S, C);
   end Observe;

   procedure Pop
     (S : in out Session; C : Context_Index; Ring : in out QQ.Completions.Ring;
      Item : out Ledgers.Job; V : out Value; Source : out Ledgers.Origin;
      Status : out Ledgers.Pop_Status)
   is
      Old_Queued : constant Natural := Ledgers.Queued (S.Ledgers_Of (C)) with Ghost;
      Before : constant Session := S with Ghost;
   begin
      Lemma_Total_Change (S, S, C);
      pragma Assert (if S.Queue_Live then S.Answers.Owed >= Old_Queued);
      Ledgers.Pop (S.Ledgers_Of (C), Item, V, Source, Status);
      pragma Assert (if Source = Ledgers.From_Queue then Old_Queued >= 1);
      Lemma_Total_Change (Before, S, C);
      if S.Queue_Live and then Source = Ledgers.From_Queue then
         QQ.Complete
           (S.Answers, Ring, QQ.Token (Item.Token),
            (Status => Q.Completion_Status'Enum_Rep
                         (if Status = Ledgers.Done then Q.Completed else Q.Device_Lost),
             Context => Unsigned_32 (C),
             Value => Unsigned_64 (V),
             Detail => Q.Fault_Reason'Enum_Rep
                         (if Status = Ledgers.Done then Q.None
                          else Ledgers.Why (S.Ledgers_Of (C))),
             Reserved => 0));
      end if;
   end Pop;

   procedure Fault_Context (S : in out Session; C : Context_Index; Why : Q.Fault_Reason) is
      Before : constant Session := S with Ghost;
   begin
      if S.Opened (C) then
         Ledgers.Fault (S.Ledgers_Of (C), Why);
      end if;
      Lemma_Total_Change (Before, S, C);
   end Fault_Context;

   procedure Lose_All (S : in out Session; Why : Q.Fault_Reason) is
   begin
      for C in Context_Index loop
         Ledgers.Lose (S.Ledgers_Of (C), Why);
         pragma Loop_Invariant
           (for all D in Context_Index'First .. C =>
              Ledgers.State (S.Ledgers_Of (D)) in Ledgers.Failed_Health);
         pragma Loop_Invariant
           (for all D in Context_Index =>
              Ledgers.Count (S.Ledgers_Of (D)) = Ledgers.Count (S.Ledgers_Of'Loop_Entry (D)) and
              Ledgers.Call_Pending (S.Ledgers_Of (D)) =
                Ledgers.Call_Pending (S.Ledgers_Of'Loop_Entry (D)) and
              Ledgers.Accepted (S.Ledgers_Of (D)) = Ledgers.Accepted (S.Ledgers_Of'Loop_Entry (D)) and
              Ledgers.Valid (S.Ledgers_Of (D)));
      end loop;
   end Lose_All;

   procedure Begin_Quiesce (S : in out Session) is
   begin
      S.Quiesce := True;
   end Begin_Quiesce;

   procedure End_Quiesce (S : in out Session) is
   begin
      S.Quiesce := False;
   end End_Quiesce;

   procedure Wake_Arrive
     (S : in out Session; C : Context_Index; Target : Value; Holds, Slot_Free : Boolean;
      Outcome : out Wakes.Arrival)
   is
   begin
      Wakes.Arrive (S.Wake_State, C, Target, Holds, Slot_Free, Outcome);
   end Wake_Arrive;

   procedure Wake_Hold_Failed (S : in out Session) is
   begin
      Wakes.Hold_Failed (S.Wake_State);
   end Wake_Hold_Failed;

   procedure Wake_Step (S : in out Session; Holds : Boolean; Answer_Held : out Boolean) is
   begin
      Wakes.Step (S.Wake_State, Holds, Answer_Held);
   end Wake_Step;

   function Wake_Holds (S : Session) return Boolean is
     (Wakes.Condition
        (Ledgers.Completed (S.Ledgers_Of (S.Wake_State.Context)), S.Wake_State.Target,
         -- Its target can no longer be reached: the context failed, or it
         -- faulted before accepting the target's job.
         Ledgers.State (S.Ledgers_Of (S.Wake_State.Context)) in Ledgers.Failed_Health or else
           (Ledgers.State (S.Ledgers_Of (S.Wake_State.Context)) = Ledgers.Faulted and then
            S.Wake_State.Target > Ledgers.Accepted (S.Ledgers_Of (S.Wake_State.Context))),
         not S.Queue_Live));

end Intel_GPU_Session_Queue;
