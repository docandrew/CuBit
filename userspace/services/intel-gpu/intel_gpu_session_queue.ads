with Interfaces; use Interfaces;
with CuBit.GPU_Queues;
with Intel_GPU_Context_Ledger;
with Intel_GPU_Queue_Admission;
with Intel_GPU_Queue_Wakes;
with Intel_GPU_Segment_Window;
with Intel_GPU_Timeline;
-- One GPU session's queue and contexts (GPU-001 step 2). Pure logic, no
-- I/O; proved at GNATprove level 2.
--
-- A session always has its contexts' ledgers and ring windows (the
-- synchronous 0x0A27 wrapper uses them too); the queue is optional and
-- opened by the client. The driver takes one descriptor at a time as the
-- session's head, decides on it (Intel_GPU_Queue_Admission), and either
-- publishes it (Commit_Head), keeps it while it waits, or refuses it with a
-- completion record (Refuse_Head). Published jobs are popped from their
-- ledger as the timeline completes them, each with one record.
--
-- Proved (Valid):
--   * one completion per take: while the queue is live, the completion
--     records owed (CuBit.Submission_Queues' Owed, every one with its
--     reserved slot) are exactly the head, if taken, plus every queue job
--     the ledgers owe. Take adds one, Refuse_Head and each queue job's Pop
--     remove one, Commit_Head moves the head into a ledger;
--   * each open context's ring window ends at the value its ledger accepted
--     last, so ring space is reclaimed only behind completed values;
--   * the quiesce rule: nothing is committed while the session quiesces,
--     and Park_Allowed (VM updates and other work that parks contexts) holds
--     only while it quiesces with nothing in flight.
package Intel_GPU_Session_Queue with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   package Q renames CuBit.GPU_Queues;
   package QQ renames Q.Queues;
   package Ledgers renames Intel_GPU_Context_Ledger;
   package Windows renames Intel_GPU_Segment_Window;
   package Admission renames Intel_GPU_Queue_Admission;
   package Wakes renames Intel_GPU_Queue_Wakes;
   subtype Value is Intel_GPU_Timeline.Value;
   subtype Microseconds is Intel_GPU_Timeline.Microseconds;
   subtype Context_Index is Q.Context_Index;
   use type Value, Microseconds, Ledgers.Health, Ledgers.Origin, Ledgers.Ledger,
            Ledgers.Observation, Ledgers.Pop_Status, Admission.Verdict,
            Q.Completion_Status, QQ.Submission, QQ.Token, QQ.Server, Wakes.Phase, Wakes.State,
            Windows.Value, Windows.Window;

   pragma Compile_Time_Error
     (Q.Contexts_Per_Session /= 4, "Queued_Total sums four contexts");
   pragma Compile_Time_Error
     (Ledgers.Max_In_Flight /= Windows.Max_In_Flight,
      "a ring window remembers every job its ledger owes");

   type Session is private;

   function Valid (S : Session) return Boolean;
   function Live (S : Session) return Boolean;          -- queue opened, not ended
   function Quiescing (S : Session) return Boolean;
   function Has_Head (S : Session) return Boolean;
   function Head (S : Session) return QQ.Submission with Pre => Has_Head (S);
   function Context_Open (S : Session; C : Context_Index) return Boolean;
   function Ledger (S : Session; C : Context_Index) return Ledgers.Ledger;
   function Window (S : Session; C : Context_Index) return Windows.Window;
   function Server (S : Session) return QQ.Server;
   function Wake (S : Session) return Wakes.State;

   -- Queue jobs the ledgers owe a record.
   function Queued_Total (S : Session) return Natural;
   -- Nothing in flight on any of the session's contexts.
   function Idle (S : Session) return Boolean;
   -- Work that parks contexts may run: quiescing and idle.
   function Park_Allowed (S : Session) return Boolean is (Quiescing (S) and then Idle (S));

   function Empty return Session
     with Post => Valid (Empty'Result) and then not Live (Empty'Result) and then
                  Idle (Empty'Result) and then not Has_Head (Empty'Result) and then
                  (for all C in Context_Index => not Context_Open (Empty'Result, C));

   -- A registered context whose setup breadcrumb wrote Done, its ring
   -- holding that one First_Bytes segment.
   procedure Open_Context
     (S : in out Session; C : Context_Index; Done : Value; First_Bytes : Unsigned_32;
      Now : Microseconds)
     with Pre => Valid (S) and then not Context_Open (S, C) and then
                 Ledgers.Count (Ledger (S, C)) = 0 and then
                 Done < Ledgers.Last_Usable and then
                 First_Bytes in Windows.Command_Alignment .. Windows.Span_Limit and then
                 First_Bytes mod Windows.Command_Alignment = 0,
          Post => Valid (S) and then Context_Open (S, C) and then
                  Ledgers.Accepted (Ledger (S, C)) = Done and then
                  Ledgers.State (Ledger (S, C)) = Ledgers.Active and then
                  Live (S) = Live (S)'Old and then Has_Head (S) = Has_Head (S)'Old;

   -- The client opened the queue: a fresh server side.
   procedure Open_Queue (S : in out Session)
     with Pre => Valid (S) and then not Live (S) and then Queued_Total (S) = 0,
          Post => Valid (S) and then Live (S) and then not Has_Head (S) and then
                  Server (S).Owed = 0 and then Wake (S) = Wake (S)'Old;

   -- The queue ends (closed, or the session retires). Its owed records are
   -- no longer written; its jobs on the GPU stay in their ledgers.
   procedure End_Queue (S : in out Session; Answer_Wake : out Boolean)
     with Pre => Valid (S),
          Post => Valid (S) and then not Live (S) and then not Has_Head (S) and then
                  Answer_Wake = (Wake (S)'Old.Current = Wakes.Held) and then
                  Wake (S).Current = Wakes.Idle and then
                  (for all C in Context_Index => Ledger (S, C) = Ledger (S'Old, C));

   procedure Accept_Produced (S : in out Session; Produced : QQ.Submissions.Index)
     with Pre => Valid (S),
          Post => Valid (S) and then Server (S).Owed = Server (S)'Old.Owed and then
                  Live (S) = Live (S)'Old and then Has_Head (S) = Has_Head (S)'Old and then
                  (for all C in Context_Index => Ledger (S, C) = Ledger (S'Old, C));
   procedure Accept_Reaped (S : in out Session; Reaped : QQ.Completions.Index)
     with Pre => Valid (S),
          Post => Valid (S) and then Server (S).Owed = Server (S)'Old.Owed and then
                  Live (S) = Live (S)'Old and then Has_Head (S) = Has_Head (S)'Old and then
                  (for all C in Context_Index => Ledger (S, C) = Ledger (S'Old, C));

   function Can_Take (S : Session) return Boolean is
     (Valid (S) and then Live (S) and then not Has_Head (S) and then QQ.Can_Take (Server (S)));

   -- Copy the next descriptor out as the head: it owes one record now.
   procedure Take_Head (S : in out Session; Ring : QQ.Submissions.Ring)
     with Pre => Can_Take (S),
          Post => Valid (S) and then Live (S) and then Has_Head (S) and then
                  Server (S).Owed = Server (S)'Old.Owed + 1 and then
                  Head (S) = Ring (QQ.Submissions.Slot_Of (Server (S)'Old.Requests.Consumed)) and then
                  (for all C in Context_Index => Ledger (S, C) = Ledger (S'Old, C));

   -- What the admission rules make of the head now.
   function View (S : Session) return Admission.Session_View;
   function Decide_Head (S : Session; Now : Microseconds) return Admission.Decision is
     (Admission.Decide (Head (S).Item, View (S), Quiescing (S), Now, Ledgers.Max_In_Flight))
     with Pre => Has_Head (S);

   -- A job may be published on context C with value Next, taking Bytes of
   -- its ring.
   function Can_Commit
     (S : Session; C : Context_Index; Next : Value; Source : Ledgers.Origin;
      Bytes : Unsigned_32) return Boolean is
     (Valid (S) and then not Quiescing (S) and then Context_Open (S, C) and then
      Ledgers.Can_Accept (Ledger (S, C), Next, Source) and then
      Windows.Can_Append (Window (S, C), Bytes, Windows.Value (Next)));

   -- The head was published (its segment written where Window's plan said,
   -- and the ring tail moved): it is now a job its context owes.
   procedure Commit_Head
     (S : in out Session; Bytes : Unsigned_32; Item : Ledgers.Job; Now : Microseconds)
     with Pre => Has_Head (S) and then Live (S) and then
                 Head (S).Item.Context <= Context_Index'Last and then
                 Can_Commit (S, Head (S).Item.Context, Value (Head (S).Item.Signal_Value),
                             Ledgers.From_Queue, Bytes) and then
                 Item.Token = Unsigned_64 (Head (S).Tag),
          Post => Valid (S) and then not Has_Head (S) and then Live (S) and then
                  Server (S).Owed = Server (S)'Old.Owed and then
                  Ledgers.Accepted (Ledger (S, Head (S'Old).Item.Context)) =
                    Value (Head (S'Old).Item.Signal_Value) and then
                  Ledgers.Count (Ledger (S, Head (S'Old).Item.Context)) =
                    Ledgers.Count (Ledger (S'Old, Head (S'Old).Item.Context)) + 1;

   -- The synchronous wrapper's job was published on C.
   procedure Commit_Call
     (S : in out Session; C : Context_Index; Next : Value; Bytes : Unsigned_32;
      Item : Ledgers.Job; Now : Microseconds)
     with Pre => Can_Commit (S, C, Next, Ledgers.From_Call, Bytes),
          Post => Valid (S) and then Live (S) = Live (S)'Old and then
                  Has_Head (S) = Has_Head (S)'Old and then
                  Server (S).Owed = Server (S)'Old.Owed and then
                  Ledgers.Call_Pending (Ledger (S, C)) and then
                  Ledgers.Accepted (Ledger (S, C)) = Next;

   -- The head is answered without running: Rejected, Deadline_Expired,
   -- Context_Faulted or Device_Lost. Fault_It also faults the context it
   -- names (rejected or expired: its value is never signalled).
   procedure Refuse_Head
     (S : in out Session; Ring : in out QQ.Completions.Ring;
      Status : Q.Completion_Status; Detail : Q.Fault_Reason; Fault_It : Boolean;
      C : Context_Index)
     with Pre => Valid (S) and then Has_Head (S) and then Live (S) and then
                 Status /= Q.Completed,
          Post => Valid (S) and then not Has_Head (S) and then Live (S) and then
                  Server (S).Owed = Server (S)'Old.Owed - 1 and then
                  Ring (QQ.Completions.Slot_Of (Server (S)'Old.Answers.Produced)).Tag =
                    Head (S'Old).Tag and then
                  Ring (QQ.Completions.Slot_Of (Server (S)'Old.Answers.Produced)).Answer.Status =
                    Q.Completion_Status'Enum_Rep (Status) and then
                  (if Fault_It and then Context_Open (S'Old, C) then
                     Ledgers.State (Ledger (S, C)) /= Ledgers.Active);

   -- One observation of context C's timeline and watchdogs; its ring window
   -- forgets segments behind the completed value.
   procedure Observe
     (S : in out Session; C : Context_Index; Read_OK : Boolean; Observed : Value;
      Gate_Open : Boolean; Now, Hang_Budget : Microseconds;
      Result : out Ledgers.Observation)
     with Pre => Valid (S) and then Context_Open (S, C) and then Hang_Budget > 0,
          Post => Valid (S) and then Live (S) = Live (S)'Old and then
                  Has_Head (S) = Has_Head (S)'Old and then
                  Server (S).Owed = Server (S)'Old.Owed and then
                  Ledgers.Count (Ledger (S, C)) = Ledgers.Count (Ledger (S'Old, C)) and then
                  (if Result = Ledgers.Advanced then
                     Ledgers.Completed (Ledger (S, C)) = Observed);

   function Can_Pop (S : Session; C : Context_Index) return Boolean is
     (Valid (S) and then Ledgers.Head_Ready (Ledger (S, C)));

   -- Pop context C's oldest owed job. A queue job of a live queue gets its
   -- record: Completed if the timeline reached it, Device_Lost otherwise.
   -- A From_Call job is the synchronous wrapper's to answer.
   procedure Pop
     (S : in out Session; C : Context_Index; Ring : in out QQ.Completions.Ring;
      Item : out Ledgers.Job; V : out Value; Source : out Ledgers.Origin;
      Status : out Ledgers.Pop_Status)
     with Pre => Can_Pop (S, C),
          Post => Valid (S) and then Live (S) = Live (S)'Old and then
                  Has_Head (S) = Has_Head (S)'Old and then
                  Ledgers.Count (Ledger (S, C)) = Ledgers.Count (Ledger (S'Old, C)) - 1 and then
                  Server (S).Owed =
                    Server (S)'Old.Owed -
                      (if Live (S'Old) and then Source = Ledgers.From_Queue then 1 else 0) and then
                  (if Live (S'Old) and then Source = Ledgers.From_Queue then
                     Ring (QQ.Completions.Slot_Of (Server (S)'Old.Answers.Produced)).Tag =
                       QQ.Token (Item.Token) and then
                     Ring (QQ.Completions.Slot_Of (Server (S)'Old.Answers.Produced)).Answer.Value =
                       Unsigned_64 (V) and then
                     Ring (QQ.Completions.Slot_Of (Server (S)'Old.Answers.Produced)).Answer.Status =
                       Q.Completion_Status'Enum_Rep
                         (if Status = Ledgers.Done then Q.Completed else Q.Device_Lost));

   -- Context C takes no more jobs; those on the GPU still complete.
   procedure Fault_Context (S : in out Session; C : Context_Index; Why : Q.Fault_Reason)
     with Pre => Valid (S),
          Post => Valid (S) and then Live (S) = Live (S)'Old and then
                  Has_Head (S) = Has_Head (S)'Old and then
                  Server (S).Owed = Server (S)'Old.Owed and then
                  (if Context_Open (S, C) then Ledgers.State (Ledger (S, C)) /= Ledgers.Active);
   -- The device or the session is lost: every context's owed jobs fail.
   procedure Lose_All (S : in out Session; Why : Q.Fault_Reason)
     with Pre => Valid (S),
          Post => Valid (S) and then Live (S) = Live (S)'Old and then
                  Has_Head (S) = Has_Head (S)'Old and then
                  Server (S).Owed = Server (S)'Old.Owed and then
                  (for all C in Context_Index =>
                     Ledgers.State (Ledger (S, C)) in Ledgers.Failed_Health);

   procedure Begin_Quiesce (S : in out Session)
     with Pre => Valid (S),
          Post => Valid (S) and then Quiescing (S) and then
                  Idle (S) = Idle (S)'Old and then Live (S) = Live (S)'Old and then
                  Has_Head (S) = Has_Head (S)'Old and then Server (S).Owed = Server (S)'Old.Owed;
   procedure End_Quiesce (S : in out Session)
     with Pre => Valid (S),
          Post => Valid (S) and then not Quiescing (S) and then
                  Idle (S) = Idle (S)'Old and then Live (S) = Live (S)'Old and then
                  Has_Head (S) = Has_Head (S)'Old and then Server (S).Owed = Server (S)'Old.Owed;

   -- Wakes (Intel_GPU_Queue_Wakes); Holds is computed by the caller from
   -- the ledgers and Live.
   procedure Wake_Arrive
     (S : in out Session; C : Context_Index; Target : Value; Holds, Slot_Free : Boolean;
      Outcome : out Wakes.Arrival)
     with Pre => Valid (S),
          Post => Valid (S) and then Live (S) = Live (S)'Old and then
                  Has_Head (S) = Has_Head (S)'Old and then Server (S).Owed = Server (S)'Old.Owed;
   procedure Wake_Hold_Failed (S : in out Session)
     with Pre => Valid (S) and then Wake (S).Current = Wakes.Held,
          Post => Valid (S) and then Live (S) = Live (S)'Old and then
                  Has_Head (S) = Has_Head (S)'Old and then Server (S).Owed = Server (S)'Old.Owed;
   procedure Wake_Step (S : in out Session; Holds : Boolean; Answer_Held : out Boolean)
     with Pre => Valid (S),
          Post => Valid (S) and then Live (S) = Live (S)'Old and then
                  Has_Head (S) = Has_Head (S)'Old and then Server (S).Owed = Server (S)'Old.Owed;

   -- The held wake's condition from the session's state.
   function Wake_Holds (S : Session) return Boolean
     with Pre => Valid (S);
private
   type Context_Array is array (Context_Index) of Ledgers.Ledger;
   type Window_Array is array (Context_Index) of Windows.Window;
   type Open_Array is array (Context_Index) of Boolean;
   type Session is record
      Ledgers_Of : Context_Array := [others => Ledgers.Started (0, 0)];
      Windows_Of : Window_Array := [others => Windows.Initial (Windows.Command_Alignment, 0)];
      Opened : Open_Array := [others => False];
      Queue_Live : Boolean := False;
      Quiesce : Boolean := False;
      Taken : Boolean := False;
      Head_Item : QQ.Submission := (Tag => 0, Item => <>);
      Answers : QQ.Server;
      Wake_State : Wakes.State;
   end record;

   function Live (S : Session) return Boolean is (S.Queue_Live);
   function Quiescing (S : Session) return Boolean is (S.Quiesce);
   function Has_Head (S : Session) return Boolean is (S.Taken);
   function Head (S : Session) return QQ.Submission is (S.Head_Item);
   function Context_Open (S : Session; C : Context_Index) return Boolean is (S.Opened (C));
   function Ledger (S : Session; C : Context_Index) return Ledgers.Ledger is (S.Ledgers_Of (C));
   function Window (S : Session; C : Context_Index) return Windows.Window is (S.Windows_Of (C));
   function Server (S : Session) return QQ.Server is (S.Answers);
   function Wake (S : Session) return Wakes.State is (S.Wake_State);

   function Queued_Total (S : Session) return Natural is
     (Ledgers.Queued (S.Ledgers_Of (0)) + Ledgers.Queued (S.Ledgers_Of (1)) +
      Ledgers.Queued (S.Ledgers_Of (2)) + Ledgers.Queued (S.Ledgers_Of (3)));

   function Idle (S : Session) return Boolean is
     (for all C in Context_Index => Ledgers.Count (S.Ledgers_Of (C)) = 0);

   function Context_Valid (S : Session; C : Context_Index) return Boolean is
     (Ledgers.Valid (S.Ledgers_Of (C)) and then
      (if S.Opened (C) then
         Windows.Valid (S.Windows_Of (C)) and then
         Windows.Last_Value (S.Windows_Of (C)) = Windows.Value (Ledgers.Accepted (S.Ledgers_Of (C)))
       else Ledgers.Count (S.Ledgers_Of (C)) = 0));

   function Valid (S : Session) return Boolean is
     ((for all C in Context_Index => Context_Valid (S, C)) and then
      QQ.Valid (S.Answers) and then
      (if S.Queue_Live then S.Answers.Owed = (if S.Taken then 1 else 0) + Queued_Total (S)
       else not S.Taken));
end Intel_GPU_Session_Queue;
