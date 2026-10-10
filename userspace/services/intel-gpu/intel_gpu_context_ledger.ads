with Interfaces; use Interfaces;
with CuBit.GPU_Queues;
with Intel_GPU_Timeline;
-- One GPU context's jobs in flight (GPU-001 step 2). Pure logic, no I/O;
-- proved at GNATprove level 2.
--
-- Every job the driver publishes on the context is accepted here with the
-- timeline value its breadcrumb writes: Last_Accepted + 1, so the values of
-- the jobs owed are consecutive and a job's slot is its value modulo the
-- capacity. The GPU-written timeline is accepted only if it is monotonic and
-- no greater than Last_Accepted (Intel_GPU_Timeline.Classify); it completes
-- every job up to it. Each owed job is popped exactly once: Completed when
-- the timeline reached it, Lost when the context hung or the device was
-- lost first. Never Completed for work the GPU did not complete.
--
-- Health:
--   Active  - takes jobs.
--   Faulted - takes no more jobs (a descriptor was refused); jobs already
--             on the GPU still complete normally.
--   Hung    - the watchdog fired: no timeline progress within the hang
--             budget, or the oldest unfinished job's deadline passed.
--   Lost    - the timeline regressed or went past what was published, it
--             could not be read, the clock failed, or the device or session
--             was lost. Hung and Lost fail every owed job.
-- Until reset recovery exists (step 7), Hung and Lost are final.
package Intel_GPU_Context_Ledger with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   package Q renames CuBit.GPU_Queues;
   subtype Value is Intel_GPU_Timeline.Value;
   subtype Microseconds is Intel_GPU_Timeline.Microseconds;
   use type Value, Microseconds, Q.Fault_Reason;

   Max_In_Flight : constant := Q.Max_In_Flight_Per_Context;
   subtype Owed_Count is Natural range 0 .. Max_In_Flight;
   subtype Reason is Q.Fault_Reason;

   -- From_Call: the synchronous 0x0A27 wrapper's job (at most one per
   -- context); From_Queue: a descriptor taken from the session queue.
   type Origin is (From_Queue, From_Call);
   type Health is (Active, Faulted, Hung, Lost);
   subtype Failed_Health is Health range Hung .. Lost;

   type Job is record
      Token : Unsigned_64 := 0;
      Deadline : Microseconds := 0;
      Submitted : Microseconds := 0;
   end record;

   type Ledger is private;

   function Valid (L : Ledger) return Boolean;
   function Accepted (L : Ledger) return Value;    -- Last_Accepted
   function Completed (L : Ledger) return Value;
   -- Jobs owed a pop.
   function Count (L : Ledger) return Owed_Count;
   function Call_Pending (L : Ledger) return Boolean;
   -- The value of the pending From_Call job (meaningful while one is).
   function Call_Value (L : Ledger) return Value;
   function State (L : Ledger) return Health;
   function Why (L : Ledger) return Reason;
   function Progress_At (L : Ledger) return Microseconds;
   -- The value of the oldest owed job.
   function First_Owed (L : Ledger) return Value
     with Pre => Valid (L) and then Count (L) > 0;
   -- Queue-origin jobs owed: each owes its session one completion record.
   function Queued (L : Ledger) return Owed_Count is
     (if Call_Pending (L) and then Count (L) > 0 then Count (L) - 1 else Count (L));
   -- Jobs the GPU has not completed.
   function Unfinished (L : Ledger) return Boolean is (Completed (L) < Accepted (L))
     with Pre => Valid (L);
   -- The owed job with value V.
   function Job_At (L : Ledger; V : Value) return Job
     with Pre => Valid (L) and then Count (L) > 0 and then
                 V >= First_Owed (L) and then V <= Accepted (L);

   -- After the setup breadcrumb Done: nothing owed.
   function Started (Done : Value; Now : Microseconds) return Ledger
     with Pre => Done < Value'Last - Max_In_Flight - 1,
          Post => Valid (Started'Result) and then Count (Started'Result) = 0 and then
                  Accepted (Started'Result) = Done and then
                  Completed (Started'Result) = Done and then
                  State (Started'Result) = Active and then
                  not Call_Pending (Started'Result) and then
                  Progress_At (Started'Result) = Now;

   -- Values stop short of the top: retire the context before exhaustion.
   Last_Usable : constant Value := Value'Last - Max_In_Flight - 1;

   function Can_Accept (L : Ledger; Next : Value; Source : Origin) return Boolean is
     (Valid (L) and then State (L) = Active and then Count (L) < Max_In_Flight and then
      Accepted (L) < Last_Usable and then Next = Accepted (L) + 1 and then
      (if Source = From_Call then not Call_Pending (L)));

   procedure Accept_Job
     (L : in out Ledger; Next : Value; Source : Origin; Item : Job; Now : Microseconds)
     with Pre => Can_Accept (L, Next, Source),
          Post => Valid (L) and then Accepted (L) = Next and then
                  Completed (L) = Completed (L)'Old and then
                  Count (L) = Count (L)'Old + 1 and then
                  State (L) = State (L)'Old and then Why (L) = Why (L)'Old and then
                  Call_Pending (L) = (Call_Pending (L)'Old or Source = From_Call) and then
                  Queued (L) = Queued (L)'Old + (if Source = From_Queue then 1 else 0) and then
                  Job_At (L, Next) = Item and then
                  First_Owed (L) = (if Count (L'Old) = 0 then Next else First_Owed (L'Old)) and then
                  -- No owed job is overwritten.
                  (for all V in First_Owed (L) .. Accepted (L)'Old => Job_At (L, V) = Job_At (L'Old, V))
                  and then
                  Progress_At (L) =
                    (if Unfinished (L)'Old then Progress_At (L)'Old else Now);

   type Observation is
     (Not_Active,     -- Hung or Lost already: nothing observed
      Idle,           -- nothing unfinished
      Unchanged,      -- unfinished, no progress, watchdogs quiet
      Advanced,       -- the timeline completed one or more jobs
      Held,           -- the GPU progressed but the gate is closed
      Failed);        -- this observation hung or lost the context

   -- One non-blocking observation of the timeline and the watchdogs.
   -- Gate_Open carries any further completion condition (the GuC
   -- acknowledged the enable that submitted the work). Hang_Budget: the
   -- longest the GPU may go without progress; explicit, no default.
   procedure Observe
     (L : in out Ledger; Read_OK : Boolean; Observed : Value; Gate_Open : Boolean;
      Now, Hang_Budget : Microseconds; Result : out Observation)
     with Pre => Valid (L) and then Hang_Budget > 0,
          Post => Valid (L) and then Accepted (L) = Accepted (L)'Old and then
                  Count (L) = Count (L)'Old and then
                  Call_Pending (L) = Call_Pending (L)'Old and then
                  Completed (L) >= Completed (L)'Old and then
                  (if State (L)'Old in Failed_Health then
                     Result = Not_Active and L = L'Old) and then
                  (if Result = Advanced then
                     Read_OK and Gate_Open and Completed (L) = Observed and
                     Observed > Completed (L)'Old and Observed <= Accepted (L)
                   else Completed (L) = Completed (L)'Old) and then
                  (if Result = Failed then
                     State (L) in Failed_Health and State (L)'Old not in Failed_Health
                   else State (L) = State (L)'Old and Why (L) = Why (L)'Old) and then
                  (if Result = Idle then not Unfinished (L)'Old) and then
                  -- A timeline that regressed or passed what was published,
                  -- or could not be read, is never accepted.
                  (if State (L)'Old not in Failed_Health and then Unfinished (L)'Old and then
                     (not Read_OK or else
                      Intel_GPU_Timeline.Classify (Completed (L)'Old, Accepted (L)'Old, Observed)
                        in Intel_GPU_Timeline.Regressed | Intel_GPU_Timeline.Beyond_Published)
                   then Result = Failed and State (L) = Lost);

   -- The oldest owed job can be popped: the timeline reached it, or the
   -- context hung or was lost.
   function Head_Ready (L : Ledger) return Boolean is
     (Valid (L) and then Count (L) > 0 and then
      (First_Owed (L) <= Completed (L) or else State (L) in Failed_Health));

   type Pop_Status is (Done, Lost_Job);
   procedure Pop
     (L : in out Ledger; Item : out Job; V : out Value; Source : out Origin;
      Status : out Pop_Status)
     with Pre => Head_Ready (L),
          Post => Valid (L) and then Count (L) = Count (L)'Old - 1 and then
                  V = First_Owed (L'Old) and then Item = Job_At (L'Old, V) and then
                  Accepted (L) = Accepted (L)'Old and then
                  Completed (L) = Completed (L)'Old and then
                  State (L) = State (L)'Old and then
                  (if Count (L) > 0 then First_Owed (L) = V + 1) and then
                  (Status = Done) = (V <= Completed (L)'Old) and then
                  (if Status = Lost_Job then State (L) in Failed_Health) and then
                  Source = (if Call_Pending (L)'Old and then V = Call_Value (L'Old)
                            then From_Call else From_Queue) and then
                  Call_Pending (L) = (Call_Pending (L)'Old and Source = From_Queue) and then
                  Queued (L) = Queued (L)'Old - (if Source = From_Queue then 1 else 0);
   -- No more jobs are taken; those on the GPU still complete.
   procedure Fault (L : in out Ledger; Cause : Reason)
     with Pre => Valid (L),
          Post => Valid (L) and then
                  State (L) = (if State (L)'Old = Active then Faulted else State (L)'Old) and then
                  Accepted (L) = Accepted (L)'Old and then Completed (L) = Completed (L)'Old and then
                  Count (L) = Count (L)'Old and then Call_Pending (L) = Call_Pending (L)'Old and then
                  (if State (L)'Old = Active then Why (L) = Cause else Why (L) = Why (L)'Old);

   -- The device or the session is lost: every owed job fails.
   procedure Lose (L : in out Ledger; Cause : Reason)
     with Pre => Valid (L),
          Post => Valid (L) and then State (L) in Failed_Health and then
                  (if State (L)'Old not in Failed_Health then State (L) = Lost and Why (L) = Cause
                   else State (L) = State (L)'Old and Why (L) = Why (L)'Old) and then
                  Accepted (L) = Accepted (L)'Old and then Completed (L) = Completed (L)'Old and then
                  Count (L) = Count (L)'Old and then Call_Pending (L) = Call_Pending (L)'Old;
private
   type Job_Slot is range 0 .. Max_In_Flight - 1;
   type Job_Array is array (Job_Slot) of Job;
   type Ledger is record
      Jobs : Job_Array;
      Owed : Owed_Count := 0;
      Last, Done, Seen : Value := 0;
      Call : Value := 0;
      Current : Health := Active;
      Cause : Reason := Q.None;
      Progress : Microseconds := 0;
   end record;

   function Slot_Of (V : Value) return Job_Slot is (Job_Slot (V mod Max_In_Flight));

   function Valid (L : Ledger) return Boolean is
     (L.Last <= Last_Usable and then L.Done <= L.Last and then
      Value (L.Owed) <= L.Last and then
      -- Jobs the GPU has not completed are owed, unless the context failed
      -- (its failed jobs are then popped unfinished).
      (if L.Current not in Failed_Health then L.Last - L.Done <= Value (L.Owed)) and then
      L.Seen >= L.Done and then L.Seen <= L.Last and then
      (L.Call = 0 or else
         (L.Owed > 0 and then L.Call > L.Last - Value (L.Owed) and then L.Call <= L.Last)));

   function Accepted (L : Ledger) return Value is (L.Last);
   function Completed (L : Ledger) return Value is (L.Done);
   function Count (L : Ledger) return Owed_Count is (L.Owed);
   function Call_Pending (L : Ledger) return Boolean is (L.Call /= 0);
   function Call_Value (L : Ledger) return Value is (L.Call);
   function State (L : Ledger) return Health is (L.Current);
   function Why (L : Ledger) return Reason is (L.Cause);
   function Progress_At (L : Ledger) return Microseconds is (L.Progress);
   function First_Owed (L : Ledger) return Value is (L.Last - Value (L.Owed) + 1);
   function Job_At (L : Ledger; V : Value) return Job is (L.Jobs (Slot_Of (V)));
end Intel_GPU_Context_Ledger;
