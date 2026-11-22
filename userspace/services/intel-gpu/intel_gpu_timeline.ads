with Interfaces; use Interfaces;
-- Per-context GPU timeline acceptance and deferred completion (GPU-001).
-- Pure logic, no I/O: the native layer reads the PPHWSP timeline slot and
-- the monotonic clock and feeds the values in. Proved at GNATprove level 2.
--
-- The timeline is level state: the GPU's final breadcrumb is its only
-- writer, so an observation can only stay put, advance up to what the
-- driver published, or reveal corruption. Completion is decided here; the
-- caller owns the reply, quarantine and every hardware fact.
package Intel_GPU_Timeline with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   type Value is new Unsigned_64;
   type Microseconds is new Unsigned_64;
   -- The clock reports this when it is unavailable.
   Clock_Unavailable : constant Microseconds := Microseconds'Last;

   -- The quadword is read as high, low, high DWORDs (H1: Linux never shows
   -- the GPU's quadword store to be single-copy atomic). A changed high
   -- half means the store raced the read; the caller reads again.
   type Half is new Unsigned_32;
   type Read_Result is record
      Stable : Boolean := False;
      Observed : Value := 0;
   end record;
   function Combine (High_First, Low, High_Second : Half) return Read_Result
     with Post => Combine'Result.Stable = (High_First = High_Second) and then
       (if Combine'Result.Stable then
          Combine'Result.Observed =
            Value (High_First) * 2 ** 32 + Value (Low));

   -- Acceptance of one observation against what is known to have completed
   -- and what the driver has published (Completed < Published).
   type Observation is (Unchanged, Advanced, Reached, Regressed, Beyond_Published);
   function Classify (Completed, Published, Observed : Value) return Observation
     with Pre => Completed < Published,
       Post => (case Classify'Result is
                  when Unchanged => Observed = Completed,
                  when Advanced => Observed > Completed and Observed < Published,
                  when Reached => Observed = Published,
                  when Regressed => Observed < Completed,
                  when Beyond_Published => Observed > Published);

   -- Monotonic and no greater than what was published.
   function Accepted (Item : Observation) return Boolean is
     (Item in Unchanged | Advanced | Reached);

   -- One deferred completion: a published target with an explicit,
   -- absolute deadline. Exactly one terminal outcome per Start.
   type Outcome is
     (Pending,          -- keep waiting: not reached (or gate closed), in time
      Complete,         -- the timeline reached the target and the gate is open
      Timeline_Fault,   -- regressed or beyond what was published
      Read_Fault,       -- the slot could not be read stably
      Clock_Fault,      -- clock unavailable or moved backwards
      Deadline_Expired, -- the deadline passed before completion
      Not_Waiting);     -- no completion outstanding: nothing changed
   subtype Terminal is Outcome range Complete .. Deadline_Expired;

   type Waiter is private;
   function Waiting (Object : Waiter) return Boolean;
   function Completed (Object : Waiter) return Value;
   function Target (Object : Waiter) return Value;
   function Deadline (Object : Waiter) return Microseconds;
   function Last_Seen (Object : Waiter) return Value;

   -- Arms the next completion: Completed is the value already observed,
   -- Target its successor. Budget is the caller's explicit bound, no default.
   function Can_Start (Object : Waiter; Done, Target_Value : Value;
                       Now, Budget : Microseconds) return Boolean is
     (not Waiting (Object) and then Done < Value'Last and then
      Target_Value = Done + 1 and then Budget > 0 and then
      Now < Clock_Unavailable and then Budget < Clock_Unavailable - Now);
   procedure Start (Object : in out Waiter; Done, Target_Value : Value;
                    Now, Budget : Microseconds; Started : out Boolean)
     with Post => Started = Can_Start (Object'Old, Done, Target_Value, Now, Budget)
       and then (if Started then
                   Waiting (Object) and Completed (Object) = Done and
                   Target (Object) = Target_Value and
                   Deadline (Object) = Now + Budget and Last_Seen (Object) = Done
                 else Object = Object'Old);

   -- One non-blocking step. Gate_Open carries any extra condition the
   -- completion needs besides the timeline (e.g. the GuC acknowledged the
   -- enable that submitted the work). A reached timeline is authoritative:
   -- it completes even when observed after the deadline, but a pending one
   -- never outlives the deadline.
   procedure Step (Object : in out Waiter; Read_OK : Boolean; Observed : Value;
                   Gate_Open : Boolean; Now : Microseconds; Result : out Outcome)
     with Post =>
       (if not Waiting (Object'Old) then
          Result = Not_Waiting and Object = Object'Old
        else
          Result /= Not_Waiting and then
          (if Result = Pending then
             Waiting (Object) and Now < Deadline (Object'Old) and
             Read_OK and not (Gate_Open and Observed = Target (Object'Old)) and
             Completed (Object) = Completed (Object'Old) and
             Target (Object) = Target (Object'Old) and
             Deadline (Object) = Deadline (Object'Old) and
             Accepted (Classify (Completed (Object'Old), Target (Object'Old), Observed))
           else not Waiting (Object)) and then
          (if Result = Complete then
             Read_OK and Gate_Open and Observed = Target (Object'Old) and
             Completed (Object) = Target (Object'Old)
           else Completed (Object) = Completed (Object'Old)) and then
          (if Result = Timeline_Fault then
             Read_OK and not Accepted (Classify
               (Completed (Object'Old), Target (Object'Old), Observed))) and then
          (if Result = Deadline_Expired then Now >= Deadline (Object'Old)));

   -- Abandon an outstanding completion (ownership lost): no outcome is
   -- manufactured; the caller reports the fault it observed.
   procedure Abandon (Object : in out Waiter)
     with Post => not Waiting (Object) and Completed (Object) = Completed (Object'Old);
private
   type Waiter is record
      Active : Boolean := False;
      Done, Goal, Seen : Value := 0;
      Limit, Previous : Microseconds := 0;
   end record;
   -- An active waiter always has a published target beyond its completed
   -- value and a deadline the clock can reach.
   function Waiting (Object : Waiter) return Boolean is
     (Object.Active and then Object.Done < Object.Goal and then
      Object.Limit /= Clock_Unavailable);
   function Completed (Object : Waiter) return Value is (Object.Done);
   function Target (Object : Waiter) return Value is (Object.Goal);
   function Deadline (Object : Waiter) return Microseconds is (Object.Limit);
   function Last_Seen (Object : Waiter) return Value is (Object.Seen);
end Intel_GPU_Timeline;
