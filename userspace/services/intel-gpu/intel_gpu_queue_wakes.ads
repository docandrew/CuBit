with CuBit.GPU_Queues;
with Intel_GPU_Timeline;
-- The wake request of a GPU session queue (OP_GPU_WAKE, GPU-001 step 2):
-- when a held wake is answered. Pure logic; proved at GNATprove level 2.
--
-- The driver keeps at most one wake per session, as a saved reply
-- capability. Its condition: the context's completed value reached the
-- target, the context failed, or the queue ended. It is answered as soon as
-- the condition holds (at once if it already does), when a newer wake
-- supersedes it (Woken: look again), or when the queue ends. Each held wake
-- is answered exactly once: Hold is the only way in, and every way out says
-- to answer it. A wake that would need a saved-reply slot while the
-- driver's slots are all held by other sessions is answered Not_Held at
-- once; nothing is held for it.
package Intel_GPU_Queue_Wakes with SPARK_Mode is
   package Q renames CuBit.GPU_Queues;
   subtype Value is Intel_GPU_Timeline.Value;
   use type Value, Q.Wake_Result;

   type Phase is (Idle, Held);
   type State is record
      Current : Phase := Idle;
      Context : Q.Context_Index := 0;
      Target : Value := 0;
   end record;

   function Condition (Completed, Target : Value; Failed, Ended : Boolean) return Boolean is
     (Completed >= Target or else Failed or else Ended);

   -- What to do with a wake request that arrives.
   type Arrival is record
      -- Answer the held wake first (Woken; its slot is reused).
      Answer_Held : Boolean := False;
      -- Answer the new request at once (with Result); otherwise hold it.
      Answer_Now : Boolean := False;
      Result : Q.Wake_Result := Q.Woken;
   end record;

   -- A wake request for Context and Target arrives. Holds: its condition
   -- holds now. Slot_Free: a saved-reply slot is free for this session
   -- (superseding its own held wake reuses that one's slot).
   procedure Arrive
     (Item : in out State; Context : Q.Context_Index; Target : Value;
      Holds, Slot_Free : Boolean; Outcome : out Arrival)
     with Global => null,
          Post => Outcome.Answer_Held = (Item'Old.Current = Held) and then
                  Outcome.Answer_Now =
                    (Holds or else not (Slot_Free or else Item'Old.Current = Held)) and then
                  (if Outcome.Answer_Now then
                     Outcome.Result = (if Holds then Q.Woken else Q.Not_Held) and
                     Item.Current = Idle
                   else Item = State'(Current => Held, Context => Context, Target => Target));

   -- The request could not be held (its reply capability was not saved):
   -- it is answered as failed, and nothing is held.
   procedure Hold_Failed (Item : in out State)
     with Global => null, Pre => Item.Current = Held, Post => Item.Current = Idle;

   -- One loop turn: answer the held wake if its condition now holds.
   procedure Step (Item : in out State; Holds : Boolean; Answer_Held : out Boolean)
     with Global => null,
          Post => Answer_Held = (Item'Old.Current = Held and Holds) and then
                  (if Answer_Held then Item.Current = Idle else Item = Item'Old);

   -- The queue ends: a held wake is answered.
   procedure Ended (Item : in out State; Answer_Held : out Boolean)
     with Global => null,
          Post => Answer_Held = (Item'Old.Current = Held) and then Item.Current = Idle;
end Intel_GPU_Queue_Wakes;
