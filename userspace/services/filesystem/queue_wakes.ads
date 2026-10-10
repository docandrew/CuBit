------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The wake request of a client's request queue (OP_FS_WAKE,
--  docs/filesystem-protocol-v2.md step 1): when a held wake is answered.
--  The service keeps at most one per queue, as a saved reply capability.
--  It is answered as soon as answers wait in the client's queue (at once
--  if some already do), when a newer wake supersedes it, or when the queue
--  ends. Each held wake is answered exactly once: Hold is the only way in,
--  and every way out says to answer it.
------------------------------------------------------------------------------
package Queue_Wakes with SPARK_Mode, Pure is

   type State is (Idle, Held);

   --  What to do with a wake request that arrives.
   type Arrival is record
      --  Answer the held wake first (superseded; its slot is reused).
      Answer_Held : Boolean := False;
      --  Answer the new request at once; otherwise hold it.
      Answer_Now  : Boolean := False;
   end record;

   --  A wake request arrives; Work_Waiting says answers wait in the queue.
   procedure Arrive
     (Item : in out State; Work_Waiting : Boolean; Outcome : out Arrival)
     with Global => null,
          Post => Outcome.Answer_Held = (Item'Old = Held) and then
                  Outcome.Answer_Now = Work_Waiting and then
                  Item = (if Work_Waiting then Idle else Held);

   --  The request could not be held (its reply capability was not saved):
   --  it is answered as failed, and nothing is held.
   procedure Hold_Failed (Item : in out State)
     with Global => null, Pre => Item = Held, Post => Item = Idle;

   --  An answer was posted to the queue: answer the held wake, if any.
   procedure Posted (Item : in out State; Answer_Held : out Boolean)
     with Global => null,
          Post => Answer_Held = (Item'Old = Held) and then Item = Idle;

   --  The queue ends: a held wake is answered (as failed).
   procedure Ended (Item : in out State; Answer_Held : out Boolean)
     with Global => null,
          Post => Answer_Held = (Item'Old = Held) and then Item = Idle;

end Queue_Wakes;
