with Interfaces;

--  One serialized owner, one registration, eight pending discrete events.
--  References/tickets are scoped to THIS owner; they are not process-global
--  capabilities and must never be routed using a token alone.
generic
   Maximum_Identity : Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Last;
package CCL.Callback_Queues with SPARK_Mode is
   use type Interfaces.Unsigned_64;
   Capacity : constant := 8;
   subtype Pending_Count is Natural range 0 .. Capacity;
   type Lifecycle is (Empty, Listening, Executing, Draining, Stopped, Faulted);
   type Open_Result is (Opened, Busy, Identity_Exhausted);
   type Enqueue_Result is (Enqueued, Inactive, Stale_Target, Queue_Full, Identity_Exhausted);
   type Queue is limited private;
   type Reference is private;
   type Invocation is private;
   function State (Item : Queue) return Lifecycle;
   function Pending (Item : Queue) return Pending_Count;
   procedure Open (Item : in out Queue; Target : out Reference; Result : out Open_Result)
     with Post => (if Result = Opened then State (Item) = Listening and Pending (Item) = 0);
   procedure Enqueue
     (Item : in out Queue; Target : Reference; Result : out Enqueue_Result)
     with Post => Pending (Item) = Pending (Item)'Old + (if Result = Enqueued then 1 else 0);
   procedure Claim (Item : in out Queue; Ticket : out Invocation; Ready : out Boolean)
     with Post => Pending (Item) + (if Ready then 1 else 0) = Pending (Item)'Old and
       (if Ready then State (Item) = Executing);
   function After_Close (Before : Lifecycle) return Lifecycle with Ghost;
   procedure Close (Item : in out Queue; Discarded : out Pending_Count)
     with Post => Pending (Item) = 0 and Discarded = Pending (Item)'Old and
       State (Item) = After_Close (State (Item)'Old);
   function Matches_Active (Item : Queue; Ticket : Invocation) return Boolean with Ghost;
   procedure Complete
     (Item : in out Queue; Ticket : Invocation; Succeeded : Boolean;
      Accepted : out Boolean; Discarded : out Pending_Count)
     with Post => Accepted = Matches_Active (Item, Ticket)'Old;
private
   subtype Identity is Interfaces.Unsigned_64 range 0 .. Maximum_Identity;
   type Reference is record
      Generation : Identity := 0;
   end record;
   type Invocation is record
      Generation, Sequence : Identity := 0;
   end record;
   subtype Slot is Positive range 1 .. Capacity;
   type Event_Array is array (Slot) of Identity;
   type Queue is limited record
      Phase : Lifecycle := Empty;
      Generation, Issued : Identity := 0;
      Events : Event_Array := [others => 0];
      Head : Slot := Slot'First;
      Count : Pending_Count := 0;
      Active : Invocation;
   end record;
   function State (Item : Queue) return Lifecycle is (Item.Phase);
   function Pending (Item : Queue) return Pending_Count is (Item.Count);
   function After_Close (Before : Lifecycle) return Lifecycle is
     (case Before is when Empty => Empty, when Executing | Draining => Draining,
      when others => Stopped);
   function Matches_Active (Item : Queue; Ticket : Invocation) return Boolean is
     (Item.Phase in Executing | Draining and then
      Ticket.Generation = Item.Generation and then Ticket.Sequence /= 0 and then Ticket = Item.Active);
end CCL.Callback_Queues;
