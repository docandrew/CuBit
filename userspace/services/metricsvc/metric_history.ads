with Interfaces; use Interfaces;
with CuBit.Metric_Records;
with CuBit.Metric_Protocol;
-- Serialized service-owned history, never an ownership fence. Each accepted
-- metric is copied once; readers do not mutate history or block append.
-- Transport must bind cursors to a service incarnation and authenticate peers.
generic
   Capacity : Positive := 256;
   Sequence_Limit : Unsigned_64 := Unsigned_64'Last;
package Metric_History with SPARK_Mode is
   package R renames CuBit.Metric_Records;
   package P renames CuBit.Metric_Protocol;
   type Event is record
      Sequence, Pid, Publisher, Batch, Producer_Dropped, Batch_Gaps : Unsigned_64 := 0;
      Value : R.Metric_Record;
   end record;
   function Valid_Event (E : Event) return Boolean is
     (E.Sequence /= 0 and E.Pid /= 0 and P.Is_Publisher (E.Publisher) and
      E.Batch /= 0 and R.Valid (E.Value));
   type State is private with Default_Initial_Condition => Valid (State);
   function Valid (S : State) return Boolean;
   function First (S : State) return Unsigned_64;
   function Following (S : State) return Unsigned_64;
   function Exhausted (S : State) return Boolean;
   function Item (S : State; Sequence : Unsigned_64) return Event
     with Pre => Valid (S) and Sequence >= First (S) and Sequence < Following (S),
       Post => Item'Result.Sequence = Sequence and Valid_Event (Item'Result);
   procedure Append (S : in out State; Pid, Publisher, Batch,
      Producer_Dropped, Batch_Gaps : Unsigned_64; Value : R.Metric_Record;
      Accepted : out Boolean)
     with Pre => Valid (S) and Pid /= 0 and P.Is_Publisher (Publisher) and Batch /= 0 and R.Valid (Value),
       Post => Valid (S) and then Accepted = not Exhausted (S'Old) and then
         (if Accepted then Following (S) = Following (S'Old) + 1 and then
            Item (S, Following (S'Old)) =
              (Following (S'Old), Pid, Publisher, Batch, Producer_Dropped, Batch_Gaps, Value)
            and then (for all Q in First (S) .. Following (S'Old) - 1 =>
              Item (S, Q) = Item (S'Old, Q))
          else S = S'Old);
   -- Cursor identifies the next desired event (starts at 1). Gap is the
   -- precise number overwritten before that cursor. Following is an empty
   -- successful read. Zero/future cursors are rejected without advancement.
   procedure Read (S : State; Cursor : Unsigned_64; Value : out Event;
      Next, Gap : out Unsigned_64; Available, Valid_Cursor : out Boolean)
     with Pre => Valid (S),
       Post => Valid_Cursor = (Cursor /= 0 and Cursor <= Following (S)) and then
         (if Valid_Cursor then
            Gap = Unsigned_64'Max (Cursor, First (S)) - Cursor and then
            Available = (Unsigned_64'Max (Cursor, First (S)) < Following (S)) and then
            (if Available then Valid_Event (Value) and then Value = Item (S, Unsigned_64'Max (Cursor, First (S))) and then
                Next = Value.Sequence + 1
             else Next = Following (S))
          else not Available and Next = Cursor and Gap = 0);
private
   subtype Slot is Positive range 1 .. Capacity;
   function Index (Sequence : Unsigned_64) return Slot is
     (Slot (Sequence mod Unsigned_64 (Capacity) + 1));
   type Events is array (Slot) of Event;
   type State is record
      Next : Unsigned_64 := 1;
      Values : Events;
   end record;
   function Following (S : State) return Unsigned_64 is (S.Next);
   function First (S : State) return Unsigned_64 is
     (if S.Next > Unsigned_64 (Capacity) then S.Next - Unsigned_64 (Capacity) else 1);
   function Exhausted (S : State) return Boolean is (S.Next >= Sequence_Limit);
   function Valid (S : State) return Boolean is
     (S.Next >= 1 and then
      (for all Q in First (S) .. S.Next - 1 =>
         S.Values (Index (Q)).Sequence = Q and Valid_Event (S.Values (Index (Q)))));
   function Item (S : State; Sequence : Unsigned_64) return Event is
     (S.Values (Index (Sequence)));
end Metric_History;
