with Interfaces; use Interfaces;
with Intel_GPU_Buffer_Backing;
with Intel_GPU_Record_Store;
package Intel_GPU_Deferred_Retirement with SPARK_Mode is
   subtype Slot is Positive;
   type Candidate is record
      Ticket, Session, Sender, Stamp, Handle : Unsigned_64 := 0;
   end record;
   type Outcome is (Waiting, Discarded, Submitted);
   type Queue is limited private;
   function Capacity (Object : Queue) return Positive with SPARK_Mode => Off;
   function Item_At (Object : Queue; Index : Slot) return Candidate
     with SPARK_Mode => Off;
   function Next_Slot (Object : Queue) return Slot with SPARK_Mode => Off;
   -- Trusted committed CPU metadata, same lifetime/nonaliasing obligations as
   -- Record_Store. Growth preserves the poll cursor and existing candidates.
   procedure Extend_Storage
     (Object : in out Queue; Base, Bytes : Unsigned_64; Accepted : out Boolean)
     with SPARK_Mode => Off;
   function Eligible (Index : Slot; Item : Candidate) return Boolean is
     (Item.Ticket /= 0 and then Item.Session /= 0 and then Item.Sender /= 0 and then
      Item.Handle /= 0 and then
      Item.Ticket <= Intel_GPU_Buffer_Backing.Ticket_Limit and then
      Item.Ticket mod Intel_GPU_Buffer_Backing.Ticket_Stride = Unsigned_64 (Index));
   function After_Remember (Previous : Candidate; Index : Slot; Item : Candidate)
     return Candidate is
       (if Eligible (Index, Item) and then
           (Previous.Ticket = 0 or else Item.Ticket > Previous.Ticket)
        then Item else Previous);
   -- Trusted serialized dispatcher only. These are saved kernel-envelope
   -- identities, not authority minted from application request words.
   procedure Remember (Object : in out Queue; Index : Slot; Item : Candidate)
     with SPARK_Mode => Off;
   generic
      with function Attempt (Index : Slot; Item : Candidate) return Outcome;
   procedure Poll (Object : in out Queue) with SPARK_Mode => Off;
   -- Poll visits at most one pending candidate, fairly; empty metadata slots
   -- consume no service turns. Callback must not mutate/reenter this queue.
   -- Waiting may repeat preflight only;
   -- Submitted removes it immediately even when completion is asynchronous.
   -- Discarded forgets an observation, never frees its backing. The callback
   -- must revalidate identity, ownership, grants and GPU/VM retirement.
private
   pragma SPARK_Mode (Off);
   type Entry_Record is record
      Item : Candidate := (others => 0);
      Next : Natural := 0;
   end record;
   package Records is new Intel_GPU_Record_Store (Entry_Record, (others => <>));
   type Queue is limited record
      Items : Records.Store;
      Head, Tail : Natural := 0;
   end record;
end Intel_GPU_Deferred_Retirement;
