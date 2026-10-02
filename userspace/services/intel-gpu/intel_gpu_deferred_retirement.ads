with Interfaces; use Interfaces;
with Intel_GPU_Buffer_Backing;
package Intel_GPU_Deferred_Retirement with SPARK_Mode is
   subtype Slot is Intel_GPU_Buffer_Backing.Slot;
   type Candidate is record
      Ticket, Session, Sender, Stamp, Handle : Unsigned_64 := 0;
   end record;
   type Outcome is (Waiting, Discarded, Submitted);
   type Entries is array (Slot) of Candidate;
   type Queue is limited private;
   function Snapshot (Object : Queue) return Entries with Global => null;
   function Next_Slot (Object : Queue) return Slot with Global => null;
   function Eligible (Index : Slot; Item : Candidate) return Boolean is
     (Item.Ticket /= 0 and then Item.Session /= 0 and then Item.Sender /= 0 and then
      Item.Handle /= 0 and then
      Item.Ticket <= Intel_GPU_Buffer_Backing.Ticket_Limit and then
      Item.Ticket mod Intel_GPU_Buffer_Backing.Ticket_Stride = Unsigned_64 (Index));
   function After_Remember (Previous : Entries; Index : Slot; Item : Candidate)
     return Entries is
       (if Eligible (Index, Item) and then
           (Previous (Index).Ticket = 0 or else Item.Ticket > Previous (Index).Ticket)
        then (Previous with delta Index => Item) else Previous);
   -- Trusted serialized dispatcher only. These are saved kernel-envelope
   -- identities, not authority minted from application request words.
   procedure Remember (Object : in out Queue; Index : Slot; Item : Candidate)
     with Global => null,
       Post => Snapshot (Object) = After_Remember (Snapshot (Object)'Old, Index, Item)
         and Next_Slot (Object) = Next_Slot (Object)'Old;
   generic
      with function Attempt (Index : Slot; Item : Candidate) return Outcome;
   procedure Poll (Object : in out Queue) with SPARK_Mode => Off;
   -- Poll visits at most one slot, fairly. Waiting may repeat preflight only;
   -- Submitted removes it immediately even when completion is asynchronous.
   -- Discarded forgets an observation, never frees its backing. The callback
   -- must revalidate identity, ownership, grants and GPU/VM retirement.
private
   type Queue is limited record
      Items : Entries;
      Next_Index : Slot := Slot'First;
   end record;
end Intel_GPU_Deferred_Retirement;
