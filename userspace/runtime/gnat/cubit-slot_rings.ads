------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  One direction of a shared ring of fixed-size elements between two
--  processes: the index bookkeeping for its producer and its consumer.
--  Frames between a driver and netstack, and submission and completion
--  entries (docs/async-rings.md), are instances.
--
--  The same trust rules as CuBit.Channel_Rings: each side keeps its own
--  index privately and accepts the peer's only if it moves forward and
--  keeps the fill within the ring, so a peer that writes arbitrary
--  indices cannot make this side touch a slot it does not own. Callers
--  snapshot what they decide on (an entry, a frame's headers) before
--  checking it; payload may be read in place, once (docs/async-rings.md).
--  Take copies a whole element, which suits small entries; for large
--  slots, use Head_Slot and Release.
--
--  Indices are free-running 32-bit element counts. The slot count is a
--  power of two (2 ** Slot_Bits), so it divides 2 ** 32 and an index's
--  slot is its low Slot_Bits bits, consistently across wrap-around.
--
--  Proved (tests/channel-rings, through instances): every slot is inside
--  the ring; the fill never exceeds the slot count; private indices only
--  move forward, by exactly the elements pushed or taken; a rejected peer
--  index changes nothing; and indices less than a ring apart never share
--  a slot, so an element in flight is never overwritten. Memory ordering
--  between the processes belongs to the callers (volatile accesses and
--  fences), which SPARK does not model.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Channel_Rings;

generic
   type Element is private;
   Slot_Bits : Natural;
package CuBit.Slot_Rings with Pure, SPARK_Mode is

   pragma Compile_Time_Error
     (Slot_Bits > 16, "a slot ring has at most 2 ** 16 slots");

   subtype Index is CuBit.Channel_Rings.Index;
   subtype Count is CuBit.Channel_Rings.Count;
   use type Index, Count;

   Slots : constant Positive := 2 ** Slot_Bits;
   Mask  : constant Index := Index (Slots - 1);

   subtype Slot is Natural range 0 .. Slots - 1;
   subtype Fill_Count is Natural range 0 .. Slots;

   type Ring is array (Slot) of Element;

   function Distance (From, To : Index) return Count
     renames CuBit.Channel_Rings.Distance;

   --  Where element I lives.
   function Slot_Of (I : Index) return Slot is (Slot (I and Mask));

   --  Elements less than a ring apart never share a slot.
   procedure Lemma_Distinct (I, J : Index) with
     Ghost, Global => null,
     Pre  => J - I in 1 .. Mask,
     Post => Slot_Of (I) /= Slot_Of (J);

   ------------------------------------------------------------------------
   --  The producing side. The peer's consumed index is Produced - Fill.
   ------------------------------------------------------------------------
   type Producer is record
      Produced : Index := 0;          --  ours
      Fill     : Fill_Count := 0;     --  pushed, not yet released
   end record;

   function Space (P : Producer) return Fill_Count is (Slots - P.Fill);

   function Consumed (P : Producer) return Index is
     (P.Produced - Index (P.Fill));

   --  How many elements the peer's consumed index Value would release.
   function Released (P : Producer; Value : Index) return Count is
     (Distance (Consumed (P), Value));

   function New_Producer (Origin : Index := 0) return Producer is
     ((Produced => Origin, Fill => 0));

   --  Take the peer's consumed index if it lies between the last one
   --  accepted and what has been produced.
   procedure Accept_Consumed
     (P : in out Producer; Value : Index; OK : out Boolean)
   with
     Post => OK = (Released (P'Old, Value) <= Count (P'Old.Fill)) and then
             (if OK then
                P = (P'Old with delta
                       Fill => P'Old.Fill - Natural (Released (P'Old, Value)))
              else P = P'Old);

   --  The slot the next element goes into (write it there, then Commit).
   function Next_Slot (P : Producer) return Slot is (Slot_Of (P.Produced));

   --  Publish the element written at Next_Slot.
   procedure Commit (P : in out Producer) with
     Pre  => Space (P) > 0,
     Post => P = (P'Old with delta Produced => P'Old.Produced + 1,
                                   Fill     => P'Old.Fill + 1);

   --  Copy E into the next slot and publish it.
   procedure Push (P : in out Producer; R : in out Ring; E : Element) with
     Pre  => Space (P) > 0,
     Post => R (Slot_Of (P'Old.Produced)) = E and then
             (for all S in Slot =>
                (if S /= Slot_Of (P'Old.Produced) then R (S) = R'Old (S)))
             and then
             P = (P'Old with delta Produced => P'Old.Produced + 1,
                                   Fill     => P'Old.Fill + 1);

   --  Pushed elements not yet released are never overwritten: the slot
   --  Next_Slot names holds none of them.
   procedure Lemma_Free_Slot (P : Producer; K : Index) with
     Ghost, Global => null,
     Pre  => Space (P) > 0 and then K - Consumed (P) < Index (P.Fill),
     Post => Slot_Of (K) /= Next_Slot (P);

   ------------------------------------------------------------------------
   --  The consuming side. The peer's produced index is Consumed + Available.
   ------------------------------------------------------------------------
   type Consumer is record
      Consumed  : Index := 0;         --  ours
      Available : Fill_Count := 0;    --  produced by the peer, not yet taken
   end record;

   function Produced (C : Consumer) return Index is
     (C.Consumed + Index (C.Available));

   function New_Consumer (Origin : Index := 0) return Consumer is
     ((Consumed => Origin, Available => 0));

   --  Take the peer's produced index if it does not go back past the last
   --  one accepted and does not overfill the ring.
   procedure Accept_Produced
     (C : in out Consumer; Value : Index; OK : out Boolean)
   with
     Post => OK = (Distance (C'Old.Consumed, Value) <= Count (Slots)
                   and then Distance (C'Old.Consumed, Value) >=
                            Count (C'Old.Available)) and then
             (if OK then
                C = (C'Old with delta
                       Available => Natural (Distance (C'Old.Consumed, Value)))
              else C = C'Old);

   --  The slot of the oldest element not yet taken.
   function Head_Slot (C : Consumer) return Slot is (Slot_Of (C.Consumed));

   --  Release the element at Head_Slot (after copying it out).
   procedure Release (C : in out Consumer) with
     Pre  => C.Available > 0,
     Post => C = (C'Old with delta Consumed  => C'Old.Consumed + 1,
                                   Available => C'Old.Available - 1);

   --  Copy the oldest element out and release it.
   procedure Take (C : in out Consumer; R : Ring; E : out Element) with
     Pre  => C.Available > 0,
     Post => E = R (Slot_Of (C'Old.Consumed)) and then
             C = (C'Old with delta Consumed  => C'Old.Consumed + 1,
                                   Available => C'Old.Available - 1);

end CuBit.Slot_Rings;
