------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The index arithmetic of a frame ring shared by netstack and a network
--  driver (the packet grant's RX and TX halves): slot 0 holds the counts,
--  frame N is in slot (N mod Slots) + 1, and the producer and consumer
--  counts are free-running 32-bit frame counts.
--
--  Each side trusts only its own count; the peer's is accepted only if it
--  keeps the ring's fill within its slots and never goes back. A frame's
--  length is used only if it fits its slot.
--
--  Proved (tests/net-tcp): slot numbers are always 1 .. Slots; an accepted
--  count keeps the fill within 0 .. Slots; a frame fits its slot.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Frame_Ring with SPARK_Mode, Pure is

   subtype Slot_Count is Positive range 1 .. 4_095;

   --  The slot holding frame N.
   function Slot_Of (N : Unsigned_32; Slots : Slot_Count) return Positive is
     (Natural (Long_Long_Integer (N) mod Long_Long_Integer (Slots)) + 1)
   with Post => Slot_Of'Result in 1 .. Slots;

   --  Frames between the two counts.
   function Fill (Produced, Consumed : Unsigned_32) return Unsigned_32 is
     (Produced - Consumed);

   --  A consumer takes the producer's count if it is at most a ring ahead
   --  of what was consumed and not behind the count accepted before.
   function Valid_Produced
     (Produced, Accepted, Consumed : Unsigned_32; Slots : Slot_Count)
      return Boolean
   is (Produced - Consumed <= Unsigned_32 (Slots) and then
       Produced - Consumed >= Accepted - Consumed);

   --  A producer takes the consumer's count if it releases no frame that
   --  was not produced and does not go back.
   function Valid_Consumed
     (Consumed, Accepted, Produced : Unsigned_32) return Boolean
   is (Consumed - Accepted <= Produced - Accepted);

   --  Room for one more frame.
   function Has_Room
     (Produced, Consumed : Unsigned_32; Slots : Slot_Count) return Boolean
   is (Produced - Consumed < Unsigned_32 (Slots));

   --  A frame of Length bytes fits a slot of Slot_Bytes after Header bytes.
   function Fits (Length : Unsigned_32; Slot_Bytes, Header : Natural)
     return Boolean
   is (Header < Slot_Bytes and then
       Length <= Unsigned_32 (Slot_Bytes - Header));

   --  The byte offset of slot S in a ring of Slot_Bytes slots.
   function Slot_Offset (S : Positive; Slot_Bytes : Natural) return Natural is
     (S * Slot_Bytes)
   with Pre => S <= Slot_Count'Last and then Slot_Bytes <= 65_536;

end Frame_Ring;
