------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The packet grant between a network driver and netstack: one header
--  page, then two rings of frame slots, receive (driver to netstack) and
--  transmit (netstack to driver). Both sides use this layout and the
--  Rings instance of CuBit.Slot_Rings for their indices.
--
--  Each direction's header (within the header page):
--  - Produced_At: the producer's free-running frame count;
--  - Space_Wanted_At: nonzero when the producer found the ring full and
--    waits for the consumer to free slots;
--  - Consumed_At (its own cache line): the consumer's count;
--  - Wake_At: a new nonzero epoch each time the consumer is about to
--    sleep; the producer sends one doorbell per epoch it sees.
--
--  Each slot holds one frame: its length (4 bytes, little endian) at the
--  slot's start and the Ethernet frame Frame_At bytes in. A consumer
--  reads the length once, uses it only if Fits, and snapshots the headers
--  it parses; the payload may be read in place, once.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Slot_Rings;

package CuBit.Frame_Rings with Pure, SPARK_Mode is

   Page_Bytes : constant := 4_096;
   Slot_Bytes : constant := 2_048;
   Slot_Bits  : constant := 7;              --  128 frames per direction

   type Frame_Slot is array (0 .. Slot_Bytes - 1) of Unsigned_8;
   package Rings is new CuBit.Slot_Rings (Frame_Slot, Slot_Bits);

   Slots : constant := 2 ** Slot_Bits;

   --  The header page: one header per direction.
   Receive_Header_At  : constant := 0;
   Transmit_Header_At : constant := 2_048;

   --  Within a direction's header.
   Produced_At     : constant := 0;
   Space_Wanted_At : constant := 4;
   Consumed_At     : constant := 64;
   Wake_At         : constant := 68;

   --  The slot areas after the header page.
   Receive_Slots_At  : constant := Page_Bytes;
   Transmit_Slots_At : constant := Receive_Slots_At + Slots * Slot_Bytes;
   Grant_Bytes       : constant := Transmit_Slots_At + Slots * Slot_Bytes;
   Grant_Pages       : constant := Grant_Bytes / Page_Bytes;

   --  Within a slot.
   Length_At : constant := 0;
   Frame_At  : constant := 16;
   Maximum_Frame : constant := Slot_Bytes - Frame_At;

   --  The smallest frame either side accepts: an Ethernet header.
   Minimum_Frame : constant := 14;

   --  A frame length read from a slot is used only if it fits the slot.
   function Fits (Length : Unsigned_32) return Boolean is
     (Length in Minimum_Frame .. Maximum_Frame);

   --  Byte offsets in the grant of a slot in each direction.
   function Receive_Slot_At (S : Rings.Slot) return Natural is
     (Receive_Slots_At + S * Slot_Bytes)
   with Post => Receive_Slot_At'Result + Slot_Bytes <= Transmit_Slots_At;

   function Transmit_Slot_At (S : Rings.Slot) return Natural is
     (Transmit_Slots_At + S * Slot_Bytes)
   with Post => Transmit_Slot_At'Result + Slot_Bytes <= Grant_Bytes;

end CuBit.Frame_Rings;
