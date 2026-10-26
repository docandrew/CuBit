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

   --  The end of a direction's header (Wake_At's word).
   Direction_Header_Bytes : constant := Wake_At + 4;

   --  The link block (docs/net-link.md): the driver's report of its link
   --  as a level value, in its own cache lines between the two direction
   --  headers. The driver alone writes it. Its words are 32-bit, little
   --  endian; word 0 is the generation (CuBit.Net_Link's seqlock: odd
   --  while the driver writes, even once a report is complete, zero when
   --  none was ever made), words 1 .. Link_Words - 1 the report
   --  (CuBit.Net_Link.Encode). A change costs no message: the driver
   --  rings the receive doorbell it already has, and netstack reads the
   --  block once per pass over the receive ring, never per frame.
   Link_At         : constant := 1_024;
   Link_Word_Bytes : constant := 4;
   Link_Words      : constant := 14;
   Link_Bytes      : constant := Link_Words * Link_Word_Bytes;
   subtype Link_Word_Index is Natural range 0 .. Link_Words - 1;
   Link_Generation_Word : constant Link_Word_Index := 0;

   --  The block lies after the receive header, before the transmit
   --  header, and within the header page.
   pragma Compile_Time_Error
     (Link_At < Receive_Header_At + Direction_Header_Bytes or else
      Link_At + Link_Bytes > Transmit_Header_At or else
      Transmit_Header_At + Direction_Header_Bytes > Page_Bytes,
      "the link block overlaps a direction header or leaves the header page");

   --  Byte offset in the grant of the link block's word W.
   function Link_Word_At (W : Link_Word_Index) return Natural is
     (Link_At + W * Link_Word_Bytes)
   with Post => Link_Word_At'Result >= Receive_Header_At + Direction_Header_Bytes
                and then Link_Word_At'Result + Link_Word_Bytes <= Transmit_Header_At
                and then Link_Word_At'Result mod Link_Word_Bytes = 0;

   --  The scan block (CuBit.Wifi_Scan, docs/wifi.md "Scan"): a wireless
   --  driver's last passive scan result, a level value under the link
   --  block's seqlock discipline (its own generation word), after the
   --  transmit header, within the header page. Only a Wireless interface's
   --  driver writes it; netstack copies it out when an inspector asks
   --  (CuBit.Net_Management.Scan_Results) and never interprets it.
   Scan_At    : constant := 2_176;
   Scan_Bytes : constant := 1_680;   --  CuBit.Wifi_Scan.Block_Bytes

   pragma Compile_Time_Error
     (Scan_At < Transmit_Header_At + Direction_Header_Bytes or else
      Scan_At + Scan_Bytes > Page_Bytes or else Scan_At mod Link_Word_Bytes /= 0,
      "the scan block overlaps the transmit header or leaves the header page");

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
