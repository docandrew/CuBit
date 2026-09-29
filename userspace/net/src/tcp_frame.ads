------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The last step of every TCP segment netstack sends: around a segment
--  already written in place (TCP_Header.Write, then its data) at
--  Frame (Segment_At ..), write the Ethernet and IPv4 headers and the TCP
--  checksum (RFC 9293 3.1, over the pseudo-header and the segment).
--
--  Proved (tests/net-tcp): no run-time errors for any segment up to one
--  frame; the frame is IPv4_Frame.Emittable (unicast source and
--  destination).
--  Tested, not proved: the checksums verify, agreeing with netstack's
--  word-wise transport checksum.
------------------------------------------------------------------------------
with IPv4_Header; use IPv4_Header;
with IPv4_Frame;

package TCP_Frame with SPARK_Mode is

   TCP_Protocol : constant := 6;
   Segment_At   : constant := IPv4_Frame.Ethernet_Header + Minimum_Size;
   TCP_Minimum  : constant := 20;
   Checksum_At  : constant := Segment_At + 16;
   Default_TTL  : constant := 64;

   procedure Finish
     (Frame : in out Bytes; Our_MAC, To_MAC : IPv4_Frame.MAC; Source, Destination : Address)
   with
     Pre  => Frame'First = 0 and then
             Frame'Length in Segment_At + TCP_Minimum .. IPv4_Frame.Maximum_Frame and then
             IPv4_Frame.Unicast (Source) and then IPv4_Frame.Unicast (Destination),
     Post => IPv4_Frame.Emittable (Frame);

   Pseudo_Header : constant := 12;

   --  A received segment's checksum verifies (over the pseudo-header and
   --  Segment, in place).
   function Checksum_OK (Source, Destination : Address; Segment : Bytes)
     return Boolean
   with Pre => Segment'Length in TCP_Minimum .. 65_535 and then
               Segment'Last < Natural'Last;

end TCP_Frame;
