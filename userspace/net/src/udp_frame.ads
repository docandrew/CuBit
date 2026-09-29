------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  UDP over IPv4 on Ethernet (RFC 768): whole frames built for sending,
--  and the checksum of a received datagram verified, both over byte arrays
--  in netstack's own memory.
--
--  Proved (tests/net-tcp): no run-time errors for any payload up to one
--  unfragmented datagram, and a built frame is IPv4_Frame.Emittable
--  (unicast source and destination).
--  Tested, not proved: the ports, length and payload are those given, the
--  checksum verifies, and both agree with netstack's word-wise transport
--  checksum.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with IPv4_Header; use IPv4_Header;
with IPv4_Frame;

package UDP_Frame with SPARK_Mode is

   UDP_Protocol    : constant := 17;
   Header_Size     : constant := 8;
   Datagram_At     : constant := IPv4_Frame.Ethernet_Header + Minimum_Size;
   Payload_At      : constant := Datagram_At + Header_Size;
   Maximum_Payload : constant := IPv4_Frame.Maximum_Frame - Payload_At;
   Pseudo_Header   : constant := 12;
   No_Checksum     : constant Unsigned_16 := 0;       --  RFC 768
   Zero_Checksum   : constant Unsigned_16 := 16#FFFF#;
   Default_TTL     : constant := 64;
   Maximum_Datagram : constant := Header_Size + Maximum_Payload;

   function U16 (B : Bytes; I : Natural) return Unsigned_16 is
     (Shift_Left (Unsigned_16 (B (I)), 8) or Unsigned_16 (B (I + 1)))
   with Pre => I >= B'First and then I < B'Last;

   --  The one's-complement checksum of the pseudo-header and Datagram
   --  (zero when Datagram carries a correct checksum).
   function Transport_Sum (Source, Destination : Address; Datagram : Bytes)
     return Unsigned_16
   with Pre => Datagram'Length <= Maximum_Datagram;

   --  A received datagram's checksum is absent (zero) or correct.
   function Checksum_OK (Source, Destination : Address; Datagram : Bytes) return Boolean
   with Pre => Datagram'Length in Header_Size .. Maximum_Datagram;

   procedure Build
     (Our_MAC, To_MAC : IPv4_Frame.MAC; Source, Destination : Address;
      Source_Port, Destination_Port : Unsigned_16; Payload : Bytes; Frame : out Bytes)
   with
     Pre  => Payload'Length <= Maximum_Payload and then Frame'First = 0 and then
             Frame'Length = Payload_At + Payload'Length and then
             IPv4_Frame.Unicast (Source) and then IPv4_Frame.Unicast (Destination),
     Post => IPv4_Frame.Emittable (Frame);

end UDP_Frame;
