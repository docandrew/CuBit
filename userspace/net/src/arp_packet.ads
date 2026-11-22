------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  ARP for IPv4 over Ethernet (RFC 826): the packet after the Ethernet
--  header, parsed in place.
--
--  Accepted: hardware type Ethernet (1), protocol type IPv4 (0x0800),
--  address lengths 6 and 4, operation request (1) or reply (2), and at
--  least the 28 bytes these need (RFC 826).
--
--  Proved (tests/net-tcp): Well_Formed is exactly that rule; every parsed
--  field is its bytes on the wire.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package ARP_Packet with SPARK_Mode is

   type Bytes is array (Natural range <>) of Unsigned_8;
   type IPv4 is array (0 .. 3) of Unsigned_8;
   type MAC is array (0 .. 5) of Unsigned_8;

   Size : constant := 28;
   Hardware_Ethernet : constant := 1;
   Protocol_IPv4     : constant := 16#0800#;
   Request_Operation : constant := 1;
   Reply_Operation   : constant := 2;

   type Operation is (Request, Reply);

   type Packet is record
      Op         : Operation := Request;
      Sender_HW  : MAC := [others => 0];
      Sender_IP  : IPv4 := [others => 0];
      Target_HW  : MAC := [others => 0];
      Target_IP  : IPv4 := [others => 0];
   end record;

   function U16 (B : Bytes; I : Natural) return Unsigned_16 is
     (Shift_Left (Unsigned_16 (B (I)), 8) or Unsigned_16 (B (I + 1)))
   with Pre => I >= B'First and then I < B'Last;

   function Well_Formed (B : Bytes) return Boolean is
     (B'First = 0 and then B'Length >= Size and then
      U16 (B, 0) = Hardware_Ethernet and then U16 (B, 2) = Protocol_IPv4 and then
      B (4) = 6 and then B (5) = 4 and then
      U16 (B, 6) in Request_Operation | Reply_Operation);

   procedure Parse (B : Bytes; P : out Packet) with
     Pre  => Well_Formed (B),
     Post => P.Op = (if U16 (B, 6) = Request_Operation then Request else Reply) and then
             P.Sender_HW = [B (8), B (9), B (10), B (11), B (12), B (13)] and then
             P.Sender_IP = [B (14), B (15), B (16), B (17)] and then
             P.Target_HW = [B (18), B (19), B (20), B (21), B (22), B (23)] and then
             P.Target_IP = [B (24), B (25), B (26), B (27)];

   Ethernet_Header : constant := 14;
   Frame_Size      : constant := Ethernet_Header + Size;
   Ethertype_High  : constant := 16#08#;
   Ethertype_Low   : constant := 16#06#;
   subtype Frame_Bytes is Bytes (0 .. Frame_Size - 1);

   --  A whole Ethernet frame carrying P, to Eth_Destination, from
   --  P.Sender_HW (a sender always sends from its own address).
   procedure Build (P : Packet; Eth_Destination : MAC; Frame : out Frame_Bytes) with
     Post => Frame (12) = Ethertype_High and then Frame (13) = Ethertype_Low and then
             (for all K in MAC'Range =>
                Frame (K) = Eth_Destination (K) and then
                Frame (6 + K) = P.Sender_HW (K) and then
                Frame (Ethernet_Header + 8 + K) = P.Sender_HW (K) and then
                Frame (Ethernet_Header + 18 + K) = P.Target_HW (K)) and then
             (for all K in IPv4'Range =>
                Frame (Ethernet_Header + 14 + K) = P.Sender_IP (K) and then
                Frame (Ethernet_Header + 24 + K) = P.Target_IP (K)) and then
             U16 (Frame, Ethernet_Header) = Hardware_Ethernet and then
             U16 (Frame, Ethernet_Header + 2) = Protocol_IPv4 and then
             Frame (Ethernet_Header + 4) = 6 and then Frame (Ethernet_Header + 5) = 4 and then
             U16 (Frame, Ethernet_Header + 6) =
               (if P.Op = Request then Request_Operation else Reply_Operation);

end ARP_Packet;
