------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body ARP_Packet with SPARK_Mode is

   procedure Parse (B : Bytes; P : out Packet) is
   begin
      P := (Op        => (if U16 (B, 6) = Request_Operation then Request else Reply),
            Sender_HW => [B (8), B (9), B (10), B (11), B (12), B (13)],
            Sender_IP => [B (14), B (15), B (16), B (17)],
            Target_HW => [B (18), B (19), B (20), B (21), B (22), B (23)],
            Target_IP => [B (24), B (25), B (26), B (27)]);
   end Parse;

   procedure Build (P : Packet; Eth_Destination : MAC; Frame : out Frame_Bytes) is
      Op : constant Unsigned_16 :=
        (if P.Op = Request then Request_Operation else Reply_Operation);
   begin
      Frame := [others => 0];
      for K in MAC'Range loop
         Frame (K) := Eth_Destination (K);
         Frame (6 + K) := P.Sender_HW (K);
         Frame (Ethernet_Header + 8 + K) := P.Sender_HW (K);
         Frame (Ethernet_Header + 18 + K) := P.Target_HW (K);
         pragma Loop_Invariant
           (for all J in MAC'First .. K =>
              Frame (J) = Eth_Destination (J) and then
              Frame (6 + J) = P.Sender_HW (J) and then
              Frame (Ethernet_Header + 8 + J) = P.Sender_HW (J) and then
              Frame (Ethernet_Header + 18 + J) = P.Target_HW (J));
      end loop;
      for K in IPv4'Range loop
         Frame (Ethernet_Header + 14 + K) := P.Sender_IP (K);
         Frame (Ethernet_Header + 24 + K) := P.Target_IP (K);
         pragma Loop_Invariant
           (for all J in IPv4'First .. K =>
              Frame (Ethernet_Header + 14 + J) = P.Sender_IP (J) and then
              Frame (Ethernet_Header + 24 + J) = P.Target_IP (J));
         pragma Loop_Invariant
           (for all J in MAC'Range =>
              Frame (J) = Eth_Destination (J) and then
              Frame (6 + J) = P.Sender_HW (J) and then
              Frame (Ethernet_Header + 8 + J) = P.Sender_HW (J) and then
              Frame (Ethernet_Header + 18 + J) = P.Target_HW (J));
      end loop;
      Frame (12) := Ethertype_High;
      Frame (13) := Ethertype_Low;
      Frame (Ethernet_Header) := 0;
      Frame (Ethernet_Header + 1) := Hardware_Ethernet;
      Frame (Ethernet_Header + 2) := Unsigned_8 (Protocol_IPv4 / 256);
      Frame (Ethernet_Header + 3) := Unsigned_8 (Protocol_IPv4 mod 256);
      Frame (Ethernet_Header + 4) := 6;
      Frame (Ethernet_Header + 5) := 4;
      Frame (Ethernet_Header + 6) := 0;
      Frame (Ethernet_Header + 7) := Unsigned_8 (Op);
   end Build;

end ARP_Packet;
