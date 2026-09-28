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

end ARP_Packet;
