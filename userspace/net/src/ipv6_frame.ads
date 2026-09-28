------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What netstack may put on an Ethernet link as IPv6 (IPv6_Link's Send
--  precondition, so proved for every frame it sends): type 86DD, a whole
--  IPv6 header within one standard frame, a source that is neither
--  IPv4-mapped nor multicast, and a destination that is neither mapped nor
--  unspecified.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with IPv6_Header; use IPv6_Header;

package IPv6_Frame with SPARK_Mode is

   Ethernet_Header : constant := 14;
   Maximum_Frame   : constant := 1_514;
   Type_High       : constant := 16#86#;
   Type_Low        : constant := 16#DD#;
   Source_At       : constant := Ethernet_Header + 8;
   Destination_At  : constant := Ethernet_Header + 24;

   function Emittable (Frame : Bytes) return Boolean is
     (Frame'First = 0 and then
      Frame'Length in Ethernet_Header + Size .. Maximum_Frame and then
      Frame (12) = Type_High and then Frame (13) = Type_Low and then
      Acceptable_Source (Address_At (Frame, Source_At)) and then
      Acceptable_Destination (Address_At (Frame, Destination_At)) and then
      --  Stated on its own: no IPv4-mapped address goes out on IPv6.
      not Is_Mapped (Address_At (Frame, Source_At)) and then
      not Is_Mapped (Address_At (Frame, Destination_At)));

end IPv6_Frame;
