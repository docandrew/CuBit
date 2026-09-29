------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What netstack may put on an Ethernet link as IPv4 from its own
--  protocols (IPv4_ICMP's Send precondition, so proved for every frame it
--  sends): type 0800, a 20-byte version-4 header, and a unicast source and
--  destination: not 0.0.0.0, loopback, multicast or the limited broadcast.
--  So nothing it originates can be aimed at a whole network (no smurf
--  amplification), nor claim a source it cannot be.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with IPv4_Header; use IPv4_Header;

package IPv4_Frame with SPARK_Mode is

   Ethernet_Header : constant := 14;
   Maximum_Frame   : constant := 1_514;
   Type_High       : constant := 16#08#;
   Type_Low        : constant := 16#00#;
   Header_At       : constant := Ethernet_Header;
   Source_At       : constant := Ethernet_Header + 12;
   Destination_At  : constant := Ethernet_Header + 16;
   Loopback_Net    : constant := 127;
   First_Multicast : constant := 224;

   type MAC is array (0 .. 5) of Unsigned_8;

   function Unicast (A : Address) return Boolean is
     (A (0) /= 0 and then A (0) /= Loopback_Net and then A (0) < First_Multicast);

   function Address_At (Frame : Bytes; I : Natural) return Address is
     [Frame (I), Frame (I + 1), Frame (I + 2), Frame (I + 3)]
   with Pre => I >= Frame'First and then Frame'Last >= 3 and then I <= Frame'Last - 3;

   function Emittable (Frame : Bytes) return Boolean is
     (Frame'First = 0 and then
      Frame'Length in Ethernet_Header + Minimum_Size .. Maximum_Frame and then
      Frame (12) = Type_High and then Frame (13) = Type_Low and then
      Frame (Header_At) = Version_IHL and then
      Unicast (Address_At (Frame, Source_At)) and then
      Unicast (Address_At (Frame, Destination_At)));

end IPv4_Frame;
