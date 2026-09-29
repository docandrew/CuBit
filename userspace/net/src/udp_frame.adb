------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with Internet_Checksum;

package body UDP_Frame with SPARK_Mode is

   procedure Put16 (B : in out Bytes; I : Natural; V : Unsigned_16) with
     Pre  => I >= B'First and then I < B'Last,
     Post => U16 (B, I) = V and then
             (for all J in B'Range => (if J /= I and then J /= I + 1 then B (J) = B'Old (J)))
   is
   begin
      B (I) := Unsigned_8 (Shift_Right (V, 8));
      B (I + 1) := Unsigned_8 (V and 16#FF#);
   end Put16;

   function Transport_Sum (Source, Destination : Address; Datagram : Bytes)
     return Unsigned_16
   is
      Scratch : Bytes (0 .. Pseudo_Header + Datagram'Length - 1) := [others => 0];
   begin
      Scratch (0 .. 3) := Bytes (Source);
      Scratch (4 .. 7) := Bytes (Destination);
      Scratch (9) := UDP_Protocol;
      Scratch (10) := Unsigned_8 (Datagram'Length / 256);
      Scratch (11) := Unsigned_8 (Datagram'Length mod 256);
      Scratch (Pseudo_Header .. Scratch'Last) := Datagram;
      return Internet_Checksum.Of_Bytes (Internet_Checksum.Bytes (Scratch));
   end Transport_Sum;

   function Checksum_OK (Source, Destination : Address; Datagram : Bytes) return Boolean is
     (U16 (Datagram, Datagram'First + 6) = No_Checksum or else
      Transport_Sum (Source, Destination, Datagram) = 0);

   procedure Build
     (Our_MAC, To_MAC : IPv4_Frame.MAC; Source, Destination : Address;
      Source_Port, Destination_Port : Unsigned_16; Payload : Bytes; Frame : out Bytes)
   is
      Length : constant Natural := Header_Size + Payload'Length;
      Packet : Bytes (0 .. Minimum_Size + Length - 1) := [others => 0];
      Sum    : Unsigned_16;
   begin
      Frame := [others => 0];
      --  The datagram, then its checksum over the pseudo-header.
      Put16 (Packet, Minimum_Size, Source_Port);
      Put16 (Packet, Minimum_Size + 2, Destination_Port);
      Put16 (Packet, Minimum_Size + 4, Unsigned_16 (Length));
      Packet (Minimum_Size + Header_Size .. Packet'Last) := Payload;
      Sum := Transport_Sum (Source, Destination, Packet (Minimum_Size .. Packet'Last));
      Put16 (Packet, Minimum_Size + 6, (if Sum = No_Checksum then Zero_Checksum else Sum));
      Build ((Size => Minimum_Size, Total_Length => Minimum_Size + Length,
              Protocol => UDP_Protocol, TTL => Default_TTL,
              Source => Source, Destination => Destination),
             DF => True, B => Packet);
      for K in 0 .. 5 loop
         Frame (K) := To_MAC (K);
         Frame (6 + K) := Our_MAC (K);
      end loop;
      Frame (12) := IPv4_Frame.Type_High;
      Frame (13) := IPv4_Frame.Type_Low;
      for K in Packet'Range loop
         Frame (IPv4_Frame.Ethernet_Header + K) := Packet (K);
         pragma Loop_Invariant
           (for all J in 0 .. K => Frame (IPv4_Frame.Ethernet_Header + J) = Packet (J));
         pragma Loop_Invariant
           (Frame (12) = IPv4_Frame.Type_High and then Frame (13) = IPv4_Frame.Type_Low);
      end loop;
      pragma Assert (Frame (IPv4_Frame.Header_At) = Packet (0));
      pragma Assert (Frame (IPv4_Frame.Source_At) = Packet (12));
      pragma Assert (Frame (IPv4_Frame.Destination_At) = Packet (16));
      pragma Assert (IPv4_Frame.Address_At (Frame, IPv4_Frame.Source_At) (0) = Source (0));
      pragma Assert
        (IPv4_Frame.Address_At (Frame, IPv4_Frame.Destination_At) (0) = Destination (0));
   end Build;

end UDP_Frame;
