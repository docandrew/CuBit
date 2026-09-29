------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Internet_Checksum;

package body TCP_Frame with SPARK_Mode is

   procedure Finish
     (Frame : in out IPv4_Header.Bytes; Our_MAC, To_MAC : IPv4_Frame.MAC;
      Source, Destination : Address)
   is
      Length : constant Natural := Frame'Length - Segment_At;
      Pseudo : constant Internet_Checksum.Bytes (0 .. 11) :=
        [Source (0), Source (1), Source (2), Source (3),
         Destination (0), Destination (1), Destination (2), Destination (3),
         0, TCP_Protocol, Unsigned_8 (Length / 256), Unsigned_8 (Length mod 256)];
      Hdr : Header_Bytes;
      Sum : Unsigned_16;
   begin
      --  The TCP checksum, over the pseudo-header and the segment in place.
      Frame (Checksum_At) := 0;
      Frame (Checksum_At + 1) := 0;
      Sum := Internet_Checksum.Fold (Internet_Checksum.Add_Bytes (Internet_Checksum.Add_Bytes (0, Pseudo),
                              Internet_Checksum.Bytes (Frame (Segment_At .. Frame'Last))));
      Frame (Checksum_At) := Unsigned_8 (Shift_Right (Sum, 8));
      Frame (Checksum_At + 1) := Unsigned_8 (Sum and 16#FF#);
      --  The IPv4 header, then Ethernet.
      Build_Header ((Size => Minimum_Size, Total_Length => Minimum_Size + Length,
                     Protocol => TCP_Protocol, TTL => Default_TTL,
                     Source => Source, Destination => Destination),
                    DF => True, Hdr => Hdr);
      for K in Hdr'Range loop
         Frame (IPv4_Frame.Ethernet_Header + K) := Hdr (K);
         pragma Loop_Invariant
           (for all J in 0 .. K => Frame (IPv4_Frame.Ethernet_Header + J) = Hdr (J));
      end loop;
      for K in 0 .. 5 loop
         Frame (K) := To_MAC (K);
         Frame (6 + K) := Our_MAC (K);
         pragma Loop_Invariant
           (for all J in 0 .. Minimum_Size - 1 =>
              Frame (IPv4_Frame.Ethernet_Header + J) = Hdr (J));
      end loop;
      Frame (12) := IPv4_Frame.Type_High;
      Frame (13) := IPv4_Frame.Type_Low;
      pragma Assert (Frame (IPv4_Frame.Header_At) = Hdr (0));
      pragma Assert (Frame (IPv4_Frame.Source_At) = Hdr (12));
      pragma Assert (Frame (IPv4_Frame.Destination_At) = Hdr (16));
      pragma Assert (IPv4_Frame.Address_At (Frame, IPv4_Frame.Source_At) (0) = Source (0));
      pragma Assert
        (IPv4_Frame.Address_At (Frame, IPv4_Frame.Destination_At) (0) = Destination (0));
   end Finish;

   function Checksum_OK (Source, Destination : Address; Segment : Bytes)
     return Boolean
   is
      Length : constant Natural := Segment'Length;
      Pseudo : constant Internet_Checksum.Bytes (0 .. Pseudo_Header - 1) :=
        [Source (0), Source (1), Source (2), Source (3),
         Destination (0), Destination (1), Destination (2), Destination (3),
         0, TCP_Protocol, Unsigned_8 (Length / 256), Unsigned_8 (Length mod 256)];
   begin
      return Internet_Checksum.Fold
        (Internet_Checksum.Add_Bytes
           (Internet_Checksum.Add_Bytes (0, Pseudo), Internet_Checksum.Bytes (Segment))) = 0;
   end Checksum_OK;

end TCP_Frame;
