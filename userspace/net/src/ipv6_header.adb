------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body IPv6_Header with SPARK_Mode is

   procedure Parse (B : Bytes; H : out Header) is
   begin
      H :=
        (Traffic_Class  => Shift_Left (B (0), 4) or Shift_Right (B (1), 4),
         Flow_Label     => Shift_Left (Unsigned_32 (B (1) and 16#0F#), 16) or
                           Shift_Left (Unsigned_32 (B (2)), 8) or Unsigned_32 (B (3)),
         Payload_Length => Natural (U16 (B, 4)),
         Next_Header    => B (6),
         Hop_Limit      => B (7),
         Source         => Address_At (B, 8),
         Destination    => Address_At (B, 24));
   end Parse;

   procedure Build (H : Header; B : in out Bytes) is
   begin
      B (0) := 16#60# or Shift_Right (H.Traffic_Class, 4);
      B (1) := Shift_Left (H.Traffic_Class, 4) or
               Unsigned_8 (Shift_Right (H.Flow_Label, 16) and 16#0F#);
      B (2) := Unsigned_8 (Shift_Right (H.Flow_Label, 8) and 16#FF#);
      B (3) := Unsigned_8 (H.Flow_Label and 16#FF#);
      B (4) := Unsigned_8 (Shift_Right (Unsigned_16 (H.Payload_Length), 8));
      B (5) := Unsigned_8 (Unsigned_16 (H.Payload_Length) and 16#FF#);
      pragma Assert (U16 (B, 4) = Unsigned_16 (H.Payload_Length));
      B (6) := H.Next_Header;
      B (7) := H.Hop_Limit;
      for K in Address'Range loop
         B (8 + K) := H.Source (K);
         B (24 + K) := H.Destination (K);
         pragma Loop_Invariant
           (for all J in 0 .. K => B (8 + J) = H.Source (J) and then
                                  B (24 + J) = H.Destination (J));
         pragma Loop_Invariant (for all J in 0 .. 7 => B (J) = B'Loop_Entry (J));
         pragma Loop_Invariant (B (Size .. B'Last) = B'Loop_Entry (Size .. B'Last));
      end loop;
      pragma Assert (U16 (B, 4) = Unsigned_16 (H.Payload_Length));
      pragma Assert (Size + Natural (U16 (B, 4)) <= B'Length);
      pragma Assert (for all K in Address'Range => Address_At (B, 8) (K) = H.Source (K));
      pragma Assert (Address_At (B, 8) = H.Source);
      pragma Assert (for all K in Address'Range =>
                       Address_At (B, 24) (K) = H.Destination (K));
      pragma Assert (Address_At (B, 24) = H.Destination);
   end Build;

end IPv6_Header;
