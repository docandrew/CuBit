------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with Internet_Checksum;

package body IPv4_Header with SPARK_Mode is

   procedure Parse (B : Bytes; H : out Header) is
   begin
      H := (Size         => Stated_Size (B),
            Total_Length => Natural (U16 (B, 2)),
            Protocol     => B (9),
            TTL          => B (8),
            Source       => [B (12), B (13), B (14), B (15)],
            Destination  => [B (16), B (17), B (18), B (19)]);
   end Parse;

   procedure Put16 (B : in out Bytes; I : Natural; V : Unsigned_16) with
     Pre  => I >= B'First and then I < B'Last,
     Post => U16 (B, I) = V and then
             (for all J in B'Range => (if J /= I and then J /= I + 1 then B (J) = B'Old (J)))
   is
   begin
      B (I) := Unsigned_8 (Shift_Right (V, 8));
      B (I + 1) := Unsigned_8 (V and 16#FF#);
   end Put16;

   procedure Build (H : Header; DF : Boolean; B : in out Bytes) is
      Flags : constant Unsigned_16 := (if DF then Dont_Fragment else 0);
      Total : constant Unsigned_16 := Unsigned_16 (H.Total_Length);
      Sum   : Unsigned_16;
   begin
      B (0) := Version_IHL;
      B (1) := 0;
      Put16 (B, 2, Total);
      pragma Assert (Natural (Total) = H.Total_Length);
      B (4) := 0;
      B (5) := 0;
      Put16 (B, 6, Flags);
      B (8) := H.TTL;
      B (9) := H.Protocol;
      B (10) := 0;
      B (11) := 0;
      for K in Address'Range loop
         B (12 + K) := H.Source (K);
         B (16 + K) := H.Destination (K);
         pragma Loop_Invariant
           (U16 (B, 2) = Total and then
            B (0) = Version_IHL and then B (8) = H.TTL and then B (9) = H.Protocol and then
            U16 (B, 6) = Flags and then
            B (Minimum_Size .. B'Last) = B'Loop_Entry (Minimum_Size .. B'Last) and then
            (for all J in 0 .. K => B (12 + J) = H.Source (J) and then
                                    B (16 + J) = H.Destination (J)));
      end loop;
      pragma Assert (U16 (B, 2) = Total);
      Sum := Internet_Checksum.Of_Bytes (Internet_Checksum.Bytes (B (0 .. Minimum_Size - 1)));
      declare
         High : constant Unsigned_8 := B (2);
         Low  : constant Unsigned_8 := B (3);
      begin
         pragma Assert ((Shift_Left (Unsigned_16 (High), 8) or Unsigned_16 (Low)) = Total);
         Put16 (B, Checksum_At, Sum);
         pragma Assert (B (2) = High and then B (3) = Low);
         pragma Assert (U16 (B, 2) = Total);
      end;
      pragma Assert (Natural (U16 (B, 2)) = H.Total_Length);
      pragma Assert ((U16 (B, 6) and 16#BFFF#) = 0);
   end Build;

end IPv4_Header;
