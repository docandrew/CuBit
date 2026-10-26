------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Wifi_Scan with SPARK_Mode is

   function Byte_Of (W : Unsigned_32; K : Natural) return Unsigned_8 is
     (Unsigned_8 (Shift_Right (W, NL.Octet_Bits * K) and Byte_Mask))
   with Pre => K < Octets_Per_Word;

   function Pack (B0, B1, B2, B3 : Unsigned_8) return Unsigned_32 is
     (Unsigned_32 (B0) or Shift_Left (Unsigned_32 (B1), 8) or
      Shift_Left (Unsigned_32 (B2), 16) or Shift_Left (Unsigned_32 (B3), 24));

   function Signal_Octet (S : Signal_dBm) return Unsigned_32 is
     (Unsigned_32 (Integer (S) + Signal_Modulus))
   with Pre => S < 0, Post => Signal_Octet'Result <= Byte_Mask;

   ---------------------------------------------------------------------------
   --  Entries
   ---------------------------------------------------------------------------
   function Encode (E : BSS_Entry) return Entry_Words_Array is
      W : Entry_Words_Array := [others => 0];
      B : NL.Hardware_Address renames E.BSSID;
   begin
      W (BSSID_Low_Word) := Pack (B (0), B (1), B (2), B (3));
      W (BSSID_High_Word) := Pack (B (4), B (5), 0, 0)
        or Shift_Left (Unsigned_32 (E.Channel), Channel_Shift)
        or Shift_Left (Unsigned_32 (Band'Enum_Rep (E.On_Band)), Band_Shift);
      W (Signal_Word) := (if E.Signal = 0 then 0 else Signal_Octet (E.Signal))
        or Shift_Left (Unsigned_32 (E.Length), Length_Shift)
        or Shift_Left ((if E.RSN then RSN_Flag else 0), Flags_Shift);
      W (Capability_Word) := Unsigned_32 (E.Capability)
        or Shift_Left (Unsigned_32 (E.Interval), Interval_Shift);
      for K in 0 .. NL.SSID_Octets / Octets_Per_Word - 1 loop
         W (SSID_First_Word + K) :=
           Pack (E.SSID (K * Octets_Per_Word + 1), E.SSID (K * Octets_Per_Word + 2),
                 E.SSID (K * Octets_Per_Word + 3), E.SSID (K * Octets_Per_Word + 4));
      end loop;
      W (Heard_Word) := Unsigned_32 (E.Heard);
      return W;
   end Encode;

   function Decode (W : Entry_Words_Array) return Decoded_Entry is
      Channel_Value : constant Unsigned_32 :=
        Shift_Right (W (BSSID_High_Word), Channel_Shift) and Byte_Mask;
      Band_Value    : constant Unsigned_32 := Shift_Right (W (BSSID_High_Word), Band_Shift);
      Signal_Value  : constant Unsigned_32 := W (Signal_Word) and Byte_Mask;
      Length_Value  : constant Unsigned_32 :=
        Shift_Right (W (Signal_Word), Length_Shift) and Byte_Mask;
      Flags_Value   : constant Unsigned_32 := Shift_Right (W (Signal_Word), Flags_Shift);
      E : BSS_Entry;
   begin
      if Band_Value = Unsigned_32 (Band'Enum_Rep (Band_2_4_GHz)) then
         E.On_Band := Band_2_4_GHz;
      elsif Band_Value = Unsigned_32 (Band'Enum_Rep (Band_5_GHz)) then
         E.On_Band := Band_5_GHz;
      else
         return (Valid => False);
      end if;
      if Channel_Value not in Unsigned_32 (Channel_Number'First) .. Unsigned_32 (Channel_Number'Last)
        or else Length_Value > NL.SSID_Octets
        or else (Flags_Value and not RSN_Flag) /= 0
        or else W (Heard_Word) > Unsigned_32 (Hearing_Count'Last)
      then
         return (Valid => False);
      end if;
      E.Channel := Channel_Number (Channel_Value);
      if not Valid_Channel (E.On_Band, E.Channel) then
         return (Valid => False);
      end if;
      --  0 is 0 dBm; 128 .. 255 are -128 .. -1; 1 .. 127 would be positive.
      if Signal_Value = 0 then
         E.Signal := 0;
      elsif Signal_Value >= Signal_Modulus / 2 then
         E.Signal := Signal_dBm (Integer (Signal_Value) - Signal_Modulus);
      else
         return (Valid => False);
      end if;
      E.Length := NL.SSID_Length (Length_Value);
      E.RSN := (Flags_Value and RSN_Flag) /= 0;
      E.BSSID := [Byte_Of (W (BSSID_Low_Word), 0), Byte_Of (W (BSSID_Low_Word), 1),
                  Byte_Of (W (BSSID_Low_Word), 2), Byte_Of (W (BSSID_Low_Word), 3),
                  Byte_Of (W (BSSID_High_Word), 0), Byte_Of (W (BSSID_High_Word), 1)];
      E.Capability := Capability_Information (W (Capability_Word) and Half_Mask);
      E.Interval := Beacon_Interval_TU (Shift_Right (W (Capability_Word), Interval_Shift));
      for K in 0 .. NL.SSID_Octets / Octets_Per_Word - 1 loop
         for J in 0 .. Octets_Per_Word - 1 loop
            E.SSID (K * Octets_Per_Word + J + 1) := Byte_Of (W (SSID_First_Word + K), J);
         end loop;
      end loop;
      E.Heard := Hearing_Count (W (Heard_Word));
      if not Normal (E) then
         return (Valid => False);
      end if;
      --  Unused bits must be zero: re-encoding gives W back.
      if Encode (E) /= W then
         return (Valid => False);
      end if;
      return (Valid => True, Item => E);
   end Decode;

   ---------------------------------------------------------------------------
   --  Summary
   ---------------------------------------------------------------------------
   function Encode (S : Scan_Summary) return Summary_Words_Array is
     [0 => Unsigned_32 (S.Count),
      1 => S.Passes,
      2 => Unsigned_32 (S.Total) or Shift_Left (Unsigned_32 (S.On_2_4), On_2_4_Shift)
           or Shift_Left (Unsigned_32 (S.On_5), On_5_Shift)
           or Shift_Left (Unsigned_32 (S.Channels), Channels_Shift)];

   function Decode (W : Summary_Words_Array) return Decoded_Summary is
      Total_Value    : constant Unsigned_32 := W (2) and Byte_Mask;
      On_2_4_Value   : constant Unsigned_32 := Shift_Right (W (2), On_2_4_Shift) and Byte_Mask;
      On_5_Value     : constant Unsigned_32 := Shift_Right (W (2), On_5_Shift) and Byte_Mask;
      Channels_Value : constant Unsigned_32 := Shift_Right (W (2), Channels_Shift);
      S : Scan_Summary;
   begin
      if W (0) > Maximum_Entries then
         return (Valid => False);
      end if;
      S := (Count    => Entry_Count (W (0)),
            Passes   => W (1),
            Total    => Network_Count (Total_Value),
            On_2_4   => Network_Count (On_2_4_Value),
            On_5     => Network_Count (On_5_Value),
            Channels => Channel_Count (Channels_Value));
      if not Normal (S) or else Encode (S) /= W then
         return (Valid => False);
      end if;
      return (Valid => True, Item => S);
   end Decode;

   ---------------------------------------------------------------------------
   --  Blocks
   ---------------------------------------------------------------------------
   function Entry_Slice (B : Block; I : Entry_Index) return Entry_Words_Array is
     [for K in Entry_Word_Index => B (Entry_Word (I, K))];

   function Encode (R : Scan_Result; Generation : NL.Generation) return Block is
      B : Block := [others => 0];
      S : constant Summary_Words_Array := Encode (R.Summary);
   begin
      B (Generation_Word) := Generation;
      for K in S'Range loop
         B (Summary_First_Word + K) := S (K);
      end loop;
      for I in 1 .. R.Summary.Count loop
         pragma Loop_Invariant (B (Generation_Word) = Generation);
         declare
            E : constant Entry_Words_Array := Encode (R.Entries (I));
         begin
            for K in Entry_Word_Index loop
               pragma Loop_Invariant (B (Generation_Word) = Generation);
               B (Entry_Word (I, K)) := E (K);
            end loop;
         end;
      end loop;
      return B;
   end Encode;

   function Decode (B : Block) return Decoded_Result is
      S : constant Decoded_Summary :=
        Decode (Summary_Words_Array'[for K in 0 .. Summary_Words - 1 => B (Summary_First_Word + K)]);
      R : Scan_Result;
   begin
      if not S.Valid then
         return (Valid => False);
      end if;
      R.Summary := S.Item;
      for I in 1 .. R.Summary.Count loop
         pragma Loop_Invariant (Normal (R.Summary) and then R.Summary = S.Item);
         pragma Loop_Invariant
           (for all J in Entry_Index =>
              (if J < I then Normal (R.Entries (J)) else R.Entries (J) = No_Entry));
         declare
            E : constant Decoded_Entry := Decode (Entry_Slice (B, I));
         begin
            if not E.Valid then
               return (Valid => False);
            end if;
            R.Entries (I) := E.Item;
         end;
      end loop;
      if not Normal (R) then
         return (Valid => False);
      end if;
      --  Words past Count must be zero: re-encoding gives B back.
      if Encode (R, B (Generation_Word)) /= B then
         return (Valid => False);
      end if;
      return (Valid => True, Item => R);
   end Decode;

end CuBit.Wifi_Scan;
