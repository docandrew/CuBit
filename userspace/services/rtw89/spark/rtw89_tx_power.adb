------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with RTW89_TX_Power_8852B; use RTW89_TX_Power_8852B;

package body RTW89_TX_Power with SPARK_Mode is

   --  Rate sections (core.h enum rtw89_rate_section) and their sizes
   --  (phy.c rtw89_rs_idx_num_ax, rtw89_rs_nss_num_ax).
   CCK_Section    : constant := 0;
   OFDM_Section   : constant := 1;
   MCS_Section    : constant := 2;
   HEDCM_Section  : constant := 3;
   Offset_Section : constant := 4;
   subtype Section is Natural range CCK_Section .. HEDCM_Section;
   Indices : constant array (Section) of Positive := [4, 8, 12, 4];
   Streams : constant array (Section) of Positive := [1, 1, 4, 2];
   Offset_Indices : constant := 5;   --  RTW89_RATE_OFFSET_NUM_AX

   Band_2G : constant := 0;   --  RTW89_BAND_2G
   Band_5G : constant := 1;   --  RTW89_BAND_5G
   function Band_Index (B : WS.Band) return Natural is
     (case B is when WS.Band_2_4_GHz => Band_2G, when WS.Band_5_GHz => Band_5G);

   Octet : constant := 256;
   Sign_Bit : constant := 128;
   subtype Power is Integer range -128 .. 127;

   --  The byrate table's value for (band, section, nss, index): the last
   --  entry naming it, else 0 (rtw89_phy_load_txpwr_byrate).
   function Byrate_Value (Band, Rate_Section, NSS, Index : Natural) return Power is
      V : Power := 0;
   begin
      for E of RTW89_TX_Power_8852B.Byrate loop
         if E.Band = Band and then E.Section = Rate_Section and then E.NSS = NSS
           and then Index >= E.First and then Index - E.First < Natural'Min (E.Count, 4)
         then
            declare
               Raw : constant Natural :=
                 Natural (Shift_Right (Unsigned_32 (E.Data), 8 * (Index - E.First)) and (Octet - 1));
            begin
               V := (if Raw >= Sign_Bit then Raw - Octet else Raw);
            end;
         end if;
      end loop;
      return V;
   end Byrate_Value;

   --  RF units (1/4 dB) to MAC units (1/2 dB): an arithmetic shift right.
   function To_MAC (V : Power) return Power is
     (if V >= 0 then V / 2 else -((1 - V) / 2));

   function Octet_Of (V : Power) return Unsigned_32 is
     (Unsigned_32 (if V < 0 then V + Octet else V));

   function Byrate (Band : WS.Band) return Byrate_Array is
      Result : Byrate_Array := [others => 0];
      Next : Natural := 0;
      Word : Unsigned_32;
   begin
      for NSS in 0 .. Paths - 1 loop
         for S in Section loop
            if NSS < Streams (S) then
               for Group in 0 .. Indices (S) / 4 - 1 loop
                  Word := 0;
                  for K in 0 .. 3 loop
                     Word := Word or Shift_Left
                       (Octet_Of (To_MAC (Byrate_Value ((if S = CCK_Section then Band_2G else Band_Index (Band)),
                                                        S, NSS, 4 * Group + K))), 8 * K);
                  end loop;
                  if Next < Byrate_Words then
                     Result (Next) := Word;
                     Next := Next + 1;
                  end if;
               end loop;
            end if;
         end loop;
      end loop;
      return Result;
   end Byrate;

   Nibble_Bits : constant := 4;
   Nibble : constant := 16;

   function Offset (Band : WS.Band) return Unsigned_32 is
      Word : Unsigned_32 := 0;
   begin
      for I in 0 .. Offset_Indices - 1 loop
         Word := Word or Shift_Left
           (Unsigned_32 (To_MAC (Byrate_Value (Band_Index (Band), Offset_Section, 0, I)) mod Nibble),
            Nibble_Bits * I);
      end loop;
      return Word;
   end Offset;

   function Channel_Index (Band : WS.Band; Channel : WS.Channel_Number) return Natural is
     (case Band is
        when WS.Band_2_4_GHz => Natural (Channel) - 1,
        when WS.Band_5_GHz =>
          (case Channel is
             when 36 .. 64   => (Natural (Channel) - 36) / 2,
             when 100 .. 144 => (Natural (Channel) - 100) / 2 + 15,
             when 149 .. 177 => (Natural (Channel) - 149) / 2 + 38,
             when others     => 0))
   with Pre => Supported (Band, Channel);

   Width_20 : constant := 0;
   Width_40 : constant := 1;
   --  The MAC's ceiling (rtw89_phy_txpwr_dbm_to_mac clamps to 63).
   MAC_Ceiling : constant := 63;

   function Limit (Band : WS.Band; Width, NTX, Rate_Section, BF : Natural; Index : Natural) return Power is
     (case Band is
        when WS.Band_2_4_GHz => Limit_2G (Row (Width, NTX, Rate_Section, BF), Natural'Min (Index, 13)),
        when WS.Band_5_GHz   => Limit_5G (Row (Width, NTX, Rate_Section, BF), Natural'Min (Index, 52)))
   with Pre => Width <= 1 and then NTX <= 1 and then Rate_Section <= 2 and then BF <= 1;

   function Limits (Band : WS.Band; Channel : WS.Channel_Number) return Limit_Array is
      Result : Limit_Array := [others => 0];
      Index : constant Natural := Channel_Index (Band, Channel);
      type Page is array (0 .. 4 * Limit_Words_Per_Path - 1) of Power;
      --  Page offsets (struct rtw89_txpwr_limit_ax).
      CCK_20M : constant := 0;
      CCK_40M : constant := 2;
      OFDM    : constant := 4;
      MCS_20M : constant := 6;
   begin
      for NTX in 0 .. Paths - 1 loop
         declare
            P : Page := [others => 0];
            Word : Unsigned_32;
         begin
            for BF in 0 .. 1 loop
               P (CCK_20M + BF) := Power'Min (To_MAC (Limit (Band, Width_20, NTX, CCK_Section, BF, Index)), MAC_Ceiling);
               P (CCK_40M + BF) := Power'Min (To_MAC (Limit (Band, Width_40, NTX, CCK_Section, BF, Index)), MAC_Ceiling);
               P (OFDM + BF) := Power'Min (To_MAC (Limit (Band, Width_20, NTX, OFDM_Section, BF, Index)), MAC_Ceiling);
               P (MCS_20M + BF) := Power'Min (To_MAC (Limit (Band, Width_20, NTX, MCS_Section, BF, Index)), MAC_Ceiling);
            end loop;
            for W in 0 .. Limit_Words_Per_Path - 1 loop
               Word := 0;
               for K in 0 .. 3 loop
                  Word := Word or Shift_Left (Octet_Of (P (4 * W + K)), 8 * K);
               end loop;
               Result (NTX * Limit_Words_Per_Path + W) := Word;
            end loop;
         end;
      end loop;
      return Result;
   end Limits;

   --  rtw8852bx_bb_cal_txpwr_ref with reference 0.
   TSSI_16dBm_Code : constant := 16#12C#;
   Base_Code_0dB   : constant := 16#27#;
   Eighths         : constant := 8;
   Reference_16dBm : constant := 16 * Eighths;
   Field_9_Bits    : constant := 16#1FF#;
   Low_3_Bits      : constant := 8;
   RF_Code_Low     : constant := 15;
   RF_Code_High    : constant := 63;
   TSSI_Shift      : constant := 18;
   Power_Shift     : constant := 9;

   function Reference (Decrease : Decrease_Eighths) return Unsigned_32 is
      Code : constant Integer := Base_Code_0dB * Eighths - Decrease;    --  s10.3
      BB_Code : constant Natural := Code mod Low_3_Bits;
      RF_Code : constant Natural := Natural'Max (RF_Code_Low, Natural'Min (RF_Code_High, (Code / Low_3_Bits) mod 64));
      Power_Code : constant Natural := RF_Code * Low_3_Bits + BB_Code;
      TSSI_Code : constant Natural := (TSSI_16dBm_Code - Reference_16dBm - Decrease) mod (Field_9_Bits + 1);
   begin
      return Shift_Left (Unsigned_32 (TSSI_Code), TSSI_Shift)
        or Shift_Left (Unsigned_32 (Power_Code mod (Field_9_Bits + 1)), Power_Shift);
   end Reference;

end RTW89_TX_Power;
