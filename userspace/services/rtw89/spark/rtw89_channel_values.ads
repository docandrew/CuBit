------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The per-channel values the RTL8852B baseband and RF need for a 20 MHz
--  channel (NET-WIFI-001 W1): RF register 0x18's channel/band/bandwidth
--  fields, the BB channel index, SCO compensation, CCK correlator
--  thresholds, gain band, and the per-band gain-error and RPL values.
--
--  @description
--  From rtw89 rtw8852b_rfk.c (_ch_setting, _bw_setting), rtw8852b_common.c
--  (rtw8852bx_ctrl_sco_cck, rtw8852bx_ctrl_ch, set_gain_error,
--  set_rxsc_rpl_comp), phy.c (rtw89_encode_chan_idx) and the decoded BB
--  gain table of rtw8852b_table.c, GPL-2.0 OR BSD-3-Clause, taken under
--  BSD-3-Clause. Pure functions over tight channel types; proved free of
--  run-time errors.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Wifi_Scan;

package RTW89_Channel_Values with Pure, SPARK_Mode is
   package WS renames CuBit.Wifi_Scan;
   use type WS.Band;

   --  The channels this radio supports at 20 MHz (rtw89's 8852B list).
   function Supported (B : WS.Band; C : WS.Channel_Number) return Boolean is
     (case B is
        when WS.Band_2_4_GHz => C in 1 .. 14,
        when WS.Band_5_GHz   =>
          (C in 36 .. 64 and then C mod 4 = 0)
          or else (C in 100 .. 144 and then C mod 4 = 0)
          or else (C in 149 .. 177 and then C mod 4 = 1));

   ---------------------------------------------------------------------------
   --  RF 0x18 (RR_CFGCH): CH [7:0], BAND0 [9:8], BW [11:10], BW2 12,
   --  BCN 13, TRX_AH 14, POW_LCK 15, BAND1 [17:16]; [19:18] preserved.
   ---------------------------------------------------------------------------
   subtype RF_Value is Unsigned_32 range 0 .. 16#F_FFFF#;
   CFGCH_Channel  : constant Unsigned_32 := 16#0_00FF#;
   CFGCH_Band0    : constant Unsigned_32 := 16#0_0300#;
   CFGCH_Band0_5G : constant Unsigned_32 := 16#0_0100#;
   CFGCH_BW       : constant Unsigned_32 := 16#0_0C00#;
   CFGCH_BW_20M   : constant Unsigned_32 := 16#0_0C00#;   --  3 << 10
   CFGCH_BW2      : constant Unsigned_32 := 16#0_1000#;
   CFGCH_BCN      : constant Unsigned_32 := 16#0_2000#;
   CFGCH_TRX_AH   : constant Unsigned_32 := 16#0_4000#;
   CFGCH_POW_LCK  : constant Unsigned_32 := 16#0_8000#;
   CFGCH_Band1    : constant Unsigned_32 := 16#3_0000#;
   CFGCH_Band1_5G : constant Unsigned_32 := 16#1_0000#;

   --  _ch_setting: the new channel and band in Old.
   function With_Channel (Old : RF_Value; B : WS.Band; C : WS.Channel_Number) return RF_Value is
     (((Old and not (CFGCH_Band1 or CFGCH_POW_LCK or CFGCH_TRX_AH or CFGCH_BCN
                     or CFGCH_Band0 or CFGCH_Channel or CFGCH_BW2))
       or Unsigned_32 (C)
       or (if B = WS.Band_5_GHz then CFGCH_Band1_5G or CFGCH_Band0_5G else 0)
       or CFGCH_BW2) and 16#F_FFFF#);

   --  _bw_setting at 20 MHz.
   function With_20_MHz (Old : RF_Value) return RF_Value is
     (((Old and not (CFGCH_BW or CFGCH_POW_LCK or CFGCH_TRX_AH or CFGCH_BCN or CFGCH_BW2))
       or CFGCH_BW_20M or CFGCH_BW2) and 16#F_FFFF#);

   ---------------------------------------------------------------------------
   --  BB channel index (BB 0x734 [23:16]): 2.4 GHz the channel number;
   --  5 GHz (base index << 4) | (channel - base) / 2.
   ---------------------------------------------------------------------------
   function Channel_Index (B : WS.Band; C : WS.Channel_Number) return Unsigned_32 is
     (case B is
        when WS.Band_2_4_GHz => Unsigned_32 (C),
        when WS.Band_5_GHz =>
          (if C >= 149 then 16#50# + Unsigned_32 (C - 149) / 2
           elsif C >= 132 then 16#40# + Unsigned_32 (C - 132) / 2
           elsif C >= 100 then 16#30# + Unsigned_32 (C - 100) / 2
           elsif C >= 36 then 16#20# + Unsigned_32 (C - 36) / 2
           else 0))
   with Pre => Supported (B, C);

   --  SCO compensation (BB 0x49C0 [6:0], rtw8852bx_sco_mapping).
   function SCO (B : WS.Band; C : WS.Channel_Number) return Unsigned_32 is
     (case B is
        when WS.Band_2_4_GHz =>
          (if C = 1 then 109 elsif C <= 6 then 108 elsif C <= 10 then 107 else 106),
        when WS.Band_5_GHz =>
          (if C = 36 then 51 elsif C <= 56 then 50 elsif C <= 64 then 49
           elsif C = 100 then 48 elsif C <= 124 then 47 elsif C <= 149 then 46 else 45))
   with Pre => Supported (B, C);

   --  CCK correlator thresholds (BB 0x23B0 / 0x23B4 [18:0]), 2.4 GHz only.
   type Threshold_Table is array (WS.Channel_Number range 1 .. 14) of Unsigned_32;
   Barker_Thresholds : constant Threshold_Table :=
     [16#1CFEA#, 16#1D0E1#, 16#1D1D7#, 16#1D2CD#, 16#1D3C3#, 16#1D4B9#, 16#1D5B0#,
      16#1D6A6#, 16#1D79C#, 16#1D892#, 16#1D988#, 16#1DA7F#, 16#1DB75#, 16#1DDC4#];
   CCK_Thresholds : constant Threshold_Table :=
     [16#27DE3#, 16#27F35#, 16#28088#, 16#281DA#, 16#2832D#, 16#2847F#, 16#285D2#,
      16#28724#, 16#28877#, 16#289C9#, 16#28B1C#, 16#28C6E#, 16#28DC1#, 16#290ED#];

   ---------------------------------------------------------------------------
   --  Gain band and the per-band LNA/TIA gain errors (bytes as written:
   --  LNA 0 .. 6 then TIA 0 .. 1) and RPL references, decoded from
   --  rtw89_8852b_phy_bb_reg_gain.
   ---------------------------------------------------------------------------
   type Gain_Band is (Band_2G, Band_5G_Low, Band_5G_Mid, Band_5G_High);
   function Gain_Band_Of (B : WS.Band; C : WS.Channel_Number) return Gain_Band is
     (case B is
        when WS.Band_2_4_GHz => Band_2G,
        when WS.Band_5_GHz =>
          (if C <= 64 then Band_5G_Low elsif C <= 144 then Band_5G_Mid else Band_5G_High));

   type Path is (Path_A, Path_B);
   Gain_Entries : constant := 9;
   type Gain_Bytes is array (1 .. Gain_Entries) of Unsigned_8;
   type Gain_Table is array (Gain_Band, Path) of Gain_Bytes;
   Gain_Errors : constant Gain_Table :=
     [Band_2G      => [Path_A => [16#B7#, 16#DD#, 16#FB#, 16#18#, 16#36#, 16#54#, 16#6F#, 16#31#, 16#4F#],
                       Path_B => [16#B7#, 16#E0#, 16#FE#, 16#1B#, 16#38#, 16#52#, 16#6C#, 16#31#, 16#50#]],
      Band_5G_Low  => [Path_A => [16#9E#, 16#C3#, 16#E6#, 16#07#, 16#26#, 16#45#, 16#65#, 16#50#, 16#67#],
                       Path_B => [16#9F#, 16#C6#, 16#E9#, 16#09#, 16#27#, 16#46#, 16#67#, 16#50#, 16#67#]],
      Band_5G_Mid  => [Path_A => [16#9F#, 16#C4#, 16#E8#, 16#06#, 16#26#, 16#45#, 16#65#, 16#50#, 16#67#],
                       Path_B => [16#A0#, 16#C6#, 16#E9#, 16#07#, 16#28#, 16#47#, 16#67#, 16#50#, 16#68#]],
      Band_5G_High => [Path_A => [16#9D#, 16#C3#, 16#E5#, 16#04#, 16#25#, 16#43#, 16#63#, 16#50#, 16#67#],
                       Path_B => [16#9F#, 16#C6#, 16#E9#, 16#06#, 16#27#, 16#45#, 16#65#, 16#50#, 16#67#]]];

   type RPL_Values is record
      RPL1_Field : Unsigned_32;   --  0x49B0 / 0x4A00 [31:8]
      RPL2       : Unsigned_32;   --  0x49B4 / 0x4A04
      RPL3       : Unsigned_32;   --  0x49B8 / 0x4A08
   end record;
   RPL_2G : constant RPL_Values := (16#FC_FCF6#, 16#0000_00FC#, 16#0000_0000#);
   RPL_5G : constant RPL_Values := (16#F8_F8F4#, 16#F8E8_F8F8#, 16#F808_08E8#);

end RTW89_Channel_Values;
