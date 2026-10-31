------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  RTL8852BE transmit power (NET-WIFI-001 W3): the MAC's power-by-rate,
--  rate-offset and power-limit register words for a 20 MHz channel, and
--  the BB's power reference word, computed from Realtek's tables
--  (RTW89_TX_Power_8852B) as rtw89 does.
--
--  @description
--  rtw89 phy.c rtw89_phy_load_txpwr_byrate, rtw89_phy_set_txpwr_byrate_ax,
--  _offset_ax, _limit_ax and rtw89_phy_fill_txpwr_limit_20m_ax;
--  rtw8852b_common.c rtw8852bx_bb_cal_txpwr_ref (GPL-2.0 OR BSD-3-Clause,
--  taken under BSD-3-Clause). The tables are in RF units (1/4 dBm); the
--  MAC takes 1/2 dBm (txpwr_factor_rf 2, txpwr_factor_mac 1). Limits are
--  the worldwide ones (the lowest of every regulation's), with no SAR or
--  antenna-gain adjustment; the RU limits (trigger-based OFDMA uplink,
--  which CuBit does not use) are written as zero.
--
--  Proved (level 2): no run-time errors.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Wifi_Scan;

package RTW89_TX_Power with Pure, SPARK_Mode is
   package WS renames CuBit.Wifi_Scan;

   --  MAC registers (reg.h).
   PWR_RATE_CTRL      : constant := 16#D200#;
   PWR_REF_MASK       : constant Unsigned_32 := 16#0FFF_FC00#;   --  B_AX_PWR_REF [27:10]
   PWR_RATE_OFST_CTRL : constant := 16#D204#;
   PWR_OFFSET_MASK    : constant Unsigned_32 := 16#000F_FFFF#;   --  [19:0]
   PWR_BY_RATE        : constant := 16#D2C0#;
   PWR_LMT            : constant := 16#D2EC#;
   PWR_RU_LMT         : constant := 16#D33C#;

   Byrate_Words : constant := 11;   --  (CCK 1 + OFDM 2 + MCS 3 + HEDCM 1) + (MCS 3 + HEDCM 1)
   Limit_Words_Per_Path : constant := 10;   --  RTW89_TXPWR_LMT_PAGE_SIZE_AX / 4
   RU_Limit_Words_Per_Path : constant := 6; --  RTW89_TXPWR_LMT_RU_PAGE_SIZE_AX / 4
   Paths : constant := 2;
   type Byrate_Array is array (0 .. Byrate_Words - 1) of Unsigned_32;
   type Limit_Array is array (0 .. Paths * Limit_Words_Per_Path - 1) of Unsigned_32;

   function Supported (Band : WS.Band; Channel : WS.Channel_Number) return Boolean is
     (case Band is
        when WS.Band_2_4_GHz => Channel in 1 .. 14,
        when WS.Band_5_GHz   => Channel in 36 .. 64 | 100 .. 144 | 149 .. 177);

   function Byrate (Band : WS.Band) return Byrate_Array;
   function Offset (Band : WS.Band) return Unsigned_32;
   function Limits (Band : WS.Band; Channel : WS.Channel_Number) return Limit_Array
   with Pre => Supported (Band, Channel);

   --  BB power reference (rtw8852bx_bb_cal_txpwr_ref, reference 0 dBm):
   --  TSSI offset [26:18], power code [17:9], reference [8:0], for a path
   --  whose power is lowered by Decrease (1/8 dB; 0 for CuBit).
   BB_Reference_Mask : constant Unsigned_32 := 16#07FF_FFFF#;
   subtype Decrease_Eighths is Natural range 0 .. 255;
   function Reference (Decrease : Decrease_Eighths) return Unsigned_32;
   --  The BB registers it goes to: path A 0x5800, path B 0x7800, +4 OFDM, +8 CCK.
   Reference_Path_A : constant := 16#5800#;
   Reference_Path_B : constant := 16#7800#;
   Reference_OFDM   : constant := 16#4#;
   Reference_CCK    : constant := 16#8#;

   --  Transmit spectrum shaping for the worldwide regulation (tx_shape_lmt
   --  has no RTW89_WW entry: index 0): the CCK DFIR on 2.4 GHz ("flat",
   --  "sharp_14" on channel 14), and OFDM triangular shaping 0.
   TXFIR0 : constant := 16#2300#;
   DFIR_Words : constant := 8;
   type DFIR_Array is array (0 .. DFIR_Words - 1) of Unsigned_32;
   DFIR_Flat : constant DFIR_Array :=
     [16#023D_23FF#, 16#0029_B354#, 16#000F_C1C8#, 16#00FD_B053#,
      16#00F8_6F9A#, 16#06FA_EF92#, 16#00FE_5FCC#, 16#00FF_DFF5#];
   DFIR_Sharp_14 : constant DFIR_Array :=
     [16#023B_13FF#, 16#001C_42DE#, 16#00FD_B0AD#, 16#00F6_0F6E#,
      16#00FD_8F92#, 16#0602_D011#, 16#0001_C02C#, 16#00FF_F00A#];
   DCFO_OPT : constant := 16#4494#;
   TXSHAPE_TRIANGULAR_MASK : constant Unsigned_32 := 16#0300_0000#;   --  [25:24]
   Triangular_Worldwide : constant := 0;

end RTW89_TX_Power;
