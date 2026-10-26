------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The RTL8852BE baseband and RF for a receive-only station
--  (NET-WIFI-001 W1, docs/wifi.md "Scanning"): BB/RF reset, the Realtek
--  BB, RF and NCTL tables, WLAN-only antenna grant, the receive-side
--  dynamic-mechanism init, the calibrations reception needs (RCK, DACK
--  with ADC DC, RX DCK), the RX paths, and the 20 MHz channel switch.
--
--  @description
--  Sequences from rtw89 phy.c, rtw8852b.c, rtw8852b_common.c,
--  rtw8852b_rfk.c, rtw8852b_rfk_table.c and mac.c (coex PTA and grant),
--  GPL-2.0 OR BSD-3-Clause, taken under BSD-3-Clause, resolved for the
--  8852B (cut CBV), RFE type 1, two RF paths, PHY 0. Transmit-only steps
--  (TX power, TSSI, DPK, IQK's transmit half) are left out: nothing here
--  makes the radio transmit. BB registers are at BAR2 0x10000 + address;
--  RF registers go through the direct windows (address bit 16) or the
--  serial interface (SWSI).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Wifi_Scan;
with RTW89_Outcomes; use RTW89_Outcomes;
with RTW89_MMIO;
with RTW89_Efuse;

package RTW89_PHY is
   package WS renames CuBit.Wifi_Scan;

   --  What the board's efuse says that the PHY uses.
   Phycap_First : constant := 16#580#;
   Phycap_Bytes : constant := 128;
   type Phycap_Map is array (0 .. Phycap_Bytes - 1) of Unsigned_8;

   type Board is record
      RFE      : Unsigned_8 := 0;
      Cut      : Unsigned_8 := 0;
      Logical  : RTW89_Efuse.Logical_Map := [others => RTW89_Efuse.Unprogrammed];
      Phycap   : Phycap_Map := [others => RTW89_Efuse.Unprogrammed];
   end record;

   type Table_Counts is record
      BB, Radio_A, Radio_B, NCTL : Natural := 0;
   end record;

   --  rtw89_chip_reset_bb_rf: BB/RF disable then enable.
   procedure Reset_BB_RF (Result : out Outcome) with Pre => RTW89_MMIO.Attached;

   --  The BB table, BB reset, then the RF A and B tables for this board.
   procedure Load_Tables (Card : Board; Counts : out Table_Counts; Result : out Outcome)
     with Pre => RTW89_MMIO.Attached;

   --  PTA init, then the antenna granted to WLAN on both paths (BT unused).
   procedure Grant_Antenna_To_WLAN (Result : out Outcome) with Pre => RTW89_MMIO.Attached;

   --  rtw89_phy_dm_init's receive-side parts: RPL bases, PHY status
   --  contents, packet-detect thresholds, crystal trim, NCTL engine and
   --  table (Counts.NCTL).
   procedure DM_Init (Card : Board; Counts : in out Table_Counts; Result : out Outcome)
     with Pre => RTW89_MMIO.Attached;

   --  RCK, DACK (AFE init, ADC DC calibration, DAC calibration), RX DCK.
   --  Timeouts inside a calibration are counted (rtw89 only logs them).
   procedure Calibrate (Timeouts : out Natural; Result : out Outcome)
     with Pre => RTW89_MMIO.Attached;

   --  Both RX paths on (rtw89 cfg_txrx_path, RX side).
   procedure Configure_RX_Paths (Card : Board) with Pre => RTW89_MMIO.Attached;

   --  Tune to a 20 MHz channel (host-driven, no firmware command). Locked
   --  reports the path A synthesizer lock after the switch.
   procedure Set_Channel (Card : Board; Band : WS.Band; Channel : WS.Channel_Number;
                          Locked : out Boolean; Result : out Outcome)
     with Pre => RTW89_MMIO.Attached;

   --  RX DC offset calibration on the current channel (both paths).
   procedure RX_DC_Calibrate with Pre => RTW89_MMIO.Attached;

end RTW89_PHY;
