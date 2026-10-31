------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The RTL8852BE transmit WD page for the data and management channels
--  (NET-WIFI-001 W3/W4, docs/wifi.md "DMA rings"): WD body, WD info, the
--  WP sequence and one address info entry, as rtw89 builds them
--  (core.c rtw89_core_fill_txdesc, rtw89_build_txwd_*; pci.c
--  rtw89_pci_txwd_submit, rtw89_pci_fill_txaddr_info; txrx.h and pci.h
--  field layouts; GPL-2.0 OR BSD-3-Clause).
--
--  @description
--  Unlike channel 12, where the WD body heads the payload in one buffer,
--  ACH0..3 and the management channel take a TX BD that points at a WD
--  page; its address info points at the frame. The WP sequence identifies
--  the page in the release report (RPQ) that says the hardware is done
--  with it, so the page and its frame may be reused.
--
--  Management frames go at a fixed basic rate with fallback off and the
--  hardware's sequence numbers; data frames carry their sequence number,
--  name a security CAM entry when the link is protected (the hardware
--  encrypts and appends the MIC; the CCMP header is in the frame), and
--  for now go at a fixed rate too (rate adaptation is a later step).
--
--  Proved (level 2): no run-time errors; every field is written within
--  its mask (each Page byte is what the field encodings say).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with RTW89_Descriptors;

package RTW89_TX with Pure, SPARK_Mode is

   ---------------------------------------------------------------------------
   --  Channels and queue selectors (core.h enum rtw89_dma_ch, txrx.h
   --  enum rtw89_tx_qsel; core.c rtw89_core_get_ch_dma).
   ---------------------------------------------------------------------------
   type Channel is (ACH0, B0MG);
   for Channel use (ACH0 => 0, B0MG => 8);

   QSel_Best_Effort : constant := 16#00#;   --  RTW89_TX_QSEL_BE_0 (TID 0, 3)
   QSel_Management  : constant := 16#12#;   --  RTW89_TX_QSEL_B0_MGMT

   --  Hardware rate codes (core.h enum rtw89_hw_rate).
   Rate_CCK_1  : constant := 16#00#;
   Rate_OFDM_6 : constant := 16#04#;
   Rate_OFDM_24 : constant := 16#08#;
   subtype Rate_Code is Unsigned_32 range 0 .. 16#1FF#;

   --  Security (core.h enum rtw89_sec_key_type).
   Key_Type_CCMP_128 : constant := 6;
   subtype Security_CAM_Index is Natural range 0 .. 255;

   --  core.h RTW89_MGMT_HW_SSN_SEL, RTW89_MGMT_HW_SEQ_MODE.
   Management_SSN_Select : constant := 1;
   Management_Sequence_Mode : constant := 1;

   subtype MACID is Natural range 0 .. 127;
   subtype Sequence is Natural range 0 .. 4095;
   subtype Frame_Size is Natural range 1 .. 16#3FFF#;
   subtype WP_Sequence is Natural range 0 .. 16#7FFF#;

   type Request is record
      On          : Channel := B0MG;
      Station     : MACID := 0;
      Size        : Frame_Size := 1;
      Frame_Bus   : RTW89_Descriptors.Bus_Address := 0;
      Page_Sequence : WP_Sequence := 0;
      SW_Sequence : Sequence := 0;          --  data frames (management: hardware)
      Rate        : Rate_Code := Rate_OFDM_6;
      Header_Bytes : Natural range 0 .. 62 := 0;   --  802.11 header (data), in octets, even
      Group       : Boolean := False;       --  broadcast or multicast: no RTS
      Encrypt     : Boolean := False;
      Security_Entry : Security_CAM_Index := 0;
   end record;

   ---------------------------------------------------------------------------
   --  The page: body (24), info (24), WP info (8), address info (8).
   ---------------------------------------------------------------------------
   Body_Bytes : constant := 24;
   Info_Bytes : constant := 24;
   WP_Bytes   : constant := 8;
   Address_Info_Bytes : constant := 8;
   Page_Bytes : constant := Body_Bytes + Info_Bytes + WP_Bytes + Address_Info_Bytes;
   type Page is array (0 .. Page_Bytes - 1) of Unsigned_8;

   --  WD body dword 0 (RTW89_TXWD_BODY0_*).
   Body0_WD_Info_Enable : constant Unsigned_32 := 2 ** 22;
   Body0_Channel_Shift  : constant := 16;          --  [19:16]
   Body0_Header_Shift   : constant := 11;          --  HDR_LLC_LEN [15:11], 2-octet units
   Body0_WD_Page        : constant Unsigned_32 := 2 ** 7;
   Body0_SSN_Select_Shift : constant := 2;         --  [3:2]
   --  dword 2
   Body2_MACID_Shift    : constant := 24;          --  [30:24]
   Body2_QSel_Shift     : constant := 17;          --  [22:17]
   Body2_Size_Mask      : constant Unsigned_32 := 16#3FFF#;
   --  dword 3
   Body3_Sequence_Mask  : constant Unsigned_32 := 16#0FFF#;
   --  info dword 0
   Info0_Use_Rate       : constant Unsigned_32 := 2 ** 30;
   Info0_Rate_Shift     : constant := 16;          --  [24:16]
   Info0_Disable_Fallback : constant Unsigned_32 := 2 ** 10;
   --  info dword 1
   Info1_Lowest_Rate_Shift : constant := 16;       --  [24:16]
   --  info dword 2
   Info2_Key_Type_Shift : constant := 9;           --  [12:9]
   Info2_Hardware_Encrypt : constant Unsigned_32 := 2 ** 8;
   --  info dword 4
   Info4_HW_RTS_Enable  : constant Unsigned_32 := 2 ** 31;
   Info4_RTS_Enable     : constant Unsigned_32 := 2 ** 27;
   --  WP info: seq0 = page sequence | valid (pci.h RTW89_PCI_TXWP_VALID).
   WP_Valid             : constant Unsigned_16 := 2 ** 15;
   --  Address info (pci.h rtw89_pci_tx_addr_info_32): length, option
   --  (MSDU last segment 15, DMA high [13:6], count [5:0]), address.
   Address_MSDU_Last    : constant Unsigned_16 := 2 ** 15;
   Address_Count_One    : constant Unsigned_16 := 1;

   --  What the TX BD's length names: the whole page.
   function Fill (R : Request) return Page;

   --  The page's sequence as the release report gives it back
   --  (pci.h RTW89_PCI_RPP_SEQ [30:16]).
   Report_Sequence_Shift : constant := 16;
   Report_Sequence_Mask  : constant Unsigned_32 := 16#7FFF#;
   Report_Polluted       : constant Unsigned_32 := 2 ** 31;
   function Report_Sequence (Word : Unsigned_32) return WP_Sequence is
     (WP_Sequence (Shift_Right (Word, Report_Sequence_Shift) and Report_Sequence_Mask));

end RTW89_TX;
