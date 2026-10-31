------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The RTL8852BE receive path's untrusted structures (NET-WIFI-001 W1,
--  docs/wifi.md "DMA rings"): the RX buffer's leading info word, the RX
--  descriptor, the PPDU status (PHY status header and the IEs that carry
--  signal and channel), the C2H header, and the PHY's channel index.
--
--  @description
--  Layouts from Linux rtw89 pci.h (rtw89_pci_rxbd_info), txrx.h (RX
--  descriptor, rtw89_rxinfo, rtw89_phy_sts_hdr, phy_sts IE00/IE01), fw.h
--  (rtw89_c2h_hdr), core.c (rtw89_core_query_rxdesc,
--  rtw89_core_rx_process_mac_ppdu, rtw89_core_rx_parse_phy_sts), phy.c
--  (rtw89_decode_chan_idx, the AX IE length table), GPL-2.0 OR BSD-3-Clause,
--  taken under BSD-3-Clause; resolved for the 8852B (AX, PCIe).
--
--  Everything here was written by the device's DMA engine or firmware and
--  is hostile. The driver copies a buffer out of DMA memory once (a
--  Buffer) and only then decodes it. Decoding is total and proved free of
--  run-time errors: a decoded payload always lies inside the bytes the
--  device said it wrote, which lie inside the buffer; every PHY status IE
--  lies inside the status; anything else is refused with a reason.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package RTW89_RX with Pure, SPARK_Mode is

   --  One RX buffer as posted to the ring (RTW89_Layout.RX_Buffer_Bytes).
   Buffer_Bytes : constant := 4096;
   subtype Buffer_Index is Natural range 0 .. Buffer_Bytes - 1;
   type Buffer is array (Buffer_Index) of Unsigned_8;

   function U32 (B : Buffer; At_Byte : Buffer_Index) return Unsigned_32 is
     (Unsigned_32 (B (At_Byte)) or Shift_Left (Unsigned_32 (B (At_Byte + 1)), 8)
      or Shift_Left (Unsigned_32 (B (At_Byte + 2)), 16)
      or Shift_Left (Unsigned_32 (B (At_Byte + 3)), 24))
   with Pre => At_Byte <= Buffer_Bytes - 4;

   ---------------------------------------------------------------------------
   --  The leading word (rtw89_pci_rxbd_info): bytes written into this
   --  buffer counted from offset 0 including the word itself [13:0], last
   --  segment (14), first segment (15), DMA tag [28:16] (unchecked on 8852B).
   ---------------------------------------------------------------------------
   Info_Bytes       : constant := 4;
   Write_Size_Mask  : constant Unsigned_32 := 16#3FFF#;
   Last_Segment     : constant Unsigned_32 := 2 ** 14;
   First_Segment    : constant Unsigned_32 := 2 ** 15;

   ---------------------------------------------------------------------------
   --  The RX descriptor at offset 4: 16 bytes, or 32 when LONG_RXD.
   ---------------------------------------------------------------------------
   Short_Descriptor : constant := 16;
   Long_Descriptor  : constant := 32;
   Descriptor_At    : constant := Info_Bytes;

   --  dw0
   Packet_Length_Mask : constant Unsigned_32 := 16#3FFF#;      --  AX_RXD_RPKT_LEN_MASK
   Shift_Shift        : constant := 14;                        --  AX_RXD_SHIFT_MASK [15:14]
   Two_Bit_Mask       : constant Unsigned_32 := 16#3#;
   MAC_Info_Valid_Bit : constant Unsigned_32 := 2 ** 23;       --  AX_RXD_MAC_INFO_VLD
   Packet_Type_Shift  : constant := 24;                        --  AX_RXD_RPKT_TYPE_MASK [27:24]
   Nibble_Mask        : constant Unsigned_32 := 16#F#;
   Driver_Info_Shift  : constant := 28;                        --  AX_RXD_DRV_INFO_SIZE_MASK [30:28]
   Three_Bit_Mask     : constant Unsigned_32 := 16#7#;
   Driver_Info_Unit   : constant := 8;
   Long_Bit           : constant Unsigned_32 := 2 ** 31;       --  AX_RXD_LONG_RXD
   --  dw1
   PPDU_Count_Shift   : constant := 4;                         --  [6:4]
   Rate_Shift         : constant := 16;                        --  [24:16]
   Rate_Mask          : constant Unsigned_32 := 16#1FF#;
   GI_LTF_Shift       : constant := 25;                        --  [27:25]
   Bandwidth_Shift    : constant := 30;                        --  [31:30]
   --  dw3
   A1_Match_Bit       : constant Unsigned_32 := 2 ** 0;
   SW_Decrypt_Bit     : constant Unsigned_32 := 2 ** 1;        --  AX_RXD_SW_DEC
   HW_Decrypt_Bit     : constant Unsigned_32 := 2 ** 2;        --  AX_RXD_HW_DEC
   CRC32_Error_Bit    : constant Unsigned_32 := 2 ** 9;
   ICV_Error_Bit      : constant Unsigned_32 := 2 ** 10;
   --  dw4 (long only)
   Frame_Type_Mask    : constant Unsigned_32 := 16#3#;

   --  enum rtw89_core_rx_type (core.h); 15 is undefined.
   type Packet_Type is
     (WIFI, PPDU_Status, Channel_Info, BB_Scope, F2P_TX_Command, SS2FW, TX_Report,
      TX_Release_Host, DFS_Report, TX_Release_CPU, C2H, CSI, CQI, H2C, FWDL, Undefined);

   --  enum rtw89_rx_frame_type; Unknown for a short descriptor.
   type Frame_Type is (Management, Control, Data, Reserved, Unknown);

   subtype Payload_Length is Natural range 0 .. Buffer_Bytes;
   subtype Rate_Code is Unsigned_32 range 0 .. 16#1FF#;
   subtype Small_Field is Unsigned_32 range 0 .. 7;
   subtype Bandwidth_Code is Unsigned_32 range 0 .. 3;

   type Descriptor is record
      Kind          : Packet_Type := Undefined;
      Payload_First : Buffer_Index := 0;
      Length        : Payload_Length := 0;     --  RPKT_LEN (802.11: includes the FCS)
      Long          : Boolean := False;
      MAC_Info      : Boolean := False;
      Shift         : Small_Field := 0;        --  [15:14] as read; nonzero is refused
      PPDU_Count    : Small_Field := 0;
      Rate          : Rate_Code := 0;
      GI_LTF        : Small_Field := 0;
      Bandwidth     : Bandwidth_Code := 0;
      A1_Match      : Boolean := False;
      CRC_Error     : Boolean := False;
      ICV_Error     : Boolean := False;
      HW_Decrypted  : Boolean := False;        --  decrypted (CCMP header and MIC left in place)
      SW_Decrypt    : Boolean := False;        --  the hardware asks software to decrypt
      Frame         : Frame_Type := Unknown;
      Single        : Boolean := False;        --  first and last segment in one buffer
   end record;

   type Status is
     (Decoded,
      Size_Too_Small,       --  WRITE_SIZE below info word + descriptor
      Size_Too_Large,       --  WRITE_SIZE above the buffer
      Not_First_Segment,    --  continuation buffers are not decoded (counted, dropped)
      Multi_Buffer,         --  a packet spanning buffers (oversize for us: dropped)
      Shift_Nonzero,        --  dw0 SHIFT set: payload place unknown (Linux ignores it)
      Payload_Overrun);     --  the payload runs past WRITE_SIZE

   function Name (S : Status) return String is
     (case S is
        when Decoded           => "decoded",
        when Size_Too_Small    => "size-too-small",
        when Size_Too_Large    => "size-too-large",
        when Not_First_Segment => "not-first-segment",
        when Multi_Buffer      => "multi-buffer",
        when Shift_Nonzero     => "shift-nonzero",
        when Payload_Overrun   => "payload-overrun");

   function Fits (D : Descriptor; Write_Size : Natural) return Boolean is
     (Write_Size <= Buffer_Bytes and then D.Payload_First + D.Length <= Write_Size);

   procedure Decode (B : Buffer; D : out Descriptor; Write_Size : out Payload_Length;
                     Result : out Status)
   with Post => (if Result = Decoded then Fits (D, Write_Size) and then D.Single);

   ---------------------------------------------------------------------------
   --  802.11 frames: the MPDU without its 4-octet FCS.
   ---------------------------------------------------------------------------
   FCS_Bytes : constant := 4;

   ---------------------------------------------------------------------------
   --  PPDU status (RPKT_TYPE 1): [MAC info] then the PHY status.
   ---------------------------------------------------------------------------
   MAC_Info_Bytes       : constant := 8;        --  RTW89_PPDU_MAC_INFO_SIZE
   MAC_Info_User_Bytes  : constant := 4;        --  RTW89_PPDU_MAC_INFO_USR_SIZE
   RX_Counter_Bytes     : constant := 96;       --  RTW89_PPDU_MAC_RX_CNT_SIZE
   Maximum_Users        : constant := 4;        --  rtw8852b ppdu_max_usr
   User_Count_Mask      : constant Unsigned_32 := 16#F#;     --  w0 [3:0]
   RX_Counter_Valid_Bit : constant Unsigned_32 := 2 ** 29;   --  w0 bit 29
   PLCP_Length_Shift    : constant := 16;                    --  w1 [23:16], 8-byte units
   PLCP_Unit            : constant := 8;
   Byte_Mask            : constant Unsigned_32 := 16#FF#;

   --  PHY status header (rtw89_phy_sts_hdr), 8 bytes.
   PHY_Header_Bytes     : constant := 8;
   Class_Mask           : constant Unsigned_32 := 16#1F#;    --  IE_MAP [4:0]
   Header_2_Bit         : constant Unsigned_32 := 2 ** 5;    --  HDR_2_EN (never on AX)
   Valid_Bit            : constant Unsigned_32 := 2 ** 7;
   Length_Shift         : constant := 8;                     --  [15:8], 8-byte units
   RSSI_Average_Shift   : constant := 24;
   RSSI_B_Shift         : constant := 8;
   Status_Unit          : constant := 8;
   First_Packet_Class   : constant := 11;                    --  RTW89_CCK_PKT

   --  PHY status IEs: a le32 header with TYPE [4:0] and LEN [11:5] in
   --  8-byte units (for variable-length types).
   IE_Type_Mask         : constant Unsigned_32 := 16#1F#;
   IE_Length_Shift      : constant := 5;
   IE_Length_Mask       : constant Unsigned_32 := 16#7F#;
   IE_CMN_CCK           : constant := 0;
   IE_CMN_OFDM          : constant := 1;
   --  rtw89_phy_gen_ax.physt_ie_len, bytes; 0 = variable (VAR_LEN), and
   --  type 23 has length 0, which ends the walk as malformed.
   Variable : constant := 0;
   type IE_Length_Table is array (0 .. 31) of Natural;
   IE_Lengths : constant IE_Length_Table :=
     [16, 32, 24, 24, 8, 8, 8, 8, Variable, 8, Variable, 176, Variable, Variable, Variable, Variable,
      Variable, Variable, 16, 24, Variable, Variable, Variable, 0, 24, 24, 24, 24, 32, 32, 32, 32];
   Variable_IE_Types : constant array (0 .. 31) of Boolean :=
     [8 | 10 | 12 .. 17 | 20 .. 22 => True, others => False];
   --  IE00 [15:7] RPL (rpl_avg = RPL >> 1); IE01 [15:8] RSSI_AVG_FD and
   --  [23:16] CH_IDX. Both are read from the first 16 bytes only.
   CCK_RPL_Shift        : constant := 7;
   CCK_RPL_Mask         : constant Unsigned_32 := 16#1FF#;
   OFDM_RSSI_Shift      : constant := 8;
   OFDM_Channel_Shift   : constant := 16;
   IE_Read_Bytes        : constant := 16;
   OFDM_6_Rate          : constant := 4;      --  RTW89_HW_RATE_OFDM6

   subtype Raw_RSSI is Unsigned_8;           --  0.5 dB steps, offset 110 dB

   type PPDU_Report is record
      Class        : Unsigned_8 := 0;
      RSSI_Average : Raw_RSSI := 0;
      RSSI_A       : Raw_RSSI := 0;
      RSSI_B       : Raw_RSSI := 0;
      RPL_Average  : Raw_RSSI := 0;          --  from IE00/IE01, when Has_RPL
      Has_RPL      : Boolean := False;
      Channel_Index : Unsigned_8 := 0;       --  IE01 CH_IDX (0: none)
      IEs_Walked   : Boolean := False;       --  the IE walk ran to the end
   end record;

   type PPDU_Result is
     (Taken,
      MAC_Info_Short,       --  the MAC info block is longer than the payload
      Too_Many_Users,
      PHY_Too_Short,        --  under the 8-byte header
      PHY_Not_Valid,        --  VALID clear
      PHY_Length_Mismatch); --  LEN * 8 is not the status's length

   function Name (S : PPDU_Result) return String is
     (case S is
        when Taken               => "taken",
        when MAC_Info_Short      => "mac-info-short",
        when Too_Many_Users      => "too-many-users",
        when PHY_Too_Short       => "phy-too-short",
        when PHY_Not_Valid       => "phy-not-valid",
        when PHY_Length_Mismatch => "phy-length-mismatch");

   --  Decode the PPDU status payload of D (RPKT_TYPE 1). The header RSSIs
   --  are kept even when the IE walk fails (rtw89 does the same).
   procedure Decode_PPDU (B : Buffer; D : Descriptor; Report : out PPDU_Report;
                          Result : out PPDU_Result)
   with Pre => D.Kind = PPDU_Status and then D.Payload_First + D.Length <= Buffer_Bytes;

   --  dBm from a raw RSSI (RTW89_RSSI_RAW_TO_DBM: (s8)(raw >> 1) - 110).
   RSSI_Offset_dB : constant := 110;
   subtype dBm is Integer range -RSSI_Offset_dB .. 127 - RSSI_Offset_dB;
   function To_dBm (Raw : Raw_RSSI) return dBm is
     (Integer (Shift_Right (Raw, 1)) - RSSI_Offset_dB);

   --  The frame's signal: the stronger path from the PHY status header.
   function Signal_Of (R : PPDU_Report) return dBm is
     (To_dBm (Unsigned_8'Max (R.RSSI_A, R.RSSI_B)));

   --  Whether a PPDU status belongs to a frame (rtw89 core.c matching):
   --  same PPDU counter, rate index, GI class and bandwidth.
   function Matches (Frame, Status : Descriptor) return Boolean;

   ---------------------------------------------------------------------------
   --  The PHY's channel index (rtw89_decode_chan_idx): base [7:4], offset
   --  [3:0]. 0 = none; bases 1, 6 and 15 are invalid; 7 .. 14 are 6 GHz,
   --  which this radio does not have (refused).
   ---------------------------------------------------------------------------
   type Channel_Band is (None, Band_2_4, Band_5);
   type Decoded_Channel is record
      Band    : Channel_Band := None;
      Channel : Unsigned_8 := 0;
   end record;
   function Decode_Channel_Index (Index : Unsigned_8) return Decoded_Channel
   with Post => (if Decode_Channel_Index'Result.Band = Band_2_4 then
                   Decode_Channel_Index'Result.Channel in 1 .. 14)
                and then (if Decode_Channel_Index'Result.Band = Band_5 then
                   Decode_Channel_Index'Result.Channel in 36 .. 177);

   ---------------------------------------------------------------------------
   --  C2H (RPKT_TYPE 10): an 8-byte header, category [1:0], class [7:2],
   --  function [15:8]; w1 [13:0] = total length including the header.
   ---------------------------------------------------------------------------
   C2H_Header_Bytes : constant := 8;
   C2H_Class_Shift  : constant := 2;
   C2H_Class_Mask   : constant Unsigned_32 := 16#3F#;
   C2H_Function_Shift : constant := 8;
   C2H_Length_Mask  : constant Unsigned_32 := 16#3FFF#;
   type C2H_Header is record
      Category : Unsigned_8 := 0;
      Class    : Unsigned_8 := 0;
      Func     : Unsigned_8 := 0;
      Length   : Payload_Length := 0;
   end record;
   procedure Decode_C2H (B : Buffer; D : Descriptor; H : out C2H_Header; Valid : out Boolean)
   with Pre => D.Kind = C2H and then D.Payload_First + D.Length <= Buffer_Bytes,
        Post => (if Valid then H.Length in C2H_Header_Bytes .. D.Length);

end RTW89_RX;
