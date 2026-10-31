------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  802.11 data frames <-> Ethernet frames for an infrastructure station
--  (IEEE 802.11-2020 9.3.2, RFC 1042 / 802.1H encapsulation, CCMP header
--  12.5.3.2; NET-WIFI-001 W3/W4, docs/wifi.md "Data path").
--
--  @description
--  Transmit: an Ethernet frame (destination, source, EtherType, payload)
--  becomes ToDS data: Address 1 the BSSID, 2 our address, 3 the
--  destination; then, on a protected link, the 8-octet CCMP header with
--  the packet number (the hardware encrypts and appends the MIC); then the
--  LLC/SNAP header carrying the EtherType; then the payload.
--
--  Receive: FromDS data (or QoS data) from the BSS, to us or to a group
--  address, becomes an Ethernet frame (destination Address 1, source
--  Address 3). Protected frames carry the CCMP header the hardware left in
--  place and the 8-octet MIC after the payload; Decode reports the packet
--  number for the caller's replay check (Wifi_Replay) and excludes the
--  MIC. A-MSDUs, null data, 4-address frames, and payloads without an
--  RFC 1042 or bridge-tunnel header are refused.
--
--  Proved (level 2): no run-time errors; Decode's payload lies inside the
--  frame and after every header; Build_Header's length is what it wrote.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package IEEE80211_Data with Pure, SPARK_Mode is

   Maximum_Octets : constant := 4096;
   subtype Octet_Index is Natural range 0 .. Maximum_Octets;
   type Octets is array (Octet_Index range <>) of Unsigned_8;
   type Address is array (0 .. 5) of Unsigned_8;

   --  Frame Control (9.2.4.1): octet 0 type/subtype, octet 1 flags.
   FC_Data      : constant Unsigned_8 := 16#08#;   --  type 2, subtype 0
   FC_QoS_Data  : constant Unsigned_8 := 16#88#;   --  type 2, subtype 8
   Type_Mask    : constant Unsigned_8 := 16#0C#;
   Type_Data    : constant Unsigned_8 := 16#08#;
   Flag_To_DS   : constant Unsigned_8 := 2 ** 0;
   Flag_From_DS : constant Unsigned_8 := 2 ** 1;
   Flag_Protected : constant Unsigned_8 := 2 ** 6;
   Flag_Order   : constant Unsigned_8 := 2 ** 7;   --  +HTC on QoS data
   Header_Bytes     : constant := 24;
   QoS_Control_Bytes : constant := 2;
   HT_Control_Bytes : constant := 4;
   Address_1_At : constant := 4;
   Address_2_At : constant := 10;
   Address_3_At : constant := 16;
   Sequence_At  : constant := 22;
   QoS_TID_Mask    : constant Unsigned_8 := 16#0F#;
   QoS_AMSDU_Bit   : constant Unsigned_8 := 2 ** 7;

   --  CCMP header (12.5.3.2): PN0 PN1 reserved (ExtIV | key ID << 6) PN2..PN5.
   CCMP_Header_Bytes : constant := 8;
   CCMP_MIC_Bytes    : constant := 8;
   ExtIV_Bit         : constant Unsigned_8 := 2 ** 5;
   Key_ID_Shift      : constant := 6;
   subtype Packet_Number is Unsigned_64 range 0 .. 2 ** 48 - 1;
   subtype Key_ID is Natural range 0 .. 3;

   --  LLC/SNAP (RFC 1042) and the bridge tunnel (802.1H).
   SNAP_Bytes : constant := 8;
   RFC_1042_Prefix      : constant Octets (0 .. 5) := [16#AA#, 16#AA#, 16#03#, 0, 0, 0];
   Bridge_Tunnel_Prefix : constant Octets (0 .. 5) := [16#AA#, 16#AA#, 16#03#, 0, 0, 16#F8#];
   EtherType_EAPOL : constant Unsigned_16 := 16#888E#;

   subtype TID is Natural range 0 .. 15;

   ---------------------------------------------------------------------------
   --  Transmit
   ---------------------------------------------------------------------------
   type Protection (Encrypted : Boolean := False) is record
      case Encrypted is
         when True =>
            PN  : Packet_Number;
            Key : Key_ID;
         when False => null;
      end case;
   end record;

   Maximum_Header : constant := Header_Bytes + CCMP_Header_Bytes + SNAP_Bytes;
   subtype Header_Length is Natural range Header_Bytes + SNAP_Bytes .. Maximum_Header;

   function Header_Bytes_For (P : Protection) return Header_Length is
     (Header_Bytes + SNAP_Bytes + (if P.Encrypted then CCMP_Header_Bytes else 0));

   subtype Sequence_Number is Natural range 0 .. 4095;
   Sequence_Shift : constant := 4;

   --  The 802.11 header, CCMP header and LLC/SNAP for an Ethernet frame
   --  from Own to Destination; the payload follows at Header_Bytes_For (P).
   procedure Build_Header
     (BSSID, Own, Destination : Address; Ether_Type : Unsigned_16; Sequence : Sequence_Number;
      P : Protection; Frame : in out Octets)
   with Pre  => Frame'First = 0 and then Frame'Length >= Maximum_Header;

   ---------------------------------------------------------------------------
   --  Receive
   ---------------------------------------------------------------------------
   type Refusal is
     (None, Too_Short, Not_Data, Wrong_Direction, Not_From_BSS, Not_For_Us, Null_Data,
      Aggregate, Bad_CCMP_Header, Unprotected_On_Protected_Link, Not_SNAP);

   type Received is record
      Why          : Refusal := Too_Short;
      Destination  : Address := [others => 0];
      Source       : Address := [others => 0];
      Ether_Type   : Unsigned_16 := 0;
      Payload_First : Octet_Index := 0;
      Payload_Length : Octet_Index := 0;
      Encrypted    : Boolean := False;
      PN           : Packet_Number := 0;
      Key          : Key_ID := 0;
      Group        : Boolean := False;   --  Address 1 is a group address
      Traffic      : TID := 0;           --  QoS TID (0 for non-QoS data)
      QoS          : Boolean := False;
   end record;

   Group_Bit : constant Unsigned_8 := 16#01#;

   --  Decode a received data frame (no FCS). Protected_Link: a pairwise
   --  key is installed, so unprotected frames are refused except EAPOL.
   procedure Decode (Frame : Octets; BSSID, Own : Address; Protected_Link : Boolean;
                     Item : out Received)
   with Pre  => Frame'First = 0,
        Post => (if Item.Why = None then
                   Item.Payload_First >= Header_Bytes + SNAP_Bytes
                   and then Item.Payload_First + Item.Payload_Length <= Frame'Length
                   and then (if Item.Encrypted then
                               Item.Payload_First + Item.Payload_Length + CCMP_MIC_Bytes
                                 <= Frame'Length));

end IEEE80211_Data;
