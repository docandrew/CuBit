------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  IEEE 802.11 beacon and probe-response decoding for the passive scan
--  (NET-WIFI-001 W1, docs/wifi.md "Frames and IEs"): the BSSID, SSID,
--  channel, capability, beacon interval and whether an RSN element is
--  present. Chip-independent; it moves to wlan.svc with W3.
--
--  @description
--  Facts from IEEE Std 802.11-2020: 9.2.4.1 (Frame Control), 9.3.3.2
--  (management frame format), 9.3.3.3 (Beacon), 9.3.3.11 (Probe Response),
--  9.4.1 (fixed fields), 9.4.2 (elements). Constant names follow Linux
--  include/linux/ieee80211.h where it has one.
--
--  The frame comes off the air through a closed-firmware device: it is
--  hostile. Decoding is total and proved free of run-time errors; it never
--  reads outside the frame; an element whose length runs past the body,
--  an SSID longer than 32 octets, or a DS Parameter Set that is not one
--  octet refuses the whole frame. Unknown and vendor elements are skipped
--  and counted.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package IEEE80211_Beacons with Pure, SPARK_Mode is

   --  The largest MPDU we accept (one RX buffer; IEEE 802.11-2020 Table
   --  9-34 lets an MMPDU reach 2304 octets of body).
   Maximum_Frame : constant := 4096;
   subtype Frame_Index is Natural range 0 .. Maximum_Frame - 1;
   type Frame_Bytes is array (Frame_Index range <>) of Unsigned_8;

   ---------------------------------------------------------------------------
   --  Frame Control (9.2.4.1): octet 0 = protocol version (bits 0..1),
   --  type (2..3), subtype (4..7); octet 1 = flags.
   ---------------------------------------------------------------------------
   Version_Mask       : constant Unsigned_8 := 16#03#;
   Type_Mask          : constant Unsigned_8 := 16#0C#;
   Subtype_Mask       : constant Unsigned_8 := 16#F0#;
   Type_Management    : constant Unsigned_8 := 16#00#;   --  IEEE80211_FTYPE_MGMT
   Subtype_Probe_Response : constant Unsigned_8 := 16#50#; --  IEEE80211_STYPE_PROBE_RESP
   Subtype_Beacon     : constant Unsigned_8 := 16#80#;   --  IEEE80211_STYPE_BEACON
   Flag_Protected     : constant Unsigned_8 := 16#40#;   --  IEEE80211_FCTL_PROTECTED >> 8
   Flag_Order         : constant Unsigned_8 := 16#80#;   --  IEEE80211_FCTL_ORDER >> 8 (+HT Control)

   --  Management header (9.3.3.2): FC 2, Duration 2, Address 1..3 at 4,
   --  10, 16, Sequence Control 2; +4 octets of HT Control with Order set.
   Address_Bytes       : constant := 6;
   Address_1_At        : constant := 4;
   Address_2_At        : constant := 10;
   Address_3_At        : constant := 16;
   Header_Bytes        : constant := 24;
   HT_Control_Bytes    : constant := 4;

   --  Beacon / probe response body (9.3.3.3): Timestamp 8, Beacon
   --  Interval 2, Capability Information 2, then elements.
   Timestamp_Bytes     : constant := 8;
   Interval_Offset     : constant := Timestamp_Bytes;
   Capability_Offset   : constant := Interval_Offset + 2;
   Elements_Offset     : constant := Capability_Offset + 2;

   Minimum_Frame       : constant := Header_Bytes + Elements_Offset;

   ---------------------------------------------------------------------------
   --  Elements (9.4.2): ID 1, Length 1, then Length octets.
   ---------------------------------------------------------------------------
   Element_Header_Bytes : constant := 2;
   Element_SSID         : constant Unsigned_8 := 0;    --  WLAN_EID_SSID
   Element_DS_Params    : constant Unsigned_8 := 3;    --  WLAN_EID_DS_PARAMS
   Element_RSN          : constant Unsigned_8 := 48;   --  WLAN_EID_RSN
   Element_HT_Operation : constant Unsigned_8 := 61;   --  WLAN_EID_HT_OPERATION
   Element_Rates        : constant Unsigned_8 := 1;    --  WLAN_EID_SUPP_RATES
   Element_Extended_Rates : constant Unsigned_8 := 50; --  WLAN_EID_EXT_SUPP_RATES
   Element_Vendor       : constant Unsigned_8 := 221;  --  WLAN_EID_VENDOR_SPECIFIC

   SSID_Octets          : constant := 32;               --  IEEE80211_MAX_SSID_LEN
   DS_Params_Length     : constant := 1;
   --  HT Operation (9.4.2.56): Primary Channel, then 21 more octets.
   HT_Operation_Length  : constant := 22;
   --  RSN (9.4.2.24): at least the 2-octet Version field.
   RSN_Minimum_Length   : constant := 2;

   subtype SSID_Length is Natural range 0 .. SSID_Octets;
   type SSID_Bytes is array (1 .. SSID_Octets) of Unsigned_8;
   type Address is array (0 .. Address_Bytes - 1) of Unsigned_8;

   type Frame_Kind is (Beacon, Probe_Response);

   --  0: the element was absent.
   subtype Channel_Octet is Unsigned_8;
   No_Channel : constant Channel_Octet := 0;

   --  Elements seen (bounded: a body holds at most Maximum_Frame / 2).
   subtype Element_Count is Natural range 0 .. Maximum_Frame / Element_Header_Bytes;

   --  Where an element's data lies in the frame (Length 0: absent).
   subtype Element_Length is Natural range 0 .. 255;
   type Element_Place is record
      First  : Frame_Index := 0;
      Length : Element_Length := 0;
   end record;
   No_Place : constant Element_Place := (0, 0);

   type Decoded is record
      Kind          : Frame_Kind := Beacon;
      BSSID         : Address := [others => 0];
      Transmitter   : Address := [others => 0];
      SSID          : SSID_Bytes := [others => 0];   --  past Length: zero
      Length        : SSID_Length := 0;
      Has_SSID      : Boolean := False;              --  an SSID element was present
      DS_Channel    : Channel_Octet := No_Channel;
      HT_Primary    : Channel_Octet := No_Channel;
      Capability    : Unsigned_16 := 0;
      Interval      : Unsigned_16 := 0;
      RSN           : Boolean := False;
      RSN_Element   : Element_Place := No_Place;     --  the first RSN element's data
      Rates         : Element_Place := No_Place;     --  Supported Rates
      Extended_Rates : Element_Place := No_Place;    --  Extended Supported Rates
      Elements      : Element_Count := 0;
      Skipped       : Element_Count := 0;            --  unknown, vendor or repeated
   end record;

   type Status is
     (Accepted,
      Too_Short,             --  shorter than the header and fixed fields
      Not_Version_0,
      Not_Management,
      Not_Beacon,            --  a management frame of another subtype
      Protected_Frame,       --  beacons are never protected
      Element_Overrun,       --  an element runs past the body
      SSID_Too_Long,
      DS_Params_Malformed);

   function Name (S : Status) return String is
     (case S is
        when Accepted            => "accepted",
        when Too_Short           => "too-short",
        when Not_Version_0       => "not-version-0",
        when Not_Management      => "not-management",
        when Not_Beacon          => "not-beacon",
        when Protected_Frame     => "protected",
        when Element_Overrun     => "element-overrun",
        when SSID_Too_Long       => "ssid-too-long",
        when DS_Params_Malformed => "ds-params-malformed");

   function Normal (D : Decoded) return Boolean is
     (D.Skipped <= D.Elements
      and then (for all I in 1 .. SSID_Octets => (if I > D.Length then D.SSID (I) = 0))
      and then (if not D.Has_SSID then D.Length = 0));

   --  Decode Frame (an MPDU without its FCS). Info is meaningful only
   --  when Result = Accepted.
   function Inside (P : Element_Place; Length : Natural) return Boolean is
     (P.First + P.Length <= Length);

   procedure Decode (Frame : Frame_Bytes; Info : out Decoded; Result : out Status)
   with Pre  => Frame'First = 0,
        Post => (if Result = Accepted then
                   Frame'Length >= Minimum_Frame and then Normal (Info)
                   and then Inside (Info.RSN_Element, Frame'Length)
                   and then Inside (Info.Rates, Frame'Length)
                   and then Inside (Info.Extended_Rates, Frame'Length));

   --  The channel the BSS says it is on: its DS Parameter Set, else its HT
   --  Operation's primary channel, else the channel it was heard on.
   function Channel_Of (D : Decoded; Heard_On : Channel_Octet) return Channel_Octet is
     (if D.DS_Channel /= No_Channel then D.DS_Channel
      elsif D.HT_Primary /= No_Channel then D.HT_Primary
      else Heard_On);

end IEEE80211_Beacons;
