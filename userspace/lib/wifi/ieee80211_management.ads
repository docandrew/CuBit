------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The management frames a station exchanges to join a BSS (IEEE
--  802.11-2020 9.3.3.12 Authentication, 9.3.3.6 Association Request,
--  9.3.3.7 Association Response, 9.3.3.13 Deauthentication, 9.3.3.5
--  Disassociation; NET-WIFI-001 W3): builders for what CuBit sends and
--  total decoders for what the AP answers.
--
--  @description
--  Only Open System authentication (algorithm 0) is built (WPA2-PSK uses
--  it too; SAE comes with W5). Frames are MPDUs without FCS; the hardware
--  fills the sequence number and FCS. Decoders refuse anything not from
--  the BSSID being joined. Proved free of run-time errors.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package IEEE80211_Management with Pure, SPARK_Mode is

   Maximum_Octets : constant := 4096;
   subtype Octet_Index is Natural range 0 .. Maximum_Octets;
   type Octets is array (Octet_Index range <>) of Unsigned_8;
   type Address is array (0 .. 5) of Unsigned_8;

   --  Frame Control octet 0 (type management, subtype << 4).
   FC_Association_Request  : constant Unsigned_8 := 16#00#;
   FC_Association_Response : constant Unsigned_8 := 16#10#;
   FC_Disassociation       : constant Unsigned_8 := 16#A0#;
   FC_Authentication       : constant Unsigned_8 := 16#B0#;
   FC_Deauthentication     : constant Unsigned_8 := 16#C0#;
   Header_Bytes            : constant := 24;
   Control_Flags_At        : constant := 1;
   Flag_Protected          : constant Unsigned_8 := 2 ** 6;   --  Protected Frame (9.2.4.1.9)
   Address_1_At            : constant := 4;
   Address_2_At            : constant := 10;
   Address_3_At            : constant := 16;

   Algorithm_Open_System   : constant := 0;
   Authentication_Request_Sequence  : constant := 1;
   Authentication_Response_Sequence : constant := 2;
   Status_Success          : constant := 0;
   Authentication_Bytes    : constant := Header_Bytes + 6;

   --  Capability Information we send: ESS, short preamble, short slot
   --  (9.4.1.4); Privacy for an RSN network.
   Capability_ESS            : constant Unsigned_16 := 2 ** 0;
   Capability_Privacy        : constant Unsigned_16 := 2 ** 4;
   Capability_Short_Preamble : constant Unsigned_16 := 2 ** 5;
   Capability_Short_Slot     : constant Unsigned_16 := 2 ** 10;
   --  Beacon intervals between our wake-ups (we never sleep yet).
   Listen_Interval           : constant Unsigned_16 := 10;
   Element_SSID : constant Unsigned_8 := 0;
   Element_Rates : constant Unsigned_8 := 1;
   Element_Extended_Rates : constant Unsigned_8 := 50;
   Maximum_SSID : constant := 32;
   Maximum_Rates : constant := 8;          --  Supported Rates element
   Maximum_Extended_Rates : constant := 255;
   Maximum_Frame : constant := 1024;

   procedure Build_Authentication (BSSID, Own : Address; Frame : out Octets; Length : out Natural)
   with Pre  => Frame'First = 0 and then Frame'Length >= Authentication_Bytes,
        Post => Length = Authentication_Bytes;

   --  An Association Request carrying SSID, the AP's rates (Rates and
   --  Extended_Rates as its elements' data) and, for RSN, CuBit's RSN
   --  element (Extra, a whole element).
   procedure Build_Association_Request
     (BSSID, Own : Address; Capability : Unsigned_16;
      SSID : Octets; Rates : Octets; Extended_Rates : Octets; Extra : Octets;
      Frame : out Octets; Length : out Natural)
   with Pre  => Frame'First = 0 and then Frame'Length = Maximum_Frame
                and then SSID'Length <= Maximum_SSID
                and then Rates'Length <= Maximum_Rates
                and then Extended_Rates'Length <= Maximum_Extended_Rates
                and then Extra'Length <= 255,
        Post => Length <= Maximum_Frame;

   type Answer_Kind is (Authentication_Answer, Association_Answer, Deauthenticated,
                        Disassociated, Other);
   type Answer is record
      Kind     : Answer_Kind := Other;
      Status   : Unsigned_16 := 0;     --  status code, or reason code for deauth/disassoc
      Sequence : Unsigned_16 := 0;     --  authentication transaction sequence
      Algorithm : Unsigned_16 := 0;
      AID      : Unsigned_16 := 0;     --  association ID (bits 13:0)
   end record;

   --  Decode a management frame from BSSID addressed to Own (or a
   --  broadcast deauthentication from BSSID).
   procedure Decode (Frame : Octets; BSSID, Own : Address; Item : out Answer)
   with Pre => Frame'First = 0 and then Frame'Length <= 4096;

end IEEE80211_Management;
