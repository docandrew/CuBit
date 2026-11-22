------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The RSN element (IEEE 802.11-2020 9.4.2.24): what an AP offers, the
--  policy CuBit joins with, and the element CuBit sends (NET-WIFI-001 W4,
--  docs/wifi.md "Frames and IEs", "MLME state machine").
--
--  @description
--  An RSN element's data: Version (2, = 1), Group Data Cipher Suite (4),
--  Pairwise Cipher Suite Count (2) and list, AKM Suite Count (2) and
--  list, RSN Capabilities (2), then optional PMKID and group management
--  cipher fields this policy ignores. Suites are 00-0F-AC (IEEE OUI) and
--  a type octet. Decoding is total and proved free of run-time errors; a
--  count that runs past the element refuses it.
--
--  Policy (W4): WPA2-Personal only: CCMP-128 pairwise, CCMP-128 or
--  (deprecated, receive-only) TKIP group refused, AKM PSK (2). Anything
--  else is not joined (WPA1, TKIP pairwise, SAE until W5).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Wifi_RSN with Pure, SPARK_Mode is

   Maximum_Octets : constant := 255;     --  an element's data
   subtype Octet_Index is Natural range 0 .. Maximum_Octets;
   type Octets is array (Octet_Index range <>) of Unsigned_8;

   --  Suite types under the IEEE 802.11 OUI 00-0F-AC (9.4.2.24.2, .3).
   OUI_0 : constant Unsigned_8 := 16#00#;
   OUI_1 : constant Unsigned_8 := 16#0F#;
   OUI_2 : constant Unsigned_8 := 16#AC#;
   Cipher_TKIP     : constant Unsigned_8 := 2;
   Cipher_CCMP_128 : constant Unsigned_8 := 4;
   AKM_PSK         : constant Unsigned_8 := 2;
   AKM_PSK_SHA256  : constant Unsigned_8 := 6;
   AKM_SAE         : constant Unsigned_8 := 8;
   RSN_Version     : constant := 1;
   Suite_Bytes     : constant := 4;
   Maximum_Suites  : constant := 8;   --  per list kept; more are refused

   subtype Suite_Count is Natural range 0 .. Maximum_Suites;
   type Suite is record
      IEEE : Boolean := False;        --  OUI 00-0F-AC
      Kind : Unsigned_8 := 0;
   end record;
   type Suite_List is array (1 .. Maximum_Suites) of Suite;

   type Offer is record
      Group      : Suite;
      Pairwise   : Suite_List := [others => (others => <>)];
      Pairwise_Count : Suite_Count := 0;
      AKMs       : Suite_List := [others => (others => <>)];
      AKM_Count  : Suite_Count := 0;
      Capabilities : Unsigned_16 := 0;
   end record;

   type Status is (Decoded, Too_Short, Wrong_Version, Too_Many_Suites, Overrun);

   --  Decode an RSN element's data (without its 2-octet header).
   procedure Decode (Data : Octets; Item : out Offer; Result : out Status)
   with Pre => Data'First = 0 and then Data'Length <= 255;

   function Offers (L : Suite_List; Count : Suite_Count; Kind : Unsigned_8) return Boolean is
     (for some I in 1 .. Count => L (I).IEEE and then L (I).Kind = Kind);

   --  RSN Capabilities (9.4.2.24.4): management frame protection required.
   Capability_MFP_Required : constant Unsigned_16 := 2 ** 6;

   --  Whether CuBit's W4 policy can join this network (no management
   --  frame protection yet, so not one that requires it).
   function Joinable (O : Offer) return Boolean is
     ((O.Capabilities and Capability_MFP_Required) = 0
      and then O.Group.IEEE and then O.Group.Kind = Cipher_CCMP_128
      and then Offers (O.Pairwise, O.Pairwise_Count, Cipher_CCMP_128)
      and then Offers (O.AKMs, O.AKM_Count, AKM_PSK));

   --  The element CuBit sends (Association Request, EAPOL message 2): RSN,
   --  version 1, CCMP group, one CCMP pairwise, one PSK AKM, no
   --  capabilities. 22 octets including the 2-octet header (ID 48, 20).
   Element_ID : constant Unsigned_8 := 48;
   Own_Element_Bytes : constant := 22;
   Own_Element : constant Octets (0 .. Own_Element_Bytes - 1) :=
     [Element_ID, Own_Element_Bytes - 2,
      RSN_Version, 0,
      OUI_0, OUI_1, OUI_2, Cipher_CCMP_128,
      1, 0, OUI_0, OUI_1, OUI_2, Cipher_CCMP_128,
      1, 0, OUI_0, OUI_1, OUI_2, AKM_PSK,
      0, 0];

end Wifi_RSN;
