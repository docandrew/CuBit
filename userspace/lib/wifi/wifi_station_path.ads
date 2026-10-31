------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Where a frame heard by a joined station goes (NET-WIFI-001 W3/W4): the
--  driver's receive classification, chip-independent, with no I/O.
--
--  @description
--  Management frames from the BSS being joined (its next beacon or probe
--  response once, then authentication, association, deauthentication and
--  disassociation) go to wifi.svc. Data frames are decoded
--  (IEEE80211_Data): EAPOL goes to wifi.svc whether or not the 802.1X
--  port is open; anything else goes to netstack as an Ethernet frame, only
--  while the port is open. A frame the hardware should have decrypted and
--  did not is dropped, and a decrypted one only passes with its packet
--  number above the last accepted for its key and traffic class
--  (Wifi_Replay). Every drop has a reason, for the driver's health line.
--
--  What leaves (EAPOL_PDU, Ethernet) is a fresh array indexed from 0: a
--  slice of the received frame is never handed on (W3's first hardware
--  run lost message 1 to exactly that).
--
--  Proved (level 2): no run-time errors; a payload range lies inside the
--  frame; the arrays returned are indexed from 0 and as long as the range.
------------------------------------------------------------------------------
pragma Ada_2022;

with IEEE80211_Data;
with Wifi_Replay;

package Wifi_Station_Path with SPARK_Mode is
   package DT renames IEEE80211_Data;
   use type DT.Refusal;
   use type DT.Address;

   subtype Octets is DT.Octets;
   subtype Address is DT.Address;

   type Drop_Reason is
     (Not_Joined, Too_Short, Other_BSS, Other_Management, Beacon_Already_Forwarded,
      Data_Refused, Own_Echo, Not_Decrypted, Unknown_Group_Key, Replayed, Port_Closed,
      Other_Type);

   type Verdict_Kind is (Drop, To_Wifi_Management, To_Wifi_EAPOL, To_Netstack);

   type Verdict is record
      Kind     : Verdict_Kind := Drop;
      Why      : Drop_Reason := Not_Joined;
      Refusal  : DT.Refusal := DT.None;   --  Data_Refused: the decoder's reason
      Data     : DT.Received;             --  To_Wifi_EAPOL, To_Netstack
   end record;

   subtype Key_ID is Natural range 0 .. 3;
   type Group_Counters is array (Key_ID) of Wifi_Replay.Counters;
   type Group_Keys is array (Key_ID) of Boolean;

   type Link is record
      Joined          : Boolean := False;
      BSSID           : Address := [others => 0];
      Own             : Address := [others => 0];
      Forward_Beacon  : Boolean := False;
      Port_Open       : Boolean := False;
      Pairwise_Installed : Boolean := False;
      Pairwise_Replay : Wifi_Replay.Counters;
      Group_Installed : Group_Keys := [others => False];
      Group_Replay    : Group_Counters;
   end record;

   procedure Classify (L : in out Link; Frame : Octets; Decrypted, Decrypt_Failed : Boolean;
                       V : out Verdict)
   with Pre  => Frame'First = 0,
        Post => (if V.Kind in To_Wifi_EAPOL | To_Netstack then
                   V.Data.Why = DT.None
                   and then V.Data.Payload_First + V.Data.Payload_Length <= Frame'Length);

   Ethernet_Header : constant := 14;

   --  The EAPOL PDU of a To_Wifi_EAPOL verdict, indexed from 0.
   function EAPOL_PDU (Frame : Octets; V : Verdict) return Octets
   with Pre  => Frame'First = 0 and then V.Data.Payload_First + V.Data.Payload_Length <= Frame'Length,
        Post => EAPOL_PDU'Result'First = 0 and then EAPOL_PDU'Result'Length = V.Data.Payload_Length;

   --  The Ethernet frame of a To_Netstack verdict (destination, source,
   --  EtherType, payload), indexed from 0.
   function Ethernet (Frame : Octets; V : Verdict) return Octets
   with Pre  => Frame'First = 0 and then V.Data.Payload_First + V.Data.Payload_Length <= Frame'Length
                and then V.Data.Payload_Length <= DT.Maximum_Octets - Ethernet_Header,
        Post => Ethernet'Result'First = 0
                and then Ethernet'Result'Length = Ethernet_Header + V.Data.Payload_Length;

end Wifi_Station_Path;
