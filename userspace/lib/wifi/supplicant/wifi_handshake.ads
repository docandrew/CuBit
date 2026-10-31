------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The WPA2-PSK 4-way and group-key handshakes, supplicant side
--  (IEEE 802.11-2020 12.7.6, 12.7.7; NET-WIFI-001 W4, docs/wifi.md
--  "4-way handshake"), over decoded EAPOL-Key frames and WiFi_Crypto.
--
--  @description
--  Message 1 (ANonce) -> derive the PTK with PRF-384 and send message 2
--  (SNonce, CuBit's RSN element, MIC under the KCK). Message 3 is checked
--  in this order: replay counter greater than the last accepted, MIC
--  (constant-time compare), ANonce unchanged, the RSN element equal to
--  the beacon's; then the key data is unwrapped with the KEK (AES key
--  wrap) and the GTK taken; message 4 is sent and the keys are handed out
--  for installation. A group message 1 rekeys the GTK.
--
--  Proved (level 2): no run-time errors; the pairwise key is handed out
--  at most once per handshake (a retransmitted message 3 is answered with
--  message 4 but installs nothing: no key reinstallation, KRACK); every
--  accepted frame's replay counter is greater than the previous one.
--  Tested (hosted): a recorded WPA2 4-way exchange (wpa-Induction.pcap,
--  SSID "Coherer", passphrase "Induction") reproduces the AP-side MICs.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Wifi_EAPOL;

package Wifi_Handshake with SPARK_Mode is
   package E renames Wifi_EAPOL;
   use type E.Replay_Counter;

   Key_Bytes : constant := 16;
   type Key_16 is array (0 .. Key_Bytes - 1) of Unsigned_8;
   PMK_Bytes : constant := 32;
   type PMK_Value is array (0 .. PMK_Bytes - 1) of Unsigned_8;
   type Address is array (0 .. 5) of Unsigned_8;
   Maximum_RSN_Element : constant := 64;
   type RSN_Element is array (0 .. Maximum_RSN_Element - 1) of Unsigned_8;
   subtype RSN_Length is Natural range 0 .. Maximum_RSN_Element;
   Maximum_GTK : constant := 32;
   type GTK_Value is array (1 .. Maximum_GTK) of Unsigned_8;
   subtype GTK_Length is Natural range 0 .. Maximum_GTK;
   subtype Key_ID is Natural range 0 .. 3;

   type Phase is (Waiting_Message_1, Waiting_Message_3, Established, Failed);

   type Session is private;

   --  A new handshake for one association: the PMK (from the key
   --  operations), the authenticator's (AA) and our (SPA) addresses, the
   --  AP's RSN element from its beacon or probe response, and a fresh
   --  SNonce from the system's random bit generator.
   procedure Start (S : out Session; PMK : PMK_Value; AA, SPA : Address;
                    Beacon_RSN : RSN_Element; Beacon_RSN_Bytes : RSN_Length;
                    S_Nonce : E.Nonce)
   with Post => Current (S) = Waiting_Message_1 and then not Pairwise_Handed_Out (S);

   function Current (S : Session) return Phase;
   function Pairwise_Handed_Out (S : Session) return Boolean;
   function Last_Replay (S : Session) return E.Replay_Counter;
   function Replay_Seen (S : Session) return Boolean;

   --  What a frame asks the caller to install.
   type Install_Kind is (Nothing, Pairwise_And_Group, Group_Only);
   type Install_Request is record
      Kind       : Install_Kind := Nothing;
      TK         : Key_16 := [others => 0];
      GTK        : GTK_Value := [others => 0];
      GTK_Bytes  : GTK_Length := 0;
      GTK_Index  : Key_ID := 0;
      GTK_RSC    : E.RSC_Value := [others => 0];
   end record;

   type Result is
     (Replied,            --  Reply holds a message to send (2, 4 or group 2)
      Ignored,            --  not for this state (counted)
      Replay_Refused,     --  replay counter not greater than the last
      MIC_Failed,
      Nonce_Changed,      --  message 3's ANonce differs from message 1's
      RSN_Mismatch,       --  message 3's RSN element differs from the beacon's (downgrade)
      Key_Data_Refused,   --  unwrap failed or no GTK
      Malformed);

   Reply_Bytes : constant := E.Key_Data_At + E.Maximum_Key_Data;
   subtype Reply_Frame is E.Octets (0 .. Reply_Bytes - 1);

   procedure Receive
     (S : in out Session; Frame : E.Octets; Reply : out Reply_Frame;
      Reply_Length : out Natural; Install : out Install_Request; Outcome : out Result)
   with Pre  => Frame'First = 0 and then Frame'Length <= E.Maximum_Frame,
        Post => Reply_Length <= Reply_Bytes
                --  No key reinstallation: the pairwise key leaves once.
                and then (if Pairwise_Handed_Out (S'Old)
                          then Install.Kind /= Pairwise_And_Group)
                and then (if Install.Kind = Pairwise_And_Group
                          then Pairwise_Handed_Out (S) and then not Pairwise_Handed_Out (S'Old))
                --  Accepted frames advance the replay counter.
                and then (if Outcome = Replied and then Replay_Seen (S'Old)
                          then Last_Replay (S) > Last_Replay (S'Old))
                and then (if Outcome /= Replied then Last_Replay (S) = Last_Replay (S'Old)
                          and then Pairwise_Handed_Out (S) = Pairwise_Handed_Out (S'Old));

   --  Wipe every key and nonce (the association ended).
   procedure Clear (S : out Session)
   with Post => Current (S) = Failed and then not Pairwise_Handed_Out (S);

private
   type Session is record
      State      : Phase := Failed;
      PMK        : PMK_Value := [others => 0];
      AA, SPA    : Address := [others => 0];
      Beacon     : RSN_Element := [others => 0];
      Beacon_Bytes : RSN_Length := 0;
      A_Nonce    : E.Nonce := [others => 0];
      S_Nonce    : E.Nonce := [others => 0];
      KCK, KEK, TK : Key_16 := [others => 0];
      Replay     : E.Replay_Counter := 0;
      Seen       : Boolean := False;
      Handed_Out : Boolean := False;
   end record;

   function Current (S : Session) return Phase is (S.State);
   function Pairwise_Handed_Out (S : Session) return Boolean is (S.Handed_Out);
   function Last_Replay (S : Session) return E.Replay_Counter is (S.Replay);
   function Replay_Seen (S : Session) return Boolean is (S.Seen);
end Wifi_Handshake;
