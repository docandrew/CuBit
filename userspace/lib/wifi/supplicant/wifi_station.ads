------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  wifi.svc's station: one join at a time, from the choice of a BSS to an
--  open 802.1X port (NET-WIFI-001 W3/W4, docs/wifi.md "MLME state
--  machine", "4-way handshake"). It composes the proved pieces (Wifi_MLME,
--  Wifi_Handshake, IEEE80211_Management, Wifi_RSN, IEEE80211_Beacons) and
--  has no I/O: every event goes in as a call, and what to do comes out as
--  a queue of Outputs (frames to send, commands for the driver) that the
--  service's I/O shell performs in order.
--
--  @description
--  Join (BSS from the scan) -> Do_Join; the BSS's next beacon or probe
--  response gives its rates, capability and, for WPA2, its RSN element,
--  which must be joinable (Wifi_RSN.Joinable) -> authentication ->
--  association -> Do_Associated (AID) -> for an open network Do_Port Open;
--  for WPA2 the 4-way handshake: its replies go out as EAPOL frames, its
--  keys as Do_Install_Key (the pairwise key once: no reinstallation), then
--  Do_Port Open. A failure, deauthentication, disassociation or Leave ends
--  in Do_Port Closed (when it was open) and Do_Leave.
--
--  Key custody: the PMK, KCK and KEK stay in here (wifi.svc); only the
--  temporal keys leave, in Do_Install_Key, for the driver's hardware.
--
--  Tested (hosted, tests/wifi-station): the recorded WPA2 join from
--  wpa-Induction.pcap replayed end to end. Proved: no run-time errors.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Net_Link;
with CuBit.Wifi_Scan;
with CuBit.Wifi_Driver;
with CuBit.Wifi_Control;
with Wifi_MLME;
with Wifi_Handshake;
with Wifi_EAPOL;

package Wifi_Station with SPARK_Mode is
   package NL renames CuBit.Net_Link;
   package WS renames CuBit.Wifi_Scan;
   package D renames CuBit.Wifi_Driver;
   package C renames CuBit.Wifi_Control;
   package M renames Wifi_MLME;
   package H renames Wifi_Handshake;
   package E renames Wifi_EAPOL;

   Maximum_Frame : constant := 1_024;
   subtype Frame_Index is Natural range 0 .. Maximum_Frame;
   type Frame_Octets is array (Frame_Index range <>) of Unsigned_8;
   subtype Frame_Buffer is Frame_Octets (0 .. Maximum_Frame - 1);

   type Output_Kind is
     (Send_Management,     --  Frame (0 .. Length - 1): an 802.11 management frame
      Send_EAPOL,          --  Frame (0 .. Length - 1): an EAPOL PDU to the BSS
      Do_Join, Do_Associated, Do_Install_Key, Do_Port, Do_Leave);

   type Output is record
      Kind   : Output_Kind := Do_Leave;
      Frame  : Frame_Buffer := [others => 0];
      Length : Frame_Index := 0;
      Join   : D.Join_Request;
      AID    : D.Association_ID := 1;
      Interval : Unsigned_16 := 0;   --  Do_Associated: the BSS's beacon interval (TU)
      Key    : D.Key_Install;
      Port   : D.Port_State := D.Closed;
   end record;

   Maximum_Outputs : constant := 8;
   subtype Output_Count is Natural range 0 .. Maximum_Outputs;
   type Output_List is array (1 .. Maximum_Outputs) of Output;
   type Outputs is record
      Count : Output_Count := 0;
      Items : Output_List;
   end record;

   type Security is (Open, WPA2_PSK);

   type Station is private;

   --  The join state machine's invariant holds (Wifi_MLME.Invariant).
   function Valid (S : Station) return Boolean;

   --  Our address, and the limits joins run under (no hidden defaults).
   procedure Reset (S : out Station; Own : NL.Hardware_Address; Bounds : M.Limits;
                    Beacon_Timeout : M.Milliseconds)
   with Post => Valid (S);

   --  Start a join to Target (from the scan). For WPA2_PSK, PMK and a
   --  fresh S_Nonce. Refused (with a reason) when one is under way.
   procedure Join (S : in out Station; Target : WS.BSS_Entry; Kind : Security;
                   PMK : H.PMK_Value; S_Nonce : E.Nonce; Now : M.Milliseconds;
                   Out_List : out Outputs; Accepted : out Boolean)
   with Pre => Valid (S) and then WS.Normal (Target), Post => Valid (S);

   type Frame_Bytes is array (Natural range <>) of Unsigned_8;

   --  A management frame from the driver (from the BSS being joined).
   procedure Management (S : in out Station; Frame : Frame_Bytes; Now : M.Milliseconds;
                         Out_List : out Outputs)
   with Pre => Valid (S) and then Frame'First = 0 and then Frame'Length <= 4_096, Post => Valid (S);

   --  An EAPOL PDU from the BSS.
   procedure EAPOL (S : in out Station; Frame : Frame_Bytes; Now : M.Milliseconds;
                    Out_List : out Outputs)
   with Pre => Valid (S) and then Frame'First = 0 and then Frame'Length <= E.Maximum_Frame,
        Post => Valid (S);

   procedure Tick (S : in out Station; Now : M.Milliseconds; Out_List : out Outputs)
   with Pre => Valid (S), Post => Valid (S);

   procedure Leave (S : in out Station; Now : M.Milliseconds; Out_List : out Outputs)
   with Pre => Valid (S), Post => Valid (S);

   --  The driver reported a radio fault for a command: the join ends.
   procedure Radio_Fault (S : in out Station; Now : M.Milliseconds; Out_List : out Outputs)
   with Pre => Valid (S), Post => Valid (S);

   function State_Of (S : Station) return C.State;
   function Last_Failure (S : Station) return M.Failure;
   function Target_Of (S : Station) return WS.BSS_Entry;
   --  When Tick next needs to run (0: no deadline).
   function Deadline (S : Station) return M.Milliseconds;

   --  Counters for the service's log.
   type Counter is mod 2 ** 32;
   --  What became of the last EAPOL frame (for wifi.svc's log).
   type EAPOL_Note is
     (None, Not_Expected, Sent_Message_2, Sent_Message_4, Sent_Group_2, Ignored, Replay_Refused,
      MIC_Failed, Nonce_Changed, RSN_Mismatch, Key_Data_Refused, Malformed, Group_Cipher_Refused);
   type Statistics is record
      EAPOL_Refused : Counter := 0;   --  MIC, replay, malformed (not fatal)
      Ignored_Frames : Counter := 0;
      EAPOL_Frames : Counter := 0;
      Last_EAPOL : EAPOL_Note := None;
   end record;
   function Counts (S : Station) return Statistics;

private
   type Wait_Phase is (None, Waiting_Beacon, In_MLME);

   type Station is record
      Own        : NL.Hardware_Address := NL.No_Address;
      Bounds     : M.Limits;
      Beacon_Timeout : M.Milliseconds := 0;
      Phase      : Wait_Phase := None;
      Beacon_Deadline : M.Milliseconds := 0;
      Target     : WS.BSS_Entry;
      Kind       : Security := Open;
      PMK        : H.PMK_Value := [others => 0];
      S_Nonce    : E.Nonce := [others => 0];
      Machine    : M.State := M.Initial;
      Session    : H.Session;
      Rates      : Frame_Buffer := [others => 0];
      Rates_Length : Natural range 0 .. 255 := 0;
      Extended   : Frame_Buffer := [others => 0];
      Extended_Length : Natural range 0 .. 255 := 0;
      Beacon_RSN : H.RSN_Element := [others => 0];
      Beacon_RSN_Length : H.RSN_Length := 0;
      Capability : Unsigned_16 := 0;
      AID        : D.Association_ID := 1;
      Port_Open  : Boolean := False;
      Failure    : M.Failure := M.None;
      Count      : Statistics;
   end record;

   function State_Of (S : Station) return C.State is
     (case S.Phase is
        when None => C.Idle,
        when Waiting_Beacon => C.Joining,
        when In_MLME =>
          (case S.Machine.Current is
             when M.Idle => C.Idle,
             when M.Authenticating | M.Associating => C.Joining,
             when M.Handshaking => C.Handshaking,
             when M.Connected => C.Connected));
   function Last_Failure (S : Station) return M.Failure is (S.Failure);
   function Target_Of (S : Station) return WS.BSS_Entry is (S.Target);
   function Deadline (S : Station) return M.Milliseconds is
     (case S.Phase is
        when None => 0,
        when Waiting_Beacon => S.Beacon_Deadline,
        when In_MLME => S.Machine.Deadline);
   function Counts (S : Station) return Statistics is (S.Count);
   function Valid (S : Station) return Boolean is (M.Invariant (S.Machine));
end Wifi_Station;
