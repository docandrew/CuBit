------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The station's join state machine (IEEE 802.11-2020 11.3; NET-WIFI-001
--  W3/W4, docs/wifi.md "MLME state machine"): a pure function Step
--  (State, Event) -> (State, Actions). wifi.svc's I/O shell performs the
--  actions.
--
--  @description
--  Idle -> Authenticating (Open System) -> Associating -> Connected for an
--  open network, or -> Handshaking (the 4-way, Wifi_Handshake) ->
--  Connected for WPA2-PSK. Every waiting state has a deadline and a retry
--  limit the caller passes with Join (no hidden defaults); exhausting them
--  returns to Idle with a typed reason. Deauthentication or
--  disassociation from the BSS, or Leave, returns to Idle from anywhere.
--
--  Proved (level 2): no run-time errors; the 802.1X controlled port is
--  open exactly in Connected (the only action that opens it is emitted on
--  entering Connected, and every transition out of Connected closes it);
--  every state but Idle and Connected has a deadline; an RSN network is
--  never Connected without an established handshake.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Wifi_MLME with Pure, SPARK_Mode is

   subtype Milliseconds is Unsigned_64;
   subtype Attempts is Natural range 0 .. 16;

   type Security is (Open, WPA2_PSK);

   type Phase is (Idle, Authenticating, Associating, Handshaking, Connected);

   type Failure is
     (None, Authentication_Refused, Authentication_Timeout, Association_Refused,
      Association_Timeout, Handshake_Failed, Handshake_Timeout, Deauthenticated,
      Disassociated, Left);

   --  The limits a join runs under (from wifi.svc's settings).
   type Limits is record
      Frame_Timeout     : Milliseconds := 0;   --  per authentication/association attempt
      Frame_Attempts    : Attempts := 0;
      Handshake_Timeout : Milliseconds := 0;
   end record;

   type State is record
      Current   : Phase := Idle;
      Kind      : Security := Open;
      Bounds    : Limits;
      Deadline  : Milliseconds := 0;
      Tries     : Attempts := 0;
      Port_Open : Boolean := False;
      Reason    : Failure := None;       --  why the last join ended
   end record;

   function Invariant (S : State) return Boolean is
     (S.Port_Open = (S.Current = Connected)
      and then (if S.Current in Authenticating | Associating then S.Tries in 1 .. S.Bounds.Frame_Attempts));

   Initial : constant State := (others => <>);

   type Event_Kind is
     (Join, Authentication_Answer, Association_Answer, Handshake_Established,
      Handshake_Refused, Tick, Deauthentication, Disassociation, Leave);

   type Event is record
      Kind     : Event_Kind := Tick;
      Now      : Milliseconds := 0;
      Network  : Security := Open;          --  Join
      Bounds   : Limits;                    --  Join
      Success  : Boolean := False;          --  answers: status 0 (and the right sequence)
   end record;

   --  What the I/O shell does, in order.
   type Action_Kind is
     (Send_Authentication, Send_Association, Start_Handshake, Stop_Handshake,
      Open_Port, Close_Port, Report_Connected, Report_Failure);
   Maximum_Actions : constant := 4;
   subtype Action_Count is Natural range 0 .. Maximum_Actions;
   type Action_List is array (1 .. Maximum_Actions) of Action_Kind;
   type Actions is record
      Count : Action_Count := 0;
      Items : Action_List := [others => Report_Failure];
   end record;

   function Opens (A : Actions) return Boolean is
     (for some I in 1 .. A.Count => A.Items (I) = Open_Port);

   procedure Step (S : in out State; E : Event; Do_Now : out Actions)
   with Pre  => Invariant (S),
        Post => Invariant (S)
                --  The port opens only on entering Connected.
                and then (if Opens (Do_Now) then S.Current = Connected and then S'Old.Current /= Connected)
                --  An RSN network reaches Connected only from Handshaking.
                and then (if S.Current = Connected and then S'Old.Current /= Connected
                          then (if S.Kind = WPA2_PSK then S'Old.Current = Handshaking));

end Wifi_MLME;
