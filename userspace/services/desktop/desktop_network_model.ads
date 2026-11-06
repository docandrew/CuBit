------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The Deskbar's network items (UI-027) as data: when each is shown, the
--  Wi-Fi panel's network list, and joining a network as a state machine
--  the event loop drives (each step names the request Desktop submits
--  next; replies and reports come back as completions, never awaited).
--
--  @description
--  Presence (user, 2026-11-05): the Wi-Fi item shows only while a Wi-Fi
--  adapter is bound to wifi.svc (its report's state is not No_Radio),
--  not merely while wifi.svc runs; the wired item only while netmgr
--  reports an Ethernet interface. Both follow the services' reports, so
--  they appear and go as events.
--
--  Joining: an open network, or a secured one whose passphrase is
--  remembered, is joined at once; a secured one without asks for its
--  passphrase in the row (Entering_Passphrase), which is stored with
--  wifi.svc (remembered by default) before the join. Progress and failure
--  stay in that row: Joining, Waiting_Address (associated, no DHCP lease
--  yet), Connected, or Failed with a reason. A passphrase stored for this
--  attempt that the network refuses is forgotten again, and the field
--  opens once more.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Wifi_Control;
with CuBit.Wifi_Scan;
with CuBit.Wifi_Report;
with CuBit.Net_Link;
with CuBit.Net_Manager_Report;

package Desktop_Network_Model with SPARK_Mode is
   package C renames CuBit.Wifi_Control;
   package WS renames CuBit.Wifi_Scan;
   package WR renames CuBit.Wifi_Report;
   package NMR renames CuBit.Net_Manager_Report;
   use type C.State, C.Failure, C.Refusal, WS.Signal_dBm;
   use type CuBit.Net_Link.Link_Kind;

   ---------------------------------------------------------------------------
   --  Presence
   ---------------------------------------------------------------------------
   function Wifi_Present (Have : Boolean; R : WR.Report) return Boolean is
     (Have and then R.State /= C.No_Radio);

   --  The first interface of Kind in N (0: none).
   function Interface_Of (N : NMR.Report; Kind : CuBit.Net_Link.Link_Kind) return NMR.Record_Count
   with Post => Interface_Of'Result <= N.Count;

   function Wired_Present (Have : Boolean; N : NMR.Report) return Boolean is
     (Have and then Interface_Of (N, CuBit.Net_Link.Ethernet) /= 0);

   ---------------------------------------------------------------------------
   --  Signal strength as bars (0: unusable .. 4: excellent).
   ---------------------------------------------------------------------------
   type Signal_Bars is range 0 .. 4;
   --  The weakest signal for each bar count (dBm, as Linux and Haiku draw
   --  them: -50 excellent, -60 good, -70 fair, -80 weak).
   Excellent_dBm : constant := -50;
   Good_dBm      : constant := -60;
   Fair_dBm      : constant := -70;
   Weak_dBm      : constant := -80;

   function Bars_Of (Signal : WS.Signal_dBm) return Signal_Bars is
     (if Signal >= Excellent_dBm then 4 elsif Signal >= Good_dBm then 3
      elsif Signal >= Fair_dBm then 2 elsif Signal >= Weak_dBm then 1 else 0);

   ---------------------------------------------------------------------------
   --  The network list: one row per SSID (its strongest BSS), hidden
   --  networks left out, the joined one first, then by signal.
   ---------------------------------------------------------------------------
   type Security is (Open, Secured, Unsupported);

   type Network is record
      Name       : C.SSID;
      Signal     : WS.Signal_dBm := WS.Signal_dBm'First;
      Kind       : Security := Open;
      Remembered : Boolean := False;
      Current    : Boolean := False;   --  joined or being joined
   end record;

   Maximum_Networks : constant := WS.Maximum_Entries;
   subtype Network_Count is Natural range 0 .. Maximum_Networks;
   subtype Network_Index is Positive range 1 .. Maximum_Networks;
   type Network_List is array (Network_Index) of Network;

   function Same (A, B : C.SSID) return Boolean is
     (A.Length = B.Length and then (for all I in 1 .. A.Length => A.Octets (I) = B.Octets (I)));

   function Name_Of (E : WS.BSS_Entry) return C.SSID
   with Post => Name_Of'Result.Length = E.Length;

   procedure Build_List (R : WR.Report; List : out Network_List; Count : out Network_Count)
   with Pre => WR.Normal (R);

   --  Printable text for an SSID (other octets as '?').
   function Text_Of (S : C.SSID) return String
   with Post => Text_Of'Result'Length = S.Length;

   ---------------------------------------------------------------------------
   --  Joining
   ---------------------------------------------------------------------------
   type Phase is
     (Idle, Entering_Passphrase, Storing, Starting, Joining, Waiting_Address, Connected, Failed);

   type Failure is
     (None, Wrong_Passphrase, Out_Of_Range, No_Address, Refused, Not_Supported, Unavailable,
      Disconnected);

   --  What Desktop submits next.
   type Request is (Nothing, Store_Passphrase, Join, Forget);

   --  How long an associated network may take to lease an address before
   --  the row says so (it keeps waiting; a later lease still connects).
   Address_Wait_Ms : constant := 15_000;

   type Join_State is record
      Current    : Phase := Idle;
      Target     : C.SSID;
      Why        : Failure := None;
      Stored_Now : Boolean := False;   --  this attempt stored its passphrase
      Seen_Handshake : Boolean := False; --  wifi.svc reported its 4-way handshake
      Since      : Unsigned_64 := 0;   --  when Waiting_Address began (ms)
   end record;

   function Busy (J : Join_State) return Boolean is
     (J.Current in Storing | Starting);

   --  A press on a network's row (or Enter on it).
   procedure Choose (J : in out Join_State; N : Network; Next : out Request)
   with Post => (if Next = Join then J.Current = Starting)
                and then (if N.Kind = Unsupported then Next = Nothing);

   --  Enter in the passphrase field, holding Length characters.
   procedure Passphrase_Entered (J : in out Join_State; Length : Natural; Next : out Request)
   with Post => (if Next = Store_Passphrase then
                   J.Current = Storing and then Length in C.Passphrase_Length);

   --  Escape in the field, or the row closed.
   procedure Cancel (J : in out Join_State)
   with Post => (if J'Old.Current in Entering_Passphrase | Failed then J.Current = Idle);

   --  A submitted request finished: OK, or refused with Why_Not (Delivered
   --  False: wifi.svc never answered).
   procedure Finished (J : in out Join_State; Done : Request; Delivered, OK : Boolean;
                       Why_Not : C.Refusal; Next : out Request);

   --  A new report from wifi.svc, and the Wi-Fi interface's address from
   --  netmgr (No address: 0), at monotonic time Now.
   procedure Observe (J : in out Join_State; R : WR.Report; Address : Unsigned_32;
                      Now : Unsigned_64; Next : out Request);

   --  Time passing: an address wait that ran out.
   procedure Tick (J : in out Join_State; Now : Unsigned_64);

   --  The row's words for this state ("" when there is nothing to say).
   function Status_Text (J : Join_State) return String;

   function Failure_Text (F : Failure) return String is
     (case F is
         when None             => "",
         when Wrong_Passphrase => "Wrong passphrase",
         when Out_Of_Range     => "Out of range",
         when No_Address       => "No address (DHCP)",
         when Refused          => "The network refused",
         when Not_Supported    => "Not supported (WEP/WPA1)",
         when Unavailable      => "Wi-Fi unavailable",
         when Disconnected     => "Disconnected");

   --  Why a join that wifi.svc ended with F failed.
   function Failure_From (F : C.Failure; Handshaking : Boolean) return Failure is
     (case F is
         when C.Handshake_Failed | C.Handshake_Timeout => Wrong_Passphrase,
         when C.Authentication_Timeout | C.Association_Timeout => Out_Of_Range,
         when C.Authentication_Refused | C.Association_Refused => Refused,
         when C.Deauthenticated | C.Disassociated =>
           (if Handshaking then Wrong_Passphrase else Disconnected),
         when C.None | C.Left => Disconnected);
end Desktop_Network_Model;
