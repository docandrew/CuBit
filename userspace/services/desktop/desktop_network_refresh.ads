------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The Deskbar's network items talking to wifi.svc and netmgr (UI-027),
--  never waiting on either: each request is submitted and its reply taken
--  as a completion in the event loop (the IPC graph stays a DAG; a desktop
--  that blocks on a peer is a BUG-001 freeze suspect).
--
--  @description
--  Each service gets one parked Watch, its report written into a page
--  Desktop lends it (CuBit.Wifi_Report, CuBit.Net_Manager_Report); every
--  completion re-submits the watch with the generation seen, so a change
--  reaches Desktop as an event. Flights are CuBit.Async_Requests trackers
--  and every item is a Desktop_Status_Machines machine: a watch that is
--  refused or not delivered hides its item, revokes its page and is tried
--  again later on a new one; no lent page is written or lent again until
--  the kernel confirms its grant retired.
--
--  Commands are one flight per service. A Wi-Fi passphrase is copied from
--  Desktop's field straight into a page lent read-only to wifi.svc for
--  that one Set_Passphrase; the field is wiped at once, the page when the
--  reply comes (whatever it says). It is never logged. An addressing
--  change is netmgr's Apply, then its Set_DNS when the DNS servers
--  changed; the result is reported once both are done.
------------------------------------------------------------------------------
with Interfaces;
with CuBit.Messages;
with CuBit.Wifi_Control;
with CuBit.Wifi_Report;
with CuBit.Net_Manager_Report;
with CuBit.UI.Editor;

package Desktop_Network_Refresh is
   use type Interfaces.Unsigned_64;
   package C renames CuBit.Wifi_Control;

   --  Desktop holds each service's endpoint (its manifest's optional
   --  "wifi" and "netmgr" entries).
   function Wifi_Granted return Boolean;
   function Net_Granted return Boolean;

   --  Keep each watch submitted (a failed one again after
   --  Desktop_Status_Machines.Retry_Ms, on a freshly lent page) and take
   --  back pages whose grants the kernel confirms retired. Now: monotonic
   --  milliseconds.
   procedure Start (Now : Interfaces.Unsigned_64; Sequence : in out Interfaces.Unsigned_64);

   function Matches (Token : Interfaces.Unsigned_64) return Boolean;
   procedure Collect (C_Entry : CuBit.Messages.CompletionEntry;
                      Sequence : in out Interfaces.Unsigned_64);

   --  The newest reports. Have: one was ever decoded and its watch has not
   --  ended; Fresh is cleared by Take_*.
   procedure Take_Wifi (Report : out CuBit.Wifi_Report.Report; Have, Fresh : out Boolean);
   procedure Take_Net (Report : out CuBit.Net_Manager_Report.Report; Have, Fresh : out Boolean);

   ---------------------------------------------------------------------------
   --  Wi-Fi commands (one in flight).
   ---------------------------------------------------------------------------
   type Wifi_Command is (None, Store_Passphrase, Join, Forget, Refresh);
   function Wifi_Free return Boolean;

   --  Hand Field's text (a Sensitive toolkit field, Desktop's own) to
   --  wifi.svc as Name's passphrase; Field is wiped.
   procedure Store_Passphrase (Name : C.SSID; Field : in out CuBit.UI.Editor.Edit_State;
                               Sequence : in out Interfaces.Unsigned_64)
   with Post => CuBit.UI.Editor.Length (Field) = 0;
   procedure Join (Name : C.SSID; Sequence : in out Interfaces.Unsigned_64);
   procedure Forget (Name : C.SSID; Sequence : in out Interfaces.Unsigned_64);
   --  Ask wifi.svc to look at the driver's scan (the panel's low-rate
   --  refresh while the network list is open).
   procedure Refresh (Sequence : in out Interfaces.Unsigned_64);

   type Wifi_Result is record
      Done      : Wifi_Command := None;
      Delivered : Boolean := False;
      OK        : Boolean := False;
      Why_Not   : C.Refusal := C.Malformed;
   end record;
   --  The last finished command (Done = None: nothing new).
   procedure Take_Wifi_Result (Result : out Wifi_Result);

   ---------------------------------------------------------------------------
   --  netmgr commands (one in flight).
   ---------------------------------------------------------------------------
   function Net_Free return Boolean;

   --  Apply Source (a Net_Interface's CCL) to the interface Name; then,
   --  when DNS_Changed, set the DNS servers (DNS_1 = 0: back to the
   --  settings and leases).
   procedure Apply (Name, Source : String; DNS_Changed : Boolean;
                    DNS_1, DNS_2 : Interfaces.Unsigned_32;
                    Sequence : in out Interfaces.Unsigned_64)
   with Pre => Name'Length in 1 .. 16 and then Source'Length in 1 .. 1_024;

   type Net_Result is record
      Fresh     : Boolean := False;
      Delivered : Boolean := False;
      OK        : Boolean := False;
   end record;
   procedure Take_Net_Result (Result : out Net_Result);
end Desktop_Network_Refresh;
