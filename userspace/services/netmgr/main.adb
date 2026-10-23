------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Userspace Network Manager service (netmgr.svc).
--
--  Owns IP configuration policy: reads net.* config keys from config.svc,
--  sends OP_NET_CONFIGURE / OP_NET_SET_DNS to netstack, and runs DHCP
--  for interfaces configured with method=dhcp.
--
--  Startup capabilities (its manifest; devmgr answers them at launch):
--    netstack     -> netstack, badged as the network manager (config + raw UDP)
--    config       -> config.svc (read net.* keys)
--    registration -> DRIVER_NETMGR registration
--    ready        -> devmgr (OP_READY signal)
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Net_Address;
with CuBit.Settings;
with CuBit.Settings.Readable;
with CuBit.Handles;
with DHCP;
with Startup;

procedure main is
   use ASCII;

   --  Protocol constants
   PROTO_UDP : constant Unsigned_8 := 17;

   --  IPC labels
   OP_NET_CONFIGURE : constant Unsigned_32 := 16#0430#;
   OP_NET_SET_DNS   : constant Unsigned_32 := 16#0432#;
   OP_NET_LIST_IF   : constant Unsigned_32 := 16#0435#;
   OP_NET_OPEN_RAW  : constant Unsigned_32 := 16#0426#;
   OP_READY         : constant Unsigned_32 := 16#FF00#;
   REPLY_OK         : constant Unsigned_32 := 16#F000#;
   REPLY_ERR        : constant Unsigned_32 := 16#F001#;

   --  netstack's endpoint, or the null handle without netstack (calls
   --  through it fail).
   function Netstack return CuBit.Handles.Endpoint is
     (if Startup.Netstack.Present then Startup.Netstack.Value
      else CuBit.Handles.No_Endpoint);

   --  Interface method
   type IfMethod is (METHOD_NONE, METHOD_STATIC, METHOD_DHCP);

   --  Per-interface config from system.ccl
   type IfConfig is record
      method  : IfMethod := METHOD_NONE;
      address : Unsigned_64 := 0;
      netmask : Unsigned_64 := 0;
      gateway : Unsigned_64 := 0;
   end record;

   ifCfg : IfConfig;

   --  DNS config
   primaryDNS : Unsigned_64 := 0;
   secondaryDNS : Unsigned_64 := 0;

   --  DHCP state (per interface)
   dhcpState : DHCP.DHCPState;


   ---------------------------------------------------------------------------
   --  An address setting (one 16-byte address, IPv4 as ::ffff:a.b.c.d) in
   --  the netstack protocol's IPv4 word (a | b << 8 | c << 16 | d << 24);
   --  0 when absent, refused, or not IPv4 (the configure protocol carries
   --  IPv4 only so far).
   ---------------------------------------------------------------------------
   generic
      with package Setting is new CuBit.Settings.Readable (<>);
      with function Address_Of (Item : Setting.Value) return CuBit.Net_Address.Address;
   function IPv4_Word return Unsigned_64;
   function IPv4_Word return Unsigned_64 is
      use all type CuBit.Settings.Outcome;
      Stored : constant Setting.Reading := Setting.Read;
   begin
      if Stored.Kind /= Present then
         if Stored.Kind /= Absent then
            debugPrint ("netmgr: " & Setting.Setting_Key & " " & CuBit.Settings.Outcome'Image (Stored.Kind) & LF);
         end if;
         return 0;
      end if;
      declare
         A : constant CuBit.Net_Address.Address := Address_Of (Stored.Item);
      begin
         if not CuBit.Net_Address.Is_Mapped (A) then
            debugPrint ("netmgr: " & Setting.Setting_Key & " is not an IPv4 address; IPv6 is not configured yet" & LF);
            return 0;
         end if;
         return Unsigned_64 (A (12)) or Shift_Left (Unsigned_64 (A (13)), 8) or
                Shift_Left (Unsigned_64 (A (14)), 16) or Shift_Left (Unsigned_64 (A (15)), 24);
      end;
   end IPv4_Word;
   function Same (Item : CuBit.Net_Address.Address) return CuBit.Net_Address.Address is (Item);
   function Interface_Address is new IPv4_Word (Startup.Settings.Net_If0_Address, Same);
   function Interface_Netmask is new IPv4_Word (Startup.Settings.Net_If0_Netmask, Same);
   function Interface_Gateway is new IPv4_Word (Startup.Settings.Net_If0_Gateway, Same);
   function Primary_DNS is new IPv4_Word (Startup.Settings.Net_Dns_Primary, Same);
   function Secondary_DNS is new IPv4_Word (Startup.Settings.Net_Dns_Secondary, Same);

   ---------------------------------------------------------------------------
   --  sendConfigure - send OP_NET_CONFIGURE to netstack
   ---------------------------------------------------------------------------
   procedure sendConfigure (ifIdx   : Unsigned_64;
                            addr    : Unsigned_64;
                            mask    : Unsigned_64;
                            gw      : Unsigned_64)
   is
      msg : Message;
   begin
      msg := (tag      => (label  => OP_NET_CONFIGURE,
                           length => 4,
                           protocol  => 0, ipc => <>,
                           reserved  => 0),
              authorityTag => 0,
              words    => (0 => ifIdx,
                           1 => addr,
                           2 => mask,
                           3 => gw));
      msg.tag := capCall (Netstack, msg, CuBit.Messages.Wait_Forever);

      if msg.tag.label = REPLY_OK then
         debugPrint ("netmgr: configured interface ");
      else
         debugPrint ("netmgr: configure failed for interface ");
      end if;
   end sendConfigure;

   ---------------------------------------------------------------------------
   --  sendSetDNS - send OP_NET_SET_DNS to netstack
   ---------------------------------------------------------------------------
   procedure sendSetDNS (primary : Unsigned_64;
                         secondary : Unsigned_64 := 0)
   is
      msg : Message;
   begin
      msg := (tag      => (label  => OP_NET_SET_DNS,
                           length => 2,
                           protocol  => 0, ipc => <>,
                           reserved  => 0),
              authorityTag => 0,
              words    => (0 => primary,
                           1 => secondary,
                           others => 0));
      msg.tag := capCall (Netstack, msg, CuBit.Messages.Wait_Forever);
   end sendSetDNS;

   ---------------------------------------------------------------------------
   --  signalReady - tell devmgr we are ready
   ---------------------------------------------------------------------------
   procedure signalReady is
      msg    : Message;
      ignore : MessageTag;
   begin
      msg := (tag      => (label  => OP_READY,
                           length => 0,
                           protocol  => 0, ipc => <>,
                           reserved  => 0),
              authorityTag => 0,
              words    => (others => 0));
      ignore := capCall (Startup.Ready, msg, CuBit.Messages.Wait_Forever);
   end signalReady;

   ---------------------------------------------------------------------------
   --  queryInterfaces - ask netstack how many interfaces are attached
   ---------------------------------------------------------------------------
   function queryInterfaces return Natural is
      msg : Message;
   begin
      msg := (tag      => (label  => OP_NET_LIST_IF,
                           length => 0,
                           protocol  => 0, ipc => <>,
                           reserved  => 0),
              authorityTag => 0,
              words    => (others => 0));
      msg.tag := capCall (Netstack, msg, CuBit.Messages.Wait_Forever);
      if msg.tag.label = REPLY_OK then
         return Natural (msg.words (0));
      end if;
      return 0;
   end queryInterfaces;

   ---------------------------------------------------------------------------
   --  openRawChannel - open raw UDP channel for DHCP on an interface
   ---------------------------------------------------------------------------
   function openRawChannel (ifIdx : Unsigned_64;
                            port  : Unsigned_16) return Boolean
   is
      msg : Message;
   begin
      msg := (tag      => (label  => OP_NET_OPEN_RAW,
                           length => 3,
                           protocol  => 0, ipc => <>,
                           reserved  => 0),
              authorityTag => 0,
              words    => (0 => ifIdx,
                           1 => Unsigned_64 (PROTO_UDP),
                           2 => Unsigned_64 (port),
                           others => 0));
      msg.tag := capCall (Netstack, msg, CuBit.Messages.Wait_Forever);
      return msg.tag.label = REPLY_OK;
   end openRawChannel;

   --  Variables for main loop
   ret    : Unsigned_64;
   ifCount : Natural;
   sender : Process_ID;
   msg    : Message;

begin
   debugPrint ("netmgr: starting" & LF);

   --  Register as DRIVER_NETMGR
   ret := registerDriver (DRIVER_NETMGR, Startup.Registration);
   debugPrint ("netmgr: registered" & LF);

   --  net.if0.method: "static" or "dhcp".
   declare
      use all type CuBit.Settings.Outcome;
      Method : constant Startup.Settings.Net_If0_Method.Reading := Startup.Settings.Net_If0_Method.Read;
   begin
      if Method.Kind /= Present then
         debugPrint ("netmgr: net.if0.method " & CuBit.Settings.Outcome'Image (Method.Kind) & LF);
      elsif CuBit.Settings.Image (Method.Item) = "static" then
         ifCfg.method := METHOD_STATIC;
         debugPrint ("netmgr: if0 method=static" & LF);
      elsif CuBit.Settings.Image (Method.Item) = "dhcp" then
         ifCfg.method := METHOD_DHCP;
         debugPrint ("netmgr: if0 method=dhcp" & LF);
      else
         debugPrint ("netmgr: if0 unknown method" & LF);
      end if;
   end;

   --  Read static config values (used as fallback even for DHCP)
   ifCfg.address := Interface_Address;
   ifCfg.netmask := Interface_Netmask;
   ifCfg.gateway := Interface_Gateway;
   primaryDNS    := Primary_DNS;
   secondaryDNS  := Secondary_DNS;

   --  Signal ready to devmgr
   signalReady;
   debugPrint ("netmgr: signaled ready" & LF);

   --  Wait briefly for netstack to have an interface attached
   --  (virtio-net driver should have already attached by now)
   ifCount := queryInterfaces;
   debugPrint ("netmgr: netstack reports interfaces" & LF);

   --  Apply configuration
   if ifCfg.method = METHOD_STATIC then
      if ifCfg.address /= 0 then
         sendConfigure (0, ifCfg.address, ifCfg.netmask, ifCfg.gateway);
         debugPrint ("netmgr: static config applied" & LF);
      else
         debugPrint ("netmgr: no static address configured" & LF);
      end if;

      if primaryDNS /= 0 then
         sendSetDNS (primaryDNS, secondaryDNS);
         debugPrint ("netmgr: DNS configured" & LF);
      end if;

   elsif ifCfg.method = METHOD_DHCP then
      --  Initialize DHCP state machine for interface 0
      DHCP.init (dhcpState, 0);

      --  Open raw UDP channel on port 68 (DHCP client) via netstack
      if openRawChannel (0, 68) then
         debugPrint ("netmgr: raw channel open for DHCP" & LF);

         --  Send DHCP DISCOVER
         DHCP.sendDiscover (dhcpState, Netstack);
         debugPrint ("netmgr: DHCP DISCOVER sent" & LF);
      else
         debugPrint ("netmgr: failed to open raw channel" & LF);
         --  Fall back to static config if available
         if ifCfg.address /= 0 then
            sendConfigure (0, ifCfg.address, ifCfg.netmask, ifCfg.gateway);
            if primaryDNS /= 0 then
               sendSetDNS (primaryDNS, secondaryDNS);
            end if;
         end if;
      end if;
   end if;

   --  Main loop: handle DHCP renewals and runtime reconfiguration
   loop
      receive (sender, msg);

      --  Handle DHCP packets forwarded from netstack (raw channel)
      if msg.tag.label = 16#0401# then
         --  OP_NET_RX on raw channel: DHCP response
         if ifCfg.method = METHOD_DHCP then
            declare
               action : DHCP.DHCPAction;
               ignore : Unsigned_64;
            begin
               DHCP.handlePacket (dhcpState, msg, action);

               case action.kind is
                  when DHCP.ACTION_NONE =>
                     null;

                  when DHCP.ACTION_CONFIGURE =>
                     --  DHCP acquired address, configure netstack
                     sendConfigure (0,
                                    action.address,
                                    action.netmask,
                                    action.gateway);
                     if action.dns /= 0 then
                        sendSetDNS (action.dns);
                     end if;
                     debugPrint ("netmgr: DHCP configured" & LF);

                  when DHCP.ACTION_SEND_REQUEST =>
                     DHCP.sendRequest (dhcpState, Netstack);
               end case;

               ignore := reply (sender, NULL_MESSAGE);
            end;
         else
            declare
               ignore : Unsigned_64;
            begin
               ignore := reply (sender, NULL_MESSAGE);
            end;
         end if;

      else
         --  Unknown message, reply error
         declare
            replyMsg : constant Message :=
              (tag      => (label  => REPLY_ERR,
                            length => 0,
                            protocol  => 0, ipc => <>,
                            reserved  => 0),
               authorityTag => 0,
               words    => (others => 0));
            ignore : Unsigned_64;
         begin
            ignore := reply (sender, replyMsg);
         end;
      end if;
   end loop;

end main;
