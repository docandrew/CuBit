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
--  Capability slots (assigned by devmgr):
--    4  = CAP_ENDPOINT -> netstack (config + raw UDP IPC)
--    6  = CAP_ENDPOINT -> config.svc (read net.* keys)
--    8  = CAP_NOTIFICATION (DRIVER_NETMGR registration)
--    15 = CAP_ENDPOINT -> devmgr (OP_READY signal)
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Config;
with DHCP;

procedure main is
   use ASCII;
   use type CuBit.Config.ConfigStatus;

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

   --  Capability slots (granted by devmgr)
   CAP_SLOT_NETSTACK : constant CapabilitySlot := 4;
   CAP_SLOT_CONFIG   : constant CapabilitySlot := 6;
   CAP_SLOT_READY    : constant Unsigned_64 := 15;

   --  Interface method
   type IfMethod is (METHOD_NONE, METHOD_STATIC, METHOD_DHCP);

   --  Per-interface config from system.conf
   type IfConfig is record
      method  : IfMethod := METHOD_NONE;
      address : Unsigned_64 := 0;
      netmask : Unsigned_64 := 0;
      gateway : Unsigned_64 := 0;
   end record;

   ifCfg : IfConfig;

   --  DNS config
   primaryDNS : Unsigned_64 := 0;

   --  DHCP state (per interface)
   dhcpState : DHCP.DHCPState;

   ---------------------------------------------------------------------------
   --  parseIPv4 - parse "A.B.C.D" from raw bytes, return packed Unsigned_64
   --  Format: byte0 | (byte1 << 8) | (byte2 << 16) | (byte3 << 24)
   ---------------------------------------------------------------------------
   function parseIPv4 (buf : System.Address;
                       len : Natural) return Unsigned_64
   is
      data : array (0 .. len - 1) of Unsigned_8
        with Import, Address => buf;
      octets : array (0 .. 3) of Unsigned_64 := (others => 0);
      idx    : Natural := 0;
      val    : Unsigned_64 := 0;
   begin
      if len = 0 then
         return 0;
      end if;

      for i in 0 .. len - 1 loop
         if data (i) = 16#2E# then  --  '.'
            if idx > 2 then
               return 0;
            end if;
            octets (idx) := val;
            idx := idx + 1;
            val := 0;
         elsif data (i) >= 16#30# and data (i) <= 16#39# then
            val := val * 10 + Unsigned_64 (data (i) - 16#30#);
            if val > 255 then
               return 0;
            end if;
         else
            return 0;
         end if;
      end loop;

      if idx /= 3 then
         return 0;
      end if;
      octets (3) := val;

      return octets (0) or
             Shift_Left (octets (1), 8) or
             Shift_Left (octets (2), 16) or
             Shift_Left (octets (3), 24);
   end parseIPv4;

   ---------------------------------------------------------------------------
   --  strEq - freestanding string comparison
   ---------------------------------------------------------------------------
   function strEq (a : String; b : String) return Boolean is
   begin
      if a'Length /= b'Length then
         return False;
      end if;
      for i in 0 .. a'Length - 1 loop
         if a (a'First + i) /= b (b'First + i) then
            return False;
         end if;
      end loop;
      return True;
   end strEq;

   ---------------------------------------------------------------------------
   --  readConfigKey - read a key from config.svc, return value address/len
   ---------------------------------------------------------------------------
   procedure readConfigKey (key      : String;
                            val      : out System.Address;
                            valLen   : out Natural;
                            ok       : out Boolean)
   is
      status : CuBit.Config.ConfigStatus;
   begin
      CuBit.Config.get (key, val, valLen, status);
      ok := (status = CuBit.Config.OK and valLen > 0);
   end readConfigKey;

   ---------------------------------------------------------------------------
   --  readConfigIP - read an IP address from config
   ---------------------------------------------------------------------------
   function readConfigIP (key : String) return Unsigned_64
   is
      val    : System.Address;
      valLen : Natural;
      ok     : Boolean;
   begin
      readConfigKey (key, val, valLen, ok);
      if ok then
         return parseIPv4 (val, valLen);
      end if;
      return 0;
   end readConfigIP;

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
                           flags  => 0,
                           badge  => 0),
              capBadge => 0,
              words    => (0 => ifIdx,
                           1 => addr,
                           2 => mask,
                           3 => gw));
      msg.tag := capCall (CAP_SLOT_NETSTACK, msg);

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
                           flags  => 0,
                           badge  => 0),
              capBadge => 0,
              words    => (0 => primary,
                           1 => secondary,
                           others => 0));
      msg.tag := capCall (CAP_SLOT_NETSTACK, msg);
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
                           flags  => 0,
                           badge  => 0),
              capBadge => 0,
              words    => (others => 0));
      ignore := capCall (CAP_SLOT_READY, msg);
   end signalReady;

   ---------------------------------------------------------------------------
   --  queryInterfaces - ask netstack how many interfaces are attached
   ---------------------------------------------------------------------------
   function queryInterfaces return Natural is
      msg : Message;
   begin
      msg := (tag      => (label  => OP_NET_LIST_IF,
                           length => 0,
                           flags  => 0,
                           badge  => 0),
              capBadge => 0,
              words    => (others => 0));
      msg.tag := capCall (CAP_SLOT_NETSTACK, msg);
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
                           flags  => 0,
                           badge  => 0),
              capBadge => 0,
              words    => (0 => ifIdx,
                           1 => Unsigned_64 (PROTO_UDP),
                           2 => Unsigned_64 (port),
                           others => 0));
      msg.tag := capCall (CAP_SLOT_NETSTACK, msg);
      return msg.tag.label = REPLY_OK;
   end openRawChannel;

   --  Variables for main loop
   ret    : Unsigned_64;
   val    : System.Address;
   valLen : Natural;
   ok     : Boolean;
   ifCount : Natural;
   sender : ProcessID;
   msg    : Message;

begin
   debugPrint ("netmgr: starting" & LF);

   --  Register as DRIVER_NETMGR
   ret := registerDriver (DRIVER_NETMGR);
   debugPrint ("netmgr: registered" & LF);

   --  Read net.if0.method from config
   readConfigKey ("net.if0.method", val, valLen, ok);
   if ok then
      declare
         mBuf : String (1 .. valLen) with Import, Address => val;
      begin
         if strEq (mBuf, "static") then
            ifCfg.method := METHOD_STATIC;
            debugPrint ("netmgr: if0 method=static" & LF);
         elsif strEq (mBuf, "dhcp") then
            ifCfg.method := METHOD_DHCP;
            debugPrint ("netmgr: if0 method=dhcp" & LF);
         else
            debugPrint ("netmgr: if0 unknown method" & LF);
         end if;
      end;
   else
      debugPrint ("netmgr: no net.if0.method configured" & LF);
   end if;

   --  Read static config values (used as fallback even for DHCP)
   ifCfg.address := readConfigIP ("net.if0.address");
   ifCfg.netmask := readConfigIP ("net.if0.netmask");
   ifCfg.gateway := readConfigIP ("net.if0.gateway");
   primaryDNS    := readConfigIP ("net.dns.primary");

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
         sendSetDNS (primaryDNS);
         debugPrint ("netmgr: DNS configured" & LF);
      end if;

   elsif ifCfg.method = METHOD_DHCP then
      --  Initialize DHCP state machine for interface 0
      DHCP.init (dhcpState, 0);

      --  Open raw UDP channel on port 68 (DHCP client) via netstack
      if openRawChannel (0, 68) then
         debugPrint ("netmgr: raw channel open for DHCP" & LF);

         --  Send DHCP DISCOVER
         DHCP.sendDiscover (dhcpState, CAP_SLOT_NETSTACK);
         debugPrint ("netmgr: DHCP DISCOVER sent" & LF);
      else
         debugPrint ("netmgr: failed to open raw channel" & LF);
         --  Fall back to static config if available
         if ifCfg.address /= 0 then
            sendConfigure (0, ifCfg.address, ifCfg.netmask, ifCfg.gateway);
            if primaryDNS /= 0 then
               sendSetDNS (primaryDNS);
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
                     DHCP.sendRequest (dhcpState, CAP_SLOT_NETSTACK);
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
                            flags  => 0,
                            badge  => 0),
               capBadge => 0,
               words    => (others => 0));
            ignore : Unsigned_64;
         begin
            ignore := reply (sender, replyMsg);
         end;
      end if;
   end loop;

end main;
