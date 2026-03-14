------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  DHCP client types and state machine for netmgr.
--
--  Implements DHCP DORA (Discover-Offer-Request-Ack) via raw UDP channel
--  through netstack. Packets are built/parsed manually in a grant buffer.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;

with CuBit.Messages;

package DHCP is

   --  DHCP message types (option 53)
   DHCPDISCOVER : constant Unsigned_8 := 1;
   DHCPOFFER    : constant Unsigned_8 := 2;
   DHCPREQUEST  : constant Unsigned_8 := 3;
   DHCPDECLINE  : constant Unsigned_8 := 4;
   DHCPACK      : constant Unsigned_8 := 5;
   DHCPNAK      : constant Unsigned_8 := 6;
   DHCPRELEASE  : constant Unsigned_8 := 7;

   --  DHCP option codes
   OPT_SUBNET_MASK  : constant Unsigned_8 := 1;
   OPT_ROUTER       : constant Unsigned_8 := 3;
   OPT_DNS           : constant Unsigned_8 := 6;
   OPT_REQUESTED_IP : constant Unsigned_8 := 50;
   OPT_LEASE_TIME   : constant Unsigned_8 := 51;
   OPT_MSG_TYPE     : constant Unsigned_8 := 53;
   OPT_SERVER_ID    : constant Unsigned_8 := 54;
   OPT_END          : constant Unsigned_8 := 255;

   --  DHCP client state
   type DHCPPhase is (DHCP_INIT, DHCP_SELECTING, DHCP_REQUESTING,
                      DHCP_BOUND, DHCP_RENEWING);

   type DHCPState is record
      phase     : DHCPPhase := DHCP_INIT;
      ifIdx     : Unsigned_64 := 0;
      xid       : Unsigned_32 := 16#CB17_D000#;
      --  Offered values
      offeredIP : Unsigned_64 := 0;
      serverIP  : Unsigned_64 := 0;
      netmask   : Unsigned_64 := 0;
      gateway   : Unsigned_64 := 0;
      dns       : Unsigned_64 := 0;
      leaseTime : Unsigned_32 := 0;
      --  Grant buffer for building DHCP packets
      grantBuf  : System.Address := System.Null_Address;
      grantId   : Unsigned_64 := 0;
      bufReady  : Boolean := False;
   end record;

   --  Action returned from handlePacket
   type ActionKind is (ACTION_NONE, ACTION_CONFIGURE, ACTION_SEND_REQUEST);

   type DHCPAction is record
      kind    : ActionKind := ACTION_NONE;
      address : Unsigned_64 := 0;
      netmask : Unsigned_64 := 0;
      gateway : Unsigned_64 := 0;
      dns     : Unsigned_64 := 0;
   end record;

   ---------------------------------------------------------------------------
   --  init - initialize DHCP state for an interface
   ---------------------------------------------------------------------------
   procedure init (state : out DHCPState;
                   ifIdx : Unsigned_64);

   ---------------------------------------------------------------------------
   --  sendDiscover - send DHCP DISCOVER via raw channel
   ---------------------------------------------------------------------------
   procedure sendDiscover
     (state   : in out DHCPState;
      netSlot : CuBit.Messages.CapabilitySlot);

   ---------------------------------------------------------------------------
   --  sendRequest - send DHCP REQUEST for the offered IP
   ---------------------------------------------------------------------------
   procedure sendRequest
     (state   : in out DHCPState;
      netSlot : CuBit.Messages.CapabilitySlot);

   ---------------------------------------------------------------------------
   --  handlePacket - process a DHCP response packet
   ---------------------------------------------------------------------------
   procedure handlePacket
     (state  : in out DHCPState;
      msg    : CuBit.Messages.Message;
      action : out DHCPAction);

end DHCP;
