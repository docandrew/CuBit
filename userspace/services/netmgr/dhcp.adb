------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  DHCP client implementation for netmgr.
--
--  Builds/parses DHCP packets in a shared grant buffer with netstack.
--  Uses OP_NET_WRITE on the raw UDP channel to send, receives responses
--  via OP_NET_RX forwarded from netstack.
------------------------------------------------------------------------------
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;

package body DHCP is

   --  DHCP packet offsets (from start of UDP payload)
   --  Standard DHCP message is 240+ bytes
   DHCP_OP     : constant := 0;
   DHCP_HTYPE  : constant := 1;
   DHCP_HLEN   : constant := 2;
   DHCP_XID    : constant := 4;
   DHCP_CIADDR : constant := 12;
   DHCP_YIADDR : constant := 16;
   DHCP_SIADDR : constant := 20;
   DHCP_GIADDR : constant := 24;
   DHCP_CHADDR : constant := 28;
   DHCP_MAGIC  : constant := 236;
   DHCP_OPTIONS : constant := 240;

   --  Magic cookie
   MAGIC_COOKIE : constant Unsigned_32 := 16#63825363#;

   --  IPC labels
   OP_NET_WRITE : constant Unsigned_32 := 16#0421#;
   REPLY_OK     : constant Unsigned_32 := 16#F000#;

   PAGE_SIZE : constant := 4096;

   ---------------------------------------------------------------------------
   --  putU8
   ---------------------------------------------------------------------------
   procedure putU8 (buf : System.Address; off : Natural;
                    val : Unsigned_8) is
      b : Unsigned_8 with Import, Address => buf + Storage_Offset (off);
   begin
      b := val;
   end putU8;

   ---------------------------------------------------------------------------
   --  putU16BE
   ---------------------------------------------------------------------------
   procedure putU16BE (buf : System.Address; off : Natural;
                       val : Unsigned_16) is
      hi : Unsigned_8 with
         Import, Address => buf + Storage_Offset (off);
      lo : Unsigned_8 with
         Import, Address => buf + Storage_Offset (off + 1);
   begin
      hi := Unsigned_8 (Shift_Right (val, 8) and 16#FF#);
      lo := Unsigned_8 (val and 16#FF#);
   end putU16BE;

   ---------------------------------------------------------------------------
   --  putU32BE
   ---------------------------------------------------------------------------
   procedure putU32BE (buf : System.Address; off : Natural;
                       val : Unsigned_32) is
   begin
      putU8 (buf, off,     Unsigned_8 (Shift_Right (val, 24) and 16#FF#));
      putU8 (buf, off + 1, Unsigned_8 (Shift_Right (val, 16) and 16#FF#));
      putU8 (buf, off + 2, Unsigned_8 (Shift_Right (val, 8) and 16#FF#));
      putU8 (buf, off + 3, Unsigned_8 (val and 16#FF#));
   end putU32BE;

   ---------------------------------------------------------------------------
   --  getU8
   ---------------------------------------------------------------------------
   function getU8 (buf : System.Address; off : Natural) return Unsigned_8 is
      b : Unsigned_8 with Import, Address => buf + Storage_Offset (off);
   begin
      return b;
   end getU8;

   ---------------------------------------------------------------------------
   --  getU32BE
   ---------------------------------------------------------------------------
   function getU32BE (buf : System.Address; off : Natural)
                      return Unsigned_32 is
   begin
      return Shift_Left (Unsigned_32 (getU8 (buf, off)), 24) or
             Shift_Left (Unsigned_32 (getU8 (buf, off + 1)), 16) or
             Shift_Left (Unsigned_32 (getU8 (buf, off + 2)), 8) or
             Unsigned_32 (getU8 (buf, off + 3));
   end getU32BE;

   ---------------------------------------------------------------------------
   --  packIP - pack 4 big-endian bytes into our little-endian Unsigned_64
   ---------------------------------------------------------------------------
   function packIP (buf : System.Address; off : Natural) return Unsigned_64 is
   begin
      return Unsigned_64 (getU8 (buf, off)) or
             Shift_Left (Unsigned_64 (getU8 (buf, off + 1)), 8) or
             Shift_Left (Unsigned_64 (getU8 (buf, off + 2)), 16) or
             Shift_Left (Unsigned_64 (getU8 (buf, off + 3)), 24);
   end packIP;

   ---------------------------------------------------------------------------
   --  unpackIPBE - write packed little-endian IP as big-endian bytes
   ---------------------------------------------------------------------------
   procedure unpackIPBE (buf  : System.Address;
                         off  : Natural;
                         addr : Unsigned_64) is
   begin
      putU8 (buf, off,     Unsigned_8 (addr and 16#FF#));
      putU8 (buf, off + 1, Unsigned_8 (Shift_Right (addr, 8) and 16#FF#));
      putU8 (buf, off + 2, Unsigned_8 (Shift_Right (addr, 16) and 16#FF#));
      putU8 (buf, off + 3, Unsigned_8 (Shift_Right (addr, 24) and 16#FF#));
   end unpackIPBE;

   ---------------------------------------------------------------------------
   --  ensureBuf - lazily allocate the grant buffer
   ---------------------------------------------------------------------------
   procedure ensureBuf (state : in out DHCPState) is
      rawAddr   : Unsigned_64;
      aligned   : Unsigned_64;
      netstackPID : Unsigned_64;
      gid       : Unsigned_64;
      ok        : Boolean;
   begin
      if state.bufReady then
         return;
      end if;

      rawAddr := syscall (SYSCALL_SBRK, 2 * PAGE_SIZE);
      if rawAddr = Unsigned_64'Last then
         return;
      end if;

      aligned := (rawAddr + PAGE_SIZE - 1) and not Unsigned_64 (PAGE_SIZE - 1);
      state.grantBuf := To_Address (Integer_Address (aligned));

      --  Zero buffer
      declare
         buf : array (0 .. PAGE_SIZE - 1) of Unsigned_8
           with Import, Address => state.grantBuf;
      begin
         for i in buf'Range loop
            buf (i) := 0;
         end loop;
      end;

      --  Discover netstack PID and create grant
      netstackPID := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_NETSTACK);
      if netstackPID = 0 then
         return;
      end if;

      createGrant
        (grantee   => netstackPID,
         localAddr => state.grantBuf,
         numPages  => 1,
         readWrite => True,
         grantId   => gid,
         success   => ok);

      if not ok then
         return;
      end if;

      state.grantId := gid;
      state.bufReady := True;
   end ensureBuf;

   ---------------------------------------------------------------------------
   --  buildDHCPPacket - build a DHCP message in the grant buffer
   --  Returns the total packet length.
   ---------------------------------------------------------------------------
   function buildDHCPPacket (state   : in out DHCPState;
                             msgType : Unsigned_8) return Natural
   is
      buf : constant System.Address := state.grantBuf;
      off : Natural;
   begin
      --  Zero first 300 bytes
      declare
         b : array (0 .. 299) of Unsigned_8 with Import, Address => buf;
      begin
         for i in b'Range loop
            b (i) := 0;
         end loop;
      end;

      --  BOOTP header
      putU8    (buf, DHCP_OP, 1);          -- BOOTREQUEST
      putU8    (buf, DHCP_HTYPE, 1);       -- Ethernet
      putU8    (buf, DHCP_HLEN, 6);        -- MAC len
      putU32BE (buf, DHCP_XID, state.xid);

      --  For REQUEST, set ciaddr if renewing
      if msgType = DHCPREQUEST and state.phase = DHCP_RENEWING then
         unpackIPBE (buf, DHCP_CIADDR, state.offeredIP);
      end if;

      --  CHADDR: we don't know our MAC here; netstack's raw channel
      --  handler will fill the source MAC from the interface record.
      --  Leave CHADDR zeroed (netstack will use interface MAC).

      --  Magic cookie
      putU32BE (buf, DHCP_MAGIC, MAGIC_COOKIE);

      --  Options
      off := DHCP_OPTIONS;

      --  Option 53: DHCP Message Type
      putU8 (buf, off, OPT_MSG_TYPE);
      putU8 (buf, off + 1, 1);
      putU8 (buf, off + 2, msgType);
      off := off + 3;

      --  For REQUEST: option 50 (requested IP) + option 54 (server ID)
      if msgType = DHCPREQUEST and state.phase = DHCP_REQUESTING then
         --  Option 50: Requested IP Address
         putU8 (buf, off, OPT_REQUESTED_IP);
         putU8 (buf, off + 1, 4);
         unpackIPBE (buf, off + 2, state.offeredIP);
         off := off + 6;

         --  Option 54: Server Identifier
         putU8 (buf, off, OPT_SERVER_ID);
         putU8 (buf, off + 1, 4);
         unpackIPBE (buf, off + 2, state.serverIP);
         off := off + 6;
      end if;

      --  Option 255: End
      putU8 (buf, off, OPT_END);
      off := off + 1;

      --  Pad to minimum 300 bytes
      if off < 300 then
         off := 300;
      end if;

      return off;
   end buildDHCPPacket;

   ---------------------------------------------------------------------------
   --  sendViaRaw - send the grant buffer content via OP_NET_WRITE
   ---------------------------------------------------------------------------
   procedure sendViaRaw (state   : DHCPState;
                         pktLen  : Natural;
                         netSlot : CapabilitySlot)
   is
      msg : Message;
   begin
      msg := (tag      => (label  => OP_NET_WRITE,
                           length => 3,
                           flags  => 0,
                           badge  => 0),
              capBadge => 0,
              words    => (0 => state.grantId,
                           1 => 0,                      -- offset in grant
                           2 => Unsigned_64 (pktLen),
                           others => 0));
      msg.tag := capCall (netSlot, msg);
   end sendViaRaw;

   ---------------------------------------------------------------------------
   --  init
   ---------------------------------------------------------------------------
   procedure init (state : out DHCPState;
                   ifIdx : Unsigned_64) is
   begin
      state := (phase     => DHCP_INIT,
                ifIdx     => ifIdx,
                xid       => 16#CB17_D000#,
                offeredIP => 0,
                serverIP  => 0,
                netmask   => 0,
                gateway   => 0,
                dns       => 0,
                leaseTime => 0,
                grantBuf  => System.Null_Address,
                grantId   => 0,
                bufReady  => False);
   end init;

   ---------------------------------------------------------------------------
   --  sendDiscover
   ---------------------------------------------------------------------------
   procedure sendDiscover (state   : in out DHCPState;
                           netSlot : CapabilitySlot)
   is
      pktLen : Natural;
   begin
      ensureBuf (state);
      if not state.bufReady then
         return;
      end if;

      pktLen := buildDHCPPacket (state, DHCPDISCOVER);
      sendViaRaw (state, pktLen, netSlot);
      state.phase := DHCP_SELECTING;
   end sendDiscover;

   ---------------------------------------------------------------------------
   --  sendRequest
   ---------------------------------------------------------------------------
   procedure sendRequest (state   : in out DHCPState;
                          netSlot : CapabilitySlot)
   is
      pktLen : Natural;
   begin
      ensureBuf (state);
      if not state.bufReady then
         return;
      end if;

      pktLen := buildDHCPPacket (state, DHCPREQUEST);
      sendViaRaw (state, pktLen, netSlot);
      state.phase := DHCP_REQUESTING;
   end sendRequest;

   ---------------------------------------------------------------------------
   --  parseOptions - parse DHCP options from a response
   ---------------------------------------------------------------------------
   procedure parseOptions (state  : in out DHCPState;
                           buf    : System.Address;
                           pktLen : Natural;
                           msgType : out Unsigned_8)
   is
      off    : Natural := DHCP_OPTIONS;
      optCode : Unsigned_8;
      optLen  : Unsigned_8;
   begin
      msgType := 0;

      --  Verify magic cookie
      if pktLen < DHCP_OPTIONS + 4 then
         return;
      end if;
      if getU32BE (buf, DHCP_MAGIC) /= MAGIC_COOKIE then
         return;
      end if;

      while off < pktLen loop
         optCode := getU8 (buf, off);
         off := off + 1;

         if optCode = OPT_END then
            exit;
         end if;

         --  Pad option (code 0): no length byte
         if optCode = 0 then
            null;  -- skip padding
         else
            if off >= pktLen then
               exit;
            end if;
            optLen := getU8 (buf, off);
            off := off + 1;

            if off + Natural (optLen) > pktLen then
               exit;
            end if;

            case optCode is
               when OPT_MSG_TYPE =>
                  if optLen >= 1 then
                     msgType := getU8 (buf, off);
                  end if;

               when OPT_SUBNET_MASK =>
                  if optLen >= 4 then
                     state.netmask := packIP (buf, off);
                  end if;

               when OPT_ROUTER =>
                  if optLen >= 4 then
                     state.gateway := packIP (buf, off);
                  end if;

               when OPT_DNS =>
                  if optLen >= 4 then
                     state.dns := packIP (buf, off);
                  end if;

               when OPT_LEASE_TIME =>
                  if optLen >= 4 then
                     state.leaseTime := getU32BE (buf, off);
                  end if;

               when OPT_SERVER_ID =>
                  if optLen >= 4 then
                     state.serverIP := packIP (buf, off);
                  end if;

               when others =>
                  null;
            end case;

            off := off + Natural (optLen);
         end if;
      end loop;
   end parseOptions;

   ---------------------------------------------------------------------------
   --  handlePacket
   ---------------------------------------------------------------------------
   procedure handlePacket
     (state  : in out DHCPState;
      msg    : CuBit.Messages.Message;
      action : out DHCPAction)
   is
      --  The raw channel forwarded a DHCP response into our grant buffer.
      --  The grant buffer contains the raw DHCP/BOOTP payload (after UDP).
      pktLen  : Natural;
      xid     : Unsigned_32;
      yiaddr  : Unsigned_64;
      msgType : Unsigned_8;
   begin
      action := (kind => ACTION_NONE, others => 0);

      if not state.bufReady then
         return;
      end if;

      --  Packet length from IPC message
      pktLen := Natural (msg.words (1));
      if pktLen < DHCP_OPTIONS then
         return;
      end if;

      --  Verify XID matches
      xid := getU32BE (state.grantBuf, DHCP_XID);
      if xid /= state.xid then
         return;
      end if;

      --  Extract offered IP (yiaddr)
      yiaddr := packIP (state.grantBuf, DHCP_YIADDR);

      --  Parse options
      parseOptions (state, state.grantBuf, pktLen, msgType);

      case state.phase is
         when DHCP_SELECTING =>
            if msgType = DHCPOFFER and yiaddr /= 0 then
               state.offeredIP := yiaddr;
               debugPrint ("netmgr: DHCP OFFER received" & ASCII.LF);
               action := (kind => ACTION_SEND_REQUEST, others => 0);
               state.phase := DHCP_REQUESTING;
            end if;

         when DHCP_REQUESTING | DHCP_RENEWING =>
            if msgType = DHCPACK then
               state.offeredIP := yiaddr;
               state.phase := DHCP_BOUND;
               debugPrint ("netmgr: DHCP ACK received" & ASCII.LF);
               action := (kind    => ACTION_CONFIGURE,
                          address => state.offeredIP,
                          netmask => state.netmask,
                          gateway => state.gateway,
                          dns     => state.dns);
            elsif msgType = DHCPNAK then
               --  Reset to INIT, try again
               state.phase := DHCP_INIT;
               debugPrint ("netmgr: DHCP NAK, restarting" & ASCII.LF);
            end if;

         when others =>
            null;
      end case;
   end handlePacket;

end DHCP;
