------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  TCP session state machine implementation.
--
--  Pure state transformations: events go in, actions come out.
--  Never touches IPC, sendFrame, or any external side effects.
--
--  IMPORTANT: Every ACT_SEND_SEGMENT captures seqNum/ackNum from the
--  connection state BEFORE advancing sendNext, so the caller can use
--  the action's seqNum/ackNum directly in the outgoing segment.
------------------------------------------------------------------------------

package body TCPSession with SPARK_Mode is

   ---------------------------------------------------------------------------
   --  addAction - append an action to the result list
   ---------------------------------------------------------------------------
   procedure addAction (res  : in out Result;
                        act  : Action) is
   begin
      if res.numActions < MAX_ACTIONS then
         res.actions (res.numActions) := act;
         res.numActions := res.numActions + 1;
      end if;
   end addAction;

   ---------------------------------------------------------------------------
   --  addSendAction - convenience for ACT_SEND_SEGMENT with conn's seq/ack
   ---------------------------------------------------------------------------
   procedure addSendAction (res     : in out Result;
                            conn    : Connection;
                            flags   : Unsigned_8;
                            dataLen : Natural := 0;
                            dataOff : Natural := 0) is
   begin
      addAction (res,
         (kind    => ACT_SEND_SEGMENT,
          flags   => flags,
          seqNum  => conn.sendNext,
          ackNum  => conn.recvNext,
          dataLen => dataLen,
          dataOff => dataOff));
   end addSendAction;

   ---------------------------------------------------------------------------
   --  findConn
   ---------------------------------------------------------------------------
   function findConn (conns   : ConnTable;
                      srcIP   : Net.IPv4Address;
                      srcPort : Unsigned_16;
                      dstPort : Unsigned_16) return Connection_Reference is
      use type Net.IPv4Address;
   begin
      for i in conns'Range loop
         if conns (i).state /= TCP_CLOSED and then
            conns (i).remoteIP = srcIP and then
            conns (i).remotePort = srcPort and then
            conns (i).localPort = dstPort
         then
            return i;
         end if;
      end loop;
      return -1;
   end findConn;

   ---------------------------------------------------------------------------
   --  allocateConn
   ---------------------------------------------------------------------------
   procedure allocateConn (conns     : in out ConnTable;
                          dstIP     : Net.IPv4Address;
                          dstMAC    : Net.MACAddress;
                          dstPort   : Unsigned_16;
                          localPort : Unsigned_16;
                          isn       : Unsigned_32;
                          index     : out Connection_Reference) is
   begin
      index := -1;
      for i in conns'Range loop
         if conns (i).state = TCP_CLOSED and not conns (i).reserved then
            conns (i) :=
               (state      => TCP_SYN_SENT,
                reserved   => True,
                localPort  => localPort,
                remotePort => dstPort,
                remoteIP   => dstIP,
                remoteMAC  => dstMAC,
                sendNext   => isn,
                sendUnack  => isn,
                recvNext   => 0,
                sendWindow => 0,
                receiveWindow => Unsigned_16'Last);
            index := i;
            return;
         end if;
      end loop;
   end allocateConn;

   procedure releaseReservation (conn : in out Connection) is
   begin
      conn.reserved := False;
      --  Failed opening requests have no user-visible channel to close them.
      if conn.state in TCP_SYN_SENT | TCP_SYN_RECEIVED then
         conn := (others => <>);
      end if;
   end releaseReservation;

   ---------------------------------------------------------------------------
   --  onConnect - send SYN, advance sendNext
   ---------------------------------------------------------------------------
   procedure onConnect (conn : in out Connection;
                        res  : out Result) is
   begin
      res := (others => <>);
      --  SYN segment (capture sendNext before advancing)
      addSendAction (res, conn, TCP_FLAG_SYN);
      --  SYN consumes 1 sequence number
      conn.sendNext := conn.sendNext + 1;
   end onConnect;

   procedure onPassiveOpen
     (conn : in out Connection; seg : SegmentInfo; res : out Result) is
   begin
      res := (others => <>);
      if not seg.flagSYN or seg.flagACK or seg.flagRST or seg.flagFIN then
         return;
      end if;
      conn.state := TCP_SYN_RECEIVED;
      conn.recvNext := seg.seqNum + 1;
      conn.sendWindow := seg.winSize;
      addSendAction (res, conn, TCP_FLAG_SYN or TCP_FLAG_ACK);
      conn.sendNext := conn.sendNext + 1;
      --  SYN data is not acknowledged: the peer must retransmit it after
      --  completing the handshake. No application data before establishment.
   end onPassiveOpen;

   ---------------------------------------------------------------------------
   --  onSend - send data (PSH+ACK), advance sendNext
   ---------------------------------------------------------------------------
   procedure onSend (conn    : in out Connection;
                     dataLen : Natural;
                     dataOff : Natural;
                     res     : out Result) is
   begin
      res := (others => <>);
      if conn.state /= TCP_ESTABLISHED and conn.state /= TCP_CLOSE_WAIT then
         return;
      end if;
      --  Capture sendNext/recvNext before advancing
      addSendAction (res, conn, TCP_FLAG_PSH or TCP_FLAG_ACK,
                     dataLen, dataOff);
      conn.sendNext := conn.sendNext + Unsigned_32 (dataLen);
   end onSend;

   ---------------------------------------------------------------------------
   --  onClose - send FIN+ACK, transition state, advance sendNext
   ---------------------------------------------------------------------------
   procedure onClose (conn : in out Connection;
                      res  : out Result) is
   begin
      res := (others => <>);
      if conn.state /= TCP_ESTABLISHED and
         conn.state /= TCP_CLOSE_WAIT
      then
         return;
      end if;

      --  Capture sendNext/recvNext before advancing
      addSendAction (res, conn, TCP_FLAG_FIN or TCP_FLAG_ACK);
      --  FIN consumes 1 sequence number
      conn.sendNext := conn.sendNext + 1;

      if conn.state = TCP_ESTABLISHED then
         conn.state := TCP_FIN_WAIT_1;
      else
         conn.state := TCP_LAST_ACK;
      end if;
   end onClose;

   ---------------------------------------------------------------------------
   --  onSegmentIn - process an incoming TCP segment
   ---------------------------------------------------------------------------
   procedure onSegmentIn (conns   : in out ConnTable;
                          seg     : SegmentInfo;
                          connIdx : out Connection_Reference;
                          res     : out Result) is
      procedure receiveEstablished (conn : in out Connection) is
      begin
         --  Receive only contiguous bytes that fit the advertised credit.
         --  Never advance RCV.NXT for data that the service cannot retain.
         if seg.seqNum /= conn.recvNext or else
           seg.dataLen > Natural (conn.receiveWindow)
         then
            addSendAction (res, conn, TCP_FLAG_ACK);
            return;
         end if;
         if seg.dataLen > 0 then
            conn.recvNext := conn.recvNext + Unsigned_32 (seg.dataLen);
            conn.receiveWindow := conn.receiveWindow - Unsigned_16 (seg.dataLen);
            addSendAction (res, conn, TCP_FLAG_ACK);
            addAction (res, (kind => ACT_NOTIFY_DATA,
                            dataLen => seg.dataLen, dataOff => seg.dataOff,
                            others => <>));
         end if;
         if seg.flagFIN then
            conn.recvNext := conn.recvNext + 1;
            conn.state := TCP_CLOSE_WAIT;
            addSendAction (res, conn, TCP_FLAG_ACK);
            addAction (res, (kind => ACT_NOTIFY_CLOSED, others => <>));
            --  Peer FIN closes only the receive direction. The application
            --  can still send its response and explicitly close afterwards.
         end if;
      end receiveEstablished;
   begin
      res := (others => <>);

      --  Find matching connection
      connIdx := findConn (conns, seg.srcIP, seg.srcPort, seg.dstPort);

      if connIdx < 0 then
         --  No matching connection; drop silently
         return;
      end if;

      --  An unrelated/future reset must not tear down a matched connection.
      if seg.flagRST then
         if conns (connIdx).state = TCP_SYN_SENT then
            if not seg.flagACK or else seg.ackNum /= conns (connIdx).sendNext then return; end if;
         elsif seg.seqNum /= conns (connIdx).recvNext then
            addSendAction (res, conns (connIdx), TCP_FLAG_ACK);
            return;
         end if;
         conns (connIdx).state := TCP_CLOSED;
         addAction (res,
            (kind    => ACT_NOTIFY_ERROR,
             flags   => 0,
             seqNum  => 0,
             ackNum  => 0,
             dataLen => 0,
             dataOff => 0));
         return;
      end if;

      --  State dispatch
      case conns (connIdx).state is
         when TCP_SYN_SENT =>
            if seg.flagSYN and then seg.flagACK and then not seg.flagFIN and then
              seg.ackNum = conns (connIdx).sendNext
            then
               conns (connIdx).recvNext := seg.seqNum + 1;
               conns (connIdx).sendUnack := seg.ackNum;
               conns (connIdx).sendWindow := seg.winSize;
               conns (connIdx).state := TCP_ESTABLISHED;
               --  Send ACK (sendNext unchanged, recvNext just set)
               addSendAction (res, conns (connIdx), TCP_FLAG_ACK);
               addAction (res,
                  (kind    => ACT_NOTIFY_ESTABLISHED,
                   flags   => 0,
                   seqNum  => 0,
                   ackNum  => 0,
                   dataLen => 0,
                   dataOff => 0));
            end if;

         when TCP_SYN_RECEIVED =>
            if seg.flagSYN and then not seg.flagACK and then
              seg.seqNum = conns (connIdx).recvNext - 1
            then
               --  Duplicate SYN: retransmit the original SYN-ACK without
               --  consuming another sequence number or allocating a child.
               addAction (res,
                 (kind => ACT_SEND_SEGMENT, flags => TCP_FLAG_SYN or TCP_FLAG_ACK,
                  seqNum => conns (connIdx).sendUnack,
                  ackNum => conns (connIdx).recvNext, others => <>));
            elsif not seg.flagSYN and then seg.flagACK and then
              seg.ackNum = conns (connIdx).sendNext and then
              seg.seqNum = conns (connIdx).recvNext
            then
               conns (connIdx).sendUnack := seg.ackNum;
               conns (connIdx).sendWindow := seg.winSize;
               conns (connIdx).state := TCP_ESTABLISHED;
               addAction (res, (kind => ACT_NOTIFY_ESTABLISHED, others => <>));
               receiveEstablished (conns (connIdx));
            end if;

         when TCP_ESTABLISHED =>
            if seg.flagSYN or else not seg.flagACK then
               addSendAction (res, conns (connIdx), TCP_FLAG_ACK);
               return;
            end if;
            --  Serial-number interval comparison also works across wrap.
            if seg.ackNum - conns (connIdx).sendUnack <=
              conns (connIdx).sendNext - conns (connIdx).sendUnack
            then
               conns (connIdx).sendUnack := seg.ackNum;
               conns (connIdx).sendWindow := seg.winSize;
            elsif seg.ackNum - conns (connIdx).sendNext < 16#8000_0000# then
               addSendAction (res, conns (connIdx), TCP_FLAG_ACK);
               return;
            end if;
            receiveEstablished (conns (connIdx));

         when TCP_FIN_WAIT_1 =>
            if seg.flagACK and seg.flagFIN then
               conns (connIdx).recvNext := seg.seqNum + 1;
               addSendAction (res, conns (connIdx), TCP_FLAG_ACK);
               conns (connIdx).state := TCP_CLOSED;
               addAction (res,
                  (kind    => ACT_NOTIFY_CLOSED,
                   flags   => 0,
                   seqNum  => 0,
                   ackNum  => 0,
                   dataLen => 0,
                   dataOff => 0));
            elsif seg.flagACK then
               conns (connIdx).sendUnack := seg.ackNum;
               conns (connIdx).state := TCP_FIN_WAIT_2;
            elsif seg.flagFIN then
               conns (connIdx).recvNext := seg.seqNum + 1;
               addSendAction (res, conns (connIdx), TCP_FLAG_ACK);
               conns (connIdx).state := TCP_CLOSING;
            end if;

         when TCP_FIN_WAIT_2 =>
            if seg.flagFIN then
               conns (connIdx).recvNext := seg.seqNum + 1;
               addSendAction (res, conns (connIdx), TCP_FLAG_ACK);
               conns (connIdx).state := TCP_CLOSED;
               addAction (res,
                  (kind    => ACT_NOTIFY_CLOSED,
                   flags   => 0,
                   seqNum  => 0,
                   ackNum  => 0,
                   dataLen => 0,
                   dataOff => 0));
            end if;

         when TCP_CLOSING =>
            if seg.flagACK then
               conns (connIdx).state := TCP_CLOSED;
               addAction (res,
                  (kind    => ACT_NOTIFY_CLOSED,
                   flags   => 0,
                   seqNum  => 0,
                   ackNum  => 0,
                   dataLen => 0,
                   dataOff => 0));
            end if;

         when TCP_LAST_ACK =>
            if seg.flagACK then
               conns (connIdx).state := TCP_CLOSED;
               addAction (res,
                  (kind    => ACT_NOTIFY_CLOSED,
                   flags   => 0,
                   seqNum  => 0,
                   ackNum  => 0,
                   dataLen => 0,
                   dataOff => 0));
            end if;

         when TCP_TIME_WAIT =>
            conns (connIdx).state := TCP_CLOSED;
            addAction (res,
               (kind    => ACT_NOTIFY_CLOSED,
                flags   => 0,
                seqNum  => 0,
                ackNum  => 0,
                dataLen => 0,
                dataOff => 0));

         when others =>
            null;
      end case;
   end onSegmentIn;

end TCPSession;
