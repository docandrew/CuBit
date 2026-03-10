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

package body TCPSession is

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
                      dstPort : Unsigned_16) return Integer is
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
   function allocateConn (conns     : in out ConnTable;
                          dstIP     : Net.IPv4Address;
                          dstMAC    : Net.MACAddress;
                          dstPort   : Unsigned_16;
                          localPort : Unsigned_16;
                          isn       : Unsigned_32) return Integer is
   begin
      for i in conns'Range loop
         if conns (i).state = TCP_CLOSED then
            conns (i) :=
               (state      => TCP_SYN_SENT,
                localPort  => localPort,
                remotePort => dstPort,
                remoteIP   => dstIP,
                remoteMAC  => dstMAC,
                sendNext   => isn,
                sendUnack  => isn,
                recvNext   => 0,
                sendWindow => 0);
            return i;
         end if;
      end loop;
      return -1;
   end allocateConn;

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

   ---------------------------------------------------------------------------
   --  onSend - send data (PSH+ACK), advance sendNext
   ---------------------------------------------------------------------------
   procedure onSend (conn    : in out Connection;
                     dataLen : Natural;
                     dataOff : Natural;
                     res     : out Result) is
   begin
      res := (others => <>);
      if conn.state /= TCP_ESTABLISHED then
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
                          connIdx : out Integer;
                          res     : out Result) is
   begin
      res := (others => <>);

      --  Find matching connection
      connIdx := findConn (conns, seg.srcIP, seg.srcPort, seg.dstPort);

      if connIdx < 0 then
         --  No matching connection; drop silently
         return;
      end if;

      --  RST always closes
      if seg.flagRST then
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
            if seg.flagSYN and seg.flagACK then
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

         when TCP_ESTABLISHED =>
            if seg.flagACK then
               conns (connIdx).sendUnack := seg.ackNum;
               conns (connIdx).sendWindow := seg.winSize;
            end if;

            if seg.dataLen > 0 then
               if seg.seqNum /= conns (connIdx).recvNext then
                  --  Out-of-order: send duplicate ACK
                  addSendAction (res, conns (connIdx), TCP_FLAG_ACK);
               else
                  --  In-order data: advance recvNext, ACK, notify
                  conns (connIdx).recvNext :=
                     seg.seqNum + Unsigned_32 (seg.dataLen);
                  addSendAction (res, conns (connIdx), TCP_FLAG_ACK);
                  addAction (res,
                     (kind    => ACT_NOTIFY_DATA,
                      flags   => 0,
                      seqNum  => 0,
                      ackNum  => 0,
                      dataLen => seg.dataLen,
                      dataOff => seg.dataOff));
               end if;
            end if;

            if seg.flagFIN then
               conns (connIdx).recvNext :=
                  conns (connIdx).recvNext + 1;
               --  ACK the FIN
               addSendAction (res, conns (connIdx), TCP_FLAG_ACK);
               conns (connIdx).state := TCP_CLOSE_WAIT;
               --  Immediately send FIN+ACK (capture before advancing)
               addSendAction (res, conns (connIdx),
                              TCP_FLAG_FIN or TCP_FLAG_ACK);
               conns (connIdx).sendNext :=
                  conns (connIdx).sendNext + 1;
               conns (connIdx).state := TCP_LAST_ACK;
               addAction (res,
                  (kind    => ACT_NOTIFY_CLOSED,
                   flags   => 0,
                   seqNum  => 0,
                   ackNum  => 0,
                   dataLen => 0,
                   dataOff => 0));
            end if;

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
