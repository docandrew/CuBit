------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  TCP session state machine (pure action/event model).
--
--  Events go in (incoming segment, app connect/send/close requests),
--  actions come out (send segment, notify established/data/closed/error).
--  The netstack event loop dispatches events and executes returned actions.
--
--  This package never calls sendFrame, reply, or any IPC directly.
--  It only mutates the Connection record and returns Actions.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with Net;

package TCPSession is

   ---------------------------------------------------------------------------
   --  Connection states (RFC 793)
   ---------------------------------------------------------------------------
   type TCPState is (TCP_CLOSED, TCP_SYN_SENT, TCP_ESTABLISHED,
                     TCP_FIN_WAIT_1, TCP_FIN_WAIT_2,
                     TCP_CLOSE_WAIT, TCP_CLOSING,
                     TCP_LAST_ACK, TCP_TIME_WAIT);

   ---------------------------------------------------------------------------
   --  Per-connection record
   ---------------------------------------------------------------------------
   type Connection is record
      state      : TCPState := TCP_CLOSED;
      localPort  : Unsigned_16 := 0;
      remotePort : Unsigned_16 := 0;
      remoteIP   : Net.IPv4Address := (others => 0);
      remoteMAC  : Net.MACAddress := (others => 0);
      sendNext   : Unsigned_32 := 0;   -- SND.NXT
      sendUnack  : Unsigned_32 := 0;   -- SND.UNA
      recvNext   : Unsigned_32 := 0;   -- RCV.NXT
      sendWindow : Unsigned_16 := 0;   -- SND.WND
   end record;

   ---------------------------------------------------------------------------
   --  Connection table
   ---------------------------------------------------------------------------
   MAX_TCP_CONNS : constant := 4;
   type ConnTable is array (0 .. MAX_TCP_CONNS - 1) of Connection;

   ---------------------------------------------------------------------------
   --  Input: parsed segment fields (from RecordFlux Net.TCP.Segment)
   ---------------------------------------------------------------------------
   type SegmentInfo is record
      srcIP   : Net.IPv4Address;
      srcPort : Unsigned_16;
      dstPort : Unsigned_16;
      seqNum  : Unsigned_32;
      ackNum  : Unsigned_32;
      flagSYN : Boolean;
      flagACK : Boolean;
      flagFIN : Boolean;
      flagRST : Boolean;
      winSize : Unsigned_16;
      dataLen : Natural;       -- TCP payload length
      dataOff : Natural;       -- byte offset of payload in packet buffer
   end record;

   ---------------------------------------------------------------------------
   --  Output actions (the netstack event loop executes these)
   ---------------------------------------------------------------------------
   type ActionKind is (ACT_NONE, ACT_SEND_SEGMENT,
                       ACT_NOTIFY_ESTABLISHED, ACT_NOTIFY_DATA,
                       ACT_NOTIFY_CLOSED, ACT_NOTIFY_ERROR);

   --  TCP flag bits for ACT_SEND_SEGMENT
   TCP_FLAG_FIN : constant Unsigned_8 := 16#01#;
   TCP_FLAG_SYN : constant Unsigned_8 := 16#02#;
   TCP_FLAG_RST : constant Unsigned_8 := 16#04#;
   TCP_FLAG_PSH : constant Unsigned_8 := 16#08#;
   TCP_FLAG_ACK : constant Unsigned_8 := 16#10#;

   type Action is record
      kind    : ActionKind := ACT_NONE;
      flags   : Unsigned_8 := 0;     -- TCP flags for ACT_SEND_SEGMENT
      seqNum  : Unsigned_32 := 0;    -- sequence number to use in segment
      ackNum  : Unsigned_32 := 0;    -- ack number to use in segment
      dataLen : Natural := 0;        -- payload length
      dataOff : Natural := 0;        -- offset of payload in packet buffer
   end record;

   MAX_ACTIONS : constant := 4;
   type ActionArray is array (0 .. MAX_ACTIONS - 1) of Action;

   type Result is record
      actions    : ActionArray := (others => (others => <>));
      numActions : Natural := 0;
   end record;

   ---------------------------------------------------------------------------
   --  State machine entry points
   ---------------------------------------------------------------------------

   --  Process an incoming TCP segment
   procedure onSegmentIn (conns   : in out ConnTable;
                          seg     : SegmentInfo;
                          connIdx : out Integer;
                          res     : out Result);

   --  Initiate a TCP connection (SYN)
   procedure onConnect (conn : in out Connection;
                        res  : out Result);

   --  Send data on an established connection (PSH+ACK)
   procedure onSend (conn    : in out Connection;
                     dataLen : Natural;
                     dataOff : Natural;
                     res     : out Result);

   --  Initiate close (FIN+ACK)
   procedure onClose (conn : in out Connection;
                      res  : out Result);

   ---------------------------------------------------------------------------
   --  Connection table utilities
   ---------------------------------------------------------------------------

   --  Find a connection matching the given segment fields.
   --  Returns index into ConnTable, or -1 if not found.
   function findConn (conns   : ConnTable;
                      srcIP   : Net.IPv4Address;
                      srcPort : Unsigned_16;
                      dstPort : Unsigned_16) return Integer;

   --  Allocate a new connection slot and initialize it for outgoing SYN.
   --  Returns index into ConnTable, or -1 if table is full.
   function allocateConn (conns     : in out ConnTable;
                          dstIP     : Net.IPv4Address;
                          dstMAC    : Net.MACAddress;
                          dstPort   : Unsigned_16;
                          localPort : Unsigned_16;
                          isn       : Unsigned_32) return Integer;

end TCPSession;
