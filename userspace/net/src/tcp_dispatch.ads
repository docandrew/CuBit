------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Where an arriving TCP segment goes (RFC 9293 3.10.7): to its
--  connection, to a TIME-WAIT entry, to a new passive open, a reset, or
--  nowhere. The caller has already verified the checksum and the header,
--  looked the 4-tuple up, and let TIME-WAIT consume what it keeps.
--
--  Proved (tests/net-tcp): exactly this table, and, stated on their own:
--  - a new connection is opened only for a plain SYN (no ACK, RST or FIN)
--    to a listening port from a usable source;
--  - nothing is ever sent to an unusable source (zero address or port,
--    multicast, broadcast), so the stack is not a reflector;
--  - a segment that belongs to a connection always goes to it.
------------------------------------------------------------------------------
package TCP_Dispatch with SPARK_Mode, Pure is

   type Action is (To_Connection, Taken_By_Time_Wait, Open_Passive, Refuse, Drop);

   type Arrival is record
      Has_Connection  : Boolean := False;   --  the 4-tuple is in the table
      Time_Wait_Taken : Boolean := False;   --  TIME-WAIT consumed it
      Usable_Source   : Boolean := False;   --  unicast, nonzero, port nonzero
      SYN, ACK, RST, FIN : Boolean := False;
      Listening       : Boolean := False;   --  a listener on the destination
   end record;

   function Plain_SYN (A : Arrival) return Boolean is
     (A.SYN and then not A.ACK and then not A.RST and then not A.FIN);

   function Decide (A : Arrival) return Action is
     (if A.Has_Connection then To_Connection
      elsif A.Time_Wait_Taken then Taken_By_Time_Wait
      elsif not A.Usable_Source then Drop
      elsif Plain_SYN (A) and then A.Listening then Open_Passive
      else Refuse)
   with Post =>
     (if Decide'Result = Open_Passive then
        A.SYN and then not A.ACK and then not A.RST and then not A.FIN and then
        A.Listening and then A.Usable_Source and then not A.Has_Connection) and then
     (if Decide'Result = Refuse then A.Usable_Source) and then
     (if A.Has_Connection then Decide'Result = To_Connection);

end TCP_Dispatch;
