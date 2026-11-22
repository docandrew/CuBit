------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The reset for a segment that belongs to no connection (RFC 9293
--  3.10.7.1, CLOSED state): a closed port refuses at once rather than
--  leaving the peer to time out, and a stale peer learns that its
--  connection is gone.
--  - A segment carrying RST is never answered (no reset storms).
--  - With ACK: <SEQ=SEG.ACK><CTL=RST>.
--  - Without: <SEQ=0><ACK=SEG.SEQ+SEG.LEN><CTL=RST,ACK>, where SEG.LEN
--    counts the data and one each for SYN and FIN.
--
--  Proved (tests/net-tcp): exactly that rule.
------------------------------------------------------------------------------
with TCP_Sequence; use TCP_Sequence;

package TCP_Reset with SPARK_Mode is

   Maximum_Data : constant := 65_535;
   subtype Data_Length is Natural range 0 .. Maximum_Data;

   type Arrival is record
      SYN, ACK, FIN, RST : Boolean := False;
      Seq_No, Ack_No     : Seq := 0;
      Length             : Data_Length := 0;
   end record;

   type Reply is record
      Send     : Boolean := False;
      With_ACK : Boolean := False;
      Seq_No   : Seq := 0;
      Ack_No   : Seq := 0;
   end record;

   --  SEG.LEN: the data, and one for each of SYN and FIN.
   function Segment_Length (A : Arrival) return Seq is
     (Seq (A.Length) + (if A.SYN then 1 else 0) + (if A.FIN then 1 else 0));

   function For_Closed (A : Arrival) return Reply with
     Post => (if A.RST then not For_Closed'Result.Send
              elsif A.ACK then
                For_Closed'Result.Send and then not For_Closed'Result.With_ACK and then
                For_Closed'Result.Seq_No = A.Ack_No
              else
                For_Closed'Result.Send and then For_Closed'Result.With_ACK and then
                For_Closed'Result.Seq_No = 0 and then
                For_Closed'Result.Ack_No = A.Seq_No + Segment_Length (A) and then
                --  Stated on its own: SEG.SEQ + SEG.LEN, SYN and FIN counted.
                For_Closed'Result.Ack_No =
                  A.Seq_No + Seq (A.Length) + (if A.SYN then 1 else 0) +
                  (if A.FIN then 1 else 0));

end TCP_Reset;
