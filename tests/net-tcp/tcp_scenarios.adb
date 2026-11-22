--  Linux-hosted scenarios for the TCP state machine (TCP_Connection):
--  whole connections through their lifecycle. The proofs cover each step;
--  these check that the steps compose into working connections.
with Ada.Text_IO;    use Ada.Text_IO;
with TCP_Sequence;   use TCP_Sequence;
with TCP_Connection; use TCP_Connection;

procedure TCP_Scenarios is
   Failures : Natural := 0;

   procedure Check (OK : Boolean; Name : String) is
   begin
      if not OK then
         Put_Line ("FAIL " & Name);
         Failures := Failures + 1;
      end if;
   end Check;

   function Seg (Seq_No, Ack_No : Seq; SYN, ACK, FIN, RST : Boolean := False;
                 Length : TCP_Sequence.Seq := 0) return Segment
   is ((Seq_No => Seq_No, Ack_No => Ack_No, SYN => SYN, ACK => ACK, FIN => FIN,
        RST => RST, Length => Length, Window => 1000));

   A, B : Connection;   --  client and server
   O : Outcome;
begin
   --  Three-way handshake (client ISS 100, server ISS 5000).
   Open_Active (A, 100, 1000);
   Open_Passive (B, 1000);
   Arrive (B, Seg (100, 0, SYN => True), 5000, O);
   Check (B.St = Syn_Received and then O.Answer = Send_Syn_Ack and then B.Rcv_Nxt = 101,
          "server: SYN -> SYN-RECEIVED, SYN-ACK");
   Arrive (A, Seg (5000, 101, SYN => True, ACK => True), 0, O);
   Check (A.St = Established and then O.Happened = Connected and then A.Rcv_Nxt = 5001
          and then A.Snd_Una = 101, "client: SYN-ACK -> ESTABLISHED");
   Arrive (B, Seg (101, 5001, ACK => True), 0, O);
   Check (B.St = Established and then O.Happened = Connected, "server: ACK -> ESTABLISHED");

   --  Data client -> server, in order.
   Data_Sent (A, 10);
   Arrive (B, Seg (101, 5001, ACK => True, Length => 10), 0, O);
   Check (O.Count = 10 and then O.Deliver_First = 101 and then B.Rcv_Nxt = 111
          and then O.Answer = Send_Ack, "in-order data delivered and acknowledged");
   Arrive (A, Seg (5001, 111, ACK => True), 0, O);
   Check (A.Snd_Una = 111, "acknowledgement advances SND.UNA");

   --  Out of order: acknowledged, not delivered (no reassembly yet).
   Arrive (B, Seg (121, 5001, ACK => True, Length => 10), 0, O);
   Check (O.Count = 0 and then B.Rcv_Nxt = 111 and then O.Answer = Send_Ack,
          "out-of-order data acknowledged, not delivered");

   --  Blind attacks (RFC 5961).
   Arrive (B, Seg (115, 0, RST => True), 0, O);
   Check (B.St = Established and then O.Answer = Send_Challenge_Ack,
          "in-window RST not at RCV.NXT: challenge ACK");
   Arrive (B, Seg (111, 0, SYN => True), 0, O);
   Check (B.St = Established and then O.Answer = Send_Challenge_Ack,
          "SYN on an established connection: challenge ACK");
   Arrive (B, Seg (90_000, 0, RST => True), 0, O);
   Check (B.St = Established and then O.Answer = No_Reply,
          "out-of-window RST ignored");
   Arrive (A, Seg (5001, 200, ACK => True), 0, O);
   Check (A.Snd_Una = 111 and then O.Answer = Send_Ack,
          "ACK of unsent data refused");

   --  Active close by the client, passive by the server.
   Close (A);
   Check (A.St = Fin_Wait_1 and then A.Fin_Pending, "client close -> FIN-WAIT-1");
   Fin_Sent_Now (A);                                   --  FIN is seq 111
   Arrive (B, Seg (111, 5001, ACK => True, FIN => True), 0, O);
   Check (B.St = Close_Wait and then B.Rcv_Nxt = 112 and then O.Happened = Peer_Closed,
          "server: FIN -> CLOSE-WAIT");
   Arrive (A, Seg (5001, 112, ACK => True), 0, O);
   Check (A.St = Fin_Wait_2, "client: FIN acknowledged -> FIN-WAIT-2");
   Close (B);
   Check (B.St = Last_Ack, "server close -> LAST-ACK");
   Fin_Sent_Now (B);                                   --  FIN is seq 5001
   Arrive (A, Seg (5001, 112, ACK => True, FIN => True), 0, O);
   Check (A.St = Time_Wait and then A.Rcv_Nxt = 5002, "client: FIN -> TIME-WAIT");
   Arrive (B, Seg (112, 5002, ACK => True), 0, O);
   Check (B.St = Closed and then O.Happened = Closed_Fully, "server: last ACK -> CLOSED");
   Time_Wait_Expired (A);
   Check (A.St = Closed, "client: 2MSL -> CLOSED");

   --  Exact RST ends a connection.
   Open_Active (A, 7, 1000);
   Arrive (A, Seg (900, 8, SYN => True, ACK => True), 0, O);
   Arrive (A, Seg (901, 8, RST => True), 0, O);
   Check (A.St = Closed and then O.Happened = Reset, "RST at RCV.NXT resets");

   --  Refused connection.
   Open_Active (A, 7, 1000);
   Arrive (A, Seg (0, 8, ACK => True, RST => True), 0, O);
   Check (A.St = Closed and then O.Happened = Refused, "RST,ACK to SYN: refused");

   --  Simultaneous close: FIN crosses FIN -> CLOSING -> TIME-WAIT.
   Open_Active (A, 100, 1000);
   Arrive (A, Seg (300, 101, SYN => True, ACK => True), 0, O);
   Close (A);
   Fin_Sent_Now (A);                                   --  FIN is seq 101
   Arrive (A, Seg (301, 101, ACK => True, FIN => True), 0, O);
   Check (A.St = Closing, "simultaneous close -> CLOSING");
   Arrive (A, Seg (302, 102, ACK => True), 0, O);
   Check (A.St = Time_Wait, "CLOSING: FIN acknowledged -> TIME-WAIT");

   --  Crossing FINs as seen under slirp: the peer's FIN reaches us after
   --  ours left (CLOSING), then the peer resends its FIN with the ACK of
   --  ours. That segment ends exactly at RCV.NXT, so it is old (answered
   --  with an ACK, its FIN ignored), but its acknowledgement is current.
   A := (others => <>);
   Open_Active (A, 100, 1000);
   Arrive (A, Seg (300, 101, SYN => True, ACK => True), 0, O);
   Close (A);
   Fin_Sent_Now (A);                                   --  our FIN is seq 101
   Arrive (A, Seg (301, 101, ACK => True, FIN => True), 0, O);
   Check (A.St = Closing and then A.Rcv_Nxt = 302, "crossing FINs -> CLOSING");
   Arrive (A, Seg (301, 102, ACK => True, FIN => True), 0, O);
   Check (A.St = Time_Wait and then A.Rcv_Nxt = 302 and then O.Answer = Send_Ack,
          "a resent FIN acknowledging ours ends CLOSING (its ACK is taken)");
   --  An old segment that does not reach RCV.NXT stays ignored entirely.
   A := (others => <>);
   Open_Active (A, 100, 1000);
   Arrive (A, Seg (300, 101, SYN => True, ACK => True), 0, O);
   Close (A);
   Fin_Sent_Now (A);
   Arrive (A, Seg (301, 101, ACK => True, Length => 5), 0, O);   --  data 301 .. 305
   Arrive (A, Seg (301, 102, ACK => True, Length => 2), 0, O);   --  old, ends at 302
   Check (A.St = Fin_Wait_1 and then A.Snd_Una = 101,
          "an old segment ending before RCV.NXT is ignored, its ACK too");

   --  A segment to a closed port draws a reset.
   A := (others => <>);
   Arrive (A, Seg (55, 0, SYN => True), 0, O);
   Check (O.Answer = Send_Reset_Ack and then O.Reset_Ack = 56, "closed port: RST,ACK");

   Put_Line (if Failures = 0 then "TCP-SCENARIOS: PASS" else "TCP-SCENARIOS: FAIL");
end TCP_Scenarios;
