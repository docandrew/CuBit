------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A TCP connection's state machine: segment arrival (RFC 9293 3.10.7)
--  with RFC 5961's defences, and the application's open and close.
--
--  Pure logic over parsed header fields (from the RecordFlux parsers);
--  it decides the next state, what to send in reply, which bytes to
--  deliver and how far the peer has acknowledged. It owns no buffers: the
--  send queue (TCP_Send_Queue) and the receive side follow its results.
--
--  Proved (tests/net-tcp):
--  - every state change is an edge of RFC 9293's diagram (Allowed);
--  - a RST only ends a synchronized connection if its sequence number is
--    exactly RCV.NXT (RFC 5961 3.2); elsewhere in the window it draws a
--    challenge ACK;
--  - a SYN on a synchronized connection never changes its state (RFC 5961
--    4.2: challenge ACK);
--  - data is delivered only in the data states (and with the ACK that
--    completes the handshake), only in order from
--    RCV.NXT and only within the receive window; RCV.NXT advances by what
--    is delivered, plus one for a FIN;
--  - SND.UNA never moves backwards and never passes SND.NXT.
--  Not modelled here: out-of-order reassembly (out-of-order data is
--  acknowledged and dropped for now), urgent data, timers other than
--  TIME-WAIT's end, congestion control.
------------------------------------------------------------------------------
with TCP_Sequence;   use TCP_Sequence;
with TCP_Acceptance; use TCP_Acceptance;

package TCP_Connection with SPARK_Mode is

   type State is
     (Closed, Listen, Syn_Sent, Syn_Received, Established, Fin_Wait_1,
      Fin_Wait_2, Close_Wait, Closing, Last_Ack, Time_Wait);

   subtype Synchronized_State is State range Syn_Received .. Time_Wait;
   subtype Data_State is State range Established .. Fin_Wait_2;

   --  RFC 9293 figure 5, plus the resets of 3.10.7 (any state to CLOSED;
   --  SYN-RECEIVED back to LISTEN for a passive open).
   function Allowed (From, To : State) return Boolean is
     (From = To or else To = Closed or else
      (case From is
         when Closed       => To in Listen | Syn_Sent,
         when Listen       => To in Syn_Received | Syn_Sent,
         when Syn_Sent     => To in Syn_Received | Established,
         --  Close_Wait: one segment can carry the handshake's ACK and a FIN.
         when Syn_Received => To in Established | Fin_Wait_1 | Close_Wait | Listen,
         when Established  => To in Fin_Wait_1 | Close_Wait,
         when Fin_Wait_1   => To in Fin_Wait_2 | Closing | Time_Wait,
         when Fin_Wait_2   => To = Time_Wait,
         when Close_Wait   => To = Last_Ack,
         when Closing      => To = Time_Wait,
         when Last_Ack     => False,
         when Time_Wait    => False));

   type Connection is record
      St       : State := Closed;
      Passive  : Boolean := False;   --  opened by LISTEN
      ISS, IRS : Seq := 0;
      Snd_Una, Snd_Nxt : Seq := 0;
      Snd_Wnd  : Window_Size := 0;
      --  The segment that last set SND.WND (RFC 9293 3.3.1): its sequence
      --  and acknowledgement numbers, so an older segment cannot undo a
      --  newer window.
      Snd_Wl1, Snd_Wl2 : Seq := 0;
      Rcv_Nxt  : Seq := 0;
      Rcv_Wnd  : Window_Size := 0;
      Fin_Pending : Boolean := False;   --  the application closed
      Fin_Sent    : Boolean := False;   --  our FIN occupies Snd_Nxt - 1
   end record;

   --  SND.UNA .. SND.NXT is a valid in-flight range.
   function Valid (C : Connection) return Boolean is
     (Distance (C.Snd_Una, C.Snd_Nxt) <= Maximum_Window);

   --  Parsed header of an arriving segment. Length counts data only.
   type Segment is record
      Seq_No, Ack_No : Seq := 0;
      SYN, ACK, FIN, RST : Boolean := False;
      Length : Segment_Length := 0;
      Window : Window_Size := 0;
   end record;

   --  RFC 9293 3.10.7.4: an acceptable ACK updates the send window when
   --  SND.UNA =< SEG.ACK =< SND.NXT and the segment is no older than the
   --  one that last set it. A pure window update (no new data
   --  acknowledged) is taken; a reordered older segment is not.
   function Updates_Window (C : Connection; S : Segment) return Boolean is
     (Le (C.Snd_Una, S.Ack_No) and then Le (S.Ack_No, C.Snd_Nxt) and then
      (Lt (C.Snd_Wl1, S.Seq_No) or else
       (C.Snd_Wl1 = S.Seq_No and then Le (C.Snd_Wl2, S.Ack_No))));

   type Reply is
     (No_Reply,
      Send_Ack,              --  <SEQ=SND.NXT><ACK=RCV.NXT><CTL=ACK>
      Send_Challenge_Ack,    --  same segment, RFC 5961
      Send_Syn_Ack,          --  <SEQ=ISS><ACK=RCV.NXT><CTL=SYN,ACK>
      Send_Reset,            --  <SEQ=Reset_Seq><CTL=RST>
      Send_Reset_Ack);       --  <SEQ=0><ACK=Reset_Ack><CTL=RST,ACK>

   type Event is (None, Connected, Peer_Closed, Refused, Reset, Closed_Fully);

   type Outcome is record
      Answer    : Reply := No_Reply;
      Reset_Seq : Seq := 0;
      Reset_Ack : Seq := 0;
      Happened  : Event := None;
      --  Data to deliver: Count bytes starting Skip bytes into the
      --  segment's data (sequence number Deliver_First).
      Deliver_First : Seq := 0;
      Skip, Count   : Segment_Length := 0;
   end record;

   --  An arriving segment (RFC 9293 3.10.7). New_ISS is the initial
   --  sequence number to use if this segment opens a passive connection.
   procedure Arrive (C : in out Connection; S : Segment; New_ISS : Seq;
                     O : out Outcome)
   with
     Pre  => Valid (C),
     Post => Valid (C) and then
       Allowed (C'Old.St, C.St) and then
       --  Our FIN's state is the application's and the sender's, not the
       --  peer's.
       C.Fin_Pending = C'Old.Fin_Pending and then C.Fin_Sent = C'Old.Fin_Sent and then
       --  A passive open sends a SYN at New_ISS.
       (if C'Old.St = Listen and then C.St = Syn_Received then
          C.Snd_Una = New_ISS and then C.Snd_Nxt = New_ISS + 1) and then
       --  In SYN-SENT only the SYN is in flight, and a SYN-ACK acknowledges
       --  exactly it.
       (if C'Old.St = Syn_Sent and then C'Old.Snd_Nxt = C'Old.Snd_Una + 1 then
          C.Snd_Nxt = C'Old.Snd_Nxt and then
          (if C.St = Established then C.Snd_Una = C'Old.Snd_Nxt else C.Snd_Una = C'Old.Snd_Una)) and then
       --  Segments never open a connection from CLOSED, and a listener
       --  only moves to SYN-RECEIVED (opening actively is the
       --  application's).
       (if C'Old.St = Closed then C.St = Closed) and then
       (if C'Old.St = Listen then C.St in Listen | Syn_Received) and then
       --  Staying in SYN-RECEIVED moves nothing.
       (if C'Old.St = Syn_Received and then C.St = Syn_Received then
          C.Snd_Una = C'Old.Snd_Una and then C.Snd_Nxt = C'Old.Snd_Nxt) and then
       --  Completing a passive handshake acknowledges exactly our SYN.
       (if C'Old.St = Syn_Received and then C'Old.Snd_Nxt = C'Old.Snd_Una + 1 and then
           C.St not in Syn_Received | Closed | Listen
        then C.Snd_Una = C'Old.Snd_Nxt) and then
       --  RFC 5961 3.2: a RST ends a synchronized connection only exactly
       --  at RCV.NXT.
       (if C'Old.St in Synchronized_State and then S.RST and then C.St /= C'Old.St
        then S.Seq_No = C'Old.Rcv_Nxt) and then
       --  RFC 5961 4.2: a SYN never moves a synchronized connection.
       (if C'Old.St in Synchronized_State and then S.SYN and then not S.RST
        then C.St = C'Old.St) and then
       --  Delivery: data states (or the segment whose ACK completes the
       --  handshake, which may carry data), in order, in window.
       (if O.Count > 0 then
          C'Old.St in Data_State | Syn_Received and then
          O.Deliver_First = C'Old.Rcv_Nxt and then
          O.Count <= C'Old.Rcv_Wnd and then
          O.Skip + O.Count <= S.Length) and then
       --  RCV.NXT advances by exactly what was delivered, plus a FIN.
       (if C'Old.St in Synchronized_State then
          C.Rcv_Nxt = C'Old.Rcv_Nxt + O.Count or else
          C.Rcv_Nxt = C'Old.Rcv_Nxt + O.Count + 1 or else
          C.St = Closed or else C.St = Listen) and then
       --  Acknowledgements move SND.UNA forward only, never past SND.NXT.
       (if C'Old.St in Synchronized_State and then C.St /= Closed and then C.St /= Listen
        then Le (C'Old.Snd_Una, C.Snd_Una) and then
             Le (C.Snd_Una, C.Snd_Nxt) and then C.Snd_Nxt = C'Old.Snd_Nxt);

   --  The application opens actively: CLOSED -> SYN-SENT, send SYN.
   procedure Open_Active (C : in out Connection; ISS : Seq; Window : Window_Size)
   with Pre => C.St = Closed,
        Post => C.St = Syn_Sent and then C.Snd_Una = ISS and then
                C.Snd_Nxt = ISS + 1 and then Valid (C) and then
                not C.Fin_Pending and then not C.Fin_Sent;

   --  The application listens: CLOSED -> LISTEN.
   procedure Open_Passive (C : in out Connection; Window : Window_Size)
   with Pre => C.St = Closed,
        Post => C.St = Listen and then Valid (C) and then
                not C.Fin_Pending and then not C.Fin_Sent;

   --  The application closes: ESTABLISHED and SYN-RECEIVED -> FIN-WAIT-1,
   --  CLOSE-WAIT -> LAST-ACK (our FIN is then pending until the queued data
   --  has been sent); LISTEN and SYN-SENT -> CLOSED.
   procedure Close (C : in out Connection) with
     Pre  => Valid (C),
     Post => Valid (C) and then Allowed (C'Old.St, C.St) and then
             C.Snd_Nxt = C'Old.Snd_Nxt and then C.Snd_Una = C'Old.Snd_Una and then
             C.Fin_Sent = C'Old.Fin_Sent and then
             C.St = (case C'Old.St is
                       when Listen | Syn_Sent => Closed,
                       when Syn_Received | Established => Fin_Wait_1,
                       when Close_Wait => Last_Ack,
                       when others => C'Old.St) and then
             C.Fin_Pending = (C'Old.Fin_Pending or else
                              C'Old.St in Syn_Received | Established | Close_Wait) and then
             C.Rcv_Nxt = C'Old.Rcv_Nxt and then C.Rcv_Wnd = C'Old.Rcv_Wnd;

   --  N data bytes were sent from SND.NXT.
   procedure Data_Sent (C : in out Connection; N : Segment_Length) with
     Pre  => Valid (C) and then not C.Fin_Sent and then
             Distance (C.Snd_Una, C.Snd_Nxt) <= Maximum_Window - N,
     Post => Valid (C) and then C.Snd_Nxt = C'Old.Snd_Nxt + N and then
             C.St = C'Old.St and then C.Snd_Una = C'Old.Snd_Una and then
             C.Fin_Pending = C'Old.Fin_Pending and then not C.Fin_Sent and then
             C.Rcv_Nxt = C'Old.Rcv_Nxt and then C.Rcv_Wnd = C'Old.Rcv_Wnd;

   --  Our pending FIN was sent: it takes one sequence number.
   procedure Fin_Sent_Now (C : in out Connection) with
     Pre  => Valid (C) and then C.Fin_Pending and then not C.Fin_Sent and then
             Distance (C.Snd_Una, C.Snd_Nxt) < Maximum_Window,
     Post => Valid (C) and then C.Fin_Sent and then
             C.Snd_Nxt = C'Old.Snd_Nxt + 1 and then C.St = C'Old.St and then
             C.Snd_Una = C'Old.Snd_Una and then C.Fin_Pending and then
             C.Rcv_Nxt = C'Old.Rcv_Nxt and then C.Rcv_Wnd = C'Old.Rcv_Wnd;

   --  TIME-WAIT's 2*MSL timer expired.
   procedure Time_Wait_Expired (C : in out Connection) with
     Pre  => C.St = Time_Wait,
     Post => C.St = Closed;
end TCP_Connection;
