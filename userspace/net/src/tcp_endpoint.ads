------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  One TCP endpoint's send side: the state machine (TCP_Connection)
--  coupled to its send queue (Chunked_Send_Queue), so that what the state
--  machine believes is in flight is exactly what the queue holds.
--
--  Proved (tests/net-tcp): every operation keeps Consistent. In SYN-SENT
--  and SYN-RECEIVED only the SYN is in flight and the queue's first byte
--  is ISS + 1. Once synchronized, SND.UNA and SND.NXT are the queue's,
--  plus one sequence number for a FIN once sent, which is sent only after
--  all queued data. Acknowledgements the state machine accepts free
--  exactly the acknowledged bytes; a retransmission timeout rewinds both
--  views together; a connection reset returns its chunks to the pool.
------------------------------------------------------------------------------
with TCP_Sequence;   use TCP_Sequence;
with TCP_Acceptance; use TCP_Acceptance;
with TCP_Connection; use TCP_Connection;
with Chunk_Pool;
with Chunked_Send_Queue;
with TCP_Receive_Queue;

generic
   with package Chunks is new Chunk_Pool (<>);
   with package Sends is new Chunked_Send_Queue (Chunks => Chunks, others => <>);
   with package Receives is new TCP_Receive_Queue (<>);
package TCP_Endpoint with SPARK_Mode is
   use type Sends.Queue;
   use type Receives.Queue;

   type Endpoint is record
      C   : Connection;
      S   : Sends.Queue;
      R   : Receives.Queue;
      --  Retransmission point: resending runs from here up to SND.NXT,
      --  which only moves forward (an ACK for data sent before a timeout
      --  stays valid).
      Rtx : Seq := 0;
   end record;

   --  The peer's FIN has been consumed.
   subtype Peer_Closed_State is State with
     Static_Predicate => Peer_Closed_State in Close_Wait | Closing | Last_Ack | Time_Wait;

   --  The state machine's send side and the queue agree.
   function Send_Consistent (C : Connection; S : Sends.Queue) return Boolean is
     ((if C.Fin_Sent then C.Fin_Pending) and then
      (case C.St is
         when Closed => True,
         when Listen => not C.Fin_Pending and then not C.Fin_Sent,
         when Syn_Sent | Syn_Received =>
           Sends.Una (S) = C.Snd_Una + 1 and then C.Snd_Nxt = C.Snd_Una + 1 and then
           Sends.Sent (S) = 0 and then not C.Fin_Pending and then not C.Fin_Sent,
         when Established .. Time_Wait =>
           (if C.Fin_Sent then
              --  The FIN follows all the data and takes one number.
              Sends.Sent (S) = Sends.Count (S) and then
              C.Snd_Nxt = Sends.Nxt (S) + 1 and then
              (if C.Snd_Una = C.Snd_Nxt then Sends.Una (S) + 1 = C.Snd_Una
               else Sends.Una (S) = C.Snd_Una)
            else
              Sends.Una (S) = C.Snd_Una and then Sends.Nxt (S) = C.Snd_Nxt)));

   function Consistent (E : Endpoint) return Boolean is
     (Send_Consistent (E.C, E.S) and then
      (if E.C.St in Established .. Time_Wait then
         Le (E.C.Snd_Una, E.Rtx) and then Le (E.Rtx, E.C.Snd_Nxt)));

   --  The state machine's receive side and the receive queue agree: RCV.NXT
   --  is the queue's contiguous edge (plus one once the peer's FIN is
   --  consumed), and the window is the queue's free space.
   function Recv_Consistent (E : Endpoint) return Boolean is
     (case E.C.St is
        when Closed | Listen | Syn_Sent => True,
        when others =>
          Receives.Contiguous (E.R) and then
          E.C.Rcv_Wnd = Seq (Receives.Window (E.R)) and then
          E.C.Rcv_Nxt = Receives.Rcv_Nxt (E.R) + (if E.C.St in Peer_Closed_State then 1 else 0));

   function Valid (E : Endpoint; P : Chunks.Pool) return Boolean is
     (TCP_Connection.Valid (E.C) and then Sends.Valid (E.S, P) and then Consistent (E) and then
      Recv_Consistent (E))
   with Ghost;

   --  CLOSED to SYN-SENT: our SYN is ISS, the first data byte ISS + 1.
   procedure Open_Active (E : out Endpoint; P : Chunks.Pool; Me : Chunks.Owner_Id; ISS : Seq)
   with
     Pre  => Chunks.Valid (P),
     Post => Valid (E, P) and then E.C.St = Syn_Sent and then Sends.Owner_Of (E.S) = Me and then
             Sends.Count (E.S) = 0;

   procedure Open_Passive (E : out Endpoint; P : Chunks.Pool; Me : Chunks.Owner_Id)
   with
     Pre  => Chunks.Valid (P),
     Post => Valid (E, P) and then E.C.St = Listen and then Sends.Owner_Of (E.S) = Me and then
             Sends.Count (E.S) = 0 and then Sends.Chunks_Held (E.S) = 0;

   --  The application writes.
   procedure Write (E : in out Endpoint; P : in out Chunks.Pool; Data : Sends.Byte_Array;
                    Accepted : out Sends.Byte_Count)
   with
     Pre  => Valid (E, P) and then
             E.C.St in Syn_Sent | Syn_Received | Established | Close_Wait and then
             not E.C.Fin_Pending and then
             Data'Length <= Sends.Capacity and then Data'Last < Positive'Last,
     Post => Valid (E, P) and then E.C = E.C'Old and then
             Sends.Owner_Of (E.S) = Sends.Owner_Of (E.S'Old) and then
             Sends.Isolated (Sends.Owner_Of (E.S), P'Old, P) and then
             Sends.Count (E.S) = Sends.Count (E.S'Old) + Accepted;

   --  The application closes (not during SYN-RECEIVED: the engine waits
   --  for the handshake to finish).
   procedure Close (E : in out Endpoint; P : Chunks.Pool) with
     Pre  => Valid (E, P) and then E.C.St /= Syn_Received,
     Post => Valid (E, P) and then E.S = E.S'Old;

   --  The next segment: first whatever is to be resent from the
   --  retransmission point, then up to Limit new bytes from SND.NXT, and
   --  our FIN once everything before it has been sent.
   procedure Next_Segment (E : in out Endpoint; P : Chunks.Pool; Limit : Natural;
                           Data : out Sends.Byte_Array; First : out Seq;
                           Taken : out Sends.Byte_Count; FIN : out Boolean)
   with
     Pre  => Valid (E, P) and then
             E.C.St in Established | Close_Wait | Fin_Wait_1 | Closing | Last_Ack and then
             Data'Length >= Limit and then Data'First = 1,
     Post => Valid (E, P) and then E.C.St = E.C.St'Old and then
             Sends.Owner_Of (E.S) = Sends.Owner_Of (E.S'Old) and then
             First = E.Rtx'Old and then Taken <= Limit and then
             E.Rtx = First + Seq (Taken) + (if FIN then 1 else 0);

   --  A segment arrived (RFC 9293 3.10.7); New_ISS is used if it opens a
   --  passive connection. The queue follows what the state machine
   --  accepted.
   --  Payload is the segment's text (S.Length bytes).
   procedure Segment_Arrived (E : in out Endpoint; P : in out Chunks.Pool; S : Segment;
                              Payload : Receives.Byte_Array; New_ISS : Seq; O : out Outcome)
   with
     Pre  => Valid (E, P) and then Payload'First = 1 and then Payload'Length = Natural (S.Length),
     Post => Valid (E, P) and then Sends.Owner_Of (E.S) = Sends.Owner_Of (E.S'Old) and then
             Sends.Isolated (Sends.Owner_Of (E.S), P'Old, P) and then
             Allowed (E.C'Old.St, E.C.St) and then
             (if E.C.St = Closed and then E.C'Old.St /= Closed then Sends.Chunks_Held (E.S) = 0);

   --  The application reads in-order bytes; the window reopens.
   procedure Read (E : in out Endpoint; P : Chunks.Pool; Output : out Receives.Byte_Array;
                   Got : out Receives.Byte_Count)
   with
     Pre  => Valid (E, P) and then Output'First = 1 and then
             E.C.St not in Closed | Listen | Syn_Sent,
     Post => Valid (E, P) and then E.C.St = E.C.St'Old and then E.S = E.S'Old and then
             E.C.Rcv_Nxt = E.C.Rcv_Nxt'Old;

   --  The retransmission timer expired (or a fast retransmit): resend from
   --  SND.UNA. SND.NXT does not move back.
   procedure Timeout (E : in out Endpoint; P : Chunks.Pool) with
     Pre  => Valid (E, P) and then E.C.St in Syn_Sent | Synchronized_State,
     Post => Valid (E, P) and then E.C = E.C'Old and then
             Sends.Sent (E.S) = Sends.Sent (E.S'Old) and then
             Sends.Owner_Of (E.S) = Sends.Owner_Of (E.S'Old) and then
             (if E.C.St in Established .. Time_Wait then E.Rtx = E.C.Snd_Una);
end TCP_Endpoint;
