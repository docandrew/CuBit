------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A TCP endpoint with its sending policy: retransmission timing (RFC 6298,
--  Karn's algorithm), NewReno congestion control (RFC 5681, 6582) and
--  segment sizing. Built on TCP_Endpoint, whose proofs it keeps.
--
--  Proved (tests/net-tcp): every operation keeps the endpoint valid and
--  the congestion controller valid; segments never exceed the MSS, the
--  congestion allowance or the peer's window; an RTT is sampled only from
--  a segment that was never retransmitted (Karn); the retransmission
--  timer runs while any sequence space (data, SYN or FIN) is in flight,
--  and restarts, backed off, when it fires (RFC 6298 5.5 - 5.6).
--  The algorithms' own properties are proved in TCP_RTO and
--  TCP_Congestion.
------------------------------------------------------------------------------
with Interfaces;     use Interfaces;
with TCP_Sequence;   use TCP_Sequence;
with TCP_Connection; use TCP_Connection;
with TCP_RTO;
with TCP_Congestion;
with TCP_Endpoint;

generic
   with package Endpoints is new TCP_Endpoint (<>);
package TCP_Flow with SPARK_Mode is
   package EP renames Endpoints;
   use type EP.Endpoint;
   use type TCP_Congestion.Controller;

   subtype Time is Unsigned_64;   --  milliseconds, monotonic

   --  Consecutive timeouts before the connection is abandoned (RFC 9293
   --  3.8.3 R2; Linux's tcp_syn_retries and tcp_retries2 defaults).
   Maximum_Syn_Retries : constant := 6;
   Maximum_Retries     : constant := 15;
   subtype Retry_Count is Natural range 0 .. Maximum_Retries;

   --  Unanswered window probes before the connection is abandoned (as
   --  Linux's tcp_retries2), and the longest wait between probes.
   Maximum_Probes    : constant := Maximum_Retries;
   Maximum_Persist_Ms : constant := 60_000;
   subtype Probe_Count is Natural range 0 .. Maximum_Probes;

   type Flow is record
      E        : EP.Endpoint;
      RTO      : TCP_RTO.Estimator;
      CC       : TCP_Congestion.Controller;
      --  Karn: at most one segment timed, and never a retransmitted one.
      Timing   : Boolean := False;
      Timed_End : Seq := 0;       --  the ACK that covers the timed segment
      Timed_At : Time := 0;
      --  The retransmission timer.
      Armed    : Boolean := False;
      Deadline : Time := 0;
      --  Retransmitting after a timeout (later timeouts keep ssthresh).
      Retrying : Boolean := False;
      --  Timeouts since the last new acknowledgement.
      Retries  : Retry_Count := 0;
      --  The persist timer (RFC 9293 3.8.6.1): data waits behind a zero
      --  window with nothing in flight to draw the window update, so
      --  probes go out until the window opens; Probes counts those the
      --  peer has not answered.
      Persisting : Boolean := False;
      Persist_At : Time := 0;
      Probes     : Probe_Count := 0;
   end record;

   function Valid (F : Flow; P : EP.Chunks.Pool) return Boolean is
     (EP.Valid (F.E, P) and then TCP_Congestion.Valid (F.CC))
   with Ghost;

   --  Bytes sent and not yet acknowledged.
   function Outstanding (F : Flow) return Natural is (EP.Sends.Sent (F.E.S));

   --  Sequence space sent and not yet acknowledged: data, or our SYN or
   --  FIN.
   function In_Flight (F : Flow) return Boolean is
     (Outstanding (F) > 0 or else F.E.C.Snd_Una /= F.E.C.Snd_Nxt);

   --  Too many timeouts without progress: the caller aborts the
   --  connection.
   function Exhausted (F : Flow) return Boolean is
     (F.Retries >= (if F.E.C.St in Syn_Sent | Syn_Received then Maximum_Syn_Retries
                    else Maximum_Retries));

   --  Bytes believed in the network (RFC 6675 "pipe"): after a timeout
   --  everything past the retransmission point counts as lost, so only
   --  what has been resent since counts.
   function Pipe (F : Flow) return TCP_Congestion.Bytes is
     (if Le (F.E.C.Snd_Una, F.E.Rtx) and then Distance (F.E.C.Snd_Una, F.E.Rtx) <= Seq (TCP_Congestion.Maximum_Cwnd)
      then Natural (Distance (F.E.C.Snd_Una, F.E.Rtx)) else TCP_Congestion.Maximum_Cwnd);

   --  Our SYN is sent now; its retransmission timer starts.
   procedure Open_Active (F : out Flow; P : EP.Chunks.Pool; Me : EP.Chunks.Owner_Id;
                          ISS : Seq; MSS : TCP_Congestion.Segment_Size; Now : Time)
   with
     Pre  => EP.Chunks.Valid (P),
     Post => Valid (F, P) and then F.E.C.St = Syn_Sent and then F.Armed and then
             F.Retries = 0;

   procedure Open_Passive (F : out Flow; P : EP.Chunks.Pool; Me : EP.Chunks.Owner_Id;
                           MSS : TCP_Congestion.Segment_Size)
   with
     Pre  => EP.Chunks.Valid (P),
     Post => Valid (F, P) and then F.E.C.St = Listen and then not F.Armed;

   --  The next segment to send now, if any: bounded by the MSS, the
   --  congestion allowance and the peer's window.
   procedure Send (F : in out Flow; P : EP.Chunks.Pool; Now : Time;
                   Data : out EP.Sends.Byte_Array; First : out Seq;
                   Taken : out EP.Sends.Byte_Count; FIN : out Boolean)
   with
     Pre  => Valid (F, P) and then
             F.E.C.St in Established | Close_Wait | Fin_Wait_1 | Closing | Last_Ack and then
             Data'First = 1 and then Data'Length >= F.CC.SMSS,
     Post => Valid (F, P) and then
             Taken <= F.CC.SMSS and then
             Taken <= TCP_Congestion.Allowance
                        (F'Old.CC, Pipe (F'Old), Natural (F'Old.E.C.Snd_Wnd)) and then
             (if In_Flight (F) then F.Armed);

   --  A segment arrived. A passive open's SYN-ACK starts the timer.
   procedure Arrive (F : in out Flow; P : in out EP.Chunks.Pool; Now : Time; S : Segment;
                     Payload : EP.Receives.Byte_Array; New_ISS : Seq; O : out Outcome)
   with
     Pre  => Valid (F, P) and then Payload'First = 1 and then
             Payload'Length = Natural (S.Length),
     Post => Valid (F, P) and then
             EP.Sends.Isolated (EP.Sends.Owner_Of (F.E.S), P'Old, P) and then
             (if F.E.C.St = Syn_Received then F.Armed) and then
             (if F.E.C.St in Closed | Listen then not F.Armed) and then
             (if Distance (F'Old.E.C.Snd_Una, F.E.C.Snd_Una) in 1 .. TCP_Congestion.Maximum_Cwnd and then
                 F.E.C.St in Established .. Time_Wait
              then F.Retries = 0 and then F.Armed = In_Flight (F));

   --  The retransmission deadline passed (RFC 6298 5.4 - 5.7): back off
   --  and restart the timer; the caller resends (the SYN or SYN-ACK, or
   --  through Send).
   procedure Retransmit_Timeout (F : in out Flow; P : EP.Chunks.Pool; Now : Time)
   with
     Pre  => Valid (F, P) and then F.E.C.St in Syn_Sent | Synchronized_State,
     Post => Valid (F, P) and then not F.Timing and then F.Armed and then
             F.Retries = Natural'Min (F'Old.Retries + 1, Maximum_Retries);

   --  An ICMP "packet too big" for this connection, already validated by
   --  the caller: segments shrink to SMSS and everything unacknowledged is
   --  sent again at the new size, at once. No backoff, and the window is
   --  kept (RFC 8201 5.4: not a congestion signal); the resent segments
   --  are not timed (Karn).
   procedure Path_MTU_Reduced (F : in out Flow; P : EP.Chunks.Pool;
                               SMSS : TCP_Congestion.Segment_Size)
   with
     Pre  => Valid (F, P) and then F.E.C.St in Synchronized_State and then
             SMSS < F.CC.SMSS,
     Post => Valid (F, P) and then F.CC.SMSS = SMSS and then F.E.C = F.E.C'Old and then
             F.Retries = F.Retries'Old and then F.Armed = F.Armed'Old and then
             not F.Timing;

   --  Data is waiting behind a zero window, and nothing is in flight whose
   --  acknowledgement would bring the window update.
   function Window_Blocked (F : Flow) return Boolean is
     (F.E.C.St in Established | Close_Wait and then F.E.C.Snd_Wnd = 0 and then
      EP.Sends.Count (F.E.S) > EP.Sends.Sent (F.E.S) and then not In_Flight (F));

   --  After sending or an arrival: start the persist timer when the flow
   --  becomes window-blocked, stop it when it no longer is.
   procedure Update_Persist (F : in out Flow; Now : Time) with
     Post => F.E = F.E'Old and then F.CC = F.CC'Old and then F.Armed = F.Armed'Old and then
             F.Persisting = Window_Blocked (F) and then
             (if F.Persisting and then not F'Old.Persisting then F.Probes = 0);

   --  The peer answered (any acceptable segment): probes are not going
   --  unanswered.
   procedure Probe_Answered (F : in out Flow) with
     Post => F.Probes = 0 and then F.E = F.E'Old and then F.Persisting = F.Persisting'Old;

   --  The persist timer fired: the caller sends a window probe unless
   --  Give_Up, when the peer has stopped answering. The wait doubles, up
   --  to Maximum_Persist_Ms.
   procedure Persist_Timeout (F : in out Flow; Now : Time; Give_Up : out Boolean) with
     Pre  => F.Persisting,
     Post => F.E = F.E'Old and then F.Persisting and then
             Give_Up = (F'Old.Probes >= Maximum_Probes) and then
             (if not Give_Up then
                F.Probes = F'Old.Probes + 1 and then
                (F.Persist_At > Now or else F.Persist_At = Time'Last));
end TCP_Flow;
