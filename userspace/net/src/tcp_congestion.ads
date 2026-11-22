------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  TCP congestion control: NewReno (RFC 5681, RFC 6582), windows in bytes.
--
--  Slow start and congestion avoidance (RFC 5681 3.1), fast retransmit and
--  fast recovery with NewReno's partial acknowledgements (RFC 6582 3.2,
--  full-ACK option 1), the loss window after a timeout, the initial window
--  of RFC 6928 and restart after idle (RFC 5681 4.1). Pure logic: the
--  connection reports acknowledgements, duplicates and timeouts; this
--  decides the window and when to retransmit.
--
--  Proved (tests/net-tcp): the window stays within [SMSS, Maximum_Cwnd]
--  and ssthresh at or above 2 * SMSS; outside recovery an ACK grows the
--  window by at most one SMSS, by no more than it acknowledged in slow
--  start, and in congestion avoidance only once a window's worth of bytes
--  has been acknowledged; fast retransmit happens exactly on the third
--  duplicate ACK, outside recovery, and never for data sent before the
--  last loss (RFC 6582's recover guard); a loss sets ssthresh to
--  max (FlightSize / 2, 2 * SMSS), and a repeated timeout does not lower
--  it again; recovery ends exactly on an ACK covering recover; partial
--  ACKs retransmit and never grow the window; the sender's allowance
--  keeps data in flight within both the congestion and the peer's window.
--
--  Recover follows acknowledgements forward outside recovery, so after
--  more than 2 GiB without loss it is still comparable (modulo 2**32)
--  with SND.UNA; otherwise the guard would eventually refuse every fast
--  retransmit.
--
--  Not here: SACK-based recovery (RFC 6675), RACK-TLP, CUBIC, limited
--  transmit (RFC 3042), ECN.
------------------------------------------------------------------------------
with TCP_Sequence; use TCP_Sequence;
with TCP_Limits;   use TCP_Limits;

package TCP_Congestion with SPARK_Mode is

   Maximum_Cwnd : constant := Maximum_Scaled_Window;

   --  RFC 6928: min (10 * SMSS, max (2 * SMSS, 14600)).
   Initial_Window_Segments : constant := 10;
   Initial_Window_Floor    : constant := 2;        --  segments
   Initial_Window_Bytes    : constant := 14_600;

   --  RFC 5681 equation (4): ssthresh = max (FlightSize / 2, 2 * SMSS).
   Loss_Divisor           : constant := 2;
   Minimum_Ssthresh_Segs  : constant := 2;

   subtype Bytes is Natural range 0 .. Maximum_Cwnd;
   subtype Segment_Size is Natural range 1 .. Maximum_MTU;

   function Initial_Window (SMSS : Segment_Size) return Bytes is
     (Natural'Min (Initial_Window_Segments * SMSS,
                       Natural'Max (Initial_Window_Floor * SMSS, Initial_Window_Bytes)));

   function Loss_Threshold (SMSS : Segment_Size; Flight : Bytes) return Bytes is
     (Natural'Max (Flight / Loss_Divisor, Minimum_Ssthresh_Segs * SMSS));

   type Controller is record
      SMSS        : Segment_Size := Default_IPv4_MSS;
      Cwnd        : Bytes := Default_IPv4_MSS;
      Ssthresh    : Bytes := Maximum_Cwnd;
      In_Recovery : Boolean := False;
      Recover     : Seq := 0;       --  SND.NXT when the last loss was seen
      Dup_Acks    : Natural := 0;
      Acked_Bytes : Bytes := 0;     --  counted towards the next CA increase
   end record;

   function Valid (C : Controller) return Boolean is
     (C.Cwnd >= C.SMSS and then C.Ssthresh >= Minimum_Ssthresh_Segs * C.SMSS and then
      C.Acked_Bytes < C.Cwnd);

   procedure Initialize (C : out Controller; SMSS : Segment_Size; ISS : Seq) with
     Post => Valid (C) and then C.SMSS = SMSS and then
             C.Cwnd = Initial_Window (SMSS) and then C.Ssthresh = Maximum_Cwnd and then
             not C.In_Recovery and then C.Recover = ISS and then C.Dup_Acks = 0;

   type Ack_Action is (Nothing, Retransmit_First);

   --  An ACK that advanced SND.UNA to Ack, acknowledging Acked new bytes;
   --  Flight is what remains in flight. Retransmit_First: a partial ACK
   --  during recovery, so resend the first unacknowledged segment.
   procedure On_Ack (C : in out Controller; Acked : Bytes; Ack : Seq;
                     Flight : Bytes; Action : out Ack_Action)
   with
     Pre  => Valid (C) and then Acked > 0,
     Post => Valid (C) and then C.SMSS = C.SMSS'Old and then C.Dup_Acks = 0 and then
             C.Ssthresh = C.Ssthresh'Old and then
             --  Recovery ends exactly on an ACK covering recover (RFC 6582
             --  3.2 step 3); a partial ACK retransmits.
             C.In_Recovery = (C.In_Recovery'Old and then not Ge (Ack, C.Recover'Old)) and then
             (Action = Retransmit_First) = C.In_Recovery and then
             C.Recover = (if not C.In_Recovery'Old and then Ge (Ack, C.Recover'Old)
                          then Ack else C.Recover'Old) and then
             --  Leaving recovery, the window is at most ssthresh.
             (if C.In_Recovery'Old and then not C.In_Recovery then C.Cwnd <= C.Ssthresh) and then
             --  A partial ACK never grows the window.
             (if C.In_Recovery then C.Cwnd <= C.Cwnd'Old) and then
             --  RFC 5681 3.1: outside recovery the window grows by at most
             --  one SMSS per ACK, in slow start by no more than was
             --  acknowledged, and in congestion avoidance only once a
             --  window's worth has been acknowledged.
             (if not C.In_Recovery'Old then
                C.Cwnd >= C.Cwnd'Old and then C.Cwnd - C.Cwnd'Old <= C.SMSS and then
                (if C.Cwnd'Old < C.Ssthresh then C.Cwnd - C.Cwnd'Old <= Acked
                 elsif C.Cwnd > C.Cwnd'Old then C.Acked_Bytes'Old + Acked >= C.Cwnd'Old));

   type Duplicate_Action is (Nothing, Fast_Retransmit);

   --  A duplicate ACK (RFC 5681 2) with SND.UNA and SND.NXT as they are.
   procedure On_Duplicate_Ack (C : in out Controller; Snd_Una, Snd_Nxt : Seq;
                               Flight : Bytes; Action : out Duplicate_Action)
   with
     Pre  => Valid (C),
     Post => Valid (C) and then C.SMSS = C.SMSS'Old and then
             --  Exactly the third duplicate, outside recovery, and not for
             --  data sent before the last loss (RFC 6582 3.2 steps 1-2).
             (Action = Fast_Retransmit) =
               (not C.In_Recovery'Old and then
                C.Dup_Acks'Old = Duplicate_Threshold - 1 and then
                Ge (Snd_Una, C.Recover'Old)) and then
             (if Action = Fast_Retransmit then
                C.In_Recovery and then C.Recover = Snd_Nxt and then
                C.Ssthresh = Loss_Threshold (C.SMSS, Flight) and then
                C.Cwnd = Natural'Min (C.Ssthresh + Duplicate_Threshold * C.SMSS, Maximum_Cwnd)
              else
                C.In_Recovery = C.In_Recovery'Old and then
                C.Recover = C.Recover'Old and then C.Ssthresh = C.Ssthresh'Old and then
                --  Only recovery inflates the window, by one SMSS.
                (if C.In_Recovery
                 then C.Cwnd = Natural'Min (C.Cwnd'Old + C.SMSS, Maximum_Cwnd)
                 else C.Cwnd = C.Cwnd'Old));

   --  The retransmission timer expired (RFC 5681 3.1, RFC 6582 3.2 step 4).
   --  First: the first retransmission of this segment; later ones keep
   --  ssthresh (RFC 5681: it is not lowered again).
   procedure On_Timeout (C : in out Controller; Flight : Bytes; Snd_Nxt : Seq;
                         First : Boolean)
   with
     Pre  => Valid (C),
     Post => Valid (C) and then C.SMSS = C.SMSS'Old and then
             C.Cwnd = C.SMSS and then
             C.Ssthresh = (if First then Loss_Threshold (C.SMSS, Flight)
                           else C.Ssthresh'Old) and then
             not C.In_Recovery and then C.Recover = Snd_Nxt and then C.Dup_Acks = 0;

   --  The path's MTU fell (RFC 1191, RFC 8201): segments shrink. The
   --  window is left alone; a smaller path is not congestion.
   procedure Reduce_Segment_Size (C : in out Controller; SMSS : Segment_Size) with
     Pre  => Valid (C) and then SMSS < C.SMSS,
     Post => Valid (C) and then C.SMSS = SMSS and then C.Cwnd = C.Cwnd'Old and then
             C.Ssthresh = C.Ssthresh'Old and then C.In_Recovery = C.In_Recovery'Old;

   --  The connection was idle longer than an RTO (RFC 5681 4.1).
   procedure Restart_After_Idle (C : in out Controller) with
     Pre  => Valid (C),
     Post => Valid (C) and then C.SMSS = C.SMSS'Old and then
             C.Cwnd = Natural'Min (C.Cwnd'Old, Initial_Window (C.SMSS)) and then
             C.Ssthresh = C.Ssthresh'Old and then C.In_Recovery = C.In_Recovery'Old and then
             C.Recover = C.Recover'Old;

   --  New bytes the sender may put in flight now.
   function Allowance (C : Controller; Flight : Bytes; Peer_Window : Natural)
     return Natural
   is (if Flight >= Natural'Min (C.Cwnd, Peer_Window) then 0
       else Natural'Min (C.Cwnd, Peer_Window) - Flight)
   with Post => (if Allowance'Result > 0 then
                   Flight + Allowance'Result <= C.Cwnd and then
                   Flight + Allowance'Result <= Peer_Window);
end TCP_Congestion;
