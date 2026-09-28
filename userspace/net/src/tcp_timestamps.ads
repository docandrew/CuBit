------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  TCP timestamps (RFC 7323 4-5): PAWS, TS.Recent and RTT samples.
--
--  Timestamps compare modulo 2**32 like sequence numbers (RFC 7323 5.2),
--  so they reuse TCP_Sequence's order. The clock for ages is the kernel's
--  monotonic milliseconds.
--
--  To be proved (tests/net-tcp): a segment is refused by PAWS exactly
--  when it is not a RST, TS.Recent is valid and younger than 24 days, and
--  the segment's TSval is older than TS.Recent (RFC 7323 5.3 R1, R3);
--  after 24 days idle TS.Recent stops being trusted and the next segment
--  sets it (5.5); TS.Recent is taken only from a segment covering
--  Last.ACK.sent (4.3) and never moves backwards while valid; an RTT
--  sample is the time since the echoed TSval, within TCP_RTO's range.
------------------------------------------------------------------------------
with Interfaces;   use Interfaces;
with TCP_Sequence; use TCP_Sequence;
with TCP_RTO;

package TCP_Timestamps with SPARK_Mode is

   --  RFC 7323 5.5: 24 days, in milliseconds.
   Recent_Lifetime : constant := 24 * 24 * 60 * 60 * 1000;

   type State is record
      Recent       : Seq := 0;           --  TS.Recent
      Recent_Valid : Boolean := False;
      Recent_Time  : Unsigned_64 := 0;   --  when TS.Recent was set
   end record;

   function Fresh (S : State; Now : Unsigned_64) return Boolean is
     (S.Recent_Valid and then Now - S.Recent_Time <= Recent_Lifetime)
   with Pre => Now >= S.Recent_Time;

   type Verdict is (Pass, Refuse);   --  Refuse: drop; ACK unless RST

   --  PAWS (RFC 7323 5.3), before the sequence-number check.
   procedure Check (S : in out State; TSval : Seq; RST : Boolean;
                    Now : Unsigned_64; V : out Verdict)
   with
     Pre  => Now >= S.Recent_Time,
     Post => (V = Refuse) =
               (not RST and then Fresh (S'Old, Now) and then Lt (TSval, S'Old.Recent)) and then
             S.Recent = S'Old.Recent and then S.Recent_Time = S'Old.Recent_Time and then
             --  An expired TS.Recent is no longer trusted.
             S.Recent_Valid = Fresh (S'Old, Now);

   --  RFC 7323 4.3 (3): the segment [Seg_Seq, ...] was accepted; take its
   --  TSval if it covers Last.ACK.sent.
   procedure Update (S : in out State; TSval : Seq; Seg_Seq, Last_Ack_Sent : Seq;
                     Now : Unsigned_64)
   with
     Pre  => Now >= S.Recent_Time,
     Post => Now >= S.Recent_Time and then
             (if Le (Seg_Seq, Last_Ack_Sent) and then
                 (not S'Old.Recent_Valid or else Ge (TSval, S'Old.Recent))
              then S.Recent = TSval and then S.Recent_Valid and then S.Recent_Time = Now
              else S = S'Old) and then
             --  TS.Recent never moves backwards while it is valid.
             (if S'Old.Recent_Valid then S.Recent_Valid and then Ge (S.Recent, S'Old.Recent));

   --  RFC 7323 4.1: an ACK that advanced SND.UNA echoes TSecr; Now_TS is
   --  our timestamp clock now.
   function RTT_Sample (Now_TS, TSecr : Seq) return TCP_RTO.Sample is
     (if Distance (TSecr, Now_TS) > TCP_RTO.Maximum_Sample then TCP_RTO.Maximum_Sample
      else Unsigned_32 (Distance (TSecr, Now_TS)))
   with Post => (if Distance (TSecr, Now_TS) <= TCP_RTO.Maximum_Sample
                 then RTT_Sample'Result = Unsigned_32 (Distance (TSecr, Now_TS)));
end TCP_Timestamps;
