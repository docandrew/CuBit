------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body TCP_Congestion with SPARK_Mode is

   procedure Initialize (C : out Controller; SMSS : Segment_Size; ISS : Seq) is
   begin
      C := (SMSS => SMSS, Cwnd => Initial_Window (SMSS), Ssthresh => Maximum_Cwnd,
            In_Recovery => False, Recover => ISS, Dup_Acks => 0, Acked_Bytes => 0);
   end Initialize;

   procedure On_Ack (C : in out Controller; Acked : Bytes; Ack : Seq;
                     Flight : Bytes; Action : out Ack_Action)
   is
      Old   : constant Bytes := C.Cwnd;
      Total : Natural;
   begin
      Action := Nothing;
      C.Dup_Acks := 0;
      if C.In_Recovery then
         if Ge (Ack, C.Recover) then
            --  Full acknowledgement (RFC 6582 3.2 step 3, option 1).
            C.Cwnd := Natural'Min
              (C.Ssthresh, Natural'Max (Flight, C.SMSS) + C.SMSS);
            C.In_Recovery := False;
         else
            --  Partial: deflate by what was acknowledged, add one SMSS
            --  back if at least that much was (step 5), and resend the
            --  next hole.
            C.Cwnd := Natural'Max
              ((if Acked >= Old then 0 else Old - Acked) +
                 (if Acked >= C.SMSS then C.SMSS else 0),
               C.SMSS);
            Action := Retransmit_First;
         end if;
         C.Acked_Bytes := 0;
      else
         if Ge (Ack, C.Recover) then
            C.Recover := Ack;
         end if;
         if Old < C.Ssthresh then
            --  Slow start (RFC 5681 equation 2).
            C.Cwnd := Natural'Min (Old + Natural'Min (Acked, C.SMSS), Maximum_Cwnd);
         else
            --  Congestion avoidance: one SMSS per window acknowledged
            --  (byte counting, RFC 3465 with L = 1 SMSS).
            Total := C.Acked_Bytes + Acked;
            if Total >= Old then
               C.Cwnd := Natural'Min (Old + C.SMSS, Maximum_Cwnd);
               C.Acked_Bytes := Natural'Min (Total - Old, C.Cwnd - 1);
            else
               C.Acked_Bytes := Total;
            end if;
         end if;
      end if;
   end On_Ack;

   procedure On_Duplicate_Ack (C : in out Controller; Snd_Una, Snd_Nxt : Seq;
                               Flight : Bytes; Action : out Duplicate_Action)
   is
   begin
      Action := Nothing;
      if C.Dup_Acks < Natural'Last then
         C.Dup_Acks := C.Dup_Acks + 1;
      end if;
      if C.In_Recovery then
         --  Each duplicate means a segment left the network (step 4).
         C.Cwnd := Natural'Min (C.Cwnd + C.SMSS, Maximum_Cwnd);
      elsif C.Dup_Acks = Duplicate_Threshold and then Ge (Snd_Una, C.Recover) then
         C.Ssthresh := Loss_Threshold (C.SMSS, Flight);
         --  RFC 5681 3.2 step 3: the three duplicates' segments have left.
         C.Cwnd := Natural'Min (C.Ssthresh + Duplicate_Threshold * C.SMSS, Maximum_Cwnd);
         C.Recover := Snd_Nxt;
         C.In_Recovery := True;
         C.Acked_Bytes := 0;
         Action := Fast_Retransmit;
      end if;
   end On_Duplicate_Ack;

   procedure On_Timeout (C : in out Controller; Flight : Bytes; Snd_Nxt : Seq;
                         First : Boolean)
   is
   begin
      if First then
         C.Ssthresh := Loss_Threshold (C.SMSS, Flight);
      end if;
      C.Cwnd := C.SMSS;                 --  the loss window
      C.In_Recovery := False;
      C.Recover := Snd_Nxt;
      C.Dup_Acks := 0;
      C.Acked_Bytes := 0;
   end On_Timeout;

   procedure Restart_After_Idle (C : in out Controller) is
   begin
      C.Cwnd := Natural'Min (C.Cwnd, Initial_Window (C.SMSS));
      C.Acked_Bytes := 0;
   end Restart_After_Idle;
   procedure Reduce_Segment_Size (C : in out Controller; SMSS : Segment_Size) is
   begin
      C.SMSS := SMSS;
   end Reduce_Segment_Size;

end TCP_Congestion;
