------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Which part of an arriving segment the receiver keeps (RFC 9293
--  section 3.10.7.4).
--
--  Acceptable is the RFC's four-case test. Trim gives the part of the
--  segment inside the receive window; its postcondition is the safety
--  property that matters: kept data lies within both the segment and the
--  window, so nothing outside the window is ever stored or acknowledged.
--  (Proved with gnatprove, tests/net-tcp.)
------------------------------------------------------------------------------
with TCP_Sequence; use TCP_Sequence;

package TCP_Acceptance with SPARK_Mode, Pure is

   --  Receive windows (scaled) and segment lengths stay below half the
   --  sequence space, as TCP requires (RFC 7323: window < 2**30).
   Maximum_Window : constant Seq := 2 ** 30;
   subtype Window_Size is Seq range 0 .. Maximum_Window;
   subtype Segment_Length is Seq range 0 .. Maximum_Window;

   --  RFC 9293 3.10.7.4. SEG.LEN counts SYN and FIN as one each.
   function Acceptable
     (Seg_Seq : Seq; Seg_Len : Segment_Length;
      Rcv_Nxt : Seq; Rcv_Wnd : Window_Size) return Boolean
   is
     (if Seg_Len = 0 then
        (if Rcv_Wnd = 0 then Seg_Seq = Rcv_Nxt
         else In_Window (Seg_Seq, Rcv_Nxt, Rcv_Wnd))
      else
        (Rcv_Wnd /= 0 and then
           (In_Window (Seg_Seq, Rcv_Nxt, Rcv_Wnd) or else
            In_Window (Seg_Seq + (Seg_Len - 1), Rcv_Nxt, Rcv_Wnd))));

   --  The kept part of an acceptable segment with data: it starts at
   --  First (a sequence number) and is Count long, and Skip octets of the
   --  segment's data precede it (already received).
   procedure Trim
     (Seg_Seq : Seq; Seg_Len : Segment_Length;
      Rcv_Nxt : Seq; Rcv_Wnd : Window_Size;
      First   : out Seq; Skip : out Segment_Length; Count : out Segment_Length)
   with
     Pre  => Seg_Len > 0 and then
             Acceptable (Seg_Seq, Seg_Len, Rcv_Nxt, Rcv_Wnd),
     Post =>
       --  Kept data is part of the segment ...
       Skip + Count <= Seg_Len and then
       First = Seg_Seq + Skip and then
       --  ... and lies inside the window.
       Count <= Rcv_Wnd and then
       Distance (Rcv_Nxt, First) + Count <= Rcv_Wnd and then
       --  Nothing earlier than RCV.NXT is kept again unless it was
       --  never received (the segment starts in the window).
       (if In_Window (Seg_Seq, Rcv_Nxt, Rcv_Wnd) then Skip = 0
        else First = Rcv_Nxt) and then
       --  An acceptable segment always keeps something.
       Count > 0;
end TCP_Acceptance;
