------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body TCP_Acceptance with SPARK_Mode is

   --  A segment that starts before the window but ends inside it starts
   --  less than its length before RCV.NXT.
   procedure Lemma_Ends_In_Window (S : Seq; L : Segment_Length; N : Seq; W : Window_Size)
   with Ghost, Global => null,
        Pre  => L > 0 and then not In_Window (S, N, W) and then
                In_Window (S + (L - 1), N, W),
        Post => Distance (S, N) < L;
   procedure Lemma_Ends_In_Window (S : Seq; L : Segment_Length; N : Seq; W : Window_Size) is
      E : constant Seq := S + (L - 1) - N;   --  where the segment ends, from N
   begin
      pragma Assert (E < W);
      if E < L - 1 then
         --  Then N - S = (L - 1) - E, below L.
         pragma Assert (N - S = (L - 1) - E);
      else
         --  Then S is E - (L - 1) past N: inside the window.
         pragma Assert (S - N = E - (L - 1));
         pragma Assert (S - N < W);
      end if;
   end Lemma_Ends_In_Window;

   procedure Trim
     (Seg_Seq : Seq; Seg_Len : Segment_Length;
      Rcv_Nxt : Seq; Rcv_Wnd : Window_Size;
      First   : out Seq; Skip : out Segment_Length; Count : out Segment_Length)
   is
      Offset    : Seq;   --  where the kept part starts, from RCV.NXT
      Remaining : Seq;   --  window room from there
   begin
      if In_Window (Seg_Seq, Rcv_Nxt, Rcv_Wnd) then
         --  Starts inside the window: keep from the start.
         Skip   := 0;
         First  := Seg_Seq;
         Offset := Distance (Rcv_Nxt, Seg_Seq);
      else
         --  Starts before RCV.NXT and ends inside: drop the old prefix.
         Lemma_Ends_In_Window (Seg_Seq, Seg_Len, Rcv_Nxt, Rcv_Wnd);
         Skip   := Distance (Seg_Seq, Rcv_Nxt);
         First  := Rcv_Nxt;
         Offset := 0;
      end if;
      Remaining := Rcv_Wnd - Offset;
      Count := (if Seg_Len - Skip < Remaining then Seg_Len - Skip else Remaining);
   end Trim;
end TCP_Acceptance;
