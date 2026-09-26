------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body TCP_Acceptance with SPARK_Mode is

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
         Skip   := Distance (Seg_Seq, Rcv_Nxt);
         First  := Rcv_Nxt;
         Offset := 0;
      end if;
      Remaining := Rcv_Wnd - Offset;
      Count := (if Seg_Len - Skip < Remaining then Seg_Len - Skip else Remaining);
   end Trim;
end TCP_Acceptance;
