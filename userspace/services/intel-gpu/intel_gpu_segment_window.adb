package body Intel_GPU_Segment_Window with SPARK_Mode is

   function Initial (First_Bytes : Unsigned_32; First_Value : Value) return Window is
      Result : Window;
   begin
      Result.Items (1) := (Start => 0, Stop => Position (First_Bytes), Seq => First_Value);
      Result.Size := 1;
      Result.First := 0;
      Result.Last := Position (First_Bytes);
      return Result;
   end Initial;

   function Plan (W : Window; Bytes : Unsigned_32) return RR.Plan is
     (RR.Reserve (Ring_Offset (Head (W)), Ring_Offset (Tail (W)), Bytes));

   -- The ring distance from tail to head is what the window does not span.
   procedure Lemma_Distance (W : Window)
     with Ghost, Global => null, Pre => Valid (W),
          Post => RR.Free_Distance (Ring_Offset (Head (W)), Ring_Offset (Tail (W))) =
                  Unsigned_32 (Ring_Bytes - (Tail (W) - Head (W)));
   procedure Lemma_Distance (W : Window) is null;

   procedure Append (W : in out Window; Bytes : Unsigned_32; Next : Value) is
      P : constant RR.Plan := Plan (W, Bytes);
      Start : constant Position := W.Last + Position (P.Padding);
   begin
      Lemma_Distance (W);
      pragma Assert (P.Consumed + Guard_Bytes <=
                     Unsigned_32 (Ring_Bytes - (W.Last - W.First)));
      pragma Assert (P.Consumed mod Command_Alignment = 0);
      pragma Assert (P.Padding mod Command_Alignment = 0);
      pragma Assert (Start mod Command_Alignment = 0);
      pragma Assert (Start < W.Last + Position (P.Consumed));
      pragma Assert (W.Last + Position (P.Consumed) - W.First <= Span_Limit);
      pragma Assert (for all I in 1 .. W.Size => W.Items (I).Stop <= Start);
      pragma Assert (for all I in 1 .. W.Size => W.Items (I).Seq < Next);
      W.Size := W.Size + 1;
      W.Items (W.Size) := (Start => Start, Stop => W.Last + Position (P.Consumed), Seq => Next);
      W.Last := W.Last + Position (P.Consumed);
      pragma Assert (W.Last mod Command_Alignment = 0);
      pragma Assert (for all I in 1 .. W.Size =>
                       W.Items (I).Start < W.Items (I).Stop and then
                       W.Items (I).Start mod Command_Alignment = 0 and then
                       W.Items (I).Start >= W.First and then W.Items (I).Stop <= W.Last);
      pragma Assert (for all I in 1 .. W.Size =>
                       (for all J in I + 1 .. W.Size =>
                          W.Items (I).Stop <= W.Items (J).Start and then
                          W.Items (I).Seq < W.Items (J).Seq));
      pragma Assert (W.Items (1).Start = W.First);
   end Append;

   procedure Retire (W : in out Window; Completed : Value) is
      Old : constant Window := W with Ghost;
      Old_Items : Segment_Array with Ghost;
   begin
      loop
         pragma Loop_Invariant (Valid (W));
         pragma Loop_Invariant (W.Size >= 1 and W.Size <= Old.Size);
         pragma Loop_Invariant (W.Last = Old.Last and W.First >= Old.First);
         pragma Loop_Invariant (Last_Value (W) = Last_Value (Old));
         exit when W.Size < 2 or else W.Items (2).Seq > Completed;
         Old_Items := W.Items;
         for I in 1 .. W.Size - 1 loop
            W.Items (I) := W.Items (I + 1);
            pragma Loop_Invariant
              (for all J in 1 .. I => W.Items (J) = Old_Items (J + 1));
            pragma Loop_Invariant
              (for all J in I + 1 .. Max_Live => W.Items (J) = Old_Items (J));
         end loop;
         pragma Assert (for all J in 1 .. W.Size - 1 => W.Items (J) = Old_Items (J + 1));
         pragma Assert (for all J in 1 .. W.Size - 1 =>
                          (for all K in J + 1 .. W.Size - 1 =>
                             W.Items (J).Stop <= W.Items (K).Start and then
                             W.Items (J).Seq < W.Items (K).Seq));
         pragma Assert (for all J in 1 .. W.Size - 1 =>
                          W.Items (J).Start >= W.Items (1).Start and then
                          W.Items (J).Start mod Command_Alignment = 0 and then
                          W.Items (J).Start < W.Items (J).Stop);
         W.Size := W.Size - 1;
         W.First := W.Items (1).Start;
         pragma Assert (W.Items (W.Size).Stop = W.Last);
         pragma Assert (for all I in 1 .. W.Size =>
                          W.Items (I).Start < W.Items (I).Stop and then
                          W.Items (I).Start >= W.First and then W.Items (I).Stop <= W.Last);
      end loop;
   end Retire;

   procedure Lemma_No_Overwrite (W : Window; Bytes : Unsigned_32; X, Y : Position) is
   begin
      Lemma_Distance (W);
      pragma Assert (Y - X < Ring_Bytes);
      pragma Assert (Y - X > 0);
   end Lemma_No_Overwrite;

end Intel_GPU_Segment_Window;
