------------------------------------------------------------------------------
--  Hosted tests of the charts core (userspace/lib/charts, MET-002's app):
--  time and value axes, exact column quantiles against a sorted reference,
--  tier-window merging, the column ring's scroll and resample, histogram
--  buckets, and labels. Linux-hosted, assertions on.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Charts_Buckets;
with Charts_Columns;
with Charts_Format;
with Charts_Palette;
with Charts_Quantiles;
with Charts_Time_Axis;
with Charts_Value_Axis;

procedure Charts_Core_Tests is
   package Axis renames Charts_Time_Axis;
   package Values renames Charts_Value_Axis;
   package Columns renames Charts_Columns;
   package Q renames Charts_Quantiles;
   use type Columns.Origin_Kind;

   Passed, Failed : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      if Condition then
         Passed := Passed + 1;
      else
         Failed := Failed + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   --  A small deterministic generator (xorshift64).
   State : Unsigned_64 := 16#9E37_79B9_7F4A_7C15#;
   function Next return Unsigned_64 is
   begin
      State := State xor Shift_Left (State, 13);
      State := State xor Shift_Right (State, 7);
      State := State xor Shift_Left (State, 17);
      return State;
   end Next;

   procedure Sort (Items : in out Q.Sample_Array) is
   begin
      for I in Items'First + 1 .. Items'Last loop
         declare
            V : constant Unsigned_64 := Items (I);
            J : Natural := I;
         begin
            while J > Items'First and then Items (J - 1) > V loop
               Items (J) := Items (J - 1);
               J := J - 1;
            end loop;
            Items (J) := V;
         end;
      end loop;
   end Sort;

   procedure Time_Axis is
      V : constant Axis.Viewport := Axis.Ending_At (10_000_000, Axis.Width_For (5_000_000, 1_000), 1_000);
   begin
      Check (V.Width = 5_000, "5 s over 1000 columns is 5 ms a column");
      Check (Axis.Width_For (1_000_001, 1_000) = 1_001, "widths round up");
      Check (V.Last = 2_000 and then Axis.First (V) = 1_001, "viewport ends at the clock's column");
      Check (Axis.X_Of (V, V.Last) = 999 and then Axis.X_Of (V, Axis.First (V)) = 0, "x of the edges");
      Check (Axis.Column_At (V, 999) = V.Last and then Axis.Has_Column_At (V, 0), "column at x");
      declare
         Early : constant Axis.Viewport := Axis.Ending_At (100_000, 5_000, 1_000);
      begin
         Check (not Axis.Has_Column_At (Early, 0) and then Axis.Has_Column_At (Early, 999),
                "nothing left of the clock's zero just after boot");
         Check (Axis.First (Early) = 0, "first column clamps to zero");
      end;
      Check (Axis.Shifted (V, -10).Last = 1_990 and then Axis.Shifted (V, 25).Last = 2_025, "shift");
      Check (Axis.Shifted (V, -1_000_000).Last = 0, "shift clamps at zero");
      Check (Axis.Tick_Step (5_000, 80) = 500_000, "ticks at least 80 px apart at 5 ms/px: 500 ms");
      Check (Axis.Tick_Step (300_000, 80) = 30_000_000, "300 ms/px: 30 s");
      Check (Axis.Tick_Step (Axis.Maximum_Span_Us, 4_096) = Axis.Steps (Axis.Tick_Steps), "ticks cap");
      Check (Axis.First_Tick (1_234_567, 1_000_000) = 2_000_000, "first tick");
   end Time_Axis;

   procedure Value_Axis is
   begin
      Check (Values.Nice_Ceiling (0) = 1 and then Values.Nice_Ceiling (1) = 1, "nice of 0 and 1");
      Check (Values.Nice_Ceiling (3) = 5 and then Values.Nice_Ceiling (7) = 10 and then
             Values.Nice_Ceiling (11) = 20 and then Values.Nice_Ceiling (250) = 500, "nice 1-2-5");
      Check (Values.Nice_Ceiling (Values.Value_Ceiling) = Values.Value_Ceiling, "nice at the ceiling");
      Check (Values.Tick_Step (1_000, 5) = 200 and then Values.Axis_Top (930, 5) = 1_000, "ticks and top");
      Check (Values.Axis_Top (260, 4) = 300, "top of 260 in 4 ticks: 300 (100 apart)");
      Check (Values.Row_Of (0, 100, 101) = 100 and then Values.Row_Of (100, 100, 101) = 0 and then
             Values.Row_Of (50, 100, 101) = 50 and then Values.Row_Of (5_000, 100, 101) = 0, "rows");
      for I in 1 .. 2_000 loop
         declare
            V : constant Values.Value := Next mod Values.Value_Ceiling;
            N : constant Values.Value := Values.Nice_Ceiling (V);
         begin
            Check (N >= V and then (2 * N <= 5 * V or else N = Values.Value_Ceiling or else V <= 1),
                   "nice ceiling within 2.5x");
         end;
      end loop;
   end Value_Axis;

   procedure Quantiles is
   begin
      for Round in 1 .. 300 loop
         declare
            N : constant Positive := 1 + Natural (Next mod 700);
            Spread : constant Unsigned_64 := (if Round mod 3 = 0 then 3 else 100_000);
            Items, Reference : Q.Sample_Array (0 .. N - 1);
            S : Columns.Summary;
         begin
            for I in Items'Range loop
               Items (I) := Next mod Spread;
            end loop;
            Reference := Items;
            Sort (Reference);
            Columns.Summarize_Raw (Items, 42, S);
            Check (S.Origin = Columns.Raw and then S.Count = Unsigned_64 (N), "raw count");
            Check (S.Minimum = Reference (0) and then S.Maximum = Reference (N - 1), "raw extremes");
            Check (S.P50 = Reference (Q.Rank (Q.Median, N) - 1), "exact median, nearest rank");
            Check (S.P99 = Reference (Q.Rank (Q.P99, N) - 1), "exact p99, nearest rank");
            Check (S.Last = 42, "last is the caller's");
         end;
      end loop;
      Check (Q.Rank (Q.P99, 1) = 1 and then Q.Rank (Q.P99, 100) = 99 and then Q.Rank (Q.Median, 3) = 2, "ranks");
   end Quantiles;

   procedure Windows is
      One : constant Columns.Window_Rows (1 .. 1) :=
        [1 => (Start_Us => 0, Count => 10, Sum => 100, Minimum => 1, Maximum => 40, Last => 7, P50 => 9, P99 => 39)];
      Three : constant Columns.Window_Rows (1 .. 3) :=
        [(Start_Us => 0, Count => 100, Sum => 1_000, Minimum => 2, Maximum => 50, Last => 3, P50 => 10, P99 => 40),
         (Start_Us => 1, Count => 10, Sum => 900, Minimum => 5, Maximum => 900, Last => 8, P50 => 90, P99 => 800),
         (Start_Us => 2, Count => 50, Sum => 600, Minimum => 1, Maximum => 30, Last => 6, P50 => 12, P99 => 25)];
      S : Columns.Summary := Columns.Summarize_Windows (One);
   begin
      Check (S.Origin = Columns.Window and then S.P50 = 9 and then S.P99 = 39, "one window keeps its sketch");
      S := Columns.Summarize_Windows (Three);
      Check (S.Origin = Columns.Merged and then S.Count = 160 and then S.Sum = 2_500, "merged counts");
      Check (S.Minimum = 1 and then S.Maximum = 900 and then S.Last = 6, "merged extremes and last");
      Check (S.P99 = 800, "merged p99 is the largest: a spike is never hidden");
      Check (S.P50 = 10, "merged p50 is the count-weighted median of medians");
      Check (Columns.Mean (S) = 15, "mean");
   end Windows;

   procedure Ring is
      R : Columns.Ring (8);
      Wide : Columns.Ring (4);
      function One (V : Unsigned_64) return Columns.Summary is
        ((Origin => Columns.Raw, Count => 1, Sum => V, Minimum => V, Maximum => V, Last => V, P50 => V, P99 => V));
   begin
      Columns.Reset (R, 100);
      for C in Unsigned_64'(93) .. 100 loop
         Columns.Put (R, C, One (C));
      end loop;
      Check (Columns.Get (R, 93).P50 = 93 and then Columns.Get (R, 100).P50 = 100, "ring holds its columns");
      Check (Columns.Get (R, 92).Origin = Columns.Empty and then Columns.Get (R, 101).Origin = Columns.Empty,
             "outside the ring is empty");
      Columns.Move_To (R, 103);
      Check (Columns.Get (R, 96).P50 = 96 and then Columns.Get (R, 100).P50 = 100, "scrolling keeps what stays");
      Check (Columns.Get (R, 101).Origin = Columns.Empty and then Columns.Get (R, 103).Origin = Columns.Empty,
             "columns scrolling in start empty");
      Columns.Move_To (R, 99);
      Check (Columns.Get (R, 96).P50 = 96 and then Columns.Get (R, 99).P50 = 99 and then
             Columns.Get (R, 92).Origin = Columns.Empty and then Columns.Get (R, 95).Origin = Columns.Empty,
             "scrolling back clears the columns entering on the left");
      Columns.Move_To (R, 1_000);
      Check ((for all C in Unsigned_64'(993) .. 1_000 => Columns.Get (R, C).Origin = Columns.Empty),
             "a jump past the ring clears it");
      --  Zoom out by two: columns of 20 us from columns of 10 us.
      Columns.Reset (R, 107);
      for C in Unsigned_64'(100) .. 107 loop
         Columns.Put (R, C, One (C));
      end loop;
      Wide.Last := 53;
      Columns.Resample (R, 10, Wide, 20);
      Check (Columns.Get (Wide, 50).Count = 2 and then Columns.Get (Wide, 50).Maximum = 101 and then
             Columns.Get (Wide, 53).Minimum = 106, "resample merges pairs");
      Check (Columns.Get (Wide, 50).Origin = Columns.Preview, "resampled columns are a preview");
   end Ring;

   procedure Buckets is
      package B renames Charts_Buckets;
      L : constant B.Layout := (First => 0, Width => 1, Count => 4);
      R : B.Ring (Capacity => 4, Buckets => 4);
      T : B.Totals;
   begin
      Check (B.Bucket_Of (L, 0) = 0 and then B.Bucket_Of (L, 2) = 2 and then B.Bucket_Of (L, 3) = 3
             and then B.Bucket_Of (L, 99) = 3, "linear buckets, the last open");
      B.Reset (R, 10);
      B.Add (R, L, 10, 0);
      B.Add (R, L, 10, 0);
      B.Add (R, L, 9, 1);
      B.Add (R, L, 8, 7);
      B.Sum (R, 0, 10, T);
      Check (T (0) = 2 and then T (1) = 1 and then T (3) = 1, "sum over the visible columns");
      B.Move_To (R, 12);
      B.Sum (R, 0, 12, T);
      Check (T (0) = 2 and then T (1) = 1 and then T (3) = 0, "a column scrolled out leaves the histogram");
      B.Clear (R, 10);
      B.Sum (R, 0, 12, T);
      Check (T (0) = 0, "clear");
      B.Add_Window (R, L, 12, (Start_Us => 0, Count => 102, Sum => 0, Minimum => 0, Maximum => 3, Last => 0,
                               P50 => 0, P99 => 2));
      B.Sum (R, 12, 12, T);
      Check (T (0) + T (1) + T (2) + T (3) = 102, "a window's count is spread, all of it");
   end Buckets;

   procedure Labels is
      package F renames Charts_Format;
   begin
      Check (F.Value_Text (250, F.Microseconds) = "250 us", "us");
      Check (F.Value_Text (1_250, F.Microseconds) = "1.25 ms", "ms two decimals");
      Check (F.Value_Text (12_500, F.Microseconds) = "12.5 ms", "ms one decimal");
      Check (F.Value_Text (2_000_000, F.Microseconds) = "2 s", "s");
      Check (F.Value_Text (1_536, F.Bytes) = "1.5 KiB", "KiB");
      Check (F.Value_Text (12_345, F.Count) = "12.3k", "k");
      Check (F.Value_Text (7, F.Count) = "7", "count");
      Check (F.Clock_Text (3_723_000_000, 1_000_000) = "1:02:03", "h:mm:ss");
      Check (F.Clock_Text (83_000_000, 5_000_000) = "1:23", "m:ss");
      Check (F.Clock_Text (83_250_000, 500_000) = "1:23.250", "with ms");
      Check (F.Span_Text (300_000_000) = "5 min" and then F.Span_Text (3_600_000_000) = "1 h", "spans");
      Check (F.Span_Text (86_400_000_000) = "1 day" and then F.Span_Text (30_000_000) = "30 s", "spans 2");
   end Labels;

   procedure Palette is
      package P renames Charts_Palette;
   begin
      Check (P.Mix (16#FFFFFF#, 0, 100) = 16#FFFFFF# and then P.Mix (16#FFFFFF#, 0, 0) = 0, "mix ends");
      Check (P.Mix (16#FF0000#, 16#0000FF#, 50) = 16#80_0080#, "mix half");
      Check (P.Step_For (0, 100) = 1 and then P.Step_For (100, 100) = P.Ramp_Steps and then
             P.Step_For (50, 100) = 5, "ramp steps");
   end Palette;
begin
   Time_Axis;
   Value_Axis;
   Quantiles;
   Windows;
   Ring;
   Buckets;
   Labels;
   Palette;
   Put_Line ("charts core:" & Natural'Image (Passed) & " passed," & Natural'Image (Failed) & " failed");
   if Failed > 0 then
      raise Program_Error with "charts core tests failed";
   end if;
end Charts_Core_Tests;
