pragma Ada_2022;

package body Charts_Columns with SPARK_Mode is

   function Slot (R : Ring; C : Column_Number) return Positive is
      --  Column numbers stay below 2^62 (Charts_Time_Axis): the remainder
      --  is taken in integer arithmetic, where it is plainly below Capacity.
      Whole : constant Long_Long_Integer := Long_Long_Integer (Unsigned_64'Min (C, Axis.Maximum_Time_Us));
      Remainder : constant Long_Long_Integer := Whole mod Long_Long_Integer (R.Capacity);
   begin
      return Natural (Remainder) + 1;
   end Slot;

   procedure Summarize_Raw
     (Values : in out Charts_Quantiles.Sample_Array; Last_Value : Unsigned_64; Result : out Summary)
   is
      Count : constant Positive := Values'Length;
      S : Summary := (Origin => Raw, Count => Unsigned_64 (Count), Last => Last_Value, others => <>);
   begin
      for V of Values loop
         S.Sum := Saturating_Add (S.Sum, V);
         S.Minimum := Unsigned_64'Min (S.Minimum, V);
         S.Maximum := Unsigned_64'Max (S.Maximum, V);
      end loop;
      Charts_Quantiles.Select_Kth (Values, Charts_Quantiles.Rank (Charts_Quantiles.P99, Count), S.P99);
      Charts_Quantiles.Select_Kth (Values, Charts_Quantiles.Rank (Charts_Quantiles.Median, Count), S.P50);
      Result := S;
   end Summarize_Raw;

   function Summarize_Windows (Rows : Window_Rows) return Summary is
      type Median_Entry is record
         P50, Count : Unsigned_64 := 0;
      end record;
      type Median_Entries is array (1 .. Maximum_Column_Windows) of Median_Entry;
      Order : Median_Entries := [others => <>];
      Used : Window_Count := 0;
      S : Summary := (Origin => (if Rows'Length = 1 then Window else Merged), others => <>);
      Latest : Unsigned_64 := 0;
      Half, Seen : Unsigned_64 := 0;
   begin
      for R of Rows loop
         S.Count := Saturating_Add (S.Count, R.Count);
         S.Sum := Saturating_Add (S.Sum, R.Sum);
         S.Minimum := Unsigned_64'Min (S.Minimum, R.Minimum);
         S.Maximum := Unsigned_64'Max (S.Maximum, R.Maximum);
         S.P99 := Unsigned_64'Max (S.P99, R.P99);
         if R.Start_Us >= Latest then
            Latest := R.Start_Us;
            S.Last := R.Last;
         end if;
         --  Insertion by p50: at most Maximum_Column_Windows entries.
         if Used < Maximum_Column_Windows then
            declare
               At_Position : Positive := Used + 1;
            begin
               while At_Position > 1 and then Order (At_Position - 1).P50 > R.P50 loop
                  pragma Loop_Invariant (At_Position in 2 .. Used + 1);
                  pragma Loop_Variant (Decreases => At_Position);
                  Order (At_Position) := Order (At_Position - 1);
                  At_Position := At_Position - 1;
               end loop;
               Order (At_Position) := (P50 => R.P50, Count => R.Count);
               Used := Used + 1;
            end;
         end if;
      end loop;
      --  The count-weighted median of the windows' medians.
      Half := S.Count / 2;
      for I in 1 .. Used loop
         S.P50 := Order (I).P50;
         Seen := Saturating_Add (Seen, Order (I).Count);
         exit when Seen > Half;
      end loop;
      return S;
   end Summarize_Windows;

   function Merge (A, B : Summary) return Summary is
   begin
      if A.Origin = Empty then
         return B;
      elsif B.Origin = Empty then
         return A;
      end if;
      return (Origin => Merged,
              Count => Saturating_Add (A.Count, B.Count),
              Sum => Saturating_Add (A.Sum, B.Sum),
              Minimum => Unsigned_64'Min (A.Minimum, B.Minimum),
              Maximum => Unsigned_64'Max (A.Maximum, B.Maximum),
              Last => B.Last,
              P50 => (if A.Count >= B.Count then A.P50 else B.P50),
              P99 => Unsigned_64'Max (A.P99, B.P99));
   end Merge;

   procedure Copy_Run (R : Ring; From : Column_Number; Into : out Summary_Array) is
      Oldest : constant Column_Number :=
        (if R.Last >= Unsigned_64 (R.Capacity - 1) then R.Last - Unsigned_64 (R.Capacity - 1) else 0);
      At_Slot : Positive := Slot (R, From);
      C : Column_Number := From;
   begin
      for I in Into'Range loop
         pragma Loop_Invariant (At_Slot in 1 .. R.Capacity);
         Into (I) := (if C >= Oldest and then C <= R.Last then R.Items (At_Slot) else Nothing);
         At_Slot := (if At_Slot = R.Capacity then 1 else At_Slot + 1);
         if C < Column_Number'Last then
            C := C + 1;
         end if;
      end loop;
   end Copy_Run;

   procedure Put (R : in out Ring; C : Column_Number; S : Summary) is
   begin
      R.Items (Slot (R, C)) := S;
   end Put;

   procedure Reset (R : in out Ring; Last : Column_Number) is
   begin
      R.Items := [others => Nothing];
      R.Last := Last;
   end Reset;

   procedure Move_To (R : in out Ring; Last : Column_Number) is
      Capacity : constant Unsigned_64 := Unsigned_64 (R.Capacity);
   begin
      if Last = R.Last then
         return;
      elsif (if Last > R.Last then Last - R.Last else R.Last - Last) >= Capacity then
         Reset (R, Last);
         return;
      end if;
      if Last > R.Last then
         --  Columns R.Last + 1 .. Last scroll in on the right; their slots
         --  held the oldest columns, which leave.
         for C in R.Last + 1 .. Last loop
            R.Items (Slot (R, C)) := Nothing;
         end loop;
      else
         --  Columns Last - Capacity + 1 .. R.Last - Capacity scroll in on
         --  the left; their slots held the newest, which leave.
         for C in Last + 1 .. R.Last loop
            R.Items (Slot (R, C)) := Nothing;
         end loop;
      end if;
      R.Last := Last;
   end Move_To;

   procedure Resample
     (From : Ring; From_Width : Axis.Column_Width; Into : in out Ring; To_Width : Axis.Column_Width)
   is
      Into_First : constant Column_Number :=
        (if Into.Last >= Unsigned_64 (Into.Capacity - 1) then Into.Last - Unsigned_64 (Into.Capacity - 1) else 0);
      From_First : constant Column_Number :=
        (if From.Last >= Unsigned_64 (From.Capacity - 1) then From.Last - Unsigned_64 (From.Capacity - 1) else 0);
      At_Slot : Positive := Slot (Into, Into_First);
   begin
      Into.Items := [others => Nothing];
      for C in Into_First .. Into.Last loop
         pragma Loop_Invariant (At_Slot in 1 .. Into.Capacity);
         declare
            Start : constant Axis.Time_Us := Axis.Start_Of (C, To_Width);
            Stop : constant Axis.Time_Us := Axis.End_Of (C, To_Width);
            Low : constant Column_Number := Unsigned_64'Max (Axis.Column_Of (Start, From_Width), From_First);
            High : constant Column_Number := Unsigned_64'Min (Axis.Column_Of (Stop, From_Width), From.Last);
            S : Summary := Nothing;
         begin
            if Low <= High then
               --  The old columns' slots follow one another: one division
               --  for the run, not one per column.
               declare
                  Old_Slot : Positive := Slot (From, Low);
               begin
                  for Old in Low .. High loop
                     pragma Loop_Invariant (Old_Slot in 1 .. From.Capacity);
                     if From.Items (Old_Slot).Origin /= Empty then
                        S := Merge (S, From.Items (Old_Slot));
                     end if;
                     Old_Slot := (if Old_Slot = From.Capacity then 1 else Old_Slot + 1);
                  end loop;
               end;
            end if;
            if S.Origin /= Empty then
               S.Origin := Preview;
            end if;
            Into.Items (At_Slot) := S;
            At_Slot := (if At_Slot = Into.Capacity then 1 else At_Slot + 1);
         end;
      end loop;
   end Resample;
end Charts_Columns;
