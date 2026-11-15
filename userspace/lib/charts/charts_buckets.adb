pragma Ada_2022;

package body Charts_Buckets with SPARK_Mode is

   function Slot (R : Ring; C : Column_Number) return Positive
     with Post => Slot'Result <= R.Capacity;
   function Slot (R : Ring; C : Column_Number) return Positive is
      --  Column numbers stay below 2^62 (Charts_Time_Axis): the remainder
      --  is taken in integer arithmetic, where it is plainly below Capacity.
      Whole : constant Long_Long_Integer := Long_Long_Integer (Unsigned_64'Min (C, Axis.Maximum_Time_Us));
      Remainder : constant Long_Long_Integer := Whole mod Long_Long_Integer (R.Capacity);
   begin
      return Natural (Remainder) + 1;
   end Slot;

   procedure Clear_Slot (R : in out Ring; C : Column_Number)
     with Post => R.Last = R.Last'Old;
   procedure Clear_Slot (R : in out Ring; C : Column_Number) is
      S : constant Positive := Slot (R, C);
   begin
      for I in 1 .. R.Buckets loop
         R.Counts (S, I) := 0;
      end loop;
   end Clear_Slot;

   procedure Reset (R : in out Ring; Last : Column_Number) is
   begin
      R.Counts := [others => [others => 0]];
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
      --  As Charts_Columns.Move_To: the slots of the columns that scroll in.
      if Last > R.Last then
         for C in R.Last + 1 .. Last loop
            Clear_Slot (R, C);
         end loop;
      else
         for C in Last + 1 .. R.Last loop
            Clear_Slot (R, C);
         end loop;
      end if;
      R.Last := Last;
   end Move_To;

   procedure Clear (R : in out Ring; C : Column_Number) is
   begin
      Clear_Slot (R, C);
   end Clear;

   procedure Add_Many (R : in out Ring; L : Layout; C : Column_Number; V, Many : Unsigned_64)
     with Pre => L.Count = R.Buckets;
   procedure Add_Many (R : in out Ring; L : Layout; C : Column_Number; V, Many : Unsigned_64) is
      S : constant Positive := Slot (R, C);
      I : constant Positive := Bucket_Of (L, V) + 1;
   begin
      R.Counts (S, I) := Saturating_Add (R.Counts (S, I), Many);
   end Add_Many;

   procedure Add (R : in out Ring; L : Layout; C : Column_Number; V : Unsigned_64) is
   begin
      Add_Many (R, L, C, V, 1);
   end Add;

   procedure Add_Window (R : in out Ring; L : Layout; C : Column_Number; W : Charts_Columns.Window_Row) is
      --  Of the samples between the extremes, the share above the p99.
      Percent : constant := 100;
      Top_Percent : constant := 1;
      Inner : constant Unsigned_64 := (if W.Count >= 2 then W.Count - 2 else 0);
      Top : constant Unsigned_64 := Inner * Top_Percent / Percent;
      Below : constant Unsigned_64 := (Inner - Top) / 2;
      Above : constant Unsigned_64 := Inner - Top - Below;
   begin
      if W.Count = 0 then
         return;
      end if;
      Add_Many (R, L, C, W.Minimum, 1);
      if W.Count >= 2 then
         Add_Many (R, L, C, W.Maximum, 1);
      end if;
      Add_Many (R, L, C, W.P50, Below);
      Add_Many (R, L, C, W.P50, Above);
      Add_Many (R, L, C, W.P99, Top);
   end Add_Window;

   function Count_At (R : Ring; C : Column_Number; I : Bucket_Index) return Tally is
     (if Holds (R, C) then R.Counts (Slot (R, C), I + 1) else 0);

   procedure Sum (R : Ring; First, Last : Column_Number; Into : out Totals) is
      Oldest : constant Column_Number :=
        (if R.Last >= Unsigned_64 (R.Capacity - 1) then R.Last - Unsigned_64 (R.Capacity - 1) else 0);
      Low : constant Column_Number := Unsigned_64'Max (First, Oldest);
      High : constant Column_Number := Unsigned_64'Min (Last, R.Last);
   begin
      Into := [others => 0];
      if Low > High then
         return;
      end if;
      for C in Low .. High loop
         if Holds (R, C) then
            for I in 0 .. R.Buckets - 1 loop
               Into (I) := Charts_Columns.Saturating_Add (Into (I), Unsigned_64 (R.Counts (Slot (R, C), I + 1)));
            end loop;
         end if;
      end loop;
   end Sum;
end Charts_Buckets;
