------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The time axis of a chart (MET-002's app): pixel columns as absolute
--  slices of the monotonic clock, and nice tick steps.
--
--  @description
--  A column is Width microseconds of time, and column C holds
--  [C * Width, (C + 1) * Width). Columns are numbered from the clock's zero,
--  never from the plot's left edge, so a column's contents do not change as
--  the plot scrolls: scrolling is a shift by whole columns, and a new sample
--  touches one column (a sliver). A Viewport is Columns columns ending at
--  Last; its left part may lie before the clock's zero (just after boot).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Charts_Time_Axis with SPARK_Mode, Pure is
   Microseconds_Per_Millisecond : constant := 1_000;
   Microseconds_Per_Second : constant := 1_000_000;
   Seconds_Per_Minute : constant := 60;
   Minutes_Per_Hour : constant := 60;
   Hours_Per_Day : constant := 24;
   Microseconds_Per_Minute : constant := Seconds_Per_Minute * Microseconds_Per_Second;
   Microseconds_Per_Hour : constant := Minutes_Per_Hour * Microseconds_Per_Minute;
   Microseconds_Per_Day : constant := Hours_Per_Day * Microseconds_Per_Hour;
   Days_Per_Week : constant := 7;

   --  The longest span a chart shows: a week, what the store's coarsest
   --  tier keeps (DESIGN-metrics-store.md, tiers); the shortest, 100 ms.
   Maximum_Span_Us : constant := Days_Per_Week * Microseconds_Per_Day;
   Minimum_Span_Us : constant := 100 * Microseconds_Per_Millisecond;
   subtype Span is Unsigned_64 range Minimum_Span_Us .. Maximum_Span_Us;

   --  The widest plot, in pixel columns (an 8K panel's width).
   Maximum_Columns : constant := 8_192;
   subtype Column_Count is Positive range 1 .. Maximum_Columns;

   subtype Column_Width is Unsigned_64 range 1 .. Maximum_Span_Us;

   --  Clock readings arithmetic is done on: far beyond any uptime (2^62 us
   --  is 146 000 years), and room to add a span without overflow.
   Maximum_Time_Us : constant := 2 ** 62;
   --  Times and column numbers are whole words; the preconditions keep
   --  them under Maximum_Time_Us where arithmetic needs the room.
   subtype Time_Us is Unsigned_64;
   subtype Column_Number is Unsigned_64;

   --  The column width that shows S in Columns columns (rounded up: the
   --  plot shows at least S).
   function Width_For (S : Span; Columns : Column_Count) return Column_Width is
     ((S + Unsigned_64 (Columns) - 1) / Unsigned_64 (Columns));

   function Column_Of (T : Time_Us; W : Column_Width) return Column_Number is (T / W);

   --  The first microsecond of column C.
   function Start_Of (C : Column_Number; W : Column_Width) return Time_Us is (C * W)
     with Pre => C <= Maximum_Time_Us / W;
   --  The last microsecond of column C.
   function End_Of (C : Column_Number; W : Column_Width) return Time_Us is (C * W + (W - 1))
     with Pre => C < Maximum_Time_Us / W;

   type Viewport is record
      Width : Column_Width := 1;
      Columns : Column_Count := 1;
      --  The rightmost column.
      Last : Column_Number := 0;
   end record;

   function Valid (V : Viewport) return Boolean is (V.Last < Maximum_Time_Us / V.Width);

   --  The viewport of Columns columns of width W whose rightmost column holds T.
   function Ending_At (T : Time_Us; W : Column_Width; Columns : Column_Count) return Viewport is
     ((Width => W, Columns => Columns, Last => Column_Of (T, W)));

   function Visible (V : Viewport; C : Column_Number) return Boolean is
     (C <= V.Last and then V.Last - C < Unsigned_64 (V.Columns));

   --  The leftmost column that exists (the clock starts at zero).
   function First (V : Viewport) return Column_Number is
     (if V.Last >= Unsigned_64 (V.Columns - 1) then V.Last - Unsigned_64 (V.Columns - 1) else 0);

   --  Column C's pixel column, 0 at the left.
   function X_Of (V : Viewport; C : Column_Number) return Natural
     with Pre => Visible (V, C), Post => X_Of'Result < V.Columns;

   --  Pixel column X shows a column (not before the clock's zero).
   function Has_Column_At (V : Viewport; X : Natural) return Boolean is
     (X < V.Columns and then V.Last >= Unsigned_64 (V.Columns - 1 - X));

   function Column_At (V : Viewport; X : Natural) return Column_Number is
     (V.Last - Unsigned_64 (V.Columns - 1 - X))
     with Pre => Has_Column_At (V, X), Post => Visible (V, Column_At'Result);

   --  The time at the plot's left and right edges.
   function Left_Time (V : Viewport) return Time_Us is (Start_Of (First (V), V.Width))
     with Pre => Valid (V);
   function Right_Time (V : Viewport) return Time_Us is (End_Of (V.Last, V.Width))
     with Pre => Valid (V);

   --  The viewport moved by Columns_Moved to the right (positive: later).
   function Shifted (V : Viewport; Columns_Moved : Integer) return Viewport
     with Pre => Valid (V), Post => Valid (Shifted'Result) and then Shifted'Result.Width = V.Width
       and then Shifted'Result.Columns = V.Columns;

   --  Ticks: the labelled times, every Tick_Step microseconds, at least
   --  Minimum_Pixels apart. The steps are clock-friendly (1, 2, 5 ms ...
   --  15 s, 30 s, 1 min ... 6 h, 12 h, 1 day).
   Tick_Steps : constant := 27;
   subtype Tick_Index is Positive range 1 .. Tick_Steps;
   type Tick_Table is array (Tick_Index) of Unsigned_64;
   Steps : constant Tick_Table :=
     [1 * Microseconds_Per_Millisecond, 2 * Microseconds_Per_Millisecond, 5 * Microseconds_Per_Millisecond,
      10 * Microseconds_Per_Millisecond, 20 * Microseconds_Per_Millisecond, 50 * Microseconds_Per_Millisecond,
      100 * Microseconds_Per_Millisecond, 200 * Microseconds_Per_Millisecond, 500 * Microseconds_Per_Millisecond,
      1 * Microseconds_Per_Second, 2 * Microseconds_Per_Second, 5 * Microseconds_Per_Second,
      10 * Microseconds_Per_Second, 15 * Microseconds_Per_Second, 30 * Microseconds_Per_Second,
      1 * Microseconds_Per_Minute, 2 * Microseconds_Per_Minute, 5 * Microseconds_Per_Minute,
      10 * Microseconds_Per_Minute, 15 * Microseconds_Per_Minute, 30 * Microseconds_Per_Minute,
      1 * Microseconds_Per_Hour, 2 * Microseconds_Per_Hour, 6 * Microseconds_Per_Hour,
      12 * Microseconds_Per_Hour, 1 * Microseconds_Per_Day, 2 * Microseconds_Per_Day];
   Maximum_Tick_Pixels : constant := 4_096;
   subtype Tick_Pixels is Positive range 1 .. Maximum_Tick_Pixels;
   function Tick_Step (W : Column_Width; Minimum_Pixels : Tick_Pixels) return Unsigned_64
     with Post => Tick_Step'Result in 1 .. Steps (Tick_Steps);

   --  The first tick at or after T.
   function First_Tick (T : Time_Us; Step : Unsigned_64) return Unsigned_64 is
     ((T + Step - 1) / Step * Step)
     with Pre => Step in 1 .. Steps (Tick_Steps);
end Charts_Time_Axis;
