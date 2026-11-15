------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  One series' level of detail (MET-002's app): a summary per pixel
--  column, kept in a ring keyed by absolute column number
--  (Charts_Time_Axis), so drawing costs the plot's width, never the
--  samples behind it.
--
--  @description
--  A column's summary is made from raw samples (exact: Summarize_Raw selects
--  the quantiles) or from the store's tier windows (Summarize_Windows: one
--  window gives its sketch's quantiles; several give p99 as the largest of
--  theirs, an upper bound that never hides a spike, and p50 as their
--  count-weighted median, an approximation; the Origin says which).
--
--  The ring holds exactly the columns of a viewport: Move_To clears the
--  columns that scroll in (O(shift)); Resample rebuilds it for another
--  column width from what it holds (the immediate preview a zoom shows
--  until the store's answer refines it).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Charts_Quantiles;
with Charts_Time_Axis;

package Charts_Columns with SPARK_Mode, Pure is
   package Axis renames Charts_Time_Axis;
   subtype Column_Number is Axis.Column_Number;

   --  Where a summary came from.
   type Origin_Kind is
     (Empty,          --  no samples in the column
      Raw,            --  the column's raw samples: exact
      Window,         --  one tier window: the store's sketch (1/32 relative error)
      Merged,         --  several windows or columns: p99 an upper bound, p50 approximate
      Preview);       --  resampled from another width: shown until refetched

   type Summary is record
      Origin : Origin_Kind := Empty;
      Count : Unsigned_64 := 0;
      --  Saturates at Unsigned_64'Last.
      Sum : Unsigned_64 := 0;
      Minimum : Unsigned_64 := Unsigned_64'Last;
      Maximum : Unsigned_64 := 0;
      Last : Unsigned_64 := 0;
      P50 : Unsigned_64 := 0;
      P99 : Unsigned_64 := 0;
   end record;
   Nothing : constant Summary := (others => <>);

   function Saturating_Add (A, B : Unsigned_64) return Unsigned_64 is
     (if A > Unsigned_64'Last - B then Unsigned_64'Last else A + B);

   --  The mean, rounded down (0 when empty).
   function Mean (S : Summary) return Unsigned_64 is (if S.Count = 0 then 0 else S.Sum / S.Count);

   --  Raw samples, in any order; Values is reordered. Last is the value of
   --  the latest sample (Last_Value, given by the caller, who saw times).
   Maximum_Column_Samples : constant := Natural'Last / Charts_Quantiles.Permille_Scale;
   procedure Summarize_Raw
     (Values : in out Charts_Quantiles.Sample_Array; Last_Value : Unsigned_64; Result : out Summary)
     with Pre => Values'Length in 1 .. Maximum_Column_Samples and then Values'Last < Natural'Last;

   --  A tier window, as the store answers Read_Range (CuBit.Metric_Query_Pages).
   type Window_Row is record
      Start_Us, Count, Sum, Minimum, Maximum, Last, P50, P99 : Unsigned_64 := 0;
   end record;
   --  The windows one column can merge exactly enough to weigh medians: the
   --  store's tiers are at most ten times apart (1 s, 10 s, 1 min, 10 min),
   --  so a column holds at most ten whole windows and two partial ones.
   Maximum_Column_Windows : constant := 16;
   subtype Window_Count is Natural range 0 .. Maximum_Column_Windows;
   type Window_Rows is array (Positive range <>) of Window_Row;

   function Summarize_Windows (Rows : Window_Rows) return Summary
     with Pre => Rows'Length in 1 .. Maximum_Column_Windows and then Rows'First = 1;

   --  A and B together (columns merged for a preview or a stat): counts and
   --  sums add, extremes combine, p99 is the larger, p50 the one with more
   --  samples behind it.
   function Merge (A, B : Summary) return Summary;

   ---------------------------------------------------------------------------
   --  The ring
   ---------------------------------------------------------------------------
   subtype Ring_Capacity is Axis.Column_Count;
   type Summary_Array is array (Positive range <>) of Summary;
   type Ring (Capacity : Ring_Capacity) is record
      --  The newest column held; the ring holds Last - Capacity + 1 .. Last.
      Last : Column_Number := 0;
      Items : Summary_Array (1 .. Capacity) := [others => Nothing];
   end record;

   function Holds (R : Ring; C : Column_Number) return Boolean is
     (C <= R.Last and then R.Last - C < Unsigned_64 (R.Capacity));

   function Slot (R : Ring; C : Column_Number) return Positive is (Natural (C mod Unsigned_64 (R.Capacity)) + 1)
     with Post => Slot'Result <= R.Capacity;

   function Get (R : Ring; C : Column_Number) return Summary is
     (if Holds (R, C) then R.Items (Slot (R, C)) else Nothing);

   --  Columns From, From + 1, ... into Into (Nothing where not held): one
   --  division for the whole run, not one per column.
   procedure Copy_Run (R : Ring; From : Column_Number; Into : out Summary_Array)
     with Pre => From <= Axis.Maximum_Time_Us - Unsigned_64 (Axis.Maximum_Columns);

   procedure Put (R : in out Ring; C : Column_Number; S : Summary)
     with Pre => Holds (R, C), Post => R.Last = R.Last'Old;

   --  Every column empty, the newest being Last.
   procedure Reset (R : in out Ring; Last : Column_Number)
     with Post => R.Last = Last;

   --  Hold the columns ending at Last: those still in view keep their
   --  summaries, the others are cleared.
   procedure Move_To (R : in out Ring; Last : Column_Number)
     with Post => R.Last = Last;

   --  Into's columns (width To_Width) from From's (width From_Width): each
   --  merges the held columns it overlaps; the result's origin is Preview
   --  wherever anything was held.
   procedure Resample
     (From : Ring; From_Width : Axis.Column_Width; Into : in out Ring; To_Width : Axis.Column_Width)
     with Pre => From.Last < Axis.Maximum_Time_Us / From_Width and then
                 Into.Last < Axis.Maximum_Time_Us / To_Width;
end Charts_Columns;
