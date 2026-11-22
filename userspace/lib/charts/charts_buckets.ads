------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Histograms over time (MET-002's app): per pixel column, how many samples
--  fell in each value bucket, in a ring parallel to Charts_Columns'. A
--  histogram of the visible range sums the visible columns (cost: columns
--  times buckets, never samples); the same counts are a distribution
--  heatmap's cells.
--
--  @description
--  Linear buckets: bucket I (from 0) holds First + I * Width up to the next
--  edge; the last bucket also holds everything above (an open "or more"
--  bucket), the first everything below. From raw samples the counts are
--  exact. A tier window carries no buckets, only count, extremes and two
--  quantiles, so Add_Window spreads its count over them (approximate; the
--  caller marks the panel so).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Charts_Columns;
with Charts_Time_Axis;

package Charts_Buckets with SPARK_Mode, Pure is
   package Axis renames Charts_Time_Axis;
   subtype Column_Number is Axis.Column_Number;

   Maximum_Buckets : constant := 64;
   subtype Bucket_Count is Positive range 1 .. Maximum_Buckets;
   subtype Bucket_Index is Natural range 0 .. Maximum_Buckets - 1;
   Maximum_Bucket_Value : constant := 2 ** 40;
   subtype Bucket_Value is Unsigned_64 range 0 .. Maximum_Bucket_Value;
   subtype Bucket_Width is Unsigned_64 range 1 .. Maximum_Bucket_Value;

   type Layout is record
      First : Bucket_Value := 0;
      Width : Bucket_Width := 1;
      Count : Bucket_Count := 1;
   end record;

   function Bucket_Of (L : Layout; V : Unsigned_64) return Bucket_Index is
     (if V <= L.First then 0
      elsif (V - L.First) / L.Width >= Unsigned_64 (L.Count - 1) then L.Count - 1
      else Natural ((V - L.First) / L.Width))
     with Post => Bucket_Of'Result < L.Count;

   --  The lowest value bucket I holds (its label).
   function Lower_Edge (L : Layout; I : Bucket_Index) return Unsigned_64 is
     (L.First + Unsigned_64 (I) * L.Width);

   subtype Tally is Unsigned_32;
   function Saturating_Add (A : Tally; B : Unsigned_64) return Tally is
     (if B >= Unsigned_64 (Tally'Last - A) then Tally'Last else A + Tally (B));

   type Tally_Matrix is array (Positive range <>, Positive range <>) of Tally;
   type Totals is array (Bucket_Index) of Unsigned_64;

   --  Capacity columns of Buckets tallies each.
   type Ring (Capacity : Axis.Column_Count; Buckets : Bucket_Count) is record
      Last : Column_Number := 0;
      Counts : Tally_Matrix (1 .. Capacity, 1 .. Buckets) := [others => [others => 0]];
   end record;

   function Holds (R : Ring; C : Column_Number) return Boolean is
     (C <= R.Last and then R.Last - C < Unsigned_64 (R.Capacity));

   procedure Reset (R : in out Ring; Last : Column_Number) with Post => R.Last = Last;
   procedure Move_To (R : in out Ring; Last : Column_Number) with Post => R.Last = Last;
   --  Column C emptied (before its samples are folded again).
   procedure Clear (R : in out Ring; C : Column_Number) with Pre => Holds (R, C);

   procedure Add (R : in out Ring; L : Layout; C : Column_Number; V : Unsigned_64)
     with Pre => Holds (R, C) and then L.Count = R.Buckets;
   --  A tier window's count spread over its extremes and quantiles: one
   --  sample at its minimum and one at its maximum, the rest half below and
   --  half above its median (the part above its p99 at the p99).
   procedure Add_Window (R : in out Ring; L : Layout; C : Column_Number; W : Charts_Columns.Window_Row)
     with Pre => Holds (R, C) and then L.Count = R.Buckets;

   function Count_At (R : Ring; C : Column_Number; I : Bucket_Index) return Tally
     with Pre => I < R.Buckets;

   --  The tallies of columns First .. Last held, bucket by bucket.
   procedure Sum (R : Ring; First, Last : Column_Number; Into : out Totals);
end Charts_Buckets;
