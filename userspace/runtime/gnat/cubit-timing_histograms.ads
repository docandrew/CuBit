pragma Ada_2022;
with Interfaces; use Interfaces;

--  Bounded logarithmic histogram: four buckets per octave. Quantiles are
--  upper bounds, not exact order statistics. No allocation or logging.
package CuBit.Timing_Histograms with Pure, SPARK_Mode is
   Maximum_Samples : constant := 1_000_000;
   subtype Sample_Count is Natural range 0 .. Maximum_Samples;
   subtype Bucket_Index is Natural range 0 .. 257;
   type Histogram is private;
   Empty : constant Histogram;
   function Count (H : Histogram) return Sample_Count;
   function Minimum (H : Histogram) return Unsigned_64;
   function Maximum (H : Histogram) return Unsigned_64;
   function Upper_Bound (Index : Bucket_Index) return Unsigned_64;
   procedure Add (H : in out Histogram; Value : Unsigned_64);
   function Quantile_Upper (H : Histogram; Percent : Positive)
      return Unsigned_64 with Pre => Percent <= 100;
private
   --  Only Add mutates counters; total admission stops at one million.
   type Bucket_Counts is array (Bucket_Index) of Unsigned_64;
   type Histogram is record
      Count : Sample_Count := 0;
      Minimum : Unsigned_64 := Unsigned_64'Last;
      Maximum : Unsigned_64 := 0;
      Buckets : Bucket_Counts := [others => 0];
   end record;
   Empty : constant Histogram := (others => <>);
   function Count (H : Histogram) return Sample_Count is (H.Count);
   function Minimum (H : Histogram) return Unsigned_64 is
     (if H.Count = 0 then 0 else H.Minimum);
   function Maximum (H : Histogram) return Unsigned_64 is (H.Maximum);
end CuBit.Timing_Histograms;
