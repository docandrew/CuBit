pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Metric_Protocol;

--  Bounded logarithmic histogram with four buckets per octave (the bucket
--  bounds of CuBit.Timing_Histograms). Quantiles are bucket upper bounds, not
--  exact order statistics. Admission stops at Maximum_Samples; Saturated
--  then reports that later samples were not counted.
package Metric_Histograms with SPARK_Mode is
   Maximum_Samples : constant Unsigned_64 := 2 ** 48;
   subtype Sample_Count is Unsigned_64 range 0 .. Maximum_Samples;
   Last_Bucket : constant := 257;
   subtype Bucket_Index is Natural range 0 .. Last_Bucket;
   Buckets_Per_Octave : constant := 4;

   type Histogram is private;
   Empty : constant Histogram;
   function Count (H : Histogram) return Sample_Count;
   function Saturated (H : Histogram) return Boolean is
     (Count (H) = Maximum_Samples);
   function Minimum (H : Histogram) return Unsigned_64;
   function Maximum (H : Histogram) return Unsigned_64;
   function Upper_Bound (Index : Bucket_Index) return Unsigned_64;

   procedure Add (H : in out Histogram; Value : Unsigned_64)
     with Post =>
       (if Count (H'Old) < Maximum_Samples then
          Count (H) = Count (H'Old) + 1 and then
          Minimum (H) = (if Count (H'Old) = 0 then Value
                         else Unsigned_64'Min (Minimum (H'Old), Value))
          and then
          Maximum (H) = (if Count (H'Old) = 0 then Value
                         else Unsigned_64'Max (Maximum (H'Old), Value))
        else H = H'Old);

   --  Smallest bucket bound covering at least Rank samples, where Rank is
   --  the ceiling of Count * Fraction / 1000. Zero for an empty histogram.
   function Quantile_Upper
     (H : Histogram; Fraction : CuBit.Metric_Protocol.Permille)
      return Unsigned_64;
private
   type Bucket_Counts is array (Bucket_Index) of Sample_Count;
   type Histogram is record
      Samples : Sample_Count := 0;
      Low : Unsigned_64 := Unsigned_64'Last;
      High : Unsigned_64 := 0;
      Buckets : Bucket_Counts := [others => 0];
   end record;
   Empty : constant Histogram := (others => <>);
   function Count (H : Histogram) return Sample_Count is (H.Samples);
   function Minimum (H : Histogram) return Unsigned_64 is
     (if H.Samples = 0 then 0 else H.Low);
   function Maximum (H : Histogram) return Unsigned_64 is
     (if H.Samples = 0 then 0 else H.High);
end Metric_Histograms;
