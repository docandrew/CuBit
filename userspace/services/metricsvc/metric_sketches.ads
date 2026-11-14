pragma Ada_2022;
with Interfaces; use Interfaces;

--  A mergeable distribution sketch (MET-002, DESIGN-metrics-store.md):
--  log-linear buckets, HDR and native-histogram style. Values below
--  Sub_Buckets have a bucket each; above, every octave [2^E, 2^(E+1)) is cut
--  into Sub_Buckets equal buckets. A value's bucket bounds it within
--  1/Sub_Buckets of itself (Relative_Error_Permille), whatever its size, so
--  a percentile read from a sketch is that close to the true order
--  statistic. Sketches add bucket by bucket: a window's sketch is the sum of
--  its finer windows', an instance's p99 across processes or nodes comes
--  from summed sketches, never from averaged percentiles.
--
--  Proved (tests/metric-store/run.sh --prove, level 2): freedom from
--  run-time errors, every value has a bucket. That a value's bucket's Lower
--  and Upper enclose it, within the relative error, is tested exhaustively
--  below 2**22 and at every bucket edge (tests/metric-store), not proved:
--  the provers did not join the shift and the division forms.
package Metric_Sketches with Pure, SPARK_Mode is

   Sub_Bucket_Bits : constant := 5;
   Sub_Buckets : constant := 2 ** Sub_Bucket_Bits;
   --  Octaves above the exact buckets: 2^5 .. 2^64.
   Octaves : constant := 64 - Sub_Bucket_Bits;
   Bucket_Count : constant := Sub_Buckets + Octaves * Sub_Buckets;
   subtype Bucket is Natural range 0 .. Bucket_Count - 1;
   --  Upper - Lower is under Lower / Sub_Buckets: 31.25 per mille, rounded up.
   Relative_Error_Permille : constant := 32;

   --  The highest set bit of a value at least Sub_Buckets.
   subtype Bit_Index is Natural range Sub_Bucket_Bits .. 63;
   function High_Bit (Value : Unsigned_64) return Bit_Index
   with Pre => Value >= Sub_Buckets,
        Post => Value >= 2 ** High_Bit'Result
                and then (High_Bit'Result = 63 or else Value < 2 ** (High_Bit'Result + 1));

   --  The top Sub_Bucket_Bits + 1 bits of a value at least Sub_Buckets.
   function Value_Mantissa (Value : Unsigned_64) return Unsigned_64
   with Pre => Value >= Sub_Buckets,
        Post => Value_Mantissa'Result in Sub_Buckets .. 2 * Sub_Buckets - 1;

   function Bucket_Of (Value : Unsigned_64) return Bucket
   with Post => (if Value < Sub_Buckets then Bucket_Of'Result = Natural (Value)
                 else Bucket_Of'Result =
                        Sub_Buckets + (High_Bit (Value) - Sub_Bucket_Bits) * Sub_Buckets
                        + Natural (Value_Mantissa (Value) - Sub_Buckets));

   function Octave_Of (Item : Bucket) return Natural is ((Item - Sub_Buckets) / Sub_Buckets)
   with Pre => Item >= Sub_Buckets, Post => Octave_Of'Result < Octaves;
   function Mantissa_Of (Item : Bucket) return Natural is ((Item - Sub_Buckets) mod Sub_Buckets)
   with Pre => Item >= Sub_Buckets;
   function Lower (Item : Bucket) return Unsigned_64 is
     (if Item < Sub_Buckets then Unsigned_64 (Item)
      else Unsigned_64 (Sub_Buckets + Mantissa_Of (Item)) * 2 ** Octave_Of (Item));
   function Upper (Item : Bucket) return Unsigned_64 is
     (if Item < Sub_Buckets then Unsigned_64 (Item)
      else Lower (Item) + (2 ** Octave_Of (Item) - 1));

   --  A dense sketch: counts per bucket and the exact count, sum, extremes.
   --  For computing (a query's merge), not for keeping: stored sketches are
   --  sparse (Sparse_Entry).
   type Counts is array (Bucket) of Unsigned_64;
   type Dense is record
      Buckets : Counts := [others => 0];
      Count : Unsigned_64 := 0;
      Sum : Unsigned_64 := 0;
      Sum_Saturated : Boolean := False;
      Minimum : Unsigned_64 := Unsigned_64'Last;
      Maximum : Unsigned_64 := 0;
   end record;

   procedure Add (S : in out Dense; Value : Unsigned_64; Times : Unsigned_64 := 1);

   --  A stored sketch entry: one non-empty bucket and its count, in one
   --  word (the bucket in the top Bucket_Bits).
   Bucket_Bits : constant := 11;
   pragma Compile_Time_Error (2 ** Bucket_Bits < Bucket_Count, "a bucket fits Bucket_Bits");
   Count_Bits : constant := 64 - Bucket_Bits;
   Maximum_Entry_Count : constant := 2 ** Count_Bits - 1;
   subtype Sparse_Entry is Unsigned_64;
   function Entry_Of (Item : Bucket; Count : Unsigned_64) return Sparse_Entry is
     (Shift_Left (Unsigned_64 (Item), Count_Bits) or Unsigned_64'Min (Count, Maximum_Entry_Count));
   function Bucket_Part (Item : Sparse_Entry) return Bucket is
     (Natural'Min (Natural (Shift_Right (Item, Count_Bits)), Bucket'Last));
   function Count_Part (Item : Sparse_Entry) return Unsigned_64 is (Item and Maximum_Entry_Count);

   --  The value at Permille of the way through S's samples, as its bucket's
   --  upper bound clamped to S's maximum (0 for an empty sketch).
   subtype Permille is Natural range 0 .. 1000;
   function Quantile (S : Dense; At_Permille : Permille) return Unsigned_64;
end Metric_Sketches;
