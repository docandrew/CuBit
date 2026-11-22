pragma Ada_2022;
package body Metric_Histograms with SPARK_Mode is
   Permille_Scale : constant := 1000;
   Last_Octave_Bucket : constant := Last_Bucket - 1;

   function Upper_Bound (Index : Bucket_Index) return Unsigned_64 is
      Base : Unsigned_64;
   begin
      if Index = Bucket_Index'First then
         return 0;
      elsif Index = Last_Bucket then
         return Unsigned_64'Last;
      end if;
      pragma Assert (Index <= Last_Octave_Bucket);
      Base := Shift_Left (1, (Index - 1) / Buckets_Per_Octave);
      return Base + (Base / Buckets_Per_Octave) *
        Unsigned_64 ((Index - 1) mod Buckets_Per_Octave);
   end Upper_Bound;

   function Bucket_Of (Value : Unsigned_64) return Bucket_Index is
   begin
      for Index in Bucket_Index'First .. Last_Octave_Bucket loop
         if Value <= Upper_Bound (Index) then
            return Index;
         end if;
      end loop;
      return Last_Bucket;
   end Bucket_Of;

   procedure Add (H : in out Histogram; Value : Unsigned_64) is
      Index : Bucket_Index;
   begin
      if H.Samples = Maximum_Samples then
         return;
      end if;
      Index := Bucket_Of (Value);
      --  A bucket never holds more than all admitted samples.
      if H.Buckets (Index) < Maximum_Samples then
         H.Buckets (Index) := H.Buckets (Index) + 1;
      end if;
      if H.Samples = 0 then
         H.Low := Value;
         H.High := Value;
      else
         H.Low := Unsigned_64'Min (H.Low, Value);
         H.High := Unsigned_64'Max (H.High, Value);
      end if;
      H.Samples := H.Samples + 1;
   end Add;

   function Quantile_Upper
     (H : Histogram; Fraction : CuBit.Metric_Protocol.Permille)
      return Unsigned_64
   is
      --  Count <= 2**48 and Fraction <= 1000: the product cannot wrap.
      Rank : constant Unsigned_64 :=
        (H.Samples * Unsigned_64 (Fraction) + (Permille_Scale - 1)) /
          Permille_Scale;
      Seen : Unsigned_64 := 0;
   begin
      if H.Samples = 0 then
         return 0;
      end if;
      for Index in Bucket_Index loop
         pragma Loop_Invariant (Seen <= Unsigned_64 (Index) * Maximum_Samples);
         Seen := Seen + H.Buckets (Index);
         if Seen >= Rank then
            return Upper_Bound (Index);
         end if;
      end loop;
      return Unsigned_64'Last;
   end Quantile_Upper;
end Metric_Histograms;
