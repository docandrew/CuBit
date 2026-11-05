package body CuBit.Timing_Histograms with SPARK_Mode is
   function Upper_Bound (Index : Bucket_Index) return Unsigned_64 is
      Base : Unsigned_64;
   begin
      if Index = 0 then
         return 0;
      elsif Index = 257 then
         return Unsigned_64'Last;
      end if;
      Base := Shift_Left (Unsigned_64'(1), (Index - 1) / 4);
      return Base + (Base / 4) * Unsigned_64 ((Index - 1) mod 4);
   end Upper_Bound;

   procedure Add (H : in out Histogram; Value : Unsigned_64) is
   begin
      if H.Count = Maximum_Samples then
         return;
      end if;
      for Index in Bucket_Index loop
         if Value <= Upper_Bound (Index) then
            H.Buckets (Index) := H.Buckets (Index) + 1;
            H.Count := H.Count + 1;
            H.Minimum := Unsigned_64'Min (H.Minimum, Value);
            H.Maximum := Unsigned_64'Max (H.Maximum, Value);
            return;
         end if;
      end loop;
   end Add;

   function Quantile_Upper (H : Histogram; Percent : Positive)
      return Unsigned_64
   is
      Rank : constant Natural := (H.Count * Percent + 99) / 100;
      Seen : Unsigned_64 := 0;
   begin
      if H.Count = 0 then
         return 0;
      end if;
      for Index in Bucket_Index loop
         Seen := Seen + H.Buckets (Index);
         if Seen >= Unsigned_64 (Rank) then
            return Upper_Bound (Index);
         end if;
      end loop;
      return Unsigned_64'Last;
   end Quantile_Upper;
end CuBit.Timing_Histograms;
