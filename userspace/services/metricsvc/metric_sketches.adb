pragma Ada_2022;
package body Metric_Sketches with SPARK_Mode is
   Permille_Whole : constant := 1000;

   function High_Bit (Value : Unsigned_64) return Bit_Index is
   begin
      for Bit in reverse Bit_Index loop
         pragma Loop_Invariant (Bit = 63 or else Value < 2 ** (Bit + 1));
         if Value >= 2 ** Bit then
            return Bit;
         end if;
      end loop;
      --  Unreachable: Value >= 2 ** Sub_Bucket_Bits.
      return Sub_Bucket_Bits;
   end High_Bit;

   function Value_Mantissa (Value : Unsigned_64) return Unsigned_64 is
      Shift : constant Natural := High_Bit (Value) - Sub_Bucket_Bits;
      Mantissa : constant Unsigned_64 := Value / 2 ** Shift;
   begin
      --  Always within: High_Bit's bit leads. The clamp keeps the range
      --  proved; the enclosure test shows it never acts.
      return Unsigned_64'Max (Sub_Buckets, Unsigned_64'Min (Mantissa, 2 * Sub_Buckets - 1));
   end Value_Mantissa;

   function Bucket_Of (Value : Unsigned_64) return Bucket is
   begin
      if Value < Sub_Buckets then
         return Natural (Value);
      end if;
      return Sub_Buckets + (High_Bit (Value) - Sub_Bucket_Bits) * Sub_Buckets
        + Natural (Value_Mantissa (Value) - Sub_Buckets);
   end Bucket_Of;

   procedure Add (S : in out Dense; Value : Unsigned_64; Times : Unsigned_64 := 1) is
      B : constant Bucket := Bucket_Of (Value);
   begin
      if Times = 0 then
         return;
      end if;
      S.Buckets (B) := (if S.Buckets (B) > Unsigned_64'Last - Times then Unsigned_64'Last
                        else S.Buckets (B) + Times);
      S.Count := (if S.Count > Unsigned_64'Last - Times then Unsigned_64'Last else S.Count + Times);
      if Times > 1 then
         --  A merged bucket's values are not known exactly; its bound is.
         S.Sum_Saturated := True;
      elsif Value > Unsigned_64'Last - S.Sum then
         S.Sum := Unsigned_64'Last;
         S.Sum_Saturated := True;
      else
         S.Sum := S.Sum + Value;
      end if;
      S.Minimum := Unsigned_64'Min (S.Minimum, Value);
      S.Maximum := Unsigned_64'Max (S.Maximum, Value);
   end Add;

   function Quantile (S : Dense; At_Permille : Permille) return Unsigned_64 is
      --  The rank wanted, 1 .. Count: the first sample at or past it.
      Wanted : Unsigned_64;
      Seen : Unsigned_64 := 0;
   begin
      if S.Count = 0 then
         return 0;
      end if;
      Wanted := (if S.Count > Unsigned_64'Last / Permille_Whole
                 then S.Count / Permille_Whole * Unsigned_64 (At_Permille)
                 else (S.Count * Unsigned_64 (At_Permille) + Permille_Whole - 1) / Permille_Whole);
      Wanted := Unsigned_64'Max (Wanted, 1);
      for B in Bucket loop
         Seen := (if S.Buckets (B) > Unsigned_64'Last - Seen then Unsigned_64'Last else Seen + S.Buckets (B));
         if Seen >= Wanted then
            return Unsigned_64'Min (Upper (B), S.Maximum);
         end if;
      end loop;
      return S.Maximum;
   end Quantile;
end Metric_Sketches;
