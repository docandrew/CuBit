with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Timing_Histograms; use CuBit.Timing_Histograms;
procedure Histogram_Test is
   H : Histogram;
   Previous : Unsigned_64 := 0;
begin
   pragma Assert (Count (H) = 0 and Minimum (H) = 0);
   pragma Assert (Quantile_Upper (H, 99) = 0);
   for Index in Bucket_Index loop
      pragma Assert (Upper_Bound (Index) >= Previous);
      Previous := Upper_Bound (Index);
      H := Empty;
      Add (H, Previous);
      pragma Assert (Count (H) = 1);
      pragma Assert (Quantile_Upper (H, 100) = Previous);
      pragma Assert (Minimum (H) = Previous and Maximum (H) = Previous);
      if Previous < Unsigned_64'Last then
         H := Empty;
         Add (H, Previous + 1);
         pragma Assert (Quantile_Upper (H, 100) >= Previous + 1);
      end if;
   end loop;
   H := Empty;
   for Value in 1 .. 100 loop
      Add (H, Unsigned_64 (Value));
   end loop;
   pragma Assert (Count (H) = 100);
   pragma Assert (Minimum (H) = 1 and Maximum (H) = 100);
   pragma Assert (Quantile_Upper (H, 50) = 56);
   pragma Assert (Quantile_Upper (H, 95) = 96);
   pragma Assert (Quantile_Upper (H, 99) = 112);
   H := Empty;
   for I in 1 .. Maximum_Samples loop
      Add (H, 0);
   end loop;
   Add (H, Unsigned_64'Last);
   pragma Assert (Count (H) = Maximum_Samples);
   pragma Assert (Maximum (H) = 0);
   Ada.Text_IO.Put_Line
     ("histogram: PASS boundaries, ranks, extrema, saturation");
end Histogram_Test;
