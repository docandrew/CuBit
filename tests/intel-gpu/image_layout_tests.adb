with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Image_Layout; use Intel_GPU_Image_Layout;
procedure Image_Layout_Tests is
   Image : Descriptor := (BGRA8_UNorm, Linear, 800, 600, 3200, 0);
begin
   pragma Assert (Valid (Image, 1_920_000) and Span (Image, 1_920_000) = 1_920_000);
   pragma Assert (not Valid (Image, 1_919_999));
   pragma Assert (not Valid ((Image with delta Format => Unsupported_Format), Unsigned_64'Last));
   pragma Assert (not Valid ((Image with delta Layout => Unsupported_Layout), Unsigned_64'Last));
   pragma Assert (not Valid ((Image with delta Width => 0), Unsigned_64'Last));
   pragma Assert (not Valid ((Image with delta Height => 0), Unsigned_64'Last));
   pragma Assert (not Valid ((Image with delta Pitch => 3196), Unsigned_64'Last));
   pragma Assert (not Valid ((Image with delta Pitch => 3201), Unsigned_64'Last));
   pragma Assert (not Valid ((Image with delta Offset => 1), Unsigned_64'Last));
   Image := (BGRA8_UNorm, Linear, 1, 2, 64, 8);
   pragma Assert (Span (Image, 76) = 68 and not Valid (Image, 75));
   Image := (BGRA8_UNorm, Linear, 1, 2, Unsigned_64'Last - 3, 0);
   pragma Assert (not Valid (Image, Unsigned_64'Last));
   Image := (BGRA8_UNorm, Linear, 1, 1, 4, Unsigned_64'Last - 3);
   pragma Assert (not Valid (Image, Unsigned_64'Last));
   Image := (BGRA8_UNorm, Linear, 1, 1, 4, 0);
   pragma Assert (Span (Image, 4) = 4);
   -- Independent small-domain arithmetic oracle, including padding/offsets.
   for W in 0 .. 7 loop
      for H in 0 .. 7 loop
         for P in 0 .. 35 loop
            for O in 0 .. 7 loop
               Image := (BGRA8_UNorm, Linear, Unsigned_32 (W), Unsigned_32 (H),
                         Unsigned_64 (P), Unsigned_64 (O));
               for B in 0 .. 80 loop
                  declare
                     Expected : constant Boolean := W > 0 and H > 0 and
                       P >= W * 4 and P mod 4 = 0 and O mod 4 = 0 and
                       O + (H - 1) * P + W * 4 <= B;
                  begin
                     pragma Assert (Valid (Image, Unsigned_64 (B)) = Expected);
                     pragma Assert ((Span (Image, Unsigned_64 (B)) /= 0) = Expected);
                  end;
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Image layout PASS: exact bounds, padding, unsupported layouts, overflow and small-domain oracle");
end Image_Layout_Tests;
