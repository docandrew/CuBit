with Ada.Text_IO;
with Compositor_Source_Damage;
procedure Source_Damage_Tests is
   package P renames Compositor_Source_Damage;
   use P;
   Count, Pixel_Checks : Natural := 0;
   type Sizes is array (Positive range <>) of Extent;
   Samples : constant Sizes := (1, 2, 3, 7, 16, 127, 256, 4096, 65_535);
   R : Box;
   A : Interval;
   -- Exact cross-products are independent of the implementation's division.
   procedure Check_Axis (Low, High : Edge; Physical, Logical : Extent) is
   begin
      A := Axis (Low, High, Physical, Logical);
      pragma Assert (A.First < A.Last and A.Last <= Logical);
      pragma Assert (Wide (A.First) * Wide (Physical) <= Wide (Low) * Wide (Logical));
      pragma Assert (Wide (A.First + 1) * Wide (Physical) > Wide (Low) * Wide (Logical));
      pragma Assert (Wide (A.Last) * Wide (Physical) >= Wide (High) * Wide (Logical));
      pragma Assert (Wide (A.Last - 1) * Wide (Physical) < Wide (High) * Wide (Logical));
      Count := Count + 1;
   end Check_Axis;
begin
   for Physical in Extent range 1 .. 32 loop
      for Logical in Extent range 1 .. 32 loop
         for Low in Edge range 0 .. Physical - 1 loop
            for High in Edge range Low + 1 .. Physical loop
               Check_Axis (Low, High, Physical, Logical);
            end loop;
         end loop;
      end loop;
   end loop;
   -- Every changed nearest-neighbor sample must fall inside outward-rounded
   -- logical damage, then inside outward-rounded output damage.
   for Physical in Extent range 1 .. 12 loop
      for Logical in Extent range 1 .. 12 loop
         for Low in Edge range 0 .. Physical - 1 loop
            for High in Edge range Low + 1 .. Physical loop
               A := Axis (Low, High, Physical, Logical);
               for N in 1 .. 4 loop
                  for D in 1 .. 4 loop
                     for Pixel in 0 .. (Logical * N + D - 1) / D - 1 loop
                        declare
                           Sample : constant Natural :=
                             ((2 * Pixel + 1) * D * Physical) / (2 * N * Logical);
                        begin
                           if Sample >= Low and Sample < High then
                              pragma Assert (Pixel >= A.First * N / D);
                              pragma Assert (Pixel < (A.Last * N + D - 1) / D);
                           end if;
                           Pixel_Checks := Pixel_Checks + 1;
                        end;
                     end loop;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   for Physical of Samples loop
      for Logical of Samples loop
         Check_Axis (0, Physical, Physical, Logical);
         Check_Axis (0, 1, Physical, Logical);
         Check_Axis (Physical - 1, Physical, Physical, Logical);
         R := Map (Physical, Physical, Logical, Logical, (0, 0, Physical, Physical), False);
         pragma Assert (R = (0, 0, Logical, Logical));
         R := Map (Physical, Physical, Logical, Logical, (65_535, 0, 65_535, 1), False);
         pragma Assert (R = Empty);
         R := Map (Physical, Physical, Logical, Logical, (0, 0, 0, 0), False);
         pragma Assert (R = Empty);
         R := Map (Physical, Physical, Logical, Logical, (65_535, 65_535, 65_535, 65_535), True);
         pragma Assert (R = (0, 0, Logical, Logical));
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("SOURCE-DAMAGE-SAMPLES: PASS" & Natural'Image (Pixel_Checks));
   Ada.Text_IO.Put_Line ("SOURCE-DAMAGE: PASS" & Natural'Image (Count) & " conservative tight intervals and clipped/full rectangles");
end Source_Damage_Tests;
