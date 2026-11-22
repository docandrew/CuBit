with Ada.Text_IO;
with Interfaces; use Interfaces;
with Compositor_Gradient;
procedure Gradient_Tests is
   package G renames Compositor_Gradient;
   Accumulator : Integer;
   Previous, Current : G.Byte;
begin
   -- Independent scalar oracle over every 8-bit channel/coverage combination.
   for Top in 0 .. 255 loop
      for Bottom in 0 .. 255 loop
         -- Walk the line by a signed difference; do not reproduce the
         -- implementation's pair of weighted channel products.
         Accumulator := Top * 255 + 127;
         for Alpha in 0 .. 255 loop
            pragma Assert (G.Channel (Top, Bottom, Alpha) = Accumulator / 255);
            pragma Assert (G.Channel (Top, Bottom, Alpha) =
              G.Channel (Bottom, Top, 255 - Alpha));
            Accumulator := Accumulator + Bottom - Top;
         end loop;
      end loop;
   end loop;
   for Height in 1 .. 4096 loop
      Previous := 0;
      for Row in 0 .. Height - 1 loop
         Current := G.Weight (Row, Height);
         pragma Assert (Current >= Previous);
         if Height > 1 then
            pragma Assert (Current * (Height - 1) <= Row * 255);
            pragma Assert ((Current + 1) * (Height - 1) > Row * 255);
         end if;
         Previous := Current;
      end loop;
   end loop;
   -- Independently walk every row in each emitted band, including clipped
   -- starts. Verify exact coverage, maximal runs and bounded operation count.
   for Height in 1 .. 4096 loop
      declare
         Row : Natural := 0;
         Last, Runs : Natural := 0;
      begin
         while Row < Height loop
            Last := G.Run_Last (Row, Height);
            pragma Assert (Last >= Row and Last < Height);
            for Y in Row .. Last loop
               pragma Assert (G.Weight (Y, Height) = G.Weight (Row, Height));
               pragma Assert (G.Run_Last (Y, Height) = Last);
            end loop;
            if Last < Height - 1 then
               pragma Assert (G.Weight (Last + 1, Height) /= G.Weight (Row, Height));
            end if;
            Runs := Runs + 1;
            Row := Last + 1;
         end loop;
         pragma Assert (Runs <= 256);
      end;
   end loop;
   pragma Assert (G.Run_Last (Natural'Last - 1, Natural'Last) = Natural'Last - 1);
   pragma Assert (G.Run_Last (0, Natural'Last) < Natural'Last);
   pragma Assert (G.Weight (Natural'Last - 1, Natural'Last) = 255);
   pragma Assert (G.At_Row (16#FF12_3456#, 16#AAFE_DCBA#, 0, 1) = 16#FF12_3456#);
   for Height in 2 .. 4096 loop
      pragma Assert (G.At_Row (16#FF12_3456#, 16#AAFE_DCBA#, 0, Height) = 16#0012_3456#);
      pragma Assert (G.At_Row (16#FF12_3456#, 16#AAFE_DCBA#, Height - 1, Height) = 16#00FE_DCBA#);
   end loop;
   Ada.Text_IO.Put_Line ("PASS gradient: 16777216 channel cases, row weights through 4096, maximum extent, maximal bands and RGB endpoints");
end Gradient_Tests;
