with Ada.Text_IO;
with Interfaces; use Interfaces;
with Compositor_Gradient;
procedure Gradient_Color_Tests is
   package G renames Compositor_Gradient;
   type Pair is record Top, Bottom : Unsigned_32; end record;
   Colors : constant array (Positive range <>) of Pair :=
     [(0, 0), (16#FF12_3456#, 16#AA12_3456#),
      (16#0020_2020#, 16#0030_3030#), (16#0030_3030#, 16#0020_2020#),
      (0, 16#00FF_FFFF#), (16#00FF_FFFF#, 0),
      (16#0030_E410#, 16#00DA_1905#), (16#FF00_FF00#, 16#00FF_00FF#),
      (16#0010_1010#, 16#0011_1010#), (16#0011_1010#, 16#0010_1010#)];
   Heights : constant array (Positive range <>) of Positive :=
     [1, 2, 3, 17, 255, 256, 257, 2160, 4096];
   Pixels, Cases : Natural := 0;

   function Reference (P : Pair; Row : Natural; Height : Positive) return Unsigned_32 is
      Result : Unsigned_32 := 0;
      Alpha : Natural;
      First, Last, Accumulator : Integer;
   begin
      if Height = 1 then return P.Top; end if;
      Alpha := Natural (Long_Long_Integer (Row) * 255 / Long_Long_Integer (Height - 1));
      for Component in 0 .. 2 loop
         First := Integer (Shift_Right (P.Top, Component * 8) and 255);
         Last := Integer (Shift_Right (P.Bottom, Component * 8) and 255);
         -- Independent signed-difference form of the scalar row painter.
         Accumulator := First * 255 + 127 + (Last - First) * Alpha;
         Result := Result or Shift_Left (Unsigned_32 (Accumulator / 255), Component * 8);
      end loop;
      return Result;
   end Reference;
begin
   for P of Colors loop
      for Height of Heights loop
         for Clip in 0 .. 2 loop
            declare
               Row : Natural := (case Clip is when 0 => 0,
                 when 1 => Height / 3, when others => Height - 1);
               Last, Runs : Natural := 0;
               Color : Unsigned_32;
            begin
               while Row < Height loop
                  Color := Reference (P, Row, Height);
                  Last := G.Color_Run_Last (P.Top, P.Bottom, Row, Height);
                  pragma Assert (Last >= Row and Last < Height);
                  for Y in Row .. Last loop
                     pragma Assert (Reference (P, Y, Height) = Color);
                     pragma Assert (G.At_Row (P.Top, P.Bottom, Y, Height) = Color);
                     Pixels := Pixels + 1;
                  end loop;
                  if Last < Height - 1 then
                     pragma Assert (Reference (P, Last + 1, Height) /= Color);
                  end if;
                  Runs := Runs + 1;
                  Row := Last + 1;
               end loop;
               pragma Assert (Runs <= Natural'Min (Height, 256));
               if Height = 2160 and Clip = 0 then
                  if P = Colors (1) then pragma Assert (Runs = 1); end if;
                  if P = Colors (3) then pragma Assert (Runs = 17); end if;
                  if P = Colors (5) then pragma Assert (Runs = 256); end if;
               end if;
               Cases := Cases + 1;
            end;
         end loop;
      end loop;
   end loop;
   -- Arithmetic near Natural'Last without iterating over billions of pixels.
   pragma Assert (G.Color_Run_Last (0, 0, Natural'Last - 100, Positive'Last) = Natural'Last - 1);
   for P of Colors loop
      declare
         Row : constant Natural := Natural'Last - 100;
         Last : constant Natural := G.Color_Run_Last (P.Top, P.Bottom, Row, Positive'Last);
      begin
         for Y in Row .. Last loop
            pragma Assert (Reference (P, Y, Positive'Last) = Reference (P, Row, Positive'Last));
         end loop;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("PASS exact RGB gradient runs:" & Natural'Image (Cases) &
     " clipped cases," & Natural'Image (Pixels) & " exact pixels; 2160-row flat=1, subtle=17, full-range=256 fills");
end Gradient_Color_Tests;
