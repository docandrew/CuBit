with Ada.Text_IO;
with Desktop_Composition;

--  Hosted geometry regression, not GPU execution or a Desktop IPC test.
--  Desktop currently expands a 64x64 Window_Surface request to 360x220.
--  Its client rectangle at the initial (80,64) position is (84,94,352,186).
--  The attached allocation nevertheless remains exactly 64x64 BGRA pixels.
procedure Triangle_Window_Geometry is
   package D renames Desktop_Composition;
   use type D.Blit_Plan;
   type Pixel_Array is array (Natural range 0 .. 4095) of Natural;
   Pixels : Pixel_Array := (others => 0);
   Client : constant D.Rectangle := (84, 94, 352, 186);
   Count : Natural := 0;

   procedure Check
     (Target_W, Target_H : Natural; Clipped : Boolean;
      Clip : D.Rectangle; Expected : D.Blit_Plan)
   is
      P : constant D.Blit_Plan := D.Plan
        (Target_W, Target_H, 64, 64, Client, Clipped, Clip);
      Red, Blue : Natural := 0;
   begin
      pragma Assert (P = Expected);
      if P.Width > 0 and P.Height > 0 then
         for Y in 0 .. P.Height - 1 loop
            for X in 0 .. P.Width - 1 loop
               --  Checked array indexing models each source read, including
               --  clipping offsets; the surrounding window is not backing.
               if Pixels ((P.Source_Y + Y) * 64 + P.Source_X + X) = 1 then
                  Red := Red + 1;
               else
                  Blue := Blue + 1;
               end if;
            end loop;
         end loop;
      end if;
      pragma Assert (Red + Blue = P.Width * P.Height);
      if P.Width = 64 and P.Height = 64 then
         pragma Assert (Red = 1152 and Blue = 2944);
      end if;
      Count := Count + 1;
   end Check;
begin
   --  Same doubled pixel-center oracle as the Vulkan triangle probe.
   for Y in 0 .. 63 loop
      for X in 0 .. 63 loop
         if 2 * Y + 1 > 16 and then
           2 * (2 * X + 1) - (2 * Y + 1) > 16 and then
           2 * (2 * X + 1) + (2 * Y + 1) < 240
         then
            Pixels (Y * 64 + X) := 1;
         end if;
      end loop;
   end loop;
   Check (1920, 1080, False, (others => 0), (84, 94, 0, 0, 64, 64));
   Check (1920, 1080, True, Client, (84, 94, 0, 0, 64, 64));
   Check (120, 130, False, (others => 0), (84, 94, 0, 0, 36, 36));
   Check (1920, 1080, True, (100, 110, 20, 20), (100, 110, 16, 16, 20, 20));
   --  Damage in the window's unused area must not read any source pixels.
   Check (1920, 1080, True, (148, 94, 200, 186), (others => 0));
   Check (1920, 1080, True, (84, 158, 352, 100), (others => 0));
   Ada.Text_IO.Put_Line ("Triangle window geometry PASS cases=" & Count'Image);
end Triangle_Window_Geometry;
