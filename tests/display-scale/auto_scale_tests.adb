--  Hosted tests of Desktop_Auto_Scale: the scale an output gets by default.
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with CuBit.Display_Arrangement;
with Desktop_Auto_Scale;

procedure Auto_Scale_Tests is
   package D renames CuBit.Display_Arrangement;
   package R renames Desktop_Auto_Scale;
   use type D.Scale_Preset;
   Checks, Failures : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;
begin
   Check (R.Choose (1024, 768) = D.Scale_100, "1024x768 at 100%");
   Check (R.Choose (1920, 1080) = D.Scale_100, "1920x1080 at 100%");
   Check (R.Choose (1920, 1200) = D.Scale_100, "1920x1200 at 100%");
   Check (R.Choose (2559, 1440) = D.Scale_100, "just under QHD at 100%");
   Check (R.Choose (2560, 1440) = D.Scale_150, "2560x1440 at 150%");
   Check (R.Choose (2560, 1600) = D.Scale_150, "2560x1600 at 150%");
   Check (R.Choose (3839, 2160) = D.Scale_150, "just under UHD at 150%");
   Check (R.Choose (3840, 2160) = D.Scale_200, "3840x2160 at 200%");
   Check (R.Choose (5120, 2880) = D.Scale_200, "5K at 200%");
   Check (R.Choose (2160, 3840) = D.Scale_200, "portrait 4K at 200%");
   Check (R.Choose (1440, 2560) = D.Scale_150, "portrait QHD at 150%");
   --  A very wide, short panel steps down to the largest preset leaving
   --  480 logical rows: 3840x600 -> 125% (480 rows).
   Check (R.Choose (3840, 600) = D.Scale_125, "3840x600 steps down to 125%");
   Check (R.Choose (3840, 479) = D.Scale_100, "nothing fits: 100%");
   Check (R.Fits (3840, 2160, D.Scale_200) and then not R.Fits (1500, 900, D.Scale_200), "fits");
   Check (R.Logical (3840, D.Scale_200) = 1920 and then R.Logical (1025, D.Scale_200) = 513,
          "logical extents round up");
   --  The result never exceeds what the size asks for and always fits.
   for W in 1 .. 80 loop
      for H in 1 .. 50 loop
         declare
            Width : constant R.G.Physical_Extent := R.G.Physical_Extent (W * 64);
            Height : constant R.G.Physical_Extent := R.G.Physical_Extent (H * 64);
            P : constant D.Scale_Preset := R.Choose (Width, Height);
         begin
            Check (P <= R.By_Size (Width, Height) and then
                   (P = D.Scale_100 or else R.Fits (Width, Height, P)), "choice bounded and fits");
         end;
      end loop;
   end loop;
   Put_Line ("auto scale:" & Checks'Image & " checks," & Failures'Image & " failures");
   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Auto_Scale_Tests;
