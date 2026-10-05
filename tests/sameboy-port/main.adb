--  Hosted checks of SameBoy's proved units against independent references:
--  the C frontend's bindings and arithmetic.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;

with CuBit.SameBoy_Batches; use CuBit.SameBoy_Batches;
with CuBit.SameBoy_Frames;  use CuBit.SameBoy_Frames;
with CuBit.SameBoy_Keys;    use CuBit.SameBoy_Keys;

procedure Main is
   Failures : Natural := 0;
   Checks   : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   procedure Keys_Tests is
      type Expected is record
         Code : Scancode;
         Item : Binding;
      end record;
      --  The C frontend's switch (sameboy/main.c).
      Table : constant array (Positive range <>) of Expected :=
        [(16#48#, (True, Up)), (16#50#, (True, Down)), (16#4B#, (True, Left)),
         (16#4D#, (True, Right)), (16#2C#, (True, B)), (16#2D#, (True, A)),
         (16#0F#, (True, Select_Button)), (16#1C#, (True, Start)),
         (16#01#, (False, Quit)), (16#19#, (False, Toggle_Pause)),
         (16#3C#, (False, Next_Rom)), (16#3F#, (False, Reset)),
         (16#42#, (False, Toggle_Mute)), (16#43#, (False, Quieter)),
         (16#44#, (False, Louder))];
      Held : Held_Keys := No_Keys_Held;
      Press : Boolean;
      Listed : Boolean;
   begin
      for Code in Scancode loop
         Listed := False;
         for E of Table loop
            if E.Code = Code then
               Listed := True;
               Check (Bound (Code) = E.Item, "binding" & Code'Image);
            end if;
         end loop;
         if not Listed then
            Check (Bound (Code) = (False, No_Command), "unbound" & Code'Image);
         end if;
      end loop;
      Note (Held, 16#19#, True, Press);
      Check (Press, "first press");
      Note (Held, 16#19#, True, Press);
      Check (not Press, "auto-repeat is not a press");
      Note (Held, 16#19#, False, Press);
      Check (not Press and not Held (16#19#), "release");
      Note (Held, 16#19#, True, Press);
      Check (Press, "press after release");
      Check (Quieter (70) = 65 and Quieter (4) = 4 and Quieter (5) = 0,
             "quieter");
      Check (Louder (70) = 75 and Louder (96) = 96 and Louder (95) = 100,
             "louder");
   end Keys_Tests;

   procedure Frames_Tests is
      Screen : Screen_Pixels;
      View   : View_Pixels;
      type Numbers is array (Positive range <>) of Natural;
      Rates : constant Numbers := [4_194_304, 8_388_608, 1, 3];
      Cycle_Counts : constant Numbers :=
        [0, 1, 70_224 * 2, 140_448 * 2, 123_456_789];
   begin
      for I in Screen'Range loop
         Screen (I) := Unsigned_32 (I) * 2_654_435_761;
      end loop;
      Enlarge (Screen, View);
      --  The C frontend's loop: view (y, x) = screen (y / 3, x / 3).
      for Y in 0 .. View_Height - 1 loop
         for X in 0 .. View_Width - 1 loop
            if View (Y * View_Width + X) /=
              Screen ((Y / Scale) * Screen_Width + X / Scale)
            then
               Check (False, "pixel" & X'Image & Y'Image);
            end if;
         end loop;
      end loop;
      Check (True, "enlarged picture");
      --  cycles * 10**9 / (2 * rate), exactly, for frame-sized runs.
      for Rate of Rates loop
         for Cycles of Cycle_Counts loop
            Check (Nanoseconds (Unsigned_64 (Cycles), Unsigned_32 (Rate)) =
                     Unsigned_64 (Cycles) * 1_000_000_000
                       / (2 * Unsigned_64 (Rate)),
                   "nanoseconds" & Cycles'Image & Rate'Image);
         end loop;
      end loop;
      Check (Nanoseconds (Unsigned_64'Last, 1) =
               Nanoseconds (Maximum_Cycles, 1), "saturates");
   end Frames_Tests;

   procedure Batches_Tests is
      B : Batch;
   begin
      Clear (B);
      for I in 1 .. 100 loop
         Append (B, Stereo_Frame (I));
      end loop;
      Check (Waiting (B) = 100, "appended");
      Accept_Written (B, 30);
      Check (Waiting (B) = 70 and B.Frames (B.First) = 31, "partial write");
      Accept_Written (B, 70);
      Check (Waiting (B) = 0 and B.Count = 0, "drained batch restarts");
      for I in 1 .. Capacity + 3 loop
         Append (B, Stereo_Frame (I));
      end loop;
      Check (B.Overflow and Waiting (B) = Capacity, "overflow noted");
      Check (B.Frames (Capacity - 1) = Stereo_Frame (Capacity),
             "overflow keeps the earlier frames");
      Clear (B);
      Check (not B.Overflow and Waiting (B) = 0, "clear");
   end Batches_Tests;
begin
   Keys_Tests;
   Frames_Tests;
   Batches_Tests;
   if Failures = 0 then
      Put_Line ("SAMEBOY-PORT: PASS" & Checks'Image & " checks");
   else
      Put_Line ("SAMEBOY-PORT: FAIL" & Failures'Image & " of" & Checks'Image);
      raise Program_Error;
   end if;
end Main;
