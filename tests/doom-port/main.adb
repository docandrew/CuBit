--  Hosted checks of DOOM's proved units against independent references:
--  the C port's scan-code table and lump rules, and a direct mixing model.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;

with CuBit.Doom_Keys; use CuBit.Doom_Keys;
with CuBit.Doom_Lumps;
with CuBit.Doom_Mixer;

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
      Q : Queue := Empty_Queue;
      E : Event;
      Found : Boolean;
   begin
      --  Spot checks from the C port's table (doomgeneric_cubit.c).
      Check (Translate (16#01#) = 27, "escape");
      Check (Translate (16#1C#) = 13, "enter");
      Check (Translate (16#1D#) = 16#A3#, "left ctrl fires");
      Check (Translate (16#39#) = 16#A2#, "space uses");
      Check (Translate (16#38#) = 16#80# + 16#38#, "left alt strafes");
      Check (Translate (16#10#) = Character'Pos ('q'), "q");
      Check (Translate (16#48#) = 16#AD#, "up arrow");
      Check (Translate (16#4C#) = Character'Pos ('5'), "keypad 5");
      Check (Translate (16#57#) = 16#80# + 16#57#, "F11");
      Check (Translate (16#54#) = No_Key, "0x54 unmapped");
      Check (Translate (16#59#) = No_Key, "0x59 unmapped");
      Check (Translate (127) = No_Key, "0x7f unmapped");
      Check (Translate (0) = No_Key, "0 unmapped");
      declare
         Mapped : Natural := 0;
      begin
         for Code in Scancode loop
            if Translate (Code) /= No_Key then
               Mapped := Mapped + 1;
            end if;
         end loop;
         --  0x01 .. 0x53 (83 codes) plus F11 and F12.
         Check (Mapped = 85, "mapped scan codes:" & Mapped'Image);
      end;

      --  FIFO order, drops when full, ignored keys dropped.
      Push (Q, (Code => No_Key, Pressed => True));
      Check (Length (Q) = 0, "no-key event dropped");
      for I in 1 .. Queue_Capacity + 5 loop
         Push (Q, (Code => Key (I), Pressed => I mod 2 = 0));
      end loop;
      Check (Length (Q) = Queue_Capacity, "full queue holds capacity");
      for I in 1 .. Queue_Capacity loop
         Pop (Q, E, Found);
         Check (Found and then E.Code = Key (I)
                and then E.Pressed = (I mod 2 = 0), "FIFO order" & I'Image);
      end loop;
      Pop (Q, E, Found);
      Check (not Found and Length (Q) = 0, "empty after draining");
      --  Wrap around the ring.
      for Round in 1 .. 3 loop
         for I in 1 .. 20 loop
            Push (Q, (Code => Key (I + Round), Pressed => True));
         end loop;
         for I in 1 .. 20 loop
            Pop (Q, E, Found);
            Check (Found and then E.Code = Key (I + Round), "wrapped order");
         end loop;
      end loop;
   end Keys_Tests;

   procedure Lumps_Tests is
      use CuBit.Doom_Lumps;
      function Head (Tag, Rate : Unsigned_16; Count : Unsigned_32) return Header is
        [Unsigned_8 (Tag mod 256), Unsigned_8 (Tag / 256),
         Unsigned_8 (Rate mod 256), Unsigned_8 (Rate / 256),
         Unsigned_8 (Count mod 256), Unsigned_8 (Count / 2 ** 8 mod 256),
         Unsigned_8 (Count / 2 ** 16 mod 256), Unsigned_8 (Count / 2 ** 24)];
      S : Sound;
   begin
      S := Parse (Head (3, 11_025, 1000), 1008);
      Check (S.Valid and then S.Rate = 11_025 and then S.First = 24
             and then S.Count = 968, "padded sound");
      S := Parse (Head (3, 11_025, 20), 28);
      Check (S.Valid and then S.First = 8 and then S.Count = 20,
             "short sound keeps its padding");
      S := Parse (Head (3, 11_025, 32), 40);
      Check (S.Valid and then S.First = 8 and then S.Count = 32,
             "exactly two paddings is not trimmed");
      S := Parse (Head (3, 11_025, 33), 41);
      Check (S.Valid and then S.First = 24 and then S.Count = 1,
             "one sample beyond the paddings");
      S := Parse (Head (3, 11_025, 7), 15);
      Check (not S.Valid, "fewer than 8 samples refused");
      S := Parse (Head (2, 11_025, 100), 108);
      Check (not S.Valid, "wrong format tag refused");
      S := Parse (Head (3, 0, 100), 108);
      Check (not S.Valid, "zero rate refused");
      S := Parse (Head (3, 11_025, 5000), 108);
      Check (S.Valid and then S.First = 24 and then S.Count = 68,
             "count cut to the lump");
      --  The C version computed data_len + 8 in 32 bits: this wrapped to 4
      --  and passed its bound check, reading far past the lump.
      S := Parse (Head (3, 11_025, Unsigned_32'Last - 3), 108);
      Check (S.Valid and then S.First + S.Count <= 108,
             "huge declared count stays in the lump");
      S := Parse (Head (3, 11_025, 100), 8);
      Check (not S.Valid, "header only refused");
   end Lumps_Tests;

   procedure Mixer_Tests is
      use CuBit.Doom_Mixer;
      Left, Right : Volume;
      Sums : Mix_Buffer;
      C : Channel;
   begin
      --  The C port's panning: vol * (254 - sep) / 254, clamped.
      for Vol in 0 .. 127 loop
         for Sep in 0 .. 254 loop
            Pan (Vol, Sep, Left, Right);
            Check (Left = Vol * (254 - Sep) / 254 and Right = Vol * Sep / 254,
                   "pan" & Vol'Image & Sep'Image);
         end loop;
      end loop;
      Pan (500, -9, Left, Right);
      Check (Left = 127 and Right = 0, "pan clamps");

      --  48 kHz source: one sample per frame.
      declare
         Samples : constant Byte_Array (0 .. 99) :=
           [for I in 0 .. 99 => Unsigned_8 ((I * 37) mod 256)];
      begin
         Sums := [others => 0];
         C := Started (100, 48_000, 100, 128);
         Mix (Samples, C, Sums);
         Check (not C.Active, "channel ends");
         for F in 0 .. 99 loop
            Check (Sums (2 * F) = (Integer_32 (Samples (F)) - 128)
                     * Integer_32 (C.Left) * 2
                   and Sums (2 * F + 1) = (Integer_32 (Samples (F)) - 128)
                     * Integer_32 (C.Right) * 2, "mixed frame" & F'Image);
         end loop;
         Check (Sums (200) = 0 and Sums (2 * Mix_Frames - 1) = 0,
                "silence after the end");
      end;

      --  A long 11025 Hz sound: positions past 65536 samples (where the C
      --  version's 32-bit 16.16 position wrapped) keep advancing.
      declare
         Length : constant := 200_000;
         Samples : constant Byte_Array (0 .. Length - 1) :=
           [for I in 0 .. Length - 1 => Unsigned_8 (I mod 251)];
         Batches : Natural := 0;
         Expected_Step : constant Position := 11_025 * 65_536 / 48_000;
      begin
         C := Started (Length, 11_025, 127, 0);
         while C.Active loop
            Sums := [others => 0];
            Mix (Samples, C, Sums);
            Batches := Batches + 1;
            if C.Active then
               Check (C.At_Sample = Position (Batches * Mix_Frames)
                        * Expected_Step, "position" & Batches'Image);
            end if;
            exit when Batches > 10_000;
         end loop;
         --  200000 samples at 11025 Hz last about 870 000 output frames.
         Check (Position (Batches) =
                  (Length * 65_536 + Expected_Step - 1) / Expected_Step
                  / Mix_Frames + 1,
                "long sound plays to its end:" & Batches'Image);
      end;

      Check (Clamped (40_000) = 32_767 and Clamped (-40_000) = -32_768
             and Clamped (123) = 123, "clamp");
   end Mixer_Tests;
begin
   Keys_Tests;
   Lumps_Tests;
   Mixer_Tests;
   if Failures = 0 then
      Put_Line ("DOOM-PORT: PASS" & Checks'Image & " checks");
   else
      Put_Line ("DOOM-PORT: FAIL" & Failures'Image & " of" & Checks'Image);
      raise Program_Error;
   end if;
end Main;
