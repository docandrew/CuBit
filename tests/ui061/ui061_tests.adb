------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  UI-061 hosted tests: Client_Thumbnails (fit and box-filtered scale, with
--  timing), Desktop_Task_Cards (the hover card's layout and placement) and
--  CuBit.App_Names.Valid_Name (the rule Desktop applies to what procmgr
--  reads from the signed identity section).
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with Client_Thumbnails;
with Desktop_Task_Cards;
with CuBit.App_Names;

procedure UI061_Tests is
   package CT renames Client_Thumbnails;
   package TC renames Desktop_Task_Cards;
   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         Put_Line ("PASS " & Name);
      else
         Put_Line ("FAIL " & Name);
         Failures := Failures + 1;
      end if;
   end Check;

   function Same (A, B : CT.Size) return Boolean is (A.Width = B.Width and A.Height = B.Height);

   function BGR (B, G, R : Unsigned_32) return CT.Pixel is
     (16#FF00_0000# or Shift_Left (R, 16) or Shift_Left (G, 8) or B);

   ---------------------------------------------------------------------------
   procedure Fit_Tests is
   begin
      Check (Same (CT.Fit ((1920, 1080), (336, 192)), (336, 189)), "fit: wide window binds on width");
      Check (Same (CT.Fit ((800, 1200), (336, 192)), (128, 192)), "fit: tall window binds on height");
      Check (Same (CT.Fit ((200, 100), (336, 192)), (200, 100)), "fit: a small window is never enlarged");
      Check (Same (CT.Fit ((16_384, 1), (336, 192)), (336, 1)), "fit: a sliver keeps at least one row");
      Check (Same (CT.Fit ((1, 16_384), (336, 192)), (1, 192)), "fit: a needle keeps at least one column");
      Check (Same (CT.Fit ((336, 192), (336, 192)), (336, 192)), "fit: exact size unchanged");
   end Fit_Tests;

   ---------------------------------------------------------------------------
   procedure Scale_Tests is
      Sentinel : constant CT.Pixel := 16#1234_5678#;
   begin
      --  1:1 copies every pixel exactly (alpha made opaque).
      declare
         Source : CT.Pixel_Array (0 .. 5 * 4 - 1);
         Target : CT.Pixel_Array (0 .. 7 * 6 - 1) := [others => Sentinel];
         Exact : Boolean := True;
         Outside : Boolean := True;
      begin
         for I in Source'Range loop
            Source (I) := BGR (Unsigned_32 (I), Unsigned_32 (2 * I), Unsigned_32 (3 * I));
         end loop;
         CT.Scale (Source, (4, 4), 5, Target, 7, 2, 1, 4, 4, 0, 4);
         for Y in 0 .. 3 loop
            for X in 0 .. 3 loop
               Exact := Exact and then Target ((1 + Y) * 7 + 2 + X) = Source (Y * 5 + X);
            end loop;
         end loop;
         for Y in 0 .. 5 loop
            for X in 0 .. 6 loop
               if Y not in 1 .. 4 or else X not in 2 .. 5 then
                  Outside := Outside and then Target (Y * 7 + X) = Sentinel;
               end if;
            end loop;
         end loop;
         Check (Exact, "scale: 1:1 is an exact copy (pitch and offset honoured)");
         Check (Outside, "scale: pixels outside the box are untouched");
      end;
      --  2:1 averages each 2x2 block exactly.
      declare
         Source : constant CT.Pixel_Array (0 .. 3) :=
           [BGR (0, 0, 0), BGR (100, 40, 8), BGR (200, 80, 16), BGR (100, 40, 8)];
         Target : CT.Pixel_Array (0 .. 0) := [0];
      begin
         CT.Scale (Source, (2, 2), 2, Target, 1, 0, 0, 1, 1, 0, 1);
         Check (Target (0) = BGR (100, 40, 8), "scale: 2x2 to 1x1 is the mean");
      end;
      --  A one-pixel checkerboard reduced 8:1 is mid grey, not aliased.
      declare
         W : constant := 64;
         Source : CT.Pixel_Array (0 .. W * W - 1);
         Target : CT.Pixel_Array (0 .. 8 * 8 - 1) := [others => 0];
         Grey : Boolean := True;
      begin
         for Y in 0 .. W - 1 loop
            for X in 0 .. W - 1 loop
               Source (Y * W + X) := (if (X + Y) mod 2 = 0 then BGR (255, 255, 255) else BGR (0, 0, 0));
            end loop;
         end loop;
         CT.Scale (Source, (W, W), W, Target, 8, 0, 0, 8, 8, 0, 8);
         for P of Target loop
            Grey := Grey and then (P and 16#FF#) in 120 .. 136
              and then (Shift_Right (P, 8) and 16#FF#) in 120 .. 136;
         end loop;
         Check (Grey, "scale: a fine checkerboard averages to grey");
      end;
      --  A solid source stays that colour at any ratio, and the far edge
      --  row and column are reached.
      declare
         Source : constant CT.Pixel_Array (0 .. 37 * 23 - 1) := [others => BGR (10, 20, 30)];
         Target : CT.Pixel_Array (0 .. 5 * 3 - 1) := [others => 0];
      begin
         CT.Scale (Source, (37, 23), 37, Target, 5, 0, 0, 5, 3, 0, 3);
         Check ((for all P of Target => P = BGR (10, 20, 30)), "scale: odd ratios keep a solid colour");
      end;
      declare
         Source : CT.Pixel_Array (0 .. 9 * 9 - 1) := [others => BGR (0, 0, 0)];
         Target : CT.Pixel_Array (0 .. 2) := [others => 0];
      begin
         for Y in 0 .. 8 loop
            Source (Y * 9 + 8) := BGR (255, 255, 255);    --  the last column
         end loop;
         CT.Scale (Source, (9, 9), 9, Target, 3, 0, 0, 3, 1, 0, 1);
         Check ((Target (2) and 16#FF#) > 0 and then (Target (0) and 16#FF#) = 0,
                "scale: the last source column reaches the last target column only");
      end;
      --  Bands give the same picture as one pass, and touch only their rows.
      declare
         Source : CT.Pixel_Array (0 .. 50 * 40 - 1);
         Whole, Banded : CT.Pixel_Array (0 .. 12 * 9 - 1) := [others => Sentinel];
      begin
         for I in Source'Range loop
            Source (I) := BGR (Unsigned_32 (I mod 256), Unsigned_32 ((I / 7) mod 256), Unsigned_32 ((I / 3) mod 256));
         end loop;
         CT.Scale (Source, (50, 40), 50, Whole, 12, 0, 0, 12, 9, 0, 9);
         CT.Scale (Source, (50, 40), 50, Banded, 12, 0, 0, 12, 9, 0, 4);
         Check ((for all I in 4 * 12 .. Banded'Last => Banded (I) = Sentinel), "scale: a band leaves later rows");
         CT.Scale (Source, (50, 40), 50, Banded, 12, 0, 0, 12, 9, 4, 5);
         Check (CT."=" (Whole, Banded), "scale: bands make the same picture");
      end;
   end Scale_Tests;

   ---------------------------------------------------------------------------
   --  The frame cost: a 1920x1080 window into the card's largest thumbnail.
   procedure Timing is
      SW : constant := 1920;
      SH : constant := 1080;
      type Source_Access is access CT.Pixel_Array;
      Source : constant Source_Access := new CT.Pixel_Array (0 .. SW * SH - 1);
      Fitted : constant CT.Size := CT.Fit ((SW, SH), TC.Thumbnail_Bound);
      Target : CT.Pixel_Array (0 .. TC.Width * TC.Thumbnail_Height_Bound - 1) := [others => 0];
      Rounds : constant := 500;
      Start : Time;
      Elapsed : Duration;
   begin
      for I in Source'Range loop
         Source (I) := BGR (Unsigned_32 (I mod 251), Unsigned_32 (I mod 241), Unsigned_32 (I mod 239));
      end loop;
      Start := Clock;
      for R in 1 .. Rounds loop
         CT.Scale (Source.all, (SW, SH), SW, Target, TC.Width, 0, 0, Fitted.Width, Fitted.Height,
                   0, Fitted.Height);
      end loop;
      Elapsed := (Clock - Start) / Rounds;
      Put_Line ("TIME: thumbnail" & Natural'Image (SW) & "x" & Natural'Image (SH) & " ->" &
                Natural'Image (Fitted.Width) & "x" & Natural'Image (Fitted.Height) & ":" &
                Natural'Image (Natural (Elapsed * 1_000_000)) & " us per scale");
      Check (Target (0) /= 0, "timing: the scale wrote pixels");
   end Timing;

   ---------------------------------------------------------------------------
   procedure Card_Tests is
      One : constant TC.Layout := TC.Plan ((Windows => 1, Note => False, Has_Thumbnail => True,
                                            Thumbnail => (336, 189)));
      Many : constant TC.Layout := TC.Plan ((Windows => 11, Note => True, Has_Thumbnail => True,
                                             Thumbnail => (128, 192)));
      Bare : constant TC.Layout := TC.Plan ((Windows => 2, Note => False, Has_Thumbnail => False,
                                             Thumbnail => (1, 1)));
      P : TC.Placement;
   begin
      Check (One.Shown = 1 and then One.More.H = 0 and then One.Note.H = 0, "card: one window, one row");
      Check (One.Thumbnail.W = 336 and then One.Thumbnail.H = 189 and then One.Thumbnail.X = TC.Padding,
             "card: a full-width thumbnail sits at the padding");
      Check (One.Rows (1).Y >= One.Thumbnail.Y + One.Thumbnail.H, "card: the row is below the thumbnail");
      Check (Many.Shown = TC.Maximum_Rows and then Many.More.H > 0, "card: eleven windows, eight rows and more");
      Check (Many.Thumbnail.X = TC.Padding + (TC.Width - 2 * TC.Padding - 128) / 2,
             "card: a narrow thumbnail is centred");
      Check (Many.Note.H > 0 and then Many.Thumbnail.Y = Many.Note.Y + Many.Note.H,
             "card: the note sits between header and thumbnail");
      Check (Many.Height <= TC.Maximum_Height, "card: within its largest height");
      Check (Bare.Thumbnail.H = 0 and then Bare.Rows (1).Y = Bare.Header.Y + Bare.Header.H,
             "card: no pixels yet, no thumbnail gap");
      P := TC.Place (Button_X => 900, Screen_Left => 0, Screen_Right => 1024, Bar_Top => 727,
                     Height => One.Height);
      Check (P.Fits and then P.X = 1024 - TC.Width and then P.Y + One.Height + TC.Margin = 727,
             "card: kept on screen at the right edge, above the bar");
      P := TC.Place (115, 0, 1024, 727, One.Height);
      Check (P.Fits and then P.X = 115, "card: left-aligned with its button");
      P := TC.Place (0, 0, 200, 727, One.Height);
      Check (not P.Fits, "card: a screen narrower than the card shows none");
      P := TC.Place (0, 0, 1024, 100, One.Height);
      Check (not P.Fits, "card: no room above the bar shows none");
   end Card_Tests;

   ---------------------------------------------------------------------------
   procedure Name_Tests is
      use CuBit.App_Names;
      --  Out of line, so the checks run here rather than fold at compile time.
      function Valid (Text : String) return Boolean with No_Inline;
      function Valid (Text : String) return Boolean is (Valid_Name (Text));
   begin
      Check (Valid ("Files") and then Valid ("CCL Workbench") and then Valid (String'(1 .. 32 => 'x')),
             "names: ordinary names");
      Check (not Valid ("") and then not Valid (" Files") and then not Valid ("Files ")
             and then not Valid ("Two  Spaces") and then not Valid ("Ctl" & ASCII.SOH)
             and then not Valid (String'(1 .. 33 => 'x')) and then not Valid ("Tab" & ASCII.HT & "x"),
             "names: padded, doubled, control and overlong names are refused");
   end Name_Tests;
begin
   --  "time": only the timing, from the release build (run.sh).
   if Ada.Command_Line.Argument_Count = 1 and then Ada.Command_Line.Argument (1) = "time" then
      Timing;
      return;
   end if;
   Fit_Tests;
   Scale_Tests;
   Card_Tests;
   Name_Tests;
   if Failures = 0 then
      Put_Line ("ui061: all passed");
   else
      Put_Line ("ui061:" & Natural'Image (Failures) & " failed");
      raise Program_Error;
   end if;
end UI061_Tests;
