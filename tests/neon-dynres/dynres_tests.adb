------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Linux-hosted checks of Neon Blast's dynamic resolution (APP-008): the
--  controller against a modelled renderer (render time proportional to
--  pixels, scale time to the window's), the present scaler's arithmetic,
--  and the software renderer's buffer reuse. Not a CuBit result.
------------------------------------------------------------------------------
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with Neon_Resolution; use Neon_Resolution;
with Neon_Upscale;
with Neon_Game;
with Neon_Scene;
with Neon_Soft;

procedure Dynres_Tests is
   Failed : Boolean := False;
   procedure Check (Ok : Boolean; What : String) is
   begin
      Put_Line ("NEON-DYNRES: " & (if Ok then "PASS " else "FAIL ") & What);
      Failed := Failed or else not Ok;
   end Check;

   function Image (N : Integer) return String is
      S : constant String := Integer'Image (N);
   begin
      return (if N < 0 then S else S (S'First + 1 .. S'Last));
   end Image;

   ---------------------------------------------------------------------------
   --  A modelled renderer: Render_Ps_Per_Pixel picoseconds per internal
   --  pixel, plus Scale_Ps_Per_Pixel per window pixel when scaling.
   ---------------------------------------------------------------------------
   Picoseconds_Per_Microsecond : constant := 1_000_000;
   type Model is record
      Render_Ps_Per_Pixel, Scale_Ps_Per_Pixel : Natural;
   end record;

   function Clamp (Us : Long_Long_Integer) return Microseconds is
     (if Us > Long_Long_Integer (Maximum_Measure_Us) then Maximum_Measure_Us else Microseconds (Us));

   function Render_Us (M : Model; W, H : Extent; L : Level) return Microseconds is
     (Clamp (Long_Long_Integer (Internal (W, L)) * Long_Long_Integer (Internal (H, L))
             * Long_Long_Integer (M.Render_Ps_Per_Pixel) / Picoseconds_Per_Microsecond));

   function Scale_Us (M : Model; W, H : Extent; L : Level) return Microseconds is
     (if L = Native then 0
      else Clamp (Long_Long_Integer (W) * Long_Long_Integer (H) * Long_Long_Integer (M.Scale_Ps_Per_Pixel)
                  / Picoseconds_Per_Microsecond));

   type Run_Result is record
      Final : Level;
      Changes, Late_Changes : Natural;
      Worst_Late_Us : Natural;      --  the worst render + scale over the last quarter
   end record;

   --  Frames frames of M at W x H from C; the last quarter is "late".
   function Run (C : in out Controller; M : Model; W, H : Extent; Frames : Positive) return Run_Result is
      Result : Run_Result := (Final => Current (C), others => 0);
      Before : Level;
      R, S : Microseconds;
   begin
      for Frame in 1 .. Frames loop
         Before := Current (C);
         R := Render_Us (M, W, H, Before);
         S := Scale_Us (M, W, H, Before);
         Note_Frame (C, W, H, R, S);
         if Current (C) /= Before then
            Result.Changes := Result.Changes + 1;
            if Frame > Frames * 3 / 4 then
               Result.Late_Changes := Result.Late_Changes + 1;
            end if;
         end if;
         if Frame > Frames * 3 / 4 then
            Result.Worst_Late_Us := Natural'Max (Result.Worst_Late_Us, R + S);
         end if;
      end loop;
      Result.Final := Current (C);
      return Result;
   end Run;

   procedure Controller_Checks is
      C : Controller := Initial;
      --  The T580's measured cost: 51.6 ms for a ~2560 x 1600 window
      --  (about 12.6 ns a pixel). The scale: 0.5 ns a window pixel.
      Slow : constant Model := (Render_Ps_Per_Pixel => 12_600, Scale_Ps_Per_Pixel => 500);
      Fast : constant Model := (Render_Ps_Per_Pixel => 1_500, Scale_Ps_Per_Pixel => 500);
      Quiet : constant Model := (Render_Ps_Per_Pixel => 3_000, Scale_Ps_Per_Pixel => 500);
      Out_W : constant := 2560;
      Out_H : constant := 1600;
      R : Run_Result;
      Dropped_To : Level;
   begin
      --  The bounds.
      Check (Internal (Out_W, Native) = Out_W and then Internal (Out_H, Lowest) = Out_H / 4,
             "the ladder runs from the window's own size to 25%");
      Check (Floor_For (2160) = Lowest and then Floor_For (1600) = Lowest,
             "a 1600- or 2160-line window may drop to 25%");
      Check (Floor_For (1080) = Level (9) and then Internal (1080, Floor_For (1080)) = 378,
             "a 1080-line window stops at 35% (378 lines)");
      Check (Internal (720, Floor_For (720)) >= Minimum_Lines and then Floor_For (720) = Level (6),
             "a 720-line window stops at 50% (360 lines)");
      Check (Floor_For (399) = Native and then Floor_For (360) = Native,
             "a window under 400 lines stays at its own size");
      for L in Level range Native + 1 .. Lowest loop
         if Scale_Of (L) >= Scale_Of (L - 1) then
            Check (False, "the ladder falls at every step");
         end if;
      end loop;

      --  A fast machine stays at the window's resolution.
      R := Run (C, (Render_Ps_Per_Pixel => 2_000, Scale_Ps_Per_Pixel => 500), 1024, 640, 600);
      Check (R.Final = Native and then R.Changes = 0, "a frame inside the budget stays native");

      --  The slow machine on a big window: it drops, fits, and holds.
      C := Initial;
      R := Run (C, Slow, Out_W, Out_H, 600);
      Dropped_To := R.Final;
      Put_Line ("NEON-DYNRES: slow window " & Image (Out_W) & "x" & Image (Out_H) & " -> level "
                & Image (Integer (Dropped_To)) & " (" & Image (Scale_Of (Dropped_To)) & "%, "
                & Image (Internal (Out_W, Dropped_To)) & "x" & Image (Internal (Out_H, Dropped_To))
                & ") changes " & Image (R.Changes) & " worst late " & Image (R.Worst_Late_Us) & " us");
      Check (Dropped_To > Native, "a frame over the budget drops the resolution");
      Check (R.Worst_Late_Us <= Budget_Us, "after dropping, render plus scale fits the budget");
      Check (R.Changes <= 3, "it gets there in a few changes, not a step a frame");
      Check (R.Late_Changes = 0, "and then holds (no oscillation)");
      Check (Drops (C) >= 1 and then Raises (C) <= 1, "the changes are counted");

      --  The load lifts (the burners stop, a cooler CPU): it recovers.
      R := Run (C, Quiet, Out_W, Out_H, 1200);
      Check (R.Final < Dropped_To, "with time to spare it raises the resolution again");
      Check (R.Late_Changes = 0, "and settles without oscillating");
      Check (R.Worst_Late_Us <= Budget_Us, "still inside the budget once settled");
      R := Run (C, Fast, Out_W, Out_H, 1200);
      Check (R.Final = Native, "a fast frame returns to the window's own resolution");

      --  One slow frame (a page fault, a preemption) does not drop it.
      C := Initial;
      for Frame in 1 .. 300 loop
         Note_Frame (C, 1024, 640, (if Frame = 150 then 60_000 else 4_000), 0);
      end loop;
      Check (Current (C) = Native and then Drops (C) = 0, "a single 60 ms frame does not drop it");

      --  A small window: never below its floor.
      C := Initial;
      R := Run (C, (Render_Ps_Per_Pixel => 100_000, Scale_Ps_Per_Pixel => 0), 1024, 400, 300);
      Check (R.Final = Floor_For (400) and then Internal (400, R.Final) >= Minimum_Lines,
             "a heavy small window stops at its 360-line floor");

      --  Shrinking the window while scaled clamps to the new floor at once.
      C := Initial;
      R := Run (C, Slow, Out_W, Out_H, 200);
      Note_Frame (C, 800, 380, 1_000, 100);
      Check (Current (C) <= Floor_For (380), "a resize clamps to the new window's floor");

      --  Hysteresis: 11.8 ms at native drops one step, to 9.6 ms; native is
      --  then predicted at 11.8 ms, over the 8 ms raise mark, so it stays.
      C := Initial;
      R := Run (C, (Render_Ps_Per_Pixel => 18_000, Scale_Ps_Per_Pixel => 0), 1024, 640, 600);
      Check (R.Final = Native + 1 and then R.Changes = 1 and then R.Late_Changes = 0,
             "a frame just over the budget drops one step and stays (the hysteresis band)");
   end Controller_Checks;

   ---------------------------------------------------------------------------
   --  The scaler
   ---------------------------------------------------------------------------
   procedure Scaler_Checks is
      use Neon_Upscale;
      Rows : Pixel_Array (0 .. 2 * 64 - 1) := [others => 0];
      Left, Right : Index_Array (0 .. 63) := [others => 0];
      Weights : Weight_Array (0 .. 63) := [others => 0];
      Sentinel : constant Unsigned_32 := 16#FF12_3456#;
   begin
      --  The same size: an exact copy, and the pitch's tail untouched.
      declare
         W : constant := 37;
         H : constant := 23;
         Pitch : constant := 41;
         Source : Pixel_Array (0 .. W * H - 1);
         Target : Pixel_Array (0 .. Pitch * H - 1) := [others => Sentinel];
         Same, Tail : Boolean := True;
      begin
         for I in Source'Range loop
            Source (I) := (Unsigned_32 (I) * 2_654_435_761) and 16#00FF_FFFF#;
         end loop;
         Scale (Source, W, H, Target, W, H, Pitch, Rows, Left, Right, Weights, Scanlines => False);
         for Y in 0 .. H - 1 loop
            for X in 0 .. W - 1 loop
               Same := Same and then Target (Y * Pitch + X) = Source (Y * W + X);
            end loop;
            for X in W .. Pitch - 1 loop
               Tail := Tail and then Target (Y * Pitch + X) = Sentinel;
            end loop;
         end loop;
         Check (Same, "a frame scaled to its own size is copied exactly");
         Check (Tail, "pixels past the width in a row are left alone");
      end;

      --  Two pixels to four: pixel centres, clamped at the edges.
      declare
         Source : constant Pixel_Array (0 .. 1) := [16#000000#, 16#FF0000#];
         Target : Pixel_Array (0 .. 3) := [others => 0];
      begin
         Scale (Source, 2, 1, Target, 4, 1, 4, Rows, Left, Right, Weights, Scanlines => False);
         Check (Target = [16#000000#, 16#3F0000#, 16#BF0000#, 16#FF0000#],
                "2 -> 4 samples at the pixel centres (0, 1/4, 3/4, 1)");
      end;

      --  A constant frame stays constant at any scale, and the scanlines
      --  darken the odd lines by 230/256.
      declare
         C : constant Unsigned_32 := 16#80C0FF#;
         Source : constant Pixel_Array (0 .. 7 * 5 - 1) := [others => C];
         Target : Pixel_Array (0 .. 64 * 48 - 1) := [others => 0];
         Even, Odd : Boolean := True;
         Dark : constant Unsigned_32 := Shift_Left (16#80# * 230 / 256, 16)
           or Shift_Left (16#C0# * 230 / 256, 8) or 16#FF# * 230 / 256;
      begin
         Scale (Source, 7, 5, Target, 64, 48, 64, Rows, Left, Right, Weights, Scanlines => True);
         for Y in 0 .. 47 loop
            for X in 0 .. 63 loop
               if Y mod 2 = 0 then
                  Even := Even and then Target (Y * 64 + X) = C;
               else
                  Odd := Odd and then Target (Y * 64 + X) = Dark;
               end if;
            end loop;
         end loop;
         Check (Even, "a constant frame scales to the same constant");
         Check (Odd, "the odd lines get the scanline weight");
      end;

      --  A ramp stays a ramp: no row or column out of order.
      declare
         SW : constant := 20;
         SH : constant := 11;
         W : constant := 57;
         H : constant := 31;
         Source : Pixel_Array (0 .. SW * SH - 1);
         Target : Pixel_Array (0 .. W * H - 1) := [others => 0];
         Ordered : Boolean := True;
      begin
         for Y in 0 .. SH - 1 loop
            for X in 0 .. SW - 1 loop
               Source (Y * SW + X) := Shift_Left (Unsigned_32 (X * 12), 16) or Unsigned_32 (Y * 20);
            end loop;
         end loop;
         Scale (Source, SW, SH, Target, W, H, W, Rows, Left, Right, Weights, Scanlines => False);
         for Y in 0 .. H - 1 loop
            for X in 1 .. W - 1 loop
               Ordered := Ordered and then
                 Shift_Right (Target (Y * W + X), 16) >= Shift_Right (Target (Y * W + X - 1), 16);
            end loop;
         end loop;
         for Y in 1 .. H - 1 loop
            for X in 0 .. W - 1 loop
               Ordered := Ordered and then
                 (Target (Y * W + X) and 16#FF#) >= (Target ((Y - 1) * W + X) and 16#FF#);
            end loop;
         end loop;
         Check (Ordered, "a ramp scaled up stays ordered across and down");
         Check (Shift_Right (Target (W - 1), 16) = 19 * 12 and then (Target ((H - 1) * W) and 16#FF#) = 10 * 20,
                "the far edges keep the source's edge values");
      end;
   end Scaler_Checks;

   ---------------------------------------------------------------------------
   --  The software renderer reuses its buffers across sizes
   ---------------------------------------------------------------------------
   procedure Renderer_Checks is
      type Game_Access is access Neon_Game.Game;
      type Frame_Access is access Neon_Scene.Frame;
      type Pixels is array (0 .. 640 * 400 - 1) of Unsigned_32;
      type Pixels_Access is access Pixels;
      G : constant Game_Access := new Neon_Game.Game;
      F : constant Frame_Access := new Neon_Scene.Frame;
      Target : constant Pixels_Access := new Pixels;
      R : Neon_Soft.Renderer;
      Sizes : constant array (1 .. 4) of Positive := [640, 448, 576, 320];
   begin
      Neon_Game.Start (G.all, 7);
      for Round in 1 .. 5 loop
         for W of Sizes loop
            Neon_Scene.Build (F.all, G.all, W, W * 5 / 8);
            Neon_Soft.Render (R, F.all, Target.all'Address, W, W * 5 / 8, W);
         end loop;
      end loop;
      Check (Neon_Soft.Buffer_Growths (R) = 1,
             "the renderer allocates once for its largest size and reuses it (growths"
             & Natural'Image (Neon_Soft.Buffer_Growths (R)) & ")");
   end Renderer_Checks;

begin
   Controller_Checks;
   Scaler_Checks;
   Renderer_Checks;
   Put_Line ("NEON-DYNRES: " & (if Failed then "FAIL" else "PASS"));
   if Failed then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Dynres_Tests;
