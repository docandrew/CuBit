------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
with Neon_Sim; use Neon_Sim;
with Neon_Time; use Neon_Time;
with Neon_Composer;
with Neon_Font;

package body Neon_Scene is
   package C renames Neon_Composer;

   --  The camera: 55 degrees down, high above the near edge, the floor
   --  bending away past the far edge (see the spec).
   Pitch_Turns : constant := 55.0 / 360.0;
   Camera_Height : constant := 22.0;
   Camera_Back : constant := -16.0;
   Floor_Bend : constant := 0.02;
   Bend_Begins : constant := Half_Depth + 0.5;
   --  The arena's near corners at 3% from the sides and the bottom.
   Side_Margin : constant := 0.47;
   Bottom_Row : constant := 0.97;
   Height_Focal_Limit : constant := 1.04;

   Scroll_Speed : constant := 2.4;        --  world units a second
   Kick_Decay : constant := 6.0;          --  1/s
   Swell_Decay : constant := 1.2;
   Shake_Pixels : constant := 18.0;

   function Camera_For (Width, Height : Positive) return Camera is
      Up : constant Vec3 := (0.0, Cos_Turns (Pitch_Turns), Sin_Turns (Pitch_Turns));
      Forward : constant Vec3 := (0.0, -Sin_Turns (Pitch_Turns), Cos_Turns (Pitch_Turns));
      W : constant Float := Float (Width);
      H : constant Float := Float (Height);
      --  The near edge's centre in camera space.
      Near_DY : constant Float := -Camera_Height;
      Near_DZ : constant Float := -Half_Depth - Camera_Back;
      Near_YC : constant Float := Near_DY * Up.Y + Near_DZ * Up.Z;
      Near_ZC : constant Float := Near_DY * Forward.Y + Near_DZ * Forward.Z;
      Focal : constant Float := Min (Side_Margin * W * Near_ZC / Half_Width, Height_Focal_Limit * H);
   begin
      return (Position => (0.0, Camera_Height, Camera_Back), Up => Up, Forward => Forward,
              Focal => Focal, Center_X => W / 2.0,
              Center_Y => Bottom_Row * H + Focal * Near_YC / Near_ZC,
              Width => W, Height => H, Bend_Start => Bend_Begins, Bend => Floor_Bend);
   end Camera_For;

   procedure Project (Cam : Camera; P : Vec3; X, Y, Depth : out Float; Visible : out Boolean) is
      DY : constant Float := P.Y - Cam.Position.Y;
      DZ : constant Float := P.Z - Cam.Position.Z;
      YC : constant Float := DY * Cam.Up.Y + DZ * Cam.Up.Z;
      ZC : constant Float := DY * Cam.Forward.Y + DZ * Cam.Forward.Z;
   begin
      Depth := ZC;
      Visible := ZC > 0.5;
      if Visible then
         X := Cam.Center_X + Cam.Focal * (P.X - Cam.Position.X) / ZC;
         Y := Cam.Center_Y - Cam.Focal * YC / ZC;
      else
         X := 0.0;
         Y := 0.0;
      end if;
   end Project;

   procedure Row_Floor (Cam : Camera; Y : Float; Hit : out Boolean; Z, T : out Float) is
      V : constant Float := (Cam.Center_Y - Y) / Cam.Focal;
      DY : constant Float := Cam.Forward.Y + V * Cam.Up.Y;
      DZ : constant Float := Cam.Forward.Z + V * Cam.Up.Z;
   begin
      Hit := False;
      Z := 0.0;
      T := 0.0;
      if DY >= 0.0 then
         return;
      end if;
      T := -Cam.Position.Y / DY;
      Z := Cam.Position.Z + DZ * T;
      if Z <= Cam.Bend_Start then
         Hit := True;
         return;
      end if;
      declare
         Offset : constant Float := Cam.Position.Z - Cam.Bend_Start;
         A : constant Float := Cam.Bend * DZ * DZ;
         B : constant Float := 2.0 * Cam.Bend * DZ * Offset + DY;
         CC : constant Float := Cam.Bend * Offset * Offset + Cam.Position.Y;
         Disc : constant Float := B * B - 4.0 * A * CC;
      begin
         if A <= 0.0 or else Disc < 0.0 then
            return;
         end if;
         T := (-B - Sqrt (Disc)) / (2.0 * A);
         Z := Cam.Position.Z + DZ * T;
         Hit := T > 0.0 and then Z > Cam.Bend_Start;
      end;
   end Row_Floor;

   function Floor_Point (Cam : Camera; X, Y : Float) return Vec2 is
      U : constant Float := (X - Cam.Center_X) / Cam.Focal;
      V : constant Float := (Cam.Center_Y - Y) / Cam.Focal;
      DY : constant Float := Cam.Forward.Y + V * Cam.Up.Y;
      DZ : constant Float := Cam.Forward.Z + V * Cam.Up.Z;
      T : Float;
   begin
      if DY >= -1.0e-3 then
         return (U * 1000.0, 1000.0);
      end if;
      T := -Cam.Position.Y / DY;
      return (Cam.Position.X + U * T, Cam.Position.Z + DZ * T);
   end Floor_Point;

   ---------------------------------------------------------------------------
   --  Frame building
   ---------------------------------------------------------------------------

   procedure Add_Line (F : in out Frame; A, B : Vec3; Color : RGB; Width : Float := 2.0) is
   begin
      if F.Line_Count < Line_Ceiling then
         F.Line_Count := F.Line_Count + 1;
         F.Lines (F.Line_Count) := (A => A, B => B, Color => Color, Width => Width);
      else
         F.Dropped := F.Dropped + 1;
      end if;
   end Add_Line;

   procedure Add_Stroke (F : in out Frame; A, B : Vec2; Color : RGB; Width : Float) is
   begin
      if F.Stroke_Count < Stroke_Ceiling then
         F.Stroke_Count := F.Stroke_Count + 1;
         F.Strokes (F.Stroke_Count) := (A => A, B => B, Color => Color, Width => Width);
      else
         F.Dropped := F.Dropped + 1;
      end if;
   end Add_Stroke;

   procedure Add_Point (F : in out Frame; P : Vec3; Color : RGB; Size : Float) is
   begin
      if F.Point_Count < Point_Ceiling then
         F.Point_Count := F.Point_Count + 1;
         F.Points (F.Point_Count) := (Position => P, Color => Color, Size => Size);
      else
         F.Dropped := F.Dropped + 1;
      end if;
   end Add_Point;

   procedure Add_Light (F : in out Frame; P : Vec3; Color : RGB; Radius : Float) is
   begin
      if F.Light_Count < Light_Ceiling then
         F.Light_Count := F.Light_Count + 1;
         F.Lights (F.Light_Count) := (Position => P, Color => Color, Radius => Radius);
      else
         F.Dropped := F.Dropped + 1;
      end if;
   end Add_Light;

   procedure Add_Text (F : in out Frame; Text : String; X, Y, Size : Float; Color : RGB;
                       Width : Float := 2.0; Centered : Boolean := False) is
      Unit : constant Float := Size / Float (Neon_Font.Grid_Height);
      Left : constant Float :=
        (if Centered then X - Unit * Float (Neon_Font.Width (Text)) / 2.0 else X);
      Glyph : Neon_Font.Glyph;
      Origin : Float;
   begin
      for I in Text'Range loop
         Glyph := Neon_Font.Shape (Text (I));
         Origin := Left + Unit * Float ((I - Text'First) * Neon_Font.Advance);
         for S in 1 .. Glyph.Count loop
            declare
               Seg : constant Neon_Font.Segment := Glyph.Segments (S);
            begin
               Add_Stroke (F, (Origin + Unit * Float (Seg.X0), Y + Unit * Float (Seg.Y0)),
                           (Origin + Unit * Float (Seg.X1), Y + Unit * Float (Seg.Y1)), Color, Width);
            end;
         end loop;
      end loop;
   end Add_Text;

   function Image (Value : Natural) return String is
      Text : constant String := Natural'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   --  A point rotated about the vertical axis by Turns, then tilted about
   --  X by Tilt turns, placed at Center.
   function Place (Local : Vec3; Turns, Tilt : Float; Center : Vec3) return Vec3 is
      CX : constant Float := Cos_Turns (Turns);
      SX : constant Float := Sin_Turns (Turns);
      R : constant Vec3 := (Local.X * CX - Local.Z * SX, Local.Y, Local.X * SX + Local.Z * CX);
      CT : constant Float := Cos_Turns (Tilt);
      ST : constant Float := Sin_Turns (Tilt);
   begin
      return Center + (R.X, R.Y * CT - R.Z * ST, R.Y * ST + R.Z * CT);
   end Place;

   procedure Add_Circle (F : in out Frame; Center : Vec3; Radius : Float; Color : RGB;
                         Segments : Positive; Width : Float := 2.0) is
      A, B : Vec3;
   begin
      for I in 0 .. Segments - 1 loop
         A := Center + (Radius * Cos_Turns (Float (I) / Float (Segments)), 0.0,
                        Radius * Sin_Turns (Float (I) / Float (Segments)));
         B := Center + (Radius * Cos_Turns (Float (I + 1) / Float (Segments)), 0.0,
                        Radius * Sin_Turns (Float (I + 1) / Float (Segments)));
         Add_Line (F, A, B, Color, Width);
      end loop;
   end Add_Circle;

   procedure Add_Cube (F : in out Frame; Center : Vec3; Half, Turns, Tilt : Float; Color : RGB) is
      type Corners is array (0 .. 7) of Vec3;
      P : Corners;
      type Edge is record
         A, B : Natural;
      end record;
      Edges : constant array (1 .. 12) of Edge :=
        [(0, 1), (1, 3), (3, 2), (2, 0), (4, 5), (5, 7), (7, 6), (6, 4), (0, 4), (1, 5), (2, 6), (3, 7)];
   begin
      for I in P'Range loop
         P (I) := Place (((if I mod 2 = 0 then -Half else Half),
                          (if (I / 2) mod 2 = 0 then -Half else Half),
                          (if I / 4 = 0 then -Half else Half)), Turns, Tilt, Center);
      end loop;
      for E of Edges loop
         Add_Line (F, P (E.A), P (E.B), Color, 2.4);
      end loop;
      --  An inner cube, smaller and fainter: depth in the wireframe.
      for I in P'Range loop
         P (I) := Place (((if I mod 2 = 0 then -Half else Half) * 0.45,
                          (if (I / 2) mod 2 = 0 then -Half else Half) * 0.45,
                          (if I / 4 = 0 then -Half else Half) * 0.45), -Turns, Tilt, Center);
      end loop;
      for E of Edges loop
         Add_Line (F, P (E.A), P (E.B), 0.5 * Color, 1.4);
      end loop;
   end Add_Cube;

   procedure Add_Pyramid (F : in out Frame; Base : Vec3; Size, Turns : Float; Color : RGB) is
      Apex : constant Vec3 := Base + (0.0, 1.6 * Size, 0.0);
      Corner : array (0 .. 3) of Vec3;
   begin
      for I in Corner'Range loop
         Corner (I) := Place ((Size * Cos_Turns (Float (I) / 4.0), 0.1, Size * Sin_Turns (Float (I) / 4.0)),
                              Turns, 0.0, Base);
      end loop;
      for I in Corner'Range loop
         Add_Line (F, Corner (I), Corner ((I + 1) mod 4), Color, 2.4);
         Add_Line (F, Corner (I), Apex, Color, 2.4);
      end loop;
   end Add_Pyramid;

   procedure Add_Torus (F : in out Frame; Center : Vec3; Turns : Float; Color : RGB) is
      Major : constant := 1.1;
      Minor : constant := 0.36;
      Around : constant := 16;
      Rings : constant := 8;
      Tilt : constant := 0.06;
      function Surface (U, V : Float) return Vec3 is
        (Place (((Major + Minor * Cos_Turns (V)) * Cos_Turns (U), Minor * Sin_Turns (V),
                 (Major + Minor * Cos_Turns (V)) * Sin_Turns (U)), Turns, Tilt, Center));
   begin
      for Band in 0 .. 3 loop
         for I in 0 .. Around - 1 loop
            Add_Line (F, Surface (Float (I) / Float (Around), Float (Band) / 4.0),
                      Surface (Float (I + 1) / Float (Around), Float (Band) / 4.0),
                      (if Band = 0 then Color else 0.6 * Color), (if Band = 0 then 2.4 else 1.6));
         end loop;
      end loop;
      for R in 0 .. Rings - 1 loop
         for J in 0 .. 5 loop
            Add_Line (F, Surface (Float (R) / Float (Rings), Float (J) / 6.0),
                      Surface (Float (R) / Float (Rings), Float (J + 1) / 6.0), 0.7 * Color, 1.6);
         end loop;
      end loop;
   end Add_Torus;

   procedure Add_Octahedron (F : in out Frame; Center : Vec3; Size, Turns : Float; Color : RGB) is
      Top : constant Vec3 := Place ((0.0, 1.4 * Size, 0.0), Turns, 0.0, Center);
      Bottom : constant Vec3 := Place ((0.0, -1.4 * Size, 0.0), Turns, 0.0, Center);
      Ring : array (0 .. 3) of Vec3;
   begin
      for I in Ring'Range loop
         Ring (I) := Place ((Size * Cos_Turns (Float (I) / 4.0), 0.0, Size * Sin_Turns (Float (I) / 4.0)),
                            Turns, 0.08, Center);
      end loop;
      for I in Ring'Range loop
         Add_Line (F, Ring (I), Ring ((I + 1) mod 4), Color, 2.2);
         Add_Line (F, Ring (I), Top, Color, 2.2);
         Add_Line (F, Ring (I), Bottom, Color, 2.2);
      end loop;
   end Add_Octahedron;

   procedure Add_Ship (F : in out Frame; Position, Facing : Vec2; Color : RGB; Scale : Float := 1.0) is
      Right : constant Vec2 := (Facing.Y, -Facing.X);
      function At_Local (Side, Ahead, Up : Float) return Vec3 is
        ((Position.X + Scale * (Side * Right.X + Ahead * Facing.X), Scale * Up + 0.15,
          Position.Y + Scale * (Side * Right.Y + Ahead * Facing.Y)));
      Nose : constant Vec3 := At_Local (0.0, 1.0, 0.3);
      Left : constant Vec3 := At_Local (-0.75, -0.6, 0.2);
      Rite : constant Vec3 := At_Local (0.75, -0.6, 0.2);
      Notch : constant Vec3 := At_Local (0.0, -0.25, 0.25);
      Top : constant Vec3 := At_Local (0.0, -0.35, 0.65);
   begin
      Add_Line (F, Nose, Left, Color, 2.6);
      Add_Line (F, Nose, Rite, Color, 2.6);
      Add_Line (F, Left, Notch, Color, 2.6);
      Add_Line (F, Rite, Notch, Color, 2.6);
      Add_Line (F, Nose, Top, White, 1.8);
      Add_Line (F, Left, Top, 0.7 * Color, 1.6);
      Add_Line (F, Rite, Top, 0.7 * Color, 1.6);
      Add_Line (F, Notch, Top, 0.7 * Color, 1.6);
   end Add_Ship;

   procedure Add_Arena (F : in out Frame; Pulse : Float) is
      Edge : constant RGB := (0.6 + 0.6 * Pulse) * Cyan;
      Lift : constant := 0.05;
      Corners : constant array (0 .. 3) of Vec3 :=
        [(-Half_Width, Lift, -Half_Depth), (Half_Width, Lift, -Half_Depth),
         (Half_Width, Lift, Half_Depth), (-Half_Width, Lift, Half_Depth)];
   begin
      for I in Corners'Range loop
         Add_Line (F, Corners (I), Corners ((I + 1) mod 4), Edge, 2.2);
      end loop;
      --  Posts at the corners, a little height to frame the floor.
      for I in Corners'Range loop
         Add_Line (F, Corners (I), Corners (I) + (0.0, 1.2, 0.0), Hot_Pink, 2.2);
      end loop;
   end Add_Arena;

   procedure Build_HUD (F : in out Frame; G : Neon_Game.Game; Current : C.Step_Record;
                        Beat_Phase : Float) is
      W : World renames G.Sim;
      Aspect : constant Float := Float (F.Width) / Float (F.Height);
      Pulse : constant Float := F.Kick;
      Margin : constant := 0.03;
   begin
      if W.Demo and then W.Phase in Playing | Dying | Game_Over
        and then Fraction (F.Seconds * 0.5) < 0.75
      then
         Add_Text (F, "DEMO   CLICK TO PLAY", Aspect / 2.0, 0.93, 0.024, 0.8 * Hot_Pink, 1.6,
                   Centered => True);
      end if;
      case W.Phase is
         when Playing | Dying =>
            Add_Text (F, "SCORE", Margin, Margin, 0.022, 0.8 * Cyan, 1.6);
            Add_Text (F, Image (W.Score), Margin, Margin + 0.032, 0.045, White, 2.6);
            if W.Multiplier > 1 then
               Add_Text (F, "x" & Image (W.Multiplier), Margin, Margin + 0.09, 0.04,
                         Mix (Gold, White, 0.5 * Pulse), 2.4);
               --  The combo timer as a shrinking bar.
               Add_Stroke (F, (Margin, Margin + 0.145),
                           (Margin + 0.18 * W.Combo_Timer / 1.6, Margin + 0.145), Gold, 3.0);
            end if;
            declare
               Label : constant String := "WAVE " & Image (W.Wave);
               Size : constant := 0.03;
            begin
               --  Right-aligned: shift left by the text's width.
               Add_Text (F, Label, Aspect - Margin
                         - Size / Float (Neon_Font.Grid_Height) * Float (Neon_Font.Width (Label)),
                         Margin, Size, Violet, 2.0);
            end;
            for Life in 1 .. W.Player.Lives loop
               declare
                  X : constant Float := Aspect - Margin - 0.04 * Float (Life);
                  Y : constant Float := Margin + 0.07;
               begin
                  Add_Stroke (F, (X, Y + 0.025), (X + 0.012, Y), Cyan, 2.2);
                  Add_Stroke (F, (X + 0.012, Y), (X + 0.024, Y + 0.025), Cyan, 2.2);
                  Add_Stroke (F, (X, Y + 0.025), (X + 0.024, Y + 0.025), Cyan, 2.2);
               end;
            end loop;
            --  The beat indicator: four lamps, the current beat lit.
            for B in 0 .. Neon_Time.Beats_Per_Bar - 1 loop
               declare
                  X : constant Float := Aspect / 2.0 + 0.05 * (Float (B) - 1.5);
                  Lit : constant Boolean := C.Beat_In_Bar (Current) = B;
                  Glow : constant Float := (if Lit then 1.0 - 0.6 * Beat_Phase else 0.25);
                  Color : constant RGB := (if B = 0 then Hot_Pink else Cyan);
               begin
                  Add_Stroke (F, (X - 0.012, Margin + 0.01), (X + 0.012, Margin + 0.01), Glow * Color, 4.0);
                  Add_Stroke (F, (X - 0.012, Margin + 0.022), (X + 0.012, Margin + 0.022),
                              Glow * Color, 4.0);
               end;
            end loop;
            if W.Rhythm_Streak > 0 then
               Add_Text (F, "RHYTHM " & Image (W.Rhythm_Streak), Aspect / 2.0, Margin + 0.045, 0.02,
                         (if W.Rhythm_Streak >= 8 then Gold elsif W.Rhythm_Streak >= 4 then Lime
                          else Cyan), 1.6, Centered => True);
            end if;
            if W.Last_Kill_On_Beat > 0.0 then
               Add_Text (F, "ON BEAT", Aspect / 2.0, 0.86, 0.03 + 0.02 * W.Last_Kill_On_Beat,
                         W.Last_Kill_On_Beat * 1.6 * Gold, 2.2, Centered => True);
            end if;
            if W.Banner > 0.0 then
               declare
                  Fade : constant Float := Saturate (W.Banner);
               begin
                  if W.Clear_Bars > 0 then
                     Add_Text (F, "WAVE CLEAR", Aspect / 2.0, 0.42, 0.07, Fade * Gold, 3.0,
                               Centered => True);
                     Add_Text (F, "+" & Image (1000 * W.Wave), Aspect / 2.0, 0.52, 0.035,
                               Fade * White, 2.0, Centered => True);
                  else
                     Add_Text (F, "WAVE " & Image (W.Wave), Aspect / 2.0, 0.42, 0.08,
                               Fade * Mix (Hot_Pink, White, 0.3 * Pulse), 3.4, Centered => True);
                  end if;
               end;
            end if;
         when Title =>
            declare
               Bob : constant Float := 0.008 * Sin_Turns (F.Seconds * 0.25);
            begin
               Add_Text (F, "NEON", Aspect / 2.0, 0.08 + Bob, 0.11, Mix (Hot_Pink, White, 0.25 * Pulse),
                         5.0, Centered => True);
               Add_Text (F, "BLAST", Aspect / 2.0, 0.215 + Bob, 0.11, Mix (Cyan, White, 0.25 * Pulse),
                         5.0, Centered => True);
            end;
            if Fraction (F.Seconds * 0.5) < 0.7 then
               Add_Text (F, "CLICK OR PRESS ENTER", Aspect / 2.0, 0.62, 0.032, Gold, 2.2,
                         Centered => True);
            end if;
            Add_Text (F, "WASD MOVE   MOUSE AIM   CLICK FIRE   SPACE DASH", Aspect / 2.0, 0.7, 0.02,
                      0.8 * Cyan, 1.5, Centered => True);
            Add_Text (F, "C CALIBRATE   F3 OVERLAY   F9 BENCHMARK   ESC QUIT", Aspect / 2.0, 0.74, 0.02,
                      0.7 * Violet, 1.5, Centered => True);
            if W.Scores (1).Score > 0 then
               Add_Text (F, "HIGH SCORES", Aspect / 2.0, 0.8, 0.022, Hot_Pink, 1.6, Centered => True);
               for I in 1 .. 3 loop
                  exit when W.Scores (I).Score = 0;
                  Add_Text (F, Image (I) & ". " & Image (W.Scores (I).Score) & "  WAVE "
                            & Image (W.Scores (I).Wave), Aspect / 2.0, 0.8 + 0.035 * Float (I),
                            0.02, White, 1.4, Centered => True);
               end loop;
            end if;
         when Game_Over =>
            Add_Text (F, "GAME OVER", Aspect / 2.0, 0.12, 0.09, Mix (Hot_Pink, White, 0.3 * Pulse),
                      4.0, Centered => True);
            Add_Text (F, Image (W.Score), Aspect / 2.0, 0.26, 0.06, White, 3.0, Centered => True);
            Add_Text (F, "WAVE " & Image (W.Wave) & "   BEST RHYTHM " & Image (W.Best_Streak)
                      & "   ON BEAT " & Image (W.On_Beat_Kills), Aspect / 2.0, 0.35, 0.022, Cyan, 1.6,
                      Centered => True);
            if W.New_High_Rank > 0 then
               Add_Text (F, "NEW HIGH SCORE", Aspect / 2.0, 0.40, 0.03, Gold, 2.0, Centered => True);
            end if;
            for I in W.Scores'Range loop
               exit when W.Scores (I).Score = 0;
               Add_Text (F, Image (I) & ". " & Image (W.Scores (I).Score) & "  WAVE "
                         & Image (W.Scores (I).Wave), Aspect / 2.0, 0.46 + 0.04 * Float (I), 0.024,
                         (if I = W.New_High_Rank then Gold else White), 1.6, Centered => True);
            end loop;
            if W.Phase_Time > 1.0 and then Fraction (F.Seconds * 0.5) < 0.7 then
               Add_Text (F, "CLICK TO PLAY AGAIN", Aspect / 2.0, 0.84, 0.03, Hot_Pink, 2.0,
                         Centered => True);
            end if;
         when Calibrating =>
            Add_Text (F, "CALIBRATE", Aspect / 2.0, 0.1, 0.07, Cyan, 3.0, Centered => True);
            Add_Text (F, "TAP SPACE ON EVERY BEAT YOU HEAR", Aspect / 2.0, 0.22, 0.028, White, 1.8,
                      Centered => True);
            Add_Text (F, Image (W.Tap_Count) & " / " & Image (Calibration_Taps), Aspect / 2.0, 0.3,
                      0.04, Gold, 2.2, Centered => True);
            Add_Text (F, "C TO CANCEL", Aspect / 2.0, 0.88, 0.02, Violet, 1.4, Centered => True);
            --  The beat ring.
            for B in 0 .. 31 loop
               declare
                  R : constant Float := 0.08 + 0.05 * (1.0 - Beat_Phase);
                  A : constant Float := Float (B) / 32.0;
                  Next : constant Float := Float (B + 1) / 32.0;
               begin
                  Add_Stroke (F, (Aspect / 2.0 + R * Cos_Turns (A), 0.55 + R * Sin_Turns (A)),
                              (Aspect / 2.0 + R * Cos_Turns (Next), 0.55 + R * Sin_Turns (Next)),
                              (1.0 - 0.7 * Beat_Phase) * Hot_Pink, 3.0);
               end;
            end loop;
      end case;
      if W.Phase = Title and then W.Calibration_Offset_Ms /= 0.0 then
         Add_Text (F, "CALIBRATION " & (if W.Calibration_Offset_Ms < 0.0 then "-" else "+")
                   & Image (Natural (abs W.Calibration_Offset_Ms)) & " MS", Aspect / 2.0, 0.95, 0.018,
                   0.6 * Cyan, 1.3, Centered => True);
      end if;
   end Build_HUD;

   procedure Build (F : in out Frame; G : Neon_Game.Game; Width, Height : Positive) is
      W : World renames G.Sim;
      View : constant Samples := W.Clock;
      Current : C.Step_Record;
      Found : Boolean;
      Kick_At, Chord_At : Samples;
      Beat_Phase : Float := 0.0;
   begin
      F.Width := Width;
      F.Height := Height;
      F.Cam := Camera_For (Width, Height);
      F.Line_Count := 0;
      F.Stroke_Count := 0;
      F.Point_Count := 0;
      F.Light_Count := 0;
      F.Ripple_Count := 0;
      F.Dropped := 0;
      F.Seconds := Neon_Time.Seconds (Sample_Offset (View));
      F.Scroll := Fraction (F.Seconds * Scroll_Speed / (2.0 * Grid_Cell)) * 2.0 * Grid_Cell;

      C.Find (G.Composer, View, Current, Found);
      if Found then
         Beat_Phase := Saturate
           ((Float (View - Current.Start) + Float (Current.Step mod Steps_Per_Beat * Current.Length))
            / Float (Steps_Per_Beat * Current.Length));
      end if;
      C.Last_Kick (G.Composer, View, Kick_At, Found);
      F.Kick := (if Found then Exp (-Kick_Decay * Neon_Time.Seconds (Sample_Offset (View - Kick_At)))
                 else 0.0);
      C.Last_Chord_Change (G.Composer, View, Chord_At, Found);
      F.Sun_Swell := (if Found then Exp (-Swell_Decay * Neon_Time.Seconds (Sample_Offset (View - Chord_At)))
                      else 0.0);
      F.Glow := 0.5 + 0.4 * Current.Pad_Level + 0.3 * F.Kick;
      F.Underwater := (if Current.Section in C.Breakdown | C.Finale then 1.0 - Current.Cutoff else 0.0);
      F.Flash := W.Screen_Flash;
      F.Hurt := W.Hurt_Flash;
      F.Shake := (Shake_Pixels * W.Shake * Sin_Turns (F.Seconds * 13.0),
                  Shake_Pixels * W.Shake * Cos_Turns (F.Seconds * 11.0));

      Add_Arena (F, F.Kick);

      --  Ripples.
      for R of W.Ripples loop
         if R.Age < 1.4 and then F.Ripple_Count < Ripple_Ceiling then
            F.Ripple_Count := F.Ripple_Count + 1;
            F.Ripples (F.Ripple_Count) :=
              (Center => R.Center, Radius => 0.3 + 9.0 * R.Age, Width => 0.6 + 0.8 * R.Age,
               Strength => R.Strength * (1.0 - R.Age / 1.4), Color => R.Color);
         end if;
      end loop;

      --  Flashes: lights that bloom on their sixteenth.
      for Fl of W.Flashes loop
         if Fl.Duration > 0.0 and then View >= Fl.At_Time then
            declare
               Age : constant Float := Neon_Time.Seconds (Sample_Offset (View - Fl.At_Time));
               Fade : constant Float := Saturate (1.0 - Age / Fl.Duration);
            begin
               Add_Light (F, Fl.Position, (4.0 * Fade) * Fl.Color, 5.0);
               Add_Point (F, Fl.Position, (2.0 * Fade) * Mix (Fl.Color, White, 0.5), 0.5 * Fade + 0.1);
            end;
         end if;
      end loop;

      --  Shots: streaks, each a moving light.
      for S of W.Shots loop
         if S.Alive then
            Add_Line (F, (S.Position.X - 0.035 * S.Velocity.X, 0.5, S.Position.Y - 0.035 * S.Velocity.Y),
                      (S.Position.X, 0.5, S.Position.Y), Mix (S.Hue, White, 0.4), 3.2);
            Add_Light (F, (S.Position.X, 0.5, S.Position.Y), 1.6 * S.Hue, 3.2);
         end if;
      end loop;
      for O of W.Orbs loop
         if O.Alive then
            Add_Point (F, (O.Position.X, 0.45, O.Position.Y), 1.4 * Hot_Pink, 0.32);
            Add_Point (F, (O.Position.X, 0.45, O.Position.Y), White, 0.12);
         end if;
      end loop;

      --  Enemies.
      for E of W.Enemies loop
         if E.Alive then
            declare
               Base : constant Vec3 := (E.Position.X, E.Height, E.Position.Y);
               Color : constant RGB := Mix (Enemy_Color (E.Kind), White, E.Hit_Flash);
            begin
               if E.Spawn_Left > 0.0 then
                  declare
                     Grow : constant Float := 1.0 - E.Spawn_Left / 0.6;
                  begin
                     Add_Circle (F, (E.Position.X, 0.05, E.Position.Y),
                                 Enemy_Radius (E.Kind) * (2.5 - 1.5 * Grow), (0.4 + Grow) * Color, 20);
                     Add_Line (F, (E.Position.X, 0.0, E.Position.Y),
                               (E.Position.X, 6.0 * (1.0 - Grow), E.Position.Y), Grow * Color, 2.0);
                  end;
               else
                  case E.Kind is
                     when Cube =>
                        Add_Cube (F, Base + (0.0, 0.8, 0.0), 0.75, E.Spin, 0.05, Color);
                     when Pyramid =>
                        Add_Pyramid (F, Base, 0.85, E.Spin, Color);
                     when Torus =>
                        Add_Torus (F, Base + (0.0, 0.6, 0.0), E.Spin, Color);
                     when Shard =>
                        Add_Octahedron (F, Base + (0.0, 1.1, 0.0), 0.65, E.Spin, Color);
                     when Splinter =>
                        Add_Octahedron (F, Base + (0.0, 0.8, 0.0), 0.42, E.Spin, Color);
                  end case;
                  --  A soft shadow-light under each enemy.
                  Add_Light (F, (E.Position.X, 0.6, E.Position.Y), 0.5 * Enemy_Color (E.Kind), 2.2);
               end if;
            end;
         end if;
      end loop;

      --  The player, its trail and the aim reticle.
      if W.Phase in Playing | Dying then
         declare
            P : Player_State renames W.Player;
            Blink : constant Boolean :=
              P.Invulnerable > 0.0 and then Fraction (P.Invulnerable * 8.0) < 0.5 and then P.Dash_Left = 0.0;
         begin
            if W.Phase = Playing then
               if not Blink then
                  Add_Ship (F, P.Position, P.Facing, Mix (Cyan, White, 0.2));
               end if;
               Add_Light (F, (P.Position.X, 0.7, P.Position.Y), 0.9 * Cyan, 3.0);
               for I in 1 .. Trail_Length - 1 loop
                  declare
                     A : constant Vec2 := P.Trail ((P.Trail_Head - I + 1 + Trail_Length) mod Trail_Length);
                     B : constant Vec2 := P.Trail ((P.Trail_Head - I + Trail_Length) mod Trail_Length);
                     Fade : constant Float := 1.0 - Float (I) / Float (Trail_Length);
                  begin
                     if Length (A - B) < 3.0 then
                        Add_Line (F, (A.X, 0.2, A.Y), (B.X, 0.2, B.Y),
                                  Fade * Mix (Hot_Pink, Cyan, Fade), 2.0 * Fade + 0.5);
                     end if;
                  end;
               end loop;
            end if;
         end;
      end if;

      --  Title: wireframe shapes turning slowly over the floor.
      if W.Phase in Title | Game_Over | Calibrating then
         Add_Cube (F, (-9.0, 1.6 + 0.3 * Sin_Turns (F.Seconds * 0.2), 2.0), 1.0, F.Seconds * 0.07, 0.08,
                   (0.8 + 0.4 * F.Kick) * Cyan);
         Add_Torus (F, (9.0, 1.4 + 0.3 * Sin_Turns (F.Seconds * 0.17 + 0.5), 2.5), F.Seconds * 0.05,
                    (0.8 + 0.4 * F.Kick) * Hot_Pink);
         Add_Pyramid (F, (0.0, 0.2, 5.5), 1.2, F.Seconds * 0.04, (0.7 + 0.4 * F.Kick) * Gold);
         Add_Light (F, (-9.0, 1.0, 2.0), 0.8 * Cyan, 5.0);
         Add_Light (F, (9.0, 1.0, 2.5), 0.8 * Hot_Pink, 5.0);
      end if;

      --  Particles (alive ones), newest last.
      for P of W.Particles loop
         if P.Life > 0.0 then
            Add_Point (F, P.Position, (1.5 * P.Life / P.Lifetime) * P.Color,
                       P.Size * (0.4 + 0.6 * P.Life / P.Lifetime));
         end if;
      end loop;

      Build_HUD (F, G, Current, Beat_Phase);
   end Build;

end Neon_Scene;
