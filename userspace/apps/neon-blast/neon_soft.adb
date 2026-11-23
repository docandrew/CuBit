------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
with Neon_Math; use Neon_Math;
with Neon_Sim; use Neon_Sim;

package body Neon_Soft is
   use Neon_Scene;

   type Pixel_Array is array (Natural range <>) of Unsigned_32;

   --  The floor's light map: world X -20 .. 20 and Z -12 .. 15 in half units.
   Map_Cell : constant := 0.5;
   Map_X0 : constant := -20.0;
   Map_Z0 : constant := -12.0;
   Map_Columns : constant := 80;
   Map_Rows : constant := 54;

   Glow_Scale : constant := 4;           --  glow buffer: a quarter of the frame each way
   Reference_Height : constant := 1080.0;
   Channel_Max : constant := 255;
   Scanline_Dim : constant := 0.9;
   Vignette_Strength : constant := 0.35;

   procedure Free_Unused is null;

   procedure Ensure (R : in out Renderer; Width, Height : Positive) is
      GW : constant Positive := Width / Glow_Scale + 2;
      GH : constant Positive := Height / Glow_Scale + 2;
   begin
      if R.Glow_R = null or else R.Glow_Width /= GW or else R.Glow_Height /= GH then
         --  Grow (or reshape) from the frame's size; old planes are left to
         --  the allocator's process heap (a resize is rare).
         R.Glow_Width := GW;
         R.Glow_Height := GH;
         R.Glow_R := new Float_Plane (0 .. GW * GH - 1);
         R.Glow_G := new Float_Plane (0 .. GW * GH - 1);
         R.Glow_B := new Float_Plane (0 .. GW * GH - 1);
         R.Scratch := new Float_Plane (0 .. Natural'Max (GW, GH) * 3);
         R.Row_Max := new Float_Plane (0 .. GH - 1);
      end if;
      if R.Light_R = null then
         R.Light_R := new Light_Plane (0 .. Map_Columns * Map_Rows - 1);
         R.Light_G := new Light_Plane (0 .. Map_Columns * Map_Rows - 1);
         R.Light_B := new Light_Plane (0 .. Map_Columns * Map_Rows - 1);
      end if;
   end Ensure;

   function Ceiling_Of (X : Float) return Float is (-Floor (-X));

   function Channel (Value : Float) return Integer is
     (if Value <= 0.0 then 0 elsif Value >= 1.0 then Channel_Max
      else Integer (Value * Float (Channel_Max)));

   function Pack (C : RGB) return Unsigned_32 is
     (Shift_Left (Unsigned_32 (Channel (C.R)), 16) or Shift_Left (Unsigned_32 (Channel (C.G)), 8)
      or Unsigned_32 (Channel (C.B)));

   --  A small integer hash for the stars.
   function Hash (X, Y : Natural) return Unsigned_32 is
      H : Unsigned_32 := Unsigned_32 (X mod 65536) * 16#9E37_79B1# xor Unsigned_32 (Y mod 65536) * 16#85EB_CA77#;
   begin
      H := H xor Shift_Right (H, 15);
      H := H * 16#C2B2_AE3D#;
      return H xor Shift_Right (H, 13);
   end Hash;

   --  The ridge line of the mountains at view fraction U, as a fraction of
   --  the view height; zero in the middle, where the sun sets.
   function Ridge (U : Float) return Float is
      Side : constant Float := Smooth_Step (0.1, 0.33, abs (U - 0.5));
      Shape : constant Float :=
        0.55 + 0.3 * Sin_Turns (U * 1.3 + 0.05) + 0.18 * Sin_Turns (U * 3.1 + 0.2)
        + 0.12 * abs Sin_Turns (U * 7.3) + 0.05 * abs Sin_Turns (U * 19.0);
   begin
      return 0.07 * Side * Max (0.0, Shape);
   end Ridge;

   --  A grid line's brightness at distance D (cells) from it, with Pixel
   --  cells per pixel: a sharp core, a soft glow, and the average once lines
   --  get closer than a few pixels (no moire toward the horizon).
   function Line_Profile (D, Pixel : Float) return Float is
      Pixels : constant Float := D / Max (Pixel, 1.0e-5);
      Core : constant Float := Saturate (1.3 - 0.85 * Pixels);
      Fade : constant Float := Saturate (1.0 - Pixels * 0.16);
      Halo : constant Float := 0.3 * Fade * Fade;
      Near : constant Float := Core + Halo;
      Average : constant Float := Min (1.0, 2.2 * Pixel + 0.25);
      Blend : constant Float := Saturate ((Pixel - 0.12) * 4.0);
   begin
      return Near + (Average - Near) * Blend;
   end Line_Profile;

   function Cell_Distance (G : Float) return Float is (abs (G - Nearest (G)));

   procedure Add_Pixel (Pixels : in out Pixel_Array; Index : Natural; C : RGB; Weight : Float) is
      P : constant Unsigned_32 := Pixels (Index);
      R : constant Integer := Integer (Shift_Right (P, 16) and 16#FF#) + Channel (C.R * Weight);
      G : constant Integer := Integer (Shift_Right (P, 8) and 16#FF#) + Channel (C.G * Weight);
      B : constant Integer := Integer (P and 16#FF#) + Channel (C.B * Weight);
   begin
      Pixels (Index) := Shift_Left (Unsigned_32 (Integer'Min (R, Channel_Max)), 16)
        or Shift_Left (Unsigned_32 (Integer'Min (G, Channel_Max)), 8)
        or Unsigned_32 (Integer'Min (B, Channel_Max));
   end Add_Pixel;

   ---------------------------------------------------------------------------
   --  The light map
   ---------------------------------------------------------------------------

   procedure Splat_Lights (R : in out Renderer; F : Frame) is
      LR : Light_Plane renames R.Light_R.all;
      LG : Light_Plane renames R.Light_G.all;
      LB : Light_Plane renames R.Light_B.all;
   begin
      LR := [others => 0.0];
      LG := [others => 0.0];
      LB := [others => 0.0];
      for I in 1 .. F.Light_Count loop
         declare
            L : constant Light := F.Lights (I);
            Reach : constant Float := L.Radius;
            C0 : constant Integer := Integer (Floor ((L.Position.X - Reach - Map_X0) / Map_Cell));
            C1 : constant Integer := Integer (Ceiling_Of ((L.Position.X + Reach - Map_X0) / Map_Cell));
            R0 : constant Integer := Integer (Floor ((L.Position.Z - Reach - Map_Z0) / Map_Cell));
            R1 : constant Integer := Integer (Ceiling_Of ((L.Position.Z + Reach - Map_Z0) / Map_Cell));
            Lift : constant Float := 0.3 * L.Position.Y * L.Position.Y;
         begin
            for Row in Integer'Max (0, R0) .. Integer'Min (Map_Rows - 1, R1) loop
               for Col in Integer'Max (0, C0) .. Integer'Min (Map_Columns - 1, C1) loop
                  declare
                     X : constant Float := Map_X0 + Map_Cell * Float (Col) - L.Position.X;
                     Z : constant Float := Map_Z0 + Map_Cell * Float (Row) - L.Position.Z;
                     Q : constant Float := (X * X + Z * Z + Lift) / (Reach * Reach);
                  begin
                     if Q < 1.0 then
                        declare
                           Fall : constant Float := (1.0 - Q) * (1.0 - Q);
                           Index : constant Natural := Row * Map_Columns + Col;
                        begin
                           LR (Index) := LR (Index) + Fall * L.Color.R;
                           LG (Index) := LG (Index) + Fall * L.Color.G;
                           LB (Index) := LB (Index) + Fall * L.Color.B;
                        end;
                     end if;
                  end;
               end loop;
            end loop;
         end;
      end loop;
      for I in 1 .. F.Ripple_Count loop
         declare
            P : constant Floor_Ripple := F.Ripples (I);
            Outer : constant Float := P.Radius + P.Width;
            C0 : constant Integer := Integer (Floor ((P.Center.X - Outer - Map_X0) / Map_Cell));
            C1 : constant Integer := Integer (Ceiling_Of ((P.Center.X + Outer - Map_X0) / Map_Cell));
            R0 : constant Integer := Integer (Floor ((P.Center.Y - Outer - Map_Z0) / Map_Cell));
            R1 : constant Integer := Integer (Ceiling_Of ((P.Center.Y + Outer - Map_Z0) / Map_Cell));
         begin
            for Row in Integer'Max (0, R0) .. Integer'Min (Map_Rows - 1, R1) loop
               for Col in Integer'Max (0, C0) .. Integer'Min (Map_Columns - 1, C1) loop
                  declare
                     X : constant Float := Map_X0 + Map_Cell * Float (Col) - P.Center.X;
                     Z : constant Float := Map_Z0 + Map_Cell * Float (Row) - P.Center.Y;
                     Ring : constant Float := Saturate (1.0 - abs (Sqrt (X * X + Z * Z) - P.Radius) / P.Width);
                     Amount : constant Float := 1.6 * P.Strength * Ring * Ring;
                     Index : constant Natural := Row * Map_Columns + Col;
                  begin
                     LR (Index) := LR (Index) + Amount * P.Color.R;
                     LG (Index) := LG (Index) + Amount * P.Color.G;
                     LB (Index) := LB (Index) + Amount * P.Color.B;
                  end;
               end loop;
            end loop;
         end;
      end loop;
   end Splat_Lights;

   ---------------------------------------------------------------------------
   --  Background: sky, sun, mountains, floor
   ---------------------------------------------------------------------------

   procedure Background (R : in out Renderer; F : Frame; Cam : Camera; Pixels : in out Pixel_Array;
                         Width, Height, Pitch : Positive) is
      H : constant Float := Float (Height);
      Horizon : Natural := Height;
      Hit : Boolean;
      Z, T, Previous_Z : Float;
      Sun_R : constant Float := Sun_Radius * H * (1.0 + 0.05 * F.Sun_Swell);
      Sun_X : Float := Cam.Center_X;
      Sun_Y : Float;
      Haze : constant RGB := Mix (Sky_Low, Horizon_Glow, 0.55);
      Grid_Gain : constant Float := 0.55 + 0.75 * F.Kick;
      Ridges : array (0 .. Width - 1) of Float;
      LR : Light_Plane renames R.Light_R.all;
      LG : Light_Plane renames R.Light_G.all;
      LB : Light_Plane renames R.Light_B.all;
      Star_Time : constant Float := F.Seconds;
      --  Light map rows with any light (either of a pair, for bilinear).
      Row_Lit : array (0 .. Map_Rows - 1) of Boolean := [others => False];
      --  One floor row: a base colour, the grid lines across the row
      --  splatted where they fall, then the lights over the lit span only.
      procedure Floor_Row (Y : Natural; Z, T, Last_Z : Float) is
         Row_Base : constant Natural := Y * Pitch;
         Pixel_World : constant Float := T / Cam.Focal;
         Pixel_Cells : constant Float := Pixel_World / Grid_Cell;
         Row_Cells : constant Float := Max (abs (Last_Z - Z), 1.0e-4) / Grid_Cell;
         Across_Z : constant Float :=
           Line_Profile (Cell_Distance ((Z + F.Scroll) / Grid_Cell), Row_Cells);
         Inside_Z : constant Boolean := abs Z <= Neon_Sim.Half_Depth;
         Fog : constant Float := Smooth_Step (Neon_Sim.Half_Depth + 2.0, 80.0, Z);
         Keep : constant Float := 1.0 - 0.85 * Fog;
         Fogged : constant RGB := (0.85 * Fog) * Haze;
         Map_Row_F : constant Float := (Z - Map_Z0) / Map_Cell;
         Map_Row : constant Integer := (if abs Map_Row_F < 1.0e6 then Floor_Int (Map_Row_F) else -1);
         Row_Frac : constant Float := Map_Row_F - Float (Map_Row);
         Lit : constant Boolean := Map_Row >= 0 and then Map_Row < Map_Rows - 1 and then Row_Lit (Map_Row);
         Start_X : constant Float := Cam.Position.X + (0.5 - Cam.Center_X) * Pixel_World;
         --  The share of a line's brightness that is spread evenly (far
         --  rows) and the share that stays a line.
         Blend : constant Float := Saturate ((Pixel_Cells - 0.12) * 4.0);
         Even : constant Float := Blend * Min (1.0, 2.2 * Pixel_Cells + 0.25);
         Arena_Left, Arena_Right : Integer;
         Inner_Gain : constant Float := Keep * Grid_Gain;
         Outer_Gain : constant Float := 0.5 * Keep * Grid_Gain;
         function Column_Of (World_X : Float) return Float is ((World_X - Start_X) / Pixel_World);
      begin
         if Inside_Z then
            Arena_Left := Integer (Clamp (Column_Of (-Neon_Sim.Half_Width) + 0.5, -1.0, Float (Width)));
            Arena_Right := Integer (Clamp (Column_Of (Neon_Sim.Half_Width) - 0.5, -1.0, Float (Width)));
         else
            Arena_Left := Width;
            Arena_Right := -1;
         end if;
         declare
            Base : constant Float := Across_Z + Even;
            Outside : constant Unsigned_32 :=
              Pack (Keep * Floor_Base + (Base * Outer_Gain) * Grid_Violet + Fogged);
            Inside : constant Unsigned_32 :=
              Pack (Keep * Floor_Base + (Base * Inner_Gain) * Grid_Pink + Fogged);
         begin
            for X in 0 .. Width - 1 loop
               Pixels (Row_Base + X) := (if X >= Arena_Left and then X <= Arena_Right then Inside else Outside);
            end loop;
         end;
         --  The lines along Z, where they cross this row.
         if Blend < 1.0 then
            declare
               Reach : constant Float := 1.53 / 0.16 * Pixel_Cells + Pixel_Cells;   --  cells
               First : constant Integer := Floor_Int ((Start_X / Grid_Cell) - Reach) ;
               Last : constant Integer := Floor_Int ((Start_X + Pixel_World * Float (Width)) / Grid_Cell + Reach) + 1;
               Line_Share : constant Float := 1.0 - Blend;
            begin
               for K in First .. Last loop
                  declare
                     Center : constant Float := Column_Of (Float (K) * Grid_Cell);
                     Span : constant Float := Reach / Pixel_Cells;
                     X0 : constant Integer := Integer'Max (0, Floor_Int (Center - Span));
                     X1 : constant Integer := Integer'Min (Width - 1, Floor_Int (Center + Span) + 1);
                     Inside_Line : constant Boolean := Inside_Z and then abs (Float (K) * Grid_Cell) <= Neon_Sim.Half_Width;
                     Hue : constant RGB := (if Inside_Line then Grid_Pink else Grid_Violet);
                     Gain : constant Float := (if Inside_Line then Inner_Gain else Outer_Gain);
                  begin
                     for X in X0 .. X1 loop
                        declare
                           Near : constant Float :=
                             Line_Profile (abs (Float (X) - Center) * Pixel_Cells, Pixel_Cells);
                        begin
                           if Near > 0.0 then
                              Add_Pixel (Pixels, Row_Base + X, Hue, Gain * Line_Share * Near);
                           end if;
                        end;
                     end loop;
                  end;
               end loop;
            end;
         end if;
         --  Lights: grid lines take them strongly, the floor softly.
         if Lit then
            declare
               First_Col : Integer := Map_Columns;
               Last_Col : Integer := -1;
            begin
               for Col in 0 .. Map_Columns - 1 loop
                  declare
                     I0 : constant Natural := Map_Row * Map_Columns + Col;
                     I1 : constant Natural := I0 + Map_Columns;
                  begin
                     if LR (I0) + LG (I0) + LB (I0) + LR (I1) + LG (I1) + LB (I1) > 0.0 then
                        First_Col := Integer'Min (First_Col, Col - 1);
                        Last_Col := Col + 1;
                     end if;
                  end;
               end loop;
               if Last_Col >= 0 then
                  declare
                     X0 : constant Integer := Integer'Max
                       (0, Floor_Int (Clamp (Column_Of (Map_X0 + Map_Cell * Float (First_Col)), -1.0, Float (Width))));
                     X1 : constant Integer := Integer'Min
                       (Width - 1, Floor_Int (Clamp (Column_Of (Map_X0 + Map_Cell * Float (Last_Col)), -1.0, Float (Width))) + 1);
                  begin
                     for X in X0 .. X1 loop
                        declare
                           WX : constant Float := Start_X + Float (X) * Pixel_World;
                           Col_F : constant Float := (WX - Map_X0) / Map_Cell;
                           Col : constant Integer := Floor_Int (Col_F);
                        begin
                           if Col >= 0 and then Col < Map_Columns - 1 then
                              declare
                                 Fx : constant Float := Col_F - Float (Col);
                                 I00 : constant Natural := Map_Row * Map_Columns + Col;
                                 I10 : constant Natural := I00 + Map_Columns;
                                 W00 : constant Float := (1.0 - Fx) * (1.0 - Row_Frac);
                                 W01 : constant Float := Fx * (1.0 - Row_Frac);
                                 W10 : constant Float := (1.0 - Fx) * Row_Frac;
                                 W11 : constant Float := Fx * Row_Frac;
                                 Light_Color : constant RGB :=
                                   (LR (I00) * W00 + LR (I00 + 1) * W01 + LR (I10) * W10 + LR (I10 + 1) * W11,
                                    LG (I00) * W00 + LG (I00 + 1) * W01 + LG (I10) * W10 + LG (I10 + 1) * W11,
                                    LB (I00) * W00 + LB (I00 + 1) * W01 + LB (I10) * W10 + LB (I10 + 1) * W11);
                                 Along : constant Float :=
                                   Line_Profile (Cell_Distance (WX / Grid_Cell), Pixel_Cells) * (1.0 - Blend) + Even;
                                 Grid : constant Float := Min (1.6, Along + Across_Z);
                              begin
                                 Add_Pixel (Pixels, Row_Base + X, Light_Color, Keep * (1.3 * Grid + 0.22));
                              end;
                           end if;
                        end;
                     end loop;
                  end;
               end if;
            end;
         end if;
      end Floor_Row;

   begin
      for Row in 0 .. Map_Rows - 1 loop
         for Col in 0 .. Map_Columns - 1 loop
            declare
               I : constant Natural := Row * Map_Columns + Col;
            begin
               if LR (I) + LG (I) + LB (I) > 0.0 then
                  Row_Lit (Row) := True;
                  if Row > 0 then
                     Row_Lit (Row - 1) := True;
                  end if;
                  exit;
               end if;
            end;
         end loop;
      end loop;
      --  The horizon: the first row from the top that meets the floor.
      declare
         Low : Natural := 0;
         High : Natural := Height;
         Mid : Natural;
      begin
         while Low < High loop
            Mid := (Low + High) / 2;
            Row_Floor (Cam, Float (Mid) + 0.5, Hit, Z, T);
            if Hit then
               High := Mid;
            else
               Low := Mid + 1;
            end if;
         end loop;
         Horizon := Low;
      end;
      Sun_Y := Float (Horizon) - 0.3 * Sun_R;
      Sun_X := Float (Width) / 2.0;
      for X in Ridges'Range loop
         Ridges (X) := Float (Horizon) - H * Ridge (Float (X) / Float (Width));
      end loop;

      Previous_Z := 1.0e6;
      for Y in 0 .. Height - 1 loop
         declare
            Row_Base : constant Natural := Y * Pitch;
            FY : constant Float := Float (Y) + 0.5;
         begin
            if Y < Horizon then
               --  Sky.
               declare
                  Up : constant Float := Saturate (FY / Max (Float (Horizon), 1.0));
                  Sky : RGB :=
                    (if Up < 0.55 then Mix (Sky_Top, Sky_Mid, Up / 0.55)
                     elsif Up < 0.85 then Mix (Sky_Mid, Sky_Low, (Up - 0.55) / 0.3)
                     else Mix (Sky_Low, Horizon_Glow, (Up - 0.85) / 0.15));
                  Above : constant Float := Float (Horizon) - FY;
                  Glow_Band : constant Float := 0.45 / (1.0 + (Above / (0.05 * H)) ** 2);
                  DY : constant Float := FY - Sun_Y;
                  In_Sun_Rows : constant Boolean := abs DY < Sun_R * 2.2;
                  Band_Cut : Boolean := False;
                  Sky_Packed : Unsigned_32;
               begin
                  Sky := Sky + Glow_Band * Horizon_Glow;
                  Sky_Packed := Pack (Sky);
                  --  The sun's stripes: wider gaps lower down, drifting down.
                  if DY > -0.15 * Sun_R then
                     declare
                        Depth : constant Float := (DY + 0.15 * Sun_R) / Sun_R;
                        Phase : constant Float := Fraction (Depth * 7.0 - F.Seconds * 0.35);
                     begin
                        Band_Cut := Phase < 0.1 + 0.55 * Depth;
                     end;
                  end if;
                  for X in 0 .. Width - 1 loop
                     declare
                        C : RGB := Sky;
                        FX : constant Float := Float (X) + 0.5;
                     begin
                        if FY < Ridges (X) and then not In_Sun_Rows
                          and then (Up >= 0.6 or else (Hash (X, Y) and 16#3FF#) /= 0)
                        then
                           --  Plain sky: most of it.
                           Pixels (Row_Base + X) := Sky_Packed;
                           goto Next_Sky_Pixel;
                        end if;
                        if FY >= Ridges (X) then
                           --  Mountains: dark, with a neon ridge line.
                           declare
                              Below : constant Float := FY - Ridges (X);
                           begin
                              C := Mix (Mountain_Fill, Sky, 0.25)
                                + (Saturate (1.6 - Below) * 1.2) * Mountain_Edge;
                           end;
                        else
                           if Up < 0.6 then
                              declare
                                 S : constant Unsigned_32 := Hash (X, Y);
                              begin
                                 if (S and 16#3FF#) = 0 then
                                    C := C + ((0.4 + 0.4 * Sin_Turns (Star_Time * 0.5
                                       + Float (S mod 97) / 97.0)) * (1.0 - Up / 0.6)) * White;
                                 end if;
                              end;
                           end if;
                           if In_Sun_Rows then
                              declare
                                 DX : constant Float := FX - Sun_X;
                                 D : constant Float := Sqrt (DX * DX + DY * DY);
                              begin
                                 if D < Sun_R and then not Band_Cut then
                                    C := Mix (Sun_Top, Sun_Bottom,
                                              Saturate ((DY + Sun_R) / (2.0 * Sun_R)))
                                      + (0.25 * F.Sun_Swell) * White;
                                 elsif D >= Sun_R then
                                    C := C + (0.55 / (1.0 + ((D - Sun_R) / (0.18 * Sun_R)) ** 2)
                                              * (1.0 + 0.4 * F.Sun_Swell)) * Sun_Bottom;
                                 end if;
                              end;
                           end if;
                        end if;
                        Pixels (Row_Base + X) := Pack (C);
                     end;
                     <<Next_Sky_Pixel>>
                  end loop;
               end;
            else
               Row_Floor (Cam, FY, Hit, Z, T);
               if not Hit then
                  Z := 200.0;
                  T := 1000.0;
               end if;
               Floor_Row (Y, Z, T, Previous_Z);
               Previous_Z := Z;
            end if;
         end;
      end loop;
   end Background;

   ---------------------------------------------------------------------------
   --  Additive drawing
   ---------------------------------------------------------------------------


   --  An anti-aliased line of Thickness pixels, calling Plot with a
   --  coverage weight for every pixel it touches inside Width x Height.
   generic
      with procedure Plot (X, Y : Natural; Weight : Float);
   procedure Raster_Line (X0, Y0, X1, Y1, Thickness : Float; Width, Height : Positive);

   procedure Raster_Line (X0, Y0, X1, Y1, Thickness : Float; Width, Height : Positive) is
      DX : constant Float := X1 - X0;
      DY : constant Float := Y1 - Y0;
      Half : Float;
   begin
      if abs DX >= abs DY then
         if abs DX < 1.0e-4 then
            return;
         end if;
         Half := 0.5 * Thickness * Sqrt (1.0 + (DY / DX) * (DY / DX));
         declare
            Left : constant Float := Max (Min (X0, X1), 0.0);
            Right : constant Float := Min (Max (X0, X1), Float (Width - 1));
         begin
            if Left > Right then
               return;
            end if;
            for X in Natural (Floor (Left)) .. Natural (Floor (Right)) loop
               declare
                  Center : constant Float := Y0 + (Float (X) + 0.5 - X0) * DY / DX;
                  Top : constant Integer := Integer (Floor (Center - Half - 0.5));
                  Bottom : constant Integer := Integer (Floor (Center + Half + 0.5));
               begin
                  for Y in Integer'Max (0, Top) .. Integer'Min (Height - 1, Bottom) loop
                     Plot (X, Y, Saturate (Half + 0.5 - abs (Float (Y) + 0.5 - Center)));
                  end loop;
               end;
            end loop;
         end;
      else
         Half := 0.5 * Thickness * Sqrt (1.0 + (DX / DY) * (DX / DY));
         declare
            Top : constant Float := Max (Min (Y0, Y1), 0.0);
            Bottom : constant Float := Min (Max (Y0, Y1), Float (Height - 1));
         begin
            if Top > Bottom then
               return;
            end if;
            for Y in Natural (Floor (Top)) .. Natural (Floor (Bottom)) loop
               declare
                  Center : constant Float := X0 + (Float (Y) + 0.5 - Y0) * DX / DY;
                  Left : constant Integer := Integer (Floor (Center - Half - 0.5));
                  Right : constant Integer := Integer (Floor (Center + Half + 0.5));
               begin
                  for X in Integer'Max (0, Left) .. Integer'Min (Width - 1, Right) loop
                     Plot (X, Y, Saturate (Half + 0.5 - abs (Float (X) + 0.5 - Center)));
                  end loop;
               end;
            end loop;
         end;
      end if;
   end Raster_Line;

   --  Clip a segment to a generous box around the view so far-off endpoints
   --  never make long loops or overflow (Liang-Barsky).
   procedure Clip (X0, Y0, X1, Y1 : in out Float; Width, Height : Float; Inside : out Boolean) is
      T0 : Float := 0.0;
      T1 : Float := 1.0;
      DX : constant Float := X1 - X0;
      DY : constant Float := Y1 - Y0;
      Margin : constant := 8.0;
      procedure Edge (P, Q : Float) is
      begin
         if P = 0.0 then
            if Q < 0.0 then
               T0 := 2.0;
            end if;
         else
            declare
               R : constant Float := Q / P;
            begin
               if P < 0.0 then
                  T0 := Max (T0, R);
               else
                  T1 := Min (T1, R);
               end if;
            end;
         end if;
      end Edge;
   begin
      Edge (-DX, X0 + Margin);
      Edge (DX, Width + Margin - X0);
      Edge (-DY, Y0 + Margin);
      Edge (DY, Height + Margin - Y0);
      Inside := T0 <= T1;
      if Inside then
         X1 := X0 + T1 * DX;
         Y1 := Y0 + T1 * DY;
         X0 := X0 + T0 * DX;
         Y0 := Y0 + T0 * DY;
      end if;
   end Clip;

   ---------------------------------------------------------------------------

   procedure Render (R : in out Renderer; F : Frame; Target : System.Address;
                     Width, Height, Pitch_Pixels : Positive) is
      Pixels : Pixel_Array (0 .. Pitch_Pixels * Height - 1) with Import, Address => Target;
      Scale : constant Float := Float (Height) / Reference_Height;
      Cam : Camera := F.Cam;
      GW, GH : Positive;
      Glow_Gain : constant Float := 1.1 * F.Glow;
   begin
      Ensure (R, Width, Height);
      GW := R.Glow_Width;
      GH := R.Glow_Height;
      Cam.Center_X := Cam.Center_X + F.Shake.X * Scale;
      Cam.Center_Y := Cam.Center_Y + F.Shake.Y * Scale;
      Splat_Lights (R, F);
      Background (R, F, Cam, Pixels, Width, Height, Pitch_Pixels);
      R.Glow_R.all := [others => 0.0];
      R.Glow_G.all := [others => 0.0];
      R.Glow_B.all := [others => 0.0];

      declare
         GR : Float_Plane renames R.Glow_R.all;
         GG : Float_Plane renames R.Glow_G.all;
         GB : Float_Plane renames R.Glow_B.all;
         Color : RGB;

         procedure Plot_Frame (X, Y : Natural; Weight : Float) is
         begin
            Add_Pixel (Pixels, Y * Pitch_Pixels + X, Color, Weight);
         end Plot_Frame;
         procedure Plot_Glow (X, Y : Natural; Weight : Float) is
            I : constant Natural := Y * GW + X;
         begin
            GR (I) := GR (I) + Weight * Color.R;
            GG (I) := GG (I) + Weight * Color.G;
            GB (I) := GB (I) + Weight * Color.B;
         end Plot_Glow;
         procedure Frame_Line is new Raster_Line (Plot_Frame);
         procedure Glow_Line is new Raster_Line (Plot_Glow);

         procedure Draw (X0, Y0, X1, Y1, Thickness : Float; C : RGB) is
            A, B, P, Q : Float;
            Inside : Boolean;
            G : constant Float := 1.0 / Float (Glow_Scale);
         begin
            A := X0; B := Y0; P := X1; Q := Y1;
            Clip (A, B, P, Q, Float (Width), Float (Height), Inside);
            if not Inside then
               return;
            end if;
            Color := C;
            Frame_Line (A, B, P, Q, Max (1.0, Thickness * Scale), Width, Height);
            Color := 0.9 * C;
            Glow_Line (A * G, B * G, P * G, Q * G, Max (1.0, 1.5 * Thickness * Scale * G + 0.8), GW, GH);
         end Draw;

         procedure Dot (X, Y, Radius : Float; C : RGB) is
            G : constant Float := 1.0 / Float (Glow_Scale);
         begin
            if X < -Radius or else Y < -Radius or else X > Float (Width) + Radius
              or else Y > Float (Height) + Radius
            then
               return;
            end if;
            if Radius < 0.8 then
               declare
                  IX : constant Integer := Integer (Floor (X));
                  IY : constant Integer := Integer (Floor (Y));
                  Area : constant Float := Max (Radius, 0.3) / 0.8;
               begin
                  if IX in 0 .. Width - 1 and then IY in 0 .. Height - 1 then
                     Add_Pixel (Pixels, IY * Pitch_Pixels + IX, C, 1.4 * Area);
                  end if;
               end;
            else
               declare
                  X0 : constant Integer := Integer'Max (0, Integer (Floor (X - Radius)));
                  X1 : constant Integer := Integer'Min (Width - 1, Integer (Floor (X + Radius)));
                  Y0 : constant Integer := Integer'Max (0, Integer (Floor (Y - Radius)));
                  Y1 : constant Integer := Integer'Min (Height - 1, Integer (Floor (Y + Radius)));
               begin
                  for PY in Y0 .. Y1 loop
                     for PX in X0 .. X1 loop
                        declare
                           DX : constant Float := Float (PX) + 0.5 - X;
                           DY : constant Float := Float (PY) + 0.5 - Y;
                           W : constant Float := Saturate (1.0 - Sqrt (DX * DX + DY * DY) / Radius);
                        begin
                           if W > 0.0 then
                              Add_Pixel (Pixels, PY * Pitch_Pixels + PX, C, 1.3 * W * W);
                           end if;
                        end;
                     end loop;
                  end loop;
               end;
            end if;
            --  Into the glow buffer, bilinear.
            declare
               GX : constant Float := X * G - 0.5;
               GY : constant Float := Y * G - 0.5;
               IX : constant Integer := Integer (Floor (GX));
               IY : constant Integer := Integer (Floor (GY));
               FX : constant Float := GX - Float (IX);
               FY : constant Float := GY - Float (IY);
               Amount : constant Float := 0.6 * Min (4.0, 0.5 + Radius * Radius * 0.25);
               procedure Put (PX, PY : Integer; W : Float) is
               begin
                  if PX in 0 .. GW - 1 and then PY in 0 .. GH - 1 then
                     Color := C;
                     Plot_Glow (PX, PY, W * Amount);
                  end if;
               end Put;
            begin
               Put (IX, IY, (1.0 - FX) * (1.0 - FY));
               Put (IX + 1, IY, FX * (1.0 - FY));
               Put (IX, IY + 1, (1.0 - FX) * FY);
               Put (IX + 1, IY + 1, FX * FY);
            end;
         end Dot;

         X0, Y0, X1, Y1, D0, D1 : Float;
         V0, V1 : Boolean;
      begin
         for I in 1 .. F.Line_Count loop
            declare
               L : Line_3D renames F.Lines (I);
            begin
               Project (Cam, L.A, X0, Y0, D0, V0);
               Project (Cam, L.B, X1, Y1, D1, V1);
               if V0 and then V1 then
                  Draw (X0, Y0, X1, Y1, L.Width, L.Color);
               end if;
            end;
         end loop;
         for I in 1 .. F.Point_Count loop
            declare
               P : Point renames F.Points (I);
            begin
               Project (Cam, P.Position, X0, Y0, D0, V0);
               if V0 then
                  Dot (X0, Y0, P.Size * Cam.Focal / D0, P.Color);
               end if;
            end;
         end loop;
         for I in 1 .. F.Stroke_Count loop
            declare
               S : Stroke renames F.Strokes (I);
               H : constant Float := Float (Height);
            begin
               Draw (S.A.X * H, S.A.Y * H, S.B.X * H, S.B.Y * H, S.Width, S.Color);
            end;
         end loop;
      end;

      --  Blur the glow: two box passes each way (close to a Gaussian).
      declare
         Radius : constant := 2;
         Taps : constant Float := Float (2 * Radius + 1);
         procedure Blur (Plane : in out Float_Plane) is
            Line : Float_Plane renames R.Scratch.all;
            Sum : Float;
         begin
            for Pass in 1 .. 2 loop
               for Y in 0 .. GH - 1 loop
                  for X in 0 .. GW - 1 loop
                     Line (X) := Plane (Y * GW + X);
                  end loop;
                  Sum := 0.0;
                  for K in -Radius .. Radius loop
                     Sum := Sum + Line (Integer'Max (0, Integer'Min (GW - 1, K)));
                  end loop;
                  for X in 0 .. GW - 1 loop
                     Plane (Y * GW + X) := Sum / Taps;
                     Sum := Sum - Line (Integer'Max (0, X - Radius))
                       + Line (Integer'Min (GW - 1, X + Radius + 1));
                  end loop;
               end loop;
               for X in 0 .. GW - 1 loop
                  for Y in 0 .. GH - 1 loop
                     Line (Y) := Plane (Y * GW + X);
                  end loop;
                  Sum := 0.0;
                  for K in -Radius .. Radius loop
                     Sum := Sum + Line (Integer'Max (0, Integer'Min (GH - 1, K)));
                  end loop;
                  for Y in 0 .. GH - 1 loop
                     Plane (Y * GW + X) := Sum / Taps;
                     Sum := Sum - Line (Integer'Max (0, Y - Radius))
                       + Line (Integer'Min (GH - 1, Y + Radius + 1));
                  end loop;
               end loop;
            end loop;
         end Blur;
      begin
         Blur (R.Glow_R.all);
         Blur (R.Glow_G.all);
         Blur (R.Glow_B.all);
      end;

      --  Composite: glow, flashes, tint, scanlines and vignette. The glow is
      --  interpolated between two glow rows once a row, then across.
      declare
         GR : Float_Plane renames R.Glow_R.all;
         GG : Float_Plane renames R.Glow_G.all;
         GB : Float_Plane renames R.Glow_B.all;
         Line : Float_Plane renames R.Scratch.all;   --  3 * GW: the row's R, G, B
         Inverse : constant Float := 1.0 / Float (Glow_Scale);
         Wash : constant RGB :=
           (0.15 * F.Flash + 0.45 * F.Hurt, 0.12 * F.Flash, 0.15 * F.Flash + 0.08 * F.Hurt);
         Tint : constant RGB :=
           (1.0 - 0.2 * F.Underwater, 1.0 - 0.35 * F.Underwater, 1.0 - 0.05 * F.Underwater);
         Column_Cell : array (0 .. Width - 1) of Natural;
         Column_Frac, Column_Shade : array (0 .. Width - 1) of Float;
         Scale_255 : constant Float := 1.0 / 255.0;
      begin
         for X in 0 .. Width - 1 loop
            declare
               GXF : constant Float := Clamp ((Float (X) + 0.5) * Inverse - 0.5, 0.0, Float (GW - 2));
               VX : constant Float := (Float (X) / Float (Width) - 0.5) * 2.0;
            begin
               Column_Cell (X) := Natural (Floor (GXF));
               Column_Frac (X) := GXF - Float (Column_Cell (X));
               Column_Shade (X) := Vignette_Strength * 0.5 * VX * VX;
            end;
         end loop;
         for Y in 0 .. Height - 1 loop
            declare
               GYF : constant Float := Clamp ((Float (Y) + 0.5) * Inverse - 0.5, 0.0, Float (GH - 2));
               GY : constant Natural := Natural (Floor (GYF));
               FY : constant Float := GYF - Float (GY);
               VY : constant Float := (Float (Y) / Float (Height) - 0.5) * 2.0;
               Row_Shade : constant Float :=
                 (if F.Scanlines and then Y mod 2 = 1 then Scanline_Dim else 1.0);
               Row_Vignette : constant Float := 1.0 - Vignette_Strength * 0.5 * VY * VY;
               Upper : constant Natural := GY * GW;
               Lower : constant Natural := Upper + GW;
               Shaded_R : constant Float := Row_Shade * Tint.R;
               Shaded_G : constant Float := Row_Shade * Tint.G;
               Shaded_B : constant Float := Row_Shade * Tint.B;
            begin
               for GX in 0 .. GW - 1 loop
                  Line (GX) := Glow_Gain * (GR (Upper + GX) + FY * (GR (Lower + GX) - GR (Upper + GX)));
                  Line (GW + GX) := Glow_Gain * (GG (Upper + GX) + FY * (GG (Lower + GX) - GG (Upper + GX)));
                  Line (2 * GW + GX) :=
                    Glow_Gain * (GB (Upper + GX) + FY * (GB (Lower + GX) - GB (Upper + GX)));
               end loop;
               for X in 0 .. Width - 1 loop
                  declare
                     C0 : constant Natural := Column_Cell (X);
                     FX : constant Float := Column_Frac (X);
                     Index : constant Natural := Y * Pitch_Pixels + X;
                     P : constant Unsigned_32 := Pixels (Index);
                     Shade : constant Float := Row_Vignette - Column_Shade (X);
                     Red : constant Float := Float (Shift_Right (P, 16) and 16#FF#) * Scale_255
                       + Line (C0) + FX * (Line (C0 + 1) - Line (C0)) + Wash.R;
                     Green : constant Float := Float (Shift_Right (P, 8) and 16#FF#) * Scale_255
                       + Line (GW + C0) + FX * (Line (GW + C0 + 1) - Line (GW + C0)) + Wash.G;
                     Blue : constant Float := Float (P and 16#FF#) * Scale_255
                       + Line (2 * GW + C0) + FX * (Line (2 * GW + C0 + 1) - Line (2 * GW + C0)) + Wash.B;
                  begin
                     Pixels (Index) := Pack ((Shade * Shaded_R * Red, Shade * Shaded_G * Green,
                                              Shade * Shaded_B * Blue));
                  end;
               end loop;
            end;
         end loop;
      end;
   end Render;

   function Frame_Hash (Target : System.Address; Width, Height, Pitch_Pixels : Positive)
     return Unsigned_64 is
      Pixels : Pixel_Array (0 .. Pitch_Pixels * Height - 1) with Import, Address => Target;
      Digest : Unsigned_64 := 16#CBF2_9CE4_8422_2325#;
      Prime : constant Unsigned_64 := 16#100_0000_01B3#;
   begin
      for Y in 0 .. Height - 1 loop
         for X in 0 .. Width - 1 loop
            Digest := (Digest xor Unsigned_64 (Pixels (Y * Pitch_Pixels + X) and 16#FF_FFFF#)) * Prime;
         end loop;
      end loop;
      return Digest;
   end Frame_Hash;

end Neon_Soft;
