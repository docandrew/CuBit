------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Neon Blast's scene: the camera, and one frame's description that both
--  renderers draw (the Vulkan canvas and the software fallback).
--
--  @description
--  The camera looks down on the arena at a steep angle; past the arena's
--  far edge the floor bends away (y = -Bend * (z - Bend_Start)^2), so the
--  horizon and the sun show at the top of the screen while the arena stays
--  nearly top-down and readable. Each screen row meets the floor at one
--  depth, which is what lets the software path shade the floor a row at a
--  time.
--
--  A frame is plain data: glowing 3D line segments (wireframes, shots,
--  trails), points (particles), coloured point lights that light the grid,
--  ripples on the floor, 2D HUD strokes, the beat-driven values (kick
--  pulse, sun swell, glow level) and the stage's look (palette and grid). Ceilings are named; what does not fit is
--  counted in Dropped and shown on the overlay.
------------------------------------------------------------------------------
with Neon_Math; use Neon_Math;
with Neon_Style;
with Neon_Game;

package Neon_Scene is

   type Camera is record
      Position : Vec3;
      Up, Forward : Vec3;        --  Right is +X
      Focal : Float := 1.0;      --  pixels
      Center_X, Center_Y : Float := 0.0;
      Width, Height : Float := 1.0;
      Bend_Start : Float := 9.5;
      Bend : Float := 0.02;
   end record;

   --  The camera for a Width x Height viewport (pixels).
   function Camera_For (Width, Height : Positive) return Camera;
   --  Screen position of a world point; Visible False behind the camera.
   procedure Project (Cam : Camera; P : Vec3; X, Y, Depth : out Float; Visible : out Boolean);
   --  The arena floor point under screen position (X, Y) (the aim).
   function Floor_Point (Cam : Camera; X, Y : Float) return Vec2;
   --  Where screen row Y meets the (bent) floor: Hit, the world depth Z and
   --  the ray distance T (the world size of a pixel there is T / Focal).
   procedure Row_Floor (Cam : Camera; Y : Float; Hit : out Boolean; Z, T : out Float);
   --  The floor height at depth Z.
   function Floor_Height (Cam : Camera; Z : Float) return Float is
     (if Z <= Cam.Bend_Start then 0.0 else -Cam.Bend * (Z - Cam.Bend_Start) * (Z - Cam.Bend_Start));

   Line_Ceiling : constant := 6000;
   Stroke_Ceiling : constant := 3000;
   Point_Ceiling : constant := 4096;
   Light_Ceiling : constant := 32;
   Ripple_Ceiling : constant := 16;

   type Line_3D is record
      A, B : Vec3;
      Color : RGB;
      Width : Float := 1.0;     --  pixels at a 1080-pixel-high view
   end record;
   --  HUD strokes in view units: Y 0 (top) .. 1 (bottom), X 0 .. aspect.
   type Stroke is record
      A, B : Vec2;
      Color : RGB;
      Width : Float := 1.0;     --  pixels at a 1080-pixel-high view
   end record;
   type Point is record
      Position : Vec3;
      Color : RGB;
      Size : Float := 0.1;      --  world radius
   end record;
   type Light is record
      Position : Vec3;
      Color : RGB;              --  intensity included
      Radius : Float := 3.0;    --  world units of reach
   end record;
   type Floor_Ripple is record
      Center : Vec2;
      Radius, Width, Strength : Float := 0.0;
      Color : RGB;
   end record;

   type Line_Array is array (1 .. Line_Ceiling) of Line_3D;
   type Stroke_Array is array (1 .. Stroke_Ceiling) of Stroke;
   type Point_Array is array (1 .. Point_Ceiling) of Point;
   type Light_Array is array (1 .. Light_Ceiling) of Light;
   type Ripple_Array is array (1 .. Ripple_Ceiling) of Floor_Ripple;

   type Frame is record
      Width, Height : Positive := 1;
      Cam : Camera;
      Seconds : Float := 0.0;         --  song time shown
      Scroll : Float := 0.0;          --  grid scroll toward the viewer, world units
      Kick : Float := 0.0;            --  0 .. 1, the grid pulse
      Sun_Swell : Float := 0.0;       --  0 .. 1 on chord changes
      Glow : Float := 0.7;            --  bloom gain, follows the pad
      Flash : Float := 0.0;           --  white flash
      Hurt : Float := 0.0;            --  red flash
      Shake : Vec2;                   --  pixels at 1080 high
      Underwater : Float := 0.0;      --  breakdown tint 0 .. 1
      Scanlines : Boolean := True;
      Look : Neon_Style.Look := Neon_Style.Classic;   --  the stage's palette and grid
      Lines : Line_Array;
      Line_Count : Natural range 0 .. Line_Ceiling := 0;
      Strokes : Stroke_Array;
      Stroke_Count : Natural range 0 .. Stroke_Ceiling := 0;
      Points : Point_Array;
      Point_Count : Natural range 0 .. Point_Ceiling := 0;
      Lights : Light_Array;
      Light_Count : Natural range 0 .. Light_Ceiling := 0;
      Ripples : Ripple_Array;
      Ripple_Count : Natural range 0 .. Ripple_Ceiling := 0;
      Dropped : Natural := 0;
   end record;

   --  Build the frame for a Width x Height view from the game's state at
   --  its simulation time.
   procedure Build (F : in out Frame; G : Neon_Game.Game; Width, Height : Positive);

   --  HUD helpers (also used by the native overlay).
   procedure Add_Stroke (F : in out Frame; A, B : Vec2; Color : RGB; Width : Float);
   --  Text in the stroke font: Size is the glyph height in view units;
   --  Centered places X at the text's middle.
   procedure Add_Text (F : in out Frame; Text : String; X, Y, Size : Float; Color : RGB;
                       Width : Float := 2.0; Centered : Boolean := False);

   --  The sky, floor, sun and mountain colours and the grid's cell are the
   --  frame's Look (the stage's style draws them).
   --  The sun's radius as a fraction of the view height.
   Sun_Radius : constant := 0.14;

end Neon_Scene;
