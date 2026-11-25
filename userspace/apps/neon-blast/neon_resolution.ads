------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Neon Blast's dynamic resolution (APP-008): the internal resolution the
--  software renderer draws at, chosen from the measured render time so a
--  frame fits its budget. A big screen on a slow CPU costs sharpness, not
--  frame rate.
--
--  @description
--  The software path renders at a Level of a fixed ladder of linear scales
--  (100% down to 25%, each step a fifth to a quarter fewer pixels) and the frame
--  is then scaled up to the window (Neon_Upscale). The controller is fed,
--  once a frame, the window's size and the measured render and scale times
--  in microseconds:
--  - it keeps a smoothed (exponential, a quarter per frame) render and
--    scale time;
--  - it predicts any level's cost from the current one by pixel count (the
--    render's cost per pixel holds across levels; the scale's cost is the
--    output's, so it carries over unscaled, and is zero at Native);
--  - over the budget for Drop_After frames in a row (the average and each
--    frame itself: one slow frame does not drop it), it drops at once to
--    the finest level predicted to fit (as many steps as that takes);
--  - with the next finer level predicted under Raise_Below_Us (the
--    hysteresis band: 80% of the budget) for Raise_After frames in a row,
--    it raises one step;
--  - after a change it ignores Settle_Frames frames (new buffers, the
--    first frame at the new size).
--  It never renders above the window's own resolution (no supersampling),
--  never below the ladder's lowest scale, and never below Minimum_Lines
--  lines unless the window itself is smaller (then it stays at Native).
--
--  The game's logic never sees this: the simulation, the aim (the window's
--  own camera) and the replay hashes do not depend on the render size.
------------------------------------------------------------------------------
package Neon_Resolution with SPARK_Mode, Pure is

   --  Pixels on one axis.
   Maximum_Extent : constant := 16_384;
   subtype Extent is Positive range 1 .. Maximum_Extent;

   --  A measured time in microseconds, clamped at one second.
   Maximum_Measure_Us : constant := 1_000_000;
   subtype Microseconds is Natural range 0 .. Maximum_Measure_Us;

   --  The ladder of linear scales, in percent of the window.
   subtype Percent is Positive range 1 .. 100;
   type Level is range 0 .. 12;
   Native : constant Level := Level'First;
   Lowest : constant Level := Level'Last;
   type Scale_Table is array (Level) of Percent;
   Scale_Of : constant Scale_Table := [100, 90, 80, 71, 63, 56, 50, 45, 40, 35, 31, 28, 25];

   --  The fewest lines the game is drawn at: its stroke text and thin grid
   --  lines stay legible (a 640 x 360 view).
   Minimum_Lines : constant := 360;

   --  The software path presents at most every Frame_Period_Us (60 Hz).
   --  Rendering plus scaling gets Budget_Us of it; the rest is the scene,
   --  the overlay, publishing and the desktop's composition.
   Frame_Period_Us : constant := 16_667;
   Budget_Us : constant := 10_000;
   Raise_Below_Us : constant := 8_000;
   Drop_After : constant := 3;
   Raise_After : constant := 45;
   Settle_Frames : constant := 2;
   pragma Compile_Time_Error
     (Raise_Below_Us >= Budget_Us or else Budget_Us >= Frame_Period_Us,
      "the raise mark must sit under the budget, the budget under the frame");

   --  One axis at a level: Output scaled, rounded down, at least one pixel.
   function Internal (Output : Extent; At_Level : Level) return Extent
     with Post => Internal'Result <= Output
                  and then (if At_Level = Native then Internal'Result = Output);

   --  The lowest (coarsest) level a window Output_Height lines high may
   --  use: the coarsest that keeps Minimum_Lines lines, or Native.
   function Floor_For (Output_Height : Extent) return Level
     with Post => (Floor_For'Result = Native
                   or else Internal (Output_Height, Floor_For'Result) >= Minimum_Lines);

   type Controller is private;

   function Initial return Controller;

   --  The level the next frame renders at.
   function Current (C : Controller) return Level;

   --  How often the level went down and up.
   type Change_Count is range 0 .. 2**31 - 1;
   function Drops (C : Controller) return Change_Count;
   function Raises (C : Controller) return Change_Count;

   --  The smoothed render and scale time of the frames at the current
   --  level (zero before the first measurement).
   function Smoothed_Render_Us (C : Controller) return Microseconds;
   function Smoothed_Scale_Us (C : Controller) return Microseconds;

   --  Take one frame: drawn at Current (C) for a window Output_Width x
   --  Output_Height, rendering took Render_Us and scaling Scale_Us. The
   --  next level never goes below the window's floor.
   procedure Note_Frame
     (C : in out Controller; Output_Width, Output_Height : Extent; Render_Us, Scale_Us : Microseconds)
     with Post => Current (C) <= Floor_For (Output_Height);

private

   subtype Over_Count is Natural range 0 .. Drop_After;
   subtype Room_Count is Natural range 0 .. Raise_After;
   subtype Settle_Count is Natural range 0 .. Settle_Frames;

   type Controller is record
      At_Level : Level := Native;
      Measured : Boolean := False;
      Render_Avg, Scale_Avg : Microseconds := 0;
      Scale_Known : Boolean := False;
      Last_Width, Last_Height : Extent := 1;
      Over : Over_Count := 0;
      Room : Room_Count := 0;
      Settle : Settle_Count := 0;
      Down, Up : Change_Count := 0;
   end record;

   function Initial return Controller is ((others => <>));
   function Current (C : Controller) return Level is (C.At_Level);
   function Drops (C : Controller) return Change_Count is (C.Down);
   function Raises (C : Controller) return Change_Count is (C.Up);
   function Smoothed_Render_Us (C : Controller) return Microseconds is (C.Render_Avg);
   function Smoothed_Scale_Us (C : Controller) return Microseconds is (C.Scale_Avg);

end Neon_Resolution;
