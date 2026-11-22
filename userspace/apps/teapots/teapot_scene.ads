------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The teapot gallery's scene: nine pots in a 3 x 3 grid, each in its own
--  viewport with its own perspective camera and one shading model, spinning
--  over time. Pure geometry and matrices; teapot_vk.c draws what this says.
------------------------------------------------------------------------------
with Teapot_Math;

package Teapot_Scene with SPARK_Mode is

   --  Positions are the shaders' SHADING specialization values.
   type Shading is
     (Flat, Gouraud, Phong, Blinn_Phong, Toon, Wireframe, Normals, Environment, Procedural);

   Columns : constant := 3;
   Rows : constant := 3;
   pragma Compile_Time_Error
     (Columns * Rows /= Shading'Pos (Shading'Last) + 1, "one cell per shading");

   function Label (S : Shading) return String
     with Post => Label'Result'Length in 1 .. 32;

   --  A rectangle in pixels (target pixels for viewports, logical pixels
   --  for the labels the window draws).
   type Cell is record
      X, Y, Width, Height : Natural := 0;
   end record;

   --  Grid cell of S inside a Width x Height area with Gap pixels between
   --  and around cells. Empty when the area is too small.
   Maximum_Edge : constant := 65_535;
   subtype Edge is Natural range 0 .. Maximum_Edge;
   function Cell_Of (S : Shading; Width, Height, Gap : Edge) return Cell
     with Post => Cell_Of'Result.X + Cell_Of'Result.Width <= Width and then
                  Cell_Of'Result.Y + Cell_Of'Result.Height <= Height;

   --  The animation loops seamlessly every Loop_Seconds.
   Loop_Seconds : constant := 120.0;
   subtype Scene_Time is Float range 0.0 .. Loop_Seconds;
   function Wrap (Seconds : Float) return Scene_Time;

   --  Push constants of one pot (teapot.vert's Pot block): mvp, three rows
   --  of the model-view matrix, params (x = scene seconds).
   Push_Floats : constant := 32;
   type Push_Constants is array (0 .. Push_Floats - 1) of Float with Convention => C;

   subtype Aspect_Ratio is Teapot_Math.Aspect_Ratio;
   function Pot_Constants (S : Shading; Aspect : Aspect_Ratio; Seconds : Scene_Time)
     return Push_Constants;

end Teapot_Scene;
