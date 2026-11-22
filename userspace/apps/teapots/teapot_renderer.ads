------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The gallery's binding to teapot_vk.c: create pipelines and the vertex
--  buffer once, then render one draw list per frame into a lent canvas
--  target. Foreign calls; not SPARK.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with CuBit.UI.GPU_Canvas;
with Teapot_Mesh;
with Teapot_Scene;

package Teapot_Renderer with SPARK_Mode => Off is

   procedure Create (Device : CuBit.UI.GPU_Canvas.Device_View; Format : Unsigned_32;
                     Mesh : Teapot_Mesh.Vertex_Array; OK : out Boolean);

   type Draw is record
      X, Y : Integer_32 := 0;
      Width, Height : Unsigned_32 := 0;
      Shading : Unsigned_32 := 0;
      Reserved : Unsigned_32 := 0;
      Reserved_2 : Unsigned_32 := 0;
      Reserved_3 : Unsigned_32 := 0;
      Push : Teapot_Scene.Push_Constants := [others => 0.0];
   end record with Convention => C;
   type Draw_List is array (Teapot_Scene.Shading) of Draw with Convention => C;

   type Clear_Color is array (0 .. 3) of Float with Convention => C;

   procedure Render (T : CuBit.UI.GPU_Canvas.Target; Draws : Draw_List;
                     Clear : Clear_Color; OK : out Boolean);

   procedure Destroy;

end Teapot_Renderer;
