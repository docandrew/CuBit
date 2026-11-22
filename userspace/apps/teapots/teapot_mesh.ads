------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Tessellate Newell's teapot (Teapot_Patches) at startup into a plain
--  triangle list for the GPU. Every patch copy (four quadrants or two
--  mirrors) is evaluated as a bicubic Bezier surface on a Divisions x
--  Divisions grid; normals come from the analytic partial derivatives.
--  Each vertex also carries the patch (U, V) for procedural texturing and
--  a barycentric corner for the wireframe overlay (no geometry shader or
--  non-solid fill mode needed). Both triangles of a grid cell put the
--  cell's diagonal between corners 0 and 2, so the overlay can draw the
--  quad edges without it. Coordinates stay in Newell's frame: Z up.
------------------------------------------------------------------------------
with Teapot_Patches;

package Teapot_Mesh with SPARK_Mode is

   --  Grid cells along each patch edge.
   Divisions : constant := 12;
   Triangles_Per_Patch : constant := Divisions * Divisions * 2;
   Corners : constant := 3;
   Vertex_Count : constant := Teapot_Patches.Surface_Patches * Triangles_Per_Patch * Corners;
   subtype Vertex_Index is Natural range 0 .. Vertex_Count - 1;

   --  The GPU vertex layout: 12 floats, 48 bytes (teapot_vk.c binds the
   --  same offsets).
   type Vertex is record
      PX, PY, PZ : Float := 0.0;   --  position
      NX, NY, NZ : Float := 0.0;   --  unit normal
      U, V : Float := 0.0;         --  patch parameters
      BX, BY, BZ : Float := 0.0;   --  barycentric corner
      Pad : Float := 0.0;
   end record with Convention => C;
   Vertex_Bytes : constant := 48;
   for Vertex'Size use Vertex_Bytes * 8;

   type Vertex_Array is array (Vertex_Index) of Vertex with Convention => C;

   procedure Build (Mesh : out Vertex_Array);

end Teapot_Mesh;
