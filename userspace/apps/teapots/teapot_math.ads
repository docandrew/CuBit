------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Small 3D math for the teapot gallery: vectors, column-major 4x4 matrices
--  in the layout GLSL reads, and Vulkan's clip space (Y down, depth 0..1).
--  Coordinates stay within the gallery's scene, so every operand is bounded
--  (Coordinate); the bounds are what the proof uses to rule out overflow.
------------------------------------------------------------------------------
package Teapot_Math with SPARK_Mode is

   --  Scene coordinates, matrix entries and products stay well inside this.
   Coordinate_Limit : constant := 1.0e4;
   subtype Coordinate is Float range -Coordinate_Limit .. Coordinate_Limit;
   subtype Unit is Float range -1.0 .. 1.0;
   --  Angles the gallery uses: a few hundred turns of animation at most.
   Angle_Limit : constant := 1.0e4;
   subtype Angle is Float range -Angle_Limit .. Angle_Limit;

   Pi : constant := 3.14159_26535_89793;

   function Sqrt (X : Float) return Float
     with Import, Convention => Intrinsic, External_Name => "__builtin_sqrtf",
          Global => null,
          Pre => X >= 0.0 and then X <= 3.0 * Coordinate_Limit * Coordinate_Limit,
          Post => Sqrt'Result >= 0.0 and then Sqrt'Result <= 2.0 * Coordinate_Limit;
   function Sin (X : Float) return Float
     with Import, Convention => Intrinsic, External_Name => "__builtin_sinf",
          Global => null, Pre => X in Angle, Post => Sin'Result in Unit;
   function Cos (X : Float) return Float
     with Import, Convention => Intrinsic, External_Name => "__builtin_cosf",
          Global => null, Pre => X in Angle, Post => Cos'Result in Unit;

   type Vec3 is record
      X, Y, Z : Coordinate := 0.0;
   end record;

   function "+" (A, B : Vec3) return Vec3
     with Pre => abs A.X <= Coordinate_Limit / 2.0 and then abs B.X <= Coordinate_Limit / 2.0 and then
                 abs A.Y <= Coordinate_Limit / 2.0 and then abs B.Y <= Coordinate_Limit / 2.0 and then
                 abs A.Z <= Coordinate_Limit / 2.0 and then abs B.Z <= Coordinate_Limit / 2.0;
   function "-" (A, B : Vec3) return Vec3
     with Pre => abs A.X <= Coordinate_Limit / 2.0 and then abs B.X <= Coordinate_Limit / 2.0 and then
                 abs A.Y <= Coordinate_Limit / 2.0 and then abs B.Y <= Coordinate_Limit / 2.0 and then
                 abs A.Z <= Coordinate_Limit / 2.0 and then abs B.Z <= Coordinate_Limit / 2.0;

   --  Small-magnitude vectors: derivatives and normals of the teapot.
   Small_Limit : constant := 50.0;
   function Is_Small (V : Vec3) return Boolean is
     (abs V.X <= Small_Limit and then abs V.Y <= Small_Limit and then abs V.Z <= Small_Limit);
   function Cross (A, B : Vec3) return Vec3
     with Pre => Is_Small (A) and then Is_Small (B);
   function Dot (A, B : Vec3) return Float
     with Pre => Is_Small (A) and then Is_Small (B);
   function Length_Squared (V : Vec3) return Float
     with Pre => abs V.X <= Coordinate_Limit / 100.0 and then abs V.Y <= Coordinate_Limit / 100.0 and then
                 abs V.Z <= Coordinate_Limit / 100.0,
          Post => Length_Squared'Result >= 0.0;
   --  Unit-length V, or Fallback when V is too short to have a direction.
   function Normalize (V : Vec3; Fallback : Vec3) return Vec3
     with Pre => abs V.X <= Coordinate_Limit / 100.0 and then abs V.Y <= Coordinate_Limit / 100.0 and then
                 abs V.Z <= Coordinate_Limit / 100.0;

   --  Column-major: M (Column, Row), the order GLSL's mat4 reads.
   subtype Axis is Natural range 0 .. 3;
   type Mat4 is array (Axis, Axis) of Float with Convention => C;
   Identity : constant Mat4 :=
     [0 => [1.0, 0.0, 0.0, 0.0], 1 => [0.0, 1.0, 0.0, 0.0],
      2 => [0.0, 0.0, 1.0, 0.0], 3 => [0.0, 0.0, 0.0, 1.0]];

   --  Matrix products are not range-proved: the gallery composes only a
   --  few rotations, translations within Coordinate and one projection.
   function "*" (A, B : Mat4) return Mat4;

   function Rotation_X (Radians : Angle) return Mat4;
   function Rotation_Y (Radians : Angle) return Mat4;
   function Rotation_Z (Radians : Angle) return Mat4;
   function Translation (Offset : Vec3) return Mat4;
   subtype Scale_Factor is Float range 1.0e-3 .. 1.0e3;
   function Scaling (Factor : Scale_Factor) return Mat4;

   --  Right-handed view space looking down -Z, into Vulkan clip space.
   subtype Field_Of_View is Float range 0.1 .. 3.0;   --  vertical, radians
   subtype Aspect_Ratio is Float range 0.05 .. 20.0;
   subtype Depth_Distance is Float range 0.01 .. 1.0e3;
   function Perspective
     (Vertical_FOV : Field_Of_View; Aspect : Aspect_Ratio; Near, Far : Depth_Distance) return Mat4
     with Pre => Far >= Near + 0.1;


end Teapot_Math;
