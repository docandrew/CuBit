package body Teapot_Math with SPARK_Mode is

   --  Shorter vectors have no usable direction (Normalize's Fallback).
   Direction_Epsilon : constant := 1.0e-2;

   function "+" (A, B : Vec3) return Vec3 is ((A.X + B.X, A.Y + B.Y, A.Z + B.Z));
   function "-" (A, B : Vec3) return Vec3 is ((A.X - B.X, A.Y - B.Y, A.Z - B.Z));

   function Cross (A, B : Vec3) return Vec3 is
     ((A.Y * B.Z - A.Z * B.Y, A.Z * B.X - A.X * B.Z, A.X * B.Y - A.Y * B.X));

   function Dot (A, B : Vec3) return Float is (A.X * B.X + A.Y * B.Y + A.Z * B.Z);

   function Length_Squared (V : Vec3) return Float is (V.X * V.X + V.Y * V.Y + V.Z * V.Z);

   function Normalize (V : Vec3; Fallback : Vec3) return Vec3 is
      Length : constant Float := Sqrt (Length_Squared (V));
   begin
      if Length < Direction_Epsilon then
         return Fallback;
      end if;
      pragma Assert (Length >= Direction_Epsilon);
      return (V.X / Length, V.Y / Length, V.Z / Length);
   end Normalize;

   function "*" (A, B : Mat4) return Mat4 is
      Result : Mat4 := [others => [others => 0.0]];
   begin
      for C in Axis loop
         for R in Axis loop
            Result (C, R) := A (0, R) * B (C, 0) + A (1, R) * B (C, 1) +
                             A (2, R) * B (C, 2) + A (3, R) * B (C, 3);
         end loop;
      end loop;
      return Result;
   end "*";

   function Rotation_X (Radians : Angle) return Mat4 is
      S : constant Unit := Sin (Radians);
      C : constant Unit := Cos (Radians);
   begin
      return [0 => [1.0, 0.0, 0.0, 0.0], 1 => [0.0, C, S, 0.0],
              2 => [0.0, -S, C, 0.0], 3 => [0.0, 0.0, 0.0, 1.0]];
   end Rotation_X;

   function Rotation_Y (Radians : Angle) return Mat4 is
      S : constant Unit := Sin (Radians);
      C : constant Unit := Cos (Radians);
   begin
      return [0 => [C, 0.0, -S, 0.0], 1 => [0.0, 1.0, 0.0, 0.0],
              2 => [S, 0.0, C, 0.0], 3 => [0.0, 0.0, 0.0, 1.0]];
   end Rotation_Y;

   function Rotation_Z (Radians : Angle) return Mat4 is
      S : constant Unit := Sin (Radians);
      C : constant Unit := Cos (Radians);
   begin
      return [0 => [C, S, 0.0, 0.0], 1 => [-S, C, 0.0, 0.0],
              2 => [0.0, 0.0, 1.0, 0.0], 3 => [0.0, 0.0, 0.0, 1.0]];
   end Rotation_Z;

   function Translation (Offset : Vec3) return Mat4 is
     ([0 => [1.0, 0.0, 0.0, 0.0], 1 => [0.0, 1.0, 0.0, 0.0],
       2 => [0.0, 0.0, 1.0, 0.0], 3 => [Offset.X, Offset.Y, Offset.Z, 1.0]]);

   function Scaling (Factor : Scale_Factor) return Mat4 is
     ([0 => [Factor, 0.0, 0.0, 0.0], 1 => [0.0, Factor, 0.0, 0.0],
       2 => [0.0, 0.0, Factor, 0.0], 3 => [0.0, 0.0, 0.0, 1.0]]);

   function Perspective
     (Vertical_FOV : Field_Of_View; Aspect : Aspect_Ratio; Near, Far : Depth_Distance) return Mat4
   is
      Half : constant Float := Vertical_FOV / 2.0;
      --  cot (Half) = cos / sin; Half in 0.05 .. 1.5 keeps sin well above 0.
      Sine : constant Float := Sin (Half);
      Focal : constant Float := Cos (Half) / Float'Max (Sine, Direction_Epsilon);
      Depth : constant Float := Far - Near;
   begin
      --  Vulkan: Y points down in clip space, depth maps Near..Far to 0..1.
      return [0 => [Focal / Aspect, 0.0, 0.0, 0.0],
              1 => [0.0, -Focal, 0.0, 0.0],
              2 => [0.0, 0.0, Far / (Near - Far), -1.0],
              3 => [0.0, 0.0, -(Far * Near) / Depth, 0.0]];
   end Perspective;


end Teapot_Math;
