with Teapot_Math; use Teapot_Math;

package body Teapot_Scene with SPARK_Mode is

   function Label (S : Shading) return String is
     (case S is
        when Flat => "Flat",
        when Gouraud => "Gouraud",
        when Phong => "Phong",
        when Blinn_Phong => "Blinn-Phong",
        when Toon => "Toon",
        when Wireframe => "Wireframe overlay",
        when Normals => "Normals",
        when Environment => "Environment map",
        when Procedural => "Procedural texture");

   function Cell_Of (S : Shading; Width, Height, Gap : Edge) return Cell is
      Index : constant Natural := Shading'Pos (S);
      Column : constant Natural := Index mod Columns;
      Row : constant Natural := Index / Columns;
      Cell_W, Cell_H : Natural;
   begin
      if Width <= Gap * (Columns + 1) or else Height <= Gap * (Rows + 1) then
         return (others => 0);
      end if;
      Cell_W := (Width - Gap * (Columns + 1)) / Columns;
      Cell_H := (Height - Gap * (Rows + 1)) / Rows;
      return (X => Gap + Column * (Cell_W + Gap), Y => Gap + Row * (Cell_H + Gap),
              Width => Cell_W, Height => Cell_H);
   end Cell_Of;

   --  Wrap's input bound: decades of uptime.
   Maximum_Seconds : constant := 1.0e9;

   function Wrap (Seconds : Float) return Scene_Time is
      Loops : Natural;
      Rest : Float;
   begin
      if not (Seconds >= 0.0) or else Seconds > Maximum_Seconds then
         return 0.0;
      end if;
      --  The conversion rounds to nearest (no Float'Floor in the user
      --  runtime): step back one loop if it rounded up.
      Loops := Natural (Seconds / Loop_Seconds);
      Rest := Seconds - Float (Loops) * Loop_Seconds;
      if Rest < 0.0 then
         Rest := Rest + Loop_Seconds;
      end if;
      return Scene_Time'Max (0.0, Scene_Time'Min (Loop_Seconds, Rest));
   end Wrap;

   --  Newell's teapot: Z up, base at Z = 0, lid knob at 3.15, from the
   --  handle (X = -3) to the spout tip (X = 3.525). Centre it and turn Y up.
   Teapot_Centre : constant Vec3 := (0.26, 0.0, 1.575);
   Turn_Upright : constant Float := -Pi / 2.0;

   --  One spin every Spin_Seconds and a slow nod every Nod_Seconds; both
   --  divide Loop_Seconds, so the loop is seamless.
   Spin_Seconds : constant := 20.0;
   Nod_Seconds : constant := 12.0;
   Nod_Radians : constant := 0.25;
   --  Pots start a little apart in phase so the grid is not in lock step.
   Phase_Step : constant := 0.7;

   Camera_Distance : constant := 9.5;
   Camera_Lift : constant := 0.35;      --  radians looking down
   Field_Of_View_Radians : constant := 0.72;
   Near_Plane : constant := 1.0;
   Far_Plane : constant := 30.0;

   --  Push block layout (teapot.vert): mat4 mvp, vec4 mv0..mv2, vec4 params.
   Vector_Floats : constant := 4;
   MVP_First : constant := 0;
   Model_View_First : constant := MVP_First + Vector_Floats * Vector_Floats;
   Model_View_Rows : constant := 3;
   Params_First : constant := Model_View_First + Model_View_Rows * Vector_Floats;
   pragma Compile_Time_Error (Params_First + Vector_Floats /= Push_Floats, "push block layout");

   function Pot_Constants (S : Shading; Aspect : Aspect_Ratio; Seconds : Scene_Time)
     return Push_Constants
   is
      Phase : constant Float := Float (Shading'Pos (S)) * Phase_Step;
      Spin : constant Float := 2.0 * Pi * Seconds / Spin_Seconds + Phase;
      Nod : constant Float := Nod_Radians * Sin (2.0 * Pi * Seconds / Nod_Seconds + Phase);
      Model : constant Mat4 :=
        Rotation_X (Nod) * Rotation_Y (Spin) * Rotation_X (Turn_Upright) *
        Translation ((-Teapot_Centre.X, -Teapot_Centre.Y, -Teapot_Centre.Z));
      View : constant Mat4 :=
        Translation ((0.0, 0.0, -Camera_Distance)) * Rotation_X (Camera_Lift);
      Model_View : constant Mat4 := View * Model;
      Projection : constant Mat4 :=
        Perspective (Field_Of_View_Radians, Aspect, Near_Plane, Far_Plane);
      MVP : constant Mat4 := Projection * Model_View;
      Result : Push_Constants := [others => 0.0];
   begin
      for C in Axis loop
         for R in Axis loop
            Result (MVP_First + C * Vector_Floats + R) := MVP (C, R);
         end loop;
      end loop;
      --  Rows of the model-view matrix (columns of its transpose).
      for R in 0 .. Model_View_Rows - 1 loop
         for C in Axis loop
            Result (Model_View_First + R * Vector_Floats + C) := Model_View (C, R);
         end loop;
      end loop;
      Result (Params_First) := Seconds;
      return Result;
   end Pot_Constants;

end Teapot_Scene;
