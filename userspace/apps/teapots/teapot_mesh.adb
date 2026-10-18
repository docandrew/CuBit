with Teapot_Math; use Teapot_Math;

package body Teapot_Mesh with SPARK_Mode is

   package P renames Teapot_Patches;

   subtype Parameter is Float range 0.0 .. 1.0;

   --  How a patch copy maps Newell's quadrant: a quarter turn about Z
   --  (Quarter_Turns) and an optional mirror across Y = 0.
   subtype Quarter is Natural range 0 .. P.Quadrants - 1;
   type Copy_Transform is record
      Quarter_Turns : Quarter := 0;
      Mirror : Boolean := False;
   end record;

   function Place (Point : P.Point; Copy : Copy_Transform) return Vec3 is
      X : constant Float := Point.X;
      Y : constant Float := (if Copy.Mirror then -Point.Y else Point.Y);
   begin
      case Copy.Quarter_Turns is
         when 0 => return (X, Y, Point.Z);
         when 1 => return (-Y, X, Point.Z);
         when 2 => return (-X, -Y, Point.Z);
         when 3 => return (Y, -X, Point.Z);
      end case;
   end Place;

   type Basis is array (P.Control_Index) of Float;

   --  Cubic Bernstein weights and their derivatives at T.
   function Weights (T : Parameter) return Basis is
     [(1.0 - T) * (1.0 - T) * (1.0 - T), 3.0 * T * (1.0 - T) * (1.0 - T),
      3.0 * T * T * (1.0 - T), T * T * T];
   function Slopes (T : Parameter) return Basis is
     [-3.0 * (1.0 - T) * (1.0 - T), 3.0 * (1.0 - T) * (1.0 - 3.0 * T),
      3.0 * T * (2.0 - 3.0 * T), 3.0 * T * T];

   type Sample is record
      Position, Du, Dv : Vec3;
   end record;

   function Evaluate (Patch : P.Patch_Index; Copy : Copy_Transform; U, V : Parameter) return Sample is
      WU : constant Basis := Weights (U);
      WV : constant Basis := Weights (V);
      SU : constant Basis := Slopes (U);
      SV : constant Basis := Slopes (V);
      Result : Sample := ((0.0, 0.0, 0.0), (0.0, 0.0, 0.0), (0.0, 0.0, 0.0));
      Point : Vec3;
   begin
      --  Row I runs along V, column J along U (Newell's ordering).
      for I in P.Control_Index loop
         for J in P.Control_Index loop
            Point := Place (P.Points (P.Patches (Patch) (I, J)), Copy);
            Result.Position :=
              (Result.Position.X + WV (I) * WU (J) * Point.X,
               Result.Position.Y + WV (I) * WU (J) * Point.Y,
               Result.Position.Z + WV (I) * WU (J) * Point.Z);
            Result.Du :=
              (Result.Du.X + WV (I) * SU (J) * Point.X,
               Result.Du.Y + WV (I) * SU (J) * Point.Y,
               Result.Du.Z + WV (I) * SU (J) * Point.Z);
            Result.Dv :=
              (Result.Dv.X + SV (I) * WU (J) * Point.X,
               Result.Dv.Y + SV (I) * WU (J) * Point.Y,
               Result.Dv.Z + SV (I) * WU (J) * Point.Z);
         end loop;
      end loop;
      return Result;
   end Evaluate;

   --  Unit direction of V, robust for tiny vectors (degenerate patch corners
   --  such as the lid apex): scale by the largest component first.
   Tiny : constant := 1.0e-20;
   Up : constant Vec3 := (0.0, 0.0, 1.0);
   procedure Direction (V : Vec3; Result : out Vec3; Found : out Boolean) is
      Largest : constant Float := Float'Max (abs V.X, Float'Max (abs V.Y, abs V.Z));
   begin
      Found := Largest > Tiny;
      Result := (if Found then Normalize ((V.X / Largest, V.Y / Largest, V.Z / Largest), Up)
                 else Up);
   end Direction;

   --  Step toward the patch centre when a corner is degenerate.
   Nudge : constant := 0.02;
   Centre : constant := 0.5;

   function Normal (Patch : P.Patch_Index; Copy : Copy_Transform; U, V : Parameter) return Vec3 is
      S : Sample := Evaluate (Patch, Copy, U, V);
      Found : Boolean;
      N : Vec3;
   begin
      Direction (Cross (S.Du, S.Dv), N, Found);
      if not Found then
         S := Evaluate (Patch, Copy, U + (Centre - U) * Nudge, V + (Centre - V) * Nudge);
         Direction (Cross (S.Du, S.Dv), N, Found);
      end if;
      --  A mirror reverses the patch's orientation.
      if Copy.Mirror then
         N := (-N.X, -N.Y, -N.Z);
      end if;
      return N;
   end Normal;

   procedure Build (Mesh : out Vertex_Array) is
      Next : Natural := 0;
      Step : constant Float := 1.0 / Float (Divisions);

      procedure Emit (Patch : P.Patch_Index; Copy : Copy_Transform;
                      U, V : Parameter; Corner : Natural) is
         S : constant Sample := Evaluate (Patch, Copy, U, V);
         N : constant Vec3 := Normal (Patch, Copy, U, V);
      begin
         if Next <= Vertex_Index'Last then
            Mesh (Next) :=
              (PX => S.Position.X, PY => S.Position.Y, PZ => S.Position.Z,
               NX => N.X, NY => N.Y, NZ => N.Z, U => U, V => V,
               BX => (if Corner = 0 then 1.0 else 0.0),
               BY => (if Corner = 1 then 1.0 else 0.0),
               BZ => (if Corner = 2 then 1.0 else 0.0), Pad => 0.0);
            Next := Next + 1;
         end if;
      end Emit;

      procedure Patch_Grid (Patch : P.Patch_Index; Copy : Copy_Transform) is
         U0, U1, V0, V1 : Parameter;
      begin
         for Row in 0 .. Divisions - 1 loop
            for Column in 0 .. Divisions - 1 loop
               U0 := Float (Column) * Step;
               U1 := (if Column = Divisions - 1 then 1.0 else Float (Column + 1) * Step);
               V0 := Float (Row) * Step;
               V1 := (if Row = Divisions - 1 then 1.0 else Float (Row + 1) * Step);
               Emit (Patch, Copy, U0, V0, 0);
               Emit (Patch, Copy, U1, V0, 1);
               Emit (Patch, Copy, U1, V1, 2);
               Emit (Patch, Copy, U0, V0, 0);
               Emit (Patch, Copy, U0, V1, 1);
               Emit (Patch, Copy, U1, V1, 2);
            end loop;
         end loop;
      end Patch_Grid;
   begin
      Mesh := [others => <>];
      for Patch in P.Patch_Index loop
         if Patch in P.Rotated_Patch then
            for Turn in Quarter loop
               Patch_Grid (Patch, (Quarter_Turns => Turn, Mirror => False));
            end loop;
         else
            Patch_Grid (Patch, (Quarter_Turns => 0, Mirror => False));
            Patch_Grid (Patch, (Quarter_Turns => 0, Mirror => True));
         end if;
      end loop;
   end Build;

end Teapot_Mesh;
