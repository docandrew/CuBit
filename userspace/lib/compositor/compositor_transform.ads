with Compositor_Affine;
package Compositor_Transform with SPARK_Mode, Pure is
   package A renames Compositor_Affine;
   package G renames A.G;
   subtype Signed is A.Signed;
   use type Signed, A.Word;
   subtype Base is Signed range -(2 ** 36) .. 2 ** 36;
   subtype Step is Signed range -16 .. 16;
   subtype Divisor is Signed range 1 .. 2 ** 35;
   -- Normalized source coordinates at an output edge (x,y):
   -- U=(U0+UX*x+UY*y)/UD, V=(V0+VX*x+VY*y)/VD.
   -- Pixel centres use x+1/2,y+1/2. These are exact rationals;
   -- the foreign boundary performs only the final floating conversion.
   type Coefficients is record
      U0 : Base; UX, UY : Step;
      V0 : Base; VX, VY : Step;
      UD, VD : Divisor;
   end record with Convention => C;
   function Build (D : A.Draw; Width, Height : G.Physical_Extent)
     return Coefficients
     with Pre => A.Valid (D, Width, Height),
       Post =>
         Build'Result.UD = Signed (D.Numerator) * Signed (D.Logical_W) and
         Build'Result.VD = Signed (D.Numerator) * Signed (D.Logical_H) and
         (case D.Rotation is
            when 0 =>
              Build'Result.U0 = D.Origin_X * Signed (D.Numerator) and
              Build'Result.V0 = D.Origin_Y * Signed (D.Numerator) and
              Build'Result.UX = Signed (D.Denominator) and Build'Result.UY = 0 and
              Build'Result.VX = 0 and Build'Result.VY = Signed (D.Denominator),
            when 1 =>
              Build'Result.U0 = D.Origin_X * Signed (D.Numerator) and
              Build'Result.V0 = D.Origin_Y * Signed (D.Numerator) + Signed (Width) * Signed (D.Denominator) and
              Build'Result.UX = 0 and Build'Result.UY = Signed (D.Denominator) and
              Build'Result.VX = -Signed (D.Denominator) and Build'Result.VY = 0,
            when 2 =>
              Build'Result.U0 = D.Origin_X * Signed (D.Numerator) + Signed (Width) * Signed (D.Denominator) and
              Build'Result.V0 = D.Origin_Y * Signed (D.Numerator) + Signed (Height) * Signed (D.Denominator) and
              Build'Result.UX = -Signed (D.Denominator) and Build'Result.UY = 0 and
              Build'Result.VX = 0 and Build'Result.VY = -Signed (D.Denominator),
            when others =>
              Build'Result.U0 = D.Origin_X * Signed (D.Numerator) + Signed (Height) * Signed (D.Denominator) and
              Build'Result.V0 = D.Origin_Y * Signed (D.Numerator) and
              Build'Result.UX = 0 and Build'Result.UY = -Signed (D.Denominator) and
              Build'Result.VX = Signed (D.Denominator) and Build'Result.VY = 0);
   type UV is record
      U, V : Signed;
   end record with Convention => C;
   type Corner_Array is array (0 .. 3) of UV with Convention => C;
   type Quad is record
      Corners : Corner_Array;
      UD, VD : Divisor;
      Width, Height : A.Word;
   end record with Convention => C;
   function Vertices (D : A.Draw; Width, Height : G.Physical_Extent) return Quad
     with Pre => A.Valid (D, Width, Height),
       Post => Vertices'Result.Width = A.Word (Width) and
         Vertices'Result.Height = A.Word (Height) and
         Vertices'Result.UD = Build (D, Width, Height).UD and
         Vertices'Result.VD = Build (D, Width, Height).VD and
         (for all I in 0 .. 3 =>
           Vertices'Result.Corners (I).U = Build (D, Width, Height).U0 +
             Build (D, Width, Height).UX * (if I in 1 .. 2 then Signed (Width) else 0) +
             Build (D, Width, Height).UY * (if I in 2 .. 3 then Signed (Height) else 0) and
           Vertices'Result.Corners (I).V = Build (D, Width, Height).V0 +
             Build (D, Width, Height).VX * (if I in 1 .. 2 then Signed (Width) else 0) +
             Build (D, Width, Height).VY * (if I in 2 .. 3 then Signed (Height) else 0));
end Compositor_Transform;
