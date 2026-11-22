with Interfaces;
with CuBit.Display_Geometry;
package Compositor_Affine with SPARK_Mode, Pure is
   package G renames CuBit.Display_Geometry;
   subtype Word is Interfaces.Unsigned_32;
   subtype Signed is Interfaces.Integer_64;
   use type Word, Signed;
   -- Over: 0 replaces, 1 premultiplied source-over, 2 straight source-over.
   type Draw is record
      Origin_X, Origin_Y : Signed := 0;
      Logical_W, Logical_H, Numerator, Denominator, Rotation : Word := 0;
      Clip_X, Clip_Y, Clip_W, Clip_H, Over : Word := 0;
   end record with Convention => C;
   function Valid (D : Draw; Width, Height : G.Physical_Extent) return Boolean is
     (D.Origin_X in -(2 ** 31) .. 2 ** 31 and then
      D.Origin_Y in -(2 ** 31) .. 2 ** 31 and then
      D.Logical_W in 1 .. 2 ** 31 and then D.Logical_H in 1 .. 2 ** 31 and then
      D.Numerator in 1 .. 16 and then D.Denominator in 1 .. 16 and then
      D.Rotation <= 3 and then D.Over <= 2 and then
      Signed (D.Clip_X) < Signed (Width) and then Signed (D.Clip_Y) < Signed (Height) and then
      Signed (D.Clip_W) in 1 .. Signed (Width) - Signed (D.Clip_X) and then
      Signed (D.Clip_H) in 1 .. Signed (Height) - Signed (D.Clip_Y));
   type Result (Visible : Boolean := False) is record
      case Visible is
         when True => Value : Draw;
         when False => null;
      end case;
   end record;
   function Plan (Screen : G.Output; Surface : G.Logical_Rectangle;
                  Over : Boolean := False; Straight_Alpha : Boolean := False) return Result
     with Post => (if Plan'Result.Visible then
       Valid (Plan'Result.Value, Screen.Width, Screen.Height) and then
       Signed (Plan'Result.Value.Logical_W) =
         Signed (Surface.Right) - Signed (Surface.Left) and then
       Signed (Plan'Result.Value.Logical_H) =
         Signed (Surface.Bottom) - Signed (Surface.Top));
   function Same_Transform (L, R : Draw) return Boolean is
     (L.Origin_X = R.Origin_X and L.Origin_Y = R.Origin_Y and
      L.Logical_W = R.Logical_W and L.Logical_H = R.Logical_H and
      L.Numerator = R.Numerator and L.Denominator = R.Denominator and
      L.Rotation = R.Rotation and L.Over = R.Over);
   function Clip (D : Draw; Width, Height : G.Physical_Extent;
                  Area : G.Physical_Rectangle) return Result
     with Pre => Valid (D, Width, Height),
       Post => Clip'Result.Visible =
         (Signed'Max (Signed (D.Clip_X), Signed (Area.Left)) <
            Signed'Min (Signed (D.Clip_X) + Signed (D.Clip_W), Signed (Area.Right)) and
          Signed'Max (Signed (D.Clip_Y), Signed (Area.Top)) <
            Signed'Min (Signed (D.Clip_Y) + Signed (D.Clip_H), Signed (Area.Bottom))) and then
         (if Clip'Result.Visible then
            Valid (Clip'Result.Value, Width, Height) and
            Same_Transform (D, Clip'Result.Value) and
            Signed (Clip'Result.Value.Clip_X) = Signed'Max (Signed (D.Clip_X), Signed (Area.Left)) and
            Signed (Clip'Result.Value.Clip_Y) = Signed'Max (Signed (D.Clip_Y), Signed (Area.Top)) and
            Signed (Clip'Result.Value.Clip_X) + Signed (Clip'Result.Value.Clip_W) =
              Signed'Min (Signed (D.Clip_X) + Signed (D.Clip_W), Signed (Area.Right)) and
            Signed (Clip'Result.Value.Clip_Y) + Signed (Clip'Result.Value.Clip_H) =
              Signed'Min (Signed (D.Clip_Y) + Signed (D.Clip_H), Signed (Area.Bottom)));
end Compositor_Affine;
