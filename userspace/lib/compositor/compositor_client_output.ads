with Desktop_Composition;
with CuBit.Display_Geometry;
package Compositor_Client_Output with SPARK_Mode, Pure is
   package G renames CuBit.Display_Geometry;
   use type G.Logical_Coordinate, G.Orientation, G.Scale_Component;
   function Eligible
     (Screen : G.Output; Target_W, Target_H, Source_W, Source_H, X, Y : Natural;
      Blit : Desktop_Composition.Blit_Plan) return Boolean is
     (Screen.X = 0 and then Screen.Y = 0 and then Screen.Rotation = G.Unrotated and then
      Screen.Scale.Numerator = Screen.Scale.Denominator and then
      Target_W = Natural (Screen.Width) and then Target_H = Natural (Screen.Height) and then
      Source_W in 1 .. 65_535 and then Source_H in 1 .. 65_535 and then
      X < Target_W and then Y < Target_H and then
      Blit.Target_X in X .. Target_W and then Blit.Target_Y in Y .. Target_H and then
      Blit.Source_X = Blit.Target_X - X and then Blit.Source_Y = Blit.Target_Y - Y and then
      Blit.Source_X <= Source_W and then Blit.Source_Y <= Source_H and then
      Blit.Width in 1 .. Target_W - Blit.Target_X and then
      Blit.Height in 1 .. Target_H - Blit.Target_Y and then
      Blit.Width <= Source_W - Blit.Source_X and then Blit.Height <= Source_H - Blit.Source_Y);
   type Result (Valid : Boolean := False) is record
      case Valid is
         when True =>
            Surface : G.Logical_Rectangle;
            Damage : G.Physical_Rectangle;
         when False => null;
      end case;
   end record;
   -- Bridge the existing unscaled blit to a physical-output draw without
   -- stretching old client storage after a window resize. Reject malformed
   -- geometry rather than perform unchecked conversions in the service.
   function Plan
     (Screen : G.Output; Target_W, Target_H, Source_W, Source_H, X, Y : Natural;
      Blit : Desktop_Composition.Blit_Plan) return Result
     with Post => Plan'Result.Valid = Eligible
       (Screen, Target_W, Target_H, Source_W, Source_H, X, Y, Blit) and then
       (if Plan'Result.Valid then
          Plan'Result.Surface.Left = G.Logical_Coordinate (X) and
          Plan'Result.Surface.Top = G.Logical_Coordinate (Y) and
          Plan'Result.Surface.Right = G.Logical_Coordinate (X + Source_W) and
          Plan'Result.Surface.Bottom = G.Logical_Coordinate (Y + Source_H) and
          Natural (Plan'Result.Damage.Left) = Blit.Target_X and
          Natural (Plan'Result.Damage.Top) = Blit.Target_Y and
          Natural (Plan'Result.Damage.Right) = Blit.Target_X + Blit.Width and
          Natural (Plan'Result.Damage.Bottom) = Blit.Target_Y + Blit.Height);
end Compositor_Client_Output;
