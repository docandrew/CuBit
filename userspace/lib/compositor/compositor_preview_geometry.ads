with Compositor_Affine;
with Compositor_Image_Sampling;
-- Geometry for an endpoint-bilinear image inside a logical UI rectangle.
-- No pixel backing, allocation, or import authority. This is not a C ABI.
package Compositor_Preview_Geometry with SPARK_Mode, Pure is
   package A renames Compositor_Affine;
   package G renames A.G;
   package S renames Compositor_Image_Sampling;
   type Result (Visible : Boolean := False) is record
      case Visible is
         when False => null;
         when True =>
            Transform : A.Draw;
            Placement : S.Layout;
      end case;
   end record;
   function Plan
     (Screen : G.Output; Bounds : G.Logical_Rectangle;
      Damage : G.Physical_Rectangle; Source_W, Source_H : S.Extent;
      Mode : S.Placement) return Result
     with Post => (if Plan'Result.Visible then
       A.Valid (Plan'Result.Transform, Screen.Width, Screen.Height) and then
       Plan'Result.Transform.Logical_W in 1 .. 65_535 and then
       Plan'Result.Transform.Logical_H in 1 .. 65_535 and then
       S.Source_Width (Plan'Result.Placement) = Source_W and then
       S.Source_Height (Plan'Result.Placement) = Source_H);
end Compositor_Preview_Geometry;
