with CuBit.Display_Layouts;
package Compositor_Layout_Restore with SPARK_Mode, Pure is
   package L renames CuBit.Display_Layouts;
   use type L.Admission_Status, L.Named_Display_ID;
   use type L.G.Orientation, L.G.Scale_Component;
   use type L.G.Pixel_Edge, L.G.Logical_Coordinate, L.Layout;
   -- Viewport names are not import/scanout authority. This only preserves
   -- logical desktop geometry when freshly discovered physical modes match.
   function Compatible (Saved, Fresh : L.Layout) return Boolean is
     (Saved.Count > 0 and then Saved.Count = Fresh.Count and then
      L.Validate (Saved).Status = L.Accepted and then
      (for all I in 1 .. Saved.Count =>
        Saved.Items (I).Display = Fresh.Items (I).Display and
        Saved.Items (I).Geometry.Width = Fresh.Items (I).Geometry.Width and
        Saved.Items (I).Geometry.Height = Fresh.Items (I).Geometry.Height and
        Saved.Items (I).Geometry.Rotation = Fresh.Items (I).Geometry.Rotation and
        Saved.Items (I).Geometry.X >= 0 and Saved.Items (I).Geometry.Y >= 0 and
        Saved.Items (I).Geometry.Scale.Numerator >= Saved.Items (I).Geometry.Scale.Denominator));
   function Choose (Saved, Fresh : L.Layout) return L.Layout
     with Post => Choose'Result = (if Compatible (Saved, Fresh) then Saved else Fresh);
end Compositor_Layout_Restore;
