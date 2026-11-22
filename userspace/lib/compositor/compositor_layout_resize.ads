pragma Ada_2022;
with CuBit.Display_Layouts;
--  Outputs changed mode (a resized virtual machine window, a new monitor
--  mode): carry each output's UI scale and rotation over from the layout
--  in use, so a resize never silently resets the user's scale.
--
--  Fresh lists the outputs at their new modes in output order (as Desktop
--  builds it). Outputs keep their order and are packed left to right along
--  the top edge with no gap. Each output takes the scale and rotation of
--  the saved output with the same display name at the same position; a
--  saved scale that would leave less than the minimum logical extent, or a
--  packing the layout rules reject, falls back to Fresh unchanged.
package Compositor_Layout_Resize with SPARK_Mode, Pure is
   package L renames CuBit.Display_Layouts;
   package G renames L.G;
   use type L.Admission_Status, L.Named_Display_ID, L.Layout;
   use type G.Pixel_Edge, G.Logical_Coordinate, G.Scale_Component, G.Orientation,
     G.UI_Scale;

   --  Logical extent of one axis: native pixels over the scale, rounded up
   --  (CuBit.Display_Geometry.Bounds).
   Largest_Logical : constant := 65_535 * 16;
   function Logical_Extent (Native : G.Physical_Extent; Scale : G.UI_Scale)
     return Positive is
     ((Natural (Native) * Natural (Scale.Denominator) +
         Natural (Scale.Numerator) - 1) / Natural (Scale.Numerator))
     with Post => Logical_Extent'Result <= Largest_Logical;

   function Upright_Width (Screen : G.Output) return G.Physical_Extent is
     (if Screen.Rotation in G.Clockwise_90 | G.Clockwise_270
      then Screen.Height else Screen.Width);
   function Upright_Height (Screen : G.Output) return G.Physical_Extent is
     (if Screen.Rotation in G.Clockwise_90 | G.Clockwise_270
      then Screen.Width else Screen.Height);

   --  Minimum logical extent a carried scale may leave (Desktop's
   --  arrangement minimum).
   type Minimum_Extent is record
      Width, Height : Positive;
   end record;

   function Carries (Saved, Fresh : L.Layout; I : L.Viewport_Index) return Boolean is
     (I <= Saved.Count and then I <= Fresh.Count and then
      Saved.Items (I).Display = Fresh.Items (I).Display)
     with Pre => Fresh.Count <= L.Max_Viewports;

   function Carry_Settings
     (Saved, Fresh : L.Layout; Minimum : Minimum_Extent) return L.Layout
     with Post =>
       Carry_Settings'Result = Fresh or else
       (Carry_Settings'Result.Count = Fresh.Count and then
        L.Validate (Carry_Settings'Result).Status = L.Accepted and then
        (for all I in 1 .. Fresh.Count =>
           Carry_Settings'Result.Items (I).Display = Fresh.Items (I).Display and then
           Carry_Settings'Result.Items (I).Geometry.Width =
             Fresh.Items (I).Geometry.Width and then
           Carry_Settings'Result.Items (I).Geometry.Height =
             Fresh.Items (I).Geometry.Height and then
           (if Carries (Saved, Fresh, I) then
              Carry_Settings'Result.Items (I).Geometry.Scale =
                Saved.Items (I).Geometry.Scale and then
              Carry_Settings'Result.Items (I).Geometry.Rotation =
                Saved.Items (I).Geometry.Rotation)));
end Compositor_Layout_Resize;
