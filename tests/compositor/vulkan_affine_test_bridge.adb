with Vulkan_Affine_Binding;
package body Vulkan_Affine_Test_Bridge is
   package B renames Vulkan_Affine_Binding;
   package G renames B.G;
   function Draw (Borrowed : System.Address; V : access constant Input)
     return Interfaces.C.int is
      use type Interfaces.C.int;
      Screen : constant G.Output :=
        (G.Physical_Extent (V.W), G.Physical_Extent (V.H), G.Orientation'Val (V.Rotation),
         (G.Scale_Component (V.N), G.Scale_Component (V.D)), G.Output_Origin (V.X), G.Output_Origin (V.Y));
      Result : B.Outcome;
   begin
      pragma Assert (Input'Size = 72 * 8);
      B.Draw_Output
        (Borrowed, Screen,
         (G.Logical_Coordinate (V.L), G.Logical_Coordinate (V.T), G.Logical_Coordinate (V.R), G.Logical_Coordinate (V.B)),
         (G.Pixel_Edge (V.DL), G.Pixel_Edge (V.DT), G.Pixel_Edge (V.DR), G.Pixel_Edge (V.DB)),
         V.Over /= 0, V.Mask /= 0, V.Tint, Result, Straight_Alpha => V.Over = 2);
      return B.Outcome'Pos (Result);
   end Draw;
end Vulkan_Affine_Test_Bridge;
