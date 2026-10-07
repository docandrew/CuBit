with System;
with Compositor_Preview_Geometry;
package Vulkan_Preview_Binding with SPARK_Mode is
   package P renames Compositor_Preview_Geometry;
   type Outcome is (Empty, Recorded, Rejected);
   procedure Draw_Output
     (Borrowed : System.Address; Screen : P.G.Output;
      Bounds : P.G.Logical_Rectangle; Damage : P.G.Physical_Rectangle;
      Source_W, Source_H : P.S.Extent; Mode : P.S.Placement;
      Result : out Outcome) with Global => null;
end Vulkan_Preview_Binding;
