with Compositor_Source_Region;
-- A source window shares the whole source ticket's lifetime. Empty destination
-- clips record nothing; invalid windows reject even for an invisible draw.
package Vulkan_Affine_Binding.Regions with SPARK_Mode is
   procedure Draw_Output (Borrowed : System.Address; Screen : G.Output;
      Surface : G.Logical_Rectangle; Damage : G.Physical_Rectangle;
      Region : Compositor_Source_Region.Rectangle; Over, Straight_Alpha : Boolean;
      Result : out Outcome)
     with Global => null;
end Vulkan_Affine_Binding.Regions;
