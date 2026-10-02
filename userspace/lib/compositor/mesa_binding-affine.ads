with Compositor_Affine;
package Mesa_Binding.Affine with SPARK_Mode is
   procedure Render (Library : in out Context; Target, Source : System.Address;
                     Description : Compositor_Affine.Draw;
                     Width, Height : Compositor_Affine.G.Physical_Extent;
                     Result : out Compositor_Policy.Completion) with Global => null;
end Mesa_Binding.Affine;
