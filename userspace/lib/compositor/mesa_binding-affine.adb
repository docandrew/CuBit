with Mesa_Affine_FFI;
package body Mesa_Binding.Affine with SPARK_Mode => Off is
   procedure Render (Library : in out Context; Target, Source : System.Address;
                     Description : Compositor_Affine.Draw;
                     Width, Height : Compositor_Affine.G.Physical_Extent;
                     Result : out Compositor_Policy.Completion) is
      Value : aliased Compositor_Affine.Draw := Description;
      Code : Compositor_Affine.Word;
   begin
      Code := Mesa_Affine_FFI.Render (Library.Pointer, Target, Source, Value'Access, Width, Height);
      Result := (case Code is
        when 0 => Compositor_Policy.Rendered,
        when 1 => Compositor_Policy.Rejected,
        when 2 => Compositor_Policy.Failed_Quiescent,
        when others => Compositor_Policy.Access_Unknown);
   end Render;
end Mesa_Binding.Affine;
