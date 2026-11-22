with Compositor_Transform;
package body Mesa_Affine_FFI with SPARK_Mode => Off is
   function Submit (Context, Target, Source : System.Address;
                    Value : access constant Compositor_Affine.Draw;
                    Corners : access constant Compositor_Transform.Quad)
     return Compositor_Affine.Word
     with Import, Convention => C, External_Name => "cubit_mesa_draw_affine";
   function Render (Context, Target, Source : System.Address;
                    Value : access constant Compositor_Affine.Draw;
                    Width, Height : Compositor_Affine.G.Physical_Extent)
     return Compositor_Affine.Word is
   begin
      if Value = null or else not Compositor_Affine.Valid (Value.all, Width, Height) then
         return 1;
      end if;
      declare
         Corners : aliased constant Compositor_Transform.Quad :=
           Compositor_Transform.Vertices (Value.all, Width, Height);
      begin
         return Submit (Context, Target, Source, Value, Corners'Access);
      end;
   end Render;
end Mesa_Affine_FFI;
