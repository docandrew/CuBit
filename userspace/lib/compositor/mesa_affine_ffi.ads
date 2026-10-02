with System;
with Compositor_Affine;
-- Trusted affine extension. Same retained images/context and completion codes
-- as Mesa_FFI; floating interpolation is native-tested, not SPARK-proved.
package Mesa_Affine_FFI with SPARK_Mode => Off is
   function Render (Context, Target, Source : System.Address;
                    Value : access constant Compositor_Affine.Draw;
                    Width, Height : Compositor_Affine.G.Physical_Extent)
     return Compositor_Affine.Word;
end Mesa_Affine_FFI;
