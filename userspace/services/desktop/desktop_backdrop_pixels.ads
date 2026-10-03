with System;
with Compositor_Upload;
with CuBit.Appearance;
-- Narrow pointer boundary: caller supplies an exclusive writable staging
-- mapping covering Bytes, disjoint from both immutable embedded assets.
-- The synchronous call never retains Mapping. Padding remains untouched.
package Desktop_Backdrop_Pixels with SPARK_Mode is
   procedure Copy_Chunk
     (Asset : CuBit.Appearance.Background; Mapping : System.Address;
      Bytes : Compositor_Upload.Byte_Count; Plan : Compositor_Upload.Plan;
      Complete : out Boolean)
     with Global => null;
end Desktop_Backdrop_Pixels;
