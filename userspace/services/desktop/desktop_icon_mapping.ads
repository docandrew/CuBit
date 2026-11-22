with System;
with Desktop_Icon_Pixels;
with Compositor_Upload;
-- Trusted mapping boundary. Caller supplies exclusive writable memory covering
-- Bytes, disjoint from immutable icon assets. No pointer survives this call.
package Desktop_Icon_Mapping with SPARK_Mode is
   procedure Copy_Chunk (Item : Desktop_Icon_Pixels.Asset;
      Mapping : System.Address; Bytes : Compositor_Upload.Byte_Count;
      Plan : Compositor_Upload.Plan; Complete : out Boolean)
     with Global => null;
   procedure Copy_Atlas (Kind : Desktop_Icon_Pixels.Family;
      Mapping : System.Address; Bytes : Compositor_Upload.Byte_Count;
      Plan : Compositor_Upload.Plan; Complete : out Boolean)
     with Global => null;
end Desktop_Icon_Mapping;
