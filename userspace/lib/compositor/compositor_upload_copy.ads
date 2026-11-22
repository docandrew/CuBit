with System;
with Compositor_Upload;
-- Trusted synchronous CPU copy into an already-owned staging writer. Caller
-- holds an immutable, accessible source and exclusive staging mapping, with
-- no physical alias. This neither submits nor retires either allocation.
package Compositor_Upload_Copy with SPARK_Mode is
   use type System.Address;
   -- No Ada global state is accessed. Raw bytes behind Target are modified;
   -- their mapping, exclusive ownership and physical non-aliasing are trusted
   -- caller obligations outside the SPARK address model. Complete reports a
   -- synchronous checked copy, never GPU completion or allocation retirement.
   procedure Copy (Source, Target : System.Address;
      Source_Bytes, Source_Pitch : Natural; Width, Height : Compositor_Upload.Edge;
      Kind : Compositor_Upload.Pixel_Format; Plan : Compositor_Upload.Plan;
      Complete : out Boolean)
     with Global => null,
       Post => (if Complete then
         Source /= System.Null_Address and Target /= System.Null_Address and
         Compositor_Upload.Valid (Plan) and
         Width = Compositor_Upload.Image_Width (Plan) and
         Height = Compositor_Upload.Image_Height (Plan));
end Compositor_Upload_Copy;
