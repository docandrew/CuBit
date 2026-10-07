with System;
with Compositor_Upload;
-- Trusted synchronous CPU copy into an already-owned staging writer. Caller
-- holds an immutable, accessible source and exclusive staging mapping, with
-- no physical alias. This neither submits nor retires either allocation.
package Compositor_Upload_Copy with SPARK_Mode => Off is
   procedure Copy (Source, Target : System.Address;
      Source_Bytes, Source_Pitch : Natural; Width, Height : Compositor_Upload.Edge;
      Kind : Compositor_Upload.Pixel_Format; Plan : Compositor_Upload.Plan;
      Complete : out Boolean);
end Compositor_Upload_Copy;
