with System; with Compositor_Upload;
package Vulkan_Upload_Record_FFI with SPARK_Mode is
   procedure Record_Transfer (Context, Upload, Image : System.Address;
      Plan : Compositor_Upload.Plan; Discard : Boolean; Accepted : out Boolean)
     with Global => null, Pre => Compositor_Upload.Valid (Plan);
end Vulkan_Upload_Record_FFI;
