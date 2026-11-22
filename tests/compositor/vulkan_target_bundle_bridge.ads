with System; with Interfaces; with Interfaces.C;
package Vulkan_Target_Bundle_Bridge is
   function Upload_Open (Request : System.Address; Size : Interfaces.Unsigned_32) return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_target_upload_open";
   function Upload_Begin return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_target_upload_begin";
   function Upload_First_Row return Interfaces.Unsigned_32
     with Export, Convention => C, External_Name => "test_target_upload_first_row";
   function Upload_Submit return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_target_upload_submit";
   function Source_Restart (Width, Height, Mask : Interfaces.Unsigned_32) return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_target_source_restart";
   function Source_Detach return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_target_source_detach";
   function Upload_Finish return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_target_upload_finish";
   function Upload_Mapping return System.Address
     with Export, Convention => C, External_Name => "test_target_upload_mapping";
   function Upload_Close return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_target_upload_close";
   function Source_Configure (Width, Height, Mask : Interfaces.Unsigned_32) return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_target_source_configure";
   function Source_Image_Request return System.Address
     with Export, Convention => C, External_Name => "test_target_source_image";
   function Source_Import (Request : System.Address) return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_target_source_import";
   function Source_Release return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_target_source_release";
   function Textured_Fill return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_target_textured_fill";
   function Context_Open (Description : System.Address) return System.Address
     with Export, Convention => C, External_Name => "test_target_context_open";
   function Context_Close return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_target_context_close";
   function Open (Description, A, B, C, Submission : System.Address; Allowed : Interfaces.Unsigned_32) return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_target_bundle_open";
   function Open_Device_Targets (Width, Height : Interfaces.Unsigned_32) return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_target_device_open";
   -- Test-only introspection for independent Vulkan readback/retirement oracle.
   function Device_Request (Index : Interfaces.C.int) return System.Address
     with Export, Convention => C, External_Name => "test_target_device_request";
   function Close (Hold : Interfaces.C.int) return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_target_bundle_close";
   function Fill (Rotation, N, D, L, T, R, B : Interfaces.C.int) return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_target_bundle_fill";
   function Cancel_First return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_target_cancel_first";
   function Settle_Fill return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_target_settle_fill";
   function Partial_Fill return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_target_partial_fill";
   function Finish return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_target_bundle_finish";
end Vulkan_Target_Bundle_Bridge;
