package body Vulkan_Device_Upload_FFI with SPARK_Mode => Off is
   function Native_Prepare return System.Address with Import, Convention => C,
     External_Name => "cubit_vulkan_device_upload_prepare";
   function Prepare return System.Address is (Native_Prepare);
   function Native_Readback return System.Address with Import, Convention => C,
     External_Name => "cubit_vulkan_device_readback_prepare";
   function Prepare_Readback return System.Address is (Native_Readback);
end Vulkan_Device_Upload_FFI;
