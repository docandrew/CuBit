package body Vulkan_Device_Pipeline_FFI with SPARK_Mode => Off is
   function Native_Create return Interfaces.Unsigned_32
     with Import, Convention => C, External_Name => "cubit_vulkan_device_pipeline_create";
   function Native_Close return Interfaces.Unsigned_32
     with Import, Convention => C, External_Name => "cubit_vulkan_device_pipeline_close";
   procedure Create (Result : out Interfaces.Unsigned_32) is
   begin Result := Native_Create; end Create;
   procedure Close (Result : out Interfaces.Unsigned_32) is
   begin Result := Native_Close; end Close;
   function Native_Source_Request (Index : Interfaces.Unsigned_32; Image : System.Address)
      return System.Address with Import, Convention => C,
        External_Name => "cubit_vulkan_device_source_request";
   function Source_Request (Index : Interfaces.Unsigned_32; Image : System.Address)
      return System.Address is
   begin return Native_Source_Request (Index, Image); end Source_Request;
end Vulkan_Device_Pipeline_FFI;
