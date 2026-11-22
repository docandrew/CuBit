package body Vulkan_Device_Pipeline_FFI with SPARK_Mode => Off is
   function Native_Last_Failure
     (Stage, Index : access Interfaces.Unsigned_32;
      Result : access Interfaces.Integer_32) return Interfaces.Unsigned_32
     with Import, Convention => C, External_Name => "cubit_vulkan_pipeline_last_failure";
   procedure Last_Failure
     (Stage, Index : out Interfaces.Unsigned_32;
      Result : out Interfaces.Integer_32; Valid : out Boolean) is
      S, I : aliased Interfaces.Unsigned_32 := 0;
      R : aliased Interfaces.Integer_32 := 0;
      V : Interfaces.Unsigned_32;
      use type Interfaces.Unsigned_32;
   begin
      V := Native_Last_Failure (S'Access, I'Access, R'Access);
      Stage := S; Index := I; Result := R; Valid := V = 1;
   end Last_Failure;
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
