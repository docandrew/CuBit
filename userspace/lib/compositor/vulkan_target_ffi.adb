package body Vulkan_Target_FFI with SPARK_Mode => Off is
   function Native_Create (Description : System.Address; A, B, C : out System.Address) return Code
     with Import, Convention => C, External_Name => "cubit_vulkan_targets_create";
   function Native_Release (Description : System.Address) return Code
     with Import, Convention => C, External_Name => "cubit_vulkan_targets_release";
   procedure Create (Description : System.Address; A, B, C : out System.Address; Result : out Code) is
   begin Result := Native_Create (Description, A, B, C); end Create;
   procedure Release (Description : System.Address; Result : out Code) is
   begin Result := Native_Release (Description); end Release;
end Vulkan_Target_FFI;
