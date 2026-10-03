package body Vulkan_Context_FFI with SPARK_Mode => Off is
   function Native_Create (Description : System.Address; Context : out System.Address) return Code
     with Import, Convention => C, External_Name => "cubit_vulkan_context_create";
   function Native_Release (Description : System.Address) return Code
     with Import, Convention => C, External_Name => "cubit_vulkan_context_release";
   procedure Create (Description : System.Address; Context : out System.Address; Result : out Code) is
   begin Result := Native_Create (Description, Context); end Create;
   procedure Release (Description : System.Address; Result : out Code) is
   begin Result := Native_Release (Description); end Release;
end Vulkan_Context_FFI;
