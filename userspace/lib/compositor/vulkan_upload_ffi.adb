package body Vulkan_Upload_FFI with SPARK_Mode => Off is
   function C_Prepare (Request : System.Address; Capacity : U32;
      Bytes : out U64; Types : out U32) return U32
     with Import, Convention => C, External_Name => "cubit_vulkan_upload_prepare";
   function C_Bind (Request : System.Address; Bytes : U64; Memory_Type : U32;
      Mapping : out System.Address) return U32
     with Import, Convention => C, External_Name => "cubit_vulkan_upload_bind";
   function C_Release (Request : System.Address) return U32
     with Import, Convention => C, External_Name => "cubit_vulkan_upload_release";
   procedure Prepare (Request : System.Address; Capacity : U32;
      Bytes : out U64; Types : out U32; Result : out U32) is
   begin Result := C_Prepare (Request, Capacity, Bytes, Types); end Prepare;
   procedure Bind (Request : System.Address; Bytes : U64; Memory_Type : U32;
      Mapping : out System.Address; Result : out U32) is
   begin Result := C_Bind (Request, Bytes, Memory_Type, Mapping); end Bind;
   procedure Release (Request : System.Address; Result : out U32) is
   begin Result := C_Release (Request); end Release;
end Vulkan_Upload_FFI;
