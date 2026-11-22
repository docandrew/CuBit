package body Vulkan_Image_FFI with SPARK_Mode => Off is
   function C_Prepare (Request : System.Address; Bytes : out U64;
                       Types : out U32) return U32
     with Import, Convention => C, External_Name => "cubit_vulkan_owned_image_prepare";
   function C_Bind (Request : System.Address; Bytes : U64; Memory_Type : U32) return U32
     with Import, Convention => C, External_Name => "cubit_vulkan_owned_image_bind";
   function C_Release (Request : System.Address) return U32
     with Import, Convention => C, External_Name => "cubit_vulkan_owned_image_release";
   procedure Prepare (Request : System.Address; Bytes : out U64;
                      Types : out U32; Result : out U32) is
   begin Result := C_Prepare (Request, Bytes, Types); end Prepare;
   procedure Bind (Request : System.Address; Bytes : U64;
                   Memory_Type : U32; Result : out U32) is
   begin Result := C_Bind (Request, Bytes, Memory_Type); end Bind;
   procedure Release (Request : System.Address; Result : out U32) is
   begin Result := C_Release (Request); end Release;
end Vulkan_Image_FFI;
