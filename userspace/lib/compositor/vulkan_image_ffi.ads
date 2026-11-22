with Interfaces;
with System;
-- Private caller-owned request; foreign state is abstracted by Global null.
-- Handle validity, matching authorized device, driver compliance and exclusive
-- request ownership are trusted, as is the caller's retirement evidence.
package Vulkan_Image_FFI with SPARK_Mode is
   subtype U32 is Interfaces.Unsigned_32;
   subtype U64 is Interfaces.Unsigned_64;
   procedure Prepare (Request : System.Address; Bytes : out U64;
                      Types : out U32; Result : out U32) with Global => null;
   procedure Bind (Request : System.Address; Bytes : U64;
                   Memory_Type : U32; Result : out U32) with Global => null;
   procedure Release (Request : System.Address; Result : out U32) with Global => null;
end Vulkan_Image_FFI;
