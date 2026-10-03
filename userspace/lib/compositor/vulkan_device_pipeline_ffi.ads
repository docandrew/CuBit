with System;
with Interfaces;
-- Trusted singleton object boundary. The SPARK owner registers a context child
-- before Create and requires GPU/source quiescence before Close.
package Vulkan_Device_Pipeline_FFI with SPARK_Mode is
   procedure Create (Result : out Interfaces.Unsigned_32) with Global => null;
   procedure Close (Result : out Interfaces.Unsigned_32) with Global => null;
   function Source_Request (Index : Interfaces.Unsigned_32; Image : System.Address)
      return System.Address with Global => null;
end Vulkan_Device_Pipeline_FFI;
