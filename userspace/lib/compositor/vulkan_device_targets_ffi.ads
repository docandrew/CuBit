with Interfaces;
with System;
with Vulkan_Frame;
-- Trusted metadata construction only. The caller serializes this with the
-- singleton device/context owner and calls only while it is ready.
package Vulkan_Device_Targets_FFI with SPARK_Mode is
   procedure Prepare
     (Width, Height : Interfaces.Unsigned_32;
      Description : out System.Address; Requests : out Vulkan_Frame.Targets;
      Allowed_Types : out Interfaces.Unsigned_32)
     with Global => null;
end Vulkan_Device_Targets_FFI;
