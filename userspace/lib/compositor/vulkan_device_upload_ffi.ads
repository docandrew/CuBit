with System;
-- Private metadata only. Caller serializes with all upload activity and owns
-- a Fresh/confirmed-Closed upload owner for this admitted device.
package Vulkan_Device_Upload_FFI with SPARK_Mode is
   function Prepare return System.Address with Global => null;
   function Prepare_Readback return System.Address with Global => null;
end Vulkan_Device_Upload_FFI;
