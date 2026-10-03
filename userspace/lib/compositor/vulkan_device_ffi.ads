with Interfaces;
with System;
package Vulkan_Device_FFI with SPARK_Mode is
   -- Abstracts one process-private Mesa owner and request storage. Calls may
   -- block in native IPC. Exactly one uncopied device controller per process.
   procedure Start
     (Slot : Interfaces.Unsigned_64; Owned : out Boolean;
      Description : out System.Address) with Global => null;
   procedure Health (Healthy : out Boolean) with Global => null;
   type Retirement is (Retired, Pending, Unsafe);
   procedure Close (Result : out Retirement) with Global => null;
end Vulkan_Device_FFI;
