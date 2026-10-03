with System;
with Interfaces;
-- Private metadata constructor. Caller must own the selected slot with a
-- Fresh/confirmed-Closed Vulkan_Owned_Source, serialized against import/close.
-- All 140 source backings share the aggregate target/upload byte budget.
package Vulkan_Device_Source_FFI with SPARK_Mode is
   subtype Slot is Interfaces.Unsigned_32 range 0 .. 139;
   procedure Prepare (Index : Slot; Width, Height : Interfaces.Unsigned_32;
      Mask : Boolean; Request : out System.Address;
      Allowed_Types : out Interfaces.Unsigned_32) with Global => null;
end Vulkan_Device_Source_FFI;
