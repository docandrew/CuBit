with System;
with Interfaces;
package Vulkan_Context_FFI with SPARK_Mode is
   subtype Code is Interfaces.Unsigned_32;
   -- Global null abstracts private request/object storage, not side effects.
   procedure Create (Description : System.Address; Context : out System.Address; Result : out Code)
     with Global => null;
   procedure Release (Description : System.Address; Result : out Code) with Global => null;
end Vulkan_Context_FFI;
