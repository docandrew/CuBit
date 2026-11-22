with System;
with Interfaces;
-- Trusted Vulkan metadata construction/destruction. The request and its leases
-- remain live/immutable through close. Global null abstracts private ownership.
package Vulkan_Target_FFI with SPARK_Mode is
   subtype Code is Interfaces.Unsigned_32;
   procedure Create (Description : System.Address; A, B, C : out System.Address; Result : out Code)
     with Global => null;
   procedure Release (Description : System.Address; Result : out Code) with Global => null;
end Vulkan_Target_FFI;
