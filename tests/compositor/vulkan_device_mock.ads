with Interfaces;
package Vulkan_Device_Mock with SPARK_Mode is
   procedure Set (Owned : Boolean; Description : Boolean; Retirement : Interfaces.Unsigned_32)
     with Global => null;
   function Starts return Interfaces.Unsigned_32 with Global => null;
   function Closes return Interfaces.Unsigned_32 with Global => null;
end Vulkan_Device_Mock;
