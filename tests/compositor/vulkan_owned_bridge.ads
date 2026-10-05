with Interfaces.C; with Interfaces; with System;
package Vulkan_Owned_Bridge is
   function Allocate (Slot : Interfaces.Unsigned_32; Request : System.Address;
                      Allowed : Interfaces.Unsigned_32) return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_owned_allocate";
   function Release (Slot : Interfaces.Unsigned_32) return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_owned_release";
   function Empty return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_owned_empty";
end Vulkan_Owned_Bridge;
