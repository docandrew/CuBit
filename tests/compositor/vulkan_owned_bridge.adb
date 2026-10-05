with Vulkan_Image_Owner;
package body Vulkan_Owned_Bridge is
   package O renames Vulkan_Image_Owner;
   package A renames O.Accounting;
   use type O.Phase;
   Owners : array (Interfaces.Unsigned_32 range 0 .. 7) of O.State;
   Budget : A.State := A.Open (16 * 1024 * 1024);
   function Allocate (Slot : Interfaces.Unsigned_32; Request : System.Address;
                      Allowed : Interfaces.Unsigned_32) return Interfaces.C.int is
   begin
      O.Prepare (Owners (Slot), Request);
      if O.Status (Owners (Slot)) /= O.Prepared then return 1; end if;
      O.Allocate (Owners (Slot), Budget, Allowed);
      return (if O.Status (Owners (Slot)) = O.Live then 0 else 1);
   end Allocate;
   function Release (Slot : Interfaces.Unsigned_32) return Interfaces.C.int is
   begin
      -- Host fixture has waited device idle and destroyed every dependent view.
      O.Release (Owners (Slot), Budget, True);
      return (if O.Status (Owners (Slot)) = O.Closed then 0 else 1);
   end Release;
   function Empty return Interfaces.C.int is (if A.Charged (Budget) = 0 then 1 else 0);
end Vulkan_Owned_Bridge;
