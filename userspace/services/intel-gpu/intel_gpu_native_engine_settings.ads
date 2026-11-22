with Interfaces;
with Intel_GPU_ADLN_Inventory;
with Intel_GPU_ADLN_Steering;
generic
   Item : Intel_GPU_ADLN_Inventory.Engine;
   with function Owner_Ready return Boolean;
   with function Inventory return Intel_GPU_ADLN_Inventory.Inventory;
   with function Topology return Intel_GPU_ADLN_Steering.Topology;
package Intel_GPU_Native_Engine_Settings is
   function Address_For (Offset : Interfaces.Unsigned_32) return Interfaces.Unsigned_64;
   function Read32 (Offset : Interfaces.Unsigned_32; MCR : Boolean)
     return Interfaces.Unsigned_32;
   procedure Write32 (Offset, Value : Interfaces.Unsigned_32; MCR : Boolean;
                      Success : out Boolean);
end Intel_GPU_Native_Engine_Settings;
