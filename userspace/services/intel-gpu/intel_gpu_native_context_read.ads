with Interfaces;
with Intel_GPU_ADLN_Steering;
generic
   -- Serialized owner, reset page zero mapped, original MMIO read aperture
   -- mapped, required render forcewake retained throughout every callback.
   with function Owner_Ready return Boolean;
   with function Topology return Intel_GPU_ADLN_Steering.Topology;
package Intel_GPU_Native_Context_Read is
   -- Selects an enabled DSS, reads only WM_CHICKEN2, restores MCR selector.
   -- Last is an invalid sentinel. Any attempted MCR failure quarantines this
   -- instance. There is no CPU write interface for the context register.
   function Read_WM_Chicken2 return Interfaces.Unsigned_32;
end Intel_GPU_Native_Context_Read;
