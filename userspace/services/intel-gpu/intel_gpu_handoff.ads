with Interfaces;
with Intel_GPU_ADLN_Inventory;
-- One initial ownership attempt, not runtime reset/recovery. The caller must
-- authenticate identity/fuses, exclude other submissions and preserve display
-- mappings. All callbacks are bounded/non-raising and report ambiguity false.
generic
   with procedure Hold_Forcewake (Success : out Boolean);
   with procedure Stop_Engine
     (Item : Intel_GPU_ADLN_Inventory.Engine; Success : out Boolean);
   with procedure Prepare_Engine
     (Item : Intel_GPU_ADLN_Inventory.Engine; Success : out Boolean);
   with procedure Reset_And_Settle (Success : out Boolean);
   with procedure Cancel_Preparation
     (Item : Intel_GPU_ADLN_Inventory.Engine; Success : out Boolean);
package Intel_GPU_Handoff is
   type Phase is (Fresh, Quarantined, Reset_Held);
   type Result is (Rejected, Forcewake_Failed, Stop_Failed, Prepare_Failed,
                   Reset_Failed, Cleanup_Failed, Complete);
   type Attempt is limited private;
   function State (Object : Attempt) return Phase;
   procedure Execute (Object : in out Attempt;
                      Vendor, Device : Interfaces.Unsigned_16;
                      Fuse : Interfaces.Unsigned_32; Status : out Result);
   -- Holds forcewake on both success and post-acquisition failure. No release,
   -- resume, retry, PTE writes or buffer freeing occurs here. Complete means
   -- reset+cleanup completed under retained power, not a usable graphics engine.
private
   type Attempt is limited record
      Current : Phase := Fresh;
   end record;
end Intel_GPU_Handoff;
