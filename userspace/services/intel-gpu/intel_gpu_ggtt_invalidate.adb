with Interfaces; use Interfaces;
with Intel_GPU_Native_Reset;
with Intel_GPU_GGTT_Mapping;
with Intel_GPU_TLB_Registers;
with Intel_GPU_Native_TLB_IO;
package body Intel_GPU_GGTT_Invalidate is
   function Issue (Control_Pages_Ready : Boolean) return Boolean is
      function Owner_Ready return Boolean is
        (Control_Pages_Ready and then Intel_GPU_Native_Reset.Last_Succeeded
         and then Intel_GPU_GGTT_Mapping.Ready);
      package IO is new Intel_GPU_Native_TLB_IO (Owner_Ready, GuC_Only => True);
      OK : Boolean;
   begin
      -- Both mappings are UC/NX. Match the Gen12 i915 MMIO fallback: an exact
      -- 32-bit invalidate write, not a read/modify/write. This issuance-only
      -- API does not poll the documented self-clearing completion field.
      -- Adapter orders preceding PTE stores and rechecks ownership after MMIO.
      -- A failure can follow a posted write: caller must retain backing.
      IO.Write_Register (Intel_GPU_TLB_Registers.GuC_Offset,
        Intel_GPU_TLB_Registers.Encode_GuC ((Request => 1, Reserved => 0)), OK);
      return OK;
   end Issue;
end Intel_GPU_GGTT_Invalidate;
