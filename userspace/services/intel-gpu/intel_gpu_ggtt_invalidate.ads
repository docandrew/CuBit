package Intel_GPU_GGTT_Invalidate is
   -- ADL-N Gen12 MMIO fallback. Linux v6.16 intel_ggtt.c uses this when
   -- has_guc_tlb_invalidation is absent; i915_pci.c does NOT enable that
   -- capability for ADL-N/adl_p_info, even after GuC CT becomes ready.
   -- Do not replace it with the 7000 CT action based on CT readiness alone.
   -- Caller retains the exclusive device owner and all required forcewake.
   -- Control_Pages_Ready is trusted driver state after every fixed-page map.
   -- True means the ordered MMIO command was issued, NOT GuC authentication,
   -- execution, engine-idle, or an independently observed completion event.
   -- Reclamation still requires caller-established quiescence and the complete
   -- platform translation/order contract, not just this Boolean result.
   function Issue (Control_Pages_Ready : Boolean) return Boolean;
end Intel_GPU_GGTT_Invalidate;
