package Intel_GPU_GGTT_Invalidate is
   -- Initial ADL-N bring-up only, before GuC CT-based invalidation exists.
   -- Caller retains the exclusive device owner and all required forcewake.
   -- Control_Pages_Ready is trusted driver state after every fixed-page map.
   -- True means the ordered MMIO command was issued, NOT GuC authentication,
   -- execution, engine-idle, or an independently observed completion event.
   function Issue (Control_Pages_Ready : Boolean) return Boolean;
end Intel_GPU_GGTT_Invalidate;
