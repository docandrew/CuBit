generic
package Intel_GPU_VM_Image.Snapshots is
   -- True only after successful Forget_Retired and before another preparation
   -- attempt. This distinguishes disposed offline receipts from failed/unsealed
   -- images in cross-context alias scans; it is NOT hardware completion evidence
   -- or permission to free backing. The coordinator supplies that evidence to
   -- Forget_Retired before this state can be reached.
   function Retired (Object : Image) return Boolean;
   -- Update only the service's logical CURRENT image after a successful
   -- VM_Update transaction. No hardware writes, invalidation, reclamation,
   -- allocation or authorization occur here. The serialized caller must keep
   -- submissions excluded until this returns, retain all old/new DMA backing,
   -- and retain the original hardware root independently (Application_Image).
   -- Candidate must be the sealed direct successor of Object. Rejected,
   -- stale, replayed or unrelated candidates leave Object unchanged.
   procedure Adopt_Committed
     (Object : in out Image; Candidate : Image; Accepted : out Boolean);
   -- Trusted coordinator only, after confirmed hardware/TLB retirement and
   -- allocator acknowledgement. Clears OFFLINE metadata, not DMA memory.
   -- The stable hardware root must remain retained independently. Exact
   -- revision AND root reject stale completions across physical-address reuse.
   -- Failed/unsealed images remain quarantined; no rollback inferred here.
   procedure Forget_Retired
     (Object : in out Image; Expected_Revision, Expected_Root : Unsigned_64;
      References_Retired : Boolean; Accepted : out Boolean);
end Intel_GPU_VM_Image.Snapshots;
