generic
package Intel_GPU_VM_Image.Snapshots is
   -- Update only the service's logical CURRENT image after a successful
   -- VM_Update transaction. No hardware writes, invalidation, reclamation,
   -- allocation or authorization occur here. The serialized caller must keep
   -- submissions excluded until this returns, retain all old/new DMA backing,
   -- and retain the original hardware root independently (Application_Image).
   -- Candidate must be the sealed direct successor of Object. Rejected,
   -- stale, replayed or unrelated candidates leave Object unchanged.
   procedure Adopt_Committed
     (Object : in out Image; Candidate : Image; Accepted : out Boolean);
end Intel_GPU_VM_Image.Snapshots;
