with Interfaces;
with Intel_GPU_VM_Image;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_ADLN_PPGTT;
generic
   with package VM is new Intel_GPU_VM_Image (<>);
package Intel_GPU_VM_Buffer is
   function Matches_Range
     (Object : VM.Image; Backing : Intel_GPU_Buffer_Reply.Extent_View;
      GPU, Offset, Bytes : Interfaces.Unsigned_64) return Boolean;
   procedure Unbind_Range
     (Object : in out VM.Image; Backing : Intel_GPU_Buffer_Reply.Extent_View;
      GPU, Offset, Bytes : Interfaces.Unsigned_64;
      Accepted : out Boolean);
   -- Matching requires a sealed image; unbinding requires an unpublished
   -- image and exact backing identity for every page. Neither releases RAM.
   -- Trusted retained allocation view only; offsets are relative to the view.
   -- Produces ordinary 4KiB leaves across nonadjacent 2MiB backing blocks.
   -- Large GPU pages and live publication are separate operations.
   procedure Bind_Range
     (Object : in out VM.Image; Backing : Intel_GPU_Buffer_Reply.Extent_View;
      GPU, Offset, Bytes : Interfaces.Unsigned_64;
      Accepted : out Boolean;
      Access_Mode : Intel_GPU_ADLN_PPGTT.Page_Access := Intel_GPU_ADLN_PPGTT.Read_Write);
   -- Access must come from the authenticated backing-use contract, not a
   -- CPU grant's flags. ADLN currently rejects Read_Only due to the Gen12
   -- read-only fault restriction; never silently widen it to Read_Write.
   -- The default retains ordinary owner-buffer binding behavior only.
   -- Verify every page of a byte slice against a sealed retained VM image.
   -- No mutation or DMA address output. The image must describe the currently
   -- published generation; this does not inspect hardware tables or commands.
   function Matches_Range
     (Object : VM.Image; Backing : Intel_GPU_Buffer_Reply.Backing;
      GPU, Offset, Bytes : Interfaces.Unsigned_64) return Boolean;
   -- Driver-internal, offline binding of an already-owned retained buffer.
   -- Backing must come from this device's Buffer_Memory pool, not IPC words
   -- supplied by an application. Numeric checks below do not prove authority.
   -- GPU is raw48 (the Mesa boundary removes verified sign extension).
   -- This pool has one immutable WB policy: kernel DMA and grant mappings
   -- use CPU PAT0/WB, and all GPU bindings use our GPU PAT0/WB. Reject other
   -- policies, including in separate images; do not infer HOST_COHERENT from
   -- this choice. A future alternate pool needs an explicit backing contract.
   procedure Bind_Range
     (Object : in out VM.Image; Backing : Intel_GPU_Buffer_Reply.Backing;
      GPU, Offset, Bytes : Interfaces.Unsigned_64;
      Policy : Intel_GPU_ADLN_PPGTT.Cache_Policy;
      Access_Mode : Intel_GPU_ADLN_PPGTT.Page_Access;
      Accepted : out Boolean);
   -- Entire range succeeds or leaves the offline image unchanged. Retention
   -- and owner checks at materialization/publication remain mandatory. This
   -- is not live rebinding, a TLB invalidation, or permission to submit work.
   procedure Unbind_Range
     (Object : in out VM.Image; Backing : Intel_GPU_Buffer_Reply.Backing;
      GPU, Offset, Bytes : Interfaces.Unsigned_64;
      Accepted : out Boolean);
   -- Same owner-only backing contract as Bind_Range. Removes only an exact
   -- matching range in an unpublished image; does not release backing or
   -- authorize reuse in a live context. Sealed images are rejected.
end Intel_GPU_VM_Buffer;
