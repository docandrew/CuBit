generic
   with function Exclusive return Boolean;
   with procedure Read_Word
     (DMA : Unsigned_64; Index : Intel_GPU_ADLN_PPGTT.Table_Index;
      Value : out Unsigned_64; OK : out Boolean);
   with procedure Write_Word
     (DMA : Unsigned_64; Index : Intel_GPU_ADLN_PPGTT.Table_Index;
      Value : Unsigned_64; OK : out Boolean);
   with function Flush_Page (DMA : Unsigned_64) return Boolean;
   with function Invalidation_Confirmed return Boolean;
package Intel_GPU_VM_Image.Growth.Backing.Writer is
   subtype State is Growth_Receipt;
   procedure Start
     (Object : in out State; Source : Image; GPU, Bytes, Retained_Root : Unsigned_64;
      New_Pages : Data_Pages; Accepted : out Boolean);
   procedure Step (Object : in out State; Source : Image);
   function Pending (Object : State) return Boolean;
   -- Start performs topology/authority preflight only (no memory IO). Each
   -- Step performs at most one read, write or page-flush callback, rechecking
   -- exclusion and the source epoch across yields. Pending=False terminates
   -- publication; Published distinguishes success from retained failure.
   function Attempted (Object : State) return Boolean;
   function Published (Object : State) return Boolean;
   procedure Commit
     (Object : in out State; Source : in out Image; Accepted : out Boolean);
   function Committed (Object : State) return Boolean;
   -- Consume once after publication and confirmed invalidation. Adopts only
   -- empty directories, advances source epoch, preserves existing mappings.
   -- Rejection leaves Source unchanged; retain/quarantine hardware backing.
   -- Confirmation must belong to this serialized update, not an old TLB event.
   -- Trusted callbacks resolve retained DMA to CPU mappings, bounded/nonraising.
   -- Exclusive includes GPU drain and acknowledged context disable, not just a
   -- CPU lock. Flush completes visibility/ordering. Serialized, non-reentrant.
   -- One attempt including preflight rejection. Failure retains all backing;
   -- no rollback or retry. Success requires later TLB invalidation and software
   -- image adoption before resuming. No application leaf mapping is written.
   procedure Rearm
     (Object : in out State; Source : Image; Retained_Root : Unsigned_64;
      Accepted : out Boolean);
   -- Metadata-only reuse after a successful commit. Every adopted table must
   -- still belong to this same sealed source and retained hardware root.
   -- Does not release pages or replay a failed/uncertain transaction.
end Intel_GPU_VM_Image.Growth.Backing.Writer;
