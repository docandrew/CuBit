with Intel_GPU_ADLN_PPGTT;
generic
   with package Accessor is new Authority (<>);
   with function Exclusive return Boolean;
   with function Flush_CPU_Page (CPU : Unsigned_64) return Boolean;
package Intel_GPU_Table_Provenance.IO is
   procedure Read_Word
     (Object : Ledger; Session, Expected_Generation : Unsigned_64; Table : Positive;
      Expected_DMA : Unsigned_64; Index : Intel_GPU_ADLN_PPGTT.Table_Index;
      Value : out Unsigned_64; Accepted : out Boolean);
   procedure Write_Word
     (Object : Ledger; Session, Expected_Generation : Unsigned_64; Table : Positive;
      Expected_DMA : Unsigned_64; Index : Intel_GPU_ADLN_PPGTT.Table_Index;
      Value : Unsigned_64; Accepted : out Boolean);
   function Flush
     (Object : Ledger; Session, Expected_Generation : Unsigned_64; Table : Positive;
      Expected_DMA : Unsigned_64) return Boolean;
   -- Volatile CPU accesses through authenticated retained provenance only.
   -- Caller serializes mapping lifetime, forbids aliases/reentrant callbacks,
   -- holds drained/disabled GPU exclusion and retains pages even on failure.
   -- Flush callback supplies actual cache visibility/ordering; this adapter
   -- alone neither publishes a transaction nor confirms TLB retirement.
end Intel_GPU_Table_Provenance.IO;
