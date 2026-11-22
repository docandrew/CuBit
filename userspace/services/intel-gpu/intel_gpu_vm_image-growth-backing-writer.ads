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
   type Preparation is limited private;
   procedure Begin_Preparation
     (Work : in out Preparation; Object : in out State; Source : Image;
      GPU, Bytes, Retained_Root : Unsigned_64; Page_Count : Natural; Accepted : out Boolean);
   generic
      with function Read_Page (Ordinal : Positive) return Unsigned_64;
   procedure Prepare_Step (Work : in out Preparation; Object : in out State; Source : Image);
   procedure Cancel_Preparation (Work : in out Preparation; Object : in out State);
   function Preparing (Work : Preparation) return Boolean;
   function Prepared (Work : Preparation; Object : State; Source : Image) return Boolean;
   -- Retain Work, the exact receipt and source through the attempt. One input
   -- callback/capture OR one bounded backing-resolution step per call. No GPU
   -- memory IO before Prepared. Capture reads each supplied page exactly once;
   -- subsequent validation uses the receipt's retained growable storage.
   -- Failure consumes the receipt, keeps backing, and forbids publication.
   generic
      with function Read_Page (Ordinal : Positive) return Unsigned_64;
   procedure Start_From_Pages
     (Object : in out State; Source : Image; GPU, Bytes, Retained_Root : Unsigned_64;
      Page_Count : Natural; Accepted : out Boolean);
   -- Read authenticated input once into private growable receipt storage.
   -- Resolve/Commit use those same retained values, never a borrowed array.
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
   type Adoption is limited private;
   procedure Begin_Commit
     (Work : in out Adoption; Object : in out State; Source : Image; Accepted : out Boolean);
   procedure Commit_Step (Work : in out Adoption; Object : in out State; Source : in out Image);
   procedure Cancel_Commit (Work : in out Adoption);
   function Committing (Work : Adoption) return Boolean;
   -- One bounded resolver step,32mirror-word clears, or32directory links per
   -- call. Source remains valid during preflight, then hidden until all mirror
   -- changes are adopted atomically. Retain the exact Source/receipt/controller.
   -- Losing authority after hiding leaves the image invalid and backing held;
   -- no rollback, retry, or allocator reuse is authorized.
   procedure Commit
     (Object : in out State; Source : in out Image; Accepted : out Boolean);
   function Committed (Object : State) return Boolean;
   -- Consume once after publication and confirmed invalidation. Adopts only
   -- empty directories, advances source epoch, preserves existing mappings.
   -- Preflight rejection leaves Source unchanged. Interrupted adoption leaves
   -- Source invalid; retain/quarantine hardware backing in either case.
   -- Confirmation must belong to this serialized update, not an old TLB event.
   -- Trusted callbacks resolve retained DMA to CPU mappings, bounded/nonraising.
   -- Exclusive includes GPU drain and acknowledged context disable, not just a
   -- CPU lock. Flush completes visibility/ordering. Serialized, non-reentrant.
   -- One attempt including preflight rejection. Failure retains all backing;
   -- no rollback or retry. Success requires later TLB invalidation and software
   -- image adoption before resuming. No application leaf mapping is written.
   type Rearming is limited private;
   procedure Begin_Rearm
     (Work : in out Rearming; Object : State; Source : Image;
      Retained_Root : Unsigned_64; Accepted : out Boolean);
   procedure Rearm_Step
     (Work : in out Rearming; Object : in out State; Source : Image);
   procedure Cancel_Rearm (Work : in out Rearming);
   function Rearm_Pending (Work : Rearming) return Boolean;
   function Rearmed (Work : Rearming) return Boolean;
   -- One adopted-table ownership check per step, plus retained-root authority.
   -- Keep the same receipt/source/controller until completion. Interruption
   -- retains the consumed receipt; no hardware writes or page release.
   procedure Rearm
     (Object : in out State; Source : Image; Retained_Root : Unsigned_64;
      Accepted : out Boolean);
   -- Metadata-only reuse after a successful commit. Every adopted table must
   -- still belong to this same sealed source and retained hardware root.
   -- Does not release pages or replay a failed/uncertain transaction.
private
   type Rearm_Phase is (Rearm_Unused, Rearm_Checking, Rearm_Done, Rearm_Failed);
   type Rearming is limited record
      Phase : Rearm_Phase := Rearm_Unused;
      Root, Epoch, Receipt_Epoch, Hardware_Root : Unsigned_64 := 0;
      Count, First, Cursor : Natural := 0;
   end record;
   type Preparation_Phase is (Unused, Capture_Pages, Resolve_Pages, Ready, Rejected);
   type Preparation is limited record
      Phase : Preparation_Phase := Unused;
      Resolver : Resolution;
      Root, Epoch, Hardware_Root : Unsigned_64 := 0;
      Count, Cursor : Natural := 0;
      Cancelled : Boolean := False;
   end record;
   type Adoption_Phase is (No_Adoption, Validate_Plan, Clear_Mirrors, Link_Mirrors,
                          Adoption_Done, Adoption_Failed);
   type Adoption is limited record
      Phase : Adoption_Phase := No_Adoption;
      Resolver : Resolution;
      Root, Epoch, Hardware_Root : Unsigned_64 := 0;
      Base, Count, Cursor : Natural := 0;
      Cancelled : Boolean := False;
   end record;
end Intel_GPU_VM_Image.Growth.Backing.Writer;
