with Interfaces;
with Intel_GPU_VA_Placement;
package Intel_GPU_GGTT_Reservations with SPARK_Mode is
   use Interfaces;
   -- One ledger per exclusively owned GGTT aperture, serialized by its owner.
   -- Admission is NOT discovery: the caller must establish ownership of this
   -- entire interval and exclude firmware/platform reservations. Protect
   -- scanout with retained claims or the mandatory publication exclusion gate;
   -- Space_Free alone does not consult display state or hardware PTEs.
   -- No reset/release exists during bring-up: even failed publications retain
   -- their ranges. Never replace a ledger while DMA can still reference it.
   type Ledger is limited private;
   -- Proof-only ledger invariant: admitted claims are nonempty, contained in
   -- the admitted aperture and pairwise disjoint. This does not establish
   -- that platform firmware has relinquished that aperture.
   function Valid (Object : Ledger) return Boolean with Ghost;
   function Count (Object : Ledger) return Natural;
   type Claim_Model is private with Ghost;
   function Claims (Object : Ledger) return Claim_Model with Ghost;
   function Preserves
     (Before, After : Claim_Model; Prior_Count : Natural) return Boolean
   with Ghost, Pre => Prior_Count <= 64;
   function Last_Claim_Is
     (Object : Ledger; First, Bytes : Unsigned_64) return Boolean with Ghost;
   type Result is (Rejected, Overlap, Exhausted, Reserved);
   procedure Admit
     (Object : in out Ledger; Table_Bytes, First, Bytes : Unsigned_64;
      Success : out Boolean)
   with Pre => Valid (Object),
        Post => Valid (Object) and Count (Object) = Count (Object)'Old and
          Preserves (Claims (Object)'Old, Claims (Object), Count (Object)'Old);
   procedure Reserve
     (Object : in out Ledger; First, Bytes : Unsigned_64;
      Status : out Result)
   with Pre => Valid (Object),
        Post => Valid (Object) and
          Count (Object) = Count (Object)'Old + (if Status = Reserved then 1 else 0) and
          Preserves (Claims (Object)'Old, Claims (Object), Count (Object)'Old) and
          (if Status = Reserved then Last_Claim_Is (Object, First, Bytes));
   function Table_Size (Object : Ledger) return Unsigned_64;
   function Aperture_First (Object : Ledger) return Unsigned_64;
   function Aperture_Bytes (Object : Ledger) return Unsigned_64;
   function Space_Free
     (Object : Ledger; First, Bytes : Unsigned_64) return Boolean;
   -- Runtime read-only query as well as a contract predicate. False for an
   -- unadmitted ledger, outside its aperture, or overlap with retained claims.
   -- Read-only proposal, not admission or reservation. Caller must serialize
   -- search and subsequent Reserve/Publish against this same ledger. Never
   -- infer ownership from this result or from zero hardware PTEs.
   procedure Find_Free
     (Object : Ledger; Bytes, Alignment : Unsigned_64;
      First : out Unsigned_64; Found : out Boolean)
   with Pre => Valid (Object),
        Post => (if Found then Space_Free (Object, First, Bytes) else First = 0);
   -- Find and retain a claim in one owner operation. This is not a lock:
   -- callers must serialize access to the ledger, including publication.
   -- Failure never returns a usable address or changes existing claims.
   procedure Allocate
     (Object : in out Ledger; Bytes, Alignment : Unsigned_64;
      First : out Unsigned_64; Status : out Result)
   with Pre => Valid (Object),
        Post => Valid (Object) and
          Count (Object) = Count (Object)'Old + (if Status = Reserved then 1 else 0) and
          Preserves (Claims (Object)'Old, Claims (Object), Count (Object)'Old) and
          (if Status = Reserved then Last_Claim_Is (Object, First, Bytes)
           else First = 0);
private
   subtype Extent is Intel_GPU_VA_Placement.Extent;
   subtype Extents is Intel_GPU_VA_Placement.Extents (1 .. 64);
   type Claim_Model is record
      Values : Extents;
   end record;
   type Ledger is limited record
      Ready : Boolean := False;
      Table_Bytes : Unsigned_64 := 0;
      Aperture : Extent;
      Used : Natural range 0 .. 64 := 0;
      Claims : Extents;
   end record;
end Intel_GPU_GGTT_Reservations;
