with Interfaces; use Interfaces;
generic
   -- ADL-N RCS-only owner. Caller HOLDS reset/invalidation serialization and
   -- forcewake throughout; all users of the affected VM are excluded/drained,
   -- pipeline memory accesses flushed, and updated PTEs already visible.
   -- Other engines must not access this VM; OA collection must be inactive
   -- with no outstanding OA memory accesses. Gate checks do not acquire locks
   -- or prove those preconditions. All callbacks must be bounded/nonraising.
   with function Gate return Boolean;
   with procedure Write_Register (Offset, Value : Unsigned_32; OK : out Boolean);
   with procedure Read_Register (Offset : Unsigned_32; Value : out Unsigned_32;
                                 OK : out Boolean);
   with procedure Clock_US (Value : out Unsigned_64; OK : out Boolean);
package Intel_GPU_ADLN_TLB_Invalidate is
   type Result is (Rejected, Complete, Ownership_Lost, Write_Failed,
                   Read_Failed, Invalid_Clock, Timed_Out);
   type Attempt is limited private;
   procedure Execute (Object : in out Attempt; Status : out Result;
                      Poll_Limit : Positive := 4096);
   -- One attempt, two requests (GFX then OA), then bounded field-only polling.
   -- Four millisecond deadline plus poll cap protects a stalled clock source.
   -- Failure after any request is uncertain: retain both VM generations and
   -- keep submission disabled. Success alone does not grant memory ownership.
private
   type Attempt is limited record
      Used : Boolean := False;
   end record;
end Intel_GPU_ADLN_TLB_Invalidate;
