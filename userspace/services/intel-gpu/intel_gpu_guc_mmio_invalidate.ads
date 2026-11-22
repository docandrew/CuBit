with Interfaces; use Interfaces;
generic
   -- Trusted, serialized ownership AND quiescence gate: all prior accesses
   -- flushed/drained, PTE stores visible, required power/forcewake held.
   -- Gate checks do not establish those hardware conditions themselves.
   -- All callbacks bounded/nonraising; no reentry into this attempt.
   with function Gate return Boolean;
   with procedure Write_Request (Value : Unsigned_32; OK : out Boolean);
   with procedure Read_Status (Value : out Unsigned_32; OK : out Boolean);
   with procedure Clock_US (Value : out Unsigned_64; OK : out Boolean);
package Intel_GPU_GuC_MMIO_Invalidate is
   type Result is (Rejected, Complete, Ownership_Lost, Write_Failed,
                   Read_Failed, Invalid_Clock, Timed_Out);
   type Attempt is limited private;
   -- One CEE8 request, then field-only polling. Never infer completion from
   -- idle before our write. Four millisecond deadline and finite poll cap.
   -- Any failure retains old backing/mappings; no replay of this attempt.
   -- Completion alone does not establish GPU/CPU grant retirement or free RAM.
   procedure Execute (Object : in out Attempt; Status : out Result;
                      Poll_Limit : Positive := 4096);
private
   type Attempt is limited record
      Used : Boolean := False;
   end record;
end Intel_GPU_GuC_MMIO_Invalidate;
