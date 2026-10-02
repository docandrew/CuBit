with Interfaces; use Interfaces;
with Intel_GPU_ADLN_TLB_Invalidate;
with Intel_GPU_GuC_MMIO_Invalidate;
generic
   -- Serialized ADL-N RCS-only cleanup, after consumer drain and visible PTE
   -- replacement. Gate includes power/forcewake and excludes new publishers.
   with function Gate return Boolean;
   with procedure Write_Engine (Offset, Value : Unsigned_32; OK : out Boolean);
   with procedure Read_Engine (Offset : Unsigned_32; Value : out Unsigned_32; OK : out Boolean);
   with procedure Write_GuC (Value : Unsigned_32; OK : out Boolean);
   with procedure Read_GuC (Value : out Unsigned_32; OK : out Boolean);
   with procedure Clock_US (Value : out Unsigned_64; OK : out Boolean);
package Intel_GPU_Retirement_Invalidate is
   type Result is (Rejected, Engine_Failed, GuC_Failed, Ownership_Lost, Complete);
   type Attempt is limited private;
   -- One attempt: engine/OA completion precedes GuC completion. Never run
   -- GuC invalidation after an uncertain engine result. No implicit retry or
   -- backing release; even Complete is only translation-retirement evidence.
   procedure Execute (Object : in out Attempt; Status : out Result;
                      Poll_Limit : Positive := 4096);
private
   package Engine is new Intel_GPU_ADLN_TLB_Invalidate
     (Gate, Write_Engine, Read_Engine, Clock_US);
   package GuC is new Intel_GPU_GuC_MMIO_Invalidate
     (Gate, Write_GuC, Read_GuC, Clock_US);
   type Attempt is limited record
      Used : Boolean := False;
      Engine_Attempt : Engine.Attempt;
      GuC_Attempt : GuC.Attempt;
   end record;
end Intel_GPU_Retirement_Invalidate;
