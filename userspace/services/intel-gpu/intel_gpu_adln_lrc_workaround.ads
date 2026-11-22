with Interfaces; use Interfaces;
package Intel_GPU_ADLN_LRC_Workaround with SPARK_Mode is
   type Batch_Words is array (Natural range 0 .. 31) of Unsigned_32;
   type Indirect_Batch is record
      Valid : Boolean := False;
      Words : Batch_Words := [others => 0];
   end record;
   -- ADL-N RCS0 only, main GT (GSI offset0), no flat CCS.
   -- Context_GPU names retained GGTT backing of the FULL context including
   -- HWSP; Capacity must cover14 saved-state pages +2 workaround pages.
   -- Numeric assembly only. No allocation, mapping or execution is performed.
   function Admissible (Context_GPU, Capacity : Unsigned_64) return Boolean is
     (Context_GPU /= 0 and then Context_GPU mod 4096 = 0 and then
      Context_GPU < 16#FEE0_0000# and then Capacity >= 16 * 4096 and then
      16 * 4096 <= 16#FEE0_0000# - Context_GPU);
   function Build (Context_GPU, Capacity : Unsigned_64) return Indirect_Batch
     with Post => Build'Result.Valid = Admissible (Context_GPU, Capacity)
       and then (if not Build'Result.Valid then
       (for all Word of Build'Result.Words => Word = 0));
   -- Install at context+14*4096. Indirect pointer encodes address OR2 cachelines;
   -- register23 gets13*64. Separate per-context page still needs initialization.
   -- No MI_BATCH_BUFFER_END here: INDIRECT_CTX is length-delimited.
end Intel_GPU_ADLN_LRC_Workaround;
