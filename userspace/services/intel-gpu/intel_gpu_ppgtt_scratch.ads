with Interfaces; use Interfaces;
with Intel_GPU_ADLN_PPGTT;
package Intel_GPU_PPGTT_Scratch with SPARK_Mode is
   -- Private per-VM fallback hierarchy, following Linux v6.16
   -- gen8_ppgtt.c:gen8_init_scratch. No allocation or publication here.
   -- Owner must exclusively retain four distinct DMA pages: a zeroed data
   -- page, then PT, PD and PDP. Initialize each table's 512 words with Fill.
   -- A root's unused entries point at PDP via Fallback (..., 3).
   -- Writable scratch is NOT immutable zero/discard semantics. Never share
   -- it across protection domains, or use it as an application's real BO.
   -- Backing authority, zeroing, cache visibility and TLB completion remain
   -- caller obligations; numeric validation cannot establish ownership.
   subtype Level is Natural range 0 .. 3;
   subtype Table_Level is Level range 1 .. 3;
   type Backing_Pages is array (Level) of Unsigned_64;
   function Valid (Pages : Backing_Pages) return Boolean is
     ((for all L in Level => Intel_GPU_ADLN_PPGTT.Valid_DMA_Page (Pages (L)))
      and then (for all L in Level =>
        (for all R in Level => (if L /= R then Pages (L) /= Pages (R)))));
   function Contains (Pages : Backing_Pages; DMA : Unsigned_64) return Boolean is
     (for some Page of Pages => Page = DMA);
   function Fallback (Pages : Backing_Pages; L : Level) return Unsigned_64 is
     (if not Valid (Pages) then 0
      elsif L = 0 then Intel_GPU_ADLN_PPGTT.Encode_Leaf
        (Pages (0), Intel_GPU_ADLN_PPGTT.Uncached, Intel_GPU_ADLN_PPGTT.Read_Write)
      else Intel_GPU_ADLN_PPGTT.Encode_Directory (Pages (L)))
     with Post => (if not Valid (Pages) then Fallback'Result = 0
                   else Fallback'Result / 4096 = Pages (L) / 4096);
   function Fill (Pages : Backing_Pages; L : Table_Level) return Unsigned_64 is
     (Fallback (Pages, L - 1));
   -- L=1: PT entries -> data; L=2: PD -> PT; L=3: PDP -> PD.
end Intel_GPU_PPGTT_Scratch;
