with Intel_GPU_ADLN_Context_Image;
with Intel_GPU_Initial_VM;
package body Intel_GPU_Submission_Image with SPARK_Mode is
   function Build (DMA_Base, GGTT_Start : Unsigned_64) return Image is
      use Intel_GPU_Submission_Backing;
      use Intel_GPU_Initial_VM;
      Result : Image;
   begin
      if not Valid_Layout or else DMA_Base = 0 or else DMA_Base mod 4096 /= 0 or else
        DMA_Base > 2 ** 32 - 1024 * 1024 or else
        GGTT_Start = 0 or else GGTT_Start mod 4096 /= 0 or else
        GGTT_Start >= 16#FEE00000# or else GGTT_Bytes > 16#FEE00000# - GGTT_Start
      then return Result; end if;
      declare
         Context : constant Intel_GPU_ADLN_Context_Image.Prepared_Image :=
           Intel_GPU_ADLN_Context_Image.Build
             (GGTT_Start, 65536, GGTT_Start + 65536,
              DMA_Base + Offsets (PML4), 14);
         VM : constant Plan := Intel_GPU_Initial_VM.Build
           (Batch_VA,
            [DMA_Base + Offsets (PML4), DMA_Base + Offsets (PDPT),
             DMA_Base + Offsets (PD), DMA_Base + Offsets (PT)],
            [0 => DMA_Base + Offsets (Batch_Buffer),
             1 => DMA_Base + Offsets (Completion_Page), others => 0]);
         Index : Natural;
         Entry_Value : Unsigned_64;
      begin
         if not Context.Valid or else not VM.Valid then return Result; end if;
         for I in Context.Words'Range loop Result.Words (I) := Context.Words (I); end loop;
         for L in Level loop
            for I in VM.Entries (L)'Range loop
               Index := 20480 + Level'Pos (L) * 1024 + I * 2;
               Entry_Value := VM.Entries (L) (I);
               Result.Words (Index) := Unsigned_32 (Entry_Value and 16#FFFFFFFF#);
               Result.Words (Index + 1) := Unsigned_32 (Shift_Right (Entry_Value, 32));
            end loop;
         end loop;
         -- Gen8+ MI_STORE_DWORD_IMM, four DWORDs, MI_USE_GGTT deliberately
         -- clear: destination belongs to this context's private PPGTT.
         -- Fixed driver-owned probe, not arbitrary client command admission.
         Result.Words (24576 .. 24580) :=
           [16#10000002#, Unsigned_32 (Completion_VA), 0,
            Batch_Probe_Value, 16#05000000#];
         Result.Valid := True;
      end;
      return Result;
   end Build;
end Intel_GPU_Submission_Image;
