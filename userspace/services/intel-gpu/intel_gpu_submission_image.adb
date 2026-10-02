with Intel_GPU_ADLN_PPGTT;
with Intel_GPU_ADLN_Context_Image;
with Intel_GPU_Initial_VM;
with Intel_GPU_ADLN_Offscreen_Surface;
with Intel_GPU_ADLN_MOCS;
with Intel_GPU_ADLN_Probe_Shaders;
with Intel_GPU_ADLN_Viewport;
with Intel_GPU_ADLN_Pixel_Blend;
with Intel_GPU_ADLN_Coarse_Pixel;
with Intel_GPU_ADLN_Color_Calc;
with Intel_GPU_ADLN_Offscreen_Batch;
with Intel_GPU_Memory_Copy_Command;
package body Intel_GPU_Submission_Image with SPARK_Mode is
   function Valid_Extent (DMA_Start, GGTT_Start : Unsigned_64) return Boolean is
     (Intel_GPU_Submission_Backing.Valid_Layout and then
      DMA_Start /= 0 and then DMA_Start mod 4096 = 0 and then
      DMA_Start <= 2 ** 32 - Byte_Count and then
      GGTT_Start /= 0 and then GGTT_Start mod 4096 = 0 and then
      GGTT_Start < 16#FEE00000# and then GGTT_Bytes <= 16#FEE00000# - GGTT_Start);
   function Build_For_VM (DMA_Start, GGTT_Start, Root_DMA : Unsigned_64) return Image is
      Pages : Backing_Pages;
      Empty : Image;
   begin
      if not Valid_Extent (DMA_Start, GGTT_Start) then return Empty; end if;
      for P in Pages'Range loop Pages (P) := DMA_Start + Unsigned_64 (P) * 4096; end loop;
      return Build_For_VM (Pages, GGTT_Start, Root_DMA);
   end Build_For_VM;
   function Build_For_VM
     (Pages : Backing_Pages; GGTT_Start, Root_DMA : Unsigned_64) return Image is
      Result : Image;
   begin
      if not Intel_GPU_Submission_Backing.Valid_Layout or else
        GGTT_Start = 0 or else GGTT_Start mod 4096 /= 0 or else
        GGTT_Start >= 16#FEE00000# or else GGTT_Bytes > 16#FEE00000# - GGTT_Start or else
        not Intel_GPU_ADLN_PPGTT.Valid_DMA_Page (Root_DMA)
      then return Result; end if;
      for P in Pages'Range loop
         if not Intel_GPU_ADLN_PPGTT.Valid_DMA_Page (Pages (P)) or else
           Pages (P) = Root_DMA then return Result; end if;
         for Q in Pages'First .. P loop
            if Q /= P and then Pages (Q) = Pages (P) then return Result; end if;
         end loop;
      end loop;
      declare
         Context : constant Intel_GPU_ADLN_Context_Image.Prepared_Image :=
           Intel_GPU_ADLN_Context_Image.Build
             (GGTT_Start, 65536, GGTT_Start + 65536, Root_DMA, 14);
      begin
         if not Context.Valid then return Result; end if;
         for I in Context.Words'Range loop Result.Words (I) := Context.Words (I); end loop;
         Result.Valid := True;
      end;
      return Result;
   end Build_For_VM;
   function Build (DMA_Start, GGTT_Start : Unsigned_64) return Image is
      Pages : Backing_Pages;
      Empty : Image;
   begin
      if not Valid_Extent (DMA_Start, GGTT_Start) then return Empty; end if;
      for P in Pages'Range loop Pages (P) := DMA_Start + Unsigned_64 (P) * 4096; end loop;
      return Build (Pages, GGTT_Start);
   end Build;
   function Build (Pages : Backing_Pages; GGTT_Start : Unsigned_64) return Image is
      use Intel_GPU_Submission_Backing;
      use Intel_GPU_Initial_VM;
      Result : Image;
      function Address (R : Region; Extra : Unsigned_64 := 0) return Unsigned_64 is
        (Pages (Natural ((Offsets (R) - First + Extra) / 4096)))
        with Pre => Extra < Sizes (R) and then Extra mod 4096 = 0;
   begin
      if not Valid_Layout or else
        GGTT_Start = 0 or else GGTT_Start mod 4096 /= 0 or else
        GGTT_Start >= 16#FEE00000# or else GGTT_Bytes > 16#FEE00000# - GGTT_Start
      then return Result; end if;
      for P in Pages'Range loop
         if not Intel_GPU_ADLN_PPGTT.Valid_DMA_Page (Pages (P)) then return Result; end if;
         for Q in Pages'First .. P loop
            if Q /= P and then Pages (Q) = Pages (P) then return Result; end if;
         end loop;
      end loop;
      declare
         Context : constant Intel_GPU_ADLN_Context_Image.Prepared_Image :=
           Intel_GPU_ADLN_Context_Image.Build
             (GGTT_Start, 65536, GGTT_Start + 65536,
              Address (PML4), 14);
         VM : constant Plan := Intel_GPU_Initial_VM.Build
           (Batch_VA,
            [Address (PML4), Address (PDPT), Address (PD), Address (PT)],
            [0 => Address (Batch_Buffer), 1 => Address (Completion_Page),
             2 => Address (Offscreen_Buffer), 3 => Address (Offscreen_Buffer, 4096),
             4 => Address (Offscreen_Buffer, 8192), 5 => Address (Offscreen_Buffer, 12288),
             6 => Address (Render_State_Page), 7 => Address (Shader_Page), others => 0]);
         Surface : constant Intel_GPU_ADLN_Offscreen_Surface.Image :=
           Intel_GPU_ADLN_Offscreen_Surface.Build
             (2 * Unsigned_32 (Intel_GPU_ADLN_MOCS.Uncached_Index));
         Draw : constant Intel_GPU_ADLN_Offscreen_Batch.Image :=
           Intel_GPU_ADLN_Offscreen_Batch.Build
             (2 * Unsigned_32 (Intel_GPU_ADLN_MOCS.Uncached_Index),
              Draw_URB_KiB, Draw_VS_Threads, Draw_PS_Threads);
         State_Index : constant Natural := Natural
           ((Offsets (Render_State_Page) - First) / 4);
         Shader_Index : constant Natural := Natural
           ((Offsets (Shader_Page) - First) / 4);
         package Shaders renames Intel_GPU_ADLN_Probe_Shaders;
         package Viewport renames Intel_GPU_ADLN_Viewport;
         package Blend renames Intel_GPU_ADLN_Pixel_Blend;
         package CPS renames Intel_GPU_ADLN_Coarse_Pixel;
         package Color_Calc renames Intel_GPU_ADLN_Color_Calc;
         CPS_Array : constant CPS.Array_Words := CPS.Initial_Array;
         Index : Natural;
         Entry_Value : Unsigned_64;
      begin
         if not Context.Valid or else not VM.Valid or else not Surface.Valid or else
           not Draw.Valid or else Draw.Count > (4096 - Draw_Batch_Offset) / 4
         then return Result; end if;
         Result.Words (State_Index) :=
           Intel_GPU_ADLN_Offscreen_Surface.Encode_Binding ((others => <>));
         for I in Surface.Words'Range loop
            Result.Words (State_Index + 16 + I) := Surface.Words (I);
         end loop;
         for I in Shaders.Vertices'Range loop
            Result.Words (State_Index + Shaders.Vertex_Data_Offset / 4 + I) :=
              Shaders.Vertices (I);
         end loop;
         for I in Viewport.SF'Range loop
            Result.Words (State_Index + Viewport.SF_Offset / 4 + I) :=
              Viewport.SF (I);
         end loop;
         for I in Viewport.CC'Range loop
            Result.Words (State_Index + Viewport.CC_Offset / 4 + I) :=
              Viewport.CC (I);
         end loop;
         for I in Shaders.Vertex'Range loop
            -- Shader page is distinct from all dynamic-state allocations.
            Result.Words (Shader_Index + Shaders.Vertex_Offset / 4 + I) :=
              Shaders.Vertex (I);
         end loop;
         for I in Shaders.Fragment'Range loop
            Result.Words (Shader_Index + Shaders.Fragment_Offset / 4 + I) :=
              Shaders.Fragment (I);
         end loop;
         for I in Blend.State'Range loop
            Result.Words (State_Index + Blend.State_Offset / 4 + I) :=
              Blend.State (I);
         end loop;
         for I in CPS_Array'Range loop
            Result.Words (State_Index + CPS.State_Offset / 4 + I) := CPS_Array (I);
         end loop;
         for I in Color_Calc.State'Range loop
            Result.Words (State_Index + Color_Calc.State_Offset / 4 + I) := Color_Calc.State (I);
         end loop;
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
         Result.Words (24576 .. 24579) :=
           [16#10000002#, Unsigned_32 (Completion_VA), 0,
            Batch_Probe_Value];
         declare
            Copy : constant Intel_GPU_Memory_Copy_Command.Command :=
              Intel_GPU_Memory_Copy_Command.Build
                (Completion_VA + Copy_Source_Offset,
                 Completion_VA + Copy_Result_Offset);
         begin
            for I in Copy.Words'Range loop
               Result.Words (24580 + I) := Copy.Words (I);
            end loop;
         end;
         Result.Words (24585) := 16#05000000#;
         for I in 0 .. Draw.Count - 1 loop
            Result.Words (24576 + Draw_Batch_Offset / 4 + I) := Draw.Data (I);
         end loop;
         Result.Valid := True;
      end;
      return Result;
   end Build;
end Intel_GPU_Submission_Image;
