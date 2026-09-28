with Intel_GPU_GGTT;
package body Intel_GPU_GGTT_Publish is
   use Interfaces;
   function Current (Object : Attempt) return Phase is (Object.Value);
   procedure Publish
     (Object : in out Attempt;
      Table_Bytes, GPU_Start, DMA_Start, Bytes : Unsigned_64;
      Status : out Result)
   is
      Plan : constant Intel_GPU_GGTT.Window :=
        Intel_GPU_GGTT.Plan_Window (Table_Bytes, GPU_Start, Bytes);
      Value, Expected : Unsigned_64;
      OK : Boolean;
   begin
      Status := Rejected;
      if Object.Value /= Fresh then return; end if;
      Object.Value := Consumed_No_Writes;
      -- Bounded initial firmware upload window. Whole retained pages only;
      -- no implicit tail coverage beyond the caller's backing allocation.
      if not Plan.Valid or else Bytes = 0 or else Bytes > 1024 * 1024 or else
        Bytes mod 4096 /= 0 or else
        Intel_GPU_GGTT.Encode_System_Page (DMA_Start) = 0 or else
        DMA_Start > 2 ** 32 - Bytes
      then return; end if;
      for Offset in Unsigned_64 range 0 .. Plan.Entry_Count - 1 loop
         Read_PTE (Plan.First_Entry + Offset, Value, OK);
         if not OK then Status := Read_Failed; return; end if;
         if Value /= 0 then Status := Occupied; return; end if;
      end loop;
      Prepare_Buffer (OK);
      if not OK then Status := Prepare_Failed; return; end if;
      -- Conservative BEFORE the first MMIO callback, even if it reports failure.
      Status := Quarantined;
      Object.Value := Possibly_Published;
      for Offset in Unsigned_64 range 0 .. Plan.Entry_Count - 1 loop
         Expected := Intel_GPU_GGTT.Encode_System_Page (DMA_Start + Offset * 4096);
         Write_PTE (Plan.First_Entry + Offset, Expected, OK);
         if not OK then return; end if;
      end loop;
      -- Readback is distinct from translation invalidation and DMA coherence.
      for Offset in Unsigned_64 range 0 .. Plan.Entry_Count - 1 loop
         Expected := Intel_GPU_GGTT.Encode_System_Page (DMA_Start + Offset * 4096);
         Read_PTE (Plan.First_Entry + Offset, Value, OK);
         if not OK or else Value /= Expected then return; end if;
      end loop;
      Invalidate (OK);
      if OK then
         Object.Value := Complete;
         Status := Published;
      end if;
   end Publish;
end Intel_GPU_GGTT_Publish;
