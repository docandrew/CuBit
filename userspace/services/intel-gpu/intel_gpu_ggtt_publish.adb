with Intel_GPU_GGTT;
with Intel_GPU_GGTT_Search;
package body Intel_GPU_GGTT_Publish is
   use Interfaces;
   function Current (Object : Attempt) return Phase is (Object.Value);
   function Search_Detail (Object : Attempt) return Search_Evidence is (Object.Search);
   procedure Publish_Available
     (Object : in out Attempt;
      Reservations : in out Intel_GPU_GGTT_Reservations.Ledger;
      DMA_Start, Bytes, Alignment : Unsigned_64;
      Selected_Start : out Unsigned_64;
      Status : out Result)
   is
      function Page_Available (Address : Unsigned_64) return Boolean is
        (Intel_GPU_GGTT_Reservations.Space_Free (Reservations, Address, 4096)
         and then Range_Allowed (Address, 4096));
      package Search is new Intel_GPU_GGTT_Search (Page_Available, Read_PTE);
      Search_Status : Search.Result;
      use type Search.Result;
   begin
      Selected_Start := 0;
      Status := Rejected;
      if Object.Value /= Fresh then return; end if;
      if Bytes = 0 or else Bytes > Maximum_Bytes or else Bytes > 16 * 1024 * 1024 or else
        Bytes mod 4096 /= 0 or else Intel_GPU_GGTT.Encode_System_Page (DMA_Start) = 0 or else
        DMA_Start > 2 ** 32 - Bytes
      then Object.Value := Consumed_No_Writes; return; end if;
      Search.Find
        (True, Intel_GPU_GGTT_Reservations.Table_Size (Reservations),
         Intel_GPU_GGTT_Reservations.Aperture_First (Reservations),
         Intel_GPU_GGTT_Reservations.Aperture_Bytes (Reservations),
         Bytes, Alignment, Selected_Start, Search_Status);
      declare
         Detail : constant Search.Evidence := Search.Last_Evidence;
      begin
         Object.Search :=
           ((case Detail.Outcome is
                when Search.Rejected => Search_Rejected,
                when Search.Read_Failed => Search_Read_Failed,
                when Search.Exhausted => Search_Exhausted,
                when Search.Found => Search_Found),
            Detail.Reads, Detail.Blocked, Detail.Nonzero,
            Detail.First_Nonzero_Index, Detail.First_Nonzero_Value);
      end;
      if Search_Status /= Search.Found then
         Object.Value := Consumed_No_Writes;
         Status := (case Search_Status is
                      when Search.Read_Failed => Read_Failed,
                      when Search.Rejected => Rejected,
                      when others => Reservation_Failed);
         return;
      end if;
      Publish (Object, Reservations, Selected_Start, DMA_Start, Bytes, Status);
   end Publish_Available;
   procedure Publish
     (Object : in out Attempt;
      Reservations : in out Intel_GPU_GGTT_Reservations.Ledger;
      GPU_Start, DMA_Start, Bytes : Unsigned_64;
      Status : out Result)
   is
      Plan : constant Intel_GPU_GGTT.Window :=
        Intel_GPU_GGTT.Plan_Window
          (Intel_GPU_GGTT_Reservations.Table_Size (Reservations), GPU_Start, Bytes);
      Claim : Intel_GPU_GGTT_Reservations.Result;
      use type Intel_GPU_GGTT_Reservations.Result;
      Value, Expected : Unsigned_64;
      OK : Boolean;
   begin
      Status := Rejected;
      if Object.Value /= Fresh then return; end if;
      Object.Value := Consumed_No_Writes;
      -- Bounded retained firmware/runtime window. Whole retained pages only;
      -- no implicit tail coverage beyond the caller's backing allocation.
      if not Plan.Valid or else Bytes = 0 or else Bytes > Maximum_Bytes or else
        Bytes > 16 * 1024 * 1024 or else
        Bytes mod 4096 /= 0 or else
        Intel_GPU_GGTT.Encode_System_Page (DMA_Start) = 0 or else
        DMA_Start > 2 ** 32 - Bytes
      then return; end if;
      if not Range_Allowed (GPU_Start, Bytes) then
         Status := Protected_Range; return;
      end if;
      Intel_GPU_GGTT_Reservations.Reserve (Reservations, GPU_Start, Bytes, Claim);
      if Claim /= Intel_GPU_GGTT_Reservations.Reserved then
         Status := Reservation_Failed; return;
      end if;
      for Offset in Unsigned_64 range 0 .. Plan.Entry_Count - 1 loop
         Read_PTE (Plan.First_Entry + Offset, Value, OK);
         if not OK then Status := Read_Failed; return; end if;
         if Value /= 0 then Status := Occupied; return; end if;
      end loop;
      Prepare_Buffer (GPU_Start, Bytes, OK);
      if not OK then Status := Prepare_Failed; return; end if;
      if not Range_Allowed (GPU_Start, Bytes) then
         Status := Protected_Range; return;
      end if;
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
