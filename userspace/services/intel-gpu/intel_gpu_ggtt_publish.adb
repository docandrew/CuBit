package body Intel_GPU_GGTT_Publish is
   use Interfaces;
   function Current (Object : Attempt) return Phase is (Object.Value);
   function Search_Detail (Object : Attempt) return Search_Evidence is (Object.Search);
   function Valid_Backing (Base, Bytes : Unsigned_64) return Boolean is
   begin
      if Bytes = 0 or else Bytes > Maximum_Bytes or else
        Bytes > 16 * 1024 * 1024 or else Bytes mod 4096 /= 0
      then return False; end if;
      for Page in Unsigned_64 range 0 .. Bytes / 4096 - 1 loop
         if Intel_GPU_GGTT.Encode_System_Page (Resolve_Page (Base, Page * 4096)) = 0 then
            return False;
         end if;
      end loop;
      return True;
   end Valid_Backing;
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
      First : constant Unsigned_64 := Intel_GPU_GGTT_Reservations.Aperture_First (Reservations);
      Length : constant Unsigned_64 := Intel_GPU_GGTT_Reservations.Aperture_Bytes (Reservations);
      Candidate, Cursor, Limit : Unsigned_64;
      function Align (Value : Unsigned_64) return Unsigned_64 is
        ((Value + Alignment - 1) and not (Alignment - 1));
   begin
      Selected_Start := 0;
      Status := Rejected;
      if Object.Value /= Fresh then return; end if;
      if not Valid_Backing (DMA_Start, Bytes) or else
        Alignment < 4096 or else Alignment > 16 * 1024 * 1024 or else
        (Alignment and (Alignment - 1)) /= 0 or else Length = 0
      then Object.Value := Consumed_No_Writes; return; end if;
      -- The admitted ledger, NOT old PTE values, defines free GPU VA.
      -- Admission bounds the interval to <=4GiB, so alignment cannot wrap.
      Limit := First + Length;
      Candidate := Align (First);
      Object.Search.Outcome := Search_Exhausted;
      while Candidate < Limit and then Bytes <= Limit - Candidate loop
         Cursor := Candidate;
         while Cursor - Candidate < Bytes loop
            exit when not Page_Available (Cursor);
            Cursor := Cursor + 4096;
         end loop;
         if Cursor - Candidate = Bytes then
            Selected_Start := Candidate;
            Object.Search.Outcome := Search_Found;
            Publish (Object, Reservations, Selected_Start, DMA_Start, Bytes, Status);
            return;
         end if;
         Object.Search.Blocked := Object.Search.Blocked + 1;
         Candidate := Align (Cursor + 4096);
      end loop;
      Object.Value := Consumed_No_Writes;
      Status := Reservation_Failed;
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
      if not Plan.Valid or else not Valid_Backing (DMA_Start, Bytes)
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
         Object.Search.Reads := Object.Search.Reads + 1;
         if not OK or else Value = Unsigned_64'Last then Status := Read_Failed; return; end if;
         -- Readability is checked before destructive publication. Nonzero
         -- inherited contents are evidence, not allocation ownership.
         if Value /= 0 then
            if Object.Search.Nonzero = 0 then
               Object.Search.First_Nonzero_Index := Plan.First_Entry + Offset;
               Object.Search.First_Nonzero_Value := Value;
            end if;
            Object.Search.Nonzero := Object.Search.Nonzero + 1;
         end if;
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
         Expected := Intel_GPU_GGTT.Encode_System_Page (Resolve_Page (DMA_Start, Offset * 4096));
         Write_PTE (Plan.First_Entry + Offset, Expected, OK);
         if not OK then return; end if;
      end loop;
      -- Readback is distinct from translation invalidation and DMA coherence.
      for Offset in Unsigned_64 range 0 .. Plan.Entry_Count - 1 loop
         Expected := Intel_GPU_GGTT.Encode_System_Page (Resolve_Page (DMA_Start, Offset * 4096));
         Read_PTE (Plan.First_Entry + Offset, Value, OK);
         if not OK or else Value /= Expected then return; end if;
      end loop;
      Invalidate (OK);
      if OK and then Range_Allowed (GPU_Start, Bytes) then
         Object.Value := Complete;
         Status := Published;
      end if;
   end Publish;
end Intel_GPU_GGTT_Publish;
