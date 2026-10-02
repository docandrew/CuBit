package body Intel_GPU_GGTT_Retire is
   procedure Execute
     (Object : in out Attempt; Ledger : Intel_GPU_GGTT_Reservations.Ledger;
      First, DMA_Base, Bytes, Scratch_DMA : Unsigned_64; Status : out Result) is
      Plan : constant Intel_GPU_GGTT.Window := Intel_GPU_GGTT.Plan_Window
        (Intel_GPU_GGTT_Reservations.Table_Size (Ledger), First, Bytes);
      Scratch : constant Unsigned_64 := Intel_GPU_GGTT.Encode_System_Page (Scratch_DMA);
      Expected, Value : Unsigned_64;
      OK : Boolean;
   begin
      Status := Rejected;
      if Object.Used then return; end if;
      Object.Used := True;
      if Bytes = 0 or else Bytes > 16 * 1024 * 1024 or else Bytes mod 4096 /= 0 or else
        not Plan.Valid or else Scratch = 0 or else
        not Intel_GPU_GGTT_Reservations.Has_Claim (Ledger, First, Bytes) or else
        not Gate (First, Bytes)
      then return; end if;
      for Page in Unsigned_64 range 0 .. Plan.Entry_Count - 1 loop
         Expected := Intel_GPU_GGTT.Encode_System_Page (Resolve_Page (DMA_Base, Page * 4096));
         if Expected = 0 or else Expected = Scratch or else not Gate (First, Bytes) then return; end if;
         Read_PTE (Plan.First_Entry + Page, Value, OK);
         if not OK or else Value /= Expected or else not Gate (First, Bytes) then return; end if;
      end loop;
      -- Mark uncertainty BEFORE the first possibly posted write. Never claim
      -- the old mapping survives a callback reporting failure after issuance.
      Status := Quarantined;
      for Page in Unsigned_64 range 0 .. Plan.Entry_Count - 1 loop
         if not Gate (First, Bytes) then return; end if;
         Write_PTE (Plan.First_Entry + Page, Scratch, OK);
         if not OK or else not Gate (First, Bytes) then return; end if;
      end loop;
      for Page in Unsigned_64 range 0 .. Plan.Entry_Count - 1 loop
         if not Gate (First, Bytes) then return; end if;
         Read_PTE (Plan.First_Entry + Page, Value, OK);
         if not OK or else Value /= Scratch or else not Gate (First, Bytes) then return; end if;
      end loop;
      Invalidate_And_Wait (OK);
      if OK and then Gate (First, Bytes) then Status := Detached; end if;
   end Execute;
end Intel_GPU_GGTT_Retire;
