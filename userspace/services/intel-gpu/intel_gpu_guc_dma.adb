package body Intel_GPU_GuC_DMA is
   use Interfaces;
   Control : constant Unsigned_32 := 16#C314#;
   function Current (Object : Attempt) return Phase is (Object.Value);
   procedure Execute
     (Object : in out Attempt;
      Source, Bytes, WOPCM_Bytes : Unsigned_64;
      Poll_Limit : Positive; Status : out Result)
   is
      Value : Unsigned_32;
      First, Previous, Stamp : Unsigned_64;
   begin
      Status := Rejected;
      if Object.Value /= Fresh then return; end if;
      Object.Value := Consumed;
      if Source >= 2 ** 32 or else Source mod 4096 /= 0 or else
        Bytes <= 128 or else Bytes mod 4 /= 0 or else
        Bytes > 2 ** 32 - Source or else
        WOPCM_Bytes > 8 * 1024 * 1024 or else
        WOPCM_Bytes < 24 * 1024 or else
        Bytes > WOPCM_Bytes - 24 * 1024 then return; end if;
      Value := Read32 (Control);
      Status := Invalid_MMIO;
      if Value = Unsigned_32'Last then return; end if;
      Status := Busy;
      if (Value and 1) /= 0 then return; end if;
      First := Now;
      Status := Invalid_Clock;
      if First = Unsigned_64'Last then return; end if;
      Previous := First;
      -- Latch before any writes; caller retains mapping/backing on ambiguity.
      Object.Value := Quarantined;
      Write32 (16#C300#, Unsigned_32 (Source));
      Write32 (16#C304#, 0);
      Write32 (16#C308#, 16#2000#);
      Write32 (16#C30C#, 16#70000#);
      Write32 (16#C310#, Unsigned_32 (Bytes));
      Write32 (Control, 16#0011_0011#);
      Status := Timed_Out;
      for Poll in 1 .. Poll_Limit loop
         Value := Read32 (Control);
         if Value = Unsigned_32'Last then
            Status := Invalid_MMIO; exit;
         end if;
         Stamp := Now;
         if Stamp = Unsigned_64'Last or else Stamp < Previous then
            Status := Invalid_Clock; exit;
         end if;
         Previous := Stamp;
         if Stamp - First >= 100_000 then exit; end if;
         if (Value and 1) = 0 then Status := Complete; exit; end if;
         if Poll < Poll_Limit then Pause; end if;
      end loop;
      -- Clear UOS_MOVE, not START_DMA: this is NOT a DMA cancellation.
      Write32 (Control, 16#0010_0000#);
      Value := Read32 (Control);
      if Value = Unsigned_32'Last or else (Value and 16#10#) /= 0 then
         Status := Cleanup_Failed;
      elsif Status = Complete and then (Value and 1) /= 0 then
         Status := Cleanup_Failed;
      end if;
      if Status = Complete then Object.Value := Transferred; end if;
   end Execute;
end Intel_GPU_GuC_DMA;
