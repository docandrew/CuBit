package body Intel_GPU_GGTT_Search is
   use Interfaces;
   Last : Evidence;
   function Last_Evidence return Evidence is (Last);
   procedure Find
     (Admitted : Boolean;
      Table_Bytes, First, Length, Bytes, Alignment : Unsigned_64;
      Selected : out Unsigned_64; Status : out Result)
   is
      Limit, Candidate, Cursor, Value : Unsigned_64;
      OK : Boolean;
      function Align (Address : Unsigned_64) return Unsigned_64 is
        ((Address + Alignment - 1) and not (Alignment - 1));
   begin
      Last := (others => <>);
      Selected := 0; Status := Rejected;
      if not Admitted or else Table_Bytes not in 2_097_152 | 4_194_304 | 8_388_608 or else
        First = 0 or else First mod 4096 /= 0 or else Length = 0 or else
        Length mod 4096 /= 0 or else First >= Table_Bytes / 8 * 4096 or else
        Length > Table_Bytes / 8 * 4096 - First or else
        Bytes = 0 or else Bytes mod 4096 /= 0 or else Bytes > 16 * 1024 * 1024 or else
        Alignment < 4096 or else Alignment > 16 * 1024 * 1024 or else
        (Alignment and (Alignment - 1)) /= 0
      then return; end if;
      -- Validated addresses end at <=4GiB, so alignment rounding cannot wrap.
      Limit := First + Length;
      Candidate := Align (First);
      Status := Exhausted;
      Last.Outcome := Exhausted;
      while Candidate < Limit and then Bytes <= Limit - Candidate loop
         Cursor := Candidate;
         while Cursor - Candidate < Bytes loop
            -- A retained software reservation can have zero hardware PTEs.
            -- Skip it without even reading MMIO; zero never overrides a claim.
            if not Page_Available (Cursor) then
               Last.Blocked := Last.Blocked + 1;
               exit;
            end if;
            Read_PTE (Cursor / 4096, Value, OK);
            Last.Reads := Last.Reads + 1;
            if not OK or else Value = Unsigned_64'Last then
               Last.Outcome := Read_Failed;
               Status := Read_Failed; return;
            end if;
            if Value /= 0 then
               if Last.Nonzero = 0 then
                  Last.First_Nonzero_Index := Cursor / 4096;
                  Last.First_Nonzero_Value := Value;
               end if;
               Last.Nonzero := Last.Nonzero + 1;
               exit;
            end if;
            Cursor := Cursor + 4096;
         end loop;
         if Cursor - Candidate = Bytes then
            Last.Outcome := Found;
            Selected := Candidate; Status := Found; return;
         end if;
         -- Nothing beginning before this occupied entry can satisfy the
         -- requested contiguous run. Skip it and any alignment padding.
         Candidate := Align (Cursor + 4096);
      end loop;
   end Find;
end Intel_GPU_GGTT_Search;
