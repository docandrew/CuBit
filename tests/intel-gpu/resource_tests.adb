with Interfaces; use Interfaces;
with Intel_GPU_Probe; use Intel_GPU_Probe;
with Intel_GPU_Resources; use Intel_GPU_Resources;
procedure Resource_Tests is
   BAR : BAR_Result := Decode_BAR (4, 16#60#, True);
   Plan : Mapping_Plan;
   function Check (Size, Offset, Bytes : Unsigned_64) return Admission_Status is
     (Plan_Registers (Alder_Lake_N, 2, BAR, Size, Offset, Bytes).Status);
begin
   Plan := Plan_ADLN_Registers (Alder_Lake_N, 2, 4, 16#60#, 0, 4096);
   pragma Assert (Plan.Status = Admitted and Plan.Physical_Base = 16#60_0000_0000#);
   pragma Assert (Plan_ADLN_Registers (Kaby_Lake_ULT_GT2, 2, 4, 16#60#, 0, 4096).Status = Unknown_Platform);
   pragma Assert (Plan_ADLN_Registers (Alder_Lake_N, 2, 4, 16#80#, 0, 4096).Status = Invalid_BAR);
   pragma Assert (Plan_ADLN_Registers (Alder_Lake_N, 2, 16#1004#, 16#60#, 0, 4096).Status = Invalid_BAR);
   for Page in Unsigned_64 range 0 .. 4095 loop
      -- Reject the reserved region and GGTT even though they lie in BAR0.
      pragma Assert
        ((Plan_ADLN_Registers (Alder_Lake_N, 2, 4, 16#60#, Page * 4096, 4096).Status = Admitted)
          = (Page < 512));
   end loop;
   -- NUC Linux inventory base, not a CuBit-authorized resource assignment.
   Plan := Plan_Registers (Alder_Lake_N, 2, BAR, 16#100_0000#, 4096, 4096);
   pragma Assert (Plan.Status = Admitted);
   pragma Assert (Plan.Physical_Base = 16#60_0000_1000#);
   pragma Assert (Check (0, 0, 4096) = Unknown_Extent);
   pragma Assert (Check (4096, 0, 0) = Invalid_Alignment);
   pragma Assert (Check (4096, 1, 4096) = Invalid_Alignment);
   pragma Assert (Check (4097, 0, 4096) = Invalid_Alignment);
   pragma Assert (Check (4096, 4096, 4096) = Outside_Resource);
   pragma Assert (Plan_Registers (Unrecognized, 2, BAR, 4096, 0, 4096).Status = Unknown_Platform);
   for Command in Unsigned_16 range 0 .. 7 loop
      pragma Assert
        ((Plan_Registers (Alder_Lake_N, Command, BAR, 4096, 0, 4096).Status = Admitted)
          = ((Command and 2) /= 0));
   end loop;
   for Pages in Unsigned_64 range 1 .. 16 loop
      for Offset in Unsigned_64 range 0 .. 17 loop
         for Count in Unsigned_64 range 1 .. 17 loop
            pragma Assert ((Check (Pages * 4096, Offset * 4096, Count * 4096) = Admitted)
              = (Offset < Pages and then Count <= Pages - Offset));
         end loop;
      end loop;
   end loop;
   BAR.Prefetchable := True;
   pragma Assert (Check (4096, 0, 4096) = Wrong_Cache_Class);
   BAR := (Memory_64, Unsigned_64'Last - 4095, False, 2);
   pragma Assert (Check (4096, 0, 4096) = Admitted);
   pragma Assert (Check (8192, 0, 4096) = Address_Overflow);
   BAR.Status := IO_Space;
   pragma Assert (Check (4096, 0, 4096) = Invalid_BAR);
end Resource_Tests;
