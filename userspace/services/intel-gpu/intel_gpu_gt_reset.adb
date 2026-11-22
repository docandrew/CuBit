package body Intel_GPU_GT_Reset is
   use Interfaces;
   function Current (Object : Attempt) return State is (Object.Value);
   procedure Execute (Object : in out Attempt; Poll_Limit : Positive;
                      Status : out Result) is
      Started, Last : Unsigned_64;
      Value : Unsigned_32;
      Acked, Settled : Boolean;
      function Sample_Time return Boolean is
         Time : constant Unsigned_64 := Now;
      begin
         if Time = Unsigned_64'Last or else Time < Last then
            Status := Invalid_Clock; return False;
         end if;
         Last := Time;
         return True;
      end Sample_Time;
      function Within_Reset_Budget return Boolean is
      begin
         if not Sample_Time then return False; end if;
         if Last - Started >= 2_000 then Status := Timed_Out; return False; end if;
         return True;
      end Within_Reset_Budget;
   begin
      Status := Invalid_State;
      if Object.Value /= Fresh then return; end if;
      Object.Value := Quarantined;
      Last := Now;
      if Last = Unsigned_64'Last then Status := Invalid_Clock; return; end if;
      Status := Timed_Out;
      for Cycle in 1 .. 2 loop
         if not Sample_Time then return; end if;
         Started := Last;
         Write_Reset (1); -- GEN11_GRDOM_FULL, never a caller-selected mask.
         Acked := False;
         for Poll in 1 .. Poll_Limit loop
            if not Within_Reset_Budget then return; end if;
            Value := Read_Reset;
            if not Within_Reset_Budget then return; end if;
            if Value = Unsigned_32'Last then Status := Invalid_MMIO; return; end if;
            if (Value and 1) = 0 then Acked := True; exit; end if;
            if Poll < Poll_Limit then Pause; end if;
         end loop;
         if not Acked then return; end if;
      end loop;
      Started := Last;
      Settled := False;
      for Poll in 1 .. Poll_Limit loop
         Pause;
         if not Sample_Time then return; end if;
         -- Includes counter/clock error as well as integer conversion error.
         if Last - Started >= 52 then Settled := True; exit; end if;
      end loop;
      if not Settled then return; end if;
      Object.Value := Reset_Complete;
      Status := Complete;
   end Execute;
end Intel_GPU_GT_Reset;
