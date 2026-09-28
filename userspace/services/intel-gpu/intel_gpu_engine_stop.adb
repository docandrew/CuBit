package body Intel_GPU_Engine_Stop is
   use Interfaces;
   procedure Stop (Poll_Limit : Positive; Status : out Result;
                   Timeout_Us : Unsigned_64 := 100_000) is
      Started : constant Unsigned_64 := Now;
      Last : Unsigned_64 := Started;
      Value, Pending : Unsigned_32;
      function In_Time return Boolean is
         Time : constant Unsigned_64 := Now;
      begin
         if Time = Unsigned_64'Last or else Time < Last then
            Status := Invalid_Clock; return False;
         end if;
         Last := Time;
         if Time - Started >= Timeout_Us then Status := Timed_Out; return False; end if;
         return True;
      end In_Time;
      function Settle return Boolean is
         From : constant Unsigned_64 := Last;
      begin
         for Attempt in 1 .. Poll_Limit loop
            Pause;
            if not In_Time then return False; end if;
            -- Two-unit error budget includes hardware and conversion error.
            if Last - From >= 3 then return True; end if;
         end loop;
         return False;
      end Settle;
      function Wait_Bits (Power : Boolean; Mask : Unsigned_32) return Boolean is
      begin
         for Attempt in 1 .. Poll_Limit loop
            if not In_Time then return False; end if;
            Value := (if Power then Read_Power else Read_Mode);
            if not In_Time then return False; end if;
            if Value = Unsigned_32'Last then Status := Invalid_MMIO; return False; end if;
            if (Value and Mask) = Mask then return True; end if;
            if Attempt < Poll_Limit then Pause; end if;
         end loop;
         return False;
      end Wait_Bits;
   begin
      Status := Timed_Out;
      if not In_Time then return; end if;
      Write_Mode (16#0100_0100#);
      Write_Prefetch (16#0400_0400#);
      if not Wait_Bits (False, 16#200#) then return; end if;
      -- Posting read after idle acknowledgment, without treating it as a
      -- general proof that GPU caches or memory writes have been drained.
      Value := Read_Mode;
      if not In_Time then return; end if;
      if Value = Unsigned_32'Last then Status := Invalid_MMIO; return; end if;
      if (Value and 16#200#) = 0 then return; end if;
      Value := Read_Pending;
      if not In_Time then return; end if;
      if Value = Unsigned_32'Last then Status := Invalid_MMIO; return; end if;
      Pending := Shift_Right (Value and Shift_Right (Value, 16) and 16#3E00#, 9);
      if Pending /= 0 then
         if not Settle or else not Wait_Bits (True, Pending) or else not Settle then return; end if;
      end if;
      Status := Stopped;
   end Stop;
end Intel_GPU_Engine_Stop;
