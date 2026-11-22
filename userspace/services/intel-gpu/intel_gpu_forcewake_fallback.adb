package body Intel_GPU_Forcewake_Fallback is
   use Interfaces;
   procedure Recover
     (Request_Register, Ack_Register, Expected : Unsigned_32;
      Poll_Limit : Positive; Status : out Result;
      Timeout_Us : Unsigned_64 := 50_000)
   is
      Started : constant Unsigned_64 := Now_Us;
      Last_Time : Unsigned_64 := Started;
      Value : Unsigned_32 := 0;
      function In_Time return Boolean is
         T : constant Unsigned_64 := Now_Us;
      begin
         if T = Unsigned_64'Last or else Started = Unsigned_64'Last or else T < Last_Time then
            Status := Invalid_Clock; return False;
         end if;
         Last_Time := T;
         if T - Started >= Timeout_Us then Status := Timed_Out; return False; end if;
         return True;
      end In_Time;
      function Sample return Boolean is
      begin
         if not In_Time then return False; end if;
         Value := Read_32 (Ack_Register);
         if not In_Time then return False; end if;
         if Value = Unsigned_32'Last then Status := Invalid_MMIO; return False; end if;
         return True;
      end Sample;
      function Wait_Bit (Set : Boolean) return Boolean is
      begin
         for Attempt in 1 .. Poll_Limit loop
            if not Sample then return False; end if;
            if ((Value and 16#8000#) /= 0) = Set then return True; end if;
            if Attempt < Poll_Limit then Pause; end if;
         end loop;
         Status := Poll_Exhausted; return False;
      end Wait_Bit;
      function Delay_Pass (Pass : Positive) return Boolean is
         Delay_Start : Unsigned_64;
      begin
         -- Start the settling interval after the request write has returned.
         if not In_Time then return False; end if;
         Delay_Start := Last_Time;
         for Attempt in 1 .. Poll_Limit loop
            if not In_Time then return False; end if;
            if Last_Time - Delay_Start >= Unsigned_64 (10 * Pass) then return True; end if;
            if Attempt < Poll_Limit then Pause; end if;
         end loop;
         Status := Poll_Exhausted; return False;
      end Delay_Pass;
      Matched : Boolean;
   begin
      Status := Ack_Unchanged;
      if Expected > 1 then return; end if;
      for Pass in 1 .. 10 loop
         if not Wait_Bit (False) then return; end if;
         Write_32 (Request_Register, 16#8000_8000#);
         if not Delay_Pass (Pass) or else not Wait_Bit (True) or else not Sample then
            Write_32 (Request_Register, 16#8000_0000#);
            return;
         end if;
         Matched := (Value and 1) = Expected;
         Write_32 (Request_Register, 16#8000_0000#);
         if not Wait_Bit (False) then return; end if;
         -- Recheck original ACK after cleanup, rather than trusting a stale
         -- value sampled while the fallback request was still asserted.
         if Matched and then (Value and 1) = Expected then
            Status := Recovered; return;
         end if;
      end loop;
      Status := Ack_Unchanged;
   end Recover;
end Intel_GPU_Forcewake_Fallback;
