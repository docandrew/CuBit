package body Intel_GPU_Reset_Prepare is
   use Interfaces;
   procedure Prepare (Poll_Limit : Positive; Status : out Result;
                      Timeout_Us : Unsigned_64 := 700) is
      Started : constant Unsigned_64 := Now;
      Last_Time : Unsigned_64 := Started;
      Value, Mask, Expected, Request : Unsigned_32;
      function In_Time return Boolean is
         Time : constant Unsigned_64 := Now;
      begin
         if Time = Unsigned_64'Last or else Time < Last_Time then
            Status := Invalid_Clock; return False;
         end if;
         Last_Time := Time;
         if Time - Started >= Timeout_Us then Status := Timed_Out; return False; end if;
         return True;
      end In_Time;
   begin
      Status := Timed_Out;
      if not In_Time then return; end if;
      Value := Read_Control;
      if not In_Time then return; end if;
      if Value = Unsigned_32'Last then Status := Invalid_MMIO; return; end if;
      if (Value and 4) /= 0 then
         -- Catastrophic-error recovery uses the distinct hardware-clear path.
         Request := 16#0004_0004#; Mask := 4; Expected := 0;
      elsif (Value and 2) = 0 then
         Request := 16#0001_0001#; Mask := 2; Expected := 2;
      else Status := Ready; return; end if;
      Write_Control (Request);
      for Attempt in 1 .. Poll_Limit loop
         if not In_Time then return; end if;
         Value := Read_Control;
         if not In_Time then return; end if;
         if Value = Unsigned_32'Last then Status := Invalid_MMIO; return; end if;
         if (Value and Mask) = Expected then Status := Ready; return; end if;
         if Attempt < Poll_Limit then Pause; end if;
      end loop;
   end Prepare;
   procedure Cancel is
   begin
      Write_Control (16#0001_0000#);
      -- This issues cancellation only; it does not prove engine quiescence.
   end Cancel;
end Intel_GPU_Reset_Prepare;
