with Intel_GPU_DC_Write;
package body Intel_GPU_DC_Exit is
   use Interfaces;
   Attempted : Boolean := False;
   DC_Mask : constant Unsigned_32 := 16#4000000B#;
   DC3CO : constant Unsigned_32 := 16#40000000#;
   DC3CO_Status : constant Unsigned_32 := 16#20000000#;
   package Writer is new Intel_GPU_DC_Write (Read_Control, Write_Control, Pause);
   procedure Execute (Authorized, PW1_Held : Boolean;
                      Poll_Limit : Positive; Result : out Report)
   is
      Value : Unsigned_32;
      Started, Stamp, Previous : Unsigned_64;
      OK, Delayed : Boolean := False;
      Written : Writer.Report;
      use type Writer.Outcome;
   begin
      Result := (others => <>);
      if Attempted or else not Authorized or else not PW1_Held then return; end if;
      Attempted := True;
      Value := Read_Control; Result.Prior := Value;
      if Value = Unsigned_32'Last then Result.Status := Invalid_MMIO; return; end if;
      if (Value and DC3CO) /= 0 then
         -- Establish clock availability before any state-changing write.
         Started := Now_Us;
         if Started = Unsigned_64'Last then Result.Status := Clock_Unavailable; return; end if;
         Value := Value and not DC3CO_Status;
         Write_Control (Value, OK);
         if not OK then Result.Status := Write_Failed; return; end if;
      end if;
      Result.Target := Value and not DC_Mask;
      Writer.Execute (True, Result.Target, Poll_Limit, Poll_Limit, Written);
      if Written.Status /= Writer.Stable_Register then
         Result.Status := (if Written.Status = Writer.Write_Failed then Write_Failed
                           elsif Written.Status = Writer.Invalid_MMIO then Invalid_MMIO
                           else Unstable_Register);
         return;
      end if;
      if (Result.Prior and DC3CO) /= 0 then
         Started := Now_Us; Previous := Started;
         if Started = Unsigned_64'Last then Result.Status := Clock_Unavailable; return; end if;
         for N in 1 .. Poll_Limit loop
            Stamp := Now_Us; Result.Delay_Polls := N;
            if Stamp = Unsigned_64'Last or else Stamp < Previous then
               Result.Status := Delay_Failed; return;
            end if;
            if Stamp - Started >= 200 then Delayed := True; exit; end if;
            Previous := Stamp;
            if N < Poll_Limit then Pause; end if;
         end loop;
         if not Delayed then Result.Status := Delay_Failed; return; end if;
      end if;
      Restore_And_Validate (Result.Prior, OK);
      if not OK then Result.Status := Restore_Failed; return; end if;
      Value := Read_Control;
      if Value = Unsigned_32'Last or else (Value and DC_Mask) /= 0 then
         Result.Status := Invalid_MMIO; return;
      end if;
      Result.Status := Ready;
   end Execute;
end Intel_GPU_DC_Exit;
