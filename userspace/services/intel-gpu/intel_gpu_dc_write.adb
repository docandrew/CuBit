package body Intel_GPU_DC_Write is
   use Interfaces;
   Attempted : Boolean := False;
   procedure Execute
     (Authorized : Boolean; Target : Unsigned_32;
      Read_Limit, Write_Limit : Positive; Result : out Report)
   is
      OK : Boolean;
      Value : Unsigned_32;
      procedure Write is
      begin
         Result.Writes := Result.Writes + 1;
         Write_Control (Target, OK);
      end Write;
   begin
      Result := (others => <>);
      if Attempted or else not Authorized or else Target = Unsigned_32'Last then return; end if;
      Attempted := True;
      Write;
      if not OK then Result.Status := Write_Failed; return; end if;
      for N in 1 .. Read_Limit loop
         Value := Read_Control;
         Result.Reads := N;
         if Value = Unsigned_32'Last then Result.Status := Invalid_MMIO; return; end if;
         if Value = Target then
            Result.Consecutive := Result.Consecutive + 1;
            if Result.Consecutive = 7 then Result.Status := Stable_Register; return; end if;
         else
            Result.Consecutive := 0;
            if Result.Writes = Write_Limit then
               Result.Status := Write_Budget_Exhausted; return;
            end if;
            -- Do not issue a final write we have no remaining budget to verify.
            if N = Read_Limit then exit; end if;
            Write;
            if not OK then Result.Status := Write_Failed; return; end if;
         end if;
         if N < Read_Limit then Pause; end if;
      end loop;
      Result.Status := Read_Budget_Exhausted;
   end Execute;
end Intel_GPU_DC_Write;
