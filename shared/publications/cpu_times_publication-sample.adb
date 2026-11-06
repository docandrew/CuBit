------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

procedure CPU_Times_Publication.Sample (CPU : Processor; Result : out CPU_Reading)
with SPARK_Mode
is
   Before : Unsigned_64;
   Copy   : Fields;
   Now    : Unsigned_64;
begin
   for Attempt in Read_Attempt loop
      Before := Sequence (CPU);
      if not Writing (Before) then
         Copy := Values (CPU);
         Now := Counter;
         if Stable (Before, Sequence (CPU)) then
            Result := Reading_Of (Copy, Now);
            return;
         end if;
      end if;
   end loop;
   Result := (Kind => Unavailable, others => <>);
end CPU_Times_Publication.Sample;
