package body PS2_Boot_Probe is
   use Interfaces;
   procedure Drain (Result : out Probe_Result) is
      Status : Unsigned_8;
      Discarded : Unsigned_8;
      pragma Unreferenced (Discarded);
   begin
      for Consumed in 0 .. Max_Stale_Bytes loop
         Status := Read_Port (16#64#);
         -- An undecoded port commonly reads all ones. Do not mistake its
         -- output-full bit for real data, or write commands to that device.
         if Status = 16#FF# then
            Result := Controller_Unavailable;
            return;
         elsif (Status and 1) = 0 then
            Result := Quiescent;
            return;
         elsif Consumed = Max_Stale_Bytes then
            Result := Drain_Limit;
            return;
         end if;
         Discarded := Read_Port (16#60#);
      end loop;
   end Drain;
end PS2_Boot_Probe;
