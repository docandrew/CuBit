package body Intel_GPU_GuC_Wait is
   use Interfaces;
   use Intel_GPU_GuC_Status;
   procedure Execute (Poll_Limit : Positive; Status : out Result;
     Last_Raw : out Unsigned_32; Last_State : out State)
   is
      First, Previous, Stamp : Unsigned_64;
   begin
      Last_Raw := Unsigned_32'Last;
      Last_State := Invalid_MMIO;
      Status := Invalid_Clock;
      First := Now;
      if First = Unsigned_64'Last then return; end if;
      Previous := First;
      for Poll in 1 .. Poll_Limit loop
         Last_Raw := Read_Status;
         Last_State := Decode (Last_Raw);
         Stamp := Now;
         if Stamp = Unsigned_64'Last or else Stamp < Previous then return; end if;
         Previous := Stamp;
         if Last_State not in Pending | Ready then Status := Device_Failed; return; end if;
         if Stamp - First >= 3_000_000 then Status := Timed_Out; return; end if;
         if Last_State = Ready then Status := Firmware_Ready; return; end if;
         if Poll < Poll_Limit then Pause; end if;
      end loop;
      Status := Timed_Out;
   end Execute;
end Intel_GPU_GuC_Wait;
