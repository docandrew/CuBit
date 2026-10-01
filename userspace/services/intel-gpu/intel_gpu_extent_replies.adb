package body Intel_GPU_Extent_Replies with SPARK_Mode is
   procedure Cancel (Object : in out Assembly) is
   begin
      Object.Broken := True;
   end Cancel;

   procedure Start
     (Object : in out Assembly; CPU_Base, Arena_ID : Unsigned_64;
      Success : out Boolean) is
   begin
      Success := False;
      if Object.Started or else Object.Broken then return; end if;
      Object.Started := True;
      if Arena_ID = 0 or else CPU_Base mod E.Block_Bytes /= 0 or else
        CPU_Base >= 16#0000_8000_0000_0000# or else
        E.Capacity > 16#0000_8000_0000_0000# - CPU_Base
      then Cancel (Object); return; end if;
      Object.CPU := CPU_Base;
      Object.Identity := Arena_ID;
      Success := True;
   end Start;

   procedure Accept_Reply
     (Object : in out Assembly; Data : Words; Success : out Boolean) is
   begin
      Success := False;
      if not Object.Started or else Object.Broken or else Object.Count = 16 then
         Cancel (Object); return;
      end if;
      if Data (0) /= Unsigned_64 (Object.Count) or else
        Data (3) /= Object.Identity or else
        Data (2) /= Object.CPU + Unsigned_64 (Object.Count) * E.Block_Bytes or else
        Data (1) = 0 or else Data (1) mod E.Block_Bytes /= 0 or else
        Data (1) > 2 ** 32 - E.Block_Bytes
      then Cancel (Object); return; end if;
      for I in E.Block_Index loop
         if I < Object.Count and then Object.Bases (I) = Data (1) then
            Cancel (Object); return;
         end if;
      end loop;
      Object.Bases (Object.Count) := Data (1);
      Object.Count := Object.Count + 1;
      if Object.Count = 16 then
         E.Admit (Object.Bases, Object.Mapping, Success);
         if not Success then Cancel (Object); end if;
      else
         Success := True;
      end if;
   end Accept_Reply;

   function Result (Object : Assembly) return E.Map is
      Empty : E.Map;
   begin
      if not Object.Started or else Object.Broken or else Object.Count /= 16 then
         return Empty;
      end if;
      return Object.Mapping;
   end Result;
end Intel_GPU_Extent_Replies;
