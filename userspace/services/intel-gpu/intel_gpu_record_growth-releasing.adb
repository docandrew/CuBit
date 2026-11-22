package body Intel_GPU_Record_Growth.Releasing is
   procedure Release (Object : in out Controller; Accepted : out Boolean) is
   begin
      Accepted := Object.Data.State in Empty | Idle | Failed;
      if not Accepted then return; end if;
      Release_Arena (Object.Arena, Accepted);
      if Accepted then
         Object.Data := (others => <>);
         Object.Byte_Limit := 0;
         Object.Record_Limit := 1;
      end if;
   end Release;
end Intel_GPU_Record_Growth.Releasing;
